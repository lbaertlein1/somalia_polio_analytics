# =============================================================================
# analysis.R -- populated / visited 100 m grid and missed places
#
# Within each district, a 100 m x 100 m grid (cell_m) in the district's UTM
# zone, with WorldPop's under-5 estimates summed into it (totals kept):
#   populated  cell's under-5 estimate >= min_u5_cell
#   visited    at least one walking-speed vaccination team GPS point
#              falls inside the cell
#   missed     populated and not visited (only in health areas that had
#              team GPS data at all)
# Missed cells that touch (8 directions) and share a team area form one
# missed place; places too big to walk (max_place_cells) or with more
# children than one team can take on (max_place_children) are split with
# k-means. Each place gets a navigation point (its cell nearest the
# population-weighted centre) and a plain-language label relative to the
# nearest named place.
# =============================================================================

.utm_epsg <- function(lon, lat) (if (lat >= 0) 32600 else 32700) + floor((lon + 180) / 6) + 1

#' Plain-language position: "420 m NE of Hodan HC" for the nearest
#' landmark within max_m, else NA.
describe_position <- function(pts, landmarks, max_m) {
  out <- rep(NA_character_, nrow(pts))
  if (is.null(landmarks) || nrow(landmarks) == 0 || nrow(pts) == 0) return(out)
  nearest <- sf::st_nearest_feature(pts, landmarks)
  dist <- as.numeric(sf::st_distance(pts, landmarks[nearest, ], by_element = TRUE))
  compass <- c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW')
  for (i in which(dist <= max_m)) {
    az <- as.numeric(lwgeom::st_geod_azimuth(c(sf::st_geometry(landmarks)[nearest[i]], sf::st_geometry(pts)[i]))) * 180 / pi
    dir <- compass[(round(((az %% 360) / 45)) %% 8) + 1]
    out[i] <- if (dist[i] < 75) sprintf('at %s', landmarks$name[nearest[i]])
              else sprintf('%s m %s of %s', format(round(dist[i], -1), big.mark = ','), dir, landmarks$name[nearest[i]])
  }
  out
}

#' Local place names (vb_place_names rows: name, lon, lat) as landmarks.
#' The same name recorded at about the same spot (~200 m) by several
#' monitors becomes one point, spelled the way most of them wrote it.
local_name_landmarks <- function(names_df) {
  if (is.null(names_df) || nrow(names_df) == 0) return(NULL)
  d <- names_df
  d$key <- paste(tolower(gsub('[^[:alnum:]]', '', d$name)), round(d$lon / 0.002), round(d$lat / 0.002))
  d <- d |>
    dplyr::group_by(key) |>
    dplyr::summarise(name = names(sort(table(name), decreasing = TRUE))[1], lon = mean(lon), lat = mean(lat), .groups = 'drop')
  sf::st_as_sf(d[, c('name', 'lon', 'lat')], coords = c('lon', 'lat'), crs = 4326)
}

.landmarks <- function(facilities, idp) {
  parts <- list()
  if (!is.null(facilities) && nrow(facilities) > 0)
    parts$f <- sf::st_sf(name = trimws(as.character(facilities$facility_name %||% 'health facility')), geometry = sf::st_geometry(facilities))
  if (!is.null(idp) && nrow(idp) > 0)
    parts$i <- sf::st_sf(name = paste(idp$idp_name, 'IDP site'), geometry = sf::st_geometry(idp))
  if (length(parts) == 0) return(NULL)
  out <- do.call(rbind, unname(parts))
  out[!is.na(out$name) & nzchar(out$name), ]
}

# Health-area and team names reduced to their distinctive part, for
# matching: "Eldas Health Center" -> "eldas", "ELDAS_HC_T2" -> "eldas",
# "Foorilow Coordination Site" -> "foorilow", "SHUUFAC_SITE_T1" -> "shuufac".
.ha_key <- function(x) {
  x <- gsub('[^a-z]+', ' ', tolower(x))   # "eldas_hc" -> "eldas hc", so the hc word drops below
  x <- gsub('health cent(er|re)|coordination site|cordination site|cordiniation site|hospital|district|site|centre|center', ' ', x)
  x <- gsub('\\b(hc|sc|mch|phu)\\b', ' ', x)
  gsub('[^a-z]', '', x)
}
.team_key <- function(x) .ha_key(sub('_T[0-9].*$', '', toupper(trimws(x))))

#' Each team's own health area. Vaccination team names carry their health
#' area ("ELDAS_HC_T2" -> Eldas Health Center), so that's used first,
#' allowing for spelling differences (MOGADISHO / Mogadishu, SHUUFAC /
#' Shufac); only names that can't be matched fall back to where the team
#' logged the most walking-speed GPS points. data.frame(team_name, health_area).
team_homes <- function(p, ha_r, ha, epsg) {
  none <- data.frame(team_name = character(0), health_area = character(0))
  p <- p[!p$moving_fast & !is.na(p$team_name), ]
  if (nrow(p) == 0) return(none)
  p$ha_id <- terra::extract(ha_r, .to_utm(p$lon, p$lat, epsg))[, 1]
  counts <- dplyr::count(p[!is.na(p$ha_id), ], team_name, ha_id)
  team_names <- base::unique(p$team_name)
  ha_keys <- .ha_key(ha$health_area)
  home <- vapply(team_names, function(tn) {
    k <- .team_key(tn)
    if (nchar(k) >= 3 && any(nzchar(ha_keys))) {
      d <- utils::adist(k, ha_keys)[1, ] / pmax(nchar(k), nchar(ha_keys))
      best <- which(d == min(d))
      if (min(d) <= 0.3) {
        if (length(best) == 1) return(ha$health_area[best])
        # several equally good (e.g. "... Site 1" / "... Site 2"): where it worked most
        cb <- counts[counts$team_name == tn & counts$ha_id %in% best, ]
        if (nrow(cb)) return(ha$health_area[cb$ha_id[which.max(cb$n)]])
        return(ha$health_area[best[1]])
      }
    }
    c1 <- counts[counts$team_name == tn, ]
    if (nrow(c1)) ha$health_area[c1$ha_id[which.max(c1$n)]] else NA_character_
  }, character(1))
  out <- data.frame(team_name = team_names, health_area = unname(home))
  out[!is.na(out$health_area), ]
}

#' Which team each missed place is for: its adjuster team area where one
#' exists, otherwise the team *of that health area* (team_homes) whose
#' walking-speed GPS points came closest to the place -- so a supervisor is
#' only ever given their own teams. A health area with no teams of its own
#' in the data gets no team (never another health area's). team_dist_m =
#' that distance.
assign_teams <- function(places, pings, epsg, homes) {
  places$assigned_team <- places$team_area
  places$team_source <- ifelse(is.na(places$team_area), NA_character_, 'team area')
  places$team_dist_m <- NA_real_
  fixes <- pings[!pings$moving_fast & !is.na(pings$team_name), ]
  if (!any(is.na(places$assigned_team)) || nrow(fixes) == 0) return(places)
  fx <- sf::st_transform(sf::st_as_sf(fixes[, c('lon', 'lat', 'team_name')], coords = c('lon', 'lat'), crs = 4326), epsg)
  for (h in unique(places$health_area[is.na(places$assigned_team)])) {
    todo <- which(is.na(places$assigned_team) & places$health_area == h)
    cand <- fx[fx$team_name %in% homes$team_name[homes$health_area == h], ]
    if (nrow(cand) == 0) next
    outl <- sf::st_transform(places$outline[todo], epsg)
    nn <- sf::st_nearest_feature(outl, cand)
    places$assigned_team[todo] <- cand$team_name[nn]
    places$team_source[todo] <- 'nearest carrier'
    places$team_dist_m[todo] <- round(as.numeric(sf::st_distance(outl, cand[nn, ], by_element = TRUE)))
  }
  places
}

#' Usable fixes, carriers and teams per health area (one row per health
#' area, in ha_id order). `teams` = the teams whose own health area this is
#' (team_homes()).
carrier_presence <- function(p, ha_r, ha, params, epsg, homes) {
  out <- data.frame(health_area = ha$health_area, n_fixes = 0L, n_devices = 0L, teams = '')
  p <- p[!p$moving_fast, ]
  if (nrow(p) > 0) {
    p$ha_id <- terra::extract(ha_r, .to_utm(p$lon, p$lat, epsg))[, 1]
    s <- p[!is.na(p$ha_id), ] |>
      dplyr::group_by(ha_id) |>
      dplyr::summarise(n_fixes = dplyr::n(), n_devices = dplyr::n_distinct(device_id), .groups = 'drop')
    out$n_fixes[s$ha_id] <- s$n_fixes; out$n_devices[s$ha_id] <- s$n_devices
  }
  out$teams <- vapply(out$health_area, function(h) paste(sort(homes$team_name[homes$health_area == h]), collapse = ', '), '')
  out$has_data <- out$n_fixes >= params$min_ha_fixes
  out
}

# lon/lat -> metres in the given UTM zone (matrix of x, y)
.to_utm <- function(lon, lat, epsg) sf::sf_project('EPSG:4326', paste0('EPSG:', epsg), cbind(lon, lat))

#' One district. ha / teams / facilities / idp are that district's rows.
classify_district <- function(ha, teams, pings, facilities, idp, u5, params, local_names = NULL) {
  ha$ha_id <- seq_len(nrow(ha))
  ctr <- sf::st_coordinates(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(ha))))
  epsg <- .utm_epsg(ctr[1], ctr[2])

  # 100 m grid over the district (plus 200 m), on round metre coordinates
  cs <- params$cell_m
  bm <- sf::st_bbox(sf::st_transform(ha, epsg))
  grid <- terra::rast(terra::ext(floor((bm[['xmin']] - 200) / cs) * cs, ceiling((bm[['xmax']] + 200) / cs) * cs,
                                 floor((bm[['ymin']] - 200) / cs) * cs, ceiling((bm[['ymax']] + 200) / cs) * cs),
                      resolution = cs, crs = paste0('EPSG:', epsg))
  # WorldPop under-5 counts summed into the 100 m cells (area-weighted, totals kept)
  src <- terra::crop(u5, terra::ext(terra::vect(ha)) + 0.01, snap = 'out')
  r <- terra::project(src, grid, method = 'sum')

  ha_r <- terra::rasterize(terra::vect(sf::st_transform(ha, epsg)), r, field = 'ha_id')
  team_r <- if (!is.null(teams) && nrow(teams) > 0) {
    teams$team_id <- seq_len(nrow(teams))
    terra::rasterize(terra::vect(sf::st_transform(teams, epsg)), r, field = 'team_id')
  } else terra::init(r, NA)

  bb <- sf::st_bbox(ha) + c(-0.01, -0.01, 0.01, 0.01)
  p <- pings[pings$lon >= bb['xmin'] & pings$lon <= bb['xmax'] & pings$lat >= bb['ymin'] & pings$lat <= bb['ymax'], ]

  # Visited: the cell holds at least one walking-speed team GPS point
  visited_r <- terra::init(r, 0)
  walk <- p[!p$moving_fast, ]
  if (nrow(walk) > 0) {
    vc <- unique(stats::na.omit(terra::cellFromXY(r, .to_utm(walk$lon, walk$lat, epsg))))
    if (length(vc)) visited_r[vc] <- 1
  }

  stack <- c(r, ha_r, team_r, visited_r)
  names(stack) <- c('u5_pop', 'ha_id', 'team_id', 'visited')
  cells <- terra::as.data.frame(stack, cells = TRUE, na.rm = FALSE)
  cells <- cells[!is.na(cells$ha_id), ]
  cells$u5_pop[is.na(cells$u5_pop)] <- 0
  cells$health_area <- ha$health_area[cells$ha_id]
  cells$team_area <- if (!is.null(teams) && nrow(teams) > 0) teams$team_area[cells$team_id] else NA_character_
  # A team polygon slightly overlapping the next health area (smoothing)
  # doesn't move a cell into the other health area's team.
  if (!is.null(teams) && nrow(teams) > 0)
    cells$team_area[!is.na(cells$team_id) & teams$health_area[cells$team_id] != cells$health_area] <- NA
  cells$populated <- cells$u5_pop >= params$min_u5_cell
  cells$visited <- cells$visited %in% 1

  # Which health areas had team GPS at all: one with (almost) no points
  # wasn't followed by Vaccine Buddy -- every cell would look "missed".
  # Those are reported, not analysed.
  homes <- team_homes(p, ha_r, ha, epsg)
  ha_data <- carrier_presence(p, ha_r, ha, params, epsg, homes)
  cells$has_data <- ha_data$has_data[cells$ha_id]
  cells$missed <- cells$populated & !cells$visited & cells$has_data

  places <- missed_places(cells, r, params, epsg)
  if (!is.null(places)) {
    # Describe each place from, in order: a nearby OpenStreetMap or
    # campaign-monitoring name; else a nearby IDP site (in some districts
    # they're everywhere, so only when nothing else is close); else the
    # nearest facility / IDP site further away.
    idp_lm <- if (!is.null(idp) && nrow(idp) > 0) sf::st_sf(name = paste(idp$idp_name, 'IDP site'), geometry = sf::st_geometry(idp))
    places$label <- describe_position(places, local_names, params$place_name_max_m)
    todo <- is.na(places$label)
    if (any(todo)) places$label[todo] <- describe_position(places[todo, ], idp_lm, params$place_name_max_m)
    todo <- is.na(places$label)
    if (any(todo)) places$label[todo] <- describe_position(places[todo, ], .landmarks(facilities, idp), params$label_max_m)
    places$label <- ifelse(is.na(places$label), paste('in', dplyr::coalesce(places$team_area, places$health_area)), places$label)
    places$near_idp <- NA_character_
    if (!is.null(idp) && nrow(idp) > 0) {
      nn <- sf::st_nearest_feature(places, idp)
      d <- as.numeric(sf::st_distance(places, idp[nn, ], by_element = TRUE))
      places$near_idp <- ifelse(d <= params$idp_near_m, idp$idp_name[nn], NA_character_)
    }
    places$district_name <- ha$district_name[1]
    places <- assign_teams(places, p, epsg, homes)
  }

  coverage <- area_coverage(cells, places) |>
    dplyr::left_join(ha_data[, c('health_area', 'n_fixes', 'n_devices', 'teams', 'has_data')], by = 'health_area')
  coverage$district_name <- ha$district_name[1]

  list(places = places, coverage = coverage)
}

missed_places <- function(cells, r, params, epsg) {
  m <- cells[cells$missed, ]
  if (nrow(m) == 0) return(NULL)

  missed_r <- terra::init(r, NA)
  missed_r[m$cell] <- 1
  m$patch <- terra::patches(missed_r, directions = 8)[m$cell][, 1]
  m$group <- as.integer(factor(paste(m$patch, m$ha_id, m$team_area)))

  xy <- terra::xyFromCell(r, m$cell)   # metres (UTM)
  m$x <- xy[, 1]; m$y <- xy[, 2]
  # Split a missed area that is too big to walk (max_place_cells) or has
  # more children than one team can take on in a return visit
  # (max_place_children), into that many walkable pieces.
  max_cells <- max(2, params$max_place_cells)
  max_kids  <- max(1, params$max_place_children)
  m$sub <- 1L
  for (g in unique(m$group[duplicated(m$group)])) {
    idx <- which(m$group == g)
    k <- max(ceiling(length(idx) / max_cells), ceiling(sum(m$u5_pop[idx]) / max_kids))
    if (k < 2) next
    if (k >= length(idx)) { m$sub[idx] <- seq_along(idx); next }   # one cell per place
    pts <- cbind(m$x[idx], m$y[idx])
    m$sub[idx] <- withr::with_seed(1, stats::kmeans(pts, centers = k, nstart = 3)$cluster)
  }
  m$place_key <- as.integer(factor(paste(m$group, m$sub)))

  agg <- m |>
    dplyr::group_by(place_key) |>
    dplyr::summarise(
      health_area = dplyr::first(health_area),
      team_area   = dplyr::first(team_area),
      n_cells     = dplyr::n(),
      # navigation target: the place's cell nearest its population-weighted
      # centre (computed before u5_pop is summed, and lon/lat overwritten)
      target      = which.min((x - stats::weighted.mean(x, u5_pop + 1e-6))^2 + (y - stats::weighted.mean(y, u5_pop + 1e-6))^2),
      x           = x[target],
      y           = y[target],
      u5_pop      = sum(u5_pop),
      .groups = 'drop') |>
    dplyr::select(-target) |>
    dplyr::filter(u5_pop >= params$min_place_u5)
  if (nrow(agg) == 0) return(NULL)

  key_r <- terra::init(r, NA)
  key_r[m$cell] <- m$place_key
  outlines <- sf::st_transform(sf::st_as_sf(terra::as.polygons(key_r, dissolve = TRUE)), 4326)
  names(outlines)[1] <- 'place_key'

  ll <- sf::sf_project(paste0('EPSG:', epsg), 'EPSG:4326', cbind(agg$x, agg$y))
  agg$lon <- ll[, 1]; agg$lat <- ll[, 2]; agg$x <- NULL; agg$y <- NULL
  out <- sf::st_as_sf(agg, coords = c('lon', 'lat'), crs = 4326, remove = FALSE)
  out$outline <- sf::st_geometry(outlines)[match(out$place_key, outlines$place_key)]
  out <- out |>
    dplyr::arrange(health_area, dplyr::desc(u5_pop)) |>
    dplyr::group_by(health_area) |>
    dplyr::mutate(place_no = dplyr::row_number()) |>
    dplyr::ungroup()
  out$place_key <- NULL
  out
}

area_coverage <- function(cells, places) {
  summ <- function(d) dplyr::summarise(d,
    populated_cells = sum(populated),
    visited_cells   = sum(populated & visited),
    u5_pop_visited  = sum(u5_pop[populated & visited]),   # before u5_pop is overwritten below
    u5_pop          = sum(u5_pop[populated]),
    .groups = 'drop')
  # One total row per health area (team_area = '') plus one per team area
  by_ha   <- cells |> dplyr::group_by(health_area) |> summ() |> dplyr::mutate(team_area = '')
  by_team <- cells |> dplyr::filter(!is.na(team_area)) |> dplyr::group_by(health_area, team_area) |> summ()
  out <- dplyr::bind_rows(by_ha, by_team)

  out$n_places <- 0L
  if (!is.null(places)) {
    pl <- sf::st_drop_geometry(places)
    counts <- dplyr::bind_rows(
      dplyr::count(pl, health_area, name = 'n') |> dplyr::mutate(team_area = ''),
      dplyr::count(dplyr::filter(pl, !is.na(team_area)), health_area, team_area, name = 'n'))
    hit <- match(paste(out$health_area, out$team_area), paste(counts$health_area, counts$team_area))
    out$n_places <- dplyr::coalesce(counts$n[hit], 0L)
  }
  out
}

#' One or more districts. Returns places (sf, point geometry + `outline`
#' polygon column) and coverage (data.frame, kept for the record -- never
#' shown).
run_analysis <- function(areas, pings, params, u5, progress = function(i, n, d) NULL, place_names = NULL) {
  local_names <- local_name_landmarks(place_names)
  districts <- sort(unique(areas$health_areas$district_name))
  res <- list()
  for (i in seq_along(districts)) {
    d <- districts[i]
    progress(i, length(districts), d)
    pick <- function(x) if (is.null(x)) NULL else x[x$district_name == d, ]
    res[[d]] <- classify_district(pick(areas$health_areas), pick(areas$team_areas), pings,
                                  pick(areas$facilities), pick(areas$idp), u5, params, local_names)
  }
  places <- Filter(Negate(is.null), lapply(res, `[[`, 'places'))
  list(
    places   = if (length(places)) dplyr::bind_rows(places) else NULL,
    coverage = dplyr::bind_rows(lapply(res, `[[`, 'coverage'))
  )
}
