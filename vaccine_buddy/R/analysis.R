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
# In three steps (the admin's Find tab): 1. run_analysis() finds the missed
# cells; 2. they are checked and painted on or off by hand; 3.
# split_places() turns the final cells into missed places, as follows.
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

# A team's health area as its name gives it: "ELDAS_HC_T2" -> "ELDAS HC"
team_ha_name <- function(x) team_label(sub('_T[0-9].*$', '', toupper(trimws(x))))

#' Districts with no health areas drawn in the adjuster: health areas taken
#' from the vaccination team names instead (team_ha_name()), each covering
#' the ground nearest its own teams' walking-speed GPS points, up to
#' no_map_reach_m from any of those tracks and inside the district. Only
#' health areas whose teams worked mostly in this district, with at least
#' min_ha_fixes points there, are kept. The shapes are working boundaries
#' for the analysis only -- never shown on a map.
#' sf(district_name, health_area) in EPSG:4326, or NULL.
infer_health_areas <- function(district, pings, params) {
  p <- pings[!pings$moving_fast & !is.na(pings$team_name), ]
  p <- p[!p$team_name %in% unlist(params$no_area_teams), ]
  # A team's health area as its name gives it, or as an admin set it
  fixed <- unlist(params$team_areas)
  p$ha_name <- team_ha_name(p$team_name)
  set <- p$team_name %in% names(fixed)
  p$ha_name[set] <- fixed[p$team_name[set]]
  p$key <- .ha_key(p$ha_name)
  p <- p[nchar(p$key) >= 3, ]
  if (nrow(p) == 0) return(NULL)
  # Spellings of one name (TABELAHA_HC_T7 / TABEELAH_HC_T2) are one health
  # area: a key joins a more-used one this close (stricter than team_homes())
  kn <- names(sort(table(p$key), decreasing = TRUE))
  to <- stats::setNames(kn, kn)
  for (i in seq_along(kn)[-1]) {
    big <- kn[seq_len(i - 1)]; big <- big[to[big] == big]
    d <- utils::adist(kn[i], big)[1, ] / pmax(nchar(kn[i]), nchar(big))
    if (min(d) <= 0.25) to[kn[i]] <- big[which.min(d)]
  }
  p$key <- unname(to[p$key])

  ctr <- sf::st_coordinates(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(district))))
  epsg <- .utm_epsg(ctr[1], ctr[2])
  area <- sf::st_union(sf::st_geometry(sf::st_transform(district, epsg)))

  # GPS points per health area (team name) and grid cell
  cs <- params$cell_m
  xy <- .to_utm(p$lon, p$lat, epsg)
  p$cx <- floor(xy[, 1] / cs); p$cy <- floor(xy[, 2] / cs)
  cells <- dplyr::count(p, key, cx, cy)
  centre <- function(d) sf::st_as_sf(data.frame(x = (d$cx + 0.5) * cs, y = (d$cy + 0.5) * cs), coords = c('x', 'y'), crs = epsg)
  cells$inside <- lengths(sf::st_intersects(centre(cells), area)) > 0
  share <- dplyr::summarise(dplyr::group_by(cells, key), n_in = sum(n[inside]), n = sum(n), .groups = 'drop')
  keys <- share$key[share$n_in >= params$min_ha_fixes & share$n_in >= share$n / 2]
  cells <- cells[cells$key %in% keys & cells$inside, ]
  if (nrow(cells) == 0) return(NULL)

  # Each cell goes to the health area whose teams logged most there, the
  # ground between to the nearest such cell (Voronoi), within reach of a track
  own <- dplyr::distinct(dplyr::arrange(cells, dplyr::desc(n)), cx, cy, .keep_all = TRUE)
  pts <- centre(own); pts$key <- own$key
  coarse <- unique(round(cbind((own$cx + 0.5) * cs, (own$cy + 0.5) * cs) / 250) * 250)   # fewer points to buffer
  reach <- sf::st_intersection(sf::st_buffer(sf::st_sfc(sf::st_multipoint(coarse), crs = epsg), params$no_map_reach_m, nQuadSegs = 4), area)
  if (length(unique(pts$key)) == 1 || nrow(pts) < 3) {
    ha <- sf::st_sf(key = names(which.max(tapply(own$n, own$key, sum))), geometry = reach)
  } else {
    vor <- sf::st_collection_extract(sf::st_voronoi(sf::st_union(pts), envelope = sf::st_as_sfc(sf::st_bbox(reach))), 'POLYGON')
    vor <- sf::st_sf(geometry = vor)
    hit <- sf::st_intersects(vor, pts)
    vor$key <- pts$key[vapply(hit, function(i) if (length(i)) i[1] else NA_integer_, integer(1))]
    ha <- dplyr::summarise(dplyr::group_by(vor[!is.na(vor$key), ], key), .groups = 'drop')
    ha <- suppressWarnings(sf::st_intersection(ha, reach))
    ha <- ha[!sf::st_is_empty(ha), ]
    ha <- dplyr::summarise(dplyr::group_by(suppressWarnings(sf::st_collection_extract(ha, 'POLYGON')), key), .groups = 'drop')
  }

  # Named the way most of its teams' points spell it
  nm <-dplyr::distinct(dplyr::arrange(dplyr::count(p[p$key %in% keys, ], key, ha_name), dplyr::desc(n)), key, .keep_all = TRUE)
  ha$health_area <- make.unique(nm$ha_name[match(ha$key, nm$key)], sep = ' ')
  ha$district_name <- district$district_name[1]
  sf::st_make_valid(sf::st_transform(ha[, c('district_name', 'health_area')], 4326))
}

#' The health areas (indexes into `ha_names`) a team's name points to,
#' allowing for spelling differences (MOGADISHO / Mogadishu, SHUUFAC /
#' Shufac): the equally best matches, or none.
.name_match <- function(team, ha_names) {
  k <- .team_key(team); ha_keys <- .ha_key(ha_names)
  if (nchar(k) < 3 || !any(nzchar(ha_keys))) return(integer(0))
  d <- utils::adist(k, ha_keys)[1, ] / pmax(nchar(k), nchar(ha_keys))
  if (min(d) <= 0.3) which(d == min(d)) else integer(0)
}

#' Each team's role in a campaign: the admin's choice (vb_get_team_roles(),
#' Admin page), else from its name -- FETP monitors ignored, Surge teams in
#' no health area, the rest in the health area their name gives.
#' data.frame(team_name, role = 'area' | 'no_area' | 'ignore', health_area
#' (NA = from its name), set_by_admin).
team_roles <- function(team_names, overrides = NULL) {
  tn <- base::sort(base::unique(stats::na.omit(team_names)))
  role <- ifelse(grepl(IGNORE_TEAMS_DEFAULT, tn, ignore.case = TRUE), 'ignore',
                 ifelse(grepl(NO_AREA_TEAMS_DEFAULT, tn, ignore.case = TRUE), 'no_area', 'area'))
  out <- data.frame(team_name = tn, role = role, health_area = NA_character_, set_by_admin = FALSE)
  if (!is.null(overrides)) overrides <- overrides[overrides$role %in% c('area', 'no_area', 'ignore'), , drop = FALSE]   # ('auto': follows its name)
  if (!is.null(overrides) && nrow(overrides) > 0) {
    i <- match(tn, overrides$team_name); hit <- !is.na(i)
    out$role[hit] <- overrides$role[i[hit]]
    out$health_area[hit] <- overrides$health_area_name[i[hit]]
    out$set_by_admin[hit] <- TRUE
  }
  out
}

#' team_roles() into the analysis parameters (saved with each run).
apply_team_roles <- function(params, roles) {
  params$ignore_teams  <- roles$team_name[roles$role == 'ignore']
  params$no_area_teams <- roles$team_name[roles$role == 'no_area']
  a <- roles[roles$role == 'area' & !is.na(roles$health_area), ]
  params$team_areas <- as.list(stats::setNames(a$health_area, a$team_name))
  params
}

#' Each team's own health area: the one an admin set for it, else the one
#' its name gives ("ELDAS_HC_T2" -> Eldas Health Center); only names that
#' can't be matched fall back to where the team logged the most
#' walking-speed GPS points. Teams in `no_area` (e.g. Surge) belong to none.
#' `fixed`: list(team name = health area). data.frame(team_name, health_area).
team_homes <- function(p, ha_r, ha, epsg, no_area = character(0), fixed = list()) {
  none <- data.frame(team_name = character(0), health_area = character(0))
  p <- p[!p$moving_fast & !is.na(p$team_name), ]
  if (nrow(p) == 0) return(none)
  p$ha_id <- terra::extract(ha_r, .to_utm(p$lon, p$lat, epsg))[, 1]
  counts <- dplyr::count(p[!is.na(p$ha_id), ], team_name, ha_id)
  team_names <- base::setdiff(base::unique(p$team_name), unlist(no_area))
  if (length(team_names) == 0) return(none)
  home <- vapply(team_names, function(tn) {
    fx <- fixed[[tn]]
    if (!is.null(fx) && fx %in% ha$health_area) return(fx)
    best <- .name_match(tn, ha$health_area)
    if (length(best) == 1) return(ha$health_area[best])
    if (length(best) > 1) {
      # several equally good (e.g. "... Site 1" / "... Site 2"): where it worked most
      cb <- counts[counts$team_name == tn & counts$ha_id %in% best, ]
      return(if (nrow(cb)) ha$health_area[cb$ha_id[which.max(cb$n)]] else ha$health_area[best[1]])
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

  # Each cell to the health area covering most of it -- not simply the one
  # its centre falls in, or a thin sliver of a health area left along
  # another's edge (boundary smoothing) claims a line of cells, and missed
  # places, that are really the neighbour's. A cell mostly outside every
  # health area belongs to none.
  ha_v <- terra::vect(sf::st_transform(ha, epsg))
  cover <- terra::rast(lapply(seq_len(nrow(ha)), function(i) terra::rasterize(ha_v[i], r, cover = TRUE, background = 0)))
  ha_r <- terra::which.max(cover)
  ha_r[terra::app(cover, sum) < 0.5] <- NA
  # The same for team areas: each cell to the team area (of its own health
  # area) covering most of it, if team areas cover most of it at all. One
  # team area at a time, keeping only the best so far (there can be many).
  team_r <- terra::init(r, NA)
  if (!is.null(teams) && nrow(teams) > 0) {
    te_v <- terra::vect(sf::st_transform(teams, epsg))
    best <- terra::init(r, 0); total <- terra::init(r, 0)
    for (i in seq_len(nrow(teams))) {
      own_ha <- which(ha$health_area == teams$health_area[i])
      cv <- terra::rasterize(te_v[i], r, cover = TRUE, background = 0)
      in_own <- terra::classify(ha_r, cbind(own_ha, 1), others = 0)   # 1 within its own health area
      cv <- cv * in_own
      team_r <- terra::ifel(cv > best, i, team_r)
      best <- max(best, cv); total <- total + cv
    }
    team_r[total < 0.5] <- NA
  }

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
  cells$populated <- cells$u5_pop >= params$min_u5_cell
  cells$visited <- cells$visited %in% 1

  # Which health areas had team GPS at all: one with (almost) no points
  # wasn't followed by Vaccine Buddy -- every cell would look "missed".
  # Those are reported, not analysed.
  homes <- team_homes(p, ha_r, ha, epsg, params$no_area_teams %||% character(0), params$team_areas %||% list())
  ha_data <- carrier_presence(p, ha_r, ha, params, epsg, homes)
  cells$has_data <- ha_data$has_data[cells$ha_id]
  cells$missed <- cells$populated & !cells$visited & cells$has_data

  # Everything split_places() needs later, once the missed cells have been
  # checked (and painted on or off by hand)
  ctx <- list(grid = r, epsg = epsg, cells = cells[, c('cell', 'u5_pop', 'ha_id', 'health_area', 'team_area', 'populated', 'visited')],
              ha = ha, teams = teams, ha_data = ha_data, pings = p, homes = homes, idp = idp, facilities = facilities,
              local_names = local_names, district_name = ha$district_name[1])
  list(missed = cells$cell[cells$missed], coverage = .coverage(ctx, NULL), ctx = ctx)
}

#' Coverage per health area / team area (kept for the record -- never shown),
#' with the number of places once split.
.coverage <- function(ctx, places) {
  out <- area_coverage(ctx$cells, places) |>
    dplyr::left_join(ctx$ha_data[, c('health_area', 'n_fixes', 'n_devices', 'teams', 'has_data')], by = 'health_area')
  out$district_name <- ctx$district_name
  out
}

#' Step 3: the district's final missed cells (`missed`, cell numbers of
#' ctx$grid) split into discrete missed places, each described and given
#' its team. Areas with any cell painted in by hand (`painted`) are kept
#' however few children they have. list(places (sf or NULL), coverage).
split_places <- function(ctx, missed, painted = numeric(0), params) {
  cells <- ctx$cells
  cells$missed <- cells$cell %in% missed
  mp <- missed_places(cells, ctx$grid, params, ctx$epsg, keep = painted)
  places <- mp$places
  if (!is.null(places)) {
    places <- .describe_places(places, ctx$local_names, ctx$idp, ctx$facilities, params)
    places$district_name <- ctx$district_name
    places$outline <- .place_outlines(places, mp$cells, ctx$grid, ctx$ha, ctx$teams, ctx$epsg)
    places$place_key <- NULL
    # Slivers left by trimming to the health / team area borders aren't
    # places anyone can go back to: drop them, and number the rest again
    keep <- as.numeric(sf::st_area(sf::st_transform(places$outline, ctx$epsg))) >= (params$min_place_m2 %||% 10000)
    places <- if (any(keep)) places[keep, ] else NULL
    if (!is.null(places)) {
      places <- places[order(places$health_area, -places$u5_pop), ]
      places$place_no <- stats::ave(seq_len(nrow(places)), places$health_area, FUN = seq_along)
      places <- assign_teams(places, ctx$pings, ctx$epsg, ctx$homes)
    }
  }
  list(places = places, coverage = .coverage(ctx, places))
}

#' The missed cells as one layer (squares joined), EPSG:4326 -- for the map
#' before they are split into places.
missed_layer <- function(ctx, missed) {
  if (length(missed) == 0) return(NULL)
  r <- terra::init(ctx$grid, NA)
  r[missed] <- 1
  sf::st_transform(sf::st_as_sf(terra::as.polygons(r, dissolve = TRUE)), 4326)
}

#' Describe each place from, in order: a nearby OpenStreetMap or
#' campaign-monitoring name; else a nearby IDP site (in some districts
#' they're everywhere, so only when nothing else is close); else the
#' nearest facility / IDP site further away. Also flags places at an IDP
#' site (near_idp).
.describe_places <- function(places, local_names, idp, facilities, params) {
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
  places
}

#' Grid cells as one shape (squares joined), in the grid's UTM zone.
.cell_squares <- function(r, cells, epsg) {
  half <- terra::res(r)[1] / 2
  xy <- terra::xyFromCell(r, cells)
  sf::st_union(sf::st_sfc(lapply(seq_len(nrow(xy)), function(i) {
    x <- xy[i, 1]; y <- xy[i, 2]
    sf::st_polygon(list(cbind(c(x - half, x + half, x + half, x - half, x - half), c(y - half, y - half, y + half, y + half, y - half))))
  }), crs = epsg))
}

#' Splitting by distance (k-means) can leave a place in disconnected bits.
#' Each extra bit is moved to the neighbouring place it shares an edge with
#' (same health / team area), or becomes a place of its own if it touches
#' none -- so every place is one connected shape.
.make_contiguous <- function(m, r) {
  next_key <- max(m$place_key)
  for (k in unique(m$place_key)) {
    idx <- which(m$place_key == k)
    if (length(idx) < 2) next
    piece_r <- terra::init(r, NA)
    piece_r[m$cell[idx]] <- 1
    comp <- terra::patches(piece_r, directions = 4)[m$cell[idx]][, 1]
    if (length(unique(comp)) == 1) next
    main <- as.integer(names(which.max(table(comp))))
    for (cv in setdiff(unique(comp), main)) {
      frag <- idx[comp == cv]
      nb_cells <- as.vector(terra::adjacent(r, m$cell[frag], directions = 4))
      nb <- match(nb_cells, m$cell)
      nb <- nb[!is.na(nb) & m$place_key[nb] != k & m$group[nb] == m$group[frag[1]]]
      if (length(nb)) m$place_key[frag] <- as.integer(names(which.max(table(m$place_key[nb]))))
      else { next_key <- next_key + 1L; m$place_key[frag] <- next_key }
    }
  }
  m
}

#' Place outlines that follow the real health area (and team area) borders
#' instead of grid squares, and never overlap. A grid square on a border
#' belongs to the area covering most of it, so on its own each place would
#' stop short of the border, leaving gaps between neighbouring places. So
#' each place is
#'   1. its own squares, trimmed to its own area, plus
#'   2. the parts of missed squares next to it that belong to another area
#'      and lie in its own area -- except where any place's own squares are
#'      (drawn areas can overlap a little), and except what an earlier place
#'      already has;
#' then its holes are filled (one solid shape, easier to read and walk;
#' child counts are unaffected) -- but never over another place.
#' `m`: every missed cell (cell, place_key, health_area, team_area).
#' Returned as MULTIPOLYGON, EPSG:4326.
.place_outlines <- function(places, m, r, ha, teams, epsg) {
  ha_m <- sf::st_transform(ha, epsg)
  te_m <- if (!is.null(teams) && nrow(teams) > 0) sf::st_transform(teams, epsg) else NULL
  squares <- function(cells) .cell_squares(r, cells, epsg)
  # polygons only, as one shape (empty if none)
  empty <- sf::st_sfc(sf::st_polygon(), crs = epsg)
  polys <- function(g) {
    if (is.null(g)) return(empty)
    g <- g[!sf::st_is_empty(g)]
    if (length(g) == 0) return(empty)
    g <- sf::st_make_valid(g)
    type <- as.character(sf::st_geometry_type(g))
    g <- if (any(type == 'GEOMETRYCOLLECTION')) suppressWarnings(sf::st_collection_extract(g, 'POLYGON'))
         else g[type %in% c('POLYGON', 'MULTIPOLYGON')]
    if (length(g) == 0) empty else sf::st_union(g)
  }
  safely <- function(expr, otherwise) tryCatch(suppressWarnings(expr), error = function(e) otherwise)
  area_of <- function(i) {
    a <- sf::st_union(sf::st_geometry(ha_m[ha_m$health_area == places$health_area[i], ]))
    if (!is.na(places$team_area[i]) && !is.null(te_m)) {
      tp <- te_m[te_m$team_area == places$team_area[i] & te_m$health_area == places$health_area[i], ]
      if (nrow(tp)) a <- sf::st_intersection(a, sf::st_union(sf::st_geometry(tp)))
    }
    sf::st_make_valid(a)
  }
  n <- nrow(places)
  grp <- paste(m$health_area, m$team_area)
  own <- lapply(seq_len(n), function(i) m$place_key == places$place_key[i])
  areas <- lapply(seq_len(n), area_of)

  # 1. own squares, trimmed to the place's area (untrimmed if that fails)
  core <- lapply(seq_len(n), function(i) {
    sq <- squares(m$cell[own[[i]]])
    g <- safely(polys(sf::st_intersection(sq, areas[[i]])), sq)
    if (all(sf::st_is_empty(g))) sq else g
  })
  all_core <- sf::st_union(do.call(c, core))

  # 2. border squares of other areas, in this place's area, nobody's core,
  #    and not already taken by an earlier place
  shapes <- vector('list', n)
  taken <- empty
  for (i in seq_len(n)) {
    nb <- as.vector(terra::adjacent(r, m$cell[own[[i]]], directions = 8))
    extra <- m$cell %in% nb & grp != grp[own[[i]]][1]
    g <- core[[i]]
    if (any(extra)) {
      e <- safely(polys(sf::st_difference(polys(sf::st_intersection(squares(m$cell[extra]), areas[[i]])), all_core)), NULL)
      if (!is.null(e) && !all(sf::st_is_empty(e))) e <- safely(polys(sf::st_difference(e, taken)), e)
      if (!is.null(e) && !all(sf::st_is_empty(e))) { g <- polys(c(g, e)); taken <- polys(c(taken, e)) }
    }
    shapes[[i]] <- g
  }

  # 3. fill holes, then take back out any other place a hole held
  all_shapes <- do.call(c, shapes)
  filled <- sf::st_make_valid(sf::st_sfc(sfheaders::sf_remove_holes(sf::st_cast(all_shapes, 'MULTIPOLYGON')), crs = epsg))
  hits <- sf::st_intersects(filled, all_shapes)
  out <- lapply(seq_len(n), function(i) {
    others <- setdiff(hits[[i]], i)
    g <- if (length(others) == 0) polys(filled[i])
         else safely(polys(sf::st_difference(filled[i], sf::st_union(all_shapes[others]))), all_shapes[i])
    if (all(sf::st_is_empty(g))) all_shapes[i] else g
  })
  sf::st_cast(sf::st_make_valid(sf::st_transform(do.call(c, out), 4326)), 'MULTIPOLYGON')
}

#' Missed cells grouped and split into places. Places with any cell in
#' `keep` (painted in by hand) are kept below min_place_u5.
missed_places <- function(cells, r, params, epsg, keep = numeric(0)) {
  m <- cells[cells$missed, ]
  if (nrow(m) == 0) return(list(places = NULL, cells = NULL))

  missed_r <- terra::init(r, NA)
  missed_r[m$cell] <- 1
  # cells join only across a shared edge (not corner to corner)
  m$patch <- terra::patches(missed_r, directions = 4)[m$cell][, 1]
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
  m <- .make_contiguous(m, r)

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
      by_hand     = any(cell %in% keep),
      .groups = 'drop') |>
    dplyr::select(-target) |>
    dplyr::filter(u5_pop >= params$min_place_u5 | by_hand) |>
    dplyr::select(-by_hand)
  if (nrow(agg) == 0) return(list(places = NULL, cells = NULL))

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
  # place_key kept until the outlines are drawn (.place_outlines())
  list(places = out, cells = m[, c('cell', 'place_key', 'health_area', 'team_area')])
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

#' Step 1, for one or more districts: the missed cells. Returns, per
#' district (named lists): missed (cell numbers), coverage, and ctx (the
#' grid and all split_places() needs).
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
  list(missed = lapply(res, `[[`, 'missed'), coverage = lapply(res, `[[`, 'coverage'), ctx = lapply(res, `[[`, 'ctx'))
}
