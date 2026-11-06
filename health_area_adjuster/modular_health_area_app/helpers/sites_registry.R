# =============================================================================
# sites_registry.R
#
# Read/write access to the site registry -- the "sites" entity list in ODK
# Central project 36 -- for the Sites tab. ODK is the source of truth: every
# add / edit / remove is written to Central immediately.
#
#   sites_registry_get()      all sites (data frame, one row per entity),
#                             cached in-process; falls back to the last copy
#                             saved on disk (read-only) if Central is down
#   sites_registry_create()   new site -> Central, returns the new row
#   sites_registry_update()   edit -> Central with baseVersion (conflict
#                             detection: someone else changed it first)
#   sites_registry_remove()   soft delete: status "removed" + reason
#   site_mhfl_facilities()    MHFL mirror facilities for a district
#
# Every write sets last_edited_by to the app user, which also marks a
# synced IDP/nomadic site as locally edited -> syncs leave it alone.
# Needs odk_forms/central_helpers.R sourced first.
# =============================================================================

SITE_TYPES <- c(idp = "IDP settlement", nomadic = "Nomadic settlement", fixed = "Fixed vaccination site",
                cross_border = "Cross-border vaccination site", transit = "Transit vaccination site")

# Subtype choices come from the ODK form itself, so the app and the form
# can never disagree.
SITE_SUBTYPES <- tryCatch({
  ch <- readxl::read_excel("odk_forms/site_form.xlsx", "choices")
  ch[ch$list_name == "site_subtype", c("name", "label", "site_type")]
}, error = function(e) data.frame(name = character(0), label = character(0), site_type = character(0)))

REMOVAL_REASONS <- c(no_longer_exists = "No longer exists", duplicate = "Duplicate of another site",
                     entered_by_mistake = "Entered by mistake")

.sites_cache      <- new.env(parent = emptyenv())
.sites_cache_file <- file.path("cache", "sites_registry.rds")
SITES_CACHE_TTL   <- 300   # seconds before an automatic re-read from Central

# Returns list(sites = <data frame>, offline = TRUE/FALSE, fetched_at = POSIXct).
sites_registry_get <- function(force = FALSE) {
  fresh <- !is.null(.sites_cache$sites) && !isTRUE(.sites_cache$offline) &&
    as.numeric(difftime(Sys.time(), .sites_cache$fetched_at, units = "secs")) < SITES_CACHE_TTL
  if (fresh && !force) return(as.list(.sites_cache))

  s <- tryCatch(central_entities(MICROPLAN_PROJECT, "sites"), error = function(e) {
    cat("[sites] Central read failed:", conditionMessage(e), "\n"); NULL })
  if (!is.null(s)) {
    for (c in c("__id", "__version", "label", SITE_PROPS)) if (!c %in% names(s)) s[[c]] <- character(nrow(s))
    .sites_cache$sites <- s; .sites_cache$offline <- FALSE; .sites_cache$fetched_at <- Sys.time()
    tryCatch({ dir.create(dirname(.sites_cache_file), showWarnings = FALSE)
               saveRDS(list(sites = s, fetched_at = .sites_cache$fetched_at), .sites_cache_file) },
             error = function(e) NULL)
  } else if (is.null(.sites_cache$sites) || !isTRUE(.sites_cache$offline)) {
    disk <- tryCatch(readRDS(.sites_cache_file), error = function(e) NULL)
    .sites_cache$sites <- disk$sites %||% .sites_cache$sites
    .sites_cache$fetched_at <- disk$fetched_at %||% .sites_cache$fetched_at
    .sites_cache$offline <- TRUE
  }
  as.list(.sites_cache)
}

# Parse "lat lon alt acc" -> lat/lon columns.
sites_with_coords <- function(s) {
  g <- strsplit(s$geometry, " ", fixed = TRUE)
  s$lat <- suppressWarnings(as.numeric(vapply(g, function(x) if (length(x) >= 2) x[1] else NA_character_, "")))
  s$lon <- suppressWarnings(as.numeric(vapply(g, function(x) if (length(x) >= 2) x[2] else NA_character_, "")))
  s
}

# ---- site symbols (shared by every map) ---------------------------------------
# Same shape + colour per type as the printed maps (.EXPORT_SITE_STYLE,
# helpers/printable_export.R), as small SVG images for plain Leaflet
# markers. Not leaflet's awesome markers: the app loads its own Leaflet
# (ui.R, for paint-app.js) next to the R leaflet package's copy, and the
# awesome-markers plugin ends up on the wrong copy -- nothing is drawn.
.SITE_TYPE_HEX <- setNames(.EXPORT_SITE_STYLE$fill, .EXPORT_SITE_STYLE$type)
.site_marker_svg <- function(type, fill = .SITE_TYPE_HEX[[type]]) {
  shape <- .EXPORT_SITE_STYLE$shape[.EXPORT_SITE_STYLE$type == type]
  body <- switch(as.character(shape),
    `24` = '<polygon points="11,2 20,19 2,19"/>',            # triangle (IDP)
    `23` = '<polygon points="11,1 21,11 11,21 1,11"/>',       # diamond (nomadic)
    `22` = '<rect x="3" y="3" width="16" height="16"/>',      # square (fixed)
    `25` = '<polygon points="2,3 20,3 11,20"/>',              # down triangle (cross-border)
    '<circle cx="11" cy="11" r="8"/>')                        # circle (transit)
  svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="22" height="22"><g fill="%s" stroke="white" stroke-width="2">%s</g></svg>',
                 fill, body)
  paste0("data:image/svg+xml;charset=UTF-8,", utils::URLencode(svg, reserved = TRUE))
}
.SITE_MARKER_URL <- vapply(.EXPORT_SITE_STYLE$type, .site_marker_svg, "")
.SITE_MARKER_URL_REMOVED <- vapply(.EXPORT_SITE_STYLE$type, function(t) .site_marker_svg(t, "#9ca3af"), "")

# Marker size in pixels: IDP / nomadic settlements are context (small),
# fixed / cross-border / transit sites are operational (larger).
site_icon_px <- function(type, base = 14) ifelse(type %in% SITE_CONTEXT_TYPES, round(base * 0.65), base)

# Legend lines for the site symbols (HTML string, one line per type), for
# the map legends that used to show a single red "IDP Settlement" dot.
site_legend_html <- function() {
  lbl <- setNames(.EXPORT_SITE_STYLE$label, .EXPORT_SITE_STYLE$type)
  paste0(vapply(.EXPORT_SITE_STYLE$type, function(t) sprintf(
    '<img src="%s" width="11" height="11" style="margin-right:6px;vertical-align:middle;">%s<br>',
    .SITE_MARKER_URL[[t]], lbl[[t]]), ""), collapse = "")
}

# ---- registry sites as map context / IDP source for the other tabs -------------
# Active registry sites inside a district boundary (by location, so it
# matches what the map shows whatever boundary set is in use). Returned in
# the same shape the old ArcGIS IDP fetch returned -- idp_name,
# idp_population, point geometry -- plus site_type and icon, so every
# existing consumer keeps working. types = NULL -> all site types.
# NULL if the registry can't be read at all (no Central, no saved copy).
registry_context_sf <- function(district_sf, types = NULL) {
  if (is.null(district_sf) || nrow(district_sf) == 0) return(NULL)
  s <- tryCatch(sites_registry_get()$sites, error = function(e) NULL)
  if (is.null(s)) return(NULL)
  s <- s[s$status != "removed" & s$site_type %in% (types %||% .EXPORT_SITE_STYLE$type), , drop = FALSE]
  s <- sites_with_coords(s)
  s <- s[!is.na(s$lat) & !is.na(s$lon), , drop = FALSE]
  empty <- sf::st_sf(idp_name = character(0), idp_population = numeric(0), site_type = character(0),
                     icon = character(0), geometry = sf::st_sfc(crs = 4326))
  if (!nrow(s)) return(empty)
  pts <- sf::st_as_sf(s, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(old_s2)))
  area <- sf::st_union(sf::st_make_valid(sf::st_transform(sf::st_geometry(district_sf), 4326)))
  inside <- lengths(suppressMessages(sf::st_intersects(pts, area))) > 0
  pts <- pts[inside, , drop = FALSE]
  if (!nrow(pts)) return(empty)
  out <- sf::st_sf(
    idp_name = pts$site_name,
    idp_population = suppressWarnings(as.numeric(pts$u5_population)),
    site_type = pts$site_type,
    icon = unname(.SITE_MARKER_URL[pts$site_type]),
    icon_px = site_icon_px(pts$site_type),
    lon = pts$lon, lat = pts$lat,
    geometry = sf::st_geometry(pts))
  out
}

# IDP settlements for a health-area version: the registry's IDP sites in
# the district, or the version's own saved snapshot (snap$idp_settlements,
# from the retired ArcGIS fetch) if the registry can't be read. Plain data
# frame with lon/lat, like the snapshot.
version_idp_points <- function(snap, district_name, campaign_id = NULL) {
  dsf <- tryCatch(districts_shp_for_campaign(campaign_id) |> dplyr::filter(district_name == !!district_name),
                  error = function(e) NULL)
  reg <- if (!is.null(dsf) && nrow(dsf)) tryCatch(registry_context_sf(dsf, types = "idp"), error = function(e) NULL)
  if (!is.null(reg)) return(sf::st_drop_geometry(reg[, c("idp_name", "idp_population", "lon", "lat")]))
  snap$idp_settlements
}

# All registry sites in a district, for downloads (data frame with lon/lat).
district_sites_df <- function(district_name, campaign_id = NULL) {
  dsf <- tryCatch(districts_shp_for_campaign(campaign_id) |> dplyr::filter(district_name == !!district_name),
                  error = function(e) NULL)
  if (is.null(dsf) || !nrow(dsf)) return(NULL)
  s <- tryCatch(sites_registry_get()$sites, error = function(e) NULL)
  reg <- tryCatch(registry_context_sf(dsf), error = function(e) NULL)
  if (is.null(reg) || !nrow(reg) || is.null(s)) return(NULL)
  s <- sites_with_coords(s[s$status != "removed", , drop = FALSE])
  s <- s[paste(s$lon, s$lat, s$site_name) %in% paste(reg$lon, reg$lat, reg$idp_name), , drop = FALSE]
  sub <- if (nrow(SITE_SUBTYPES)) setNames(SITE_SUBTYPES$label, SITE_SUBTYPES$name)[s$site_subtype] else NA
  data.frame(site_name = s$site_name, site_type = unname(SITE_TYPES[s$site_type]),
             kind = ifelse(is.na(sub), s$site_subtype, sub), crossing_name = s$crossing_name,
             u5_population = suppressWarnings(as.numeric(s$u5_population)),
             last_confirmed = s$last_confirmed_date, site_id = s$`__id`,
             lon = s$lon, lat = s$lat, stringsAsFactors = FALSE)
}

site_label <- function(site_name, site_type, district_name) {
  sprintf("%s - %s (%s)", site_name, SITE_TYPES[[site_type]] %||% site_type, district_name)
}

.site_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

# Replace/insert one row in the in-process cache after a successful write.
.sites_cache_put <- function(id, version, label, data) {
  s <- .sites_cache$sites
  if (is.null(s)) return(invisible())
  i <- match(id, s$`__id`)
  if (is.na(i)) { s[nrow(s) + 1, ] <- ""; i <- nrow(s); s$`__id`[i] <- id }
  s$`__version`[i] <- as.character(version); s$label[i] <- label
  for (k in names(data)) { if (!k %in% names(s)) s[[k]] <- ""; s[[k]][i] <- as.character(data[[k]] %||% "") }
  .sites_cache$sites <- s
  invisible()
}

# `data`: named list of site properties (strings). Returns the new entity id.
sites_registry_create <- function(data, username) {
  data <- c(data, list(status = "active", removal_reason = "", source = "app",
                       last_edited_by = username, last_edited_at = .site_now()))
  label <- site_label(data$site_name, data$site_type, data$district_name)
  r <- central_call(sprintf("/projects/%d/datasets/sites/entities", MICROPLAN_PROJECT), "POST",
                    jsonlite::toJSON(list(label = label, data = data), auto_unbox = TRUE))
  central_stop_unless(r, what = "create site")
  b <- jsonlite::fromJSON(r$body)
  .sites_cache_put(b$uuid, b$currentVersion$version %||% 1, label, data)
  b$uuid
}

# Returns "ok" or "conflict" (someone else changed the site since it was read).
sites_registry_update <- function(id, base_version, data, username) {
  data <- c(data, list(last_edited_by = username, last_edited_at = .site_now()))
  cur <- .sites_cache$sites[match(id, .sites_cache$sites$`__id`), , drop = FALSE]
  label <- site_label(data$site_name %||% cur$site_name, data$site_type %||% cur$site_type,
                      data$district_name %||% cur$district_name)
  r <- central_call(sprintf("/projects/%d/datasets/sites/entities/%s?baseVersion=%s",
                            MICROPLAN_PROJECT, id, base_version), "PATCH",
                    jsonlite::toJSON(list(label = label, data = data), auto_unbox = TRUE))
  if (r$status == 409) return("conflict")
  central_stop_unless(r, what = "update site")
  b <- jsonlite::fromJSON(r$body)
  .sites_cache_put(id, b$currentVersion$version %||% (as.integer(base_version) + 1L), label, data)
  "ok"
}

sites_registry_remove <- function(id, base_version, reason, username) {
  sites_registry_update(id, base_version, list(status = "removed", removal_reason = reason), username)
}

# MHFL mirror facilities in a district: named vector mhfl_uuid -> label.
site_mhfl_facilities <- function(district_code) {
  if (is.null(.sites_cache$mhfl) ||
      as.numeric(difftime(Sys.time(), .sites_cache$mhfl_at, units = "secs")) > 3600) {
    m <- tryCatch(central_entities(MICROPLAN_PROJECT, "master_hf_registry"), error = function(e) NULL)
    if (!is.null(m)) { .sites_cache$mhfl <- m; .sites_cache$mhfl_at <- Sys.time() }
  }
  m <- .sites_cache$mhfl
  if (is.null(m) || !nrow(m)) return(character(0))
  m <- m[m$district_code == district_code, , drop = FALSE]
  m <- m[order(m$facility_name), , drop = FALSE]
  setNames(m$mhfl_uuid, sprintf("%s (%s)", m$facility_name, m$facility_type))
}

site_mhfl_point <- function(mhfl_uuid) {
  m <- .sites_cache$mhfl
  if (is.null(m)) return(NULL)
  r <- m[m$mhfl_uuid == mhfl_uuid, , drop = FALSE]
  if (!nrow(r)) return(NULL)
  list(name = r$facility_name[1], lat = suppressWarnings(as.numeric(r$gps_latitude[1])),
       lon = suppressWarnings(as.numeric(r$gps_longitude[1])))
}
