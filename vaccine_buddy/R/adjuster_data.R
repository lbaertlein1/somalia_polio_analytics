# =============================================================================
# adjuster_data.R -- READ-ONLY access to the health area adjuster database
#
# Mirrors the "current/published data only" reads in the adjuster's
# helpers/mod_db_v2.R and helpers/download_helpers_v2.R:
#   - health areas: the one is_shared, non-archived mapping_versions row
#     per (campaign, district); smoothed geometry preferred over the raw
#     grid, as in the adjuster's exports.
#   - team areas:   the is_shared team_area_versions row per health area,
#     kept only if it is pinned to that district's CURRENT health-area
#     version (otherwise it was drawn against an old boundary).
#   - IDP sites and facilities: the snapshots locked into the health-area
#     version (idp_settlements; odk_sf + app_sf), so supervisors see exactly
#     what the area planners saw.
# Polygons in both tables name the area in a `dfa_name` column.
# =============================================================================

adj_connect <- function() {
  pool::dbPool(
    drv      = RPostgres::Postgres(),
    host     = env_first(c('ADJ_DB_HOST', 'DB_HOST')),
    port     = as.integer(env_first(c('ADJ_DB_PORT', 'DB_PORT'), '5432')),
    dbname   = env_first(c('ADJ_DB_NAME', 'DB_NAME'), 'somalia_health_areas_v2'),
    user     = env_first(c('ADJ_DB_USER', 'DB_USER')),
    password = env_first(c('ADJ_DB_PASSWORD', 'DB_PASSWORD')),
    sslmode  = env_first(c('ADJ_DB_SSL', 'DB_SSL'), 'require'),
    minSize = 1L, maxSize = 3L, idleTimeout = 300000L
  )
}


adj_get_campaigns <- function() {
  DBI::dbGetQuery(adj_pool, "
    SELECT campaign_id, campaign_name, start_date
    FROM campaigns WHERE is_active
    ORDER BY start_date DESC NULLS LAST, created_at DESC")
}

.geojson_to_sf <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NULL)
  out <- tryCatch(geojsonsf::geojson_sf(x), error = function(e) NULL)
  if (is.null(out) || nrow(out) == 0) return(NULL)
  sf::st_make_valid(sf::st_transform(out, 4326))
}

.json_to_df <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(as.data.frame(jsonlite::fromJSON(x, simplifyDataFrame = TRUE), stringsAsFactors = FALSE),
           error = function(e) NULL)
}

#' Everything this app needs about one campaign, as sf objects in EPSG:4326:
#'   health_areas  district_name, health_area
#'   team_areas    district_name, health_area, team_area
#'   facilities    district_name, facility_name, facility_type, ...
#'   idp           district_name, idp_name, idp_population
#'   districts     district_name (district boundaries)
adj_load_campaign <- function(campaign_id) {
  cid <- as.integer(campaign_id)

  mv <- DBI::dbGetQuery(adj_pool, "
    SELECT version_id, district_name,
           COALESCE(smoothed_dfa_sf, saved_dfa_sf)::text AS ha_geojson,
           idp_settlements::text AS idp_json,
           odk_sf::text AS odk_geojson, app_sf::text AS app_geojson
    FROM mapping_versions
    WHERE campaign_id = $1 AND is_shared AND archived_at IS NULL
    ORDER BY district_name", list(cid))

  tv <- DBI::dbGetQuery(adj_pool, "
    SELECT tav.district_name, tav.health_area_name,
           COALESCE(tav.smoothed_team_sf, tav.saved_team_sf)::text AS team_geojson
    FROM team_area_versions tav
    JOIN mapping_versions mv
      ON mv.version_id = tav.based_on_health_area_version_id
     AND mv.is_shared AND mv.archived_at IS NULL
    WHERE tav.campaign_id = $1 AND tav.is_shared AND tav.archived_at IS NULL", list(cid))

  ha <- lapply(seq_len(nrow(mv)), function(i) {
    g <- .geojson_to_sf(mv$ha_geojson[i])
    if (is.null(g) || !'dfa_name' %in% names(g)) return(NULL)
    g |>
      dplyr::filter(!dfa_name %in% NON_AREA_NAMES) |>
      dplyr::transmute(district_name = mv$district_name[i], health_area = as.character(dfa_name))
  })

  teams <- lapply(seq_len(nrow(tv)), function(i) {
    g <- .geojson_to_sf(tv$team_geojson[i])
    if (is.null(g) || !'dfa_name' %in% names(g)) return(NULL)
    g |>
      dplyr::filter(!dfa_name %in% NON_AREA_NAMES) |>
      dplyr::transmute(district_name = tv$district_name[i], health_area = tv$health_area_name[i],
                       team_area = as.character(dfa_name))
  })

  idp <- lapply(seq_len(nrow(mv)), function(i) {
    d <- .json_to_df(mv$idp_json[i])
    if (is.null(d) || nrow(d) == 0 || !all(c('lon', 'lat') %in% names(d))) return(NULL)
    d$district_name <- mv$district_name[i]
    d$idp_name <- trimws(as.character(d$idp_name %||% paste('IDP site', seq_len(nrow(d)))))
    d$idp_population <- suppressWarnings(as.numeric(d$idp_population %||% NA))
    sf::st_as_sf(d[!is.na(d$lon) & !is.na(d$lat), c('district_name', 'idp_name', 'idp_population', 'lon', 'lat')],
                 coords = c('lon', 'lat'), crs = 4326)
  })

  fac_cols <- c('facility_id', 'facility_name', 'facility_type', 'polio_sia_coordination_site', 'operational')
  fac <- lapply(seq_len(nrow(mv)), function(i) {
    parts <- Filter(Negate(is.null), list(.geojson_to_sf(mv$odk_geojson[i]), .geojson_to_sf(mv$app_geojson[i])))
    if (length(parts) == 0) return(NULL)
    dplyr::bind_rows(parts) |>
      dplyr::select(dplyr::any_of(fac_cols)) |>
      dplyr::mutate(district_name = mv$district_name[i], .before = 1)
  })

  .bind <- function(parts) { parts <- Filter(Negate(is.null), parts); if (length(parts) == 0) NULL else dplyr::bind_rows(parts) }
  ha <- .bind(ha)

  # District boundaries of the campaign's districts: those published, plus
  # those added to the campaign in the adjuster with no health areas drawn
  # yet -- or, for a campaign not set up in the adjuster at all, every
  # district. Districts without health areas use the team names instead.
  cd <- tryCatch(DBI::dbGetQuery(adj_pool, 'SELECT district_name FROM campaign_districts WHERE campaign_id = $1',
                                 list(cid))$district_name, error = function(e) character(0))
  districts <- tryCatch(adj_district_shapes(cid), error = function(e) { message('[districts] ', conditionMessage(e)); NULL })
  want <- base::union(mv$district_name, cd)
  if (!is.null(districts) && length(want)) districts <- districts[districts$district_name %in% want, ]

  # Versions published without an IDP snapshot (has_idp = FALSE): fetch
  # live from the same admin-configured source the adjuster uses.
  for (i in which(vapply(idp, is.null, logical(1)))) {
    d_ha <- ha[ha$district_name == mv$district_name[i], ]
    if (nrow(d_ha) > 0) idp[[i]] <- tryCatch(adj_idp_live(cid, mv$district_name[i], d_ha),
                                             error = function(e) { message('[idp] ', mv$district_name[i], ': ', conditionMessage(e)); NULL })
  }

  list(
    health_areas = ha,
    team_areas   = .bind(teams),
    facilities   = .bind(fac),
    idp          = .bind(idp),
    districts    = districts
  )
}

#' The campaign's district boundary set (campaigns.boundary_set, else the
#' adjuster's current set), as sf(district_name) in EPSG:4326, or NULL.
adj_district_shapes <- function(campaign_id) {
  set <- tryCatch(DBI::dbGetQuery(adj_pool, 'SELECT boundary_set FROM campaigns WHERE campaign_id = $1',
                                  list(as.integer(campaign_id)))$boundary_set, error = function(e) NA_character_)
  if (length(set) == 0 || is.na(set[1]) || !nzchar(set[1]))
    set <- tryCatch(DBI::dbGetQuery(adj_pool, 'SELECT set_label FROM boundary_sets WHERE is_current')$set_label,
                    error = function(e) character(0))
  # A set not copied into BOUNDARY_DIR: the newest one that is
  have <- base::sort(list.dirs(BOUNDARY_DIR, full.names = FALSE, recursive = FALSE), decreasing = TRUE)
  have <- have[file.exists(file.path(BOUNDARY_DIR, have, 'districts_shp.Rds'))]
  if (length(set) == 0 || !set[1] %in% have) set <- have
  if (length(set) == 0) { message('[districts] no boundary sets in ', BOUNDARY_DIR); return(NULL) }
  .read_districts(set[1])
}

.read_districts <- memoise::memoise(function(set_label) {
  d <- readRDS(file.path(BOUNDARY_DIR, set_label, 'districts_shp.Rds'))
  sf::st_make_valid(sf::st_transform(d[, 'district_name'], 4326))
})

#' IDP sites from the adjuster's configured ArcGIS layer
#' (data_source_settings.idp_settlements_url, campaign override first),
#' within 500 m of the district's health areas. Field names are the IDP
#' Site Master List's (Settlement, IDP_Indivi). f=json lets GDAL's ESRIJSON
#' driver page through layers larger than the server's record limit.
adj_idp_live <- function(campaign_id, district_name, district_ha) {
  url <- DBI::dbGetQuery(adj_pool, "
    SELECT setting_value FROM data_source_settings
    WHERE setting_key = 'idp_settlements_url' AND (campaign_id = $1 OR campaign_id IS NULL)
    ORDER BY campaign_id NULLS LAST LIMIT 1", list(as.integer(campaign_id)))$setting_value
  if (length(url) == 0 || !nzchar(url)) return(NULL)
  area <- sf::st_buffer(sf::st_union(sf::st_transform(district_ha, 3857)), 500) |> sf::st_transform(4326)
  bb <- sf::st_bbox(area)
  q <- httr::modify_url(url, query = list(
    where = '1=1', outFields = 'Settlement,IDP_Indivi', returnGeometry = 'true', outSR = '4326', f = 'json',
    geometry = sprintf('%f,%f,%f,%f', bb['xmin'], bb['ymin'], bb['xmax'], bb['ymax']),
    geometryType = 'esriGeometryEnvelope', inSR = '4326', spatialRel = 'esriSpatialRelIntersects'))
  x <- sf::st_read(q, quiet = TRUE)
  if (nrow(x) == 0) return(NULL)
  x <- sf::st_transform(x, 4326)
  x <- x[lengths(sf::st_intersects(x, area)) > 0, ]
  if (nrow(x) == 0) return(NULL)
  sf::st_sf(district_name = district_name, idp_name = trimws(as.character(x$Settlement)),
            idp_population = suppressWarnings(as.numeric(x$IDP_Indivi)), geometry = sf::st_geometry(x))
}

# Published boundaries change rarely and every supervisor page open needs
# them, so they're cached per R process for 30 minutes.
adj_load_campaign_cached <- memoise::memoise(adj_load_campaign, cache = cachem::cache_mem(max_age = 30 * 60))

#' One health area's slice of adj_load_campaign_cached().
adj_area_layers <- function(campaign_id, district_name, health_area) {
  a <- adj_load_campaign_cached(campaign_id)
  pick <- function(x, by_ha = TRUE) {
    if (is.null(x)) return(NULL)
    x <- x[x$district_name == district_name, ]
    if (by_ha && 'health_area' %in% names(x)) x <- x[x$health_area == health_area, ]
    x
  }
  ha <- pick(a$health_areas)
  # Facilities and IDP sites: the area plus a margin, so ones just across
  # the boundary still show as landmarks.
  near <- function(x) {
    x <- pick(x, by_ha = FALSE)
    if (is.null(x) || nrow(x) == 0 || is.null(ha) || nrow(ha) == 0) return(x)
    x[sf::st_intersects(x, sf::st_as_sfc(sf::st_bbox(ha) + c(-0.01, -0.01, 0.01, 0.01)), sparse = FALSE)[, 1], ]
  }
  list(health_area = ha, team_areas = pick(a$team_areas), facilities = near(a$facilities), idp = near(a$idp))
}
