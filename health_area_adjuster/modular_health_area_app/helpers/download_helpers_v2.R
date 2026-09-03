# =============================================================================
# download_helpers_v2.R
#
# Replaces build_district_download() for the v2 data model. Separate file
# (additive) rather than editing download_helpers.R in place, since the v1
# function is built entirely around planning_data (microplan), which no
# longer exists.
#
# Output files (any that have no data for a given district are simply
# omitted from the zip, same behavior as v1):
#   health_areas.geojson       — health area polygons (smoothed presentation
#                                 geometry if available, else raw)
#   team_areas.geojson         — team area polygons, same basis
#   idp_settlements.geojson    — IDP settlement points
#   landmarks.geojson          — landmark points
#   facilities.csv             — all facility records
# =============================================================================

#' Build one district's v2 download package (one shared mapping_versions row).
#'
#' @param file           output zip path (as passed by a Shiny downloadHandler)
#' @param district_name  character
#' @param zone, region   character, for attribute columns
#' @param version        a single parsed version list, as returned by
#'                        db_get_version_by_id() / db_get_shared_version()
#'                        (i.e. has a `$snap` element with saved_dfa_sf,
#'                        smoothed_dfa_sf, saved_team_sf, smoothed_team_sf,
#'                        idp_settlements, landmarks, app_sf/odk_sf)
build_district_download_v2 <- function(file, district_name, zone = '', region = '', version) {

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  snap <- version$snap %||% list()

  .write_sf <- function(sf_obj, fname) {
    if (is.null(sf_obj) || nrow(sf_obj) == 0) return(invisible(NULL))
    out <- sf_obj |>
      dplyr::mutate(zone_name = zone, region_name = region, district_name = district_name) |>
      sf::st_transform(4326)
    sf::st_write(out, file.path(tmp, fname), driver = 'GeoJSON', delete_dsn = TRUE, quiet = TRUE)
  }

  # Prefer smoothed/presentation geometry for exports — raw grid-stepped
  # polygons look unprofessional outside the app; fall back to raw if no
  # smoothed geometry was generated yet (e.g. version never submitted).
  ha_sf   <- snap$smoothed_dfa_sf  %||% snap$saved_dfa_sf
  team_sf <- snap$smoothed_team_sf %||% snap$saved_team_sf

  .write_sf(ha_sf,   'health_areas.geojson')
  .write_sf(team_sf, 'team_areas.geojson')

  # IDP settlements / landmarks are stored as plain data frames with lon/lat,
  # not sf — convert if present.
  .write_points_df <- function(df, fname) {
    if (is.null(df) || nrow(df) == 0 || !all(c('lon', 'lat') %in% names(df))) return(invisible(NULL))
    pts <- sf::st_as_sf(df, coords = c('lon', 'lat'), crs = 4326, remove = FALSE) |>
      dplyr::mutate(zone_name = zone, region_name = region, district_name = district_name)
    sf::st_write(pts, file.path(tmp, fname), driver = 'GeoJSON', delete_dsn = TRUE, quiet = TRUE)
  }
  .write_points_df(snap$idp_settlements, 'idp_settlements.geojson')
  .write_points_df(snap$landmarks,       'landmarks.geojson')

  # Facilities — combine locked ODK snapshot + user-added sites, same as
  # server.R does when restoring a session.
  fac_parts <- Filter(function(x) inherits(x, 'sf') && nrow(x) > 0,
                      list(snap$odk_sf, snap$app_sf))
  if (length(fac_parts) > 0) {
    fac_sf <- do.call(rbind, fac_parts)
    fac_out <- sf::st_drop_geometry(fac_sf) |>
      dplyr::mutate(
        zone_name = zone, region_name = region, district_name = district_name,
        lon = sf::st_coordinates(fac_sf)[, 1], lat = sf::st_coordinates(fac_sf)[, 2]
      ) |>
      dplyr::select(zone_name, region_name, district_name,
                    dplyr::any_of(c('facility_id', 'facility_name', 'facility_type',
                                    'hf_ownership', 'polio_sia_coordination_site',
                                    'operational', 'lat', 'lon')))
    write.csv(fac_out, file.path(tmp, 'facilities.csv'), row.names = FALSE)
  }

  out_files <- list.files(tmp, full.names = TRUE)
  if (length(out_files) == 0) {
    write.csv(data.frame(message = 'No data available.'), file, row.names = FALSE)
    return(invisible(NULL))
  }
  tryCatch(
    zip::zip(zipfile = file, files = out_files, mode = 'cherry-pick'),
    error = function(e) zip(zipfile = file, files = out_files, flags = '-j')
  )
  invisible(NULL)
}

#' Build a zip-of-zips: every district's currently shared version for one
#' campaign, combined. Used by the admin "Download all data" action.
#'
#' @param campaign_id  integer
build_campaign_download_v2 <- function(file, campaign_id) {
  progress <- tryCatch(db_get_campaign_progress(pool, campaign_id), error = function(e) NULL)
  if (is.null(progress) || nrow(progress) == 0) {
    write.csv(data.frame(message = 'No published districts for this campaign.'), file, row.names = FALSE)
    return(invisible(NULL))
  }

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  for (i in seq_len(nrow(progress))) {
    dname   <- progress$district_name[i]
    ver_id  <- progress$version_id[i]
    version <- tryCatch(db_get_version_by_id(pool, ver_id), error = function(e) NULL)
    if (is.null(version)) next

    dinfo <- districts_shp |> dplyr::filter(district_name == dname)
    zone_val   <- if (nrow(dinfo) > 0) as.character(dinfo$zone_name[1])   else ''
    region_val <- if (nrow(dinfo) > 0) as.character(dinfo$region_name[1]) else ''

    slug     <- gsub('[^A-Za-z0-9]', '_', tolower(trimws(dname)))
    sub_zip  <- file.path(tmp, paste0(slug, '.zip'))
    tryCatch(
      build_district_download_v2(sub_zip, dname, zone_val, region_val, version),
      error = function(e) cat('[download] failed for', dname, ':', e$message, '\n')
    )
  }

  out_files <- list.files(tmp, full.names = TRUE, pattern = '\\.zip$')
  if (length(out_files) == 0) {
    write.csv(data.frame(message = 'No data available.'), file, row.names = FALSE)
    return(invisible(NULL))
  }
  tryCatch(
    zip::zip(zipfile = file, files = out_files, mode = 'cherry-pick'),
    error = function(e) zip(zipfile = file, files = out_files, flags = '-j')
  )
  invisible(NULL)
}
