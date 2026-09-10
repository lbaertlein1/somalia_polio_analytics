# =============================================================================
# download_helpers_v2.R
#
# Replaces build_district_download() for the v2 data model. Separate file
# (additive) rather than editing download_helpers.R in place, since the v1
# function is built entirely around planning_data (microplan), which no
# longer exists.
#
# Team-area rework: team areas are now their own table
# (team_area_versions), one independently-current version PER health area
# — NOT a field on the health-area version's own snapshot anymore. A
# district's team-area export has to be assembled by combining every
# currently-shared team_area_versions row across all its health areas,
# not read off version$snap the way health-area/landmark/IDP/facility data
# still can be.
#
# Format support: GeoJSON (default), ESRI Shapefile, and KML, selected via
# a driver/extension pair rather than three near-duplicate functions.
# Shapefiles are inherently multi-file (.shp/.shx/.dbf/.prj) — no special
# handling needed here, since the existing zip step already just collects
# every file in the temp directory regardless of how many one layer wrote.
#
# Output files (any that have no data for a given district are simply
# omitted from the zip, same behavior as v1):
#   health_areas.<ext>       — health area polygons (smoothed presentation
#                               geometry if available, else raw)
#   team_areas.<ext>         — team area polygons, same basis, combined
#                               across every health area's current version
#   idp_settlements.<ext>    — IDP settlement points
#   landmarks.<ext>          — landmark points
#   facilities.csv           — all facility records
#   health_area_population_summary.csv — one row per health area: WorldPop
#                               population, field target population,
#                               number of teams
#   team_population_summary.csv — one row per team: WorldPop population,
#                               implied field target share
# =============================================================================

#' write.csv() writes in the system's native encoding by default, with no
#' UTF-8 byte-order mark -- Excel then falls back to guessing the
#' encoding from the system locale, and misreads any non-ASCII character
#' as garbled text. The only place that currently bites is the Unicode
#' en-dash .fmt_field() (printable_export.R) uses for NA/blank field-
#' target values, but facility names can plausibly contain non-ASCII
#' text too, so every CSV in this file goes through this instead of
#' plain write.csv() -- cheap insurance, not just a spot fix for the one
#' symptom that happened to get noticed.
.write_csv_utf8_bom <- function(x, path, row.names = FALSE, ...) {
  con <- file(path, open = 'wb')
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw('\xef\xbb\xbf'), con)
  write.csv(x, con, row.names = row.names, ...)
}

#' Map a user-facing format choice to its sf driver name and file extension.
#' Kept in one place so the export UI and the zip-builders always agree on
#' what "shp"/"kml"/"geojson" mean.
export_format_spec <- function(format = c('geojson', 'shp', 'kml')) {
  format <- match.arg(format)
  switch(format,
    geojson = list(driver = 'GeoJSON',          ext = 'geojson'),
    shp     = list(driver = 'ESRI Shapefile',    ext = 'shp'),
    kml     = list(driver = 'KML',               ext = 'kml')
  )
}

#' Build one district's v2 download package (one shared mapping_versions
#' row for health areas, plus every current team_area_versions row across
#' its health areas).
#'
#' @param file           output zip path (as passed by a Shiny downloadHandler)
#' @param district_name  character
#' @param zone, region   character, for attribute columns
#' @param version        a single parsed HEALTH-AREA version list, as
#'                        returned by db_get_version_by_id() /
#'                        db_get_shared_version() (has a `$snap` element
#'                        with saved_dfa_sf, smoothed_dfa_sf, dfa_names,
#'                        idp_settlements, landmarks, app_sf/odk_sf —
#'                        NOT team-area fields, those live in
#'                        team_area_versions now).
#' @param campaign_id    integer — needed to look up each health area's
#'                        current team-area version independently.
#' @param format         'geojson' (default), 'shp', or 'kml'.
build_district_download_v2 <- function(file, district_name, zone = '', region = '',
                                       version, campaign_id, format = 'geojson') {

  spec <- export_format_spec(format)

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  snap <- version$snap %||% list()

  .write_sf <- function(sf_obj, fname) {
    if (is.null(sf_obj) || nrow(sf_obj) == 0) return(invisible(NULL))
    out <- sf_obj |>
      dplyr::mutate(zone_name = zone, region_name = region, district_name = district_name) |>
      sf::st_transform(4326)
    sf::st_write(out, file.path(tmp, fname), driver = spec$driver, delete_dsn = TRUE, quiet = TRUE)
  }

  # Prefer smoothed/presentation geometry for exports — raw grid-stepped
  # polygons look unprofessional outside the app; fall back to raw if no
  # smoothed geometry was generated yet (e.g. version never submitted).
  ha_sf <- snap$smoothed_dfa_sf %||% snap$saved_dfa_sf

  # Team areas: combine every health area's CURRENT team-area version —
  # there is no single "the district's team areas" anymore, only
  # per-health-area ones, each independently current or not.
  team_sf <- tryCatch({
    ha_names <- setdiff(unlist(snap$dfa_names %||% list()), c('Inaccessible', 'Unpopulated'))
    parts <- lapply(ha_names, function(han) {
      tv <- db_get_current_team_area_version(pool, campaign_id, district_name, han)
      if (is.null(tv)) return(NULL)
      geom <- tv$snap$smoothed_team_sf %||% tv$snap$saved_team_sf
      if (is.null(geom) || nrow(geom) == 0) return(NULL)
      geom$health_area <- han
      geom
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0) NULL else do.call(rbind, parts)
  }, error = function(e) NULL)

  .write_sf(ha_sf,   paste0('health_areas.', spec$ext))
  .write_sf(team_sf, paste0('team_areas.', spec$ext))

  # Subdivisions and campaign/urban extent, as their own data layers --
  # matching what the printable PDF export now also draws on its maps
  # (see printable_export.R), so someone opening this boundary export in
  # GIS software gets the same context layers, not just health/team
  # areas. Looked up from districts_shp by name, NOT snap$district_
  # boundary_sf -- that field is a pre-existing, apparently-never-
  # populated one (confirmed by checking every place it's written to;
  # nothing in the app ever sets it), so relying on it here would
  # silently skip these layers for every export. districts_shp is the
  # same reliable source mod_facility_tab.R's own district_base() and
  # printable_export.R's own zone/region lookup already trust.
  district_sf_for_fetch <- tryCatch(
    districts_shp |> dplyr::filter(district_name == !!district_name) |>
      dplyr::group_by(district_name) |> dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
      sf::st_as_sf() |> safe_make_valid() |> sf::st_transform(4326),
    error = function(e) NULL
  )
  subdiv_sf_export <- if (!is.null(district_sf_for_fetch))
    tryCatch(fetch_subdivisions_for_district(district_sf_for_fetch), error = function(e) NULL) else NULL
  urban_sf_export <- if (!is.null(district_sf_for_fetch))
    tryCatch(fetch_campaign_extent_for_district(district_sf_for_fetch, campaign_id), error = function(e) NULL) else NULL
  .write_sf(subdiv_sf_export, paste0('subdivisions.', spec$ext))
  .write_sf(urban_sf_export,  paste0('campaign_extent.', spec$ext))

  # Population/team-count summary CSVs -- reuses the exact same table
  # builders the printable PDF export already uses (printable_export.R),
  # rather than a second implementation of the same WorldPop-extraction +
  # field-target logic that could drift out of sync with the PDF's own
  # numbers. u5_rast is the app's global WorldPop raster, loaded once at
  # startup -- not passed as a parameter here since this function follows
  # the same implicit-global convention printable_export.R's own callers
  # already rely on.
  tryCatch({
    if (!is.null(ha_sf) && nrow(ha_sf) > 0) {
      team_targets <- snap$team_targets %||% list()
      # Reuses urban_sf_export (fetched once, above) rather than a
      # second fetch_campaign_extent_for_district() call for the same
      # district/campaign.
      urban_sf <- urban_sf_export
      ha_summary <- .build_health_area_summary_table(ha_sf, team_targets, u5_rast, campaign_id, urban_sf)
      if (!is.null(ha_summary) && nrow(ha_summary) > 0) {
        .write_csv_utf8_bom(cbind(district_name = district_name, ha_summary),
                 file.path(tmp, 'health_area_population_summary.csv'), row.names = FALSE)
      }

      if (!is.null(team_sf) && nrow(team_sf) > 0) {
        team_rows <- lapply(unique(team_sf$health_area), function(han) {
          team_one <- team_sf[team_sf$health_area == han, , drop = FALSE]
          tgt <- team_targets[[han]]
          field_target <- .unwrap_num(tgt$target_pop)
          tbl <- .build_team_summary_table(team_one, field_target, u5_rast, urban_sf)
          if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
          cbind(district_name = district_name, `Health Area` = han, tbl, check.names = FALSE)
        })
        team_rows <- Filter(Negate(is.null), team_rows)
        if (length(team_rows) > 0) {
          .write_csv_utf8_bom(do.call(rbind, team_rows),
                   file.path(tmp, 'team_population_summary.csv'), row.names = FALSE)
        }
      }
    }
  }, error = function(e) {
    warning(paste('Population summary export failed, continuing without it:', e$message))
  })

  # IDP settlements / landmarks are stored as plain data frames with lon/lat,
  # not sf — convert if present.
  .write_points_df <- function(df, fname) {
    if (is.null(df) || nrow(df) == 0 || !all(c('lon', 'lat') %in% names(df))) return(invisible(NULL))
    pts <- sf::st_as_sf(df, coords = c('lon', 'lat'), crs = 4326, remove = FALSE) |>
      dplyr::mutate(zone_name = zone, region_name = region, district_name = district_name)
    sf::st_write(pts, file.path(tmp, fname), driver = spec$driver, delete_dsn = TRUE, quiet = TRUE)
  }
  .write_points_df(snap$idp_settlements, paste0('idp_settlements.', spec$ext))
  .write_points_df(snap$landmarks,       paste0('landmarks.', spec$ext))

  # Facilities — combine locked ODK snapshot + user-added sites, same as
  # server.R does when restoring a session. Always CSV, regardless of
  # boundary format — a facility list isn't geometry-format-dependent.
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
    .write_csv_utf8_bom(fac_out, file.path(tmp, 'facilities.csv'), row.names = FALSE)
  }

  out_files <- list.files(tmp, full.names = TRUE)
  if (length(out_files) == 0) {
    .write_csv_utf8_bom(data.frame(message = 'No data available.'), file, row.names = FALSE)
    return(invisible(NULL))
  }
  tryCatch(
    zip::zip(zipfile = file, files = out_files, mode = 'cherry-pick'),
    error = function(e) zip(zipfile = file, files = out_files, flags = '-j')
  )
  invisible(NULL)
}

#' Build ONE stitched-together package for a whole campaign: a single
#' health_areas layer and a single team_areas layer spanning every
#' district (not a zip-of-per-district-zips, which is what this used to
#' produce), plus a districts layer covering every district in
#' districts_shp with an in_campaign flag marking which ones are
#' actually assigned to this campaign. IDP/landmarks/facilities are
#' stitched the same way, for the same reason build_district_download_v2()
#' includes them at all -- omitting them here would make the campaign
#' export silently lose data that the per-district export still has.
#'
#' @param file           output zip path
#' @param campaign_id    integer
#' @param format         'geojson' (default), 'shp', or 'kml'
#' @param progress_callback optional function(i, n, district_name) called
#'                       once per district as it's processed -- lets a
#'                       caller (e.g. Shiny's withProgress/incProgress)
#'                       show real per-district progress on what can be a
#'                       long loop, rather than a single opaque "working".
#'                       Never required; a plain function() {} default is
#'                       used when the caller doesn't need progress.
build_campaign_download_v2 <- function(file, campaign_id, format = 'geojson', progress_callback = function(i, n, district_name) {}) {
  spec <- export_format_spec(format)

  progress <- tryCatch(db_get_campaign_progress(pool, campaign_id), error = function(e) NULL)
  if (is.null(progress) || nrow(progress) == 0) {
    .write_csv_utf8_bom(data.frame(message = 'No published districts for this campaign.'), file, row.names = FALSE)
    return(invisible(NULL))
  }

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  ha_parts       <- list()
  team_parts     <- list()
  idp_parts      <- list()
  landmark_parts <- list()
  fac_parts      <- list()
  subdiv_parts   <- list()
  urban_parts    <- list()
  ha_summary_parts   <- list()
  team_summary_parts <- list()

  for (i in seq_len(nrow(progress))) {
    dname   <- progress$district_name[i]
    progress_callback(i, nrow(progress), dname)
    ver_id  <- progress$version_id[i]
    version <- tryCatch(db_get_version_by_id(pool, ver_id), error = function(e) NULL)
    if (is.null(version)) next

    dinfo <- districts_shp |> dplyr::filter(district_name == dname)
    zone_val   <- if (nrow(dinfo) > 0) as.character(dinfo$zone_name[1])   else ''
    region_val <- if (nrow(dinfo) > 0) as.character(dinfo$region_name[1]) else ''

    snap <- version$snap %||% list()

    # Subdivisions and campaign/urban extent for THIS district -- same
    # per-district fetch as the single-district export path above, same
    # reasoning for using districts_shp rather than snap$district_
    # boundary_sf (see that path's own comment for why).
    dsf_for_fetch <- if (nrow(dinfo) > 0) tryCatch(
      dinfo |> dplyr::group_by(district_name) |> dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
        sf::st_as_sf() |> safe_make_valid() |> sf::st_transform(4326),
      error = function(e) NULL
    ) else NULL
    subdiv_d <- if (!is.null(dsf_for_fetch)) tryCatch(fetch_subdivisions_for_district(dsf_for_fetch), error = function(e) NULL) else NULL
    urban_d  <- if (!is.null(dsf_for_fetch)) tryCatch(fetch_campaign_extent_for_district(dsf_for_fetch, campaign_id), error = function(e) NULL) else NULL
    if (!is.null(subdiv_d) && nrow(subdiv_d) > 0)
      subdiv_parts[[dname]] <- subdiv_d |> dplyr::mutate(zone_name = zone_val, region_name = region_val, district_name = dname)
    if (!is.null(urban_d) && nrow(urban_d) > 0)
      urban_parts[[dname]] <- urban_d |> dplyr::mutate(zone_name = zone_val, region_name = region_val, district_name = dname)

    # Health areas -- same smoothed-preferred-over-raw logic as the
    # per-district export, each district's piece tagged and reprojected
    # to a common CRS (4326) before collecting, since sf requires a
    # single matching CRS across everything eventually combined.
    ha_sf <- snap$smoothed_dfa_sf %||% snap$saved_dfa_sf
    if (!is.null(ha_sf) && nrow(ha_sf) > 0) {
      ha_parts[[dname]] <- ha_sf |>
        dplyr::mutate(zone_name = zone_val, region_name = region_val, district_name = dname) |>
        sf::st_transform(4326)
    }

    # Team areas -- same per-health-area combination as the per-district
    # export (there is no single "this district's team areas" row,
    # only independently-current ones per health area).
    team_sf <- tryCatch({
      ha_names <- setdiff(unlist(snap$dfa_names %||% list()), c('Inaccessible', 'Unpopulated'))
      tparts <- lapply(ha_names, function(han) {
        tv <- db_get_current_team_area_version(pool, campaign_id, dname, han)
        if (is.null(tv)) return(NULL)
        geom <- tv$snap$smoothed_team_sf %||% tv$snap$saved_team_sf
        if (is.null(geom) || nrow(geom) == 0) return(NULL)
        geom$health_area <- han
        geom
      })
      tparts <- Filter(Negate(is.null), tparts)
      if (length(tparts) == 0) NULL else dplyr::bind_rows(tparts)
    }, error = function(e) NULL)
    if (!is.null(team_sf) && nrow(team_sf) > 0) {
      team_parts[[dname]] <- team_sf |>
        dplyr::mutate(zone_name = zone_val, region_name = region_val, district_name = dname) |>
        sf::st_transform(4326)
    }

    if (!is.null(snap$idp_settlements) && nrow(snap$idp_settlements) > 0 &&
        all(c('lon', 'lat') %in% names(snap$idp_settlements))) {
      idp_parts[[dname]] <- sf::st_as_sf(snap$idp_settlements, coords = c('lon', 'lat'), crs = 4326, remove = FALSE) |>
        dplyr::mutate(zone_name = zone_val, region_name = region_val, district_name = dname)
    }
    if (!is.null(snap$landmarks) && nrow(snap$landmarks) > 0 &&
        all(c('lon', 'lat') %in% names(snap$landmarks))) {
      landmark_parts[[dname]] <- sf::st_as_sf(snap$landmarks, coords = c('lon', 'lat'), crs = 4326, remove = FALSE) |>
        dplyr::mutate(zone_name = zone_val, region_name = region_val, district_name = dname)
    }

    fparts <- Filter(function(x) inherits(x, 'sf') && nrow(x) > 0, list(snap$odk_sf, snap$app_sf))
    if (length(fparts) > 0) {
      fsf <- dplyr::bind_rows(fparts)
      fac_parts[[dname]] <- sf::st_drop_geometry(fsf) |>
        dplyr::mutate(
          zone_name = zone_val, region_name = region_val, district_name = dname,
          lon = sf::st_coordinates(fsf)[, 1], lat = sf::st_coordinates(fsf)[, 2]
        )
    }

    # Same population/team-count summary as the single-district export,
    # accumulated per district in this same loop rather than a separate
    # pass -- keeps ha_sf/team_sf/team_targets in scope from this
    # iteration instead of re-fetching each district's version a second
    # time after the loop.
    tryCatch({
      if (!is.null(ha_sf) && nrow(ha_sf) > 0) {
        team_targets <- snap$team_targets %||% list()
        # Reuses urban_d (fetched once per district, above) rather than
        # a second fetch_campaign_extent_for_district() call.
        urban_sf <- urban_d
        ha_summary <- .build_health_area_summary_table(ha_sf, team_targets, u5_rast, campaign_id, urban_sf)
        if (!is.null(ha_summary) && nrow(ha_summary) > 0) {
          ha_summary_parts[[dname]] <- cbind(district_name = dname, ha_summary)
        }
        if (!is.null(team_sf) && nrow(team_sf) > 0) {
          d_team_rows <- lapply(unique(team_sf$health_area), function(han) {
            team_one <- team_sf[team_sf$health_area == han, , drop = FALSE]
            tgt <- team_targets[[han]]
            field_target <- .unwrap_num(tgt$target_pop)
            tbl <- .build_team_summary_table(team_one, field_target, u5_rast, urban_sf)
            if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
            cbind(district_name = dname, `Health Area` = han, tbl, check.names = FALSE)
          })
          d_team_rows <- Filter(Negate(is.null), d_team_rows)
          if (length(d_team_rows) > 0) team_summary_parts[[dname]] <- do.call(rbind, d_team_rows)
        }
      }
    }, error = function(e) {
      warning(paste('Population summary failed for', dname, '-- continuing without it:', e$message))
    })
  }

  # dplyr::bind_rows() rather than do.call(rbind, ...) -- different
  # districts' pieces can plausibly have slightly different column sets
  # (e.g. one district never had any Inaccessible cells drawn), and
  # bind_rows() fills missing columns with NA instead of erroring.
  .write_combined_sf <- function(parts, fname) {
    if (length(parts) == 0) return(invisible(NULL))
    combined <- dplyr::bind_rows(parts)
    if (nrow(combined) == 0) return(invisible(NULL))
    sf::st_write(combined, file.path(tmp, fname), driver = spec$driver, delete_dsn = TRUE, quiet = TRUE)
  }
  .write_combined_sf(ha_parts,       paste0('health_areas.', spec$ext))
  .write_combined_sf(team_parts,     paste0('team_areas.', spec$ext))
  .write_combined_sf(idp_parts,      paste0('idp_settlements.', spec$ext))
  .write_combined_sf(landmark_parts, paste0('landmarks.', spec$ext))
  .write_combined_sf(subdiv_parts,   paste0('subdivisions.', spec$ext))
  .write_combined_sf(urban_parts,    paste0('campaign_extent.', spec$ext))

  if (length(ha_summary_parts) > 0) {
    combined <- dplyr::bind_rows(ha_summary_parts)
    if (nrow(combined) > 0) .write_csv_utf8_bom(combined, file.path(tmp, 'health_area_population_summary.csv'), row.names = FALSE)
  }
  if (length(team_summary_parts) > 0) {
    combined <- dplyr::bind_rows(team_summary_parts)
    if (nrow(combined) > 0) .write_csv_utf8_bom(combined, file.path(tmp, 'team_population_summary.csv'), row.names = FALSE)
  }

  if (length(fac_parts) > 0) {
    fac_out <- dplyr::bind_rows(fac_parts) |>
      dplyr::select(zone_name, region_name, district_name,
                    dplyr::any_of(c('facility_id', 'facility_name', 'facility_type',
                                    'hf_ownership', 'polio_sia_coordination_site',
                                    'operational', 'lat', 'lon')))
    .write_csv_utf8_bom(fac_out, file.path(tmp, 'facilities.csv'), row.names = FALSE)
  }

  # District boundaries for context -- EVERY district in districts_shp,
  # not just this campaign's, with an in_campaign flag so the
  # relationship is explicit rather than the campaign's own districts
  # simply being a visually-indistinguishable subset with no attribute
  # to filter or symbolize on. Uses db_get_campaign_districts() (the
  # campaign's actual assignment) rather than progress$district_name,
  # since a district can be assigned to a campaign without having
  # published data yet -- those two sets aren't the same thing.
  campaign_districts <- tryCatch(db_get_campaign_districts(pool, campaign_id), error = function(e) NULL)
  campaign_district_names <- if (!is.null(campaign_districts) && nrow(campaign_districts) > 0)
    campaign_districts$district_name else character(0)
  districts_out <- districts_shp |>
    dplyr::mutate(in_campaign = district_name %in% campaign_district_names) |>
    sf::st_transform(4326)
  tryCatch(
    sf::st_write(districts_out, file.path(tmp, paste0('districts.', spec$ext)),
                driver = spec$driver, delete_dsn = TRUE, quiet = TRUE),
    error = function(e) cat('[download] failed to write districts layer:', e$message, '\n')
  )

  out_files <- list.files(tmp, full.names = TRUE)
  if (length(out_files) == 0) {
    .write_csv_utf8_bom(data.frame(message = 'No data available.'), file, row.names = FALSE)
    return(invisible(NULL))
  }
  tryCatch(
    zip::zip(zipfile = file, files = out_files, mode = 'cherry-pick'),
    error = function(e) zip(zipfile = file, files = out_files, flags = '-j')
  )
  invisible(NULL)
}
