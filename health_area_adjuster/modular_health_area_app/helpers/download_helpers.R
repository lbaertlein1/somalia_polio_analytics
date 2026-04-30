# =============================================================================
# download_helpers.R
# Replaces build_district_zip — outputs GeoJSON + CSV instead of shapefiles
# =============================================================================

#' Write district download files to a temp directory and zip them.
#'
#' Output files:
#'   health_areas.geojson       — health area polygons with planning attributes
#'   sia_coordination_sites.geojson — SIA coordination site points
#'   microplan.csv              — tabular planning data (one row per health area)
#'   facilities.csv             — all facility records

build_district_download <- function(file,
                                    district_name,
                                    zone          = '',
                                    region        = '',
                                    saved_dfa_sf  = NULL,
                                    planning_data = list(),
                                    facility_data = NULL) {
  
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  
  slug <- gsub('[^A-Za-z0-9]', '_', tolower(trimws(district_name)))
  
  # ── Health areas GeoJSON ───────────────────────────────────────────────────
  if (!is.null(saved_dfa_sf) && nrow(saved_dfa_sf) > 0) {
    sf_out <- saved_dfa_sf |>
      dplyr::mutate(
        zone_name     = zone,
        region_name   = region,
        district_name = district_name
      )
    
    # Join planning data as properties
    if (length(planning_data) > 0) {
      plan_df <- dplyr::bind_rows(lapply(names(planning_data), function(a) {
        d <- planning_data[[a]]
        data.frame(
          dfa_name      = a,
          u5_pop        = as.numeric(d$u5_pop        %||% NA),
          n_teams       = as.integer(d$n_teams       %||% NA),
          n_supervisors = as.integer(d$n_supervisors %||% NA),
          complete      = isTRUE(d$complete),
          notes         = trimws(d$notes %||% ''),
          stringsAsFactors = FALSE
        )
      }))
      sf_out <- sf_out |> dplyr::left_join(plan_df, by = 'dfa_name')
    }
    
    sf_out <- sf::st_transform(sf_out, 4326)
    sf::st_write(sf_out,
                 file.path(tmp, 'health_areas.geojson'),
                 driver = 'GeoJSON', delete_dsn = TRUE, quiet = TRUE)
  }
  
  # ── SIA coordination sites GeoJSON ────────────────────────────────────────
  if (!is.null(facility_data)) {
    fac_df <- if (inherits(facility_data, 'sf')) {
      sf::st_drop_geometry(facility_data)
    } else {
      facility_data
    }
    
    sia_df <- fac_df[
      !is.na(fac_df$polio_sia_coordination_site) &
        fac_df$polio_sia_coordination_site == 'Yes', ,
      drop = FALSE
    ]
    
    if (nrow(sia_df) > 0 && all(c('lon', 'lat') %in% names(sia_df))) {
      sia_sf <- sf::st_as_sf(sia_df, coords = c('lon', 'lat'), crs = 4326, remove = FALSE) |>
        dplyr::mutate(zone_name = zone, region_name = region, district_name = district_name)
      sf::st_write(sia_sf,
                   file.path(tmp, 'sia_coordination_sites.geojson'),
                   driver = 'GeoJSON', delete_dsn = TRUE, quiet = TRUE)
    }
    
    # ── All facilities CSV ───────────────────────────────────────────────────
    fac_out <- fac_df |>
      dplyr::mutate(zone_name = zone, region_name = region, district_name = district_name) |>
      dplyr::select(zone_name, region_name, district_name,
                    dplyr::any_of(c('facility_id', 'facility_name', 'facility_type',
                                    'hf_ownership', 'polio_sia_coordination_site',
                                    'operational', 'lat', 'lon')))
    write.csv(fac_out, file.path(tmp, 'facilities.csv'), row.names = FALSE)
  }
  
  # ── Microplan CSV ──────────────────────────────────────────────────────────
  if (length(planning_data) > 0) {
    mp_rows <- dplyr::bind_rows(lapply(names(planning_data), function(a) {
      d   <- planning_data[[a]]
      sup <- d$supervisors %||% list()
      sup_cols <- list()
      for (s_i in seq_len(min(length(sup), 10L))) {
        s <- sup[[s_i]] %||% list()
        sup_cols[[paste0('supervisor_', s_i, '_name')]]  <- s$name  %||% ''
        sup_cols[[paste0('supervisor_', s_i, '_role')]]  <- s$role  %||% ''
        sup_cols[[paste0('supervisor_', s_i, '_phone')]] <- s$phone %||% ''
        sup_cols[[paste0('supervisor_', s_i, '_email')]] <- s$email %||% ''
      }
      base <- data.frame(
        zone_name     = zone,
        region_name   = region,
        district_name = district_name,
        area_name     = a,
        u5_pop        = as.numeric(d$u5_pop        %||% NA),
        n_teams       = as.integer(d$n_teams       %||% NA),
        n_supervisors = as.integer(d$n_supervisors %||% NA),
        complete      = isTRUE(d$complete),
        notes         = trimws(d$notes %||% ''),
        stringsAsFactors = FALSE
      )
      if (length(sup_cols) > 0)
        base <- cbind(base, as.data.frame(sup_cols, stringsAsFactors = FALSE))
      base
    }))
    write.csv(mp_rows, file.path(tmp, 'microplan.csv'), row.names = FALSE)
  }
  
  # ── Zip ────────────────────────────────────────────────────────────────────
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
