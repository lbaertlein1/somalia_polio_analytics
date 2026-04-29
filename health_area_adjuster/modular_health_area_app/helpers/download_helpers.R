# =============================================================================
# download_helpers.R
# Shared zip-building logic for district microplan downloads.
#
# Used by:
#   mod_microplan_tab.R  — downloads live session data
#   mod_admin_tab.R      — downloads from saved session snapshots
#
# Phase 6: replace data-extraction logic with DB queries; this function
#           signature and zip structure stay the same.
#
# Zip contents:
#   health_areas.csv    — one row per health area
#   supervisors.csv     — one row per supervisor, linked by health_area_uid
#   health_areas.shp    — area boundary polygons (+ .dbf/.prj/.shx)
#   sia_sites.shp       — SIA coordination site points (+ .dbf/.prj/.shx)
# =============================================================================

#' Build a district microplan zip file
#'
#' @param file         Output path for the .zip file
#' @param district_name, zone, region  Character strings for metadata
#' @param saved_dfa_sf sf object — health area polygons (or NULL)
#' @param planning_data Named list of per-area planning data (or empty list)
#' @param facility_data data.frame of facilities including lat/lon and
#'                      polio_sia_coordination_site column (or NULL)
build_district_zip <- function(
    file,
    district_name,
    zone            = '',
    region          = '',
    saved_dfa_sf    = NULL,
    planning_data   = list(),
    facility_data   = NULL
) {
  tmp <- tempfile()
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  slug <- gsub('[^A-Za-z0-9]', '_', tolower(trimws(district_name)))

  # ── UIDs ───────────────────────────────────────────────────────────────────
  .uid <- function(area_name) {
    paste0(slug, '__', gsub('[^A-Za-z0-9]', '_', tolower(trimws(area_name))))
  }

  # ── 1. health_areas.csv ────────────────────────────────────────────────────
  area_names <- if (!is.null(saved_dfa_sf) && nrow(saved_dfa_sf) > 0) {
    unique(saved_dfa_sf$dfa_name)
  } else {
    names(planning_data)
  }
  area_names <- setdiff(area_names, extra_dfa_names)

  ha_rows <- lapply(area_names, function(a) {
    d <- planning_data[[a]] %||% list()
    data.frame(
      uid           = .uid(a),
      zone          = zone,
      region        = region,
      district      = district_name,
      area_name     = a,
      u5_pop        = d$u5_pop        %||% NA_real_,
      n_teams       = d$n_teams       %||% NA_integer_,
      n_supervisors = d$n_supervisors %||% NA_integer_,
      complete      = isTRUE(d$complete),
      notes         = d$notes         %||% '',
      stringsAsFactors = FALSE
    )
  })

  ha_df <- if (length(ha_rows) > 0) dplyr::bind_rows(ha_rows) else
    data.frame(uid = character(0), zone = character(0), region = character(0),
               district = character(0), area_name = character(0),
               u5_pop = numeric(0), n_teams = integer(0),
               n_supervisors = integer(0), complete = logical(0),
               notes = character(0), stringsAsFactors = FALSE)

  write.csv(ha_df, file.path(tmp, 'health_areas.csv'), row.names = FALSE)

  # ── 2. supervisors.csv ─────────────────────────────────────────────────────
  sup_rows <- lapply(area_names, function(a) {
    d    <- planning_data[[a]] %||% list()
    sups <- d$supervisors %||% list()
    if (length(sups) == 0) return(NULL)
    lapply(seq_along(sups), function(i) {
      s <- sups[[i]] %||% list()
      data.frame(
        health_area_uid    = .uid(a),
        supervisor_number  = i,
        name               = s$name  %||% '',
        role               = s$role  %||% '',
        phone              = s$phone %||% '',
        email              = s$email %||% '',
        stringsAsFactors   = FALSE
      )
    })
  })

  sup_rows_flat <- Filter(Negate(is.null), unlist(sup_rows, recursive = FALSE))
  sup_df <- if (length(sup_rows_flat) > 0) dplyr::bind_rows(sup_rows_flat) else
    data.frame(health_area_uid = character(0), supervisor_number = integer(0),
               name = character(0), role = character(0),
               phone = character(0), email = character(0),
               stringsAsFactors = FALSE)

  write.csv(sup_df, file.path(tmp, 'supervisors.csv'), row.names = FALSE)

  # ── 3. health_areas.shp ───────────────────────────────────────────────────
  if (!is.null(saved_dfa_sf) && nrow(saved_dfa_sf) > 0) {
    tryCatch({
      shp <- sf::st_transform(saved_dfa_sf, 4326)

      # Normalise geometry — shapefiles require POLYGON/MULTIPOLYGON
      shp <- tryCatch(sf::st_collection_extract(shp, 'POLYGON'), error = function(e) shp)
      shp <- tryCatch(sf::st_cast(shp, 'MULTIPOLYGON', warn = FALSE), error = function(e) shp)

      shp$uid      <- vapply(shp$dfa_name, .uid, character(1))
      shp$district <- district_name
      shp$region   <- region
      shp$zone     <- zone

      shp <- shp |> dplyr::select(uid, area_name = dfa_name,
                                   district, region, zone, geometry)

      shp_path <- file.path(tmp, 'health_areas.shp')
      shp_path <- normalizePath(shp_path, mustWork = FALSE)
      sf::write_sf(shp, shp_path, delete_dsn = TRUE)
      
    }, error = function(e) {
      cat('[download] health_areas.shp write error:', e$message, '\n')
    })
  }

  # ── 4. sia_sites.shp ──────────────────────────────────────────────────────
  if (!is.null(facility_data) && nrow(facility_data) > 0) {
    tryCatch({
      sia <- facility_data[
        !is.na(facility_data$polio_sia_coordination_site) &
        facility_data$polio_sia_coordination_site == 'Yes', ,
        drop = FALSE
      ]

      # Ensure lat/lon are present
      if (nrow(sia) > 0 && all(c('lat', 'lon') %in% names(sia))) {
        sia_sf <- sf::st_as_sf(
          data.frame(
            name     = sia$facility_name %||% '',
            district = district_name,
            region   = region,
            zone     = zone,
            lat      = as.numeric(sia$lat),
            lon      = as.numeric(sia$lon),
            stringsAsFactors = FALSE
          ),
          coords = c('lon', 'lat'),
          crs    = 4326
        )
        sf::write_sf(sia_sf, file.path(tmp, 'sia_sites.shp'), delete_dsn = TRUE)
      }
    }, error = function(e) {
      cat('[download] sia_sites.shp write error:', e$message, '\n')
    })
  }

  # ── Zip everything ─────────────────────────────────────────────────────────
  all_files <- list.files(tmp, full.names = TRUE)
  if (length(all_files) == 0) {
    write.csv(data.frame(message = 'No data available.'), file, row.names = FALSE)
    return(invisible(NULL))
  }

  tryCatch(
    zip::zip(zipfile = file, files = all_files, mode = 'cherry-pick'),
    error = function(e) {
      # Fallback: base R zip
      zip(zipfile = file, files = all_files, flags = '-j')
    }
  )

  invisible(NULL)
}
