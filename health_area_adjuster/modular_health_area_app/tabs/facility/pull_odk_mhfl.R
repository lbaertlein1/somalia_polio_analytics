# =============================================================================
# ODK fetch helpers for facility tab
# =============================================================================

# Map zone name to ODK OData service URL and form ID.
# TODO: confirm the exact zone_name strings used in districts_shp for Puntland.
ODK_FORMS <- list(
  somaliland = list(
    svc     = "https://emro.nafundi.com/v1/projects/14/forms/somaliland_mhfl_facility_survey.svc",
    form_id = "somaliland_mhfl_facility_survey",
    zones   = c("Somaliland")   # <-- adjust to match zone_name values in your shapefile
  ),
  puntland = list(
    svc     = "https://emro.nafundi.com/v1/projects/9/forms/puntland_mhfl_facility_survey.svc",
    form_id = "puntland_mhfl_facility_survey",
    zones   = c("Puntland")   # <-- adjust to match zone_name values in your shapefile
  ),
  somalia = list(
    svc     = "https://emro.nafundi.com/v1/projects/9/forms/somalia_mhfl_facility_survey.svc",
    form_id = "somalia_mhfl_facility_survey",
    zones   = c("South West", "Hirshabelle", "Galmudug", "Jubaland", "Benadir", "Somaliland")
    # <-- adjust / extend to match all non-Puntland zone_name values
  )
)

odk_form_config <- function(zone_name) {
  for (cfg in ODK_FORMS) {
    if (zone_name %in% cfg$zones) return(cfg)
  }
  # Default to Somalia form if zone not matched; log a warning.
  warning("Zone '", zone_name, "' not matched in ODK_FORMS — defaulting to Somalia form.")
  ODK_FORMS$somalia
}

# -----------------------------------------------------------------------------
#' Fetch and shape facility data from ODK Central for a given district.
#'
#' Returns an sf object matching the facility schema expected by the app, or
#' NULL if no records are found for the district.
#'
#' Credentials are read from environment variables ODK_USERNAME / ODK_PASSWORD.
#' Add these to your .Renviron:
#'   ODK_USERNAME=your_email@example.com
#'   ODK_PASSWORD=your_password
# -----------------------------------------------------------------------------
fetch_facilities_odk <- function(zone_name, district_name) {
  
  cfg <- odk_form_config(zone_name)
  
  ruODK::ru_setup(
    svc     = cfg$svc,
    un      = Sys.getenv("ODK_USERNAME"),
    pw      = Sys.getenv("ODK_PASSWORD"),
    tz      = "Africa/Mogadishu",
    verbose = FALSE
  )
  
  schema <- ruODK::form_schema()
  
  raw <- ruODK::odata_submission_get(
    table    = "Submissions",
    wkt      = FALSE,
    download = FALSE
  ) |>
    ruODK::handle_ru_geopoints(schema)
  
  # Drop rejected submissions
  if ("system_review_state" %in% colnames(raw)) {
    raw <- raw |>
      dplyr::filter(is.na(system_review_state) | system_review_state != "rejected")
  }
  
  # Filter to the selected district (case-insensitive, trim whitespace)
  raw <- raw |>
    dplyr::filter(
      tolower(trimws(district)) == tolower(trimws(district_name))
    )
  
  if (nrow(raw) == 0) return(NULL)
  
  # ruODK expands the `gps` geopoint field into gps_latitude / gps_longitude.
  # ruODK converts `instanceID` (nested under `meta`) to `meta_instance_id`.
  # If your ruODK version differs, check colnames(raw) and adjust below.
  instance_col <- dplyr::case_when(
    "meta_instance_id" %in% colnames(raw) ~ "meta_instance_id",
    "instance_id"      %in% colnames(raw) ~ "instance_id",
    TRUE                                   ~ NA_character_
  )
  
  if (is.na(instance_col)) {
    warning("Could not find instanceID column in ODK response. Edit links will be empty.")
    raw$odk_edit_link <- NA_character_
  } else {
    raw$odk_edit_link <- paste0(
      "https://emro.nafundi.com/#/projects/9/forms/",
      cfg$form_id,
      "/submissions/",
      raw[[instance_col]]
    )
  }
  
  out <- raw |>
    dplyr::transmute(
      facility_id                 = facility_id,
      facility_name               = facility_name_final,
      facility_type               = facility_type,
      hf_ownership                = hf_ownership,
      region                      = region,
      district                    = district,
      incharge_name               = incharge_name,
      lat                         = gps_latitude,
      lon                         = gps_longitude,
      polio_sia_coordination_site = "No",   # app-only default; preserved on re-pull
      odk_edit_link               = odk_edit_link
    ) |>
    dplyr::filter(!is.na(lat), !is.na(lon))
  
  if (nrow(out) == 0) return(NULL)
  
  sf::st_as_sf(out, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
}

# -----------------------------------------------------------------------------
# Merge fresh ODK pull with existing app-side edits.
#
# Preserves, for facilities already known by facility_id:
#   - lat / lon overrides (coordinate adjustments)
#   - facility_name overrides (renames)
#   - polio_sia_coordination_site flags
#
# New facilities from ODK get fresh defaults.
# Facilities no longer in ODK (deleted/rejected) are dropped.
# -----------------------------------------------------------------------------
merge_odk_with_app_edits <- function(fresh_sf, existing_sf) {
  
  if (is.null(existing_sf) || nrow(existing_sf) == 0) return(fresh_sf)
  
  app_edits <- sf::st_drop_geometry(existing_sf) |>
    dplyr::select(
      facility_id,
      facility_name,
      lat,
      lon,
      polio_sia_coordination_site
    ) |>
    dplyr::rename(
      facility_name_app               = facility_name,
      lat_app                         = lat,
      lon_app                         = lon,
      polio_sia_coordination_site_app = polio_sia_coordination_site
    )
  
  merged <- sf::st_drop_geometry(fresh_sf) |>
    dplyr::left_join(app_edits, by = "facility_id") |>
    dplyr::mutate(
      # Use app override if it exists, otherwise use fresh ODK value
      facility_name               = dplyr::coalesce(facility_name_app, facility_name),
      lat                         = dplyr::coalesce(lat_app, lat),
      lon                         = dplyr::coalesce(lon_app, lon),
      polio_sia_coordination_site = dplyr::coalesce(
        polio_sia_coordination_site_app,
        polio_sia_coordination_site
      )
    ) |>
    dplyr::select(-facility_name_app, -lat_app, -lon_app, -polio_sia_coordination_site_app)
  
  sf::st_as_sf(merged, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
}

