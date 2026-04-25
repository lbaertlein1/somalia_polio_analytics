# -----------------------------------------------------------------------------
# Label lookup helpers — translate ODK codes to display labels
# -----------------------------------------------------------------------------
FACILITY_TYPE_LABELS <- c(
  phu       = "Primary Health Unit (PHU)",
  hc        = "Health Centre (HC)",
  dh        = "District Hospital (DH)",
  rh        = "Regional Hospital",
  nh        = "National Hospital",
  tb        = "TB Clinic"
)

OWNERSHIP_LABELS <- c(
  government = "Government (Public)",
  private    = "Private (For-profit)",
  ngo        = "NGO / Faith-based / Non-profit",
  other      = "Other"
)

label_facility_type <- function(x) {
  dplyr::recode(tolower(trimws(x)), !!!FACILITY_TYPE_LABELS, .default = x)
}

label_ownership <- function(x) {
  dplyr::recode(tolower(trimws(x)), !!!OWNERSHIP_LABELS, .default = x)
}

# =============================================================================
# Shared spatial helpers
# =============================================================================

# Returns the allowed drag/fetch buffer in metres based on u5 population
# density (per km²). Shared by fetch_facilities_odk() and facilityMapServer().
#
#   density >= 10  →  1 km   (dense urban)
#   density >= 1   →  5 km   (peri-urban / mixed)
#   density <  1   → 10 km   (sparse rural)
get_allowed_distance_m <- function(district_density) {
  district_density <- suppressWarnings(as.numeric(district_density))
  if (length(district_density) == 0 || all(is.na(district_density))) {
    return(5000)
  }
  district_density <- district_density[1]
  if (is.na(district_density)) return(5000)
  
  if (district_density >= 10) return(1000)
  if (district_density >= 1)  return(5000)
  return(10000)
}

# =============================================================================
# facility_odk.R
# ODK fetch helpers for the facility tab
# =============================================================================

# Credentials are read from .env in the project root:
#   ODK_USERNAME=your_email@example.com
#   ODK_PASSWORD=your_password

# -----------------------------------------------------------------------------
# Form config
# TODO: confirm zone_name strings match those in districts_shp
# -----------------------------------------------------------------------------
ODK_FORMS <- list(
  puntland = list(
    svc     = "https://emro.nafundi.com/v1/projects/9/forms/puntland_mhfl_facility_survey.svc",
    form_id = "puntland_mhfl_facility_survey",
    zones   = c("Puntland")
  ),
  somalia = list(
    svc     = "https://emro.nafundi.com/v1/projects/9/forms/somalia_mhfl_facility_survey.svc",
    form_id = "somalia_mhfl_facility_survey",
    zones   = c("South West State", "Hir-Shabelle State", "Galmudug State",
                "Jubaland State", "Banadir", "Somaliland")
  )
)

odk_form_config <- function(zone_name) {
  for (cfg in ODK_FORMS) {
    if (zone_name %in% cfg$zones) return(cfg)
  }
  warning("Zone '", zone_name, "' not matched in ODK_FORMS — defaulting to Somalia form.")
  ODK_FORMS$somalia
}

# -----------------------------------------------------------------------------
# fetch_facilities_odk()
# Pull MHFL submissions for a given zone/district.
# Returns a clean sf object matching the app schema, or NULL if none found.
# Uses wkt = TRUE to avoid handle_ru_geopoints duplicate-column bug.
# -----------------------------------------------------------------------------
fetch_facilities_odk <- function(zone_name, district_name) {
  
  cfg <- odk_form_config(zone_name)
  
  ruODK::ru_setup(
    svc     = cfg$svc,
    un      = trimws(Sys.getenv("ODK_USERNAME")),
    pw      = trimws(Sys.getenv("ODK_PASSWORD")),
    tz      = "Africa/Mogadishu",
    verbose = FALSE
  )
  
  raw <- ruODK::odata_submission_get(
    table    = "Submissions",
    wkt      = TRUE,
    download = FALSE
  )
  
  if ("system_review_state" %in% colnames(raw)) {
    raw <- raw |>
      dplyr::filter(is.na(system_review_state) | system_review_state != "rejected")
  }
  
  # Deduplicate by submission — repeat groups (e.g. partners_support_repeat)
  # cause one submission to expand into many rows. Keep one row per instanceID.
  raw <- raw |>
    dplyr::distinct(meta_instance_id, .keep_all = TRUE)
  
  # -------------------------------------------------------------------------
  # Spatial filtering — two inclusion rules (either is sufficient):
  #   Rule 1: facility GPS falls within the district polygon
  #   Rule 2: facility GPS falls within 10 km of the district polygon AND
  #            the ODK district field matches the crosswalk entry
  #
  # Using a fixed 10 km buffer here (not the density-based drag buffer).
  # -------------------------------------------------------------------------
  
  # Local alias avoids dplyr column-name shadowing on 'district_name'
  .shp <- district_name
  
  district_rows <- districts_shp |>
    dplyr::filter(district_name == .shp)
  
  district_geom_proj <- district_rows |>
    sf::st_transform(3857) |>
    sf::st_union() |>
    sf::st_make_valid()
  
  # Use the same density-based buffer as the drag-validation logic
  density              <- district_rows$u5_pop_density_km2[1]
  buffer_m             <- get_allowed_distance_m(density)
  district_buffer_proj <- sf::st_buffer(district_geom_proj, dist = buffer_m)
  
  # ODK slug(s) for this shapefile district (usually one, may be zero for unmapped)
  odk_districts <- DISTRICT_CROSSWALK |>
    dplyr::filter(shp_district == .shp) |>
    dplyr::pull(odk_district)
  
  # Only rows with valid GPS can be spatially tested
  has_coords <- !is.na(raw$geolocation_gps_latitude) & !is.na(raw$geolocation_gps_longitude)
  raw_coords <- raw[has_coords, , drop = FALSE]
  
  if (nrow(raw_coords) == 0) return(NULL)
  
  raw_pts <- sf::st_as_sf(
    raw_coords,
    coords = c("geolocation_gps_longitude", "geolocation_gps_latitude"),
    crs    = 4326,
    remove = FALSE
  ) |>
    sf::st_transform(3857)
  
  within_polygon <- lengths(sf::st_within(raw_pts, district_geom_proj)) > 0
  within_buffer  <- lengths(sf::st_within(raw_pts, district_buffer_proj)) > 0
  name_matches   <- raw_coords$facility_identification_district %in% odk_districts
  
  keep <- within_polygon | (within_buffer & name_matches)
  
  cat(
    "[fetch_facilities_odk] district:", district_name,
    "| density:", round(density, 3),
    "| buffer_km:", round(buffer_m / 1000, 1),
    "| within polygon:", sum(within_polygon),
    "| buffer+name match:", sum(within_buffer & name_matches),
    "| total kept:", sum(keep), "\n"
  )
  
  raw <- raw_coords[keep, , drop = FALSE]
  
  if (nrow(raw) == 0) return(NULL)
  
  out <- raw |>
    dplyr::transmute(
      facility_id   = facility_identification_facility_id,
      # Coalesce through the name chain: final -> correct -> manual -> base
      # facility_name_final is only populated when enumerator confirms/edits
      facility_name = dplyr::coalesce(
        dplyr::na_if(trimws(facility_identification_facility_name_final), ""),
        dplyr::na_if(trimws(facility_identification_facility_name_correct), ""),
        dplyr::na_if(trimws(facility_identification_facility_name_manual), ""),
        dplyr::na_if(trimws(facility_identification_facility_name), ""),
        paste("Unnamed facility", dplyr::row_number())
      ),
      facility_type               = label_facility_type(facility_identification_facility_type),
      hf_ownership                = label_ownership(facility_identification_hf_ownership),
      region                      = facility_identification_region,
      district                    = facility_identification_district,
      incharge_name               = contacts_incharge_name,
      lat                         = geolocation_gps_latitude,
      lon                         = geolocation_gps_longitude,
      polio_sia_coordination_site = "No",
      # ODK Central submission detail page — user clicks Edit Submission from here.
      odk_edit_link               = paste0(
        "https://emro.nafundi.com/#/projects/9/forms/",
        cfg$form_id,
        "/submissions/",
        meta_instance_id
      )
    ) |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    # If the same facility_id was surveyed multiple times, keep the most recent
    # submission (system_submission_date desc, already ordered by ODK response)
    dplyr::distinct(facility_id, .keep_all = TRUE)
  
  if (nrow(out) == 0) return(NULL)
  
  sf::st_as_sf(out, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
}

# -----------------------------------------------------------------------------
# merge_odk_with_app_edits()
# On re-pull, preserves app-side edits (renames, coordinate adjustments,
# coordination site flags) for facilities already known by facility_id.
# New ODK facilities get fresh defaults. Removed ODK facilities are dropped.
# -----------------------------------------------------------------------------
merge_odk_with_app_edits <- function(fresh_sf, existing_sf) {
  
  if (is.null(existing_sf) || nrow(existing_sf) == 0) return(fresh_sf)
  
  app_edits <- sf::st_drop_geometry(existing_sf) |>
    dplyr::select(facility_id, facility_name, lat, lon, polio_sia_coordination_site) |>
    dplyr::distinct(facility_id, .keep_all = TRUE) |>
    dplyr::rename(
      facility_name_app = facility_name,
      lat_app           = lat,
      lon_app           = lon,
      sia_app           = polio_sia_coordination_site
    )
  
  merged <- sf::st_drop_geometry(fresh_sf) |>
    dplyr::left_join(app_edits, by = "facility_id") |>
    dplyr::mutate(
      facility_name               = dplyr::coalesce(facility_name_app, facility_name),
      lat                         = dplyr::coalesce(lat_app, lat),
      lon                         = dplyr::coalesce(lon_app, lon),
      polio_sia_coordination_site = dplyr::coalesce(sia_app, polio_sia_coordination_site)
    ) |>
    dplyr::select(-facility_name_app, -lat_app, -lon_app, -sia_app)
  
  sf::st_as_sf(merged, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
}

# -----------------------------------------------------------------------------
# Schema helpers
# -----------------------------------------------------------------------------
facility_sf_to_df <- function(facility_sf) {
  if (is.null(facility_sf) || nrow(facility_sf) == 0) {
    return(data.frame(
      facility_id                 = character(),
      facility_name               = character(),
      facility_type               = character(),
      hf_ownership                = character(),
      region                      = character(),
      district                    = character(),
      incharge_name               = character(),
      lat                         = numeric(),
      lon                         = numeric(),
      polio_sia_coordination_site = character(),
      odk_edit_link               = character(),
      stringsAsFactors            = FALSE
    ))
  }
  
  sf::st_drop_geometry(facility_sf) |>
    dplyr::mutate(
      lat = as.numeric(lat),
      lon = as.numeric(lon)
    )
}

facility_df_to_sf <- function(facility_df) {
  if (is.null(facility_df) || nrow(facility_df) == 0) return(NULL)
  sf::st_as_sf(facility_df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
}


# =============================================================================
# district_crosswalk.R
# Maps shapefile district_name values to ODK facility_identification_district
# slugs. Used in fetch_facilities_odk() to filter submissions by district.
#
# Where ODK has no matching record the shapefile district is omitted.
# Entries marked TODO could not be matched with confidence — verify manually.
# =============================================================================

DISTRICT_CROSSWALK <- tibble::tribble(
  ~shp_district,        ~odk_district,
  
  # --- Banadir ---------------------------------------------------------------
  "Abdul Aziz",         "bra_benadir_abdul_aziz",
  "Bondere",            "bra_benadir_bondheere",       # spelling differs
  "Danyile",            "bra_benadir_deynile",          # spelling differs
  "Dharkenly",          "bra_benadir_dharkeynley",
  "Hamar Jabjab",       "bra_benadir_hamar_jabjab",
  "Hamar Weyn",         "bra_benadir_hamar_wayne",
  "Hawal Wadag",        "bra_benadir_hawal_wadag",
  "Heliwa",             "bra_benadir_heliwaa",
  "Hodan",              "bra_benadir_hodan",
  "Kahda",              "bra_benadir_kahda",
  "Karan",              "bra_benadir_karan",
  "Shangani",           "bra_benadir_shangani",
  "Shibis",             "bra_benadir_shibis",
  "Waberi",             "bra_benadir_waberi",
  "Yaqshid",            "bra_benadir_yaqshid",
  # TODO: "Madina"     — no clear ODK match (bra_benadir_warta_nabada?)
  # TODO: "Wardegly"   — no clear ODK match (bra_benadir_wadajir?)
  
  # --- Galmudug / Galgadud ---------------------------------------------------
  "Abudwaq",            "galmudug_galgadud_abudwak",
  "Adado",              "galmudug_galgadud_adado",
  "Balanbale",          "galmudug_galgadud_balanbale",
  "Dusamreb",           "galmudug_galgadud_dhuusamarreb",
  "El Dhere",           "galmudug_galgadud_eldheer",
  "Galinsoor",          "galmudug_galgadud_galinsoor",
  "Guriel",             "galmudug_galgadud_guriel",
  # TODO: "El Bur"     — no clear ODK match
  # TODO: "Elgaras"    — no clear ODK match
  # TODO: "Galad"      — no clear ODK match (galmudug_galgadud_gadoon?)
  # TODO: "Galhareeri" — no clear ODK match (galmudug_galgadud_godinlabe?)
  
  # --- Galmudug / South Mudug ------------------------------------------------
  "Galkayu South",      "galmudug_mudug_galkacyo",
  "Haradhere",          "galmudug_mudug_harardheere",
  "Hobyo",              "galmudug_mudug_hobyo",
  
  # --- Hir-Shabelle / Hiran --------------------------------------------------
  "Belet Weyne",        "hirshabelle_hiiraan_belet_weyne",
  "Bulo Burti",         "hirshabelle_hiiraan_buloburte",
  "Jalalaqsi",          "hirshabelle_hiiraan_jalalaqsi",
  "Mahas",              "hirshabelle_hiiraan_mahas",
  "Mataban",            "hirshabelle_hiiraan_mataban",
  
  # --- Hir-Shabelle / Middle Shabelle ----------------------------------------
  "Adale",              "hirshabelle_middle_shebelle_adale",
  "Balad",              "hirshabelle_middle_shebelle_balad",
  "Jowhar",             "hirshabelle_middle_shebelle_jowhar",
  "Mahaday",            "hirshabelle_middle_shebelle_mahaday",
  "Raga Elle",          "hirshabelle_middle_shebelle_raaga_celle",
  "Warsheikh",          "hirshabelle_middle_shebelle_warsheikh",
  # TODO: "Aden Yabal" — no ODK match
  # TODO: "Runingod"   — no ODK match
  
  # --- Jubaland / Gedo -------------------------------------------------------
  "Bardera",            "jubaland_gedo_bardera",
  "Belet Hawa",         "jubaland_gedo_beled_hawo",
  "Burdubo",            "jubaland_gedo_buurdhubo",
  "Dolo",               "jubaland_gedo_dolow",
  "El Wak",             "jubaland_gedo_ceel_waaq",
  "Garbaharey",         "jubaland_gedo_garbaharey",
  "Luuq",               "jubaland_gedo_luuq",
  # TODO: "Gedwein"    — no ODK match
  
  # --- Jubaland / Lower Juba -------------------------------------------------
  "Afmadow",            "jubaland_lower_juba_afmadow",
  "Badhadhe",           "jubaland_lower_juba_badhadhe",
  "Kismayo",            "jubaland_lower_juba_kismayo",
  # TODO: "Hagar", "Jamaame East", "Jamaame West", "Jilib West" — no ODK match
  # TODO: jubaland_lower_juba_dhobely — no shapefile match
  
  # --- South West / Bakool ---------------------------------------------------
  "El Barde",           "southwest_bakool_el_barde",
  "Hudur",              "southwest_bakool_hudur",
  "Rabdure",            "southwest_bakool_rabdhure",
  "Tiyeglo",            "southwest_bakool_tiyeglow",
  "Wajid",              "southwest_bakool_wajid",
  # TODO: no ODK match for southwest_bakool_quracjome
  
  # --- South West / Bay ------------------------------------------------------
  "Baidoa",             "southwest_bay_baidoba",
  "Berdale",            "southwest_bay_berdalle",
  "Burhakaba",          "southwest_bay_burhakaba",
  "Dinsoor",            "southwest_bay_diinsor",
  "Qansah Dheere",      "southwest_bay_qansaxdhere",
  
  # --- South West / Lower Shabelle -------------------------------------------
  "Afgoi",              "southwest_lower_shabelle_afgooye",
  "Awdheegle",          "southwest_lower_shabelle_awdhegle",
  "Brava",              "southwest_lower_shabelle_barawe",
  "Kurtunwaarey",       "southwest_lower_shabelle_kurtunwarey",
  "Marka",              "southwest_lower_shabelle_marka",
  "Qoryoley",           "southwest_lower_shabelle_qoryoolay",
  "Wanleweyne",         "southwest_lower_shabelle_wanla_weyn",
  # TODO: "Sablale" — no ODK match
  
  # --- Somaliland / Sool -----------------------------------------------------
  "Hudun",              "north_east_sool_hudun",
  "Las Anod",           "north_east_sool_lasanod",
  "Taleh",              "north_east_sool_taleh",
  
  # --- Somaliland / Sanag ----------------------------------------------------
  "Erigavo",            "north_east_sanaag_erigavo",
  
  # --- Somaliland / Togdheer -------------------------------------------------
  "Buhodle",            "north_east_cayn_buhodle"
)