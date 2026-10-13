# R/data_pull.R
# ─────────────────────────────────────────────────────────────────────────────
# Fetches both ODK forms, joins to ArcGIS district geometry, aggregates all
# dashboard data objects in memory, and exports current ODK camps to CSV
#
# Run locally:  source("odk_credentials.R"); source("refresh_data.R")
# Called by global.R on every app startup
# ─────────────────────────────────────────────────────────────────────────────

if (file.exists(".Renviron")) readRenviron(".Renviron")

# Load libs if not already loaded (handles standalone sourcing)
for (pkg in c("ruODK","dplyr","tidyr","lubridate","sf","httr","jsonlite","stringr","purrr","readxl")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

if (!exists("normalise_district", mode = "function")) source("R\\utils.R")

rename_flexible <- function(df, pattern, new_name) {
  col <- grep(pattern, names(df), value = TRUE, ignore.case = TRUE)[1]
  if (!is.na(col) && col != new_name) dplyr::rename(df, !!new_name := dplyr::all_of(col))
  else df
}

write_csv_safely <- function(data, path, ...) {
  tryCatch({
    utils::write.csv(data, path, ...)
    TRUE
  }, error = function(e) {
    warning("Could not write ", path, ": ", conditionMessage(e), call. = FALSE)
    FALSE
  })
}

parse_survey_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(lubridate::as_date(x))

  text <- trimws(as.character(x))
  parsed <- suppressWarnings(as.Date(substr(text, 1, 10), format = "%Y-%m-%d"))
  missing <- is.na(parsed) & !is.na(text) & nzchar(trimws(text))
  parsed[missing] <- suppressWarnings(lubridate::mdy(text[missing], quiet = TRUE))
  missing <- is.na(parsed) & !is.na(text) & nzchar(trimws(text))
  parsed[missing] <- suppressWarnings(lubridate::dmy(text[missing], quiet = TRUE))
  lubridate::as_date(parsed)
}

message("── [1/6] Authenticating with ODK Central ──")
ruODK::ru_setup(
  svc  = paste0(Sys.getenv("ODKC_URL"),
                "/v1/projects/", Sys.getenv("ODKC_PID"),
                "/forms/", Sys.getenv("ODKC_FID_CAMPS"), ".svc"),
  un   = Sys.getenv("ODKC_UN"),
  pw   = Sys.getenv("ODKC_PW"),
  tz   = "Africa/Mogadishu"
)

# ─────────────────────────────────────────────────────────────────────────────
# HELPER: get_schema() — prints field names to console so you can verify paths
# Usage (run once manually):  get_schema("nomad_mapping_survey_gedo")
# ─────────────────────────────────────────────────────────────────────────────
get_schema <- function(fid) {
  ruODK::ru_setup(
    svc = paste0(Sys.getenv("ODKC_URL"),
                 "/v1/projects/", Sys.getenv("ODKC_PID"),
                 "/forms/", fid, ".svc"),
    un  = Sys.getenv("ODKC_UN"),
    pw  = Sys.getenv("ODKC_PW")
  )
  schema <- ruODK::form_schema()
  print(schema, n = Inf)
  invisible(schema)
}
# get_schema(Sys.getenv("ODKC_FID_CAMPS"))
# get_schema(Sys.getenv("ODKC_FID_OUTREACH"))

message("── [2/6] Pulling camp enumeration form ──")
ruODK::ru_setup(
  svc = paste0(Sys.getenv("ODKC_URL"),
               "/v1/projects/", Sys.getenv("ODKC_PID"),
               "/forms/", Sys.getenv("ODKC_FID_CAMPS"), ".svc"),
  un  = Sys.getenv("ODKC_UN"),
  pw  = Sys.getenv("ODKC_PW"),
  tz  = "Africa/Mogadishu"
)

camps_raw <- ruODK::odata_submission_get(
  table  = "Submissions",
  parse  = TRUE,
  download = FALSE,
)

odk_camps_csv <- camps_raw
list_cols <- names(odk_camps_csv)[vapply(odk_camps_csv, is.list, logical(1))]
for (field in list_cols) {
  odk_camps_csv[[field]] <- vapply(odk_camps_csv[[field]], function(value) {
    as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null"))
  }, character(1))
}
if (!dir.exists("data")) dir.create("data")
if (write_csv_safely(odk_camps_csv, "data/odk_camps_current.csv",
                     row.names = FALSE, na = "")) {
  message("  Saved fresh ODK camp download to data/odk_camps_current.csv")
}

historical_camps_path <- file.path(
  "data", "ODK_Data_2024_2025_2026_cleanUp_18Aug26(Survey Data_Clean).csv"
)
if (!file.exists(historical_camps_path)) {
  stop("Historical camp data file not found: ", historical_camps_path)
}

camp_cutoff <- as.Date("2026-07-31")
historical_camps <- utils::read.csv(
  historical_camps_path,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)
names(historical_camps) <- gsub("-", "_", names(historical_camps), fixed = TRUE)
historical_camps <- historical_camps |>
  rename_flexible("group_profile.*today_date", "camp_survey_date") |>
  rename_flexible("^submissiondate$", "system_submission_date") |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
  dplyr::mutate(
    id = paste0("historical_", dplyr::row_number()),
    data_source = "historical"
  )

camps_odk <- camps_raw |>
  rename_flexible("group_profile.*today_date", "camp_survey_date") |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
  dplyr::mutate(
    id = dplyr::if_else(
      !is.na(id) & nzchar(id), paste0("odk_", id),
      paste0("odk_row_", dplyr::row_number())
    ),
    data_source = "odk"
  )

historical_camps <- historical_camps |>
  dplyr::mutate(camp_survey_date = parse_survey_date(camp_survey_date))
camps_odk <- camps_odk |>
  dplyr::mutate(camp_survey_date = parse_survey_date(camp_survey_date))
historical_missing_date <- sum(is.na(historical_camps$camp_survey_date))
odk_missing_date <- sum(is.na(camps_odk$camp_survey_date))
historical_camps <- historical_camps |>
  dplyr::filter(!is.na(camp_survey_date), camp_survey_date <= camp_cutoff)
camps_odk <- camps_odk |>
  dplyr::filter(!is.na(camp_survey_date), camp_survey_date > camp_cutoff)
camps_raw <- dplyr::bind_rows(historical_camps, camps_odk)

message("  Historical camp rows through 31 Jul 2026: ", nrow(historical_camps),
        " (", historical_missing_date, " missing survey dates excluded)")
message("  ODK camp rows from 1 Aug 2026: ", nrow(camps_odk),
        " (", odk_missing_date, " missing survey dates excluded)")

enumerator_district_col <- grep(
  "^group_profile.*enumerator.*district$",
  names(camps_raw), ignore.case = TRUE, value = TRUE
)[1]
if (is.na(enumerator_district_col)) {
  stop("Could not find group_profile enumerator district field in camp data")
}
enumerator_district_counts <- camps_raw |>
  dplyr::transmute(
    enumerator_district = trimws(as.character(.data[[enumerator_district_col]]))
  ) |>
  dplyr::filter(!is.na(enumerator_district), enumerator_district != "") |>
  dplyr::count(enumerator_district, name = "appearances") |>
  dplyr::arrange(dplyr::desc(appearances), enumerator_district)
message("Enumerator district appearances:")
print(as.data.frame(enumerator_district_counts), row.names = FALSE)

# Transport is a flat group

message("── [3/6] Pulling outreach sessions form ──")
ruODK::ru_setup(
  svc = paste0(Sys.getenv("ODKC_URL"),
               "/v1/projects/", Sys.getenv("ODKC_PID"),
               "/forms/", Sys.getenv("ODKC_FID_OUTREACH"), ".svc"),
  un  = Sys.getenv("ODKC_UN"),
  pw  = Sys.getenv("ODKC_PW"),
  tz  = "Africa/Mogadishu"
)

outreach_raw <- ruODK::odata_submission_get(
  table        = "Submissions",
  parse        = TRUE,
  download     = FALSE,
)

kobo_path <- "data/kobo_outreach.xlsx"

# Rename ODK columns to final names BEFORE binding with KoBoToolbox
  
message("  ODK outreach cols: ", paste(sort(names(outreach_raw)), collapse = ", "))
  
outreach_raw <- outreach_raw |>
  # Core metrics
  rename_flexible("b_total_number_of_chil",             "zd_vaccinated") |>
  rename_flexible("c_number_of_childr.*settlement$",    "zd_ri") |>
  rename_flexible("c_number_of_childr.*settlement_001", "zd_polio") |>
  # Date
  rename_flexible("^today$",                            "out_date") |>
  # GPS from split geopoint (confirmed names)
  rename_flexible("geo_reference_hh_coordinates_2",     "out_lat") |>
  rename_flexible("geo_reference_hh_coordinates_1",     "out_lon") |>
  # Source tag
  dplyr::mutate(
    id                     = as.character(id),
    source                 = "odk",
    system_submission_date = lubridate::as_datetime(system_submission_date)
    )
  
message("  ODK outreach cols after rename: ", paste(sort(names(outreach_raw)), collapse = ", "))
  
if (file.exists(kobo_path)) {
  message("  Merging KoBoToolbox historical outreach (", kobo_path, ")...")
  library(readxl)
  kobo_raw <- readxl::read_excel(kobo_path)
    
  # Map KoBoToolbox label headers > ruodk column names
  kobo_renamed <- kobo_raw |>
    dplyr::rename(
      out_date       = today,
      a_district = District,
      zd_vaccinated     = `Total number of zero dose children vaccinated during the outreach session?`,
      zd_ri          = `Number of children with zero dose routine immunization (excluding Polio) found in the settlement`,
      zd_polio       = `Number of children with zero polio doses found in the settlement`,
      out_lat        = `_Geo-reference of the settlement outreach_latitude`,
      out_lon        = `_Geo-reference of the settlement outreach_longitude`,
      system_submission_date = `_submission_time`,
      id             = `_id`
    ) |>
    dplyr::mutate(
      id                     = as.character(paste0("kobo_", id)),
      source                 = "kobo",
      system_submission_date = lubridate::as_datetime(system_submission_date)
    )
  
  outreach_raw <- dplyr::bind_rows(outreach_raw, kobo_renamed)
  message("  Combined outreach records: ", nrow(outreach_raw),
          " (ODK + KoBoToolbox)")
  message("  Merged outreach cols: ", paste(sort(names(outreach_raw)), collapse = ", "))
} else {
  message("  No KoBoToolbox file found at ", kobo_path, " — using ODK data only.")
  message("  ODK-only outreach cols: ", paste(sort(names(outreach_raw)), collapse = ", "))
}

  
# Add source tag to ODK data
outreach_raw <- outreach_raw |>
  dplyr::mutate(
    id                     = as.character(id),
    source                 = "odk",
    system_submission_date = lubridate::as_datetime(system_submission_date)
  )

# ── Merge KoBoToolbox historical outreach data ────────────────────────────────
# The outreach form was previously on KoBoToolbox (same form ID ae2VmS6pPZjpYMec7Ck3eR).
# Historical export saved as data/kobo_outreach.xlsx — map label headers to
# the same ruodk column names produced by the ODK fetch above, then bind rows.


message("── [4/6] Loading district geometry from ArcGIS ──")
arcgis_url <- paste0(
  "https://services.arcgis.com/5T5nSi527N4F7luB/ArcGIS/rest/services/",
  "Somalia_Public_Health_Boundaries_2026/FeatureServer/2/query",
  "?where=1%3D1&outFields=*&f=geojson&outSR=4326"
)

districts_sf <- tryCatch({
  resp <- httr::GET(arcgis_url, httr::timeout(60))
  httr::stop_for_status(resp)
  geojson_txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  sf::st_read(geojson_txt, quiet = TRUE) |>
    sf::st_make_valid()
}, error = function(e) {
  message("  ArcGIS fetch failed: ", e$message)
  NULL
})

# Identify the district name column — common candidates
if (!is.null(districts_sf)) {
  # Confirmed field names from Somalia PHB 2026 FeatureServer:
  # NAME_L2 = district name, NAME_L1 = state/region name
  districts_sf <- districts_sf |>
    dplyr::rename(district_name = NAME_L2,
                  state_name    = NAME_L1) |>
    dplyr::select(district_name, state_name, geometry)
  message("  Loaded ", nrow(districts_sf), " district polygons")
}

# ---- ALWAYS rename ODK outreach columns BEFORE checking KoBo ----
message("  ODK outreach cols: ", paste(sort(names(outreach_raw)), collapse = ", "))

outreach <- outreach_raw |>
  dplyr::mutate(
    out_district   = normalise_district(a_district),
    zd_ri          = suppressWarnings(as.numeric(zd_ri)),
    zd_polio       = suppressWarnings(as.numeric(zd_polio)),
    zd_vaccinated  = suppressWarnings(as.numeric(zd_vaccinated)),
    vaccinated     = zd_vaccinated,  #For UI purposes
    out_date       = lubridate::as_date(dplyr::coalesce(
      suppressWarnings(lubridate::ymd(as.character(out_date))),
      lubridate::as_date(system_submission_date)
    )),
    year           = lubridate::year(out_date),
    month_num      = lubridate::month(out_date),
    month_label    = format(out_date, "%b-%y")
  )

message("  Outreach cols after cleaning: ", paste(sort(names(outreach)), collapse = ", "))

# Spatial district assignment for outreach
if (!is.null(districts_sf) &&
    all(c("out_lat", "out_lon") %in% names(outreach))) {
  
  out_coords <- outreach |>
    dplyr::filter(!is.na(out_lat) & !is.na(out_lon) & out_lat != 0)
  
  if (nrow(out_coords) > 0) {
    out_sf <- sf::st_as_sf(
      out_coords,
      coords = c("out_lon", "out_lat"),
      crs = 4326,
      remove = FALSE
    )
    
    out_joined <- sf::st_join(out_sf, districts_sf, join = sf::st_within) |>
      sf::st_drop_geometry() |>
      dplyr::select(id, spatial_out_district = district_name)
    
    outreach <- outreach |>
      dplyr::left_join(out_joined, by = "id") |>
      dplyr::mutate(
        # Prefer spatial district if available; otherwise fall back to original
        out_district = dplyr::coalesce(spatial_out_district, out_district),
        # Normalize final district values
        out_district = normalise_district(trimws(out_district))
      )
  }
}

message("── [5/6] Cleaning and joining ──")

# ── Camp enumeration cleaning ─────────────────────────────────────────────────
camps <- camps_raw |>
  # ── Exact ruodk_name mappings confirmed from form_schema() ──────────────────
  # Movement
  rename_flexible("community_level_camp_next_adm3",     "next_district") |>
  rename_flexible("community_level_camp_previous_adm3", "prev_district") |>
  # Dates
  rename_flexible("community_level_camp_last_exit_date", "exit_date") |>
  rename_flexible("community_level_camp_first_month",    "first_month") |>
  rename_flexible("community_level_camp_last_month",     "last_month") |>
  # HF travel time — field is camp_hf_ri_num (integer minutes)
  rename_flexible("camp_hf_ri_num",                     "hf_travel_min") |>
  # Settlement and enumerator
  rename_flexible("community_level_camp_settlement",     "settlement_type") |>
  rename_flexible("community_level_camp_settlement_name", "camp_name") |>
  rename_flexible("group_profile_enumerator_district",   "enum_district") |>
  rename_flexible("group_code$",                         "camp_code") |>
  # GPS — ruODK splits geopoint; raw string col is community_level_gps exactly
  rename_flexible("^community_level_gps$",               "gps_raw") |>
  # Population — under /polio/ group
  rename_flexible("polio_camp_pop_0to11_months$",        "pop_0to11") |>
  rename_flexible("polio_camp_pop_12to59_months$",       "pop_12to59") |>
  rename_flexible("polio_camp_tot_pop",                  "pop_total") |>
  # Zero-dose — ruODK lowercases camelCase: zeroDose → zero_dose
  rename_flexible("polio_camp_pop_0to11_months_?zero_?dose",  "zd_0to11") |>
  rename_flexible("polio_camp_pop_12to59_months_?zero_?dose", "zd_12to59") |>
  # Challenges — ruODK lowercases to polio_camp_previous_service_challenges
  rename_flexible("polio_camp_previous_service_challenges", "challenge") |>
  # Transport (flat group, not repeat)
  rename_flexible("transport_type_transport_infrastructure", "transport_infra") |>
  rename_flexible("transport_type_migratory_route",         "route_type") |>
  rename_flexible("transport_name_migratory_route",         "route_name") |>
  dplyr::mutate(
    # Coerce numeric
    pop_0to11     = suppressWarnings(as.numeric(pop_0to11)),
    pop_12to59    = suppressWarnings(as.numeric(pop_12to59)),
    pop_total     = suppressWarnings(as.numeric(pop_total)),
    zd_0to11      = suppressWarnings(as.numeric(zd_0to11)),
    zd_12to59     = suppressWarnings(as.numeric(zd_12to59)),
    hf_travel_min = suppressWarnings({
      # Field is free-text with 680+ variants: "120mins", "120 minutes", "2hrs", etc.
      # Extract leading integer, then convert hours to minutes
      raw <- as.character(hf_travel_min)
      # Normalise: strip spaces, lowercase
      raw <- tolower(trimws(raw))
      # Extract numeric value
      num <- suppressWarnings(as.numeric(stringr::str_extract(raw, "^[0-9]+")))
      # If value looks like hours (<=5 with "hr"/"hour" in string), convert
      is_hrs <- grepl("hr|hour", raw)
      dplyr::if_else(is_hrs & !is.na(num) & num <= 12, num * 60L, num)
    }),
    # exit_date is free-text ("August 2025", "Deyr 2024" etc.) — extract season directly
    exit_date_raw = as.character(exit_date),
    season = dplyr::case_when(
      grepl("deyr|oct|nov",          exit_date_raw, ignore.case = TRUE) ~ "Deyr",
      grepl("guul|guu|mar|apr|may",  exit_date_raw, ignore.case = TRUE) ~ "Guul",
      grepl("jilaal|dec|jan|feb",    exit_date_raw, ignore.case = TRUE) ~ "Jilaal",
      grepl("xaaga|hagaa|jun|jul|aug|sep", exit_date_raw, ignore.case = TRUE) ~ "Xaaga",
      TRUE ~ NA_character_
    ),
    # Normalise recorded district names via crosswalk
    next_district    = normalise_district(next_district),
    prev_district    = normalise_district(prev_district),
    test_district = !is.na(enum_district) &
      tolower(trimws(as.character(enum_district))) == "test",
    enum_district = normalise_district(
      dplyr::na_if(trimws(as.character(enum_district)), "")
    )
  )

# Extract GPS coordinates from geopoint if present
# ruODK parses geopoint into separate *-Latitude, *-Longitude columns
# e.g. community_level_gps-Latitude / community_level_gps-Longitude
# Fall back to splitting the raw string if split columns aren't present
lat_col <- grep("gps.*latitude|gps.*lat$",  names(camps), ignore.case = TRUE, value = TRUE)[1]
lon_col <- grep("gps.*longitude|gps.*lon$", names(camps), ignore.case = TRUE, value = TRUE)[1]

if (!is.na(lat_col) && !is.na(lon_col)) {
  camps <- camps |>
    dplyr::mutate(
      lat = suppressWarnings(as.numeric(.data[[lat_col]])),
      lon = suppressWarnings(as.numeric(.data[[lon_col]]))
    )
} else if ("gps_raw" %in% names(camps)) {
  camps <- camps |>
    dplyr::mutate(
      gps_str = as.character(gps_raw),
      lat = suppressWarnings(as.numeric(purrr::map_chr(
        strsplit(gps_str, " "), ~ .x[1]))),
      lon = suppressWarnings(as.numeric(purrr::map_chr(
        strsplit(gps_str, " "), ~ .x[2])))
    )
} else {
  camps <- camps |>
    dplyr::mutate(lat = NA_real_, lon = NA_real_)
  message("  WARNING: no GPS columns found in camps form")
}

camps <- camps |>
  dplyr::mutate(district = enum_district)

# Use the form's survey date for both historical and ODK camp records.
camps <- camps |>
  dplyr::mutate(
    sub_date = camp_survey_date,
    year = lubridate::year(sub_date)
  )

camp_appearance_counts <- camps |>
  dplyr::filter(!is.na(camp_code), trimws(camp_code) != "") |>
  dplyr::count(camp_code, camp_name, district, name = "appearances") |>
  dplyr::arrange(dplyr::desc(appearances), camp_code)
write_csv_safely(camp_appearance_counts, "camp_appearance_counts.csv", row.names = FALSE)

camp_appearance_counts_by_source <- camps |>
  dplyr::filter(!is.na(camp_code), trimws(camp_code) != "") |>
  dplyr::count(data_source, camp_code, camp_name, district, name = "appearances") |>
  dplyr::arrange(data_source, dplyr::desc(appearances), camp_code)
write_csv_safely(camp_appearance_counts_by_source,
                 "camp_appearance_counts_by_source.csv", row.names = FALSE)
message("  Wrote camp appearance summaries to the working directory")

# ruodk_names: transport_type_transport_infrastructure, transport_type_migratory_route,
#              transport_name_migratory_route
transport <- camps |>
  dplyr::select(id, year,
                dplyr::any_of(c("transport_infra", "route_type", "route_name"))
  ) |>
  dplyr::filter(!is.na(route_name), trimws(route_name) != "")



# ── Outreach sessions cleaning ────────────────────────────────────────────────

outreach <- outreach_raw |>
  dplyr::mutate(
    out_district   = normalise_district(a_district),
    zd_ri          = suppressWarnings(as.numeric(zd_ri)),
    zd_polio       = suppressWarnings(as.numeric(zd_polio)),
    zd_vaccinated  = suppressWarnings(as.numeric(zd_vaccinated)),
    
    # Backward-compatibility alias
    vaccinated     = zd_vaccinated,
    
    out_date       = lubridate::as_date(dplyr::coalesce(
      suppressWarnings(lubridate::ymd(as.character(out_date))),
      lubridate::as_date(system_submission_date)
    )),
    year           = lubridate::year(out_date),
    month_num      = lubridate::month(out_date),
    month_label    = format(out_date, "%b-%y")
  )
message("  Outreach cols after cleaning: ", paste(sort(names(outreach)), collapse = ", "))

# Spatial district assignment for outreach
if (!is.null(districts_sf) &&
    all(c("out_lat", "out_lon") %in% names(outreach))) {
  out_coords <- outreach |>
    dplyr::filter(!is.na(out_lat) & !is.na(out_lon) & out_lat != 0)
  if (nrow(out_coords) > 0) {
    out_sf <- sf::st_as_sf(out_coords,
                           coords = c("out_lon", "out_lat"), crs = 4326, remove = FALSE)
    out_joined <- sf::st_join(out_sf, districts_sf, join = sf::st_within) |>
      sf::st_drop_geometry() |>
      dplyr::select(id, spatial_out_district = district_name)
    
    outreach <- outreach |>
      dplyr::left_join(out_joined, by = "id") |>
      dplyr::mutate(
        # Prefer spatial district if available; otherwise fall back to original
        out_district = dplyr::coalesce(spatial_out_district, out_district),
        
        # Normalize final district values (critical for eliminating duplicates)
        out_district = normalise_district(out_district),
        
        # Trim whitespace to prevent "Belet-hawa " vs "Belet-hawa"
        out_district = trimws(out_district)
      )
  }
}

message("── [6/6] Aggregating dashboard objects ──")

years <- c("all", "2024", "2025", "2026")

filter_year <- function(df, yr, date_col = "year") {
  if (yr == "all") df
  else df |> dplyr::filter(.data[[date_col]] == as.integer(yr))
}

# ── KPI aggregates ────────────────────────────────────────────────────────────
kpi <- purrr::map(years, function(yr) {
  c_yr <- filter_year(camps, yr)
  o_yr <- filter_year(outreach, yr)
  list(
    camps      = nrow(c_yr),
    pop        = sum(c_yr$pop_0to11 + c_yr$pop_12to59, na.rm = TRUE),
    zd_total   = sum(c_yr$zd_0to11 + c_yr$zd_12to59, na.rm = TRUE),
    zd_011     = sum(c_yr$zd_0to11,  na.rm = TRUE),
    zd_1259    = sum(c_yr$zd_12to59, na.rm = TRUE),
    sessions   = nrow(o_yr),
    vaccinated = sum(o_yr$zd_vaccinated, na.rm = TRUE)  # alias
  )
}) |> purrr::set_names(years)


# ── ZD by district ────────────────────────────────────────────────────────────
zd_by_dist <- purrr::map(years, function(yr) {
  filter_year(camps, yr) |>
    dplyr::group_by(district) |>
    dplyr::summarise(
      camps   = dplyr::n(),
      t011    = sum(pop_0to11,  na.rm = TRUE),
      z011    = sum(zd_0to11,   na.rm = TRUE),
      t1259   = sum(pop_12to59, na.rm = TRUE),
      z1259   = sum(zd_12to59,  na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      rate011  = safe_pct(z011,  t011),
      rate1259 = safe_pct(z1259, t1259)
    ) |>
    dplyr::filter(!is.na(district))
}) |> purrr::set_names(years)

# ── Movement flows ────────────────────────────────────────────────────────────
flows <- purrr::map(years, function(yr) {
  filter_year(camps, yr) |>
    dplyr::filter(!is.na(prev_district), !is.na(next_district)) |>
    dplyr::group_by(prev_c = prev_district, next_c = next_district) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
}) |> purrr::set_names(years)

# ── Indegree centrality ───────────────────────────────────────────────────────
indegree <- purrr::map(years, function(yr) {
  filter_year(camps, yr) |>
    dplyr::filter(!is.na(next_district)) |>
    dplyr::count(next_district, name = "count") |>
    dplyr::arrange(dplyr::desc(count)) |>
    dplyr::rename(district = next_district)
}) |> purrr::set_names(years)

# ── Seasons ───────────────────────────────────────────────────────────────────
seasons <- purrr::map(years, function(yr) {
  s <- filter_year(camps, yr) |>
    dplyr::filter(!is.na(season)) |>
    dplyr::count(season, name = "count") |>
    dplyr::mutate(pct = round(100 * count / sum(count)))
  purrr::map(c("Deyr", "Guul", "Jilaal", "Xaaga"), function(s_name) {
    row <- s[s$season == s_name, ]
    list(count = if (nrow(row) > 0) row$count else 0L,
         pct   = if (nrow(row) > 0) row$pct   else 0L)
  }) |> purrr::set_names(c("Deyr", "Guul", "Jilaal", "Xaaga"))
}) |> purrr::set_names(years)

# ── Top migratory routes ──────────────────────────────────────────────────────
routes <- purrr::map(years, function(yr) {
  d <- filter_year(camps, yr) |>
    dplyr::filter(!is.na(route_name), trimws(route_name) != "") |>
    dplyr::count(route_name, name = "count") |>
    dplyr::arrange(dplyr::desc(count)) |>
    dplyr::slice_head(n = 10)
  if (nrow(d) == 0) return(list())
  lapply(seq_len(nrow(d)), function(i) list(n = d$route_name[i], c = d$count[i]))
}) |> purrr::set_names(years)

# ── Cross-border (Dolow + Elwak camp counts) ─────────────────────────────────
cross_border <- purrr::map(years, function(yr) {
  c_yr <- filter_year(camps, yr)
  list(
    dolow = sum(!is.na(c_yr$prev_district) & c_yr$prev_district == "Dolow", na.rm = TRUE),
    elwak = sum(!is.na(c_yr$prev_district) & c_yr$prev_district == "Elwak", na.rm = TRUE)
  )
}) |> purrr::set_names(years)

# ── Outreach monthly trend ────────────────────────────────────────────────────
out_monthly <- outreach |>
  dplyr::filter(!is.na(out_date)) |>
  dplyr::group_by(month_label, year, month_num) |>
  dplyr::summarise(
    sessions    = dplyr::n(),
    zd_vaccinated = sum(zd_vaccinated, na.rm = TRUE),
    .groups     = "drop"
  ) |>
  dplyr::arrange(year, month_num) |>
  dplyr::mutate(
    month_label = factor(month_label, levels = unique(month_label), ordered = TRUE),
    vaccinated  = zd_vaccinated        # <-- alias for UI modules
  )

# ── Outreach by district (per year) ──────────────────────────────────────────
out_by_dist <- purrr::map(years, function(yr) {
  filter_year(outreach, yr) |>
    dplyr::group_by(district = out_district) |>
    dplyr::summarise(
      sessions      = dplyr::n(),
      zd_vaccinated = sum(zd_ri + zd_polio, na.rm = TRUE),
      zd_ri         = sum(zd_ri,      na.rm = TRUE),
      zd_polio      = sum(zd_polio,   na.rm = TRUE),
      vaccinated    = zd_vaccinated,              # <-- alias
      .groups       = "drop"
    ) |>
    dplyr::filter(!is.na(district))
}) |> purrr::set_names(years)


# ── Performance table (ZD identified vs vaccinated by outreach) ───────────────
perf_table <- purrr::map(years, function(yr) {
  
  # ZD camps identified
  zd <- zd_by_dist[[yr]] |>
    dplyr::mutate(
      district = normalise_district(trimws(as.character(district)))
    ) |>
    dplyr::group_by(district) |>
    dplyr::summarise(
      zd_id = sum(z011, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ZD vaccinated outreach
  out <- out_by_dist[[yr]] |>
    dplyr::mutate(
      district = normalise_district(trimws(as.character(district)))
    ) |>
    dplyr::group_by(district) |>
    dplyr::summarise(
      sessions      = sum(sessions, na.rm = TRUE),
      zd_vaccinated = sum(zd_vaccinated, na.rm = TRUE),
      .groups = "drop"
    )
  
  # coverage formula
  dplyr::full_join(zd, out, by = "district") |>
    dplyr::mutate(
      zd_id         = tidyr::replace_na(zd_id, 0L),
      sessions      = tidyr::replace_na(sessions, 0L),
      zd_vaccinated = tidyr::replace_na(zd_vaccinated, 0L),
      
      # Keep alias so UI modules expecting "vaccinated" still work
      vaccinated    = zd_vaccinated,
      
      gap           = pmax(zd_id - zd_vaccinated, 0L),
      cov_pct       = safe_pct(zd_vaccinated, zd_id)
    ) |>
    
    # double robust naming
    dplyr::group_by(district) |>
    dplyr::summarise(
      zd_id         = sum(zd_id, na.rm = TRUE),
      sessions      = sum(sessions, na.rm = TRUE),
      zd_vaccinated = sum(zd_vaccinated, na.rm = TRUE),
      vaccinated    = sum(vaccinated, na.rm = TRUE),
      gap           = sum(gap, na.rm = TRUE),
      cov_pct       = safe_pct(vaccinated, zd_id),
      .groups       = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(gap))
}) |> purrr::set_names(years)

district_coverage_gap <- dplyr::bind_rows(perf_table, .id = "year") |>
  dplyr::select(year, district, zd_identified = zd_id,
                vaccinated, gap, coverage_pct = cov_pct)
if (write_csv_safely(district_coverage_gap, "district_coverage_gap.csv", row.names = FALSE)) {
  message("  Saved District Coverage Gap table to district_coverage_gap.csv")
}


# ── ZD-RI vs ZD-Polio by district from outreach report ────────────────────────
zd_ri_pol <- purrr::map(years, function(yr) {
  out_by_dist[[yr]] |>
    dplyr::select(district, ri = zd_ri, pol = zd_polio) |>
    dplyr::filter(ri > 0 | pol > 0) |>
    dplyr::arrange(dplyr::desc(ri + pol))
}) |> purrr::set_names(years)

# ── Access: HF travel time buckets ───────────────────────────────────────────
hf_access <- purrr::map(years, function(yr) {
  filter_year(camps, yr) |>
    dplyr::filter(!is.na(hf_travel_min)) |>
    dplyr::mutate(
      bucket = dplyr::case_when(
        hf_travel_min <  30  ~ "<30 min",
        hf_travel_min <  60  ~ "30–60 min",
        hf_travel_min < 120  ~ "60–120 min",
        TRUE                 ~ "120+ min"
      )
    ) |>
    dplyr::count(bucket, name = "n") |>
    dplyr::mutate(pct = round(100 * n / sum(n)))
}) |> purrr::set_names(years)

# ── Challenge categories ──────────────────────────────────────────────────────
# Field is free-text multi-response. Map to canonical categories per field guide.
categorise_challenge <- function(x) {
  x <- tolower(trimws(x))
  cats <- c(
    "No nearby HF"          = "no.*(hf|health|facilit|clinic|hospital)",
    "Distance to HF"        = "dist|far|remote|kilomet|km|long way",
    "Lack of transport"     = "transport|vehicle|car|road|access",
    "Security concerns"     = "securi|insecur|conflict|risk|danger|armed",
    "Community awareness"   = "aware|knowled|inform|educat|communit",
    "Vaccinators absent"    = "vaccin.*absent|no.*vaccin|vaccin.*not|staff.*absent|absent.*vaccin",
    "Other"                 = "."
  )
  # Return first matching category
  for (nm in names(cats)) {
    if (grepl(cats[nm], x, ignore.case = TRUE)) return(nm)
  }
  "Other"
}

challenges <- purrr::map(years, function(yr) {
  d <- filter_year(camps, yr) |>
    dplyr::filter(!is.na(challenge), trimws(challenge) != "")
  if (nrow(d) == 0) return(data.frame(challenge = character(), n = integer()))
  d |>
    dplyr::mutate(
      challenge_cat = sapply(challenge, categorise_challenge)
    ) |>
    dplyr::count(challenge = challenge_cat, name = "n") |>
    dplyr::arrange(dplyr::desc(n))
}) |> purrr::set_names(years)

# ── Settlement / transport / route type aggregations (for Access tab donuts) ──
settlement_counts <- camps |>
  dplyr::filter(!is.na(settlement_type), trimws(settlement_type) != "") |>
  dplyr::mutate(
    cat = dplyr::case_when(
      grepl("settlement|urban|town|village", settlement_type, ignore.case = TRUE) ~ "Settlement",
      grepl("camp|idp|temp", settlement_type, ignore.case = TRUE) ~ "Camp",
      TRUE ~ "Other"
    )
  ) |>
  dplyr::count(cat, name = "n")

transport_counts <- camps |>
  dplyr::filter(!is.na(transport_infra), trimws(transport_infra) != "") |>
  dplyr::mutate(
    cat = dplyr::case_when(
      grepl("livestock|foot|walk|camel|donkey", transport_infra, ignore.case = TRUE) ~ "Livestock/foot",
      grepl("bus|truck|car|vehicle|motor", transport_infra, ignore.case = TRUE) ~ "Bus/vehicle",
      TRUE ~ "Other"
    )
  ) |>
  dplyr::count(cat, name = "n")

route_type_counts <- camps |>
  dplyr::filter(!is.na(route_type), trimws(route_type) != "") |>
  dplyr::mutate(
    cat = dplyr::case_when(
      grepl("foot|path|track|trail|bush", route_type, ignore.case = TRUE) ~ "Footpath",
      grepl("highway|main|road|tarmac|paved", route_type, ignore.case = TRUE) ~ "Highway",
      grepl("border|crossing|cross", route_type, ignore.case = TRUE) ~ "Border crossing",
      TRUE ~ "Other"
    )
  ) |>
  dplyr::count(cat, name = "n")


# ── District monthly vaccinated (top 5 districts by total) ───────────────────
top5_dists <- outreach |>
  dplyr::filter(!is.na(out_district)) |>
  dplyr::group_by(out_district) |>
  dplyr::summarise(total = sum(zd_vaccinated, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(total)) |>
  dplyr::slice_head(n = 5) |>
  dplyr::pull(out_district)

dist_monthly <- outreach |>
  dplyr::filter(!is.na(out_date), out_district %in% top5_dists) |>
  dplyr::group_by(out_district, month_label, year, month_num) |>
  dplyr::summarise(
    zd_vaccinated = sum(zd_vaccinated, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(year, month_num) |>
  dplyr::mutate(
    month_label = factor(month_label,
                         levels  = levels(out_monthly$month_label),
                         ordered = TRUE),
    vaccinated  = zd_vaccinated
  )

camp_gps <- camps |>
  dplyr::filter(!test_district, !is.na(lat), !is.na(lon), lat != 0, lon != 0) |>
  dplyr::select(id, lat, lon, district = enum_district, year,
                zd_0to11, zd_12to59, pop_0to11, pop_12to59)

if (!is.null(districts_sf) && nrow(camp_gps) > 0) {
  camp_gps_sf <- sf::st_as_sf(camp_gps,
                              coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  inside_somalia <- lengths(sf::st_intersects(camp_gps_sf, districts_sf)) > 0

  district_means <- sf::st_join(
    camp_gps_sf[inside_somalia, ],
    dplyr::select(districts_sf, district_name),
    join = sf::st_within,
    left = FALSE
  ) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(polygon_district = normalise_district(district_name)) |>
    dplyr::filter(!is.na(district), district == polygon_district) |>
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::group_by(district) |>
    dplyr::summarise(
      mean_lat = mean(lat, na.rm = TRUE),
      mean_lon = mean(lon, na.rm = TRUE),
      .groups = "drop"
    )

  camp_gps <- camp_gps |>
    dplyr::mutate(outside_somalia = !inside_somalia) |>
    dplyr::left_join(district_means, by = "district") |>
    dplyr::mutate(
      coordinates_replaced = outside_somalia & !is.na(mean_lat) & !is.na(mean_lon),
      lat = dplyr::if_else(coordinates_replaced, mean_lat, lat),
      lon = dplyr::if_else(coordinates_replaced, mean_lon, lon)
    ) |>
    dplyr::select(-outside_somalia, -mean_lat, -mean_lon)
}


out_gps <- outreach |>
  dplyr::filter(!is.na(out_lat), !is.na(out_lon), out_lat != 0) |>
  dplyr::select(
    id, lat = out_lat, lon = out_lon,
    district = out_district, year,
    vaccinated = zd_vaccinated,
    month_label
  ) |>
  dplyr::mutate(
    month_label = factor(month_label,
                         levels = levels(out_monthly$month_label),
                         ordered = TRUE)
  )

# ── District centroids for flow arrows ───────────────────────────────────────
camp_centroids <- camp_gps |>
  dplyr::mutate(
    district = normalise_district(district),
    lon = suppressWarnings(as.numeric(lon)),
    lat = suppressWarnings(as.numeric(lat))
  ) |>
  dplyr::filter(!is.na(lon), !is.na(lat), !is.na(district)) |>
  dplyr::group_by(district) |>
  dplyr::summarise(
    lon = median(lon, na.rm = TRUE),
    lat = median(lat, na.rm = TRUE),
    n_camps = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::select(district, lon, lat)

poly_centroids <- if (!is.null(districts_sf)) {
  districts_sf |>
    sf::st_point_on_surface() |>
    dplyr::mutate(
      lon = sf::st_coordinates(geometry)[, 1],
      lat = sf::st_coordinates(geometry)[, 2]
    ) |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      district = normalise_district(district_name),
      lon, lat
    )
} else {
  # Fallback hardcoded
  tibble::tribble(
    ~district,      ~lon,     ~lat,
    normalise_district("Baardhere"),  42.2851,  2.3646,
    normalise_district("Elwak"),      41.0270,  2.7771,
    normalise_district("Garbaharey"), 42.2180,  3.3309,
    normalise_district("Dolow"),      42.1149,  4.0953,
    normalise_district("Belet-hawa"), 41.9512,  3.9346,
    normalise_district("Luuq"),       42.5203,  3.8083,
    normalise_district("El Barde"),   43.6874,  4.8286,
    normalise_district("Baidoa"),     43.6509,  3.1521,
    normalise_district("Beledweyne"), 45.2115,  4.7732
  )
}

# Use camp centroids when available; fill gaps with polygon anchors
centroids <- camp_centroids |>
  dplyr::rows_append(
    poly_centroids |>
      dplyr::anti_join(camp_centroids, by = "district")
  )

# Use camp centroids when present; fill any missing districts with polygon anchors
centroids <- camp_centroids |>
  dplyr::rows_append(
    poly_centroids |>
      dplyr::anti_join(camp_centroids, by = "district")
  )


# ── District + state GeoJSON for leaflet ─────────────────────────────────────
if (!is.null(districts_sf)) {
  districts_geojson <- sf::st_simplify(
    districts_sf, dTolerance = 0.003, preserveTopology = TRUE
  ) |> sf::st_transform(4326)
  
  # Dissolve districts > state boundaries using confirmed NAME_L1 field
  states_geojson <- districts_sf |>
    dplyr::group_by(state_name) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
    sf::st_simplify(dTolerance = 0.003, preserveTopology = TRUE) |>
    sf::st_transform(4326)
} else {
  districts_geojson <- NULL
  states_geojson    <- NULL
}

# ── Bundle and save ───────────────────────────────────────────────────────────
nomad_data <- list(
  kpi               = kpi,
  zd_by_dist        = zd_by_dist,
  flows             = flows,
  indegree          = indegree,
  seasons           = seasons,
  routes            = routes,
  cross_border      = cross_border,
  out_monthly       = out_monthly,
  dist_monthly      = dist_monthly,
  out_by_dist       = out_by_dist,
  perf_table        = perf_table,
  zd_ri_pol         = zd_ri_pol,
  hf_access         = hf_access,
  challenges        = challenges,
  settlement_counts = settlement_counts,
  transport_counts  = transport_counts,
  route_type_counts = route_type_counts,
  camp_gps          = camp_gps,
  out_gps           = out_gps,
  centroids        = centroids,
  districts_geojson = districts_geojson,
  states_geojson    = states_geojson,
  pulled_at         = Sys.time(),
  camp_source_cutoff = camp_cutoff,
  latest_submission = max(
    suppressWarnings(lubridate::as_datetime(camps$system_submission_date)),
    na.rm = TRUE
  )
)

message("✓ Built nomad_data in memory (", nrow(camps), " camp records, ",
        nrow(outreach), " outreach sessions)")
saveRDS(nomad_data, "data/nomad_data.rds")
message("  Saved data/nomad_data.rds")