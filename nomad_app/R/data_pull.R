# R/data_pull.R
# ─────────────────────────────────────────────────────────────────────────────
# Fetches both ODK forms, joins to ArcGIS district geometry, aggregates all
# dashboard data objects, and writes data/nomad_data.rds
#
# Run locally:  source("R/data_pull.R")   or   source("refresh_data.R")
# ─────────────────────────────────────────────────────────────────────────────

library(ruODK)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(httr)
library(jsonlite)
library(stringr)
library(purrr)

source("R/utils.R")

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
)

# Transport is a flat group in this form — no repeat table to fetch

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

message("── [5/6] Cleaning and joining ──")

# ── Camp enumeration cleaning ─────────────────────────────────────────────────
# Field name aliases — ruODK flattens group-repeat paths with underscores or
# dots depending on version. We use rename_with + a flexible matcher.
rename_flexible <- function(df, pattern, new_name) {
  col <- grep(pattern, names(df), value = TRUE, ignore.case = TRUE)[1]
  if (!is.na(col) && col != new_name) dplyr::rename(df, !!new_name := dplyr::all_of(col))
  else df
}

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
  rename_flexible("group_profile_enumerator_district",   "enum_district") |>
  # GPS — ruODK splits geopoint; raw string col is community_level_gps exactly
  rename_flexible("^community_level_gps$",               "gps_raw") |>
  # Population — under /polio/ group
  rename_flexible("polio_camp_pop_0to11_months$",        "pop_0to11") |>
  rename_flexible("polio_camp_pop_12to59_months$",       "pop_12to59") |>
  rename_flexible("polio_camp_tot_pop",                  "pop_total") |>
  # Zero-dose — ruODK lowercases camelCase: zeroDose → zero_dose
  rename_flexible("polio_camp_pop_0to11_months_zero_dose",  "zd_0to11") |>
  rename_flexible("polio_camp_pop_12to59_months_zero_dose", "zd_12to59") |>
  # Challenges — ruODK lowercases to polio_camp_previous_service_challenges
  rename_flexible("polio_camp_previous_service_challenges", "challenge") |>
  # Transport (flat group, not repeat)
  rename_flexible("transport_type_transport_infrastructure", "transport_infra") |>
  rename_flexible("transport_type_migratory_route",         "route_type") |>
  rename_flexible("transport_name_migratory_route",         "route_name") |>
  # District fields from tribe_subtribe group (fallback)
  rename_flexible("tribe_subtribe_district",             "recorded_district") |>
  dplyr::mutate(
    # Coerce numeric
    pop_0to11     = suppressWarnings(as.numeric(pop_0to11)),
    pop_12to59    = suppressWarnings(as.numeric(pop_12to59)),
    pop_total     = suppressWarnings(as.numeric(pop_total)),
    zd_0to11      = suppressWarnings(as.numeric(zd_0to11)),
    zd_12to59     = suppressWarnings(as.numeric(zd_12to59)),
    hf_travel_min = suppressWarnings(as.numeric(hf_travel_min)),
    # exit_date is free-text ("August 2025", "Deyr 2024" etc.) — extract season directly
    exit_date_raw = as.character(exit_date),
    season = dplyr::case_when(
      grepl("deyr|oct|nov",          exit_date_raw, ignore.case = TRUE) ~ "Deyr",
      grepl("guul|guu|mar|apr|may",  exit_date_raw, ignore.case = TRUE) ~ "Guu'l",
      grepl("jilaal|dec|jan|feb",    exit_date_raw, ignore.case = TRUE) ~ "Jilaal",
      grepl("xaaga|hagaa|jun|jul|aug|sep", exit_date_raw, ignore.case = TRUE) ~ "Xaaga",
      TRUE ~ NA_character_
    ),
    # Normalise recorded district names via crosswalk
    next_district    = normalise_district(next_district),
    prev_district    = normalise_district(prev_district),
    enum_district    = normalise_district(
      dplyr::coalesce(enum_district, recorded_district)
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

# ── Spatial join: assign district from GPS, fall back to recorded ─────────────
if (!is.null(districts_sf) && "lat" %in% names(camps) && "lon" %in% names(camps)) {
  camps_with_coords <- camps |>
    dplyr::filter(!is.na(lat) & !is.na(lon) & lat != 0 & lon != 0)
  
  camps_sf <- sf::st_as_sf(camps_with_coords,
                           coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  joined <- sf::st_join(camps_sf, districts_sf, join = sf::st_within) |>
    sf::st_drop_geometry() |>
    dplyr::select(id, spatial_district = district_name)
  
  camps <- camps |>
    dplyr::left_join(joined, by = "id") |>
    dplyr::mutate(
      district = dplyr::coalesce(spatial_district, enum_district)
    )
} else {
  camps <- camps |> dplyr::mutate(district = enum_district)
}

# Submission year — use today.start (ruODK standard) or system_submission_date
if ("today.start" %in% names(camps)) {
  camps <- camps |>
    dplyr::mutate(
      sub_date = lubridate::ymd(as.character(today.start)),
      year     = lubridate::year(sub_date)
    )
} else if ("system_submission_date" %in% names(camps)) {
  camps <- camps |>
    dplyr::mutate(
      sub_date = lubridate::as_date(system_submission_date),
      year     = lubridate::year(sub_date)
    )
}

# Transport is a FLAT GROUP (not a repeat) — fields land directly in camps_raw
# ruodk_names: transport_type_transport_infrastructure, transport_type_migratory_route,
#              transport_name_migratory_route
transport <- camps |>
  dplyr::select(id, year,
                dplyr::any_of(c("transport_infra", "route_type", "route_name"))
  ) |>
  dplyr::filter(!is.na(route_name), trimws(route_name) != "")

# ── Outreach sessions cleaning ────────────────────────────────────────────────
outreach <- outreach_raw |>
  # ── Exact ruodk_name mappings confirmed from form_schema() ──────────────────
  # District — at /A/district → ruodk_name a_district
  rename_flexible("^a_district$",                        "out_district_raw") |>
  # GPS — Geo_reference_hh geopoint → ruodk_name geo_reference_hh
  rename_flexible("geo_reference_hh",                    "out_gps_raw") |>
  # Total children vaccinated at session — /B/Total_number_of_chil...
  rename_flexible("b_total_number_of_chil",              "vaccinated") |>
  # ZD RI excl. polio — /C/Number_of_children_w_nd_in_the_settlement (first)
  # ZD Polio          — /C/Number_of_children_w_nd_in_the_settlement_001
  rename_flexible("c_number_of_childr.*settlement$",     "zd_ri") |>
  rename_flexible("c_number_of_childr.*settlement_001",  "zd_polio") |>
  # Date
  rename_flexible("^today$",                             "out_date") |>
  dplyr::mutate(
    out_district = normalise_district(out_district_raw),
    zd_ri        = suppressWarnings(as.numeric(zd_ri)),
    zd_polio     = suppressWarnings(as.numeric(zd_polio)),
    vaccinated   = suppressWarnings(as.numeric(vaccinated)),
    out_date     = lubridate::as_date(coalesce(
      suppressWarnings(lubridate::ymd(as.character(out_date))),
      lubridate::as_date(system_submission_date)
    )),
    year         = lubridate::year(out_date),
    month_num    = lubridate::month(out_date),
    month_label  = format(out_date, "%b-%y")
  )

# Extract outreach GPS
# ruODK splits geopoint into *-Latitude / *-Longitude columns
out_lat_col <- grep("geo_reference.*latitude|geo_reference.*lat$",
                    names(outreach), ignore.case = TRUE, value = TRUE)[1]
out_lon_col <- grep("geo_reference.*longitude|geo_reference.*lon$",
                    names(outreach), ignore.case = TRUE, value = TRUE)[1]

if (!is.na(out_lat_col) && !is.na(out_lon_col)) {
  outreach <- outreach |>
    dplyr::mutate(
      out_lat = suppressWarnings(as.numeric(.data[[out_lat_col]])),
      out_lon = suppressWarnings(as.numeric(.data[[out_lon_col]]))
    )
} else if ("out_gps_raw" %in% names(outreach)) {
  outreach <- outreach |>
    dplyr::mutate(
      out_lat = suppressWarnings(as.numeric(purrr::map_chr(
        strsplit(as.character(out_gps_raw), " "), ~ .x[1]))),
      out_lon = suppressWarnings(as.numeric(purrr::map_chr(
        strsplit(as.character(out_gps_raw), " "), ~ .x[2])))
    )
} else {
  outreach <- outreach |> dplyr::mutate(out_lat = NA_real_, out_lon = NA_real_)
  message("  WARNING: no GPS columns found in outreach form")
}

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
      dplyr::mutate(out_district = dplyr::coalesce(spatial_out_district, out_district))
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
    vaccinated = sum(o_yr$vaccinated, na.rm = TRUE)
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
  filter_year(camps, yr) |>
    dplyr::filter(!is.na(route_name), trimws(route_name) != "") |>
    dplyr::count(route_name, name = "count") |>
    dplyr::arrange(dplyr::desc(count)) |>
    dplyr::slice_head(n = 10) |>
    purrr::transpose() |>
    purrr::map(~ list(n = .x$route_name, c = .x$count))
}) |> purrr::set_names(years)

# ── Cross-border (Dolow + Elwak camp counts) ─────────────────────────────────
cross_border <- purrr::map(years, function(yr) {
  c_yr <- filter_year(camps, yr)
  list(
    dolow = sum(c_yr$district == "Dolow", na.rm = TRUE),
    elwak = sum(c_yr$district == "Elwak", na.rm = TRUE)
  )
}) |> purrr::set_names(years)

# ── Outreach monthly trend ────────────────────────────────────────────────────
out_monthly <- outreach |>
  dplyr::filter(!is.na(out_date)) |>
  dplyr::group_by(month_label, year, month_num) |>
  dplyr::summarise(
    sessions   = dplyr::n(),
    vaccinated = sum(vaccinated, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  dplyr::arrange(year, month_num)

# ── Outreach by district (per year) ──────────────────────────────────────────
out_by_dist <- purrr::map(years, function(yr) {
  filter_year(outreach, yr) |>
    dplyr::group_by(district = out_district) |>
    dplyr::summarise(
      sessions   = dplyr::n(),
      vaccinated = sum(vaccinated, na.rm = TRUE),
      zd_ri      = sum(zd_ri,      na.rm = TRUE),
      zd_polio   = sum(zd_polio,   na.rm = TRUE),
      .groups    = "drop"
    ) |>
    dplyr::filter(!is.na(district))
}) |> purrr::set_names(years)

# ── Performance table (ZD identified vs vaccinated by outreach) ───────────────
perf_table <- purrr::map(years, function(yr) {
  zd  <- zd_by_dist[[yr]] |> dplyr::select(district, zd_id = z011)
  out <- out_by_dist[[yr]] |> dplyr::select(district, sessions, vaccinated)
  dplyr::full_join(zd, out, by = "district") |>
    dplyr::mutate(
      zd_id      = tidyr::replace_na(zd_id,    0L),
      sessions   = tidyr::replace_na(sessions,  0L),
      vaccinated = tidyr::replace_na(vaccinated, 0L),
      gap        = pmax(zd_id - vaccinated, 0L),
      cov_pct    = safe_pct(vaccinated, zd_id)
    )
}) |> purrr::set_names(years)

# ── ZD-RI vs ZD-Polio by district (outreach-reported) ────────────────────────
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
challenges <- purrr::map(years, function(yr) {
  filter_year(camps, yr) |>
    dplyr::filter(!is.na(challenge), challenge != "") |>
    dplyr::count(challenge, name = "n") |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::slice_head(n = 10)
}) |> purrr::set_names(years)

# ── GPS points for maps ───────────────────────────────────────────────────────
camp_gps <- camps |>
  dplyr::filter(!is.na(lat), !is.na(lon), lat != 0, lon != 0) |>
  dplyr::select(id, lat, lon, district, year,
                zd_0to11, zd_12to59, pop_0to11, pop_12to59)

out_gps <- outreach |>
  dplyr::filter(!is.na(out_lat), !is.na(out_lon), out_lat != 0) |>
  dplyr::select(id, lat = out_lat, lon = out_lon,
                district = out_district, year, vaccinated,
                month_label)

# ── District centroids for flow arrows ───────────────────────────────────────
centroids <- if (!is.null(districts_sf)) {
  districts_sf |>
    sf::st_centroid() |>
    dplyr::mutate(
      lon = sf::st_coordinates(geometry)[, 1],
      lat = sf::st_coordinates(geometry)[, 2]
    ) |>
    sf::st_drop_geometry() |>
    dplyr::select(district = district_name, lat, lon)
} else {
  # Fallback hardcoded centroids from the HTML
  tibble::tribble(
    ~district,    ~lat,    ~lon,
    "Baardhere",  2.3646,  42.2851,
    "Elwak",      2.7771,  41.0270,
    "Garbaharey", 3.3309,  42.2180,
    "Dolow",      4.0953,  42.1149,
    "Belet-hawa", 3.9346,  41.9512,
    "Luuq",       3.8083,  42.5203,
    "El Barde",   4.8286,  43.6874,
    "Baidoa",     3.1521,  43.6509,
    "Beledweyne", 4.7732,  45.2115
  )
}

# ── District + state GeoJSON for leaflet ─────────────────────────────────────
if (!is.null(districts_sf)) {
  districts_geojson <- sf::st_simplify(
    districts_sf, dTolerance = 0.003, preserveTopology = TRUE
  ) |> sf::st_transform(4326)
  
  # Dissolve districts → state boundaries using confirmed NAME_L1 field
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
  kpi              = kpi,
  zd_by_dist       = zd_by_dist,
  flows            = flows,
  indegree         = indegree,
  seasons          = seasons,
  routes           = routes,
  cross_border     = cross_border,
  out_monthly      = out_monthly,
  out_by_dist      = out_by_dist,
  perf_table       = perf_table,
  zd_ri_pol        = zd_ri_pol,
  hf_access        = hf_access,
  challenges       = challenges,
  camp_gps         = camp_gps,
  out_gps          = out_gps,
  centroids        = centroids,
  districts_geojson = districts_geojson,
  states_geojson    = states_geojson,
  pulled_at        = Sys.time()
)

if (!dir.exists("data")) dir.create("data")
saveRDS(nomad_data, "data/nomad_data.rds")
message("✓ Saved data/nomad_data.rds (", nrow(camps), " camp records, ",
        nrow(outreach), " outreach sessions)")