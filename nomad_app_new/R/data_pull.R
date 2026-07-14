# R/data_pull.R
# ─────────────────────────────────────────────────────────────────────────────
# Fetches both ODK forms, joins to ArcGIS district geometry, aggregates all
# dashboard data objects, and writes data/nomad_data.rds
#
# Run locally:  source("odk_credentials.R"); source("refresh_data.R")
# Called by global.R automatically when ODK has newer data
# ─────────────────────────────────────────────────────────────────────────────

for (pkg in c("dplyr","tidyr","lubridate","sf","httr","jsonlite","stringr","purrr","readxl")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

if (!exists("normalise_district", mode = "function")) source("R/utils.R")

# ─────────────────────────────────────────────────────────────────────────────
# HELPER: flexible column rename
# ─────────────────────────────────────────────────────────────────────────────
rename_flexible <- function(df, pattern, new_name) {
  col <- grep(pattern, names(df), value = TRUE, ignore.case = TRUE)[1]
  if (!is.na(col) && col != new_name) dplyr::rename(df, !!new_name := dplyr::all_of(col))
  else df
}

# ─────────────────────────────────────────────────────────────────────────────
# HELPER: odk_odata_get() — replaces ruODK::odata_submission_get()
# Direct httr/jsonlite OData fetch. Avoids ruODK's dependency chain
# (sf -> raster -> terra) which fails to build on shinyapps.io (GDAL
# API mismatch). Same fix pattern used for the RAAD Somalia app.
#
# NOTE: ruODK does its own flattening + name-cleaning (dots -> underscores,
# array indices -> _1/_2 suffixes) on top of what jsonlite gives you. This
# reproduces that convention but has NOT been verified against a live
# ODK Central response for this specific form — check names(df) against
# the rename_flexible() patterns below after first run and adjust if the
# field names don't line up (especially any geopoint fields, which ODK
# encodes as GeoJSON {type, coordinates:[lon,lat,alt]} sub-objects).
# ─────────────────────────────────────────────────────────────────────────────
odk_odata_get <- function(svc, un, pw) {
  pages <- list()
  url <- paste0(svc, "/Submissions")
  repeat {
    resp <- httr::GET(url, httr::authenticate(un, pw), httr::timeout(120))
    httr::stop_for_status(resp)
    parsed <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      flatten = FALSE
    )
    if (!is.null(parsed$value) && length(parsed$value) > 0) {
      pages[[length(pages) + 1]] <- parsed$value
    }
    next_link <- parsed[["@odata.nextLink"]]
    if (is.null(next_link) || length(next_link) == 0) break
    url <- next_link
  }
  if (length(pages) == 0) return(dplyr::tibble())
  df <- dplyr::bind_rows(pages)
  
  # jsonlite's flatten=TRUE (inside fromJSON) only resolves ONE level of
  # nesting. ODK groups can nest 2+ levels deep (e.g. community_level ->
  # camp -> next_adm3), so flatten repeatedly until no nested data.frame
  # columns remain.
  repeat {
    is_nested <- vapply(df, is.data.frame, logical(1))
    if (!any(is_nested)) break
    df <- jsonlite::flatten(df)
  }
  
  names(df) <- gsub("[.]", "_", names(df))
  
  # ODK Central's OData feed uses double-underscore system fields
  # (__id, __system.submissionDate, ...) — ruODK renamed these to
  # friendlier names (id, system_submission_date) which the rest of
  # this pipeline depends on throughout. Reproduce that here.
  if ("__id" %in% names(df)) {
    df <- dplyr::rename(df, id = `__id`)
  }
  sub_date_col <- grep("^__system_submissiondate$", names(df), ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(sub_date_col)) {
    df <- dplyr::rename(df, system_submission_date = dplyr::all_of(sub_date_col))
  }
  
  df
}

# ─────────────────────────────────────────────────────────────────────────────
# [1/6] Pull camps form
# ─────────────────────────────────────────────────────────────────────────────
message("── [1/6] Pulling camp enumeration form from ODK Central ──")
camps_svc <- paste0(Sys.getenv("ODKC_URL"),
                    "/v1/projects/", Sys.getenv("ODKC_PID"),
                    "/forms/", Sys.getenv("ODKC_FID_CAMPS"), ".svc")

message("── [2/6] Pulling camp enumeration form ──")
camps_raw <- odk_odata_get(camps_svc, Sys.getenv("ODKC_UN"), Sys.getenv("ODKC_PW"))
message("  Camps cols matching pop/zero/polio: ",
        paste(sort(grep("pop|zero|polio|dose", names(camps_raw), ignore.case = TRUE, value = TRUE)), collapse = ", "))
message("  Camps cols matching gps: ",
        paste(sort(grep("gps", names(camps_raw), ignore.case = TRUE, value = TRUE)), collapse = ", "))
message("  Camps cols matching transport/route/challenge/tribe: ",
        paste(sort(grep("transport|route|challenge|tribe", names(camps_raw), ignore.case = TRUE, value = TRUE)), collapse = ", "))
message("  Camps cols matching profile/enumerator: ",
        paste(sort(grep("profile|enumerator", names(camps_raw), ignore.case = TRUE, value = TRUE)), collapse = ", "))

# ─────────────────────────────────────────────────────────────────────────────
# [3/6] Pull + consolidate all outreach data (ODK + KoBo + XLS)
# ─────────────────────────────────────────────────────────────────────────────
message("── [3/6] Pulling & consolidating outreach data ──")

outreach_svc <- paste0(Sys.getenv("ODKC_URL"),
                       "/v1/projects/", Sys.getenv("ODKC_PID"),
                       "/forms/", Sys.getenv("ODKC_FID_OUTREACH"), ".svc")

# ── Segment 1: ODK ────────────────────────────────────────────────────────────
outreach_odk_raw <- odk_odata_get(outreach_svc, Sys.getenv("ODKC_UN"), Sys.getenv("ODKC_PW"))
message("  Raw ODK outreach cols: ", paste(sort(names(outreach_odk_raw)), collapse = ", "))

outreach_odk <- outreach_odk_raw |>
  rename_flexible("distric",                            "a_district") |>
  rename_flexible("b_total_number_of_chil",             "zd_vaccinated") |>
  rename_flexible("c_number_of_childr.*settlement$",    "zd_ri") |>
  rename_flexible("c_number_of_childr.*settlement_001", "zd_polio") |>
  rename_flexible("^today$",                            "out_date") |>
  rename_flexible("^geo_reference_hh_coordinates$",     "out_gps_raw") |>
  dplyr::mutate(
    id                     = as.character(id),
    source                 = "odk",
    system_submission_date = lubridate::as_datetime(system_submission_date)
  )

# Same GeoJSON list-column issue as camps: coordinates come through as
# [lon, lat, alt] in a single list-column, not separate _1/_2 columns.
if ("out_gps_raw" %in% names(outreach_odk) && is.list(outreach_odk[["out_gps_raw"]])) {
  outreach_odk <- outreach_odk |> dplyr::mutate(
    out_lon = suppressWarnings(as.numeric(purrr::map_dbl(out_gps_raw, ~ if (length(.x) >= 1) as.numeric(.x[[1]]) else NA_real_))),
    out_lat = suppressWarnings(as.numeric(purrr::map_dbl(out_gps_raw, ~ if (length(.x) >= 2) as.numeric(.x[[2]]) else NA_real_)))
  )
} else {
  outreach_odk <- outreach_odk |> dplyr::mutate(out_lat = NA_real_, out_lon = NA_real_)
}
message("  Outreach ODK rows with valid lat/lon: ",
        sum(!is.na(outreach_odk$out_lat) & !is.na(outreach_odk$out_lon) & outreach_odk$out_lat != 0))

message("  ODK rows: ", nrow(outreach_odk))

# ── Segment 2: KoBo (if file exists) ─────────────────────────────────────────
outreach_kobo <- dplyr::tibble()
if (file.exists("data/kobo_outreach.xlsx")) {
  message("  Loading KoBo historical data...")
  outreach_kobo <- readxl::read_excel("data/kobo_outreach.xlsx") |>
    dplyr::rename(
      out_date               = today,
      a_district             = District,
      zd_vaccinated          = `Total number of zero dose children vaccinated during the outreach session?`,
      zd_ri                  = `Number of children with zero dose routine immunization (excluding Polio) found in the settlement`,
      zd_polio               = `Number of children with zero polio doses found in the settlement`,
      out_lat                = `_Geo-reference of the settlement outreach_latitude`,
      out_lon                = `_Geo-reference of the settlement outreach_longitude`,
      system_submission_date = `_submission_time`,
      id                     = `_id`
    ) |>
    dplyr::mutate(
      id                     = paste0("kobo_", id),
      source                 = "kobo",
      system_submission_date = lubridate::as_datetime(system_submission_date)
    )
  message("  KoBo rows: ", nrow(outreach_kobo))
}

# ── Segment 3: XLS export (if file exists) ────────────────────────────────────
outreach_xls <- dplyr::tibble()
xls_path <- "data/Nomadic_Polio_Project_Outreach_Tool_-_all_versions_-_labels_-_2026-07-07-18-23-14.xlsx"
if (file.exists(xls_path)) {
  message("  Loading XLS outreach data...")
  outreach_xls <- readxl::read_excel(xls_path) |>
    dplyr::rename(
      out_date  = today,
      a_district = District,
      zd_vaccinated = `Total number of children vaccinated during the outreach session?`,
      zd_ri     = `Number of children with zero dose routine immunization (excluding Polio) found in the settlement`,
      zd_polio  = `Number of children with zero polio doses found in the settlement`,
      out_lat   = `_Geo-reference of the settlement outreach_latitude`,
      out_lon   = `_Geo-reference of the settlement outreach_longitude`
    ) |>
    dplyr::mutate(
      out_date = lubridate::as_date(out_date),  # coerce POSIXct → Date before bind
      id       = paste0("xls_", seq_len(dplyr::n())),
      source   = "xls",
      system_submission_date = lubridate::as_datetime(NA)
    )
  message("  XLS rows: ", nrow(outreach_xls))
}

# ── Merge all three sources ────────────────────────────────────────────────────
outreach_all <- dplyr::bind_rows(outreach_odk, outreach_kobo, outreach_xls)
message("  Combined outreach rows: ", nrow(outreach_all))

# ── Single unified cleaning pass ───────────────────────────────────────────────
# All date, year, and district normalization happens here — once — on the full table.
outreach <- outreach_all |>
  dplyr::mutate(
    zd_ri         = suppressWarnings(as.numeric(zd_ri)),
    zd_polio      = suppressWarnings(as.numeric(zd_polio)),
    zd_vaccinated = suppressWarnings(as.numeric(zd_vaccinated)),
    vaccinated    = zd_vaccinated,
    out_date      = dplyr::coalesce(
      suppressWarnings(lubridate::ymd(as.character(out_date))),
      lubridate::as_date(system_submission_date)
    ),
    year        = lubridate::year(out_date),
    month_num   = lubridate::month(out_date),
    month_label = format(out_date, "%b-%y"),
    out_district = normalise_district(as.character(a_district))
  )

message("  Year distribution after merge: ")
print(table(outreach$year, useNA = "always"))

# ─────────────────────────────────────────────────────────────────────────────
# [4/6] District geometry from ArcGIS
# ─────────────────────────────────────────────────────────────────────────────
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
  sf::st_read(geojson_txt, quiet = TRUE) |> sf::st_make_valid()
}, error = function(e) {
  message("  ArcGIS fetch failed: ", e$message)
  NULL
})

if (!is.null(districts_sf)) {
  districts_sf <- districts_sf |>
    dplyr::rename(district_name = NAME_L2, state_name = NAME_L1) |>
    dplyr::select(district_name, state_name, geometry)
  message("  Loaded ", nrow(districts_sf), " district polygons")
}

# ── Spatial district assignment for outreach ──────────────────────────────────
if (!is.null(districts_sf) && all(c("out_lat", "out_lon") %in% names(outreach))) {
  out_coords <- outreach |>
    dplyr::filter(!is.na(out_lat) & !is.na(out_lon) & out_lat != 0)
  if (nrow(out_coords) > 0) {
    out_sf <- sf::st_as_sf(out_coords, coords = c("out_lon", "out_lat"), crs = 4326, remove = FALSE)
    out_joined <- sf::st_join(out_sf, districts_sf, join = sf::st_within) |>
      sf::st_drop_geometry() |>
      dplyr::select(id, spatial_out_district = district_name)
    outreach <- outreach |>
      dplyr::left_join(out_joined, by = "id") |>
      dplyr::mutate(
        out_district = dplyr::coalesce(spatial_out_district, out_district),
        out_district = normalise_district(trimws(out_district))
      )
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# [5/6] Camp enumeration cleaning
# ─────────────────────────────────────────────────────────────────────────────
message("── [5/6] Cleaning camps ──")

camps <- camps_raw |>
  rename_flexible("community_level_camp_next_adm3",          "next_district") |>
  rename_flexible("community_level_camp_previous_adm3",      "prev_district") |>
  rename_flexible("community_level_camp_last_exit_date",     "exit_date") |>
  rename_flexible("community_level_camp_first_month",        "first_month") |>
  rename_flexible("community_level_camp_last_month",         "last_month") |>
  rename_flexible("camp_hf_ri_num",                          "hf_travel_min") |>
  rename_flexible("community_level_camp_settlement",         "settlement_type") |>
  rename_flexible("group_profile_enumerator_district",       "enum_district") |>
  rename_flexible("community_level_gps_coordinates",         "gps_raw") |>
  rename_flexible("polio_camp_pop_0to11_months$",            "pop_0to11") |>
  rename_flexible("polio_camp_pop_12to59_months$",           "pop_12to59") |>
  rename_flexible("polio_camp_tot_pop",                      "pop_total") |>
  rename_flexible("polio_camp_pop_0to11_months.*zero.*dose",  "zd_0to11") |>
  rename_flexible("polio_camp_pop_12to59_months.*zero.*dose", "zd_12to59") |>
  rename_flexible("polio_camp_previous_service_challenges",  "challenge") |>
  rename_flexible("transport_type_transport_infrastructure", "transport_infra") |>
  rename_flexible("transport_type_migratory_route",          "route_type") |>
  rename_flexible("transport_name_migratory_route",          "route_name") |>
  rename_flexible("tribe_subtribe_district",                 "recorded_district") |>
  dplyr::mutate(
    pop_0to11     = suppressWarnings(as.numeric(pop_0to11)),
    pop_12to59    = suppressWarnings(as.numeric(pop_12to59)),
    pop_total     = suppressWarnings(as.numeric(pop_total)),
    zd_0to11      = suppressWarnings(as.numeric(zd_0to11)),
    zd_12to59     = suppressWarnings(as.numeric(zd_12to59)),
    hf_travel_min = suppressWarnings({
      raw <- tolower(trimws(as.character(hf_travel_min)))
      num <- suppressWarnings(as.numeric(stringr::str_extract(raw, "^[0-9]+")))
      is_hrs <- grepl("hr|hour", raw)
      dplyr::if_else(is_hrs & !is.na(num) & num <= 12, num * 60L, num)
    }),
    exit_date_raw = as.character(exit_date),
    season = dplyr::case_when(
      grepl("deyr|oct|nov",               exit_date_raw, ignore.case = TRUE) ~ "Deyr",
      grepl("guul|guu|mar|apr|may",       exit_date_raw, ignore.case = TRUE) ~ "Guul",
      grepl("jilaal|dec|jan|feb",         exit_date_raw, ignore.case = TRUE) ~ "Jilaal",
      grepl("xaaga|hagaa|jun|jul|aug|sep",exit_date_raw, ignore.case = TRUE) ~ "Xaaga",
      TRUE ~ NA_character_
    ),
    next_district = normalise_district(next_district),
    prev_district = normalise_district(prev_district),
    enum_district = normalise_district(dplyr::coalesce(enum_district, recorded_district))
  )

# GPS coordinates for camps
lat_col <- grep("gps.*latitude|gps.*lat$",  names(camps), ignore.case = TRUE, value = TRUE)[1]
lon_col <- grep("gps.*longitude|gps.*lon$", names(camps), ignore.case = TRUE, value = TRUE)[1]
if (!is.na(lat_col) && !is.na(lon_col)) {
  camps <- camps |> dplyr::mutate(
    lat = suppressWarnings(as.numeric(.data[[lat_col]])),
    lon = suppressWarnings(as.numeric(.data[[lon_col]]))
  )
} else if ("gps_raw" %in% names(camps) && is.list(camps[["gps_raw"]])) {
  # geopoint came through as a GeoJSON coordinates array ([lon, lat, alt])
  # parsed into a list-column of numeric vectors, not a flat string —
  # extract directly instead of string-splitting (GeoJSON order is
  # longitude first, then latitude).
  message("  gps_raw is a list-column (GeoJSON array) — extracting lon/lat by position")
  camps <- camps |> dplyr::mutate(
    lon = suppressWarnings(as.numeric(purrr::map_dbl(gps_raw, ~ if (length(.x) >= 1) as.numeric(.x[[1]]) else NA_real_))),
    lat = suppressWarnings(as.numeric(purrr::map_dbl(gps_raw, ~ if (length(.x) >= 2) as.numeric(.x[[2]]) else NA_real_)))
  )
} else if ("gps_raw" %in% names(camps)) {
  camps <- camps |> dplyr::mutate(
    gps_str = as.character(gps_raw),
    lat = suppressWarnings(as.numeric(purrr::map_chr(strsplit(gps_str, " "), ~ .x[1]))),
    lon = suppressWarnings(as.numeric(purrr::map_chr(strsplit(gps_str, " "), ~ .x[2])))
  )
} else {
  camps <- camps |> dplyr::mutate(lat = NA_real_, lon = NA_real_)
  message("  WARNING: no GPS columns found in camps form")
}
message("  Camps with valid lat/lon: ",
        sum(!is.na(camps$lat) & !is.na(camps$lon) & camps$lat != 0 & camps$lon != 0), " of ", nrow(camps))

# Spatial district assignment for camps
# district always gets set from enum_district as a baseline first, then
# upgraded to the spatial join result where valid GPS + geometry exist —
# this avoids `district` silently never being created if GPS parsing
# fails for every row (previously: only set inside the two branches below,
# so a "has lat/lon columns but 0 valid rows" case fell through both).
camps <- camps |> dplyr::mutate(district = enum_district)
if (!is.null(districts_sf) && all(c("lat","lon") %in% names(camps))) {
  camps_coords <- camps |> dplyr::filter(!is.na(lat) & !is.na(lon) & lat != 0 & lon != 0)
  if (nrow(camps_coords) > 0) {
    camps_sf <- sf::st_as_sf(camps_coords, coords = c("lon","lat"), crs = 4326, remove = FALSE)
    joined   <- sf::st_join(camps_sf, districts_sf, join = sf::st_within) |>
      sf::st_drop_geometry() |>
      dplyr::select(id, spatial_district = district_name)
    camps <- camps |>
      dplyr::left_join(joined, by = "id") |>
      dplyr::mutate(district = normalise_district(dplyr::coalesce(spatial_district, enum_district)))
  }
}
camps <- camps |> dplyr::mutate(district = normalise_district(district))

# Submission year for camps
if ("today.start" %in% names(camps)) {
  camps <- camps |> dplyr::mutate(sub_date = lubridate::ymd(as.character(today.start)), year = lubridate::year(sub_date))
} else if ("system_submission_date" %in% names(camps)) {
  camps <- camps |> dplyr::mutate(sub_date = lubridate::as_date(system_submission_date), year = lubridate::year(sub_date))
}

transport <- camps |>
  dplyr::select(id, year, dplyr::any_of(c("transport_infra","route_type","route_name"))) |>
  dplyr::filter(!is.na(route_name), trimws(route_name) != "")

# ─────────────────────────────────────────────────────────────────────────────
# [6/6] Aggregate dashboard objects
# ─────────────────────────────────────────────────────────────────────────────
message("── [6/6] Aggregating dashboard objects ──")

years <- c("all", "2024", "2025", "2026")

filter_year <- function(df, yr, date_col = "year") {
  if (yr == "all") df
  else df |> dplyr::filter(.data[[date_col]] == as.integer(yr))
}

# KPIs
kpi <- purrr::map(years, function(yr) {
  c_yr <- filter_year(camps, yr)
  o_yr <- filter_year(outreach, yr)
  list(
    camps      = nrow(c_yr),
    pop        = sum(c_yr$pop_0to11 + c_yr$pop_12to59, na.rm = TRUE),
    zd_total   = sum(c_yr$zd_0to11  + c_yr$zd_12to59,  na.rm = TRUE),
    zd_011     = sum(c_yr$zd_0to11,  na.rm = TRUE),
    zd_1259    = sum(c_yr$zd_12to59, na.rm = TRUE),
    sessions   = nrow(o_yr),
    vaccinated = sum(o_yr$zd_vaccinated, na.rm = TRUE)
  )
}) |> purrr::set_names(years)

# ZD by district
zd_by_dist <- purrr::map(years, function(yr) {
  filter_year(camps, yr) |>
    dplyr::group_by(district) |>
    dplyr::summarise(
      camps  = dplyr::n(),
      t011   = sum(pop_0to11,  na.rm = TRUE),
      z011   = sum(zd_0to11,   na.rm = TRUE),
      t1259  = sum(pop_12to59, na.rm = TRUE),
      z1259  = sum(zd_12to59,  na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      rate011  = safe_pct(z011,  t011),
      rate1259 = safe_pct(z1259, t1259)
    ) |>
    dplyr::filter(!is.na(district))
}) |> purrr::set_names(years)

# Movement flows
flows <- purrr::map(years, function(yr) {
  filter_year(camps, yr) |>
    dplyr::filter(!is.na(prev_district), !is.na(next_district)) |>
    dplyr::group_by(prev_c = prev_district, next_c = next_district) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
}) |> purrr::set_names(years)

# Indegree centrality
indegree <- purrr::map(years, function(yr) {
  filter_year(camps, yr) |>
    dplyr::filter(!is.na(next_district)) |>
    dplyr::count(next_district, name = "count") |>
    dplyr::arrange(dplyr::desc(count)) |>
    dplyr::rename(district = next_district)
}) |> purrr::set_names(years)

# Seasons
seasons <- purrr::map(years, function(yr) {
  s <- filter_year(camps, yr) |>
    dplyr::filter(!is.na(season)) |>
    dplyr::count(season, name = "count") |>
    dplyr::mutate(pct = round(100 * count / sum(count)))
  purrr::map(c("Deyr","Guul","Jilaal","Xaaga"), function(s_name) {
    row <- s[s$season == s_name, ]
    list(count = if (nrow(row) > 0) row$count else 0L,
         pct   = if (nrow(row) > 0) row$pct   else 0L)
  }) |> purrr::set_names(c("Deyr","Guul","Jilaal","Xaaga"))
}) |> purrr::set_names(years)

# Top migratory routes
routes <- purrr::map(years, function(yr) {
  d <- filter_year(camps, yr) |>
    dplyr::filter(!is.na(route_name), trimws(route_name) != "") |>
    dplyr::count(route_name, name = "count") |>
    dplyr::arrange(dplyr::desc(count)) |>
    dplyr::slice_head(n = 10)
  if (nrow(d) == 0) return(list())
  lapply(seq_len(nrow(d)), function(i) list(n = d$route_name[i], c = d$count[i]))
}) |> purrr::set_names(years)

# Cross-border
cross_border <- purrr::map(years, function(yr) {
  c_yr <- filter_year(camps, yr)
  list(
    dolow = sum(!is.na(c_yr$prev_district) & c_yr$prev_district == "Dolow", na.rm = TRUE),
    elwak = sum(!is.na(c_yr$prev_district) & c_yr$prev_district == "Elwak", na.rm = TRUE)
  )
}) |> purrr::set_names(years)

# Outreach monthly trend
out_monthly <- outreach |>
  dplyr::filter(!is.na(out_date)) |>
  dplyr::group_by(month_label, year, month_num) |>
  dplyr::summarise(
    sessions      = dplyr::n(),
    zd_vaccinated = sum(zd_vaccinated, na.rm = TRUE),
    .groups       = "drop"
  ) |>
  dplyr::arrange(year, month_num) |>
  dplyr::mutate(
    month_label = factor(month_label, levels = unique(month_label), ordered = TRUE),
    vaccinated  = zd_vaccinated
  )

# Outreach by district
out_by_dist <- purrr::map(years, function(yr) {
  filter_year(outreach, yr) |>
    dplyr::group_by(district = out_district) |>
    dplyr::summarise(
      sessions      = dplyr::n(),
      zd_vaccinated = sum(zd_ri + zd_polio, na.rm = TRUE),
      zd_ri         = sum(zd_ri,    na.rm = TRUE),
      zd_polio      = sum(zd_polio, na.rm = TRUE),
      vaccinated    = zd_vaccinated,
      .groups       = "drop"
    ) |>
    dplyr::filter(!is.na(district))
}) |> purrr::set_names(years)

# Performance table
perf_table <- purrr::map(years, function(yr) {
  zd <- zd_by_dist[[yr]] |>
    dplyr::mutate(district = normalise_district(trimws(as.character(district)))) |>
    dplyr::group_by(district) |>
    dplyr::summarise(zd_id = sum(z011, na.rm = TRUE), .groups = "drop")
  
  out <- out_by_dist[[yr]] |>
    dplyr::mutate(district = normalise_district(trimws(as.character(district)))) |>
    dplyr::group_by(district) |>
    dplyr::summarise(
      sessions      = sum(sessions,      na.rm = TRUE),
      zd_vaccinated = sum(zd_vaccinated, na.rm = TRUE),
      .groups = "drop"
    )
  
  dplyr::full_join(zd, out, by = "district") |>
    dplyr::mutate(
      zd_id         = tidyr::replace_na(zd_id, 0L),
      sessions      = tidyr::replace_na(sessions, 0L),
      zd_vaccinated = tidyr::replace_na(zd_vaccinated, 0L),
      vaccinated    = zd_vaccinated,
      gap           = pmax(zd_id - zd_vaccinated, 0L),
      cov_pct       = safe_pct(zd_vaccinated, zd_id)
    ) |>
    dplyr::group_by(district) |>
    dplyr::summarise(
      zd_id         = sum(zd_id,         na.rm = TRUE),
      sessions      = sum(sessions,      na.rm = TRUE),
      zd_vaccinated = sum(zd_vaccinated, na.rm = TRUE),
      vaccinated    = sum(vaccinated,    na.rm = TRUE),
      gap           = sum(gap,           na.rm = TRUE),
      cov_pct       = safe_pct(vaccinated, zd_id),
      .groups       = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(gap))
}) |> purrr::set_names(years)

# ZD-RI vs ZD-Polio
zd_ri_pol <- purrr::map(years, function(yr) {
  out_by_dist[[yr]] |>
    dplyr::select(district, ri = zd_ri, pol = zd_polio) |>
    dplyr::filter(ri > 0 | pol > 0) |>
    dplyr::arrange(dplyr::desc(ri + pol))
}) |> purrr::set_names(years)

# HF travel time buckets
hf_access <- purrr::map(years, function(yr) {
  filter_year(camps, yr) |>
    dplyr::filter(!is.na(hf_travel_min)) |>
    dplyr::mutate(bucket = dplyr::case_when(
      hf_travel_min <  30  ~ "<30 min",
      hf_travel_min <  60  ~ "30–60 min",
      hf_travel_min < 120  ~ "60–120 min",
      TRUE                 ~ "120+ min"
    )) |>
    dplyr::count(bucket, name = "n") |>
    dplyr::mutate(pct = round(100 * n / sum(n)))
}) |> purrr::set_names(years)

# Challenge categories
categorise_challenge <- function(x) {
  x <- tolower(trimws(x))
  cats <- c(
    "No nearby HF"        = "no.*(hf|health|facilit|clinic|hospital)",
    "Distance to HF"      = "dist|far|remote|kilomet|km|long way",
    "Lack of transport"   = "transport|vehicle|car|road|access",
    "Security concerns"   = "securi|insecur|conflict|risk|danger|armed",
    "Community awareness" = "aware|knowled|inform|educat|communit",
    "Vaccinators absent"  = "vaccin.*absent|no.*vaccin|vaccin.*not|staff.*absent|absent.*vaccin",
    "Other"               = "."
  )
  for (nm in names(cats)) if (grepl(cats[nm], x, ignore.case = TRUE)) return(nm)
  "Other"
}

challenges <- purrr::map(years, function(yr) {
  d <- filter_year(camps, yr) |> dplyr::filter(!is.na(challenge), trimws(challenge) != "")
  if (nrow(d) == 0) return(data.frame(challenge = character(), n = integer()))
  d |>
    dplyr::mutate(challenge_cat = sapply(challenge, categorise_challenge)) |>
    dplyr::count(challenge = challenge_cat, name = "n") |>
    dplyr::arrange(dplyr::desc(n))
}) |> purrr::set_names(years)

# Settlement / transport / route type counts
settlement_counts <- camps |>
  dplyr::filter(!is.na(settlement_type), trimws(settlement_type) != "") |>
  dplyr::mutate(cat = dplyr::case_when(
    grepl("settlement|urban|town|village", settlement_type, ignore.case = TRUE) ~ "Settlement",
    grepl("camp|idp|temp",                settlement_type, ignore.case = TRUE) ~ "Camp",
    TRUE ~ "Other"
  )) |> dplyr::count(cat, name = "n")

transport_counts <- camps |>
  dplyr::filter(!is.na(transport_infra), trimws(transport_infra) != "") |>
  dplyr::mutate(cat = dplyr::case_when(
    grepl("livestock|foot|walk|camel|donkey", transport_infra, ignore.case = TRUE) ~ "Livestock/foot",
    grepl("bus|truck|car|vehicle|motor",      transport_infra, ignore.case = TRUE) ~ "Bus/vehicle",
    TRUE ~ "Other"
  )) |> dplyr::count(cat, name = "n")

route_type_counts <- camps |>
  dplyr::filter(!is.na(route_type), trimws(route_type) != "") |>
  dplyr::mutate(cat = dplyr::case_when(
    grepl("foot|path|track|trail|bush",     route_type, ignore.case = TRUE) ~ "Footpath",
    grepl("highway|main|road|tarmac|paved", route_type, ignore.case = TRUE) ~ "Highway",
    grepl("border|crossing|cross",          route_type, ignore.case = TRUE) ~ "Border crossing",
    TRUE ~ "Other"
  )) |> dplyr::count(cat, name = "n")

# District monthly vaccinated (top 5 districts)
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
  dplyr::summarise(zd_vaccinated = sum(zd_vaccinated, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(year, month_num) |>
  dplyr::mutate(
    month_label = factor(month_label, levels = levels(out_monthly$month_label), ordered = TRUE),
    vaccinated  = zd_vaccinated
  )

# GPS tables
camp_gps <- camps |>
  dplyr::filter(!is.na(lat), !is.na(lon), lat != 0, lon != 0) |>
  dplyr::select(id, lat, lon, district, year, zd_0to11, zd_12to59, pop_0to11, pop_12to59)

out_gps <- outreach |>
  dplyr::filter(!is.na(out_lat), !is.na(out_lon), out_lat != 0) |>
  dplyr::select(id, lat = out_lat, lon = out_lon, district = out_district,
                year, vaccinated = zd_vaccinated, month_label) |>
  dplyr::mutate(month_label = factor(month_label, levels = levels(out_monthly$month_label), ordered = TRUE))

# Centroids
camp_centroids <- camp_gps |>
  dplyr::mutate(
    district = normalise_district(district),
    lon = suppressWarnings(as.numeric(lon)),
    lat = suppressWarnings(as.numeric(lat))
  ) |>
  dplyr::filter(!is.na(lon), !is.na(lat), !is.na(district)) |>
  dplyr::group_by(district) |>
  dplyr::summarise(lon = median(lon, na.rm = TRUE), lat = median(lat, na.rm = TRUE),
                   n_camps = dplyr::n(), .groups = "drop") |>
  dplyr::select(district, lon, lat)

poly_centroids <- if (!is.null(districts_sf)) {
  districts_sf |>
    sf::st_point_on_surface() |>
    dplyr::mutate(lon = sf::st_coordinates(geometry)[,1], lat = sf::st_coordinates(geometry)[,2]) |>
    sf::st_drop_geometry() |>
    dplyr::transmute(district = normalise_district(district_name), lon, lat)
} else {
  tibble::tribble(
    ~district,                            ~lon,     ~lat,
    normalise_district("Baardhere"),   42.2851,  2.3646,
    normalise_district("Elwak"),       41.0270,  2.7771,
    normalise_district("Garbaharey"), 42.2180,  3.3309,
    normalise_district("Dolow"),       42.1149,  4.0953,
    normalise_district("Belet-hawa"), 41.9512,  3.9346,
    normalise_district("Luuq"),        42.5203,  3.8083,
    normalise_district("El Barde"),   43.6874,  4.8286,
    normalise_district("Baidoa"),      43.6509,  3.1521,
    normalise_district("Beledweyne"), 45.2115,  4.7732
  )
}

centroids <- camp_centroids |>
  dplyr::rows_append(poly_centroids |> dplyr::anti_join(camp_centroids, by = "district"))

# GeoJSON for leaflet
if (!is.null(districts_sf)) {
  districts_geojson <- sf::st_simplify(districts_sf, dTolerance = 0.003, preserveTopology = TRUE) |>
    sf::st_transform(4326)
  states_geojson <- districts_sf |>
    dplyr::group_by(state_name) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
    sf::st_simplify(dTolerance = 0.003, preserveTopology = TRUE) |>
    sf::st_transform(4326)
} else {
  districts_geojson <- NULL
  states_geojson    <- NULL
}

# ─────────────────────────────────────────────────────────────────────────────
# Bundle and save
# ─────────────────────────────────────────────────────────────────────────────
nomad_data <- list(
  outreach          = outreach,
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
  centroids         = centroids,
  districts_geojson = districts_geojson,
  states_geojson    = states_geojson,
  pulled_at         = Sys.time(),
  latest_submission = max(
    suppressWarnings(lubridate::as_datetime(camps$system_submission_date)),
    na.rm = TRUE
  )
)

if (!dir.exists("data")) dir.create("data")
saveRDS(nomad_data, "data/nomad_data.rds")
message("✓ Saved data/nomad_data.rds (",
        nrow(camps), " camp records, ",
        nrow(outreach), " outreach sessions across years: ",
        paste(sort(unique(outreach$year)), collapse = ", "), ")")