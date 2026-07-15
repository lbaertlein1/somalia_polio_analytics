# =============================================================================
# pull_snapshot.R
#
# Run this ONCE, locally, before deploying vaccinebuddy app.R to
# shinyapps.io. It pulls live from ODK Central (same OData approach as the
# app), filters to July 9, 2026 only, and writes the two snapshot CSVs that
# app.R reads instead of pulling live:
#
#   data/snapshot_main.csv
#   data/snapshot_entry.csv
#
# After running this, the app/ folder can be pushed to shinyapps.io with NO
# ODK Central connection required at runtime.
# =============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# --- ODK Central / form location -------------------------------------------
ODK_BASE_URL <- "https://emro.nafundi.com"
ODK_PROJECT_ID <- 32
ODK_FORM_ID <- "vaccine_buddy_events_aug2025_nopv2_snid"
ODK_TZ <- "Africa/Mogadishu"

# --- Credentials -------------------------------------------------------
# Fill these in before running.
ODK_USERNAME <- "your_email@example.com"
ODK_PASSWORD <- "your_password"

# Name of the nested repeat table ("entry" group) as exposed at the OData
# service root (i.e. GET {svc}/Submissions.entry). If ODK Central names it
# differently, update this.
ENTRY_TABLE <- "Submissions.entry"

# --- Filter window -----------------------------------------------------
# Day boundary is in ODK_TZ (Africa/Mogadishu), matching how the app itself
# defines "a day" when you use its date-range picker.
SNAPSHOT_DATE <- as.Date("2026-07-09")

# --- Output paths --------------------------------------------------------
OUT_DIR <- "/data"
OUT_MAIN <- file.path(OUT_DIR, "snapshot_main.csv")
OUT_ENTRY <- file.path(OUT_DIR, "snapshot_entry.csv")

# --- small helper: find a key in a raw parsed-JSON record -------------------
find_json_key <- function(rec, patterns) {
  nms <- names(rec)
  for (p in patterns) {
    hit <- nms[grepl(p, nms, ignore.case = TRUE)]
    if (length(hit) >= 1) return(hit[1])
  }
  NA_character_
}

# --- OData fetch/parse helpers (same approach as the app -- no ruODK) ------
odata_fetch_all <- function(url, user, pass) {
  records <- list()
  next_url <- url
  while (!is.null(next_url)) {
    resp <- httr::GET(next_url, httr::authenticate(user, pass, type = "basic"))
    httr::stop_for_status(resp, task = paste("fetch", next_url))
    parsed <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
    records <- c(records, parsed[["value"]])
    
    nxt <- parsed[["@odata.nextLink"]]
    if (is.null(nxt)) {
      next_url <- NULL
    } else if (grepl("^https?://", nxt)) {
      next_url <- nxt
    } else if (startsWith(nxt, "/")) {
      next_url <- paste0(ODK_BASE_URL, nxt)
    } else {
      base_dir <- sub("/[^/]*$", "", url)
      next_url <- paste0(base_dir, "/", nxt)
    }
  }
  records
}

odata_field <- function(rec, name, numeric = FALSE) {
  val <- rec[[name]]
  if (is.null(val)) {
    if (numeric) return(NA_real_)
    return(NA_character_)
  }
  if (numeric) return(suppressWarnings(as.numeric(val)))
  as.character(val)
}

odata_parse_main <- function(records) {
  if (length(records) == 0) {
    return(data.frame(id = character(0), device_id = character(0),
                      team_name = character(0), campaign_name = character(0),
                      stringsAsFactors = FALSE))
  }
  id_key <- find_json_key(records[[1]], c("^__id$", "^id$"))
  dplyr::bind_rows(lapply(records, function(rec) {
    data.frame(
      id = if (!is.na(id_key)) odata_field(rec, id_key) else NA_character_,
      device_id = odata_field(rec, "device_id"),
      team_name = odata_field(rec, "team_name"),
      campaign_name = odata_field(rec, "campaign_name"),
      stringsAsFactors = FALSE
    )
  }))
}

odata_parse_entry <- function(records) {
  if (length(records) == 0) {
    return(data.frame(
      id = character(0), entry_id = character(0), timestamp = character(0),
      gps_point_coordinates_1 = numeric(0), gps_point_coordinates_2 = numeric(0),
      lid_open = character(0), lid_close = character(0), temp_c = numeric(0),
      battery_percent = numeric(0), gps_accuracy_m = numeric(0),
      submissions_id = character(0), stringsAsFactors = FALSE
    ))
  }
  id_key <- find_json_key(records[[1]], c("^__id$", "^id$"))
  parent_key <- find_json_key(records[[1]], c("submissions.*id", "parent"))
  
  dplyr::bind_rows(lapply(records, function(rec) {
    gp <- rec[["gps_point"]]
    lon <- NA_real_
    lat <- NA_real_
    if (!is.null(gp) && !is.null(gp[["coordinates"]])) {
      coords <- gp[["coordinates"]]
      if (length(coords) >= 2) {
        lon <- suppressWarnings(as.numeric(coords[[1]]))
        lat <- suppressWarnings(as.numeric(coords[[2]]))
      }
    }
    data.frame(
      id = if (!is.na(id_key)) odata_field(rec, id_key) else NA_character_,
      entry_id = odata_field(rec, "entry_id"),
      timestamp = odata_field(rec, "timestamp"),
      gps_point_coordinates_1 = lon,
      gps_point_coordinates_2 = lat,
      lid_open = odata_field(rec, "lid_open"),
      lid_close = odata_field(rec, "lid_close"),
      temp_c = odata_field(rec, "temp_c", numeric = TRUE),
      battery_percent = odata_field(rec, "battery_percent", numeric = TRUE),
      gps_accuracy_m = odata_field(rec, "gps_accuracy_m", numeric = TRUE),
      submissions_id = if (!is.na(parent_key)) odata_field(rec, parent_key) else NA_character_,
      stringsAsFactors = FALSE
    )
  }))
}

# =============================================================================
# Pull, filter, save
# =============================================================================

svc_url <- sprintf("%s/v1/projects/%s/forms/%s.svc",
                   ODK_BASE_URL, ODK_PROJECT_ID, ODK_FORM_ID)

message("Fetching main submissions table...")
main_records <- odata_fetch_all(paste0(svc_url, "/Submissions"),
                                ODK_USERNAME, ODK_PASSWORD)
main_df <- odata_parse_main(main_records)

message("Fetching entry (repeat) table...")
entry_records <- odata_fetch_all(paste0(svc_url, "/", ENTRY_TABLE),
                                 ODK_USERNAME, ODK_PASSWORD)
entry_df <- odata_parse_entry(entry_records)

message(sprintf("Pulled %d submissions, %d entry rows.", nrow(main_df), nrow(entry_df)))

# --- Filter entry rows to the snapshot day, in ODK_TZ -----------------------
entry_df$ts_parsed <- lubridate::ymd_hms(entry_df$timestamp, tz = ODK_TZ, quiet = TRUE)
day_start <- as.POSIXct(SNAPSHOT_DATE, tz = ODK_TZ)
day_end <- as.POSIXct(SNAPSHOT_DATE + 1, tz = ODK_TZ)

entry_snapshot <- entry_df %>%
  filter(!is.na(ts_parsed), ts_parsed >= day_start, ts_parsed < day_end) %>%
  select(-ts_parsed)

# --- Keep only main submissions that have a matching entry in the window ----
keep_ids <- unique(entry_snapshot$submissions_id)
main_snapshot <- main_df %>% filter(id %in% keep_ids)

message(sprintf(
  "Filtered to %s: %d submissions, %d entry rows, %d unique devices.",
  format(SNAPSHOT_DATE), nrow(main_snapshot), nrow(entry_snapshot),
  length(unique(main_snapshot$device_id))
))

# --- Write out ---------------------------------------------------------
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
write.csv(main_snapshot, OUT_MAIN, row.names = FALSE)
write.csv(entry_snapshot, OUT_ENTRY, row.names = FALSE)

message(sprintf("Saved:\n  %s\n  %s", OUT_MAIN, OUT_ENTRY))
message("Done. vaccine buddy is now ready to push to shinyapps.io with no ODK Central connection needed at runtime.")