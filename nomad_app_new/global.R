# global.R — loaded once on app startup

if (file.exists(".Renviron")) readRenviron(".Renviron")

# Load all libraries up front — including data pipeline deps —
# so module sources and data_pull.R both see a fully initialised namespace
library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(httr)
library(jsonlite)
library(stringr)
library(purrr)
library(DT)
library(readxl)
library(leaflet)

source("R/utils.R")
source("R/mod_insights.R")
source("R/mod_kpi.R")
source("R/mod_movement.R")
source("R/mod_zerodose.R")
source("R/mod_access.R")
source("R/mod_report.R")

# ── Auto-refresh: pull only if ODK has submissions newer than cached data ─────
rds_path <- "data/nomad_data.rds"

needs_refresh <- function() {
  if (!file.exists(rds_path)) {
    message("No cached data — pulling from ODK...")
    return(TRUE)
  }
  
  cached      <- readRDS(rds_path)
  last_in_data <- cached$latest_submission  # date of newest submission in the data
  
  if (is.null(last_in_data)) {
    message("Cache has no submission timestamp — pulling to be safe...")
    return(TRUE)
  }
  
  tryCatch({
    # Uses ODK Central's REST API directly (not OData) — /submissions
    # returns lightweight metadata including createdAt for every submission,
    # which is all we need to check for freshness.
    list_url <- paste0(Sys.getenv("ODKC_URL"),
                       "/v1/projects/", Sys.getenv("ODKC_PID"),
                       "/forms/", Sys.getenv("ODKC_FID_CAMPS"), "/submissions")
    resp <- httr::GET(list_url,
                      httr::authenticate(Sys.getenv("ODKC_UN"), Sys.getenv("ODKC_PW")),
                      httr::timeout(30))
    httr::stop_for_status(resp)
    subs <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
    latest_odk <- max(lubridate::as_datetime(subs$createdAt), na.rm = TRUE)
    
    if (latest_odk > last_in_data) {
      message("New ODK submissions since last pull — refreshing...")
      return(TRUE)
    }
    message("Data is current (last submission: ",
            format(last_in_data, "%d %b %Y %H:%M"), ")")
    return(FALSE)
  }, error = function(e) {
    message("Could not reach ODK — using cached data. (", e$message, ")")
    return(FALSE)
  })
}

if (needs_refresh()) {
  source("R/data_pull.R")
} else {
  nomad_data <- readRDS(rds_path)
}

date_labels <- list(
  all    = "Jun 2024 \u2013 present",
  `2024` = "January \u2013 December 2024",
  `2025` = "January \u2013 December 2025",
  `2026` = "January \u2013 April 2026"
)