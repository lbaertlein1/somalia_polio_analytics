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
library(ruODK)
library(leaflet)

source("R/utils.R")
source("R/mod_insights.R")
source("R/mod_kpi.R")
source("R/mod_movement.R")
source("R/mod_zerodose.R")
source("R/mod_access.R")
source("R/mod_report.R")

#rds_path <- "data/nomad_data.rds"
#pull_error <- tryCatch({
#  source("R/data_pull.R")
#  NULL
#}, error = function(e) e)

#if (!is.null(pull_error)) {
#  if (!file.exists(rds_path)) stop(pull_error)
#  warning("Fresh data pull failed; using cached nomad_data.rds: ",
#          conditionMessage(pull_error), call. = FALSE)
#  nomad_data <- readRDS(rds_path)
#}


date_labels <- list(
  all    = "Jun 2024 \u2013 present",
  `2024` = "January \u2013 December 2024",
  `2025` = "January \u2013 December 2025",
  `2026` = "January \u2013 April 2026"
)