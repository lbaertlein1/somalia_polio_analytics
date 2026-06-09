# global.R — loaded once on app startup
# To update data: run source("refresh_data.R") in a separate R session, then restart the app

library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(sf)
library(DT)

source("R/utils.R")
source("R/mod_kpi.R")
source("R/mod_movement.R")
source("R/mod_zerodose.R")
source("R/mod_access.R")
source("R/mod_report.R")

rds_path <- "data/nomad_data.rds"
if (!file.exists(rds_path)) {
  stop("data/nomad_data.rds not found. Run source('refresh_data.R') in a separate R session to build it.")
}
nomad_data <- readRDS(rds_path)

date_labels <- list(
  all    = "Jun 2024 \u2013 present",
  `2024` = "January \u2013 December 2024",
  `2025` = "January \u2013 December 2025",
  `2026` = "January \u2013 April 2026"
)