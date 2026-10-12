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

source("R/data_pull.R")


date_labels <- list(
  all    = "Jun 2024 \u2013 present",
  `2024` = "January \u2013 December 2024",
  `2025` = "January \u2013 December 2025",
  `2026` = "January \u2013 April 2026"
)