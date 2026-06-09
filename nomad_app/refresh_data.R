# refresh_data.R
# Run this in a SEPARATE R session (not while the Shiny app is running).
# Pulls fresh data from ODK Central, builds data/nomad_data.rds.
#
# Usage (in R console, with working directory set to the app folder):
#   source("refresh_data.R")

readRenviron(".Renviron")
source("R/data_pull.R")