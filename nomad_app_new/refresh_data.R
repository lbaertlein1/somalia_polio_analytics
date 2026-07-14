# refresh_data.R
# Force a fresh pull from ODK Central, replacing data/nomad_data.rds.
# Run this when you want to update the data.
# Usage: source("refresh_data.R")

if (file.exists(".Renviron")) readRenviron(".Renviron")
source("R/data_pull.R")