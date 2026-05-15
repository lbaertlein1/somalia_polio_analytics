# =============================================================================
# get_boundaries.R
#
# Fetches Somalia public health boundaries from the ArcGIS FeatureServer:
# https://services.arcgis.com/5T5nSi527N4F7luB/arcgis/rest/services/
#   Somalia_Public_Health_Boundaries_2026/FeatureServer
#
# Layers fetched:
#   0 - som_admin_states_2026v1
#   1 - som_country_boundary_2026v1  (not saved separately, derived from districts)
#   2 - som_health_districts_2026v1  <- primary input for friction pipeline
#   3 - som_health_regions_2026v1
#
# Behaviour:
#   - If local RDS files already exist, loads from disk (no network call).
#   - Set force_refresh = TRUE to re-fetch regardless.
#   - Assigns districts, regions, states to the calling environment.
#
# Intended use:
#   source("get_boundaries.R")   # at top of build_national_friction_surface.R
# =============================================================================

suppressPackageStartupMessages(library(sf))

# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------

boundaries_dir    <- "data"
force_refresh     <- FALSE

districts_rds     <- file.path(boundaries_dir, "districts_shp.Rds")
regions_rds       <- file.path(boundaries_dir, "regions_shp.Rds")
states_rds        <- file.path(boundaries_dir, "states_shp.Rds")

feature_server    <- paste0(
  "https://services.arcgis.com/5T5nSi527N4F7luB/arcgis/rest/services/",
  "Somalia_Public_Health_Boundaries_2026/FeatureServer"
)

# -----------------------------------------------------------------------------
# Helper
# -----------------------------------------------------------------------------

fetch_layer <- function(layer_id, label) {
  url <- paste0(
    feature_server, "/", layer_id,
    "/query?where=1%3D1&outFields=*&f=geojson"
  )
  message("  Fetching ", label, "...")
  layer <- sf::st_read(url, quiet = TRUE) |> sf::st_make_valid()
  message("  ", nrow(layer), " features")
  layer
}

# -----------------------------------------------------------------------------
# Load or fetch
# -----------------------------------------------------------------------------

all_exist <- file.exists(districts_rds) &&
             file.exists(regions_rds)   &&
             file.exists(states_rds)

if (!force_refresh && all_exist) {

  message("Loading boundaries from local cache...")

  districts <- readRDS(districts_rds)
  regions   <- readRDS(regions_rds)
  states    <- readRDS(states_rds)

  message("  Districts: ", nrow(districts))
  message("  Regions:   ", nrow(regions))
  message("  States:    ", nrow(states))

} else {

  message("Fetching boundaries from ArcGIS FeatureServer...")

  dir.create(boundaries_dir, recursive = TRUE, showWarnings = FALSE)

  districts <- fetch_layer(2, "som_health_districts_2026v1")
  regions   <- fetch_layer(3, "som_health_regions_2026v1")
  states    <- fetch_layer(0, "som_admin_states_2026v1")
  
  
  #rename
  districts <- districts %>%
    mutate(
      zone_name     = DISP_LS,
      region_name   = DISP_L1,
      district_name = DISP_L2,
      admin_id      = as.integer(factor(DISP_L2)),
      region_id     = as.integer(factor(DISP_L1)),
      zone_id       = as.integer(factor(DISP_LS))
    )
  
  districts <- districts %>%
    mutate(u5_pop_density_km2 = WP_U5 / (Shape__Area / 1e6))
  
  saveRDS(districts, districts_rds)
  saveRDS(regions,   regions_rds)
  saveRDS(states,    states_rds)

  message("Boundaries saved to ", normalizePath(boundaries_dir))
}
