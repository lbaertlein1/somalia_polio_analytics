library(shiny)
library(sf)
library(dplyr)
library(jsonlite)
library(geojsonsf)
library(terra)
library(exactextractr)
library(raster)
library(DT)
library(rhandsontable)
library(leaflet)
library(later)
library(viridis)
library(smoothr)
library(dotenv)
library(zip)
library(pool)
library(DBI)
library(RPostgres)
library(bcrypt)
library(Rcpp)
library(httr)

`%||%` <- function(a, b) if (!is.null(a)) a else b

if (file.exists('.env')) dotenv::load_dot_env('.env')


# =============================================================================
# Data files
# =============================================================================
districts_file           <- 'data/districts_shp.Rds'
worldpop_t_u1_1to4_file  <- 'data/som_u5_population_2025_100m.tif'

# =============================================================================
# App constants
# =============================================================================
default_grid_n        <- 100
n_start_dfas          <- 5    # only used as a fallback default now — real health
                              # area count comes from facility-based seeding once
                              # coordination sites are selected
min_brush_m           <- 50
max_brush_m           <- 10000
brush_step_m          <- 50
show_pop_default      <- FALSE
boundary_only_default <- FALSE

starter_dfa_names <- paste('Health Area', seq_len(n_start_dfas))
extra_dfa_names   <- c('Inaccessible', 'Unpopulated')
all_dfa_names     <- c(starter_dfa_names, extra_dfa_names)

selected_fill_color    <- '#FFD400'
nonselected_fill_color <- '#757575'
special_fill_colors    <- c('Inaccessible' = '#D7301F', 'Unpopulated' = '#FFFFFF')

pop_palette <- colorRampPalette(c(
  '#feebe2', '#fbb4b9', '#fbb4b9', '#c51b8a', '#7a0177'
))

# =============================================================================
# Helpers
#
# mod_db_v2.R replaces mod_db.R. download_helpers_v2.R replaces
# download_helpers.R (built around planning_data/microplan, which no longer
# exists). idp_helpers.R is new. subdivision_helpers.R is unchanged — still
# used both for the intro tab's reference layer and by idp_helpers.R's
# fetch pattern.
# =============================================================================
source('helpers/app_helpers.R', local = TRUE)
source('helpers/download_helpers_v2.R', local = TRUE)
source('helpers/mod_db_v2.R', local = TRUE)
source('helpers/subdivision_helpers.R', local = TRUE)
source('helpers/idp_helpers.R', local = TRUE)
source('helpers/printable_export.R', local = TRUE)

sourceCpp('bfs_propagate.cpp')

# =============================================================================
# Districts shapefile — unchanged, still a fixed set (tied to pre-cut
# per-district friction .tif files)
# =============================================================================
districts_path <- path.expand(districts_file)
if (!file.exists(districts_path)) {
  stop(sprintf('Could not find districts file: %s', districts_file))
}

districts_shp          <- readRDS(districts_path)
all_district_densities <- districts_shp$u5_pop_density_km2
districts_shp          <- safe_make_valid(districts_shp)

required_cols <- c('zone_name', 'region_name', 'district_name')
missing_cols  <- setdiff(required_cols, names(districts_shp))
if (length(missing_cols) > 0) {
  stop(sprintf(
    'districts_shp is missing required column(s): %s',
    paste(missing_cols, collapse = ', ')
  ))
}

zone_choices <- sort(unique(as.character(stats::na.omit(districts_shp$zone_name))))


# Connect to Database — DB_NAME in .env should point at the v2 database
# (e.g. somalia_health_areas_v2), not the old one.
cat('DB_HOST:', Sys.getenv('DB_HOST'), '\n')
cat('DB_NAME:', Sys.getenv('DB_NAME'), '\n')
pool <- tryCatch(
  db_connect(),
  error = function(e) { message('DB connection failed: ', e$message); NULL }
)
onStop(function() pool::poolClose(pool))

# =============================================================================
# Module sources
# =============================================================================

source('tabs/auth/mod_auth.R',                  local = TRUE)
source('tabs/session/mod_session_manager_v2.R', local = TRUE)

source('tabs/intro/mod_intro_tab_v2.R',          local = TRUE)

source('tabs/orientation/mod_orientation_tab.R', local = TRUE)   # unchanged

source('tabs/facility/facility_helpers.R',             local = TRUE)
source('tabs/facility/mod_facility_map.R',             local = TRUE)   # unchanged this pass — see note below
source('tabs/facility/mod_facility_table.R',           local = TRUE)   # unchanged
source('tabs/facility/mod_facility_tab.R',             local = TRUE)   # v2: IDP fetch/review/submit added

source('tabs/health_area/health_area_helpers.R',                local = TRUE)   # unchanged
source('tabs/health_area/mod_health_area_controls.R',           local = TRUE)   # unchanged
source('tabs/health_area/mod_health_area_map.R',                local = TRUE)   # unchanged
source('tabs/health_area/mod_health_area_population.R',         local = TRUE)   # unchanged — reused by Team Areas too
source('tabs/health_area/mod_health_area_tab.R',                local = TRUE)   # unchanged — still submits stage "areas", which mod_db_v2.R matches
source('tabs/health_area/mod_initial_health_area_generation.R', local = TRUE)   # v2: compactness/max_cost removed, distance-blend removed

source('tabs/team_area/team_area_helpers.R', local = TRUE)   # new
source('tabs/team_area/mod_team_area_map.R', local = TRUE)   # new
source('tabs/team_area/mod_team_area_controls.R', local = TRUE)   # new
source('tabs/team_area/mod_team_area_tab.R', local = TRUE)   # new

# microplan tab removed entirely — no source line for it.

source('tabs/admin/mod_admin_tab_v2.R', local = TRUE)

# =============================================================================
# WorldPop raster — loaded after helpers are sourced (load_worldpop_u5_raster
# is defined in health_area_helpers.R)
# =============================================================================
u5_rast <- tryCatch(
  load_worldpop_u5_raster(t_u1_1to4_file = worldpop_t_u1_1to4_file),
  error = function(e) {
    message('WorldPop raster not loaded: ', e$message)
    NULL
  }
)
if (is.null(u5_rast)) {
  message('WARNING: WorldPop raster is NULL — population features disabled. File: ', worldpop_t_u1_1to4_file)
} else {
  cat('WorldPop raster loaded:', worldpop_t_u1_1to4_file, '\n')
}
