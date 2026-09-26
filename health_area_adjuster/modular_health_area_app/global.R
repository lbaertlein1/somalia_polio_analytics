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
boundary_sets_dir        <- 'data/boundary_sets'   # one folder per set: <set>/districts_shp.Rds
friction_root_dir        <- 'data/friction'        # one folder per set: <set>/district_standardized/
boundary_lines_file      <- 'data/boundary_lines/somalia_primary_lines.rds'   # build_boundary_lines.R
worldpop_t_u1_1to4_file  <- 'data/som_u5_population_2026_100m.tif'

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
# mod_db_v2.R replaces mod_db.R, now also owning team_area_versions
# (independently-versioned per health area — see its own header comment)
# and campaign_districts. download_helpers_v2.R replaces
# download_helpers.R (built around planning_data/microplan, which no
# longer exists) and now sources team-area export data from
# team_area_versions rather than a health-area version's own snapshot.
# idp_helpers.R is new. subdivision_helpers.R is unchanged — still used
# both for the intro tab's reference layer and by idp_helpers.R's fetch
# pattern.
# =============================================================================
source('helpers/app_helpers.R', local = TRUE)
source('helpers/download_helpers_v2.R', local = TRUE)
source('helpers/mod_db_v2.R', local = TRUE)
source('helpers/subdivision_helpers.R', local = TRUE)
source('helpers/idp_helpers.R', local = TRUE)
source('helpers/printable_export.R', local = TRUE)

sourceCpp('bfs_propagate.cpp')

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
# District boundary sets
#
# Every set in data/boundary_sets/<set>/districts_shp.Rds is loaded. Each
# campaign uses ONE set (campaigns.boundary_set, fixed at creation), so
# anything that draws or measures a district FOR A CAMPAIGN must use
# districts_shp_for_campaign(campaign_id), and friction files come from
# friction_dir_for_campaign(campaign_id).
#
# districts_shp itself is the CURRENT set (boundary_sets.is_current). Use it
# only where the set doesn't matter: district/region/zone names and IDs,
# which are identical across sets.
# =============================================================================
.set_dirs <- list.dirs(boundary_sets_dir, recursive = FALSE)
.set_dirs <- .set_dirs[file.exists(file.path(.set_dirs, 'districts_shp.Rds'))]
if (length(.set_dirs) == 0) stop('No boundary sets found in ', boundary_sets_dir)

boundary_sets_shp <- lapply(.set_dirs, function(d) {
  x <- safe_make_valid(readRDS(file.path(d, 'districts_shp.Rds')))
  missing_cols <- setdiff(c('zone_name', 'region_name', 'district_name'), names(x))
  if (length(missing_cols) > 0)
    stop(sprintf('%s is missing required column(s): %s', d, paste(missing_cols, collapse = ', ')))
  x
})
names(boundary_sets_shp) <- basename(.set_dirs)
cat('Boundary sets loaded:', paste(names(boundary_sets_shp), collapse = ', '), '\n')

current_boundary_set <- tryCatch(
  DBI::dbGetQuery(pool, 'SELECT set_label FROM boundary_sets WHERE is_current')$set_label[1],
  error = function(e) NA_character_)
if (is.na(current_boundary_set) || !(current_boundary_set %in% names(boundary_sets_shp))) {
  current_boundary_set <- tail(sort(names(boundary_sets_shp)), 1)
  message('WARNING: current boundary set not readable from the database -- using ', current_boundary_set)
}
cat('Current boundary set:', current_boundary_set, '\n')

districts_shp          <- boundary_sets_shp[[current_boundary_set]]
all_district_densities <- districts_shp$u5_pop_density_km2
zone_choices <- sort(unique(as.character(stats::na.omit(districts_shp$zone_name))))

# campaign_id -> set label. Cached for the life of the R process: a
# campaign's set is fixed at creation, so it only changes if an admin
# reassigns it directly in the database -- restart the app after that.
.campaign_set_cache <- new.env()
campaign_boundary_set <- function(campaign_id) {
  if (is.null(campaign_id) || length(campaign_id) != 1 || is.na(campaign_id)) return(current_boundary_set)
  key <- as.character(campaign_id)
  if (!is.null(.campaign_set_cache[[key]])) return(.campaign_set_cache[[key]])
  set <- tryCatch(
    DBI::dbGetQuery(pool, 'SELECT boundary_set FROM campaigns WHERE campaign_id = $1',
                    list(as.integer(campaign_id)))$boundary_set[1],
    error = function(e) NA_character_)
  if (is.na(set) || !(set %in% names(boundary_sets_shp)))
    stop(sprintf('Campaign %s uses boundary set "%s", which is not in %s', key, set, boundary_sets_dir))
  assign(key, set, envir = .campaign_set_cache)
  set
}

districts_shp_for_campaign <- function(campaign_id) boundary_sets_shp[[campaign_boundary_set(campaign_id)]]

friction_dir_for_campaign <- function(campaign_id)
  file.path(getwd(), friction_root_dir, campaign_boundary_set(campaign_id), 'district_standardized')

# =============================================================================
# Primary roads + primary rivers (build_boundary_lines.R, EPSG:3857).
# Health-area and team-area generation treat these as soft crossing
# barriers -- same channel and penalty as subdivision boundaries -- so
# boundaries tend to follow them instead of cutting across. Optional: if
# the file is missing, generation simply runs without them.
# =============================================================================
boundary_lines_sf <- if (file.exists(boundary_lines_file)) {
  tryCatch(readRDS(boundary_lines_file), error = function(e) { message('Boundary lines not loaded: ', e$message); NULL })
} else {
  message('WARNING: ', boundary_lines_file, ' not found -- roads/rivers not used as boundary preferences')
  NULL
}
if (!is.null(boundary_lines_sf))
  cat('Boundary lines loaded:', nrow(boundary_lines_sf), 'primary road/river lines\n')

# The primary road/river lines within (and buffer_m around) an area, as an
# sf of LINESTRINGs in EPSG:4326, or NULL if there are none.
primary_lines_for_area <- function(area_sf, buffer_m = 1000) {
  if (is.null(boundary_lines_sf) || is.null(area_sf) || nrow(area_sf) == 0) return(NULL)
  area <- sf::st_buffer(sf::st_union(sf::st_geometry(sf::st_transform(area_sf, 3857))), buffer_m)
  hit  <- boundary_lines_sf[lengths(sf::st_intersects(boundary_lines_sf, area)) > 0, ]
  if (nrow(hit) == 0) return(NULL)
  g <- suppressWarnings(sf::st_intersection(sf::st_geometry(hit), area))
  g <- g[!sf::st_is_empty(g)]
  g <- tryCatch(suppressWarnings(sf::st_collection_extract(g, 'LINESTRING')), error = function(e) g)
  g <- g[sf::st_geometry_type(g) %in% c('LINESTRING', 'MULTILINESTRING')]
  if (length(g) == 0) return(NULL)
  # ONE feature (a single MULTILINESTRING): these lines are also sent to
  # the browser map as GeoJSON, where the app expects one feature
  sf::st_sf(geometry = sf::st_transform(sf::st_union(g), 4326))
}

# Combine line layers (any may be NULL/empty) into one geometry-only sf,
# EPSG:4326 -- NULL if nothing is left.
combine_line_layers <- function(...) {
  parts <- Filter(function(x) !is.null(x) && inherits(x, c('sf', 'sfc')) && length(sf::st_geometry(x)) > 0, list(...))
  if (length(parts) == 0) return(NULL)
  geoms <- lapply(parts, function(x) sf::st_geometry(sf::st_transform(x, 4326)))
  # Unioned into ONE feature -- the combined layer is sent to the browser
  # map as subdivisionGeojson, which must be a single GeoJSON string; one
  # feature per line made it a vector of strings, which Shiny can't send.
  sf::st_sf(geometry = sf::st_union(do.call(c, geoms)))
}

# =============================================================================
# Module sources
# =============================================================================

source('tabs/auth/mod_auth.R',                  local = TRUE)
source('tabs/session/mod_session_manager_v2.R', local = TRUE)

source('tabs/intro/mod_intro_tab_v2.R',          local = TRUE)

source('tabs/orientation/mod_orientation_tab.R', local = TRUE)   # unchanged

source('tabs/scope/mod_campaign_scope_controls.R', local = TRUE)   # new — controls sidebar for Campaign Scope, structurally mirrors mod_health_area_controls.R
source('tabs/scope/mod_campaign_scope_tab.R',    local = TRUE)   # new — Campaign Scope stage, between Landmarks and Facilities

source('tabs/facility/facility_helpers.R',             local = TRUE)
source('tabs/facility/mod_facility_map.R',             local = TRUE)   # unchanged this pass — see note below
source('tabs/facility/mod_facility_table.R',           local = TRUE)   # unchanged
source('tabs/facility/mod_facility_tab.R',             local = TRUE)   # v2: IDP fetch/review/submit added

source('tabs/health_area/health_area_helpers.R',                local = TRUE)   # unchanged
source('tabs/health_area/mod_health_area_controls.R',           local = TRUE)   # unchanged
source('tabs/health_area/mod_health_area_map.R',                local = TRUE)   # unchanged
source('tabs/health_area/mod_health_area_population.R',         local = TRUE)   # unchanged — reused by Team Areas too
source('tabs/health_area/mod_health_area_tab.R',                local = TRUE)   # v2: submit-flow "make current" prompt + lock messaging added, still submits stage "areas"
source('tabs/health_area/mod_initial_health_area_generation.R', local = TRUE)   # v2: compactness/max_cost removed, distance-blend removed

source('tabs/team_area/team_area_helpers.R', local = TRUE)   # new
source('tabs/team_area/mod_team_area_map.R', local = TRUE)   # new
source('tabs/team_area/mod_team_area_controls.R', local = TRUE)   # v2: health-area dropdown removed — chosen externally now (see mod_intro_tab_v2.R)
source('tabs/team_area/mod_team_area_tab.R', local = TRUE)   # v2: restructured — scoped to one health area per session, no more multi-area cache

# microplan tab removed entirely — no source line for it.

source('tabs/admin/mod_admin_tab_v2.R', local = TRUE)

source('tabs/export/mod_export_tab.R', local = TRUE)   # new — standalone export page, current/published data only

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