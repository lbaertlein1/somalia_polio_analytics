# =============================================================================
# Vaccine Buddy -- missed-area follow-up
#
# 1. Reads the current published health areas, team areas, IDP sites and
#    health facilities for a campaign from the health area adjuster DB.
# 2. Pulls Vaccine Buddy carrier GPS pings from ODK Central (ruODK) and
#    cleans them.
# 3. Classifies every WorldPop 100m cell inside the health areas as
#    populated / not, and visited / not, and groups populated-but-unvisited
#    cells into "missed places".
# 4. Sends each health area supervisor a WhatsApp message (click-to-send)
#    with a summary and a link to a phone-friendly page listing that
#    area's missed places, each with a Google Maps "navigate" button.
#
# Two faces, one app (routed in app.R on the ?t= query parameter):
#   - admin (login, this app's own accounts): run analysis, check contacts, send
#
# A district with no health areas drawn in the adjuster still works: its
# health areas are taken from the vaccination team names in the GPS data
# (infer_health_areas() in analysis.R), with no boundaries shown on maps.
#   - supervisor (?t=<token>, no login): their own missed places
# =============================================================================

library(shiny)
library(shinyjs)
library(sf)
# terra is used as terra::... only -- attaching it masks sort()/unique() with
# S4 generics that turn Shiny's silent req() into a real error
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(DT)
library(rhandsontable)
library(pool)
library(DBI)
library(RPostgres)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# First non-empty environment variable among `keys` (a blank line in .env
# counts as unset), else `default`.
env_first <- function(keys, default = '') {
  for (k in keys) { v <- Sys.getenv(k); if (nzchar(v)) return(v) }
  default
}

if (file.exists('.env')) dotenv::load_dot_env('.env')

# --- Config -------------------------------------------------------------------
ODK_TZ         <- Sys.getenv('ODK_TZ', 'Africa/Mogadishu')
WORLDPOP_FILE  <- Sys.getenv('WORLDPOP_FILE', 'data/som_u5_population_2026_100m.tif')
APP_PUBLIC_URL <- Sys.getenv('APP_PUBLIC_URL', '')   # e.g. https://<account>.shinyapps.io/vaccine_buddy/
# District boundaries, one folder per adjuster boundary set (copied from
# health_area_adjuster/modular_health_area_app/data/boundary_sets/<set>/
# districts_shp.Rds) -- for districts with no health areas drawn
BOUNDARY_DIR   <- Sys.getenv('BOUNDARY_DIR', 'data/boundary_sets')

# Excluded from analysis everywhere -- the adjuster's non-health-area
# categories (see its global.R extra_dfa_names).
NON_AREA_NAMES <- c('Inaccessible', 'Unpopulated')

# Analysis parameters, stored with every saved run. Only the few in
# PARAM_UI (ui_admin.R) can be changed, by admins on the Admin page
# (app_settings, vb_get_params()); the rest are fixed here.
DEFAULT_PARAMS <- list(
  max_accuracy_m  = 50,    # drop GPS fixes with worse reported accuracy
  spike_kmh       = 150,   # a fix reached AND left faster than this is a GPS spike
  walk_max_kmh    = 12,    # movement faster than this is driving, not a house visit
  max_gap_min     = 10,    # join consecutive fixes into a track only if this close in time...
  max_link_m      = 300,   # ...and in distance
  cell_m          = 100,   # grid cell size (m); a cell is visited if a walking-speed GPS point falls inside it
  min_u5_cell     = 1,     # WorldPop under-5 count for a cell to count as populated (below 1 = unpopulated)
  max_place_cells = 25,    # larger missed areas are split into places of about this many cells (~450 m across)...
  max_place_children = 250, # ...or about this many estimated children under 5, whichever gives more places
  min_place_u5    = 10,    # drop missed places with fewer estimated under-5s (Aug 2026: drops 9 of 57 places, 2% of missed children)
  min_ha_fixes    = 200,   # health areas with fewer usable fixes had no carriers: reported, not analysed
  no_map_reach_m  = 1000,  # districts with no drawn health areas: look for missed places this far from team tracks
  idp_near_m      = 100,   # flag places this close to an IDP site ("at" a site -- in Mogadishu IDP sites are a few hundred m apart)
  place_name_max_m = 300,  # describe a place from the nearest local place name (campaign monitoring) or IDP site within this distance...
  label_max_m     = 3000   # ...else from a health facility / IDP site within this distance
)

# Each team's role, until an admin sets it for the campaign on the Admin
# page (team_roles() in analysis.R): teams whose name contains this are
# ignored (FETP monitors: their GPS doesn't count as visits)...
IGNORE_TEAMS_DEFAULT  <- 'FETP'
# ...or vaccinate (GPS counts as visits) but belong to no health area, so
# missed places are never assigned to them
NO_AREA_TEAMS_DEFAULT <- 'Surge'

# --- Code -----------------------------------------------------------------------
for (f in list.files('R', pattern = '\\.R$', full.names = TRUE)) source(f, local = TRUE)

# --- Connections ------------------------------------------------------------------
adj_pool <- tryCatch(adj_connect(), error = function(e) { message('Adjuster DB connection failed: ', conditionMessage(e)); NULL })
vb_pool  <- tryCatch(vb_connect(),  error = function(e) { message('Vaccine Buddy DB connection failed: ', conditionMessage(e)); NULL })
onStop(function() {
  if (!is.null(adj_pool)) pool::poolClose(adj_pool)
  if (!is.null(vb_pool))  pool::poolClose(vb_pool)
})

u5_rast <- tryCatch(terra::rast(WORLDPOP_FILE),
                    error = function(e) { message('WorldPop raster not loaded (', WORLDPOP_FILE, '): ', conditionMessage(e)); NULL })
