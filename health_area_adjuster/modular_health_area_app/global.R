library(shiny)
library(sf)
library(dplyr)
library(jsonlite)
library(geojsonsf)
library(terra)
library(exactextractr)
library(raster)
library(DT)

districts_file <- 'data/districts_shp.Rds'
worldpop_t_u1_1to4_file <- 'data/som_u5_population_2025_100m.tif'

default_grid_n <- 100
n_start_dfas <- 5

min_brush_m <- 50
max_brush_m <- 10000
brush_step_m <- 50

show_pop_default <- FALSE
boundary_only_default <- FALSE

starter_dfa_names <- paste('Health Area', seq_len(n_start_dfas))
extra_dfa_names <- c('Inaccessible', 'Unpopulated')
all_dfa_names <- c(starter_dfa_names, extra_dfa_names)

selected_fill_color <- '#FFD400'
nonselected_fill_color <- '#757575'
special_fill_colors <- c(
  'Inaccessible' = '#D7301F',
  'Unpopulated' = '#FFFFFF'
)

pop_palette <- colorRampPalette(c(
  '#feebe2', '#fbb4b9', '#fbb4b9', '#c51b8a', '#7a0177'
))

source('helpers/app_helpers.R', local = TRUE)

districts_path <- path.expand(districts_file)
if (!file.exists(districts_path)) {
  stop(sprintf('Could not find districts file: %s', districts_file))
}

districts_shp <- readRDS(districts_path)
districts_shp <- safe_make_valid(districts_shp)

required_cols <- c('zone_name', 'region_name', 'district_name')
missing_cols <- setdiff(required_cols, names(districts_shp))
if (length(missing_cols) > 0) {
  stop(sprintf(
    'districts_shp is missing required column(s): %s',
    paste(missing_cols, collapse = ', ')
  ))
}

zone_choices <- sort(unique(as.character(stats::na.omit(districts_shp$zone_name))))

source('tabs/intro/mod_intro_tab.R', local = TRUE)
source('tabs/facility/mod_facility_tab.R', local = TRUE)
source('tabs/health_area/health_area_helpers.R', local = TRUE)
source('tabs/health_area/mod_health_area_controls.R', local = TRUE)
source('tabs/health_area/mod_health_area_map.R', local = TRUE)
source('tabs/health_area/mod_health_area_population.R', local = TRUE)
source('tabs/health_area/mod_health_area_tab.R', local = TRUE)

