# =============================================================================
# build_national_friction_surface.R
#
# Builds a national Somalia friction surface once offline, then saves it as a
# raster for reuse in the Shiny app.
#
# Logic for health area mapping:
#   - Dense population = higher friction (teams move more slowly through many households)
#   - Sparse population = lower friction
#   - Roads modestly reduce friction
#   - Rivers strongly increase friction
#   - Bridges locally reduce river penalties
#   - Water bodies are impassable
#   - District boundaries are impassable
#
# Required inputs:
#   - data/districts_shp.Rds
#   - data/som_u5_population_2025_100m.tif
#   - data/osm_inputs/somalia_roads.gpkg
#   - data/osm_inputs/somalia_rivers.gpkg
#   - data/osm_inputs/somalia_bridges.gpkg
#   - data/osm_inputs/somalia_water_bodies.gpkg
#
# Optional inputs:
#   - landcover raster
#   - slope raster
#   - landcover lookup csv
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
})

# =============================================================================
# USER SETTINGS
# =============================================================================

cfg <- list(
  # Required inputs
  districts_file = "data/districts_shp.Rds",
  worldpop_t_u1_1to4_file = "data/som_u5_population_2025_100m.tif",
  roads_file = "data/osm_inputs/somalia_roads.gpkg",
  rivers_file = "data/osm_inputs/somalia_rivers.gpkg",
  bridges_file = "data/osm_inputs/somalia_bridges.gpkg",
  water_bodies_file = "data/osm_inputs/somalia_water_bodies.gpkg",
  
  # Optional inputs
  landcover_file = NULL,
  slope_file = NULL,
  landcover_lookup_file = NULL,
  
  # Output
  output_dir = "data/friction",
  output_friction_file = "somalia_friction_100m.tif",
  output_template_file = "somalia_template_100m.tif",
  output_population_cost_file = "somalia_population_cost_100m.tif",
  
  # Working projection and target resolution
  target_crs = "EPSG:3857",
  target_resolution_m = 100,
  
  # Friction parameters
  friction = list(
    base_walk = 1.0,
    min_positive = 0.01,
    impassable = 1e6,
    
    # Population workload gradient
    # Dense population => higher friction
    pop_min_cost = 0.7,   # very sparse
    pop_max_cost = 6.0,   # very dense
    pop_zero_cost = 0.5,  # truly empty land easier to traverse than settled land
    
    # Road modifiers
    # These are modest reductions only, not hard replacements
    road_primary = 0.85,
    road_secondary = 0.90,
    road_minor = 0.95,
    road_track = 1.00,
    
    # Rivers
    # These multiply friction strongly
    river_major = 25,
    river_minor = 8,
    river_buf_m = 200,
    
    # Bridges
    # Apply after river penalty to locally reopen crossings
    bridge_primary = 0.60,
    bridge_secondary = 0.70,
    bridge_minor = 0.80,
    bridge_track = 0.90,
    
    # District boundaries as hard barriers
    district_boundary_cost = 1e6,
    district_boundary_buf_m = 100,
    
    # Optional land cover multipliers
    landcover = c(
      `10` = 1.4,   # tree cover
      `20` = 1.1,   # shrubland
      `30` = 1.0,   # grassland
      `40` = 0.95,  # cropland
      `50` = 1.1,   # built-up
      `60` = 1.0,   # bare/sparse
      `80` = 1e6,   # water
      `90` = 1.5    # wetland
    )
  )
)

# =============================================================================
# HELPERS
# =============================================================================

message_line <- function(...) {
  message(paste0(...))
}

assert_file_exists <- function(path, label) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    stop(label, " not found: ", path, call. = FALSE)
  }
}

read_vector_safe <- function(path, label, target_crs) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    message_line("Skipping missing ", label, ".")
    return(NULL)
  }
  
  x <- sf::st_read(path, quiet = TRUE) |>
    sf::st_make_valid()
  
  if (!is.null(target_crs)) {
    x <- sf::st_transform(x, target_crs)
  }
  
  x
}

read_districts_rds <- function(path, target_crs) {
  assert_file_exists(path, "districts_file")
  
  x <- readRDS(path)
  if (!inherits(x, "sf")) {
    stop("districts_file must contain an sf object.", call. = FALSE)
  }
  
  x |>
    sf::st_make_valid() |>
    sf::st_transform(target_crs)
}

build_template_from_worldpop <- function(worldpop_file, target_crs, target_resolution_m) {
  assert_file_exists(worldpop_file, "worldpop_t_u1_1to4_file")
  
  wp <- terra::rast(worldpop_file)
  wp_proj <- terra::project(wp, target_crs, method = "bilinear")
  
  ext_proj <- terra::ext(wp_proj)
  
  template <- terra::rast(
    xmin = ext_proj[1],
    xmax = ext_proj[2],
    ymin = ext_proj[3],
    ymax = ext_proj[4],
    resolution = target_resolution_m,
    crs = target_crs
  )
  
  template <- terra::resample(wp_proj, template, method = "bilinear")
  names(template) <- "u5_pop"
  template
}

load_landcover_costs <- function(lookup_file, default_costs) {
  if (is.null(lookup_file) || !nzchar(lookup_file)) {
    return(default_costs)
  }
  
  assert_file_exists(lookup_file, "landcover_lookup_file")
  
  lk <- read.csv(lookup_file, stringsAsFactors = FALSE)
  
  req_cols <- c("class_value", "friction_cost")
  missing_cols <- setdiff(req_cols, names(lk))
  if (length(missing_cols) > 0) {
    stop(
      "landcover_lookup_file is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  vals <- as.character(lk$class_value)
  costs <- as.numeric(lk$friction_cost)
  names(costs) <- vals
  costs
}

classify_roads <- function(roads_sf, friction_cfg) {
  if (is.null(roads_sf) || nrow(roads_sf) == 0) {
    return(roads_sf)
  }
  
  if (!"road_class" %in% names(roads_sf)) {
    if ("highway" %in% names(roads_sf)) {
      roads_sf$road_class <- dplyr::case_when(
        roads_sf$highway %in% c("motorway", "trunk", "primary") ~ "primary",
        roads_sf$highway %in% c("secondary", "tertiary") ~ "secondary",
        roads_sf$highway %in% c("unclassified", "residential") ~ "minor",
        roads_sf$highway %in% c("track", "path", "service") ~ "track",
        TRUE ~ "minor"
      )
    } else {
      roads_sf$road_class <- "minor"
    }
  }
  
  roads_sf$friction_val <- dplyr::case_when(
    roads_sf$road_class == "primary" ~ friction_cfg$road_primary,
    roads_sf$road_class == "secondary" ~ friction_cfg$road_secondary,
    roads_sf$road_class == "minor" ~ friction_cfg$road_minor,
    roads_sf$road_class == "track" ~ friction_cfg$road_track,
    TRUE ~ 1.0
  )
  
  roads_sf
}

classify_rivers <- function(rivers_sf, friction_cfg) {
  if (is.null(rivers_sf) || nrow(rivers_sf) == 0) {
    return(rivers_sf)
  }
  
  if (!"river_type" %in% names(rivers_sf)) {
    if ("waterway" %in% names(rivers_sf)) {
      rivers_sf$river_type <- ifelse(rivers_sf$waterway == "river", "major", "minor")
    } else {
      rivers_sf$river_type <- "minor"
    }
  }
  
  rivers_sf$friction_val <- ifelse(
    rivers_sf$river_type == "major",
    friction_cfg$river_major,
    friction_cfg$river_minor
  )
  
  rivers_sf
}

classify_bridges <- function(bridges_sf, friction_cfg) {
  if (is.null(bridges_sf) || nrow(bridges_sf) == 0) {
    return(bridges_sf)
  }
  
  if (!"bridge_class" %in% names(bridges_sf)) {
    if ("highway" %in% names(bridges_sf)) {
      bridges_sf$bridge_class <- dplyr::case_when(
        bridges_sf$highway %in% c("motorway", "trunk", "primary") ~ "primary",
        bridges_sf$highway %in% c("secondary", "tertiary") ~ "secondary",
        bridges_sf$highway %in% c("unclassified", "residential") ~ "minor",
        TRUE ~ "track"
      )
    } else {
      bridges_sf$bridge_class <- "minor"
    }
  }
  
  bridges_sf$friction_val <- dplyr::case_when(
    bridges_sf$bridge_class == "primary" ~ friction_cfg$bridge_primary,
    bridges_sf$bridge_class == "secondary" ~ friction_cfg$bridge_secondary,
    bridges_sf$bridge_class == "minor" ~ friction_cfg$bridge_minor,
    bridges_sf$bridge_class == "track" ~ friction_cfg$bridge_track,
    TRUE ~ 1.0
  )
  
  bridges_sf
}

rasterize_if_present <- function(x, template, field, fun = NULL, background = NA) {
  if (is.null(x) || nrow(x) == 0) {
    return(NULL)
  }
  
  vx <- terra::vect(x)
  
  if (is.null(fun)) {
    terra::rasterize(vx, template, field = field, background = background)
  } else {
    terra::rasterize(vx, template, field = field, fun = fun, background = background)
  }
}

apply_landcover_costs <- function(friction, landcover_r, landcover_costs) {
  out <- friction
  for (lc_val in names(landcover_costs)) {
    out[landcover_r == as.integer(lc_val)] <- landcover_costs[[lc_val]]
  }
  out
}

apply_slope_adjustment <- function(friction, slope_r) {
  slope_rad <- slope_r * (pi / 180)
  tobler <- exp(-3.5 * abs(tan(slope_rad) + 0.05))
  tobler_flat <- exp(-3.5 * abs(0.05))
  slope_factor <- tobler_flat / tobler
  slope_factor <- terra::ifel(is.na(slope_factor), 1, slope_factor)
  friction * slope_factor
}

apply_population_gradient <- function(
    pop_r,
    target_template = NULL,
    aggregate_factor = 10,      # 100 m -> 1 km if factor = 10
    smoothing_radius_m = 1500,  # neighborhood scale
    min_cost = 0.5,
    max_cost = 10.0,
    zero_pop_cost = 0.4
) {
  
  message("  Aggregating population raster for workload smoothing...")
  
  # aggregate first to reduce memory burden
  pop_coarse <- terra::aggregate(
    pop_r,
    fact = aggregate_factor,
    fun = sum,
    na.rm = TRUE
  )
  
  message("  Building smoothing kernel...")
  w <- terra::focalMat(
    pop_coarse,
    d = smoothing_radius_m,
    type = "circle"
  )
  
  message("  Applying focal smoothing on coarse raster...")
  pop_smooth <- terra::focal(
    pop_coarse,
    w = w,
    fun = sum,
    na.rm = TRUE,
    fillvalue = 0
  )
  
  names(pop_smooth) <- "pop_smooth"
  
  message("  Converting smoothed population to friction cost...")
  pop_log <- log1p(pop_smooth)
  
  gmin <- as.numeric(terra::global(pop_log, "min", na.rm = TRUE)[[1]])
  gmax <- as.numeric(terra::global(pop_log, "max", na.rm = TRUE)[[1]])
  
  if (is.na(gmin) || is.na(gmax) || gmax <= gmin) {
    pop_cost <- pop_smooth
    pop_cost[] <- 1
  } else {
    pop_norm <- (pop_log - gmin) / (gmax - gmin)
    
    # dense population -> higher friction
    pop_cost <- min_cost + pop_norm * (max_cost - min_cost)
    
    # near-empty areas -> low friction
    pop_cost <- terra::ifel(
      pop_smooth <= 0.01,
      zero_pop_cost,
      pop_cost
    )
  }
  
  names(pop_cost) <- "pop_cost"
  
  if (!is.null(target_template)) {
    message("  Resampling population cost back to target template...")
    pop_cost_out <- terra::resample(pop_cost, target_template, method = "bilinear")
  } else {
    pop_cost_out <- pop_cost
  }
  
  list(
    pop_cost = pop_cost_out,
    pop_cost_coarse = pop_cost,
    pop_smooth = pop_smooth,
    pop_coarse = pop_coarse
  )
}

write_raster_safe <- function(x, filename) {
  terra::writeRaster(
    x,
    filename,
    overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES")
  )
}

# =============================================================================
# BUILD
# =============================================================================

message_line("============================================================")
message_line("Building national friction surface")
message_line("============================================================")

assert_file_exists(cfg$districts_file, "districts_file")
assert_file_exists(cfg$worldpop_t_u1_1to4_file, "worldpop_t_u1_1to4_file")
assert_file_exists(cfg$roads_file, "roads_file")
assert_file_exists(cfg$rivers_file, "rivers_file")
assert_file_exists(cfg$bridges_file, "bridges_file")
assert_file_exists(cfg$water_bodies_file, "water_bodies_file")

dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)

message_line("[1/9] Loading districts and deriving national boundary...")
districts_sf <- read_districts_rds(cfg$districts_file, cfg$target_crs)

if (nrow(districts_sf) == 0) {
  stop("districts_file has zero features.", call. = FALSE)
}

country_union <- districts_sf |>
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
  sf::st_make_valid()

message_line("  Districts loaded: ", nrow(districts_sf))

message_line("[2/9] Building projected WorldPop template...")
template <- build_template_from_worldpop(
  worldpop_file = cfg$worldpop_t_u1_1to4_file,
  target_crs = cfg$target_crs,
  target_resolution_m = cfg$target_resolution_m
)

template_file <- file.path(cfg$output_dir, cfg$output_template_file)
write_raster_safe(template, template_file)
message_line("  Saved template: ", normalizePath(template_file))

message_line("[3/9] Loading required vector inputs...")
roads <- read_vector_safe(cfg$roads_file, "roads", cfg$target_crs)
rivers <- read_vector_safe(cfg$rivers_file, "rivers", cfg$target_crs)
bridges <- read_vector_safe(cfg$bridges_file, "bridges", cfg$target_crs)
water_bodies <- read_vector_safe(cfg$water_bodies_file, "water bodies", cfg$target_crs)

roads <- classify_roads(roads, cfg$friction)
rivers <- classify_rivers(rivers, cfg$friction)
bridges <- classify_bridges(bridges, cfg$friction)

message_line("[4/9] Loading optional raster inputs...")
landcover <- NULL
if (!is.null(cfg$landcover_file) && nzchar(cfg$landcover_file) && file.exists(cfg$landcover_file)) {
  landcover <- terra::rast(cfg$landcover_file)
  if (!terra::same.crs(landcover, template)) {
    landcover <- terra::project(landcover, template, method = "near")
  }
  landcover <- terra::resample(landcover, template, method = "near")
}

slope <- NULL
if (!is.null(cfg$slope_file) && nzchar(cfg$slope_file) && file.exists(cfg$slope_file)) {
  slope <- terra::rast(cfg$slope_file)
  if (!terra::same.crs(slope, template)) {
    slope <- terra::project(slope, template, method = "bilinear")
  }
  slope <- terra::resample(slope, template, method = "bilinear")
}

landcover_costs <- load_landcover_costs(cfg$landcover_lookup_file, cfg$friction$landcover)

message_line("[5/9] Building baseline workload friction...")
friction <- template
friction[] <- cfg$friction$base_walk

message_line("  Applying continuous population workload gradient...")

pop_obj <- apply_population_gradient(
  pop_r = template,
  target_template = template,
  aggregate_factor = 10,
  smoothing_radius_m = 1500,
  min_cost = cfg$friction$pop_min_cost,
  max_cost = cfg$friction$pop_max_cost,
  zero_pop_cost = cfg$friction$pop_zero_cost
)


pop_cost <- pop_obj$pop_cost
pop_smooth <- pop_obj$pop_smooth
pop_coarse <- pop_obj$pop_coarse

population_cost_file <- file.path(cfg$output_dir, cfg$output_population_cost_file)
write_raster_safe(pop_cost, population_cost_file)
message_line("  Saved population cost raster: ", normalizePath(population_cost_file))

friction <- friction * pop_cost

if (!is.null(landcover)) {
  message_line("  Applying landcover modifier...")
  friction <- apply_landcover_costs(friction, landcover, landcover_costs)
} else {
  message_line("  No landcover raster provided.")
}

if (!is.null(slope)) {
  message_line("  Applying slope modifier...")
  friction <- apply_slope_adjustment(friction, slope)
} else {
  message_line("  No slope raster provided.")
}

message_line("[6/9] Applying roads as modest friction reducers...")
if (!is.null(roads) && nrow(roads) > 0) {
  road_r <- rasterize_if_present(
    roads,
    friction,
    field = "friction_val",
    fun = "min",
    background = NA
  )
  friction <- terra::ifel(!is.na(road_r), friction * road_r, friction)
} else {
  message_line("  No roads applied.")
}

message_line("[7/9] Applying rivers and bridges...")
if (!is.null(rivers) && nrow(rivers) > 0) {
  rivers_buf <- sf::st_buffer(rivers, cfg$friction$river_buf_m)
  river_r <- rasterize_if_present(
    rivers_buf,
    friction,
    field = "friction_val",
    fun = "max",
    background = NA
  )
  friction <- terra::ifel(!is.na(river_r), friction * river_r, friction)
} else {
  message_line("  No rivers applied.")
}

if (!is.null(bridges) && nrow(bridges) > 0) {
  bridge_r <- rasterize_if_present(
    bridges,
    friction,
    field = "friction_val",
    fun = "min",
    background = NA
  )
  friction <- terra::ifel(!is.na(bridge_r), friction * bridge_r, friction)
} else {
  message_line("  No bridges applied.")
}

message_line("[8/9] Applying water bodies and district boundaries as impassable...")
if (!is.null(water_bodies) && nrow(water_bodies) > 0) {
  wb_r <- rasterize_if_present(water_bodies, friction, field = 1, background = NA)
  friction <- terra::ifel(!is.na(wb_r), cfg$friction$impassable, friction)
} else {
  message_line("  No water bodies applied.")
}

district_lines <- sf::st_boundary(districts_sf) |>
  sf::st_make_valid()

district_buf <- sf::st_buffer(district_lines, cfg$friction$district_boundary_buf_m)
district_r <- rasterize_if_present(district_buf, friction, field = 1, background = NA)
friction <- terra::ifel(!is.na(district_r), cfg$friction$district_boundary_cost, friction)

message_line("[9/9] Finalising and saving...")
country_r <- rasterize_if_present(country_union, friction, field = 1, background = NA)
friction <- terra::mask(friction, country_r)

friction <- terra::ifel(
  is.na(friction) & !is.na(country_r),
  cfg$friction$base_walk,
  friction
)

friction <- terra::clamp(
  friction,
  lower = cfg$friction$min_positive,
  values = TRUE
)

out_file <- file.path(cfg$output_dir, cfg$output_friction_file)
write_raster_safe(friction, out_file)

min_val <- as.numeric(terra::global(friction, "min", na.rm = TRUE)[[1]])
max_val <- as.numeric(terra::global(friction, "max", na.rm = TRUE)[[1]])
na_count <- as.numeric(terra::global(is.na(friction), "sum", na.rm = TRUE)[[1]])

message_line("Saved friction raster: ", normalizePath(out_file))
message_line("CRS: ", terra::crs(friction))
message_line("Resolution: ", paste(terra::res(friction), collapse = " x "))
message_line("Friction range: ", round(min_val, 4), " to ", round(max_val, 2))
message_line("NA cells: ", format(na_count, big.mark = ","))
message_line("Done.")

