# =============================================================================
# build_national_friction_surface.R
#
<<<<<<< HEAD
# INTERNAL MODEL RULES:
#   - All component values are defined directly on a 0–1 scale
#   - Lower values = easier movement
#   - Higher values = harder movement
#   - Impassable = 1
#
# FINAL OUTPUT GUARANTEES:
#   - Values range 0–1
#   - Impassable = 1
#   - No silent rescaling
#   - Validation enforced at every step
#   - Intermediate rasters saved
=======
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
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
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
<<<<<<< HEAD
  districts_file = "data/districts_shp.Rds",
  worldpop_file = "data/som_u5_population_2025_100m.tif",
=======
  # Required inputs
  districts_file = "data/districts_shp.Rds",
  worldpop_t_u1_1to4_file = "data/som_u5_population_2025_100m.tif",
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
  roads_file = "data/osm_inputs/somalia_roads.gpkg",
  rivers_file = "data/osm_inputs/somalia_rivers.gpkg",
  bridges_file = "data/osm_inputs/somalia_bridges.gpkg",
  water_bodies_file = "data/osm_inputs/somalia_water_bodies.gpkg",
  
<<<<<<< HEAD
=======
  # Optional inputs
  landcover_file = NULL,
  slope_file = NULL,
  landcover_lookup_file = NULL,
  
  # Output
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
  output_dir = "data/friction",
  output_friction_file = "somalia_friction_100m.tif",
  output_template_file = "somalia_template_100m.tif",
  output_population_cost_file = "somalia_population_cost_100m.tif",
  
<<<<<<< HEAD
  target_crs = "EPSG:3857",
  target_resolution_m = 100
)

# =============================================================================
# COMPONENT RULES
# =============================================================================

rules <- list(
  
  base_walk = 1,
  impassable = 1,
  
  population = list(
    aggregate_factor = 5,
    smoothing_radius_m = 500,
    min_cost = 0.35,
    max_cost = 0.85,
    zero_pop_cost = 0.35
  ),
  
  roads = list(
    primary = 0.65,
    secondary = 0.75,
    minor = 0.85,
    track = 0.95,
    
    min_buffer_m = 15,
    max_buffer_m = 80
  ),
  
  rivers = list(
    major = 0.99,
    minor = 0.85,
    buffer_m = 400
  ),
  
  bridges = list(
    primary = 0.18,
    secondary = 0.22,
    minor = 0.28,
    track = 0.35
  ),
  
  water = list(
    major_cost = 1.00,
    minor_cost = 0.80
  ),
  
  district_boundary = list(
    cost = 1.00,
    buffer_m = 100
=======
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
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
  )
)

# =============================================================================
# HELPERS
# =============================================================================

message_line <- function(...) {
  message(paste0(...))
}

assert_file_exists <- function(path, label) {
<<<<<<< HEAD
  if (!file.exists(path)) {
=======
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
    stop(label, " not found: ", path, call. = FALSE)
  }
}

<<<<<<< HEAD
read_vector_safe <- function(path, target_crs) {
  if (!file.exists(path)) return(NULL)
  
  sf::st_read(path, quiet = TRUE) |>
=======
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
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
    sf::st_make_valid() |>
    sf::st_transform(target_crs)
}

<<<<<<< HEAD
write_raster_safe <- function(x, filename) {
  terra::writeRaster(
    x,
    filename,
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=DEFLATE",
      "TILED=YES",
      "BIGTIFF=YES"
    )
  )
}

validate_raster <- function(r, step_name) {
  
  vals <- terra::values(r, mat = FALSE)
  
  if (all(is.na(vals)))
    stop(step_name, ": all values are NA")
  
  rng <- range(vals, na.rm = TRUE)
  
  if (rng[1] < 0)
    stop(step_name, ": values below 0 detected")
  
  if (rng[2] > 1)
    stop(step_name, ": values above 1 detected")
  
  na_n <- sum(is.na(vals))
  
  ones_pct <- mean(vals == 1, na.rm = TRUE) * 100
  
  message_line("")
  message_line("[VALIDATION] ", step_name)
  message_line("Range: ",
               round(rng[1], 4),
               " to ",
               round(rng[2], 4))
  message_line("NA cells: ", na_n)
  message_line("% impassable: ",
               round(ones_pct, 2),
               "%")
  
  q <- stats::quantile(
    vals,
    probs = c(0, .01, .05, .25, .5, .75, .95, .99, 1),
    na.rm = TRUE
  )
  
  print(round(q, 4))
}

save_step <- function(r, name) {
  
  file <- file.path(
    cfg$output_dir,
    paste0(name, ".tif")
  )
  
  write_raster_safe(r, file)
  
  validate_raster(r, name)
  
}

build_template <- function(worldpop_file,
                           target_crs,
                           res_m) {
  
  wp <- terra::rast(worldpop_file)
  
  wp_proj <- terra::project(
    wp,
    target_crs
  )
=======
build_template_from_worldpop <- function(worldpop_file, target_crs, target_resolution_m) {
  assert_file_exists(worldpop_file, "worldpop_t_u1_1to4_file")
  
  wp <- terra::rast(worldpop_file)
  wp_proj <- terra::project(wp, target_crs, method = "bilinear")
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
  
  ext_proj <- terra::ext(wp_proj)
  
  template <- terra::rast(
    xmin = ext_proj[1],
    xmax = ext_proj[2],
    ymin = ext_proj[3],
    ymax = ext_proj[4],
<<<<<<< HEAD
    resolution = res_m,
    crs = target_crs
  )
  
  template <- terra::resample(
    wp_proj,
    template
  )
  
  names(template) <- "u5_pop"
  
  template
}

apply_population_gradient <- function(pop_r,
                                      template) {
  
  pop_coarse <- terra::aggregate(
    pop_r,
    fact = rules$population$aggregate_factor,
=======
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
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
    fun = sum,
    na.rm = TRUE
  )
  
<<<<<<< HEAD
  w <- terra::focalMat(
    pop_coarse,
    d = rules$population$smoothing_radius_m,
    type = "circle"
  )
  
=======
  message("  Building smoothing kernel...")
  w <- terra::focalMat(
    pop_coarse,
    d = smoothing_radius_m,
    type = "circle"
  )
  
  message("  Applying focal smoothing on coarse raster...")
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
  pop_smooth <- terra::focal(
    pop_coarse,
    w = w,
    fun = sum,
    na.rm = TRUE,
    fillvalue = 0
  )
  
<<<<<<< HEAD
  pop_log <- log1p(pop_smooth)
  
  gmin <- as.numeric(
    terra::global(
      pop_log,
      "min",
      na.rm = TRUE
    )[[1]]
  )
  
  gmax <- as.numeric(
    terra::global(
      pop_log,
      "max",
      na.rm = TRUE
    )[[1]]
  )
  
  if (is.na(gmin) ||
      is.na(gmax) ||
      gmax <= gmin) {
    
    pop_cost <- pop_smooth
    pop_cost[] <- rules$population$min_cost
    
  } else {
    
    pop_norm <-
      (pop_log - gmin) /
      (gmax - gmin)
    
    pop_cost <-
      rules$population$min_cost +
      pop_norm *
      (rules$population$max_cost -
         rules$population$min_cost)
    
    pop_cost <- terra::ifel(
      pop_smooth <= 0.01,
      rules$population$zero_pop_cost,
=======
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
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
      pop_cost
    )
  }
  
<<<<<<< HEAD
  terra::resample(
    pop_cost,
    template
=======
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
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
  )
}

# =============================================================================
# BUILD
# =============================================================================

<<<<<<< HEAD
message_line("Building national friction surface")

assert_file_exists(
  cfg$districts_file,
  "districts_file"
)

assert_file_exists(
  cfg$worldpop_file,
  "worldpop_file"
)

dir.create(
  cfg$output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

districts <- readRDS(
  cfg$districts_file
) |>
  sf::st_make_valid() |>
  sf::st_transform(
    cfg$target_crs
  )

country_union <- districts |>
  dplyr::summarise(
    geometry =
      sf::st_union(geometry)
  )

template <- build_template(
  cfg$worldpop_file,
  cfg$target_crs,
  cfg$target_resolution_m
)

write_raster_safe(
  template,
  file.path(
    cfg$output_dir,
    cfg$output_template_file
  )
)

roads <- read_vector_safe(
  cfg$roads_file,
  cfg$target_crs
)

rivers <- read_vector_safe(
  cfg$rivers_file,
  cfg$target_crs
)

bridges <- read_vector_safe(
  cfg$bridges_file,
  cfg$target_crs
)

water <- read_vector_safe(
  cfg$water_bodies_file,
  cfg$target_crs
)

# -----------------------------------------------------------------------------
# Population baseline
# -----------------------------------------------------------------------------

pop_cost <- apply_population_gradient(
  template,
  template
)

save_step(
  pop_cost,
  "01_population_cost"
)

friction_pop <- pop_cost

# -----------------------------------------------------------------------------
# Roads (class-based variable buffer, fast version with progress bar)
# -----------------------------------------------------------------------------

friction_roads <- friction_pop

if (!is.null(roads) && nrow(roads) > 0) {
  
  pb <- utils::txtProgressBar(
    min = 0,
    max = 5,
    style = 3
  )
  
  step <- 0
  
  # ---------------------------------------------------------------------------
  # Step 1 — classify road types
  # ---------------------------------------------------------------------------
  
  roads$road_class <- dplyr::case_when(
    roads$highway %in% c("motorway", "trunk", "primary") ~ "primary",
    roads$highway %in% c("secondary", "tertiary") ~ "secondary",
    roads$highway %in% c("track", "path", "service") ~ "track",
    TRUE ~ "minor"
  )
  
  roads$friction_val <- dplyr::case_when(
    roads$road_class == "primary" ~ rules$roads$primary,
    roads$road_class == "secondary" ~ rules$roads$secondary,
    roads$road_class == "minor" ~ rules$roads$minor,
    roads$road_class == "track" ~ rules$roads$track,
    TRUE ~ rules$roads$minor
  )
  
  step <- step + 1
  utils::setTxtProgressBar(pb, step)
  
  # ---------------------------------------------------------------------------
  # Step 2 — sample population at road locations
  # ---------------------------------------------------------------------------
  
  road_mid_sf <- sf::st_point_on_surface(roads)
  
  roads$pop_cost_local <- terra::extract(
    pop_cost,
    terra::vect(road_mid_sf)
  )[, 2]
  
  step <- step + 1
  utils::setTxtProgressBar(pb, step)
  
  # ---------------------------------------------------------------------------
  # Step 3 — assign buffer width by density
  # ---------------------------------------------------------------------------
  
  roads$buffer_m <- dplyr::case_when(
    is.na(roads$pop_cost_local) ~ 35,
    roads$pop_cost_local <= 0.40 ~ 60,
    roads$pop_cost_local <= 0.50 ~ 35,
    TRUE ~ 15
  )
  
  step <- step + 1
  utils::setTxtProgressBar(pb, step)
  
  # ---------------------------------------------------------------------------
  # Step 4 — buffer by class groups
  # ---------------------------------------------------------------------------
  
  roads_split <- split(roads, roads$buffer_m)
  
  roads_buf_list <- lapply(
    roads_split,
    function(x) {
      sf::st_buffer(
        x,
        dist = unique(x$buffer_m)[1]
      )
    }
  )
  
  roads_buf <- do.call(rbind, roads_buf_list)
  roads_buf <- sf::st_make_valid(roads_buf)
  
  step <- step + 1
  utils::setTxtProgressBar(pb, step)
  
  # ---------------------------------------------------------------------------
  # Step 5 — rasterize and apply friction
  # ---------------------------------------------------------------------------
  
  road_r <- terra::rasterize(
    terra::vect(roads_buf),
    friction_roads,
=======
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
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
    field = "friction_val",
    fun = "min",
    background = NA
  )
<<<<<<< HEAD
  
  friction_roads <- terra::ifel(
    !is.na(road_r),
    friction_roads * road_r,
    friction_roads
  )
  
  step <- step + 1
  utils::setTxtProgressBar(pb, step)
  
  close(pb)
}

save_step(
  friction_roads,
  "02_after_roads"
)

# -----------------------------------------------------------------------------
# Rivers
# -----------------------------------------------------------------------------

friction_rivers <- friction_roads

if (!is.null(rivers) &&
    nrow(rivers) > 0) {
  
  rivers$river_type <- dplyr::case_when(
    rivers$waterway == "river" ~ "major",
    TRUE ~ "minor"
  )
  
  rivers$friction_val <- dplyr::case_when(
    rivers$river_type == "major" ~ rules$rivers$major,
    rivers$river_type == "minor" ~ rules$rivers$minor,
    TRUE ~ rules$rivers$minor
  )
  
  rivers_buf <- sf::st_buffer(
    rivers,
    rules$rivers$buffer_m
  )
  
  river_r <- terra::rasterize(
    terra::vect(rivers_buf),
    friction_rivers,
=======
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
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
    field = "friction_val",
    fun = "max",
    background = NA
  )
<<<<<<< HEAD
  
  friction_rivers <- terra::ifel(
    !is.na(river_r) &
      river_r > friction_rivers,
    river_r,
    friction_rivers
  )
}

save_step(
  friction_rivers,
  "03_after_rivers"
)

# -----------------------------------------------------------------------------
# Bridges
# -----------------------------------------------------------------------------

friction_bridges <- friction_rivers

if (!is.null(bridges) &&
    nrow(bridges) > 0) {
  
  bridges$bridge_class <- dplyr::case_when(
    bridges$highway %in%
      c("motorway",
        "trunk",
        "primary") ~ "primary",
    
    bridges$highway %in%
      c("secondary",
        "tertiary") ~ "secondary",
    
    bridges$highway %in%
      c("track",
        "path",
        "service") ~ "track",
    
    TRUE ~ "minor"
  )
  
  bridges$friction_val <- dplyr::case_when(
    bridges$bridge_class == "primary" ~ rules$bridges$primary,
    bridges$bridge_class == "secondary" ~ rules$bridges$secondary,
    bridges$bridge_class == "minor" ~ rules$bridges$minor,
    bridges$bridge_class == "track" ~ rules$bridges$track,
    TRUE ~ rules$bridges$minor
  )
  
  bridge_r <- terra::rasterize(
    terra::vect(bridges),
    friction_bridges,
=======
  friction <- terra::ifel(!is.na(river_r), friction * river_r, friction)
} else {
  message_line("  No rivers applied.")
}

if (!is.null(bridges) && nrow(bridges) > 0) {
  bridge_r <- rasterize_if_present(
    bridges,
    friction,
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
    field = "friction_val",
    fun = "min",
    background = NA
  )
<<<<<<< HEAD
  
  friction_bridges <- terra::ifel(
    !is.na(bridge_r) &
      bridge_r < friction_bridges,
    bridge_r,
    friction_bridges
  )
}

save_step(
  friction_bridges,
  "04_after_bridges"
)

# -----------------------------------------------------------------------------
# Water bodies
# -----------------------------------------------------------------------------

friction_water <- friction_bridges

if (!is.null(water) &&
    nrow(water) > 0) {
  
  if (!"water_class" %in% names(water))
    stop("water_class field missing")
  
  water$friction_val <- dplyr::case_when(
    water$water_class == "major" ~ rules$water$major_cost,
    water$water_class == "minor" ~ rules$water$minor_cost,
    TRUE ~ rules$water$minor_cost
  )
  
  water_r <- terra::rasterize(
    terra::vect(water),
    friction_water,
    field = "friction_val",
    fun = "max",
    background = NA
  )
  
  friction_water <- terra::ifel(
    !is.na(water_r),
    water_r,
    friction_water
  )
}

save_step(
  friction_water,
  "05_after_water"
)

# -----------------------------------------------------------------------------
# District boundary
# -----------------------------------------------------------------------------

district_lines <- sf::st_boundary(
  districts
)

district_buf <- sf::st_buffer(
  district_lines,
  rules$district_boundary$buffer_m
)

district_r <- terra::rasterize(
  terra::vect(district_buf),
  friction_water,
  field = 1,
  background = NA
)

friction_boundary <- terra::ifel(
  !is.na(district_r),
  rules$district_boundary$cost,
  friction_water
)

save_step(
  friction_boundary,
  "06_after_boundary"
)

# -----------------------------------------------------------------------------
# Mask to country
# -----------------------------------------------------------------------------

country_r <- terra::rasterize(
  terra::vect(country_union),
  friction_boundary,
  field = 1,
  background = NA
)

friction_final <- terra::mask(
  friction_boundary,
  country_r
)

validate_raster(
  friction_final,
  "final_friction"
)

# -----------------------------------------------------------------------------
# Save final
# -----------------------------------------------------------------------------

out_file <- file.path(
  cfg$output_dir,
  cfg$output_friction_file
)

write_raster_safe(
  friction_final,
  out_file
)

message_line("")
message_line("Saved friction raster:")
message_line(normalizePath(out_file))
=======
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
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
message_line("Done.")

