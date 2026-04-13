# =============================================================================
# build_national_friction_surface.R
#
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
  districts_file = "data/districts_shp.Rds",
  worldpop_file = "data/som_u5_population_2025_100m.tif",
  roads_file = "data/osm_inputs/somalia_roads.gpkg",
  rivers_file = "data/osm_inputs/somalia_rivers.gpkg",
  bridges_file = "data/osm_inputs/somalia_bridges.gpkg",
  water_bodies_file = "data/osm_inputs/somalia_water_bodies.gpkg",
  
  output_dir = "data/friction",
  output_friction_file = "somalia_friction_100m.tif",
  output_template_file = "somalia_template_100m.tif",
  output_population_cost_file = "somalia_population_cost_100m.tif",
  
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
  )
)

# =============================================================================
# HELPERS
# =============================================================================

message_line <- function(...) {
  message(paste0(...))
}

assert_file_exists <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " not found: ", path, call. = FALSE)
  }
}

read_vector_safe <- function(path, target_crs) {
  if (!file.exists(path)) return(NULL)
  
  sf::st_read(path, quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(target_crs)
}

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
  
  ext_proj <- terra::ext(wp_proj)
  
  template <- terra::rast(
    xmin = ext_proj[1],
    xmax = ext_proj[2],
    ymin = ext_proj[3],
    ymax = ext_proj[4],
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
    fun = sum,
    na.rm = TRUE
  )
  
  w <- terra::focalMat(
    pop_coarse,
    d = rules$population$smoothing_radius_m,
    type = "circle"
  )
  
  pop_smooth <- terra::focal(
    pop_coarse,
    w = w,
    fun = sum,
    na.rm = TRUE,
    fillvalue = 0
  )
  
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
      pop_cost
    )
  }
  
  terra::resample(
    pop_cost,
    template
  )
}

# =============================================================================
# BUILD
# =============================================================================

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
    field = "friction_val",
    fun = "min",
    background = NA
  )
  
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
    field = "friction_val",
    fun = "max",
    background = NA
  )
  
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
    field = "friction_val",
    fun = "min",
    background = NA
  )
  
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
message_line("Done.")

