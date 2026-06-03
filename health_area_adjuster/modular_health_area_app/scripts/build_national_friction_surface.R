# =============================================================================
# build_national_friction_surface.R
#
# Rewritten to minimise terra temp disk usage:
#   - Each step reads from the previously saved .tif rather than chaining
#     in-memory SpatRasters, so terra never needs to hold more than one
#     full-country raster in its temp directory at a time.
#   - Intermediate objects are rm()'d and gc()'d immediately after saving.
#   - Terra temp is redirected to data/terra_temp which is wiped at the
#     start of each step.
#
# PIPELINE ORDER:
#   01  population baseline
#   01b land surface (land cover + slope)
#   02  roads
#   03  rivers
#   04  bridges
#   05  water bodies
#   06  district boundaries
#   final mask to country
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(elevatr)
  library(geodata)
})

# Redirect terra temp to a controlled location and clean it before starting
terra_tmp <- "data/terra_temp"
dir.create(terra_tmp, recursive = TRUE, showWarnings = FALSE)
terra::terraOptions(tempdir = terra_tmp, memfrac = 0.6)

source("scripts/get_boundaries.R")

# =============================================================================
# SETTINGS
# =============================================================================

cfg <- list(
  worldpop_file          = "data/som_u5_population_2025_100m.tif",
  roads_file             = "data/osm_inputs/somalia_roads.gpkg",
  rivers_file            = "data/osm_inputs/somalia_rivers.gpkg",
  bridges_file           = "data/osm_inputs/somalia_bridges.gpkg",
  water_bodies_file      = "data/osm_inputs/somalia_water_bodies.gpkg",
  output_dir             = "data/friction",
  land_surface_cache_dir = "data/land_surface_cache",
  target_crs             = "EPSG:3857",
  target_resolution_m    = 100
)

rules <- list(
  population = list(
    aggregate_factor   = 5,
    smoothing_radius_m = 500,
    min_cost           = 0.35,
    max_cost           = 0.85,
    zero_pop_cost      = 0.35,
    zero_pop_surcharge    = 0.25
  ),
  land_surface = list(
    lulc_penalties = list(
      cropland  = 0.00,
      bare      = 0.00,
      grassland = 0.05,
      shrubs    = 0.05,
      trees     = 0.10,
      wetland   = 0.10,
      other     = 0.05
    ),
    slope_flat_max_deg   = 10,
    slope_penalty        = 0.08,
    max_combined_penalty = 0.15
  ),
  roads = list(
    primary      = 0.35,
    secondary    = 0.50,
    min_buffer_m = 15,
    max_buffer_m = 80
  ),
  rivers          = list(major = 0.99, buffer_m = 400),
  bridges         = list(primary = 0.35, secondary = 0.50),
  water           = list(major_cost = 1.00),
  district_boundary = list(cost = 1.00, buffer_m = 100)
)

# =============================================================================
# HELPERS
# =============================================================================

step_file <- function(name) {
  file.path(cfg$output_dir, paste0(name, ".tif"))
}

wipe_terra_tmp <- function() {
  f <- list.files(terra_tmp, full.names = TRUE)
  invisible(file.remove(f[file.exists(f)]))
}

write_step <- function(r, name) {
  out <- step_file(name)
  terra::writeRaster(r, out, overwrite = TRUE,
                     gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES"))
  
  vals     <- terra::values(r, mat = FALSE)
  rng      <- range(vals, na.rm = TRUE)
  ones_pct <- round(mean(vals == 1, na.rm = TRUE) * 100, 2)
  message("\n[", name, "] range: ", round(rng[1], 4), " - ", round(rng[2], 4),
          "  |  impassable: ", ones_pct, "%")
  print(round(quantile(vals, c(0,.05,.25,.5,.75,.95,1), na.rm = TRUE), 4))
  
  rm(r); gc()
  wipe_terra_tmp()
  invisible(out)
}

read_step <- function(name) terra::rast(step_file(name))

read_vector <- function(path) {
  if (!file.exists(path)) return(NULL)
  sf::st_read(path, quiet = TRUE) |> sf::st_make_valid() |>
    sf::st_transform(cfg$target_crs)
}

# =============================================================================
# SETUP
# =============================================================================
dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
wipe_terra_tmp()

districts <- districts |>
  sf::st_make_valid() |>
  sf::st_transform(cfg$target_crs)

country_union        <- dplyr::summarise(districts, geometry = sf::st_union(geometry))
country_union_latlon <- sf::st_transform(country_union, "EPSG:4326")
country_vect         <- terra::vect(country_union)

# Build template from WorldPop
wp       <- terra::rast(cfg$worldpop_file)
wp_proj  <- terra::project(wp, cfg$target_crs); rm(wp); gc()
template <- terra::rast(ext        = terra::ext(wp_proj),
                        resolution = cfg$target_resolution_m,
                        crs        = cfg$target_crs)
template <- terra::resample(wp_proj, template)
names(template) <- "u5_pop"
rm(wp_proj); gc()

# Fill interior NA cells — WorldPop uses NA for zero-population cells,
# which would create holes throughout the friction surface.
# Replace any NA inside the country boundary with 0.
country_fill <- terra::rasterize(country_vect, template,
                                 field = 0, background = NA)
template     <- terra::cover(template, country_fill)
rm(country_fill); gc()

terra::writeRaster(template,
                   file.path(cfg$output_dir, "somalia_template_100m.tif"),
                   overwrite = TRUE)

# =============================================================================
# 01 POPULATION BASELINE
# =============================================================================
message("--- 01 Population baseline ---")

p <- rules$population

pop_coarse <- terra::aggregate(template, fact = p$aggregate_factor,
                               fun = sum, na.rm = TRUE)

w          <- terra::focalMat(pop_coarse, d = p$smoothing_radius_m, type = "circle")
pop_smooth <- terra::focal(pop_coarse, w = w, fun = sum,
                           na.rm = TRUE, fillvalue = 0)
rm(pop_coarse, w); gc()

pop_log  <- log1p(pop_smooth)
gmin     <- as.numeric(terra::global(pop_log, "min", na.rm = TRUE)[[1]])
gmax     <- as.numeric(terra::global(pop_log, "max", na.rm = TRUE)[[1]])
pop_norm <- (pop_log - gmin) / (gmax - gmin)
rm(pop_log); gc()

pop_cost <- p$min_cost + pop_norm * (p$max_cost - p$min_cost)
pop_cost <- terra::ifel(pop_smooth <= 0.01, p$zero_pop_cost, pop_cost)
pop_cost <- terra::ifel(
  pop_smooth <= 0.01,
  pop_cost + p$zero_pop_surcharge,
  pop_cost
)
pop_cost <- terra::clamp(pop_cost, 0, 1)
rm(pop_norm, pop_smooth); gc()

write_step(pop_cost, "01_population_cost")

# =============================================================================
# 01b LAND SURFACE (land cover + slope)
# =============================================================================
message("--- 01b Land surface ---")

ls  <- rules$land_surface
pen <- ls$lulc_penalties

message("Loading land cover...")
lc_layers <- c("trees","shrubs","grassland","cropland","bare","wetland")

lc_list <- lapply(lc_layers, function(var) {
  r <- geodata::landcover(var = var, path = cfg$land_surface_cache_dir)
  terra::project(r, template, method = "bilinear")
})
lc_stack        <- terra::rast(lc_list); rm(lc_list); gc()
names(lc_stack) <- lc_layers

dominant_idx <- terra::which.max(lc_stack); rm(lc_stack); gc()

penalty_vals <- c(pen$trees, pen$shrubs, pen$grassland,
                  pen$cropland, pen$bare, pen$wetland)
lulc_penalty <- terra::classify(dominant_idx,
                                rcl = cbind(seq_along(lc_layers), penalty_vals))
lulc_penalty <- terra::ifel(is.na(lulc_penalty), pen$other, lulc_penalty)
rm(dominant_idx); gc()

message("Loading elevation / slope...")
elev_raw     <- terra::rast(elevatr::get_elev_raster(
  locations = country_union_latlon, z = 7, clip = "locations"))
elev_proj    <- terra::project(elev_raw, template, method = "bilinear")
rm(elev_raw); gc()
slope_deg    <- terra::terrain(elev_proj, v = "slope", unit = "degrees")
rm(elev_proj); gc()
slope_penalty <- terra::ifel(slope_deg > ls$slope_flat_max_deg,
                             ls$slope_penalty, 0)
rm(slope_deg); gc()

land_penalty <- terra::clamp(lulc_penalty + slope_penalty,
                             lower = 0, upper = ls$max_combined_penalty)
rm(lulc_penalty, slope_penalty); gc()

pop_cost     <- read_step("01_population_cost")
land_penalty <- terra::resample(land_penalty, pop_cost, method = "bilinear")
friction     <- terra::clamp(pop_cost + land_penalty, 0, 1)
rm(pop_cost, land_penalty); gc()

write_step(friction, "01b_after_land_surface")

# =============================================================================
# 02 ROADS
# =============================================================================
message("--- 02 Roads ---")

roads <- read_vector(cfg$roads_file)

if (!is.null(roads) && nrow(roads) > 0) {
  
  friction <- read_step("01b_after_land_surface")
  pop_cost <- read_step("01_population_cost")
  
  roads$road_class <- dplyr::case_when(
    roads$highway %in% c("motorway","trunk","primary") ~ "primary",
    TRUE                                               ~ "secondary"
  )
  roads$friction_val <- dplyr::if_else(
    roads$road_class == "primary",
    rules$roads$primary, rules$roads$secondary
  )
  
  road_mid        <- sf::st_point_on_surface(roads)
  roads$pop_local <- terra::extract(pop_cost, terra::vect(road_mid))[, 2]
  rm(pop_cost, road_mid); gc()
  
  roads$buffer_m <- dplyr::case_when(
    is.na(roads$pop_local)  ~ 100,
    roads$pop_local <= 0.40 ~ 200,
    roads$pop_local <= 0.50 ~ 120,
    TRUE                    ~ 60
  )
  
  roads_buf <- sf::st_make_valid(
    do.call(rbind, lapply(split(roads, roads$buffer_m), function(x)
      sf::st_buffer(x, dist = unique(x$buffer_m)[1])))
  )
  
  # --- binary rasterization (hard road cost within buffer) ---
  road_r   <- terra::rasterize(terra::vect(roads_buf), friction,
                               field = "friction_val", fun = "min",
                               background = NA)
  rm(roads_buf); gc()
  
  friction <- terra::ifel(!is.na(road_r), friction * road_r, friction)
  rm(road_r); gc()
  
  # --- gradient falloff: taper cost reduction beyond buffer edge ---
  roads_vect <- terra::vect(roads)
  road_dist  <- terra::distance(friction, roads_vect)
  rm(roads_vect); gc()
  
  road_bonus <- terra::ifel(
    road_dist < 500,
    (1 - rules$roads$primary) * exp(-road_dist / 200),
    0
  )
  rm(road_dist); gc()
  
  friction <- terra::clamp(friction - road_bonus, 0.05, 1)
  rm(road_bonus); gc()
  
  write_step(friction, "02_after_roads")
  
} else {
  message("No roads found — copying 01b as 02")
  file.copy(step_file("01b_after_land_surface"), step_file("02_after_roads"),
            overwrite = TRUE)
}

# =============================================================================
# 03 RIVERS
# =============================================================================
message("--- 03 Rivers ---")

rivers <- read_vector(cfg$rivers_file)

if (!is.null(rivers) && nrow(rivers) > 0) {
  
  friction          <- read_step("02_after_roads")
  rivers$friction_val <- rules$rivers$major
  rivers_buf        <- sf::st_buffer(rivers, rules$rivers$buffer_m)
  rm(rivers); gc()
  
  river_r  <- terra::rasterize(terra::vect(rivers_buf), friction,
                               field = "friction_val", fun = "max",
                               background = NA)
  rm(rivers_buf); gc()
  
  friction <- terra::ifel(!is.na(river_r) & river_r > friction, river_r, friction)
  rm(river_r); gc()
  
  write_step(friction, "03_after_rivers")
  
} else {
  message("No rivers — copying 02 as 03")
  file.copy(step_file("02_after_roads"), step_file("03_after_rivers"),
            overwrite = TRUE)
}

# =============================================================================
# 04 BRIDGES
# =============================================================================
message("--- 04 Bridges ---")

bridges <- read_vector(cfg$bridges_file)

if (!is.null(bridges) && nrow(bridges) > 0) {
  
  friction <- read_step("03_after_rivers")
  
  bridges$friction_val <- dplyr::if_else(
    bridges$highway %in% c("motorway","trunk","primary"),
    rules$bridges$primary, rules$bridges$secondary
  )
  
  bridge_r <- terra::rasterize(terra::vect(bridges), friction,
                               field = "friction_val", fun = "min",
                               background = NA)
  rm(bridges); gc()
  
  friction <- terra::ifel(!is.na(bridge_r) & bridge_r < friction,
                          bridge_r, friction)
  rm(bridge_r); gc()
  
  write_step(friction, "04_after_bridges")
  
} else {
  message("No bridges — copying 03 as 04")
  file.copy(step_file("03_after_rivers"), step_file("04_after_bridges"),
            overwrite = TRUE)
}

# =============================================================================
# 05 WATER BODIES
# =============================================================================
message("--- 05 Water bodies ---")

water <- read_vector(cfg$water_bodies_file)

if (!is.null(water) && nrow(water) > 0) {
  
  friction          <- read_step("04_after_bridges")
  water$friction_val <- rules$water$major_cost
  
  water_r  <- terra::rasterize(terra::vect(water), friction,
                               field = "friction_val", fun = "max",
                               background = NA)
  rm(water); gc()
  
  friction <- terra::ifel(!is.na(water_r), water_r, friction)
  rm(water_r); gc()
  
  write_step(friction, "05_after_water")
  
} else {
  message("No water bodies — copying 04 as 05")
  file.copy(step_file("04_after_bridges"), step_file("05_after_water"),
            overwrite = TRUE)
}

# =============================================================================
# 06 DISTRICT BOUNDARIES
# =============================================================================
message("--- 06 District boundaries ---")

friction     <- read_step("05_after_water")
dist_lines   <- sf::st_boundary(districts)
dist_buf     <- sf::st_buffer(dist_lines, rules$district_boundary$buffer_m)
rm(dist_lines); gc()

district_r   <- terra::rasterize(terra::vect(dist_buf), friction,
                                 field = 1, background = NA)
rm(dist_buf); gc()

friction     <- terra::ifel(!is.na(district_r),
                            rules$district_boundary$cost, friction)
rm(district_r); gc()

write_step(friction, "06_after_boundary")

# =============================================================================
# FINAL: MASK TO COUNTRY
# =============================================================================
message("--- Final mask ---")

friction  <- read_step("06_after_boundary")
country_r <- terra::rasterize(country_vect, friction, field = 1, background = NA)
friction  <- terra::mask(friction, country_r)
rm(country_r); gc()

friction <- terra::clamp(friction, lower = 0.05, upper = 1)

out_file  <- file.path(cfg$output_dir, "somalia_friction_100m.tif")
terra::writeRaster(friction, out_file, overwrite = TRUE,
                   gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES"))

vals     <- terra::values(friction, mat = FALSE)
message("\n[FINAL] range: ", round(min(vals, na.rm=TRUE), 4),
        " - ", round(max(vals, na.rm=TRUE), 4))
print(round(quantile(vals, c(0,.05,.25,.5,.75,.95,1), na.rm=TRUE), 4))

rm(friction); gc()
wipe_terra_tmp()

message("\nDone. Saved to: ", normalizePath(out_file))