# =============================================================================
# build_national_friction_surface.R
#
# Builds the national 100m friction surface (0-1) that health-area and
# team-area generation spread across. Four ingredients:
#
#   01  population  - settled land costs 0.35-0.85 (rising with density);
#                     empty land costs more than any settled land (0.90),
#                     so areas follow where people live
#   02  roads       - cheaper corridors (road +/- 100m) that areas stretch along
#   03  rivers      - the Jubba and Shabelle channels (0.99), open at bridges
#   04  water       - lakes and other water bodies are impassable (1.0)
#   final mask to country
#
# What is deliberately NOT here (and why):
#   - seasonal rivers / togga: usually dry, unevenly mapped in OSM
#   - land cover and slope: only available at ~1 km, mostly a uniform offset
#   - a district-boundary band: each district's surface is cut to its own
#     outline, and the app's grid never extends past it
#
# Keeping boundaries ON primary roads and on the Jubba/Shabelle is done in
# the app (a crossing penalty on those lines), not here.
#
# Disk usage: each step reads the previous step's saved .tif rather than
# chaining in-memory rasters, and terra's temp folder (data/terra_temp) is
# wiped after every step.
#
# Output goes to data/friction/<BOUNDARY_SET>/. The boundary set is only
# used for the country outline (final mask).
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
})

# Redirect terra temp to a controlled location and clean it before starting
terra_tmp <- "data/terra_temp"
dir.create(terra_tmp, recursive = TRUE, showWarnings = FALSE)
terra::terraOptions(tempdir = terra_tmp, memfrac = 0.6)

# District boundary set to build for (see setup_boundary_sets.R)
BOUNDARY_SET  <- "2026b"
districts_file <- file.path("data/boundary_sets", BOUNDARY_SET, "districts_shp.Rds")
if (!file.exists(districts_file)) stop("Boundary set not found: ", districts_file)
districts <- readRDS(districts_file)
message("Boundary set ", BOUNDARY_SET, ": ", nrow(districts), " districts")

# =============================================================================
# SETTINGS
# =============================================================================

cfg <- list(
  worldpop_file       = "data/som_u5_population_2026_100m.tif",
  roads_file          = "data/osm_inputs/somalia_roads.gpkg",
  rivers_file         = "data/osm_inputs/somalia_rivers.gpkg",
  bridges_file        = "data/osm_inputs/somalia_bridges.gpkg",
  water_bodies_file   = "data/osm_inputs/somalia_water_bodies.gpkg",
  output_dir          = file.path("data/friction", BOUNDARY_SET),
  target_crs          = "EPSG:3857",
  target_resolution_m = 100
)

rules <- list(
  population = list(
    density_unit_km2   = 0.25,  # children per 0.25 km2 (= the old 500m cell)
    smoothing_radius_m = 250,   # ~0.2 km2 circle: fills streets and gaps between
    # compounds within a settlement, while gaps of
    # more than ~500m between settlements stay empty
    min_cost           = 0.35,  # sparsest settled land
    max_cost           = 0.85,  # densest settled land
    empty_threshold    = 0.01,  # smoothed density at or below this = empty
    empty_cost         = 0.90   # above any settled land, so fronts stay in
    # populated areas; not higher, because the app
    # treats friction above 0.90 as near-impassable
  ),
  # buffer_m = how far either side of the line the feature's effect reaches
  # (0 = only the 100m cells the line passes through).
  roads = list(
    primary   = 0.35,   # friction multiplier inside the corridor
    secondary = 0.50,
    buffer_m  = 100     # ~300m corridor: roughly how far teams walk off a road
  ),
  rivers = list(
    # Only the permanent Jubba and Shabelle, matched by OSM name. The app
    # keeps boundaries on them with a crossing penalty, so here they only
    # cover the channel itself. Every other river in the file is left out:
    # they're mostly seasonal togga, often dry (and then used as routes
    # rather than obstacles), and too unevenly mapped in OSM to treat as
    # barriers.
    primary_pattern = "jubba|shabeel|shebel",
    channel_cost    = 0.99,
    bridge_gap_m    = 100   # the channel is left open within this distance of
    # a bridge (the road corridor gives the discount)
  ),
  water = list(cost = 1.00)
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

copy_step <- function(from, to) {
  message("Nothing to apply -- copying ", from, " as ", to)
  file.copy(step_file(from), step_file(to), overwrite = TRUE)
}

# Rasterize line features (column friction_val) onto the template grid,
# widened by buffer_m either side. Unbuffered lines take every cell they
# touch, so the result is continuous; buffered bands take cells whose centre
# is inside the band.
rasterize_band <- function(x, template, buffer_m, fun, touches = buffer_m == 0) {
  g <- if (buffer_m > 0) sf::st_buffer(x, buffer_m) else x
  terra::rasterize(terra::vect(g), template, field = "friction_val", fun = fun,
                   touches = touches, background = NA)
}

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

country_union <- dplyr::summarise(districts, geometry = sf::st_union(geometry))
country_vect  <- terra::vect(country_union)
rm(districts, country_union); gc()

# Build template from WorldPop
wp       <- terra::rast(cfg$worldpop_file)
wp_proj  <- terra::project(wp, cfg$target_crs); rm(wp); gc()
template <- terra::rast(ext        = terra::ext(wp_proj),
                        resolution = cfg$target_resolution_m,
                        crs        = cfg$target_crs)
template <- terra::resample(wp_proj, template)
names(template) <- "u5_pop"
rm(wp_proj); gc()

# Fill interior NA cells -- WorldPop uses NA for zero-population cells,
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
# 01 POPULATION
# =============================================================================
message("--- 01 Population ---")

p <- rules$population

# Native 100m: counts per cell -> children per density_unit_km2, so values
# don't depend on the grid resolution. focalMat(type = "circle") weights sum
# to 1, so focal(fun = sum) gives the weighted MEAN density in the window.
cell_km2    <- prod(terra::res(template)) / 1e6
pop_density <- template * (p$density_unit_km2 / cell_km2)

w          <- terra::focalMat(pop_density, d = p$smoothing_radius_m, type = "circle")
message("Smoothing window: ", sum(w > 0), " cells (",
        round(sum(w > 0) * cell_km2, 2), " km2)")
pop_smooth <- terra::focal(pop_density, w = w, fun = sum,
                           na.rm = TRUE, fillvalue = 0)
rm(pop_density, w); gc()

# Settled land: min_cost to max_cost, rising with log density
pop_log  <- log1p(pop_smooth)
gmin     <- as.numeric(terra::global(pop_log, "min", na.rm = TRUE)[[1]])
gmax     <- as.numeric(terra::global(pop_log, "max", na.rm = TRUE)[[1]])
pop_norm <- (pop_log - gmin) / (gmax - gmin)
rm(pop_log); gc()

pop_cost <- p$min_cost + pop_norm * (p$max_cost - p$min_cost)
rm(pop_norm); gc()

# Empty land: one fixed cost, above any settled land
pop_cost <- terra::ifel(pop_smooth <= p$empty_threshold, p$empty_cost, pop_cost)
message(sprintf("Empty cells (smoothed density <= %s): %.1f%% of the country",
                p$empty_threshold,
                100 * as.numeric(terra::global(pop_smooth <= p$empty_threshold, "mean", na.rm = TRUE)[[1]])))
rm(pop_smooth); gc()

pop_cost <- terra::clamp(pop_cost, 0, 1)
names(pop_cost) <- "population_cost"

stopifnot("Population cost is not at target resolution" =
            isTRUE(all.equal(terra::res(pop_cost), rep(cfg$target_resolution_m, 2), tolerance = 1e-6)))

write_step(pop_cost, "01_population")

# =============================================================================
# 02 ROADS
# =============================================================================
message("--- 02 Roads ---")

roads <- read_vector(cfg$roads_file)

if (!is.null(roads) && nrow(roads) > 0) {
  
  friction <- read_step("01_population")
  
  roads$friction_val <- dplyr::if_else(
    roads$highway %in% c("motorway", "trunk", "primary"),
    rules$roads$primary, rules$roads$secondary
  )
  
  # Road corridor = the road line widened by rules$roads$buffer_m either side
  road_r <- rasterize_band(roads, friction, rules$roads$buffer_m, fun = "min")
  rm(roads); gc()
  
  friction <- terra::ifel(!is.na(road_r), friction * road_r, friction)
  friction <- terra::clamp(friction, 0.05, 1)
  rm(road_r); gc()
  
  message(sprintf("Road corridor: road +/- %sm", rules$roads$buffer_m))
  write_step(friction, "02_roads")
  
} else copy_step("01_population", "02_roads")

# =============================================================================
# 03 RIVERS (Jubba and Shabelle channels, open at bridges)
# =============================================================================
message("--- 03 Rivers ---")

rivers <- read_vector(cfg$rivers_file)
rv     <- rules$rivers

prim <- NULL
if (!is.null(rivers) && nrow(rivers) > 0) {
  nm         <- if ("name" %in% names(rivers)) tolower(rivers$name) else rep(NA_character_, nrow(rivers))
  is_primary <- !is.na(nm) & grepl(rv$primary_pattern, nm)
  message(sprintf("Rivers: %d Jubba/Shabelle segments (%.0f km) used; %d others (%.0f km) left out",
                  sum(is_primary), as.numeric(sum(sf::st_length(rivers[is_primary, ]))) / 1000,
                  sum(!is_primary), as.numeric(sum(sf::st_length(rivers[!is_primary, ]))) / 1000))
  prim <- rivers[is_primary, ]
  rm(rivers, nm, is_primary); gc()
}

if (!is.null(prim) && nrow(prim) > 0) {
  
  friction <- read_step("02_roads")
  
  prim$friction_val <- rv$channel_cost
  channel_r <- rasterize_band(prim, friction, 0, fun = "max")
  rm(prim); gc()
  
  # Leave the channel open near bridges
  bridges <- read_vector(cfg$bridges_file)
  if (!is.null(bridges) && nrow(bridges) > 0) {
    bridges$friction_val <- 1
    bridge_r  <- rasterize_band(bridges, friction, rv$bridge_gap_m, fun = "max", touches = TRUE)
    n_open    <- as.numeric(terra::global(!is.na(channel_r) & !is.na(bridge_r), "sum", na.rm = TRUE)[[1]])
    channel_r <- terra::ifel(!is.na(bridge_r), NA, channel_r)
    message(sprintf("Channel cells left open at bridges: %.0f", n_open))
    rm(bridge_r, bridges); gc()
  }
  
  friction <- terra::ifel(!is.na(channel_r) & channel_r > friction, channel_r, friction)
  rm(channel_r); gc()
  
  write_step(friction, "03_rivers")
  
} else copy_step("02_roads", "03_rivers")

# =============================================================================
# 04 WATER BODIES
# =============================================================================
message("--- 04 Water bodies ---")

water <- read_vector(cfg$water_bodies_file)

if (!is.null(water) && nrow(water) > 0) {
  
  friction <- read_step("03_rivers")
  water$friction_val <- rules$water$cost
  
  water_r <- terra::rasterize(terra::vect(water), friction,
                              field = "friction_val", fun = "max",
                              background = NA)
  rm(water); gc()
  
  friction <- terra::ifel(!is.na(water_r), water_r, friction)
  rm(water_r); gc()
  
  write_step(friction, "04_water")
  
} else copy_step("03_rivers", "04_water")

# =============================================================================
# FINAL: MASK TO COUNTRY
# =============================================================================
message("--- Final mask ---")

friction  <- read_step("04_water")
country_r <- terra::rasterize(country_vect, friction, field = 1, background = NA)
friction  <- terra::mask(friction, country_r)
rm(country_r); gc()

friction <- terra::clamp(friction, lower = 0.05, upper = 1)
names(friction) <- "friction"

stopifnot("Final friction raster is not at target resolution" =
            isTRUE(all.equal(terra::res(friction), rep(cfg$target_resolution_m, 2), tolerance = 1e-6)))

out_file <- file.path(cfg$output_dir, "somalia_friction_100m.tif")
terra::writeRaster(friction, out_file, overwrite = TRUE,
                   gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES"))

vals <- terra::values(friction, mat = FALSE)
message("\n[FINAL] range: ", round(min(vals, na.rm = TRUE), 4),
        " - ", round(max(vals, na.rm = TRUE), 4))
print(round(quantile(vals, c(0, .05, .25, .5, .75, .95, 1), na.rm = TRUE), 4))

rm(friction, vals); gc()
wipe_terra_tmp()

message("\nDone. Saved to: ", normalizePath(out_file))