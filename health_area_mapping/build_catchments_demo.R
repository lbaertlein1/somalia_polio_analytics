# =============================================================================
# build_catchments.R
# Builds health facility catchment area polygons using cost-distance analysis.
#
# Approach:
#   1. Load all inputs
#   2. Build friction raster:
#        - Initialise ALL cells within district bbox to baseline cost
#        - Land cover modifies cost per cell
#        - Roads reduce cost
#        - Rivers add high cost (soft barrier)
#        - Water bodies / ocean = very high cost (near-impassable)
#        - District boundaries add moderate friction (soft constraint)
#        - NA cells inside district filled with baseline before saving
#   3. Run gridDist() from each facility (one cost surface per facility)
#   4. Assign each pixel to nearest facility by travel cost (which.min)
#   5. Convert raster to polygons, dissolve, clip to district
#   6. Add population estimates via zonal stats
#   7. Flag oversized / remote catchments
#   8. Validate coverage, save outputs
#
# Inputs  (from generate_demo_data.R):
#   health_facilities.gpkg, district_boundaries.gpkg, rivers.gpkg,
#   water_bodies.gpkg, bridges.gpkg, ocean_mask.gpkg, roads.gpkg,
#   dem_slope.tif, landcover.tif, landcover_lookup.csv,
#   worldpop_total.tif, worldpop_u5_male_0_4.tif, worldpop_u5_female_0_4.tif
#
# Outputs:
#   catchments.gpkg          catchment polygons with population estimates
#   catchments_raster.tif    facility index raster (pre-polygonisation)
#   friction.tif             friction surface (QA / visualisation)
#   catchment_summary.csv    per-facility summary table
# =============================================================================

# install.packages(c("sf", "terra", "dplyr", "smoothr"))

library(sf)
library(terra)
library(dplyr)
library(smoothr)

# =============================================================================
# PARAMETERS
# =============================================================================

# Create facility attribute lookup
fac_attrs <- facilities |>
  st_drop_geometry() |>
  mutate(
    facility_idx = row_number()
  ) |>
  select(facility_idx, facility_id, everything())

FRICTION <- list(
  base_walk      = 1.0,    # baseline: open land / bare / grassland
  road_primary   = 0.05,
  road_secondary = 0.10,
  road_minor     = 0.20,
  road_track     = 0.50,
  landcover = list(
    `10` = 2.0,   # tree cover
    `20` = 1.2,   # shrubland
    `30` = 1.0,   # grassland
    `40` = 0.9,   # cropland
    `50` = 0.8,   # built-up
    `60` = 1.0,   # bare/sparse
    `80` = 1e6,   # water (near-impassable)
    `90` = 3.0    # wetland
  ),
  river_major    = 50,
  river_minor    = 20,
  river_buf_m    = 200,
  district_border = 10,
  district_buf_m  = 500,
  impassable     = 1e6    # used for ocean / large water bodies
)

FLAG <- list(
  max_travel_cost = 240,
  u5_min          = 1200,
  u5_max          = 2800
)

# =============================================================================
# PATHS
# =============================================================================
demo_dir <- "~/Github/somalia_polio_analytics/health_area_mapping/data/demo"
out_dir  <- "~/Github/somalia_polio_analytics/health_area_mapping/data/catchments"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("Input:  ", normalizePath(demo_dir))
message("Output: ", normalizePath(out_dir))

# =============================================================================
# 1. LOAD INPUTS
# =============================================================================
message("\n[1/8] Loading inputs...")

facilities   <- st_read(file.path(demo_dir, "health_facilities.gpkg"),   quiet = TRUE)
districts    <- st_read(file.path(demo_dir, "district_boundaries.gpkg"), quiet = TRUE)
rivers       <- st_read(file.path(demo_dir, "rivers.gpkg"),              quiet = TRUE)
water_bodies <- st_read(file.path(demo_dir, "water_bodies.gpkg"),        quiet = TRUE)
bridges      <- st_read(file.path(demo_dir, "bridges.gpkg"),             quiet = TRUE)
ocean        <- st_read(file.path(demo_dir, "ocean_mask.gpkg"),          quiet = TRUE)
roads        <- st_read(file.path(demo_dir, "roads.gpkg"),               quiet = TRUE)
lc_lookup    <- read.csv(file.path(demo_dir, "landcover_lookup.csv"))

slope     <- rast(file.path(demo_dir, "dem_slope.tif"))
lc        <- rast(file.path(demo_dir, "landcover.tif"))
pop_total <- rast(file.path(demo_dir, "worldpop_total.tif"))
u5_male   <- rast(file.path(demo_dir, "worldpop_u5_male_0_4.tif"))
u5_female <- rast(file.path(demo_dir, "worldpop_u5_female_0_4.tif"))

# Consistent CRS
crs_use    <- "EPSG:4326"
facilities   <- st_transform(facilities,   crs_use)
districts    <- st_transform(districts,    crs_use)
rivers       <- st_transform(rivers,       crs_use)
water_bodies <- st_transform(water_bodies, crs_use)
bridges      <- st_transform(bridges,      crs_use)
roads        <- st_transform(roads,        crs_use)
ocean        <- st_transform(ocean,        crs_use)

district_union <- st_union(districts) |> st_make_valid()

message("  Facilities: ", nrow(facilities))
message("  Districts:  ", nrow(districts))
message("  Resolution: ~", round(res(lc)[1] * 111000), "m")

# =============================================================================
# 2. BUILD FRICTION RASTER
# =============================================================================
message("\n[2/8] Building friction raster...")

# --- 2a. Initialise: ALL cells in bbox get baseline cost --------------------
# KEY FIX: do NOT start from a masked raster. Start from full bbox extent
# so every cell has a value. Masking to district happens at polygon stage only.
message("  [2a] Initialising friction surface (full bbox)...")

friction      <- lc
friction[]    <- FRICTION$base_walk    # every cell = baseline

# Apply land cover costs where lc has valid values
for (lc_val in names(FRICTION$landcover)) {
  val  <- as.integer(lc_val)
  cost <- FRICTION$landcover[[lc_val]]
  friction[lc == val] <- cost
}

# --- 2b. Slope adjustment ---------------------------------------------------
message("  [2b] Slope adjustment (Tobler)...")

slope_r      <- resample(slope, friction, method = "bilinear")
slope_rad    <- slope_r * (pi / 180)
tobler       <- exp(-3.5 * abs(tan(slope_rad) + 0.05))
tobler_flat  <- exp(-3.5 * abs(0.05))
slope_factor <- tobler_flat / tobler
slope_factor <- ifel(is.na(slope_factor), 1, slope_factor)  # fill NA slope with 1
friction     <- friction * slope_factor

# --- 2c. Roads: lower friction ----------------------------------------------
message("  [2c] Road friction...")

if (nrow(roads) > 0) {
  roads$friction_val <- case_when(
    roads$road_class == "primary"   ~ FRICTION$road_primary,
    roads$road_class == "secondary" ~ FRICTION$road_secondary,
    roads$road_class == "minor"     ~ FRICTION$road_minor,
    roads$road_class == "track"     ~ FRICTION$road_track,
    TRUE                            ~ FRICTION$base_walk
  )
  road_r <- rasterize(vect(roads), friction,
                      field = "friction_val", fun = "min", background = NA)
  # Only update where road cost is lower than current friction
  friction <- ifel(!is.na(road_r) & road_r < friction, road_r, friction)
}

# --- 2d. Rivers: high friction barrier -------------------------------------
message("  [2d] River barriers...")

if (nrow(rivers) > 0) {
  rivers$friction_val <- ifelse(rivers$river_type == "major",
                                FRICTION$river_major, FRICTION$river_minor)
  rivers_buf <- st_buffer(rivers, FRICTION$river_buf_m / 111000)
  river_r    <- rasterize(vect(rivers_buf), friction,
                          field = "friction_val", fun = "max", background = NA)
  friction   <- ifel(!is.na(river_r), river_r, friction)
  
  # Bridges reopen river barriers
  if (nrow(bridges) > 0) {
    bridges$friction_val <- case_when(
      bridges$highway %in% c("motorway","trunk","primary")   ~ FRICTION$road_primary,
      bridges$highway %in% c("secondary","tertiary")         ~ FRICTION$road_secondary,
      bridges$highway %in% c("unclassified","residential")   ~ FRICTION$road_minor,
      TRUE                                                    ~ FRICTION$road_track
    )
    bridge_r <- rasterize(vect(bridges), friction,
                          field = "friction_val", fun = "min", background = NA)
    friction <- ifel(!is.na(bridge_r), bridge_r, friction)
  }
}

# --- 2e. Water bodies: near-impassable -------------------------------------
message("  [2e] Water body barriers...")

if (nrow(water_bodies) > 0) {
  wb_r     <- rasterize(vect(water_bodies), friction, field = 1, background = NA)
  friction <- ifel(!is.na(wb_r), FRICTION$impassable, friction)
}

# --- 2f. Ocean: impassable -------------------------------------------------
message("  [2f] Ocean mask...")

if (nrow(ocean) > 0 && !all(st_is_empty(ocean))) {
  ocean_r  <- rasterize(vect(ocean), friction, field = 1, background = NA)
  friction <- ifel(!is.na(ocean_r), FRICTION$impassable, friction)
}

# --- 2g. District boundary: soft friction barrier --------------------------
message("  [2g] District boundary friction...")

district_lines <- st_boundary(districts) |> st_make_valid()
district_buf   <- st_buffer(district_lines, FRICTION$district_buf_m / 111000)
db_r           <- rasterize(vect(district_buf), friction, field = 1, background = NA)

# Multiply boundary cells — but not if already near-impassable
friction <- ifel(!is.na(db_r) & friction < FRICTION$impassable / 2,
                 friction * FRICTION$district_border,
                 friction)

# --- 2h. Fill any remaining NAs with baseline (guarantees full coverage) ----
message("  [2h] Filling remaining NA cells with baseline...")
friction <- ifel(is.na(friction), FRICTION$base_walk, friction)

# Floor at small positive value (gridDist needs > 0)
friction <- max(friction, 0.01)

message("  Friction range: ",
        round(global(friction, "min", na.rm = TRUE)[[1]], 3), " -- ",
        round(global(friction, "max", na.rm = TRUE)[[1]], 0))
message("  NA cells remaining: ",
        global(is.na(friction), "sum", na.rm = TRUE)[[1]])

writeRaster(friction, file.path(out_dir, "friction.tif"), overwrite = TRUE)
message("  Saved: friction.tif")

# =============================================================================
# 3. COST-DISTANCE FROM EACH FACILITY
# =============================================================================
message("\n[3/8] Running cost-distance from each facility...")

# Reload from disk to ensure values are in memory
friction <- rast(file.path(out_dir, "friction.tif"))

fac_vect <- vect(facilities) |> project(crs(friction))
n_fac    <- nrow(facilities)
message("  Facilities: ", n_fac)

cost_stack <- vector("list", n_fac)

for (i in seq_len(n_fac)) {
  fac_i    <- fac_vect[i, ]
  
  # Set facility cell to 0 in the friction surface — gridDist expands from here
  origin_r        <- rasterize(fac_i, friction, field = 0, background = NA)
  combined        <- ifel(!is.na(origin_r), 0, friction)
  
  cost_i          <- gridDist(combined, target = 0)
  names(cost_i)   <- facilities$facility_id[i]
  cost_stack[[i]] <- cost_i
  message("    ", i, "/", n_fac, " -- ", facilities$facility_id[i])
}

cost_rast <- rast(cost_stack)
message("  Cost raster range: ",
        round(global(min(cost_rast), "min", na.rm = TRUE)[[1]], 1), " -- ",
        round(global(max(cost_rast), "max", na.rm = TRUE)[[1]], 1))

# =============================================================================
# 4. ASSIGN PIXELS TO NEAREST FACILITY
# =============================================================================
message("\n[4/8] Assigning pixels to nearest facility...")

catchment_raster        <- which.min(cost_rast)
names(catchment_raster) <- "facility_idx"

min_cost        <- min(cost_rast)
names(min_cost) <- "min_travel_cost"

# Mask catchment raster to district only (ocean / outside = NA)
district_r       <- rasterize(vect(district_union), catchment_raster, field = 1)
catchment_raster <- mask(catchment_raster, district_r)

writeRaster(catchment_raster,
            file.path(out_dir, "catchments_raster.tif"),
            overwrite = TRUE)
message("  Saved: catchments_raster.tif")

# =============================================================================
# 5. CONVERT TO POLYGONS + CLIP TO DISTRICT
# =============================================================================
message("\n[5/8] Converting raster to catchment polygons...")

# Polygonise with values=TRUE to carry facility_idx through
catchment_polys_raw <- as.polygons(catchment_raster, values = TRUE) |>
  st_as_sf() |>
  st_make_valid()

# Rename the value column (terra names it after the raster layer)
names(catchment_polys_raw)[1] <- "facility_idx"

# Dissolve fragmented cells into one polygon per facility
# Step 5 — correct order: dissolve → smooth → clip → repair
catchment_polys <- catchment_polys_raw |>
  group_by(facility_idx) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_make_valid() |>
  smooth(method = "ksmooth", smoothness = 2) |>
  st_set_crs(4326) |>
  st_make_valid()

catchment_polys <- catchment_polys |>
  left_join(fac_attrs, by = "facility_idx") |>
  st_intersection(district_union) |>
  st_make_valid()
message("  Catchment polygons: ", nrow(catchment_polys))
# Join facility attributes
fac_attrs <- facilities |>
  st_drop_geometry() |>
  mutate(facility_idx = seq_len(n()))

catchment_polys <- catchment_polys |>
  left_join(fac_attrs, by = "facility_idx")

# Clip to district boundary
catchment_polys <- st_intersection(
  st_make_valid(catchment_polys),
  district_union
) |> st_make_valid()

# Smooth jagged raster edges
catchment_polys <- smooth(catchment_polys, method = "ksmooth", smoothness = 2)

message("  Catchment polygons: ", nrow(catchment_polys))

# =============================================================================
# 6. POPULATION ESTIMATES
# =============================================================================
message("\n[6/8] Estimating population per catchment...")

u5_total_rast        <- u5_male + u5_female
names(u5_total_rast) <- "u5_total"

pop_zonal <- zonal(pop_total,     catchment_raster, fun = "sum", na.rm = TRUE)
u5_zonal  <- zonal(u5_total_rast, catchment_raster, fun = "sum", na.rm = TRUE)
names(pop_zonal) <- c("facility_idx", "pop_total")
names(u5_zonal)  <- c("facility_idx", "u5_total")

pop_estimates <- left_join(pop_zonal, u5_zonal, by = "facility_idx") |>
  mutate(
    pop_total = round(pop_total),
    u5_total  = round(u5_total),
    u5_pct    = round(u5_total / pop_total * 100, 1)
  )

catchment_polys <- catchment_polys |>
  left_join(pop_estimates, by = "facility_idx")

# =============================================================================
# 7. METRICS + FLAGS
# =============================================================================
message("\n[7/8] Computing metrics and flags...")

catchment_polys <- catchment_polys |>
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)

max_cost_zonal <- zonal(min_cost, catchment_raster, fun = "max", na.rm = TRUE)
names(max_cost_zonal) <- c("facility_idx", "max_travel_cost")

catchment_polys <- catchment_polys |>
  left_join(max_cost_zonal, by = "facility_idx") |>
  mutate(
    flag = case_when(
      is.na(u5_total)                        ~ "no population data",
      u5_total < FLAG$u5_min                 ~ "small -- consider merging",
      u5_total > FLAG$u5_max                 ~ "large -- consider splitting",
      max_travel_cost > FLAG$max_travel_cost ~ "remote -- long travel time",
      TRUE                                   ~ "ok"
    )
  )

flag_tbl <- catchment_polys |>
  st_drop_geometry() |>
  count(flag, name = "n_catchments")
message("  Flag summary:")
print(flag_tbl, row.names = FALSE)

# =============================================================================
# 8. COVERAGE VALIDATION
# =============================================================================
message("\n[8/8] Validating coverage...")

na_px   <- global(is.na(catchment_raster), "sum", na.rm = TRUE)[[1]]
land_px <- global(!is.na(catchment_raster), "sum", na.rm = TRUE)[[1]]
message("  Assigned pixels: ", formatC(land_px, format = "d", big.mark = ","))
message("  NA pixels:       ", formatC(na_px,   format = "d", big.mark = ","))

catchment_union <- st_union(catchment_polys) |> st_make_valid()
gap             <- st_difference(district_union, catchment_union) |> st_make_valid()
gap_km2 <- sum(as.numeric(st_area(gap)) / 1e6)
if (gap_km2 > 1) {
  warning("Gap of ", round(gap_km2, 1), " km2 detected")
} else {
  message("  Coverage check PASSED")
}

if (gap_km2 > 1) {
  warning("Gap of ", round(gap_km2, 1), " km2 detected in catchment coverage")
} else {
  message("  Coverage check PASSED")
}

# =============================================================================
# SAVE OUTPUTS
# =============================================================================
message("\nSaving outputs...")

st_write(catchment_polys,
         file.path(out_dir, "catchments.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)
message("  Saved: catchments.gpkg")
catchment_summary <- as.data.frame(catchment_polys) |>
  dplyr::select(
    facility_idx,
    facility_id    = facility_id.x,
    facility_name  = facility_name.x,
    facility_type  = facility_type.x,
    district_name  = district_name.x,
    area_km2, pop_total, u5_total, u5_pct,
    max_travel_cost, flag
  ) |>
  dplyr::arrange(district_name, facility_name)

write.csv(catchment_summary,
          file.path(out_dir, "catchment_summary.csv"),
          row.names = FALSE)
message("  Saved: catchment_summary.csv")

# =============================================================================
# FINAL SUMMARY
# =============================================================================
message("\n", strrep("=", 65))
message("CATCHMENT BUILDING COMPLETE")
message(strrep("=", 65))
message("Facilities:              ", nrow(facilities))
message("Catchments built:        ", nrow(catchment_polys))
message("Total population:        ",
        formatC(sum(catchment_polys$pop_total, na.rm = TRUE),
                format = "d", big.mark = ","))
message("Total U5:                ",
        formatC(sum(catchment_polys$u5_total, na.rm = TRUE),
                format = "d", big.mark = ","))
message("Median U5 per catchment: ",
        round(median(catchment_polys$u5_total, na.rm = TRUE)))
message("Median area (km2):       ",
        round(median(catchment_polys$area_km2, na.rm = TRUE), 1))
message("Gap area (km2):          ", round(gap_km2, 2))
message("\nOutputs saved to: ", normalizePath(out_dir))

print(catchment_summary, row.names = FALSE)



library(tmap)
library(smoothr)
tmap_mode("view")

pal <- RColorBrewer::brewer.pal(n = nrow(catchment_polys), name = "Set2")

m <- tm_shape(districts) +
  tm_polygons(fill = NA, col = "black", lwd = 2) +
  tm_shape(catchment_polys) +
  tm_polygons(
    fill        = "facility_name.x",
    fill.scale  = tm_scale(values = pal),
    fill_alpha  = 0.5,
    col         = "white",
    col_alpha   = 0.6,
    fill.legend = tm_legend(title = "Catchment"),
    popup.vars  = c("Facility"   = "facility_name.x",
                    "Type"       = "facility_type.x",
                    "Population" = "pop_total",
                    "U5"         = "u5_total",
                    "Area (km2)" = "area_km2",
                    "Flag"       = "flag")
  ) +
  tm_shape(facilities) +
  tm_dots(size = 0.15, fill = "black", shape = 17,
          popup.vars = c("Name" = "facility_name", "Type" = "facility_type")) +
  tm_shape(roads) +
  tm_lines(
    col       = "road_class",
    col.scale = tm_scale(values = c(primary   = "#555555",
                                    secondary = "#888888",
                                    minor     = "#bbbbbb",
                                    track     = "#dddddd")),
    lwd = 1, col_alpha = 0.6
  ) +
  tm_title("Health Facility Catchment Areas — Danyile District") +
  tm_scalebar() +
  tm_compass()

# Add rivers layer only if it has features
if (nrow(rivers) > 0 && !all(st_is_empty(rivers))) {
  m <- m + tm_shape(rivers) +
    tm_lines(col = "steelblue", lwd = 1.5, col_alpha = 0.7)
}

m


# Mask out near-zero population pixels before mapping
pop_display <- ifel(pop_total < 0.5, NA, pop_total)

tm_shape(districts) +
  tm_polygons(fill = NA, col = "black", lwd = 2) +
  tm_shape(catchment_polys) +
  tm_polygons(
    fill        = "facility_name.x",
    fill.scale  = tm_scale(values = pal),
    fill_alpha  = 0.6,
    col         = "white",
    col_alpha   = 0.8,
    lwd         = 1.5,
    fill.legend = tm_legend(title = "Catchment"),
    popup.vars  = c("Facility"   = "facility_name.x",
                    "Type"       = "facility_type.x",
                    "Population" = "pop_total",
                    "U5"         = "u5_total",
                    "Area (km2)" = "area_km2",
                    "Flag"       = "flag")
  ) +
  tm_shape(facilities) +
  tm_dots(size = 0.15, fill = "red", shape = 17,
          popup.vars = c("Name" = "facility_name", "Type" = "facility_type")) +
  tm_shape(rivers) +
  tm_lines(col = "steelblue", lwd = 1.5, col_alpha = 0.8) +
  tm_shape(pop_display) +
  tm_raster(
    col        = "population",
    col.scale  = tm_scale_continuous(values = "viridis", trans = "log1p"),
    col.legend = tm_legend(title = "Population"),
    col_alpha  = 0.75
  ) +
  tm_title("Catchment Areas with Population Density") +
  tm_scalebar() +
  tm_compass()