# =============================================================================
# generate_demo_data.R
# Generates demo datasets for Somalia polio catchment area mapping.
# Selects the target district FIRST, then pulls all data clipped to it.
#
# Real data (via R packages):
#   - District boundaries     source RDS
#   - OSM roads, rivers, water bodies, bridges   osmextract (bbox filter)
#   - Elevation / slope       elevatr (SRTM, clipped to district)
#   - Land / ocean mask       rnaturalearth
#
# Simulated data:
#   - Health facilities       ~5 per district, sampled within polygon
#   - WorldPop total pop      spatially smoothed raster
#   - WorldPop U5 male 0-4    derived from total
#   - WorldPop U5 female 0-4  derived from total
#   - Land cover              ESA WorldCover classes, rule-based
#
# Output: ~/Github/somalia_polio_analytics/health_area_mapping/data/demo/
# =============================================================================

# install.packages(c("sf","terra","dplyr","osmextract","elevatr",
#                    "rnaturalearth","rnaturalearthdata"))

library(sf)
library(terra)
library(dplyr)
library(osmextract)
library(elevatr)
library(rnaturalearth)
library(rnaturalearthdata)

set.seed(42)

# =============================================================================
# PATHS
# =============================================================================
source_rds <- "~/Github/old_somalia_repo/db_setup/Somalia_SIA_DataManager/data/districts_shp.Rds"
demo_dir   <- "~/Github/somalia_polio_analytics/health_area_mapping/data/demo"
osm_cache  <- file.path(demo_dir, "osm_cache")

dir.create(demo_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(osm_cache, recursive = TRUE, showWarnings = FALSE)
message("Output directory: ", normalizePath(demo_dir))

# =============================================================================
# TARGET DISTRICT  — change this to run for a different district
# =============================================================================
TARGET_DISTRICT <- "Danyile"

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

norm_inv <- function(r) {
  mn <- global(r, "min", na.rm = TRUE)[[1]]
  mx <- global(r, "max", na.rm = TRUE)[[1]]
  1 - (r - mn) / (mx - mn)
}

# =============================================================================
# 1. DISTRICT BOUNDARIES — select target district immediately
# =============================================================================
message("\n[1/9] Loading district boundaries...")

districts_all <- readRDS(source_rds)
if (!inherits(districts_all, "sf")) stop("districts_shp is not an sf object")
districts_all <- st_transform(districts_all, 4326)
if (!"district_id" %in% names(districts_all)) {
  districts_all$district_id <- seq_len(nrow(districts_all))
}

# Select target district
demo_district <- districts_all |>
  filter(district_name == TARGET_DISTRICT)

if (nrow(demo_district) == 0) {
  stop("District '", TARGET_DISTRICT, "' not found. Available: ",
       paste(sort(districts_all$district_name), collapse = ", "))
}

demo_district_nm <- demo_district$district_name
message("  Target district: ", demo_district_nm)

# Spatial helpers
demo_vect  <- vect(demo_district)
demo_buf   <- st_buffer(demo_district, 0.05)   # 5km buffer for edge features
demo_bbox  <- st_bbox(demo_buf)

# Somalia union (for population scaling)
somalia <- st_union(districts_all) |> st_make_valid()

# Save district
saveRDS(demo_district, file.path(demo_dir, "districts_shp.Rds"))
st_write(demo_district,
         file.path(demo_dir, "district_boundaries.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)

message("  Saved: districts_shp.Rds + district_boundaries.gpkg")

# =============================================================================
# 2. LAND / OCEAN MASK  (rnaturalearth — clip to district bbox)
# =============================================================================
message("\n[2/9] Fetching land/ocean mask from Natural Earth...")

all_land <- ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(4326) |> st_make_valid()

somalia_land <- ne_countries(
  country = "Somalia", scale = "medium", returnclass = "sf"
) |> st_transform(4326) |> st_make_valid() |>
  st_intersection(demo_buf)

bbox_poly <- st_as_sfc(demo_bbox) |> st_set_crs(4326)
ocean     <- st_difference(bbox_poly, st_union(all_land) |> st_make_valid()) |>
  st_intersection(demo_buf)

st_write(somalia_land,
         file.path(demo_dir, "somalia_land.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)
st_write(ocean,
         file.path(demo_dir, "ocean_mask.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)

message("  Saved: somalia_land.gpkg, ocean_mask.gpkg")

# =============================================================================
# 3. OSM DATA — query with district bbox to minimise download
# =============================================================================
message("\n[3/9] Fetching OSM data via Geofabrik (Somalia extract, bbox filtered)...")
message("  .pbf cached at: ", osm_cache)
message("  District bbox: ",
        paste(round(demo_bbox, 4), collapse = ", "))

# osmextract filters by bbox during read — much faster than loading all Somalia
osm_bbox <- c(demo_bbox["xmin"], demo_bbox["ymin"],
              demo_bbox["xmax"], demo_bbox["ymax"])

# --- 3a. Roads ---------------------------------------------------------------
message("  [3a] Roads...")
roads <- oe_get(
  place              = "Somalia",
  layer              = "lines",
  extra_tags         = c("highway", "surface", "tracktype", "bridge"),
  query              = "SELECT * FROM lines WHERE highway IS NOT NULL",
  vectortranslate_options = c(
    "-spat", osm_bbox[1], osm_bbox[2], osm_bbox[3], osm_bbox[4]
  ),
  download_directory = osm_cache,
  quiet              = TRUE
) |>
  select(osm_id, highway, surface, tracktype, bridge, geometry) |>
  filter(highway %in% c(
    "motorway","trunk","primary","secondary","tertiary",
    "unclassified","residential","track","path","footway"
  )) |>
  mutate(
    road_class = case_when(
      highway %in% c("motorway","trunk","primary")  ~ "primary",
      highway %in% c("secondary","tertiary")         ~ "secondary",
      highway %in% c("unclassified","residential")   ~ "minor",
      highway %in% c("track","path","footway")       ~ "track",
      TRUE                                           ~ "other"
    ),
    speed_kmh = case_when(
      road_class == "primary"   ~ 80,
      road_class == "secondary" ~ 50,
      road_class == "minor"     ~ 30,
      road_class == "track"     ~ 15,
      TRUE                      ~ 10
    )
  ) |>
  st_transform(4326) |>
  st_make_valid() |>
  st_intersection(demo_buf)

st_write(roads, file.path(demo_dir, "roads.gpkg"), delete_dsn = TRUE, quiet = TRUE)
message("    ", nrow(roads), " road segments")

# --- 3b. Rivers --------------------------------------------------------------
message("  [3b] Rivers...")
rivers <- oe_get(
  place              = "Somalia",
  layer              = "lines",
  extra_tags         = c("waterway", "name"),
  query              = "SELECT * FROM lines WHERE waterway IN ('river','stream','canal')",
  vectortranslate_options = c(
    "-spat", osm_bbox[1], osm_bbox[2], osm_bbox[3], osm_bbox[4]
  ),
  download_directory = osm_cache,
  quiet              = TRUE
) |>
  select(osm_id, waterway, name, geometry) |>
  mutate(
    river_type          = if_else(waterway %in% c("river","canal"), "major", "minor"),
    friction_multiplier = if_else(river_type == "major", 50, 20)
  ) |>
  st_transform(4326) |>
  st_make_valid() |>
  st_intersection(demo_buf)
st_write(rivers, file.path(demo_dir, "rivers.gpkg"), delete_dsn = TRUE, quiet = TRUE)
message("    ", nrow(rivers), " river segments")

# --- 3c. Water bodies --------------------------------------------------------
message("  [3c] Water bodies...")
water_bodies <- oe_get(
  place              = "Somalia",
  layer              = "multipolygons",
  extra_tags         = c("natural", "water", "waterway", "name"),
  query              = "SELECT * FROM multipolygons WHERE natural = 'water' OR water IS NOT NULL",
  vectortranslate_options = c(
    "-spat", osm_bbox[1], osm_bbox[2], osm_bbox[3], osm_bbox[4]
  ),
  download_directory = osm_cache,
  quiet              = TRUE
) |>
  select(osm_id, natural, water, name, geometry) |>
  st_transform(4326) |>
  st_make_valid() |>
  st_intersection(demo_buf)

st_write(water_bodies, file.path(demo_dir, "water_bodies.gpkg"), delete_dsn = TRUE, quiet = TRUE)
message("    ", nrow(water_bodies), " water body polygons")

# --- 3d. Bridges -------------------------------------------------------------
message("  [3d] Bridges...")
bridges <- oe_get(
  place              = "Somalia",
  layer              = "lines",
  extra_tags         = c("bridge", "highway"),
  query              = "SELECT * FROM lines WHERE bridge = 'yes' AND highway IS NOT NULL",
  vectortranslate_options = c(
    "-spat", osm_bbox[1], osm_bbox[2], osm_bbox[3], osm_bbox[4]
  ),
  download_directory = osm_cache,
  quiet              = TRUE
) |>
  select(osm_id, bridge, highway, geometry) |>
  st_transform(4326) |>
  st_make_valid() |>
  st_intersection(demo_buf)

st_write(bridges, file.path(demo_dir, "bridges.gpkg"), delete_dsn = TRUE, quiet = TRUE)
message("    ", nrow(bridges), " bridge segments")

# =============================================================================
# 4. ELEVATION  (elevatr — clipped to district only)
# =============================================================================
message("\n[4/9] Fetching elevation via elevatr (z=8 for urban district)...")

dem_raw    <- elevatr::get_elev_raster(locations = demo_district, z = 8,
                                       clip = "locations")
dem        <- rast(dem_raw)
names(dem) <- "elevation_m"
slope      <- terrain(dem, v = "slope", unit = "degrees")
names(slope) <- "slope_degrees"

# Raster template — full bbox extent, no masking yet
r_template   <- dem
r_template[] <- NA

# District mask
district_mask <- rasterize(demo_vect, r_template, field = 1)

message("  DEM resolution: ~", round(res(dem)[1] * 111000), "m")
message("  District cells: ", global(district_mask, "sum", na.rm = TRUE)[[1]])

# =============================================================================
# 5. SIMULATED WORLDPOP RASTERS
# =============================================================================
message("\n[5/9] Generating simulated WorldPop rasters...")

# Distance proxies — computed over full bbox, masked to district
dist_coast <- tryCatch({
  coast_line <- st_cast(somalia_land, "MULTILINESTRING") |>
    st_make_valid() |> vect()
  distance(r_template, coast_line) |> mask(district_mask)
}, error = function(e) {
  message("  Coast distance fallback: uniform")
  ifel(!is.na(district_mask), 1, NA)
})

dist_river <- if (nrow(rivers) > 0) {
  distance(r_template, vect(rivers)) |> mask(district_mask)
} else {
  message("  No rivers — using uniform")
  ifel(!is.na(district_mask), 1, NA)
}

dist_urban <- distance(r_template, centroids <- st_centroid(demo_district) |>
                         vect()) |> mask(district_mask)

coast_score <- norm_inv(dist_coast)
river_score <- norm_inv(dist_river)
urban_score <- norm_inv(dist_urban)

# Smooth noise
noise      <- r_template
noise[]    <- runif(ncell(r_template), 0, 1)
noise      <- focal(noise, w = 9, fun = "mean", na.policy = "omit")
noise      <- mask(noise, district_mask)
noise_norm <- norm_inv(noise)

pop_surface <- (
  0.30 * coast_score +
    0.25 * river_score +
    0.25 * urban_score +
    0.20 * noise_norm
) |> mask(district_mask)

# Scale to district's proportional share of Somalia's 17M
district_share <- as.numeric(st_area(demo_district)) /
  as.numeric(st_area(somalia))
district_pop   <- 17e6 * district_share
pop_sum        <- global(pop_surface, "sum", na.rm = TRUE)[[1]]
pop_total      <- pop_surface * (district_pop / pop_sum)
pop_total      <- max(pop_total, 0)
names(pop_total) <- "population"

# U5 rasters
rural_score <- 1 - urban_score
u5_fraction <- mask(0.17 + (rural_score * 0.03), district_mask)
u5_total_r  <- pop_total * u5_fraction

male_frac   <- mask(noise_norm * 0.06 + 0.47, district_mask)
u5_male     <- u5_total_r * male_frac
u5_female   <- u5_total_r * (1 - male_frac)
names(u5_male)   <- "u5_male_0_4"
names(u5_female) <- "u5_female_0_4"

message("  District population: ",
        formatC(round(global(pop_total,  "sum", na.rm = TRUE)[[1]]),
                format = "d", big.mark = ","))
message("  District U5:         ",
        formatC(round(global(u5_male + u5_female, "sum", na.rm = TRUE)[[1]]),
                format = "d", big.mark = ","))

# =============================================================================
# 6. SIMULATED LAND COVER
# =============================================================================
message("\n[6/9] Generating simulated land cover raster...")

lc    <- r_template
lc[]  <- 60   # default: bare/sparse

pop_vals <- values(pop_surface, na.rm = FALSE)
q30 <- quantile(pop_vals, 0.30, na.rm = TRUE)
q50 <- quantile(pop_vals, 0.50, na.rm = TRUE)

lc[pop_surface > q30]                       <- 20   # shrubland
lc[pop_surface > q50]                       <- 30   # grassland
lc[dist_river  < 10000]                     <- 40   # cropland
lc[dist_river  < 3000]                      <- 90   # wetland
lc[dist_urban  < 5000]                      <- 50   # built-up
lc[dist_river < 5000 & dist_coast < 30000]  <- 10   # tree cover

if (nrow(water_bodies) > 0) {
  lc <- rasterize(vect(water_bodies), lc, field = 80, update = TRUE)
}

lc <- mask(lc, district_mask)
names(lc) <- "landcover_class"

lc_lookup <- data.frame(
  value        = c(10,    20,          30,          40,
                   50,         60,            80,      90),
  label        = c("Tree cover","Shrubland","Grassland","Cropland",
                   "Built-up","Bare/sparse","Water","Wetland"),
  speed_factor = c(0.50,  0.80,  1.00,  0.90,  1.10,  1.00,  0.00,  0.60)
)

# =============================================================================
# 7. HEALTH FACILITIES (~5 in target district)
# =============================================================================
message("\n[7/9] Generating health facilities...")

facility_types <- c("Health Centre", "MCH", "Primary Health Unit", "Hospital")
type_weights   <- c(0.45, 0.30, 0.20, 0.05)
n_fac          <- sample(4:6, 1)

pts <- tryCatch(
  st_sample(demo_district, size = n_fac * 5, type = "random"),
  error = function(e) st_sample(st_buffer(demo_district, 0.001),
                                size = n_fac, type = "random")
)
pts <- pts[seq_len(min(n_fac, length(pts)))]

facilities <- st_sf(
  facility_id   = paste0("HF_", sprintf("%04d", seq_along(pts))),
  facility_name = paste0(demo_district_nm, " HF-", seq_along(pts)),
  facility_type = sample(facility_types, length(pts),
                         replace = TRUE, prob = type_weights),
  district_id   = demo_district$district_id,
  district_name = demo_district_nm,
  geometry      = pts
)
st_crs(facilities) <- 4326
message("  ", nrow(facilities), " facilities generated")

# =============================================================================
# 8. SAVE RASTER OUTPUTS
# =============================================================================
message("\n[8/9] Saving raster outputs...")

writeRaster(dem,      file.path(demo_dir, "dem_elevation.tif"),          overwrite = TRUE)
writeRaster(slope,    file.path(demo_dir, "dem_slope.tif"),              overwrite = TRUE)
writeRaster(pop_total,file.path(demo_dir, "worldpop_total.tif"),         overwrite = TRUE)
writeRaster(u5_male,  file.path(demo_dir, "worldpop_u5_male_0_4.tif"),  overwrite = TRUE)
writeRaster(u5_female,file.path(demo_dir, "worldpop_u5_female_0_4.tif"),overwrite = TRUE)
writeRaster(lc,       file.path(demo_dir, "landcover.tif"),              overwrite = TRUE)

write.csv(lc_lookup,
          file.path(demo_dir, "landcover_lookup.csv"),
          row.names = FALSE)
st_write(facilities,
         file.path(demo_dir, "health_facilities.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)

# =============================================================================
# 9. SUMMARY
# =============================================================================
message("\n[9/9] Summary...")
message(strrep("=", 65))
message("DEMO DATA GENERATION COMPLETE")
message("District: ", demo_district_nm)
message(strrep("=", 65))

files <- c(
  "district_boundaries.gpkg", "somalia_land.gpkg", "ocean_mask.gpkg",
  "roads.gpkg", "rivers.gpkg", "water_bodies.gpkg", "bridges.gpkg",
  "dem_elevation.tif", "dem_slope.tif",
  "worldpop_total.tif", "worldpop_u5_male_0_4.tif", "worldpop_u5_female_0_4.tif",
  "landcover.tif", "landcover_lookup.csv", "health_facilities.gpkg"
)

sizes <- sapply(file.path(demo_dir, files), function(f) {
  if (file.exists(f)) paste0(round(file.size(f) / 1024 / 1024, 2), " MB") else "missing"
})

print(data.frame(file = files, size = sizes), row.names = FALSE)
message("\nSaved to: ", normalizePath(demo_dir))

