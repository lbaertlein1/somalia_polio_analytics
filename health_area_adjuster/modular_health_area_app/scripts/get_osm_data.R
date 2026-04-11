suppressPackageStartupMessages({
  library(osmextract)
  library(sf)
  library(dplyr)
})

# -------------------------------------------------------------------
# SETTINGS
# -------------------------------------------------------------------
out_dir <- "data/osm_inputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

osm_cache_dir <- file.path(out_dir, "cache")
dir.create(osm_cache_dir, recursive = TRUE, showWarnings = FALSE)

place_name <- "Somalia"

# -------------------------------------------------------------------
# DOWNLOAD OSM LAYERS
# -------------------------------------------------------------------
message("Downloading OSM lines layer...")
osm_lines <- oe_get(
  place = place_name,
  provider = "geofabrik",
  layer = "lines",
  download_directory = osm_cache_dir,
  quiet = FALSE
)

message("Downloading OSM multipolygons layer...")
osm_polys <- tryCatch(
  {
    oe_get(
      place = place_name,
      provider = "geofabrik",
      layer = "multipolygons",
      download_directory = osm_cache_dir,
      quiet = FALSE
    )
  },
  error = function(e) {
    message("multipolygons layer failed, trying polygons instead...")
    oe_get(
      place = place_name,
      provider = "geofabrik",
      layer = "polygons",
      download_directory = osm_cache_dir,
      quiet = FALSE
    )
  }
)

# -------------------------------------------------------------------
# BASIC CHECKS
# -------------------------------------------------------------------
message("Lines rows: ", nrow(osm_lines))
message("Polygons rows: ", nrow(osm_polys))
message("Lines geometry: ", paste(unique(sf::st_geometry_type(osm_lines)), collapse = ", "))
message("Polygons geometry: ", paste(unique(sf::st_geometry_type(osm_polys)), collapse = ", "))

# -------------------------------------------------------------------
# ROADS
# -------------------------------------------------------------------
message("Extracting roads...")

roads <- osm_lines |>
  filter(
    !is.na(highway),
    highway %in% c(
      "motorway", "trunk", "primary", "secondary", "tertiary",
      "unclassified", "residential", "track", "path", "service"
    )
  ) |>
  mutate(
    road_class = case_when(
      highway %in% c("motorway", "trunk", "primary") ~ "primary",
      highway %in% c("secondary", "tertiary") ~ "secondary",
      highway %in% c("unclassified", "residential") ~ "minor",
      highway %in% c("track", "path", "service") ~ "track",
      TRUE ~ "minor"
    )
  ) |>
  st_make_valid()

message("Road rows: ", nrow(roads))

# -------------------------------------------------------------------
# RIVERS / WATERWAYS
# -------------------------------------------------------------------
message("Extracting rivers...")

rivers <- osm_lines |>
  filter(
    !is.na(waterway),
    waterway %in% c("river", "stream", "canal", "drain", "ditch")
  ) |>
  mutate(
    river_type = case_when(
      waterway == "river" ~ "major",
      TRUE ~ "minor"
    )
  ) |>
  st_make_valid()

message("River rows: ", nrow(rivers))

# -------------------------------------------------------------------
# BRIDGES
# Prefer explicit bridge column if present, otherwise parse other_tags
# -------------------------------------------------------------------
message("Extracting bridges...")

if ("bridge" %in% names(osm_lines)) {
  bridges <- osm_lines |>
    filter(
      !is.na(highway),
      !is.na(bridge),
      bridge %in% c("yes", "true", "viaduct")
    ) |>
    st_make_valid()
} else {
  tag_text <- if ("other_tags" %in% names(osm_lines)) osm_lines$other_tags else rep(NA_character_, nrow(osm_lines))
  
  bridges <- osm_lines |>
    mutate(.other_tags_tmp = tag_text) |>
    filter(
      !is.na(highway),
      !is.na(.other_tags_tmp),
      grepl('bridge', .other_tags_tmp, ignore.case = TRUE)
    ) |>
    st_make_valid() |>
    dplyr::select(-.other_tags_tmp)
}

message("Bridge rows: ", nrow(bridges))

# -------------------------------------------------------------------
# WATER BODIES
# -------------------------------------------------------------------
message("Extracting water bodies...")

water_bodies <- osm_polys |>
  filter(
    (!is.na(natural) & natural %in% c("water", "wetland"))
  ) |>
  st_make_valid()

message("Water body rows: ", nrow(water_bodies))

# -------------------------------------------------------------------
# OPTIONAL: COAST / SEA / OCEAN POLYGONS
# -------------------------------------------------------------------
message("Extracting coast water polygons...")

coast_water <- osm_polys |>
  filter(
    (!is.na(natural) & natural == "coastline") 
  ) |>
  st_make_valid()

message("Coast water rows: ", nrow(coast_water))

# -------------------------------------------------------------------
# WRITE OUTPUTS
# -------------------------------------------------------------------
roads_file <- file.path(out_dir, "somalia_roads.gpkg")
rivers_file <- file.path(out_dir, "somalia_rivers.gpkg")
bridges_file <- file.path(out_dir, "somalia_bridges.gpkg")
water_file <- file.path(out_dir, "somalia_water_bodies.gpkg")
coast_file <- file.path(out_dir, "somalia_coast_water.gpkg")

message("Saving roads...")
st_write(roads, roads_file, delete_dsn = TRUE, quiet = TRUE)

message("Saving rivers...")
st_write(rivers, rivers_file, delete_dsn = TRUE, quiet = TRUE)

message("Saving bridges...")
st_write(bridges, bridges_file, delete_dsn = TRUE, quiet = TRUE)

message("Saving water bodies...")
st_write(water_bodies, water_file, delete_dsn = TRUE, quiet = TRUE)

if (nrow(coast_water) > 0) {
  message("Saving coast water polygons...")
  st_write(coast_water, coast_file, delete_dsn = TRUE, quiet = TRUE)
}

# -------------------------------------------------------------------
# SUMMARY
# -------------------------------------------------------------------
message("Done.")
message("Saved to: ", normalizePath(out_dir))
message("  Roads:        ", roads_file, "  [", nrow(roads), "]")
message("  Rivers:       ", rivers_file, "  [", nrow(rivers), "]")
message("  Bridges:      ", bridges_file, "  [", nrow(bridges), "]")
message("  Water bodies: ", water_file, "  [", nrow(water_bodies), "]")
if (nrow(coast_water) > 0) {
  message("  Coast water:  ", coast_file, "  [", nrow(coast_water), "]")
}