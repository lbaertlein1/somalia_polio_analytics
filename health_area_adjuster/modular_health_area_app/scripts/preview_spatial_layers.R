library(sf)
library(terra)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(dplyr)
library(elevatr)

# =============================================================================
# LOAD DISTRICT BOUNDARIES
# =============================================================================

message("Loading districts...")
districts <- sf::st_read(
  paste0("https://services.arcgis.com/5T5nSi527N4F7luB/arcgis/rest/services/",
         "Somalia_Public_Health_Boundaries_2026/FeatureServer/2/",
         "query?where=1%3D1&outFields=*&f=geojson"),
  quiet = TRUE
)
somalia_union <- sf::st_union(districts)
somalia_vect  <- terra::vect(somalia_union)

# =============================================================================
# OSM VECTOR LAYERS
# Simplified for country-wide display (tolerance ~500m)
# =============================================================================

read_osm <- function(path) {
  if (!file.exists(path)) return(NULL)
  sf::st_read(path, quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(4326) |>
    sf::st_simplify(dTolerance = 0.005, preserveTopology = TRUE)
}

message("Loading OSM layers...")
roads   <- read_osm("data/osm_inputs/somalia_roads.gpkg")
rivers  <- read_osm("data/osm_inputs/somalia_rivers.gpkg")
bridges <- read_osm("data/osm_inputs/somalia_bridges.gpkg")
water   <- read_osm("data/osm_inputs/somalia_water_bodies.gpkg")

# =============================================================================
# LAND COVER (WorldCover dominant class)
# =============================================================================

message("Loading WorldCover layers...")
lc_vars  <- c("trees", "shrubs", "grassland", "cropland", "bare", "wetland")
lc_files <- file.path("data/land_surface_cache/landuse",
                      paste0("WorldCover_", lc_vars, "_30s.tif"))

lc_stack <- terra::rast(lapply(lc_files, function(f) {
  terra::crop(terra::rast(f), somalia_vect)
}))
names(lc_stack) <- lc_vars

lc_dominant <- terra::which.max(lc_stack) |>
  terra::mask(somalia_vect) |>
  terra::aggregate(fact = 4, fun = "modal")

lc_colors <- c("#2d6a4f","#95d5b2","#d4e09b","#f4d35e","#c9b99a","#48cae4")
lc_labels <- c("Trees","Shrubs","Grassland","Cropland","Bare","Wetland")

# =============================================================================
# SLOPE (SRTM via elevatr)
# =============================================================================

message("Downloading elevation...")
elev_raw <- terra::rast(
  elevatr::get_elev_raster(
    locations = sf::st_as_sf(somalia_union),
    z = 7, clip = "locations"
  )
)

slope_deg <- terra::terrain(elev_raw, v = "slope", unit = "degrees") |>
  terra::mask(somalia_vect) |>
  terra::aggregate(fact = 2, fun = "mean")

slope_r <- raster::raster(slope_deg)
lc_r    <- raster::raster(lc_dominant)

# Population (log-scaled for display — raw U5 counts are very skewed)
message("Loading population...")
pop_raw <- terra::rast("data/som_u5_population_2025_100m.tif") |>
  terra::mask(somalia_vect) |>
  terra::aggregate(fact = 4, fun = "sum", na.rm = TRUE)

pop_log <- terra::app(pop_raw, fun = function(x) log1p(x))
pop_r   <- raster::raster(pop_log)

pop_pal <- colorNumeric(
  palette  = c("#f7fbff","#6baed6","#08306b"),
  domain   = terra::values(pop_log, na.rm = TRUE),
  na.color = "transparent"
)


slope_pal <- colorNumeric(
  palette  = c("#ffffcc","#fd8d3c","#800026"),
  domain   = c(0, 30),
  na.color = "transparent"
)

# =============================================================================
# COLOUR PALETTES FOR VECTOR LAYERS
# =============================================================================

road_pal <- colorFactor(
  palette = c("primary" = "#e63946", "secondary" = "#f4a261",
              "minor"   = "#a8dadc", "track"     = "#ccc"),
  domain  = roads$road_class
)

river_pal <- colorFactor(
  palette = c("major" = "#023e8a", "minor" = "#90e0ef"),
  domain  = rivers$river_type
)

# =============================================================================
# MAP
# =============================================================================

message("Building map...")

# Build base map first
m <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron,  group = "Basemap: Light") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Basemap: Satellite") |>
  addRasterImage(lc_r,    colors = lc_colors, opacity = 0.75, group = "Land cover") |>
  addRasterImage(slope_r, colors = slope_pal, opacity = 0.75, group = "Slope") |>
  addPolygons(
    data = districts, group = "Districts",
    fill = FALSE, color = "black", weight = 1, opacity = 0.6,
    label = ~NAME_L2, labelOptions = labelOptions(textsize = "11px")
  ) |>
  addLayersControl(
    baseGroups    = c("Basemap: Light", "Basemap: Satellite"),
    overlayGroups = c("Land cover", "Slope", "Water bodies",
                      "Rivers", "Roads", "Bridges", "Districts"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  hideGroup(c("Slope", "Bridges")) |>
  addLegend(colors = lc_colors, labels = lc_labels,
            title = "Land cover", position = "bottomleft") |>
  addLegend(pal = slope_pal, values = c(0, 30),
            title = "Slope (°)", position = "bottomright")

# Add vector layers conditionally
if (!is.null(water) && nrow(water) > 0){
  m <- addPolygons(m, data = water, group = "Water bodies",
                   fillColor = "#48cae4", fillOpacity = 0.6,
                   color = "#023e8a", weight = 0.5)
}

if (!is.null(rivers) && nrow(rivers) > 0){
  m <- addPolylines(m, data = rivers, group = "Rivers",
                    color = ~river_pal(river_type),
                    weight = ~ifelse(river_type == "major", 2, 1), opacity = 0.8)
}

if (!is.null(roads) && nrow(roads) > 0) {
  m <- addPolylines(m, data = roads, group = "Roads",
                    color = ~road_pal(road_class),
                    weight = ~case_when(road_class == "primary" ~ 2,
                                        road_class == "secondary" ~ 1.5, TRUE ~ 1),
                    opacity = 0.8)
  m <- addLegend(m, pal = road_pal, values = roads$road_class,
                 title = "Roads", position = "bottomleft")
}

if (!is.null(bridges) && nrow(bridges) > 0) {
  m <- addCircleMarkers(m, data = sf::st_centroid(bridges), group = "Bridges",
                        radius = 3, color = "#6a0572", fill = TRUE,
                        fillOpacity = 0.9, stroke = FALSE)
}



# =============================================================================
# SAVE AND OPEN
# =============================================================================

message("Saving...")
saveWidget(m, "friction_layers_preview.html", selfcontained = FALSE)
browseURL("friction_layers_preview.html")
message("Done.")
