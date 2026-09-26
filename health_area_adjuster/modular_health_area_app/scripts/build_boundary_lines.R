# =============================================================================
# build_boundary_lines.R
#
# Builds the national layer of lines that health-area and team-area
# boundaries should follow instead of cross: PRIMARY ROADS and PRIMARY
# RIVERS only. Used by the app's generation step as a soft crossing penalty
# (same channel and penalty as subdivision boundaries): stepping from one
# grid cell to the next ACROSS one of these lines is expensive, moving
# along it is not.
#
# Inputs (the same OSM files the friction pipeline uses):
#   data/osm_inputs/somalia_roads.gpkg   -> highway in motorway/trunk/primary
#                                           (= "primary" in the friction build)
#   data/osm_inputs/somalia_rivers.gpkg  -> already filtered to
#                                           waterway == "river"
#
# Primary rivers = the Jubba and the Shabelle, Somalia's two permanent
# rivers, matched by name (RIVER_PATTERN, case-insensitive, so the OSM name
# variants "Jubba River", "Webi Shabeelle", "Shebelle" and the Arabic-suffixed
# ones all match). The rivers file also holds ~33,000 km of unnamed and named
# seasonal watercourses (togga), which are left out. The script still prints
# every river name with its length, and which ones were kept. Water bodies
# are NOT included.
#
# Output: data/boundary_lines/somalia_primary_lines.rds  (EPSG:3857, deployed
#         with the app)
#
# Run from the project root (the app folder).
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

ROAD_CLASSES <- c('motorway', 'trunk', 'primary')
RIVER_PATTERN <- 'jubba|shabeel|shebel'   # regex on the lower-cased OSM name

roads_file  <- 'data/osm_inputs/somalia_roads.gpkg'
rivers_file <- 'data/osm_inputs/somalia_rivers.gpkg'
out_file    <- 'data/boundary_lines/somalia_primary_lines.rds'

for (f in c(roads_file, rivers_file)) if (!file.exists(f)) stop('Not found: ', f)

to_lines <- function(x) {
  x <- st_make_valid(st_transform(x, 3857))
  x <- x[st_geometry_type(x) %in% c('LINESTRING', 'MULTILINESTRING'), ]
  x
}

roads <- st_read(roads_file, quiet = TRUE) |> to_lines()
roads <- roads[roads$highway %in% ROAD_CLASSES, ]
roads <- st_sf(line_class = 'primary_road',
               name = if ('name' %in% names(roads)) roads$name else NA_character_,
               geometry = st_geometry(roads))
message(sprintf('Primary roads: %d segments, %.0f km', nrow(roads), sum(st_length(roads)) / 1000))

rivers <- st_read(rivers_file, quiet = TRUE) |> to_lines()
rivers <- st_sf(line_class = 'primary_river',
                name = if ('name' %in% names(rivers)) rivers$name else NA_character_,
                geometry = st_geometry(rivers))

river_summary <- rivers |>
  mutate(km = as.numeric(st_length(geometry)) / 1000) |>
  st_drop_geometry() |>
  group_by(name) |>
  summarise(segments = n(), km = round(sum(km)), .groups = 'drop') |>
  arrange(desc(km))
message('\nRivers in the file (name, segments, km):')
print(as.data.frame(river_summary), row.names = FALSE)

keep   <- !is.na(rivers$name) & grepl(RIVER_PATTERN, tolower(rivers$name))
rivers <- rivers[keep, ]
message('\nKept rivers: ', paste(sort(unique(rivers$name)), collapse = ' | '))
message(sprintf('Primary rivers: %d segments, %.0f km', nrow(rivers), sum(st_length(rivers)) / 1000))

lines <- rbind(roads, rivers)
lines <- st_simplify(lines, dTolerance = 10, preserveTopology = TRUE)   # 10m -- well below any grid cell
lines <- lines[!st_is_empty(lines), ]

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(lines, out_file)
message(sprintf('\nSaved %d lines to %s (%.1f MB)', nrow(lines), out_file, file.size(out_file) / 1e6))

