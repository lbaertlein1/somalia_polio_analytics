# =============================================================================
# load_osm_names.R -- OpenStreetMap place names for describing missed places
#
# Downloads the current OpenStreetMap extract for all of Somalia (Geofabrik,
# ~160 MB) and stores every named feature in vb_place_names (source 'osm'),
# alongside the campaign-monitoring names, so labels can say "40 m N of
# Masjid Al-Nuur" or "on Wadada Makkah":
#   - named points (mosques, schools, shops, markets, ...)
#   - named streets, as a point every ~100 m along them
#   - named buildings / sites, as one point inside each (large areas such as
#     districts and regions are left out -- they aren't landmarks)
# Replaces the previous OSM load each time. Run from the vaccine_buddy/
# folder, e.g. monthly:
#   source('scripts/load_osm_names.R')
# =============================================================================

library(sf)
if (file.exists('.env')) readRenviron('.env')
source('R/odk.R')   # .insert_new(), SOMALIA_BBOX

env_first <- function(keys, default = '') {
  for (k in keys) { v <- Sys.getenv(k); if (nzchar(v)) return(v) }
  default
}

pbf <- file.path(tempdir(), 'somalia-latest.osm.pbf')
message('Downloading the Somalia OpenStreetMap extract...')
utils::download.file('https://download.geofabrik.de/africa/somalia-latest.osm.pbf', pbf, mode = 'wb', quiet = TRUE)

sf_use_s2(FALSE)
# Drop the Arabic-script half of bilingual names ("Hodan <Arabic>" -> "Hodan")
clean <- function(x) trimws(gsub('[[:space:]]+', ' ', gsub('[؀-ۿ]+', '', x)))
as_rows <- function(name, pts) {
  xy <- sf::st_coordinates(pts)
  data.frame(name = clean(name), lon = xy[, 1], lat = xy[, 2])
}

message('Reading named points...')
p <- st_read(pbf, query = 'SELECT osm_id, name FROM points WHERE name IS NOT NULL', quiet = TRUE)
pts <- cbind(source_ref = paste0('n', p$osm_id), as_rows(p$name, st_geometry(p)))

message('Reading named streets...')
l <- st_read(pbf, query = 'SELECT osm_id, name FROM lines WHERE name IS NOT NULL AND highway IS NOT NULL', quiet = TRUE)
l <- st_cast(st_transform(l, 3857), 'LINESTRING', warn = FALSE)
samples <- st_line_sample(l, density = 1 / 100)                   # a point every ~100 m
keep <- lengths(samples) > 0
s <- st_cast(st_sf(osm_id = l$osm_id[keep], name = l$name[keep], geometry = samples[keep]), 'POINT', warn = FALSE)
s <- st_transform(s, 4326)
streets <- cbind(source_ref = paste0('w', s$osm_id, '_', ave(seq_len(nrow(s)), s$osm_id, FUN = seq_along)),
                 as_rows(s$name, st_geometry(s)))

message('Reading named buildings and sites...')
m <- st_read(pbf, query = 'SELECT osm_id, osm_way_id, name FROM multipolygons WHERE name IS NOT NULL', quiet = TRUE)
m <- st_make_valid(m)
m <- m[as.numeric(st_area(st_transform(m, 3857))) <= 1e6, ]     # districts, regions etc. aren't landmarks
mp <- st_point_on_surface(m)
sites <- cbind(source_ref = paste0('a', ifelse(is.na(m$osm_id), m$osm_way_id, m$osm_id)), as_rows(m$name, st_geometry(mp)))

all <- rbind(pts, streets, sites)
b <- SOMALIA_BBOX
all <- all[nchar(gsub('[^[:alpha:]]', '', all$name)) >= 3 & all$lon >= b['xmin'] & all$lon <= b['xmax'] &
             all$lat >= b['ymin'] & all$lat <= b['ymax'], ]
all <- all[!duplicated(all$source_ref), ]
all$source <- 'osm'
all$observed_at <- Sys.time()

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = env_first(c('VB_DB_HOST', 'DB_HOST')),
  port     = as.integer(env_first(c('VB_DB_PORT', 'DB_PORT'), '5432')),
  dbname   = env_first('VB_DB_NAME', 'vaccine_buddy'),
  user     = env_first(c('VB_DB_USER', 'DB_USER')),
  password = env_first(c('VB_DB_PASSWORD', 'DB_PASSWORD')),
  sslmode  = env_first(c('VB_DB_SSL', 'DB_SSL'), 'require')
)
DBI::dbWithTransaction(con, {
  DBI::dbExecute(con, "DELETE FROM vb_place_names WHERE source = 'osm'")
  n <- .insert_new(con, 'vb_place_names', all[, c('source', 'source_ref', 'name', 'lon', 'lat', 'observed_at')])
})
DBI::dbDisconnect(con)
unlink(pbf)
message(sprintf('Stored %s OpenStreetMap names (%s points, %s street points, %s buildings/sites).',
                format(n, big.mark = ','), format(nrow(pts), big.mark = ','),
                format(nrow(streets), big.mark = ','), format(nrow(sites), big.mark = ',')))
