# =============================================================================
# setup_boundary_sets.R
#
# One-time setup for versioned district boundary sets.
#
#   1. FREEZES the district file the app and friction pipeline use today
#      (data/districts_shp.Rds) as boundary set OLD_SET. The FeatureServer
#      no longer serves this version, so this copy is the only one left --
#      it is copied, never moved or overwritten.
#   2. FETCHES the current som_health_districts_2026v1 layer (layer 2) as
#      boundary set NEW_SET, with the same added columns get_boundaries.R
#      adds (district_name = DISP_L2, etc.).
#   3. COMPARES the two, matched on DIST_UID, and writes a report:
#      districts added/removed, name changes, area change, and overlap (IoU)
#      for every district. Nothing is decided from this automatically --
#      read it before building friction surfaces for NEW_SET.
#
# Output:
#   data/boundary_sets/<set>/districts_shp.Rds
#   data/boundary_sets/<set>/set_info.json
#   data/boundary_sets/compare_<OLD_SET>_vs_<NEW_SET>.csv
#
# Run from the project root (same place as build_national_friction_surface.R).
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

OLD_SET <- '2026a'   # what the app uses today
NEW_SET <- '2026b'   # layer 2 as it is now (data last edited 30 Jun 2026)

current_file <- 'data/districts_shp.Rds'
sets_dir     <- 'data/boundary_sets'
layer_url    <- paste0('https://services.arcgis.com/5T5nSi527N4F7luB/arcgis/rest/services/',
                       'Somalia_Public_Health_Boundaries_2026/FeatureServer/2')

set_path <- function(set, f = 'districts_shp.Rds') file.path(sets_dir, set, f)

# --- 1. freeze the current file as OLD_SET ----------------------------------
if (!file.exists(current_file)) stop('Not found: ', current_file)
dir.create(file.path(sets_dir, OLD_SET), recursive = TRUE, showWarnings = FALSE)

if (file.exists(set_path(OLD_SET))) {
  message('Boundary set ', OLD_SET, ' already exists -- leaving it untouched')
} else {
  file.copy(current_file, set_path(OLD_SET))
  writeLines(toJSON(list(
    set = OLD_SET,
    source = 'Frozen copy of data/districts_shp.Rds (earlier download of FeatureServer layer 2)',
    source_file_modified = format(file.mtime(current_file)),
    frozen_at = format(Sys.time())
  ), auto_unbox = TRUE, pretty = TRUE), set_path(OLD_SET, 'set_info.json'))
  message('Froze ', current_file, ' as boundary set ', OLD_SET)
}
old <- readRDS(set_path(OLD_SET))

# --- 2. fetch the current layer as NEW_SET ----------------------------------
dir.create(file.path(sets_dir, NEW_SET), recursive = TRUE, showWarnings = FALSE)

if (file.exists(set_path(NEW_SET))) {
  message('Boundary set ', NEW_SET, ' already exists -- loading it (delete the folder to re-fetch)')
  new <- readRDS(set_path(NEW_SET))
} else {
  message('Fetching layer 2 from the FeatureServer...')
  meta <- tryCatch(fromJSON(paste0(layer_url, '?f=json')), error = function(e) NULL)
  last_edit <- tryCatch(
    format(as.POSIXct(meta$editingInfo$dataLastEditDate / 1000, origin = '1970-01-01', tz = 'UTC')),
    error = function(e) NA_character_)

  new <- st_read(paste0(layer_url, '/query?where=1%3D1&outFields=*&f=geojson'), quiet = TRUE) |>
    st_make_valid() |>
    mutate(
      zone_name     = DISP_LS,
      region_name   = DISP_L1,
      district_name = DISP_L2,
      admin_id      = as.integer(factor(DISP_L2)),
      region_id     = as.integer(factor(DISP_L1)),
      zone_id       = as.integer(factor(DISP_LS)),
      u5_pop_density_km2 = WP_U5 / (Shape__Area / 1e6)
    )
  saveRDS(new, set_path(NEW_SET))
  writeLines(toJSON(list(
    set = NEW_SET, source = layer_url, layer_name = meta$name %||% NA,
    layer_data_last_edit_utc = last_edit, fetched_at = format(Sys.time()),
    n_features = nrow(new)
  ), auto_unbox = TRUE, pretty = TRUE), set_path(NEW_SET, 'set_info.json'))
  message('Saved ', nrow(new), ' districts as boundary set ', NEW_SET,
          ' (layer data last edited ', last_edit, ' UTC)')
}

# --- 3. compare ---------------------------------------------------------------
message('\nComparing ', OLD_SET, ' (', nrow(old), ' districts) with ', NEW_SET,
        ' (', nrow(new), ' districts), matched on DIST_UID')

for (x in list(old, new)) if (anyDuplicated(x$DIST_UID)) stop('DIST_UID is not unique in one of the sets')

old <- st_transform(old, 4326); new <- st_transform(new, 4326)
ids <- union(old$DIST_UID, new$DIST_UID)

report <- bind_rows(lapply(ids, function(id) {
  o <- old[old$DIST_UID == id, ]; n <- new[new$DIST_UID == id, ]
  if (nrow(o) == 0) return(data.frame(DIST_UID = id, name_old = NA, name_new = n$DISP_L2,
                                      status = 'ADDED', area_old_km2 = NA,
                                      area_new_km2 = as.numeric(st_area(n)) / 1e6, iou = NA))
  if (nrow(n) == 0) return(data.frame(DIST_UID = id, name_old = o$DISP_L2, name_new = NA,
                                      status = 'REMOVED', area_old_km2 = as.numeric(st_area(o)) / 1e6,
                                      area_new_km2 = NA, iou = NA))
  ao <- as.numeric(st_area(o)); an <- as.numeric(st_area(n))
  inter <- suppressMessages(tryCatch(as.numeric(st_area(st_intersection(st_geometry(o), st_geometry(n)))),
                                     error = function(e) NA_real_))
  inter <- if (length(inter) == 0) 0 else sum(inter)
  iou <- inter / (ao + an - inter)
  data.frame(DIST_UID = id, name_old = o$DISP_L2, name_new = n$DISP_L2,
             status = if (!is.na(iou) && iou > 0.999) 'same' else 'CHANGED',
             area_old_km2 = ao / 1e6, area_new_km2 = an / 1e6, iou = iou)
}))

report$name_changed <- !is.na(report$name_old) & !is.na(report$name_new) & report$name_old != report$name_new
report$area_change_pct <- round(100 * (report$area_new_km2 - report$area_old_km2) / report$area_old_km2, 2)
report <- report |>
  mutate(across(c(area_old_km2, area_new_km2), ~ round(.x, 1)), iou = round(iou, 4)) |>
  arrange(factor(status, c('ADDED', 'REMOVED', 'CHANGED', 'same')), iou)

out_csv <- file.path(sets_dir, sprintf('compare_%s_vs_%s.csv', OLD_SET, NEW_SET))
write.csv(report, out_csv, row.names = FALSE)

message(sprintf('\n  same: %d | CHANGED: %d | ADDED: %d | REMOVED: %d | renamed: %d',
                sum(report$status == 'same'), sum(report$status == 'CHANGED'),
                sum(report$status == 'ADDED'), sum(report$status == 'REMOVED'),
                sum(report$name_changed)))
changed <- report[report$status != 'same' | report$name_changed, ]
if (nrow(changed)) {
  message('\nDistricts that differ:')
  print(changed, row.names = FALSE)
}
message('\nFull report: ', out_csv)
