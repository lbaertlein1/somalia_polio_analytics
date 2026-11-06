# =============================================================================
# repair_s2_invalid_boundaries.R
#
# Finds stored boundaries that are valid on a flat map but invalid on the
# sphere (two edges crossing as great-circle arcs -- e.g. "Loop 0 is not
# valid: Edge 4 crosses edge 61"), which break exports and any other s2
# operation on them. New saves are repaired automatically (.to_json_for_db
# -> s2_repair_polygons()); this script handles rows saved before that.
#
# Checks every GeoJSON boundary column of mapping_versions and
# team_area_versions (all versions: drafts, published and old).
#
# Usage (from the app folder):
#   Rscript scripts/repair_s2_invalid_boundaries.R <out_dir>          report only
#   Rscript scripts/repair_s2_invalid_boundaries.R <out_dir> --apply  also write
#       the repaired geometry back (same row, same column; nothing else in
#       the row changes). <out_dir>/s2_scan.csv lists every affected row;
#       with --apply, <out_dir>/s2_backup.rds keeps the original GeoJSON of
#       every row changed, so it can be put back.
# =============================================================================

args    <- commandArgs(trailingOnly = TRUE)
out_dir <- args[1]
apply   <- '--apply' %in% args
if (is.na(out_dir) || startsWith(out_dir, '--')) stop('Usage: Rscript scripts/repair_s2_invalid_boundaries.R <out_dir> [--apply]')
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

suppressMessages(source('global.R'))

TABLES <- list(
  mapping_versions   = list(id = 'version_id',      cols = c('saved_dfa_sf', 'smoothed_dfa_sf', 'scope_saved_dfa_sf')),
  team_area_versions = list(id = 'team_version_id', cols = c('saved_team_sf', 'smoothed_team_sf'))
)

# Keep only columns that actually exist (schemas differ between installs).
existing <- DBI::dbGetQuery(pool, "SELECT table_name, column_name FROM information_schema.columns
                                   WHERE table_name IN ('mapping_versions', 'team_area_versions')")
info_cols <- c(mapping_versions   = 'campaign_id, district_name, owner_username, version_number, is_current',
               team_area_versions = 'campaign_id, district_name, health_area_name, owner_username, version_number, is_current')

found <- list(); backup <- list()
for (tbl in names(TABLES)) {
  t <- TABLES[[tbl]]
  have <- existing$column_name[existing$table_name == tbl]
  if (!t$id %in% have) { cat(tbl, ': no', t$id, 'column -- skipped\n'); next }
  cols <- intersect(t$cols, have)
  extra <- trimws(strsplit(info_cols[[tbl]], ',')[[1]]); extra <- intersect(extra, have)
  for (col in cols) {
    rows <- DBI::dbGetQuery(pool, sprintf("SELECT %s AS id, %s, %s::text AS g FROM %s WHERE %s IS NOT NULL",
                                          t$id, paste(extra, collapse = ', '), col, tbl, col))
    cat(sprintf('%s.%s: %d rows\n', tbl, col, nrow(rows)))
    for (i in seq_len(nrow(rows))) {
      g <- tryCatch(geojsonsf::geojson_sf(rows$g[i]), error = function(e) NULL)
      if (is.null(g) || nrow(g) == 0) next
      v <- tryCatch(s2::s2_is_valid(sf::st_geometry(g)), error = function(e) rep(NA, nrow(g)))
      bad <- which(is.na(v) | !v)
      if (!length(bad)) next
      fixed <- s2_repair_polygons(g)
      unfixed <- attr(fixed, 's2_unfixed') %||% NA
      area_ratio <- tryCatch({
        a0 <- sum(as.numeric(sf::st_area(sf::st_transform(sf::st_make_valid(sf::st_set_crs(g[bad, ], 4326)), 32638))))
        a1 <- sum(as.numeric(sf::st_area(sf::st_transform(fixed[bad, ], 32638))))
        round(a1 / a0, 5)
      }, error = function(e) NA)
      nm <- if ('dfa_name' %in% names(g)) paste(g$dfa_name[bad], collapse = '; ') else ''
      found[[length(found) + 1]] <- data.frame(
        table = tbl, column = col, id = rows$id[i], rows[i, extra, drop = FALSE],
        n_polygons = nrow(g), n_invalid = length(bad), names = nm,
        unfixed = unfixed, area_ratio = area_ratio, stringsAsFactors = FALSE)
      if (apply && identical(as.integer(unfixed), 0L)) {
        backup[[length(backup) + 1]] <- list(table = tbl, column = col, id = rows$id[i], geojson = rows$g[i])
        DBI::dbExecute(pool, sprintf("UPDATE %s SET %s = $1::jsonb WHERE %s = $2", tbl, col, t$id),
                       list(as.character(geojsonsf::sf_geojson(fixed)), rows$id[i]))
      }
    }
  }
}

res <- if (length(found)) dplyr::bind_rows(found) else data.frame()
utils::write.csv(res, file.path(out_dir, 's2_scan.csv'), row.names = FALSE)
if (apply) saveRDS(backup, file.path(out_dir, 's2_backup.rds'))
cat(sprintf('\n%d stored boundary set(s) with polygons invalid on the sphere%s. Report: %s\n',
            nrow(res), if (apply) sprintf(' -- %d repaired in the database', length(backup)) else ' (report only)',
            file.path(out_dir, 's2_scan.csv')))
if (nrow(res)) print(res[, intersect(c('table', 'column', 'id', 'campaign_id', 'district_name', 'health_area_name',
                                        'is_current', 'n_invalid', 'names', 'unfixed', 'area_ratio'), names(res))])
