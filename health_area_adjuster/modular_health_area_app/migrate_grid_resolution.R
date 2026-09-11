# =============================================================================
# migrate_grid_resolution_scoped.R
#
# Scoped version of migrate_grid_resolution.R -- fixes current_assignments/
# current_team_assignments ONLY for districts Garasbaley, Danyile, and Khada,
# and ONLY within the Aug SNID 2026 campaign. All other districts, campaigns,
# and practice/testing data are left completely untouched by this script.
#
# WHY THIS IS SAFE, AND WHAT IT DOES:
#   Same underlying fix as migrate_grid_resolution.R -- see that file's
#   header for the full explanation of why re-deriving current_assignments
#   from the saved boundary polygons is safe and non-destructive. This
#   version just narrows the WHERE clause to the three real districts.
#
# USAGE:
#   1. BACK UP THE DATABASE FIRST. This writes to production tables.
#   2. Run once as-is -- it will PRINT every campaign in the database and
#      STOP without touching anything, so you can confirm the correct
#      campaign_id for "Aug SNID 2026" before proceeding.
#   3. Set TARGET_CAMPAIGN_ID below to that value.
#   4. Run with DRY_RUN <- TRUE (the default) -- reports every version that
#      WOULD change and why, writes nothing.
#   5. Review the dry-run output. Once satisfied, set DRY_RUN <- FALSE and
#      run again to actually apply the fixes.
# =============================================================================

library(DBI)
library(RPostgres)
library(dotenv)
library(sf)
library(dplyr)
library(jsonlite)
library(geojsonsf)

DRY_RUN <- TRUE               # <-- set to FALSE only after reviewing a dry-run pass
TARGET_CAMPAIGN_ID <- 2    # <-- set this to the Aug SNID 2026 campaign_id, shown below
TARGET_DISTRICTS <- c('Garasbaley', 'Danyile', 'Khada')

dotenv::load_dot_env('.env_v2')
DB_HOST <- Sys.getenv('DB_HOST', '23.239.19.115')
DB_PORT <- as.integer(Sys.getenv('DB_PORT', '5432'))
DB_NAME <- Sys.getenv('DB_NAME', 'somalia_health_areas_v2')
POSTGRES_PASSWORD <- Sys.getenv('POSTGRES_PASSWORD', '')
if (!nzchar(POSTGRES_PASSWORD)) stop('POSTGRES_PASSWORD is not set. Add it to .env_v2 before running this script.')

# Only what's actually needed: make_paint_grid() (standalone, no Shiny
# dependency) and safe_make_valid()/%||%. NOT sourcing mod_db_v2.R -- its DB
# functions expect a pool object and have side effects this script
# deliberately avoids; the JSON encode/decode helpers are small enough to
# redefine directly below instead, so there's no risk of accidentally
# calling something meant for the live app.
source('mod_initial_health_area_generation.R')
source('app_helpers.R')

.from_json_vec_db <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(jsonlite::fromJSON(x, simplifyVector = TRUE), error = function(e) NULL)
}
.from_json_sf_db <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(geojsonsf::geojson_sf(x), error = function(e) NULL)
}
.to_json_for_db <- function(x) {
  if (is.null(x)) return(NA_character_)
  tryCatch(jsonlite::toJSON(x, auto_unbox = TRUE, null = 'null'), error = function(e) NA_character_)
}

cat('Loading districts_shp...\n')
districts_shp <- readRDS('districts_shp.Rds')
districts_shp <- safe_make_valid(districts_shp)

con <- dbConnect(RPostgres::Postgres(), host = DB_HOST, port = DB_PORT, dbname = DB_NAME,
                 user = 'postgres', password = POSTGRES_PASSWORD, sslmode = 'require')
on.exit(dbDisconnect(con), add = TRUE)

# Always list every campaign, every run -- this is the only way to be sure
# TARGET_CAMPAIGN_ID actually points at "Aug SNID 2026" rather than
# something that merely sounds right. Stops here (writing nothing) until
# TARGET_CAMPAIGN_ID is explicitly set to one of the ids printed below.
campaign_list <- dbGetQuery(con, "SELECT campaign_id, campaign_name, is_active, created_at FROM campaigns ORDER BY campaign_id")
cat('=== Campaigns in database ===\n')
print(campaign_list)
cat('\n')

if (is.null(TARGET_CAMPAIGN_ID)) {
  stop('TARGET_CAMPAIGN_ID is not set. Find "Aug SNID 2026" in the list above and set TARGET_CAMPAIGN_ID to its campaign_id, then re-run.')
}
if (!(TARGET_CAMPAIGN_ID %in% campaign_list$campaign_id)) {
  stop(sprintf('TARGET_CAMPAIGN_ID = %s does not match any campaign_id above.', TARGET_CAMPAIGN_ID))
}
cat(sprintf('Targeting campaign_id %s (%s), districts: %s\n\n',
            TARGET_CAMPAIGN_ID,
            campaign_list$campaign_name[campaign_list$campaign_id == TARGET_CAMPAIGN_ID],
            paste(TARGET_DISTRICTS, collapse = ', ')))

cat(sprintf('=== %s ===\n\n', if (DRY_RUN) 'DRY RUN -- no changes will be written' else 'LIVE RUN -- changes WILL be written'))

# Same clamped-cellsize formula as mod_health_area_tab.R's own grid_n
# reactive -- this is intentionally the SAME logic, not a re-derivation of
# it, since the whole point is to match what the app builds today.
.current_grid_n_for_extent <- function(max_dim, floor_n) {
  target_cellsize <- max_dim / 160
  cellsize        <- max(100, min(500, target_cellsize))
  max(floor_n, as.integer(round(max_dim / cellsize)))
}

# Spatial re-derivation, same approach as mod_health_area_tab.R's
# .spatial_remap_assignments() -- but with st_nearest_feature() as the
# fallback for cells outside every saved polygon, since there's no "fresh
# BFS proposal" available in a batch script the way there is in the live
# app's own rv$initial_assignments.
.remap_from_polygons <- function(poly_sf, grid_sf) {
  if (is.null(poly_sf) || nrow(poly_sf) == 0) return(NULL)
  area_col <- if ('dfa_name' %in% names(poly_sf)) 'dfa_name'
  else if ('team_name' %in% names(poly_sf)) 'team_name'
  else names(poly_sf)[1]
  
  tryCatch({
    poly_valid <- safe_make_valid(sf::st_transform(poly_sf, 4326))
    centroids  <- sf::st_centroid(sf::st_transform(grid_sf, sf::st_crs(poly_valid)))
    joined     <- sf::st_join(centroids, poly_valid[area_col], join = sf::st_intersects, left = TRUE)
    if (nrow(joined) != nrow(centroids)) return(NULL)   # overlap artifact -- see mod_health_area_tab.R's identical guard
    out <- as.character(sf::st_drop_geometry(joined)[[area_col]])
    
    missing <- which(is.na(out))
    if (length(missing) > 0) {
      nearest <- sf::st_nearest_feature(centroids[missing, ], poly_valid)
      out[missing] <- as.character(poly_valid[[area_col]][nearest])
    }
    out
  }, error = function(e) NULL)
}

# =============================================================================
# Health areas (mapping_versions)
# =============================================================================
cat('--- Health areas ---\n')
district_placeholders <- paste0('$', seq(2, 1 + length(TARGET_DISTRICTS)), collapse = ', ')
ha_rows <- dbGetQuery(con, sprintf("
  SELECT version_id, district_name, saved_dfa_sf, smoothed_dfa_sf, current_assignments
  FROM mapping_versions
  WHERE campaign_id = $1 AND district_name IN (%s)
", district_placeholders), c(list(as.integer(TARGET_CAMPAIGN_ID)), as.list(TARGET_DISTRICTS)))
cat(sprintf('%d health-area version(s) to check (scoped to %s, campaign_id %s).\n\n',
            nrow(ha_rows), paste(TARGET_DISTRICTS, collapse = ', '), TARGET_CAMPAIGN_ID))

ha_checked <- 0L; ha_needs_fix <- 0L; ha_fixed <- 0L; ha_skipped_no_boundary <- 0L; ha_failed <- 0L

for (i in seq_len(nrow(ha_rows))) {
  r <- ha_rows[i, ]
  poly_sf <- .from_json_sf_db(r$smoothed_dfa_sf)
  if (is.null(poly_sf) || nrow(poly_sf) == 0) poly_sf <- .from_json_sf_db(r$saved_dfa_sf)
  if (is.null(poly_sf) || nrow(poly_sf) == 0) { ha_skipped_no_boundary <- ha_skipped_no_boundary + 1L; next }
  
  ha_checked <- ha_checked + 1L
  ca <- .from_json_vec_db(r$current_assignments)
  ca_len <- if (is.null(ca)) 0L else length(ca)
  
  dinfo <- districts_shp |> dplyr::filter(district_name == r$district_name)
  if (nrow(dinfo) == 0) {
    cat(sprintf('[health] version %d (%s): SKIP -- district not found in districts_shp\n', r$version_id, r$district_name))
    next
  }
  district_3857 <- sf::st_transform(dinfo, 3857) |>
    dplyr::summarise(geometry = sf::st_union(geometry)) |> sf::st_as_sf() |> safe_make_valid()
  bbox    <- sf::st_bbox(district_3857)
  max_dim <- max(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin)
  grid_n  <- .current_grid_n_for_extent(max_dim, floor_n = 10L)
  
  district_sf_4326 <- sf::st_transform(district_3857, 4326)
  grid_info <- tryCatch(make_paint_grid(district_sf = district_sf_4326, grid_n = grid_n), error = function(e) NULL)
  if (is.null(grid_info)) {
    cat(sprintf('[health] version %d (%s): SKIP -- grid build failed\n', r$version_id, r$district_name))
    ha_failed <- ha_failed + 1L
    next
  }
  target_n <- nrow(grid_info$grid_sf)
  
  if (ca_len == target_n) next   # already correctly sized -- nothing to do
  
  ha_needs_fix <- ha_needs_fix + 1L
  cat(sprintf('[health] version %d (%s): MISMATCH -- stored %d cells, current grid needs %d cells.\n',
              r$version_id, r$district_name, ca_len, target_n))
  
  remapped <- .remap_from_polygons(poly_sf, grid_info$grid_sf)
  if (is.null(remapped)) {
    cat(sprintf('  -> FAILED to remap (spatial join error) -- left unchanged.\n'))
    ha_failed <- ha_failed + 1L
    next
  }
  
  if (DRY_RUN) {
    cat(sprintf('  -> would write %d re-derived assignments.\n', length(remapped)))
  } else {
    tryCatch({
      dbExecute(con, "UPDATE mapping_versions SET current_assignments = $2 WHERE version_id = $1",
                list(as.integer(r$version_id), .to_json_for_db(remapped)))
      cat(sprintf('  -> WROTE %d re-derived assignments.\n', length(remapped)))
      ha_fixed <- ha_fixed + 1L
    }, error = function(e) {
      cat(sprintf('  -> DB WRITE FAILED: %s\n', e$message))
      ha_failed <<- ha_failed + 1L
    })
  }
}

cat(sprintf('\nHealth areas: %d checked, %d skipped (no boundary), %d needed fixing, %d %s, %d failed.\n\n',
            ha_checked, ha_skipped_no_boundary, ha_needs_fix,
            if (DRY_RUN) ha_needs_fix - ha_failed else ha_fixed,
            if (DRY_RUN) 'would-fix' else 'fixed', ha_failed))

# =============================================================================
# Team areas (team_area_versions)
# =============================================================================
cat('--- Team areas ---\n')
ta_rows <- dbGetQuery(con, sprintf("
  SELECT team_version_id, health_area_name, based_on_health_area_version_id,
         saved_team_sf, smoothed_team_sf, current_team_assignments
  FROM team_area_versions
  WHERE campaign_id = $1 AND district_name IN (%s)
", district_placeholders), c(list(as.integer(TARGET_CAMPAIGN_ID)), as.list(TARGET_DISTRICTS)))
cat(sprintf('%d team-area version(s) to check (scoped to %s, campaign_id %s).\n\n',
            nrow(ta_rows), paste(TARGET_DISTRICTS, collapse = ', '), TARGET_CAMPAIGN_ID))

ta_checked <- 0L; ta_needs_fix <- 0L; ta_fixed <- 0L; ta_skipped_no_boundary <- 0L; ta_failed <- 0L

for (i in seq_len(nrow(ta_rows))) {
  r <- ta_rows[i, ]
  poly_sf <- .from_json_sf_db(r$smoothed_team_sf)
  if (is.null(poly_sf) || nrow(poly_sf) == 0) poly_sf <- .from_json_sf_db(r$saved_team_sf)
  if (is.null(poly_sf) || nrow(poly_sf) == 0) { ta_skipped_no_boundary <- ta_skipped_no_boundary + 1L; next }
  
  ta_checked <- ta_checked + 1L
  ca <- .from_json_vec_db(r$current_team_assignments)
  ca_len <- if (is.null(ca)) 0L else length(ca)
  
  # Team-area's own grid is built from the PARENT health area's saved
  # polygon, not the district -- pulled from mapping_versions via this
  # row's own based_on_health_area_version_id, same as the live app does.
  ha_row <- dbGetQuery(con, "SELECT saved_dfa_sf, smoothed_dfa_sf, dfa_names FROM mapping_versions WHERE version_id = $1",
                       list(as.integer(r$based_on_health_area_version_id)))
  if (nrow(ha_row) == 0) {
    cat(sprintf('[team] version %d (%s): SKIP -- parent health-area version %s not found\n',
                r$team_version_id, r$health_area_name, r$based_on_health_area_version_id))
    next
  }
  ha_poly <- .from_json_sf_db(ha_row$smoothed_dfa_sf[1])
  if (is.null(ha_poly) || nrow(ha_poly) == 0) ha_poly <- .from_json_sf_db(ha_row$saved_dfa_sf[1])
  if (is.null(ha_poly) || nrow(ha_poly) == 0) {
    cat(sprintf('[team] version %d (%s): SKIP -- parent health-area has no boundary\n', r$team_version_id, r$health_area_name))
    next
  }
  area_col <- if ('dfa_name' %in% names(ha_poly)) 'dfa_name' else names(ha_poly)[1]
  this_ha_sf <- ha_poly[ha_poly[[area_col]] == r$health_area_name, ]
  if (nrow(this_ha_sf) == 0) {
    cat(sprintf('[team] version %d (%s): SKIP -- health area not found in parent boundary\n', r$team_version_id, r$health_area_name))
    next
  }
  
  ha_3857 <- sf::st_transform(this_ha_sf, 3857) |>
    dplyr::summarise(geometry = sf::st_union(geometry)) |> sf::st_as_sf() |> safe_make_valid()
  bbox    <- sf::st_bbox(ha_3857)
  max_dim <- max(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin)
  grid_n  <- .current_grid_n_for_extent(max_dim, floor_n = 20L)   # 20-floor, matching mod_team_area_tab.R's own
  
  ha_sf_4326 <- sf::st_transform(ha_3857, 4326)
  grid_info <- tryCatch(make_paint_grid(district_sf = ha_sf_4326, grid_n = grid_n), error = function(e) NULL)
  if (is.null(grid_info)) {
    cat(sprintf('[team] version %d (%s): SKIP -- grid build failed\n', r$team_version_id, r$health_area_name))
    ta_failed <- ta_failed + 1L
    next
  }
  target_n <- nrow(grid_info$grid_sf)
  
  if (ca_len == target_n) next
  
  ta_needs_fix <- ta_needs_fix + 1L
  cat(sprintf('[team] version %d (%s): MISMATCH -- stored %d cells, current grid needs %d cells.\n',
              r$team_version_id, r$health_area_name, ca_len, target_n))
  
  remapped <- .remap_from_polygons(poly_sf, grid_info$grid_sf)
  if (is.null(remapped)) {
    cat(sprintf('  -> FAILED to remap (spatial join error) -- left unchanged.\n'))
    ta_failed <- ta_failed + 1L
    next
  }
  
  if (DRY_RUN) {
    cat(sprintf('  -> would write %d re-derived assignments.\n', length(remapped)))
  } else {
    tryCatch({
      dbExecute(con, "UPDATE team_area_versions SET current_team_assignments = $2 WHERE team_version_id = $1",
                list(as.integer(r$team_version_id), .to_json_for_db(remapped)))
      cat(sprintf('  -> WROTE %d re-derived assignments.\n', length(remapped)))
      ta_fixed <- ta_fixed + 1L
    }, error = function(e) {
      cat(sprintf('  -> DB WRITE FAILED: %s\n', e$message))
      ta_failed <<- ta_failed + 1L
    })
  }
}

cat(sprintf('\nTeam areas: %d checked, %d skipped (no boundary), %d needed fixing, %d %s, %d failed.\n\n',
            ta_checked, ta_skipped_no_boundary, ta_needs_fix,
            if (DRY_RUN) ta_needs_fix - ta_failed else ta_fixed,
            if (DRY_RUN) 'would-fix' else 'fixed', ta_failed))

if (DRY_RUN) {
  cat('This was a DRY RUN -- nothing was written. Review the output above, then set DRY_RUN <- FALSE to apply.\n')
} else {
  cat('Migration complete.\n')
}