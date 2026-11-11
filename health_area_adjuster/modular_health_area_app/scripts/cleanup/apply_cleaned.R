# Write the dry-run results in <WORK>/clean_out (clean_published.R) to the DB. Env: WORK, APPLY=1 to write (else check only),
# MAX_HA_CHANGE / MAX_TEAM_CHANGE (% of an area's size allowed to change hands; defaults 5 / 10). Backups go to <WORK>/clean_out/backup_*.rds.
# Per version: skip if edited since the dry run, unpublished, or failing checks.
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 250, warn = -1); S <- Sys.getenv('WORK'); if (!nzchar(S)) stop('set WORK to a working folder'); APPLY <- identical(Sys.getenv('APPLY'), '1')
MAXH <- as.numeric(Sys.getenv('MAX_HA_CHANGE', '5')); MAXT <- as.numeric(Sys.getenv('MAX_TEAM_CHANGE', '10'))
q <- function(sql, p = list()) if (length(p)) DBI::dbGetQuery(pool, sql, p) else DBI::dbGetQuery(pool, sql)
files <- Sys.glob(file.path(S, 'clean_out', '*.rds')); stamp <- format(Sys.time(), '%Y%m%d_%H%M%S'); out <- list()
for (f in files) { r <- readRDS(f); D <- r$ha_row$district; v <- r$ha$v
  cur <- q("SELECT is_shared, archived_at, last_updated_at, saved_dfa_sf::text s, smoothed_dfa_sf::text m, current_assignments::text ca, dfa_worldpop_pop::text wp FROM mapping_versions WHERE version_id = $1", list(v))
  hr <- r$ha_row; why <- c()
  if (!isTRUE(cur$is_shared) || !is.na(cur$archived_at)) why <- c(why, 'no longer published')
  if (!identical(as.numeric(cur$last_updated_at), as.numeric(r$ha$lu))) why <- c(why, 'edited since dry run')
  if (hr$max_change_pct > MAXH) why <- c(why, sprintf('an area changes %.1f%%', hr$max_change_pct))
  tolH <- max(25, 0.75e-4 * sum(as.numeric(sf::st_area(sf::st_transform(r$ha$sf, 3857)))))
  if (isTRUE(hr$empty_a > 0)) why <- c(why, 'a health area would be empty'); if (!isTRUE(hr$s2_ok) || hr$gap_a >= tolH || hr$spill_a >= tolH || hr$overlap_a >= 25) why <- c(why, 'fails partition check')
  tv_ok <- c(); tv_skip <- c()
  for (tv in names(r$teams)) { t <- r$teams[[tv]]; tr <- r$team_rows[r$team_rows$tv == as.integer(tv), ]
    tc <- q("SELECT is_shared, last_updated_at FROM team_area_versions WHERE team_version_id = $1", list(as.integer(tv)))
    w2 <- c(); if (!isTRUE(tc$is_shared)) w2 <- c(w2, 'unpublished'); if (!identical(as.numeric(tc$last_updated_at), as.numeric(t$lu))) w2 <- c(w2, 'edited')
    tolT <- max(25, 0.75e-4 * sum(as.numeric(sf::st_area(sf::st_transform(t$sf, 3857)))))
    if (isTRUE(tr$empty_a > 0)) w2 <- c(w2, 'a team would be empty'); if (tr$max_change_pct > MAXT) w2 <- c(w2, sprintf('team changes %.1f%%', tr$max_change_pct)); if (!isTRUE(tr$s2_ok) || tr$gap_a >= tolT || tr$spill_a >= tolT || tr$overlap_a >= 25) w2 <- c(w2, 'fails check')
    if (length(why)) w2 <- c(w2, 'health areas not written')
    if (length(w2)) tv_skip <- c(tv_skip, sprintf('%s (%s)', tv, paste(w2, collapse = ', '))) else tv_ok <- c(tv_ok, tv) }
  out[[length(out) + 1]] <- data.frame(district = D, v = v, ha = if (length(why)) paste('SKIP:', paste(why, collapse = '; ')) else 'write', teams_write = length(tv_ok), teams_skip = paste(tv_skip, collapse = ' | '))
  if (!APPLY) next
  bk <- list(ha = cbind(version_id = v, cur[, c('s', 'm', 'ca', 'wp')]),
             teams = if (length(tv_ok)) q(sprintf("SELECT team_version_id, saved_team_sf::text s, smoothed_team_sf::text m, current_team_assignments::text ca, team_worldpop_pop::text wp FROM team_area_versions WHERE team_version_id IN (%s)", paste(tv_ok, collapse = ','))) else NULL)
  saveRDS(bk, file.path(S, 'clean_out', sprintf('backup_%s_%s.rds', gsub('[^A-Za-z0-9]+', '_', D), stamp)))
  if (!length(why)) {
    DBI::dbExecute(pool, "UPDATE mapping_versions SET saved_dfa_sf = CAST($1 AS jsonb), smoothed_dfa_sf = CAST($1 AS jsonb), dfa_worldpop_pop = CAST($2 AS jsonb),
      current_assignments = COALESCE(CAST($3 AS jsonb), current_assignments) WHERE version_id = $4 AND is_shared",
      list(geojsonsf::sf_geojson(r$ha$sf), as.character(jsonlite::toJSON(r$ha$wp, auto_unbox = TRUE)),
           if (is.null(r$ha$cells)) NA_character_ else as.character(jsonlite::toJSON(unname(r$ha$cells))), v)) }
  for (tv in tv_ok) { t <- r$teams[[tv]]
    DBI::dbExecute(pool, "UPDATE team_area_versions SET saved_team_sf = CAST($1 AS jsonb), smoothed_team_sf = CAST($1 AS jsonb), team_worldpop_pop = CAST($2 AS jsonb),
      current_team_assignments = COALESCE(CAST($3 AS jsonb), current_team_assignments) WHERE team_version_id = $4 AND is_shared",
      list(geojsonsf::sf_geojson(t$sf), as.character(jsonlite::toJSON(t$wp, auto_unbox = TRUE)),
           if (is.null(t$cells)) NA_character_ else as.character(jsonlite::toJSON(as.list(t$cells), auto_unbox = TRUE)), as.integer(tv))) } }
O <- do.call(rbind, out); print(O, row.names = FALSE); cat(if (APPLY) 'APPLIED\n' else 'CHECK ONLY\n')
