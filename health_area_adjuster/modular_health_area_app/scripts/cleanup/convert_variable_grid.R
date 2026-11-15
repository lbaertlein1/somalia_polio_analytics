# Move saved health-area versions onto the variable paint grid ("finer grid in towns", helpers/variable_grid.R).
# Boundaries are NOT changed: saved_dfa_sf / smoothed_dfa_sf stay exactly as they are. Only the paint cells are
# re-derived from those saved shapes on the version's automatic variable grid (same spec the app would choose for
# new health areas), and the spec is stored in paint_grid_spec.
# Per version: outline = its In Scope area if it has one, else the district (as the app); the standard grid rebuilt
# from it must have exactly as many cells as the stored painting (else SKIPPED -- outline not what the app used);
# each new cell takes the saved area holding its centre (nearest area if none); an area left with no cell gets the
# cell holding its coordination site. Skips Banadir, archived versions, versions with no saved areas and versions
# already on a variable grid.
# Env: WORK (output folder: summary + backup), CAMPAIGN (default 10), ONLY (comma list of districts), APPLY=1 to write.
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 220, warn = -1); sf::sf_use_s2(FALSE)
S <- Sys.getenv('WORK'); if (!nzchar(S)) stop('set WORK'); dir.create(S, showWarnings = FALSE)
CID <- as.integer(Sys.getenv('CAMPAIGN', '10')); APPLY <- identical(Sys.getenv('APPLY'), '1')
shp <- districts_shp_for_campaign(CID)
banadir <- unique(shp$district_name[grepl('banadir', shp$region_name, ignore.case = TRUE)])
H <- DBI::dbGetQuery(pool, "SELECT version_id v, district_name d, owner_username o, version_number n, is_shared s FROM mapping_versions
  WHERE campaign_id = $1 AND archived_at IS NULL AND saved_dfa_sf IS NOT NULL AND current_assignments IS NOT NULL AND paint_grid_spec IS NULL
  ORDER BY d, version_id", list(CID))
H <- H[!H$d %in% banadir, ]
only <- trimws(strsplit(Sys.getenv('ONLY'), ',')[[1]]); if (length(only) && nzchar(only[1])) H <- H[H$d %in% only, ]
cat(sprintf('%d versions in %d districts (Banadir excluded: %d districts)\n', nrow(H), length(unique(H$d)), length(banadir)))
backup_f <- file.path(S, sprintf('grid_backup_%s.rds', format(Sys.time(), '%Y%m%d_%H%M%S'))); backup <- list()
rows <- list()
for (i in seq_len(nrow(H))) { V <- H$v[i]; t0 <- Sys.time()
  r <- tryCatch({
    v <- db_get_version_by_id(pool, V); D <- v$district_name
    full <- shp[shp$district_name == D, ] |> dplyr::summarise(admin_id = dplyr::first(admin_id), district_name = dplyr::first(district_name),
      region_id = dplyr::first(region_id), region_name = dplyr::first(region_name), zone_id = dplyr::first(zone_id), zone_name = dplyr::first(zone_name),
      geometry = sf::st_union(geometry), .groups = 'drop') |> sf::st_as_sf() |> safe_make_valid()
    full <- tryCatch(sf::st_collection_extract(full, 'POLYGON'), error = function(e) full)
    sc <- v$snap$scope_saved_dfa_sf; insc <- if (!is.null(sc) && 'dfa_name' %in% names(sc)) sc[sc$dfa_name == 'In Scope', ] else NULL
    outline <- if (!is.null(insc) && nrow(insc) > 0 && !all(sf::st_is_empty(insc))) {
      o <- tryCatch(sf::st_collection_extract(safe_make_valid(sf::st_transform(insc, 3857)), 'POLYGON'), error = function(e) safe_make_valid(sf::st_transform(insc, 3857)))
      sf::st_transform(o, 4326) } else sf::st_transform(full, 4326)
    md <- calc_district_max_dim(sf::st_transform(outline, 3857)); gn <- max(10L, as.integer(round(md / max(100, min(500, md / 160)))))
    old <- as.character(unlist(v$snap$current_assignments))
    g0 <- make_paint_grid(outline, grid_n = gn)$grid_sf
    if (nrow(g0) != length(old)) stop(sprintf('standard grid %d cells vs stored %d -- outline not as the app built it', nrow(g0), length(old)))
    fac <- dplyr::bind_rows(facility_sf_to_df(v$snap$odk_sf), facility_sf_to_df(v$snap$app_sf)); fac <- fac[fac$polio_sia_coordination_site %in% 'Yes', , drop = FALSE]
    sites <- if (nrow(fac)) sf::st_as_sf(fac, coords = c('lon', 'lat'), crs = 4326, remove = FALSE) else NULL
    spec <- make_grid_spec(outline, gn, u5_rast, sites)
    if (is.null(spec)) stop('no town cells qualify -- stays on the standard grid')
    g <- build_variable_paint_grid(outline, spec)$grid_sf
    poly <- sf::st_make_valid(sf::st_transform(v$snap$smoothed_dfa_sf %||% v$snap$saved_dfa_sf, 4326))
    pn <- as.character(poly$dfa_name)
    cell_owner <- function(gg) { pts <- sf::st_as_sf(data.frame(x = gg$centroid_lon, y = gg$centroid_lat), coords = c('x', 'y'), crs = 4326)
      k <- sf::st_intersects(pts, poly); o <- vapply(k, function(z) if (length(z)) pn[z[1]] else NA_character_, '')
      if (anyNA(o)) o[is.na(o)] <- pn[sf::st_nearest_feature(pts[is.na(o), ], poly)]
      o }
    new <- cell_owner(g)
    names_all <- setdiff(unique(pn), character())
    for (n in setdiff(names_all, new)) { j <- which(fac$facility_name == n); if (!length(j)) next
      ci <- unlist(sf::st_intersects(sites[j[1], ], sf::st_transform(g, 4326))); if (length(ci) && sum(new == new[ci[1]]) > 1) new[ci[1]] <- n }
    agree_old <- mean(old == cell_owner(g0))            # how well the stored painting matched the saved shapes (standard grid)
    row <- data.frame(version = V, district = D, owner = v$owner_username, v_no = v$version_number, published = v$is_shared,
      cells_before = length(old), cells_after = nrow(g), fine_m = round(spec$fine_m), town_km2 = round(sum(g$size == 1) * spec$fine_m^2 / 1e6, 1),
      areas = length(names_all), areas_no_cells_before = length(setdiff(names_all, old)), areas_no_cells_after = length(setdiff(names_all, new)),
      old_paint_matches_shapes_pct = round(100 * agree_old, 1), status = if (APPLY) 'WRITTEN' else 'dry run', secs = round(as.numeric(difftime(Sys.time(), t0, units = 'secs'))))
    if (APPLY) {
      cur <- DBI::dbGetQuery(pool, 'SELECT current_assignments::text ca, paint_grid_spec::text ps FROM mapping_versions WHERE version_id = $1', list(V))
      backup[[as.character(V)]] <- list(current_assignments = cur$ca, paint_grid_spec = cur$ps); saveRDS(backup, backup_f)
      n_up <- DBI::dbExecute(pool, "UPDATE mapping_versions SET current_assignments = $2::jsonb, paint_grid_spec = $3::jsonb
                                    WHERE version_id = $1 AND paint_grid_spec IS NULL", list(V, as.character(jsonlite::toJSON(new)), grid_spec_json(spec)))
      if (n_up != 1) row$status <- 'NOT WRITTEN (changed meanwhile)'
    }
    row
  }, error = function(e) data.frame(version = V, district = H$d[i], owner = H$o[i], v_no = H$n[i], published = H$s[i], status = paste('SKIPPED:', conditionMessage(e))))
  rows[[length(rows) + 1]] <- r
  cat(sprintf('%-16s v%-4d %-9s %s\n', H$d[i], V, H$o[i], if (!is.null(r$cells_after)) sprintf('%5d -> %5d cells, %3.0f m town cells over %5.1f km2 | areas with no cells %d -> %d | old paint matched shapes %.1f%% | %s %ds',
    r$cells_before, r$cells_after, r$fine_m, r$town_km2, r$areas_no_cells_before, r$areas_no_cells_after, r$old_paint_matches_shapes_pct, r$status, r$secs) else r$status))
}
out <- dplyr::bind_rows(rows); write.csv(out, file.path(S, 'convert_variable_grid_summary.csv'), row.names = FALSE)

# ---- Team layouts (team_area_versions): same idea. Team boundaries (saved/smoothed_team_sf) stay as they are;
# team cells are re-derived on the variable team grid of the health area's saved shape in the pinned health-area
# version (what the Team Areas tab builds on), sized as the tab does for the layout's number of teams.
TH <- DBI::dbGetQuery(pool, "SELECT team_version_id t, district_name d, health_area_name h, owner_username o, version_number n, is_shared s,
    based_on_health_area_version_id hv FROM team_area_versions
  WHERE campaign_id = $1 AND archived_at IS NULL AND saved_team_sf IS NOT NULL AND current_team_assignments IS NOT NULL AND paint_grid_spec IS NULL
  ORDER BY d, h, team_version_id", list(CID))
TH <- TH[!TH$d %in% banadir, ]; if (length(only) && nzchar(only[1])) TH <- TH[TH$d %in% only, ]
cat(sprintf('\nTEAM LAYOUTS: %d in %d districts\n', nrow(TH), length(unique(TH$d))))
ha_cache <- list(); trows <- list()
for (i in seq_len(nrow(TH))) { T_ <- TH$t[i]; t0 <- Sys.time()
  r <- tryCatch({
    hk <- as.character(TH$hv[i])
    if (is.null(ha_cache[[hk]])) ha_cache[[hk]] <- db_get_version_by_id(pool, TH$hv[i])$snap$saved_dfa_sf
    hsf <- ha_cache[[hk]]; if (is.null(hsf)) stop('pinned health-area version has no saved shapes')
    ha <- hsf[hsf$dfa_name == TH$h[i], , drop = FALSE]; if (!nrow(ha)) stop('health area not in its pinned version')
    ha <- sf::st_transform(safe_make_valid(ha), 4326)
    tv <- db_get_team_area_version_by_id(pool, T_)
    shapes <- tv$snap$smoothed_team_sf %||% tv$snap$saved_team_sf
    teams <- setdiff(unique(as.character(shapes$dfa_name)), extra_dfa_names)
    # team grid size exactly as the Team Areas tab (team_grid_n), for this layout's number of teams
    h3 <- sf::st_transform(ha, 3857); bb <- sf::st_bbox(h3); max_dim <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin)
    cs <- max(100, min(500, max_dim / 160)); cs <- max(40, min(cs, sqrt(as.numeric(sum(sf::st_area(h3))) / (max(1L, length(teams)) * 20))))
    gn <- max(20L, as.integer(round(max_dim / cs)))
    old <- tv$snap$current_team_assignments
    spec <- make_grid_spec(ha, gn, u5_rast, NULL, fine_m = 40)
    if (is.null(spec)) stop('no town cells qualify -- stays on the standard grid')
    g <- build_variable_paint_grid(ha, spec)$grid_sf
    new <- team_assignments_from_shapes(g, shapes)
    row <- data.frame(team_version = T_, district = TH$d[i], health_area = TH$h[i], owner = TH$o[i], v_no = TH$n[i], published = TH$s[i],
      teams = length(teams), cells_before = length(old), cells_after = nrow(g), fine_m = round(spec$fine_m),
      teams_no_cells_after = length(setdiff(teams, new)), status = if (APPLY) 'WRITTEN' else 'dry run', secs = round(as.numeric(difftime(Sys.time(), t0, units = 'secs'))))
    if (APPLY) {
      cur <- DBI::dbGetQuery(pool, 'SELECT current_team_assignments::text ca, paint_grid_spec::text ps FROM team_area_versions WHERE team_version_id = $1', list(T_))
      backup[[paste0('team_', T_)]] <- list(current_team_assignments = cur$ca, paint_grid_spec = cur$ps); saveRDS(backup, backup_f)
      named <- setNames(as.list(new), as.character(g$cell_id))   # team format: keyed by cell_id
      n_up <- DBI::dbExecute(pool, "UPDATE team_area_versions SET current_team_assignments = $2::jsonb, paint_grid_spec = $3::jsonb
                                    WHERE team_version_id = $1 AND paint_grid_spec IS NULL",
                             list(T_, as.character(jsonlite::toJSON(named, auto_unbox = TRUE)), grid_spec_json(spec)))
      if (n_up != 1) row$status <- 'NOT WRITTEN (changed meanwhile)'
    }
    row
  }, error = function(e) data.frame(team_version = T_, district = TH$d[i], health_area = TH$h[i], owner = TH$o[i], v_no = TH$n[i], published = TH$s[i],
                                    status = paste('SKIPPED:', conditionMessage(e))))
  trows[[length(trows) + 1]] <- r
  cat(sprintf('%-14s %-34s t%-4d %s\n', TH$d[i], substr(TH$h[i], 1, 34), T_, if (!is.null(r$cells_after)) sprintf('%2d teams | %5d -> %5d cells, %2.0f m fine | teams with no cells after %d | %s %ds',
    r$teams, r$cells_before, r$cells_after, r$fine_m, r$teams_no_cells_after, r$status, r$secs) else r$status))
}
if (length(trows)) write.csv(dplyr::bind_rows(trows), file.path(S, 'convert_variable_grid_teams_summary.csv'), row.names = FALSE)
cat('DONE\n')
