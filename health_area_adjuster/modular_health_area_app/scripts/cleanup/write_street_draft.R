# Write street_follow.R proposals made with VERSIONS=... (<WORK>/route_out/<district>_v<id>.rds) back into those UNPUBLISHED drafts.
# Per draft: skip if published or edited since the proposal; coordination sites end inside their own area (25 m disk, as generate_two_level.R);
# partition + sphere (s2) checks; cells re-derived on the draft's own paint grid; WorldPop recomputed. Env: WORK, APPLY=1 to write (else check only).
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 220, warn = -1); source('scripts/cleanup/clean_lib.R')
S <- Sys.getenv('WORK'); APPLY <- identical(Sys.getenv('APPLY'), '1'); shp <- districts_shp_for_campaign(10L)
for (f in Sys.glob(file.path(S, 'route_out', '*_v*.rds'))) { r <- readRDS(f); V <- r$v
  res <- tryCatch({
    v <- db_get_version_by_id(pool, V); D <- v$district_name
    cur <- DBI::dbGetQuery(pool, 'SELECT is_shared, last_updated_at lu, current_assignments::text ca, dfa_names::text dn FROM mapping_versions WHERE version_id = $1', list(V))
    if (isTRUE(cur$is_shared)) stop('published -- not written'); if (!isTRUE(all.equal(as.numeric(cur$lu), as.numeric(r$lu)))) stop('edited since the proposal -- rerun street_follow.R')
    new <- r$prop; gn <- as.character(new$dfa_name); crs <- utm_for(new)
    d4 <- sf::st_transform(safe_make_valid(shp[shp$district_name == D, ]), 4326); outline <- U(polys(sf::st_union(sf::st_transform(d4, crs)), crs), crs)
    fac <- dplyr::bind_rows(facility_sf_to_df(v$snap$odk_sf), facility_sf_to_df(v$snap$app_sf)); fac <- fac[fac$polio_sia_coordination_site %in% 'Yes', , drop = FALSE]
    sites <- sf::st_as_sf(fac, coords = c('lon', 'lat'), crs = 4326); sxy <- sf::st_transform(sites, crs)
    areas <- setNames(lapply(gn, function(n) U(polys(sf::st_geometry(sf::st_transform(new, crs))[gn == n], crs), crs)), gn)
    dd <- matrix(as.numeric(sf::st_distance(sxy)), nrow(sxy)); diag(dd) <- Inf; fixed <- character()
    for (j in seq_len(nrow(sxy))) { n <- sxy$facility_name[j]; if (!n %in% gn || lengths(sf::st_intersects(sf::st_geometry(sxy)[j], areas[[n]]))) next
      disk <- sf::st_intersection(sf::st_buffer(sf::st_geometry(sxy)[j], min(25, 0.45 * min(dd[j, ]))), outline); fixed <- c(fixed, n)
      for (o in setdiff(gn, n)) areas[[o]] <- U(polys(sf::st_difference(areas[[o]], disk), crs), crs); areas[[n]] <- U(polys(sf::st_union(areas[[n]], disk), crs), crs) }
    new <- sf::st_sf(dfa_name = gn, geometry = finalize_partition(lapply(areas, function(g) U(polys(sf::st_segmentize(g, 100), crs), crs)), crs))
    m <- partition_metrics(new, outline, crs)
    inn <- sf::st_intersects(sf::st_transform(sites, 4326), new)
    out_site <- sites$facility_name[!vapply(seq_len(nrow(sites)), function(j) length(inn[[j]]) && new$dfa_name[inn[[j]][1]] == sites$facility_name[j], TRUE) & sites$facility_name %in% gn]
    if (length(out_site) || m['overlap'] >= 25 || m['s2_valid'] != 1 || m['empty'] != 0) stop(sprintf('checks failed (sites outside: %s; overlap %.1f; s2 %d; empty %d)', paste(out_site, collapse = ', '), m['overlap'], m['s2_valid'], m['empty']))
    # cells on the draft's own paint grid (same size as stored)
    old <- unlist(jsonlite::fromJSON(cur$ca)); dsf <- d4 |> dplyr::summarise(geometry = sf::st_union(geometry)) |> sf::st_as_sf()
    md <- calc_district_max_dim(sf::st_transform(dsf, 3857)); gnn <- max(10L, as.integer(round(md / max(100, min(500, md / 160)))))
    grid <- make_paint_grid(district_sf = dsf, grid_n = gnn)$grid_sf; if (nrow(grid) != length(old)) stop(sprintf('paint grid %d cells vs stored %d', nrow(grid), length(old)))
    pts <- sf::st_as_sf(data.frame(x = grid$centroid_lon, y = grid$centroid_lat), coords = c('x', 'y'), crs = 4326); k_ <- sf::st_intersects(pts, new)
    cells <- vapply(k_, function(z) if (length(z)) as.character(new$dfa_name[z[1]]) else NA_character_, ''); cells[is.na(cells)] <- old[is.na(cells)]
    gpoly <- sf::st_transform(grid, 4326)
    for (n in setdiff(gn, cells)) { j <- which(sites$facility_name == n); if (!length(j)) next; ci <- unlist(sf::st_intersects(sites[j, ], gpoly)); if (length(ci) && sum(cells == cells[ci[1]]) > 1) cells[ci[1]] <- n }
    wp <- setNames(lapply(gn, function(n) sum(.polygon_u5_population(new[new$dfa_name == n, ], u5_rast), na.rm = TRUE)), gn)
    if (APPLY) db_submit_stage_v2(pool, V, v$owner_username, 'areas', list(saved_dfa_sf = new, dfa_names = unlist(jsonlite::fromJSON(cur$dn)), current_assignments = cells,
      smoothed_dfa_sf = new, team_targets = v$snap$team_targets, dfa_worldpop_pop = wp))
    sprintf('%s v%d | on-road %.0f%% -> %.0f%% | moved %.2f%% of district | max WP change %.0f%% | site disks %d | gap %.0f m2 overlap %.1f | cells changed %d | %s', D, V,
      r$row$on_road_now, r$row$on_road_prop, r$row$area_moved_pct, r$row$max_wp_change_pct, length(fixed), m['gap'], m['overlap'], sum(cells != old), if (APPLY) 'WRITTEN' else 'check only')
  }, error = function(e) sprintf('v%d SKIPPED: %s', V, conditionMessage(e)))
  cat(res, '\n') }
