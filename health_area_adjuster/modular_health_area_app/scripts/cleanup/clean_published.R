# DRY RUN for all PUBLISHED campaign-10 health areas + their published team layouts. Writes nothing to the DB (apply_cleaned.R writes).
# Env: WORK (output folder), ONLY (comma list of districts), SNAP=1 (1 cm grid snap first), ROUTE_DIR (use street_follow.R proposals as the health areas;
# team layouts are then re-fitted to the new health-area edges), KEEP_CURRENT (with ROUTE_DIR: health areas that keep their published shape),
# SPLIT_GAPS (m2: team gap pieces larger than this are split between the nearest teams instead of going to one).
# Per district: <S>/clean_out/<district>.rds (cleaned geometries, cells, WorldPop, logs, anomalies, version timestamps for the apply step).
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 250, warn = -1); S <- Sys.getenv('WORK'); if (!nzchar(S)) stop('set WORK to a working folder'); source('scripts/cleanup/clean_lib.R')
OUT <- file.path(S, 'clean_out'); dir.create(OUT, showWarnings = FALSE)
q <- function(sql, p = list()) if (length(p)) DBI::dbGetQuery(pool, sql, p) else DBI::dbGetQuery(pool, sql)
shp <- districts_shp_for_campaign(10L)
team_grid_n <- function(ha, n_teams) { h <- sf::st_transform(ha, 3857); bb <- sf::st_bbox(h); md <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin)
  cs <- max(100, min(500, md / 160)); cs <- max(40, min(cs, sqrt(as.numeric(sum(sf::st_area(h))) / (max(1L, n_teams) * 20)))); max(20L, as.integer(round(as.numeric(md) / cs))) }
contain <- function(pts, shapes) { k <- sf::st_intersects(pts, shapes); vapply(k, function(z) if (length(z)) as.character(shapes$dfa_name[z[1]]) else NA_character_, '') }
read_sf_txt <- function(txt) if (is.na(txt)) NULL else geojsonsf::geojson_sf(txt)
only <- trimws(strsplit(Sys.getenv('ONLY'), ',')[[1]])
H <- q("SELECT version_id v, district_name d, last_updated_at lu, COALESCE(smoothed_dfa_sf, saved_dfa_sf)::text g, dfa_names::text dn, current_assignments::text ca,
        scope_saved_dfa_sf::text sc, odk_sf::text o, app_sf::text a, team_targets::text tt FROM mapping_versions
        WHERE campaign_id = 10 AND is_shared AND archived_at IS NULL AND saved_dfa_sf IS NOT NULL ORDER BY d")
if (length(only) && nzchar(only[1])) H <- H[H$d %in% only, ]
for (i in seq_len(nrow(H))) {
  D <- H$d[i]; t0 <- Sys.time(); an <- list(); AN <- function(level, what, detail) an[[length(an) + 1]] <<- data.frame(district = D, level = level, what = what, detail = detail)
  res <- tryCatch({
    ha <- geojsonsf::geojson_sf(H$g[i]); ha <- sf::st_make_valid(ha); crs <- utm_for(ha)
    dist <- shp[shp$district_name == D, ]; outline <- U(polys(sf::st_union(sf::st_transform(sf::st_make_valid(dist), crs)), crs), crs)
    dn <- unlist(jsonlite::fromJSON(H$dn[i])); gn <- unique(as.character(ha$dfa_name))
    for (x in setdiff(setdiff(dn, extra_dfa_names), gn)) AN('health area', 'listed but has no shape', x)
    for (x in setdiff(gn, dn)) AN('health area', 'shape not in the name list', x)
    attrs <- sf::st_drop_geometry(ha); attrs <- attrs[!duplicated(attrs$dfa_name), , drop = FALSE]
    areas <- setNames(lapply(gn, function(n) U(polys(sf::st_union(sf::st_transform(ha[ha$dfa_name == n, ], crs)), crs), crs)), gn)
    if (identical(Sys.getenv('SNAP'), '1')) { areas <- lapply(areas, function(g) U(polys(lwgeom::st_snap_to_grid(g, 0.01), crs), crs)); outline <- U(polys(lwgeom::st_snap_to_grid(outline, 0.01), crs), crs) }
    for (n in gn) if (empty_g(areas[[n]])) AN('health area', 'no area (empty shape)', n)
    areas0 <- areas
    # ROUTE_DIR: start from the street-following proposal (street_follow.R) instead of the published shapes
    RD <- Sys.getenv('ROUTE_DIR'); if (nzchar(RD)) { rf <- file.path(RD, paste0(gsub('[^A-Za-z0-9]+', '_', D), '.rds')); if (!file.exists(rf)) stop('no street-following proposal for ', D)
      rr <- readRDS(rf); rc <- sf::st_transform(rr$cur, crs); rp <- sf::st_transform(rr$prop, crs)
      drift <- sum(vapply(gn, function(n) A(polys(sf::st_sym_difference(areas[[n]], U(sf::st_geometry(rc)[rc$dfa_name == n], crs)), crs)), 0))
      if (drift > 1) stop(sprintf('published health areas changed since the proposal was made (%.0f m2)', drift))
      if (!setequal(rp$dfa_name, gn)) stop('proposal has different health-area names')
      areas <- setNames(lapply(gn, function(n) U(polys(sf::st_geometry(rp)[rp$dfa_name == n], crs), crs)), gn)
      # KEEP_CURRENT: health areas (comma list) that keep their published shape; neighbours give the land back
      for (n in intersect(trimws(strsplit(Sys.getenv('KEEP_CURRENT'), ',')[[1]]), gn)) {
        for (o in setdiff(gn, n)) areas[[o]] <- U(polys(sf::st_difference(areas[[o]], areas0[[n]]), crs), crs); areas[[n]] <- areas0[[n]] } }
    cl <- clean_partition(areas, outline, extra = extra_dfa_names, kind = 'ha')
    new <- sf::st_sf(dfa_name = gn, geometry = finalize_partition(cl$areas, crs))
    new <- merge(new, attrs[, setdiff(names(attrs), 'geometry'), drop = FALSE], by = 'dfa_name', all.x = TRUE, sort = FALSE); new <- new[match(gn, new$dfa_name), ]
    m_b <- partition_metrics(sf::st_transform(sf::st_sf(dfa_name = gn, geometry = do.call(c, areas)), 4326), outline, crs); m_a <- partition_metrics(new, outline, crs)
    newu <- sf::st_transform(new, crs); real <- setdiff(gn, extra_dfa_names); ra <- as.numeric(sf::st_area(newu)); names(ra) <- gn
    med <- median(ra[real]); for (n in real) { if (ra[n] < 0.02 * med) AN('health area', 'very small (<2% of district median)', sprintf('%s %.3f km2', n, ra[n] / 1e6))
      np <- length(polys(sf::st_geometry(newu)[gn == n], crs)); if (np > 1) AN('health area', 'more than one piece after cleaning (exclave touching no other area, or large detached part)', sprintf('%s: %d pieces', n, np)) }
    # coordination sites vs health areas
    o <- read_sf_txt(H$o[i]); a <- read_sf_txt(H$a[i]); st <- rbind(if (!is.null(o)) o[o$polio_sia_coordination_site %in% 'Yes', 'facility_name'], if (!is.null(a)) a[a$polio_sia_coordination_site %in% 'Yes', 'facility_name'])
    if (!is.null(st) && nrow(st)) { for (x in setdiff(st$facility_name, gn)) AN('health area', 'selected coordination site has no health area of that name', x)
      for (x in setdiff(real, st$facility_name)) AN('health area', 'health area with no selected coordination site of that name', x)
      inn <- contain(sf::st_transform(st, 4326), new); bad <- which(!is.na(match(st$facility_name, gn)) & (is.na(inn) | inn != st$facility_name))
      for (b in bad) AN('health area', 'coordination site lies outside its own health area', sprintf('%s (inside: %s)', st$facility_name[b], inn[b] %||% 'none')) }
    if (!is.null(cl$log)) for (r in which(grepl('^FLAG', cl$log$type))) AN('health area', cl$log$type[r], sprintf('%s %.0f m2', cl$log$area[r], cl$log$m2[r]))
    # HA cells (same grid rule as the Health Areas tab)
    ha_cells <- NULL; ha_cells_note <- 'not re-derived'
    tryCatch({ canvas <- NULL
      if (!is.na(H$sc[i])) { sc <- geojsonsf::geojson_sf(H$sc[i]); ins <- sc[sc$dfa_name == 'In Scope', ]; if (nrow(ins) && !all(sf::st_is_empty(ins))) canvas <- sf::st_transform(safe_make_valid(ins), 4326) }
      if (is.null(canvas)) { canvas <- dist |> dplyr::summarise(geometry = sf::st_union(geometry)) |> sf::st_as_sf() |> safe_make_valid(); canvas <- tryCatch(sf::st_collection_extract(canvas, 'POLYGON'), error = function(e) canvas) }
      c3857 <- tryCatch(sf::st_collection_extract(safe_make_valid(sf::st_transform(canvas, 3857)), 'POLYGON'), error = function(e) sf::st_transform(canvas, 3857))
      md <- calc_district_max_dim(c3857); gnn <- max(10L, as.integer(round(md / max(100, min(500, md / 160)))))
      grid <- make_paint_grid(district_sf = sf::st_transform(c3857, 4326), grid_n = gnn)$grid_sf; old <- unlist(jsonlite::fromJSON(H$ca[i]))
      if (length(old) == nrow(grid)) { pts <- sf::st_as_sf(data.frame(x = grid$centroid_lon, y = grid$centroid_lat), coords = c('x', 'y'), crs = 4326)
        sp <- contain(pts, new); ha_cells <- ifelse(is.na(sp), old, sp); ha_cells_note <- sprintf('re-derived (%d of %d changed)', sum(ha_cells != old), length(old)) }
      else ha_cells_note <- sprintf('kept (grid %d vs stored %d)', nrow(grid), length(old)) }, error = function(e) ha_cells_note <<- paste('kept:', conditionMessage(e)))
    ha_wp <- setNames(lapply(real, function(n) sum(.polygon_u5_population(new[new$dfa_name == n, ], u5_rast), na.rm = TRUE)), real)
    dpct <- change_pct(areas0, setNames(lapply(gn, function(n) U(polys(sf::st_geometry(newu)[gn == n], crs), crs)), gn)); worst <- which.max(dpct)
    ha_row <- data.frame(district = D, v = H$v[i], areas = length(gn), gap_b = round(m_b['gap']), spill_b = round(m_b['spill']), overlap_b = round(m_b['overlap']), pieces_b = m_b['pieces'],
      gap_a = round(m_a['gap'], 1), spill_a = round(m_a['spill'], 1), overlap_a = round(m_a['overlap'], 1), pieces_a = m_a['pieces'], s2_ok = as.logical(m_a['s2_valid']),
      max_change_pct = round(max(dpct), 2), max_change_area = gn[worst], empty_a = m_a['empty'], fixes = if (is.null(cl$log)) 0 else sum(!grepl('^FLAG|moved', cl$log$type)), cells = ha_cells_note)
    # ---- team layouts pinned to this version
    TV <- q("SELECT team_version_id tv, health_area_name h, last_updated_at lu, COALESCE(smoothed_team_sf, saved_team_sf)::text ts, team_names::text tn, current_team_assignments::text ca
             FROM team_area_versions WHERE campaign_id = 10 AND is_shared AND archived_at IS NULL AND based_on_health_area_version_id = $1 ORDER BY 2", list(H$v[i]))
    tt <- if (!is.na(H$tt[i])) jsonlite::fromJSON(H$tt[i], simplifyVector = FALSE) else list()
    stale <- q("SELECT count(*) n FROM team_area_versions WHERE campaign_id = 10 AND is_shared AND archived_at IS NULL AND district_name = $1 AND based_on_health_area_version_id <> $2", list(D, H$v[i]))$n
    if (stale > 0) AN('team area', 'published team layout pinned to an older health-area version', paste(stale, 'layout(s)'))
    teams <- list(); team_rows <- list()
    for (k in seq_len(nrow(TV))) { h <- TV$h[k]
      tr <- tryCatch({
        if (!h %in% gn) { AN('team area', 'layout for a health area that is not in the published version', h); NULL } else {
        hb <- U(polys(sf::st_geometry(newu)[gn == h], crs), crs); ts <- sf::st_make_valid(geojsonsf::geojson_sf(TV$ts[k])); tnm <- unique(as.character(ts$dfa_name))
        tn <- tryCatch(unlist(jsonlite::fromJSON(TV$tn[k])), error = function(e) tnm)
        for (x in setdiff(tn, tnm)) AN('team area', 'team listed but has no shape', paste(h, '-', x))
        tl <- setNames(lapply(tnm, function(n) U(polys(sf::st_union(sf::st_transform(ts[ts$dfa_name == n, ], crs)), crs), crs)), tnm)
        for (n in tnm) if (empty_g(tl[[n]])) AN('team area', 'team with no area (empty shape)', paste(h, '-', n))
        req <- suppressWarnings(as.numeric(tt[[h]]$requested_teams %||% NA)); if (!is.na(req) && req != length(tnm)) AN('team area', 'team count differs from Field Requested Teams', sprintf('%s: %d teams, %g requested', h, length(tnm), req))
        tc <- clean_partition(tl, hb, kind = 'team', split_gaps = as.numeric(Sys.getenv('SPLIT_GAPS', '0')))
        tnew <- sf::st_sf(dfa_name = tnm, geometry = finalize_partition(tc$areas, crs))
        mb <- partition_metrics(sf::st_transform(sf::st_sf(dfa_name = tnm, geometry = do.call(c, tl)), 4326), hb, crs); ma <- partition_metrics(tnew, hb, crs)
        tu <- sf::st_transform(tnew, crs); ta <- as.numeric(sf::st_area(tu)); names(ta) <- tnm; tmed <- median(ta)
        for (n in tnm) { if (ta[n] < 0.15 * tmed) AN('team area', 'very small team (<15% of the layout median)', sprintf('%s - %s %.0f m2', h, n, ta[n]))
          np <- length(polys(sf::st_geometry(tu)[tnm == n], crs)); if (np > 1) AN('team area', 'team in more than one piece after cleaning', sprintf('%s - %s: %d pieces', h, n, np)) }
        if (!is.null(tc$log)) for (r in which(grepl('^FLAG', tc$log$type))) AN('team area', tc$log$type[r], sprintf('%s - %s %.0f m2', h, tc$log$area[r], tc$log$m2[r]))
        cells <- NULL; cnote <- 'kept'
        if (!is.na(req) && req >= 1) { ha4 <- sf::st_sf(geometry = sf::st_transform(hb, 4326)); grid <- make_paint_grid(district_sf = ha4, grid_n = team_grid_n(ha4, req))$grid_sf
          old <- jsonlite::fromJSON(TV$ca[k], simplifyVector = FALSE)
          cells <- team_assignments_from_shapes(grid, tnew); names(cells) <- as.character(grid$cell_id)
          cnote <- if (length(old) == nrow(grid)) 're-derived' else sprintf('re-derived on new grid (%d cells, was %d)', nrow(grid), length(old)) }
        twp <- setNames(lapply(tnm, function(n) sum(.polygon_u5_population(tnew[tnew$dfa_name == n, ], u5_rast), na.rm = TRUE)), tnm)
        tp <- change_pct(tl, setNames(lapply(tnm, function(n) U(polys(sf::st_geometry(tu)[tnm == n], crs), crs)), tnm))
        teams[[as.character(TV$tv[k])]] <- list(sf = tnew, wp = twp, cells = cells, lu = TV$lu[k], log = tc$log)
        data.frame(district = D, tv = TV$tv[k], ha = substr(h, 1, 30), teams = length(tnm), gap_b = round(mb['gap']), spill_b = round(mb['spill']), overlap_b = round(mb['overlap']), pieces_b = mb['pieces'],
          gap_a = round(ma['gap'], 1), spill_a = round(ma['spill'], 1), overlap_a = round(ma['overlap'], 1), pieces_a = ma['pieces'], s2_ok = as.logical(ma['s2_valid']),
          max_change_pct = round(max(tp), 2), empty_a = ma['empty'], fixes = if (is.null(tc$log)) 0 else sum(!grepl('^FLAG|moved', tc$log$type)), cells = cnote) } },
        error = function(e) { AN('team area', 'cleaning error', paste(h, conditionMessage(e))); NULL })
      if (!is.null(tr)) team_rows[[length(team_rows) + 1]] <- tr }
    list(ha = list(v = H$v[i], lu = H$lu[i], sf = new, cells = ha_cells, wp = ha_wp, log = cl$log), ha_row = ha_row, teams = teams,
         team_rows = if (length(team_rows)) do.call(rbind, team_rows) else NULL, anomalies = if (length(an)) do.call(rbind, an) else NULL, before_sf = ha)
  }, error = function(e) { cat('ERROR', D, conditionMessage(e), '\n'); NULL })
  if (!is.null(res)) { saveRDS(res, file.path(OUT, paste0(gsub('[^A-Za-z0-9]+', '_', D), '.rds')))
    cat(sprintf('%-14s HA fixes %3d | max change %5.2f%% | teams %2d | anomalies %2d | %.0fs\n', D, res$ha_row$fixes, res$ha_row$max_change_pct, length(res$teams), NROW(res$anomalies), as.numeric(difftime(Sys.time(), t0, units = 'secs')))) }
}
cat('DONE\n')
