# READ-ONLY: street-following proposal for every PUBLISHED campaign-10 team layout. Nothing written to the DB.
# Inside each health area, every boundary between two teams is re-routed along the cheapest OSM street route (same method as street_follow.R);
# the health-area edge is the fixed outline. A layout's proposal is ACCEPTED only if it passes the guards below; otherwise the layout is left as it is.
# Env: WORK, ONLY (comma list of districts), MIN_GAIN (on-road points, default 5), MAX_TEAM_CHANGE (% of a team's area, default 40),
#      MAX_WP_CHANGE (% of a team's WorldPop, default 25), MAX_LEN_GROWTH (boundary length ratio, default 1.3).
# Output: <WORK>/team_route_out/<team_version_id>.rds (accepted proposals only; use as TEAM_ROUTE_DIR in clean_published.R),
#         <WORK>/route_out/<district>.rds (all layouts, for street_follow_review_html.R with UNIT=team), <WORK>/team_route_summary.csv
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 250, warn = -1); S <- Sys.getenv('WORK'); if (!nzchar(S)) stop('set WORK to a working folder')
source('scripts/cleanup/clean_lib.R'); source('scripts/cleanup/street_lib.R')
TOUT <- file.path(S, 'team_route_out'); ROUT <- file.path(S, 'route_out'); dir.create(TOUT, showWarnings = FALSE); dir.create(ROUT, showWarnings = FALSE)
MIN_GAIN <- as.numeric(Sys.getenv('MIN_GAIN', '5')); MAX_CH <- as.numeric(Sys.getenv('MAX_TEAM_CHANGE', '40')); MAX_WP <- as.numeric(Sys.getenv('MAX_WP_CHANGE', '25')); MAX_LEN <- as.numeric(Sys.getenv('MAX_LEN_GROWTH', '1.3'))
shp <- districts_shp_for_campaign(10L)
q <- function(sql, p = list()) if (length(p)) DBI::dbGetQuery(pool, sql, p) else DBI::dbGetQuery(pool, sql)
TV <- q("SELECT t.team_version_id tv, t.district_name d, t.health_area_name h, t.based_on_health_area_version_id hv, COALESCE(t.smoothed_team_sf, t.saved_team_sf)::text ts,
         COALESCE(mv.smoothed_dfa_sf, mv.saved_dfa_sf)::text hs, mv.is_shared ha_pub FROM team_area_versions t JOIN mapping_versions mv ON mv.version_id = t.based_on_health_area_version_id
         WHERE t.campaign_id = 10 AND t.is_shared AND t.archived_at IS NULL ORDER BY 2, 3")
only <- trimws(strsplit(Sys.getenv('ONLY'), ',')[[1]]); if (length(only) && nzchar(only[1])) TV <- TV[TV$d %in% only, ]
wp_of <- function(sf4, n) sum(.polygon_u5_population(sf4[sf4$dfa_name == n, ], u5_rast), na.rm = TRUE)
summ <- list()
for (D in unique(TV$d)) { t0 <- Sys.time(); T <- TV[TV$d == D, ]
  ha_all <- sf::st_make_valid(geojsonsf::geojson_sf(T$hs[1])); crs <- utm_for(ha_all)
  roads_d <- load_roads(U(polys(sf::st_union(sf::st_transform(ha_all, crs)), crs), crs), crs, district = shp[shp$district_name == D, ][1, ])
  cur_all <- list(); prop_all <- list(); wp_all <- list(); rows <- list()
  for (k in seq_len(nrow(T))) { h <- T$h[k]
    r <- tryCatch({
      if (!isTRUE(T$ha_pub[k])) stop('health-area version not published')
      ha <- sf::st_make_valid(geojsonsf::geojson_sf(T$hs[k])); hb <- U(polys(sf::st_union(sf::st_transform(ha[ha$dfa_name == h, ], crs)), crs), crs); if (empty_g(hb)) stop('health area not in its version')
      ts <- sf::st_make_valid(geojsonsf::geojson_sf(T$ts[k])); tnm <- unique(as.character(ts$dfa_name))
      tl <- setNames(lapply(tnm, function(n) U(polys(sf::st_union(sf::st_transform(ts[ts$dfa_name == n, ], crs)), crs), crs)), tnm)
      start <- clean_partition(tl, hb, kind = 'team')$areas                       # exact partition of the health area to route from
      roads <- roads_d[lengths(sf::st_intersects(roads_d, sf::st_buffer(hb, 300))) > 0, ]; roads_u <- if (nrow(roads)) sf::st_union(roads) else NULL
      cur4 <- sf::st_transform(sf::st_sf(dfa_name = tnm, geometry = do.call(c, tl)), 4326); rc <- on_road_share(do.call(c, tl), roads_u)
      wc <- vapply(tnm, function(n) wp_of(cur4, n), 0); np0 <- vapply(tl, function(g) length(polys(g, crs)), 1L); pc <- sum(np0)
      # one candidate: route with `fixed` teams kept, finalize, measure, list guard failures
      cand <- function(fixed) { rt <- route_partition(start, hb, roads, cw_min = 40, cw_max = 200, snap_max = 100, kind = 'team', fixed = fixed)
        prop <- sf::st_sf(dfa_name = tnm, geometry = finalize_partition(rt$areas, crs)); propu <- sf::st_transform(prop, crs)
        npp <- vapply(seq_along(tnm), function(j) length(polys(sf::st_geometry(propu)[j], crs)), 1L); pp <- sum(npp)
        rp <- on_road_share(sf::st_geometry(propu), roads_u); ch <- change_pct(tl, setNames(lapply(tnm, function(n) U(polys(sf::st_geometry(propu)[tnm == n], crs), crs)), tnm))
        wpp <- vapply(tnm, function(n) wp_of(prop, n), 0); wpch <- 100 * abs(wpp - wc) / pmax(wc, 50)   # % of the team's WorldPop (floor 50 U5 so tiny teams don't dominate)
        why <- c(if (length(tnm) < 2) 'one team', if (rp['pct'] - rc['pct'] < MIN_GAIN) sprintf('gain %.1f pts', rp['pct'] - rc['pct']),
                 if (rc['km'] > 0 && rp['km'] > MAX_LEN * rc['km']) sprintf('boundary %.1f -> %.1f km', rc['km'], rp['km']), if (max(ch) > MAX_CH) sprintf('team change %.0f%%', max(ch)),
                 if (max(wpch) > MAX_WP) sprintf('team WorldPop change %.0f%%', max(wpch)), if (pp > pc) sprintf('pieces %d -> %d', pc, pp), if (length(rt$lost)) 'a team would vanish')
        list(rt = rt, prop = prop, propu = propu, rp = rp, ch = ch, wpp = wpp, wpch = wpch, why = why, fixed = fixed, bad = union(rt$lost, tnm[npp > np0])) }
      # teams that would vanish or split keep their current boundaries and the rest are re-routed (up to 4 rounds); first candidate passing all guards wins
      cd <- cand(character()); first <- cd
      while (length(cd$why) && length(setdiff(cd$bad, cd$fixed)) && length(cd$fixed) < 8 && length(union(cd$fixed, cd$bad)) < length(tnm) - 1) cd <- cand(union(cd$fixed, cd$bad))
      if (length(cd$why)) cd <- first
      rt <- cd$rt; prop <- cd$prop; propu <- cd$propu; rp <- cd$rp; ch <- cd$ch; wpp <- cd$wpp; wpch <- cd$wpch; why <- cd$why; fixed <- cd$fixed
      acc <- !length(why)
      if (acc) saveRDS(list(tv = T$tv[k], d = D, h = h, hv = T$hv[k], cur = cur4, prop = prop), file.path(TOUT, paste0(T$tv[k], '.rds')))
      lab <- function(x) paste0(h, ': ', sub('.* - ', '', x))
      cur_all[[k]] <- sf::st_sf(dfa_name = lab(tnm), geometry = sf::st_geometry(cur4)); prop_all[[k]] <- sf::st_sf(dfa_name = lab(tnm), geometry = if (acc) sf::st_geometry(prop) else sf::st_geometry(cur4))
      wp_all[[k]] <- data.frame(health_area = lab(tnm), worldpop_now = round(wc), worldpop_proposal = round(if (acc) wpp else wc))
      data.frame(district = D, tv = T$tv[k], health_area = h, teams = length(tnm), routed = rt$routed, kept = rt$kept, km_now = round(rc['km'], 2), km_prop = round(rp['km'], 2),
        on_road_now = round(rc['pct'], 1), on_road_prop = round(rp['pct'], 1), max_team_change = round(max(ch), 1), max_team_wp_change = round(max(wpch), 1),
        moved_m2 = round(sum(vapply(seq_along(tnm), function(j) A(polys(sf::st_difference(sf::st_geometry(propu)[j], tl[[j]]), crs)), 0))), kept_teams = length(fixed), accepted = acc, why_not = paste(why, collapse = '; '))
    }, error = function(e) data.frame(district = D, tv = T$tv[k], health_area = h, teams = NA, routed = NA, kept = NA, km_now = NA, km_prop = NA, on_road_now = NA, on_road_prop = NA,
        max_team_change = NA, max_team_wp_change = NA, moved_m2 = NA, kept_teams = NA, accepted = FALSE, why_not = paste('ERROR', conditionMessage(e))))
    rows[[k]] <- r }
  R <- do.call(rbind, rows); summ[[D]] <- R
  # district review file (same shape as street_follow.R output; layouts not accepted show no change)
  ok <- !vapply(cur_all, is.null, TRUE)
  if (any(ok)) { cu <- do.call(rbind, cur_all[ok]); pr <- do.call(rbind, prop_all[ok]); A_ <- R$accepted %in% TRUE
    roads_u <- if (nrow(roads_d)) sf::st_union(roads_d) else NULL
    v <- !is.na(R$km_now); pn <- ifelse(A_, R$on_road_prop, R$on_road_now); kn <- ifelse(A_, R$km_prop, R$km_now)   # team boundaries only (not health-area edges)
    rc <- c(pct = sum(R$km_now[v] * R$on_road_now[v]) / max(sum(R$km_now[v]), 1e-9)); rp <- c(pct = sum(kn[v] * pn[v]) / max(sum(kn[v]), 1e-9))
    ha_u <- U(polys(sf::st_union(sf::st_transform(ha_all[ha_all$dfa_name %in% T$h, ], crs)), crs), crs)
    row <- data.frame(district = D, on_road_now = rc['pct'], on_road_prop = rp['pct'], area_moved_pct = 100 * sum(R$moved_m2[A_], na.rm = TRUE) / A(ha_u),
      stretches_routed = sum(R$routed[A_], na.rm = TRUE), stretches_kept = sum(R$kept, na.rm = TRUE) + sum(R$routed[!A_], na.rm = TRUE), sites_moved_out = 0)
    saveRDS(list(row = row, cur = cu, prop = pr, roads = if (nrow(roads_d)) sf::st_transform(roads_d, 4326) else NULL, sites = NULL, wp = do.call(rbind, wp_all[ok])),
      file.path(ROUT, paste0(gsub('[^A-Za-z0-9]+', '_', D), '.rds'))) }
  cat(sprintf('%-13s layouts %2d accepted %2d | team boundaries on street %5.1f%% -> %5.1f%% | %.0fs\n', D, nrow(R), sum(R$accepted), if (any(ok)) rc['pct'] else NA, if (any(ok)) rp['pct'] else NA,
    as.numeric(difftime(Sys.time(), t0, units = 'secs')))) }
SU <- do.call(rbind, summ); write.csv(SU, file.path(S, 'team_route_summary.csv'), row.names = FALSE); print(SU[, -2], row.names = FALSE); cat('DONE\n')
