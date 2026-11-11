# Campaign 10, all PUBLISHED layers: health areas vs district, team layouts vs their health area. Read-only.
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); sf::sf_use_s2(FALSE); options(width = 250, warn = -1); S <- Sys.getenv('WORK'); if (!nzchar(S)) stop('set WORK to a working folder')
q <- function(sql, p = list()) if (length(p)) DBI::dbGetQuery(pool, sql, p) else DBI::dbGetQuery(pool, sql)
utm <- 32638
rd <- function(txt) sf::st_transform(safe_make_valid(geojsonsf::geojson_sf(txt)), utm)
parts <- function(x) { g <- sf::st_make_valid(sf::st_geometry(x)); g <- g[!sf::st_is_empty(g)]; if (!length(g)) return(g); g <- tryCatch(sf::st_collection_extract(g, 'POLYGON'), error = function(e) g[sf::st_dimension(g) == 2]); if (!length(g)) return(g); sf::st_cast(g, 'POLYGON') }
area <- function(g) if (length(g)) sum(as.numeric(sf::st_area(g))) else 0
diffp <- function(a, b) { d <- sf::st_difference(a, b); parts(d) }
pairov <- function(g) { ov <- 0; m <- sf::st_intersects(g, g); for (i in seq_along(m)) for (j in m[[i]]) if (i < j) ov <- ov + area(parts(sf::st_intersection(g[i], g[j]))); ov }
opening_loss <- function(g, w = 3) { o <- sf::st_buffer(sf::st_buffer(g, -w), w); vapply(seq_along(g), function(i) area(parts(sf::st_difference(g[i], o[i]))), 0) }
shp <- sf::st_transform(safe_make_valid(districts_shp_for_campaign(10L)), utm)
H <- q("SELECT version_id v, district_name d, saved_dfa_sf::text s FROM mapping_versions WHERE campaign_id = 10 AND is_shared AND archived_at IS NULL AND saved_dfa_sf IS NOT NULL ORDER BY 2")
ho <- list(); hal <- list()
for (i in seq_len(nrow(H))) {
  x <- rd(H$s[i]); g <- x |> dplyr::group_by(dfa_name) |> dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop'); hal[[H$d[i]]] <- g
  d <- sf::st_union(shp[shp$district_name == H$d[i], ]); gg <- sf::st_geometry(g)
  gp <- diffp(d, sf::st_union(gg)); gp <- gp[as.numeric(sf::st_area(gp)) > 1]
  np <- vapply(seq_along(gg), function(k) length(parts(gg[k])), 1L)
  ol <- opening_loss(gg)
  ho[[i]] <- data.frame(district = H$d[i], v = H$v[i], has = nrow(g), gap_m2 = round(area(gp)), gap_pieces = length(gp), overlap_m2 = round(pairov(gg)),
    multi_part_has = sum(np > 1), spike_m2 = round(sum(ol)), worst_spike = if (max(ol) > 0) paste0(substr(g$dfa_name[which.max(ol)], 1, 22), ' ', round(max(ol))) else '') }
HO <- do.call(rbind, ho); cat('== PUBLISHED HEALTH AREAS (spike_m2 = area thinner than ~6 m)\n'); print(HO, row.names = FALSE)
T <- q("SELECT t.team_version_id tv, t.district_name d, t.health_area_name h, t.based_on_health_area_version_id hv, COALESCE(t.smoothed_team_sf, t.saved_team_sf)::text ts, mv.saved_dfa_sf::text hs
        FROM team_area_versions t JOIN mapping_versions mv ON mv.version_id = t.based_on_health_area_version_id
        WHERE t.campaign_id = 10 AND t.is_shared AND t.archived_at IS NULL AND COALESCE(t.smoothed_team_sf, t.saved_team_sf) IS NOT NULL ORDER BY 2, 3")
to <- list()
for (i in seq_len(nrow(T))) {
  r <- tryCatch({
    hs <- rd(T$hs[i]); hb <- sf::st_union(hs[hs$dfa_name == T$h[i], ]); ha_a <- area(hb)
    ts <- rd(T$ts[i]); tg <- ts |> dplyr::group_by(k = dfa_name) |> dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop'); gg <- sf::st_geometry(tg)
    gp <- diffp(hb, sf::st_union(gg)); ga <- as.numeric(sf::st_area(gp)); sp <- diffp(sf::st_union(gg), hb); sa <- as.numeric(sf::st_area(sp))
    pa <- lapply(seq_along(gg), function(k) sort(as.numeric(sf::st_area(parts(gg[k]))), decreasing = TRUE))
    data.frame(district = T$d[i], tv = T$tv[i], ha = substr(T$h[i], 1, 30), teams = nrow(tg), ha_m2 = round(ha_a),
      gap_m2 = round(sum(ga)), gap_big = sum(ga > 100), spill_m2 = round(sum(sa)), overlap_m2 = round(pairov(gg)),
      islands = sum(vapply(pa, function(z) length(z) - 1L, 1L)), island_m2 = round(sum(vapply(pa, function(z) sum(z[-1]), 0))), note = '')
  }, error = function(e) data.frame(district = T$d[i], tv = T$tv[i], ha = substr(T$h[i], 1, 30), teams = NA, ha_m2 = NA, gap_m2 = NA, gap_big = NA, spill_m2 = NA, overlap_m2 = NA, islands = NA, island_m2 = NA, note = conditionMessage(e)))
  to[[i]] <- r }
TO <- do.call(rbind, to); TO$bad <- with(TO, gap_m2 + spill_m2 + overlap_m2 + island_m2)
TO$bad_pct <- round(100 * TO$bad / TO$ha_m2, 2)
cat('\n== PUBLISHED TEAM LAYOUTS:', nrow(TO), '\n')
agg <- aggregate(cbind(layouts = 1, gap_m2, spill_m2, overlap_m2, islands, island_m2, bad) ~ district, TO, sum); agg$ha_m2 <- aggregate(ha_m2 ~ district, TO, sum)$ha_m2
agg$bad_pct <- round(100 * agg$bad / agg$ha_m2, 2); print(agg[order(-agg$bad_pct), ], row.names = FALSE)
cat('\nworst 25 layouts:\n'); print(head(TO[order(-TO$bad_pct), ], 25), row.names = FALSE)
cat('\nlayouts with any overlap > 1 m2:', sum(TO$overlap_m2 > 1, na.rm = TRUE), '| any island:', sum(TO$islands > 0, na.rm = TRUE), '| gap piece > 100 m2:', sum(TO$gap_big > 0, na.rm = TRUE), '| errors:', sum(nzchar(TO$note)), '\n')
write.csv(HO, file.path(S, 'scan_ha.csv'), row.names = FALSE); write.csv(TO, file.path(S, 'scan_team.csv'), row.names = FALSE)
