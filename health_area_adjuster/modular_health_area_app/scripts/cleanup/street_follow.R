# READ-ONLY: "street-following" proposal for PUBLISHED campaign-10 health areas. Nothing written to the DB.
# Each shared boundary stretch (between two health areas) is replaced by the cheapest OSM street/track/path/river route between its two ends,
# within a corridor around the current line (main roads preferred). Ends stay fixed (short connectors), so neighbouring boundaries still meet.
# Faces of the new line network go to the health area holding most of them; clean_partition() tidies. Output <WORK>/route_out/<district>.rds
# Env: WORK (output folder), ONLY (comma list of districts). Then: street_follow_review_html.R (review maps), clean_published.R with ROUTE_DIR (apply).
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 250, warn = -1); S <- Sys.getenv('WORK'); if (!nzchar(S)) stop('set WORK to a working folder'); source('scripts/cleanup/clean_lib.R'); source('scripts/cleanup/street_lib.R')
OUT <- file.path(S, 'route_out'); dir.create(OUT, showWarnings = FALSE)
q <- function(sql) DBI::dbGetQuery(pool, sql); shp <- districts_shp_for_campaign(10L)
# VERSIONS (comma list of version ids, e.g. unpublished drafts) instead of the published versions; output then <district>_v<id>.rds (write_street_draft.R writes it back)
VS <- suppressWarnings(as.integer(trimws(strsplit(Sys.getenv('VERSIONS'), ',')[[1]]))); VS <- VS[!is.na(VS)]
H <- q(paste0("SELECT version_id v, district_name d, COALESCE(smoothed_dfa_sf, saved_dfa_sf)::text g, odk_sf::text o, app_sf::text a, last_updated_at lu FROM mapping_versions
        WHERE ", if (length(VS)) paste0('version_id IN (', paste(VS, collapse = ','), ')') else 'campaign_id = 10 AND is_shared', " AND archived_at IS NULL AND saved_dfa_sf IS NOT NULL ORDER BY d, version_id"))
only <- trimws(strsplit(Sys.getenv('ONLY'), ',')[[1]]); if (length(only) && nzchar(only[1])) H <- H[H$d %in% only, ]
for (i in seq_len(nrow(H))) { D <- H$d[i]; t0 <- Sys.time()
  r <- tryCatch({
    ha <- sf::st_make_valid(geojsonsf::geojson_sf(H$g[i])); crs <- utm_for(ha); gn <- unique(as.character(ha$dfa_name)); k <- length(gn)
    if (nzchar(Sys.getenv("SNAP"))) ha <- sf::st_make_valid(sf::st_set_precision(sf::st_transform(ha, crs), as.numeric(Sys.getenv("SNAP"))))  # SNAP=100: 1 cm grid, clears GEOS non-noded errors
    cur <- lapply(gn, function(n) U(polys(sf::st_union(sf::st_transform(ha[ha$dfa_name == n, ], crs)), crs), crs)); names(cur) <- gn
    outline <- U(polys(sf::st_union(sf::st_transform(sf::st_make_valid(shp[shp$district_name == D, ]), crs)), crs), crs)
    roads <- load_roads(outline, crs, district = shp[shp$district_name == D, ][1, ]); roads_u <- if (nrow(roads)) sf::st_union(roads) else NULL
    rt <- route_partition(cur, outline, roads, extra = extra_dfa_names); routed <- rt$routed; kept <- rt$kept; lost <- rt$lost; cl <- list(areas = rt$areas)
    curs <- sf::st_sf(dfa_name = gn, geometry = do.call(c, cur))
    prop <- sf::st_sf(dfa_name = gn, geometry = finalize_partition(cl$areas, crs)); propu <- sf::st_transform(prop, crs); cur4 <- sf::st_transform(curs, 4326)
    wp_c <- vapply(gn, function(n) sum(.polygon_u5_population(cur4[cur4$dfa_name == n, ], u5_rast), na.rm = TRUE), 0); wp_p <- vapply(gn, function(n) sum(.polygon_u5_population(prop[prop$dfa_name == n, ], u5_rast), na.rm = TRUE), 0)
    st <- NULL; for (z in c(H$o[i], H$a[i])) if (!is.na(z)) { x <- geojsonsf::geojson_sf(z); st <- rbind(st, x[x$polio_sia_coordination_site %in% 'Yes', 'facility_name']) }
    site_bad <- if (!is.null(st)) { inn <- sf::st_intersects(sf::st_transform(st, 4326), prop); st$facility_name[!vapply(seq_len(nrow(st)), function(j) length(inn[[j]]) > 0 && prop$dfa_name[inn[[j]][1]] == st$facility_name[j], TRUE) & st$facility_name %in% gn] } else character()
    rc <- on_road_share(sf::st_geometry(curs), roads_u); rp <- on_road_share(sf::st_geometry(propu), roads_u)
    moved <- sum(vapply(seq_len(k), function(j) A(polys(sf::st_difference(sf::st_geometry(propu)[j], cur[[j]]), crs)), 0))
    pieces <- sum(vapply(seq_len(k), function(j) length(polys(sf::st_geometry(propu)[j], crs)), 1L))
    row <- data.frame(district = D, areas = k, road_km = round(if (is.null(roads_u)) 0 else as.numeric(sf::st_length(roads_u)) / 1000), stretches_routed = routed, stretches_kept = kept,
      boundary_km_now = round(rc['km'], 1), on_road_now = round(rc['pct'], 1), boundary_km_prop = round(rp['km'], 1), on_road_prop = round(rp['pct'], 1),
      area_moved_km2 = round(moved / 1e6, 3), area_moved_pct = round(100 * moved / A(outline), 2), max_wp_change_pct = round(100 * max(abs(wp_p - wp_c) / pmax(wp_c, 1)), 1),
      max_wp_change_u5 = round(max(abs(wp_p - wp_c))), pieces_prop = pieces, guides_now = paste(sprintf('%s %.0f%%', names(gs_c <- guide_shares(sf::st_geometry(curs), roads)), gs_c), collapse = ', '),
      guides_prop = paste(sprintf('%s %.0f%%', names(gs_p <- guide_shares(sf::st_geometry(propu), roads)), gs_p), collapse = ', '), sites_moved_out = length(site_bad), kept_unchanged = paste(lost, collapse = '; '))
    list(v = H$v[i], lu = H$lu[i], row = row, cur = cur4, prop = prop, roads = if (nrow(roads)) sf::st_transform(roads, 4326) else NULL, sites = st, site_bad = site_bad,
         wp = data.frame(health_area = gn, worldpop_now = round(wp_c), worldpop_proposal = round(wp_p)))
  }, error = function(e) { cat('ERROR', D, conditionMessage(e), '\n'); NULL })
  if (!is.null(r)) { saveRDS(r, file.path(OUT, paste0(gsub('[^A-Za-z0-9]+', '_', D), if (length(VS)) paste0('_v', H$v[i]) else '', '.rds')))
    cat(sprintf('%-14s routed %3d kept %3d | on guide %5.1f%% -> %5.1f%% | boundary %.1f -> %.1f km | moved %.2f%% of district | max WP change %4.1f%% | sites out %d | %.0fs\n', D,
      r$row$stretches_routed, r$row$stretches_kept, r$row$on_road_now, r$row$on_road_prop, r$row$boundary_km_now, r$row$boundary_km_prop, r$row$area_moved_pct, r$row$max_wp_change_pct, r$row$sites_moved_out, as.numeric(difftime(Sys.time(), t0, units = 'secs')))); cat('     by guide now:', r$row$guides_now, '| proposal:', r$row$guides_prop, '
') } }
cat('DONE\n')
