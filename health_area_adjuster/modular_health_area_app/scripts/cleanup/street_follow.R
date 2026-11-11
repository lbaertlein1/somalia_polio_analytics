# READ-ONLY: "street-following" proposal for PUBLISHED campaign-10 health areas. Nothing written to the DB.
# Each shared boundary stretch (between two health areas) is replaced by the cheapest OSM street/track/path/river route between its two ends,
# within a corridor around the current line (main roads preferred). Ends stay fixed (short connectors), so neighbouring boundaries still meet.
# Faces of the new line network go to the health area holding most of them; clean_partition() tidies. Output <WORK>/route_out/<district>.rds
# Env: WORK (output folder), ONLY (comma list of districts). Then: street_follow_review_html.R (review maps), clean_published.R with ROUTE_DIR (apply).
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 250, warn = -1); S <- Sys.getenv('WORK'); if (!nzchar(S)) stop('set WORK to a working folder'); source('scripts/cleanup/clean_lib.R')
OUT <- file.path(S, 'route_out'); dir.create(OUT, showWarnings = FALSE)
q <- function(sql) DBI::dbGetQuery(pool, sql); shp <- districts_shp_for_campaign(10L)
GPKG <- 'data/osm_inputs/cache/geofabrik_somalia-latest.gpkg'
W <- c(motorway = .6, trunk = .6, primary = .6, secondary = .7, tertiary = .8, motorway_link = .7, trunk_link = .7, primary_link = .7, secondary_link = .8, tertiary_link = .9,
       unclassified = 1, residential = 1, living_street = 1, road = 1, service = 1.3, track = 1.2, path = 1.5, pedestrian = 1.4, river = .8, stream = 1, canal = 1, wadi = 1)
H <- q("SELECT version_id v, district_name d, COALESCE(smoothed_dfa_sf, saved_dfa_sf)::text g, odk_sf::text o, app_sf::text a FROM mapping_versions
        WHERE campaign_id = 10 AND is_shared AND archived_at IS NULL AND saved_dfa_sf IS NOT NULL ORDER BY d")
only <- trimws(strsplit(Sys.getenv('ONLY'), ',')[[1]]); if (length(only) && nzchar(only[1])) H <- H[H$d %in% only, ]
lines_of <- function(g) { g <- g[!sf::st_is_empty(g)]; if (!length(g)) return(g); g <- tryCatch(sf::st_collection_extract(g, 'LINESTRING'), error = function(e) g[sf::st_dimension(g) == 1]); if (!length(g)) return(g)
  sf::st_cast(sf::st_cast(g, 'MULTILINESTRING', warn = FALSE), 'LINESTRING', warn = FALSE) }
on_road_share <- function(geoms, roads_u, tol = 3) { b <- sf::st_union(sf::st_boundary(geoms)); ext <- sf::st_boundary(sf::st_union(geoms)); internal <- sf::st_difference(b, sf::st_buffer(ext, 1))
  L <- sum(as.numeric(sf::st_length(internal))); if (L == 0 || is.null(roads_u)) return(c(km = L / 1000, pct = 0)); c(km = L / 1000, pct = 100 * sum(as.numeric(sf::st_length(sf::st_intersection(internal, sf::st_buffer(roads_u, tol))))) / L) }
for (i in seq_len(nrow(H))) { D <- H$d[i]; t0 <- Sys.time()
  r <- tryCatch({
    ha <- sf::st_make_valid(geojsonsf::geojson_sf(H$g[i])); crs <- utm_for(ha); gn <- unique(as.character(ha$dfa_name)); k <- length(gn)
    cur <- lapply(gn, function(n) U(polys(sf::st_union(sf::st_transform(ha[ha$dfa_name == n, ], crs)), crs), crs)); names(cur) <- gn
    outline <- U(polys(sf::st_union(sf::st_transform(sf::st_make_valid(shp[shp$district_name == D, ]), crs)), crs), crs)
    wkt <- sf::st_as_text(sf::st_as_sfc(sf::st_bbox(sf::st_transform(sf::st_buffer(outline, 400), 4326))))
    ln <- sf::st_read(GPKG, layer = 'lines', quiet = TRUE, wkt_filter = wkt); ln$cls <- ifelse(!is.na(ln$highway), ln$highway, ln$waterway); ln <- ln[ln$cls %in% names(W), ]
    roads <- sf::st_transform(ln[, c('cls', 'highway', 'name')], crs); roads <- roads[lengths(sf::st_intersects(roads, sf::st_buffer(outline, 300))) > 0, ]
    roads_u <- if (nrow(roads)) sf::st_union(roads) else NULL
    # noded network with class weights
    seg <- lines_of(sf::st_union(sf::st_geometry(roads))); seg <- seg[as.numeric(sf::st_length(seg)) > 0.05]
    mid <- sf::st_line_sample(seg, sample = 0.5); seg_cls <- roads$cls[sf::st_nearest_feature(sf::st_cast(mid, 'POINT'), roads)]
    xy <- lapply(seg, function(s) { m <- sf::st_coordinates(s); rbind(m[1, 1:2], m[nrow(m), 1:2]) })
    key <- function(p) sprintf('%.1f_%.1f', p[1], p[2]); from <- vapply(xy, function(m) key(m[1, ]), ''); to <- vapply(xy, function(m) key(m[2, ]), '')
    len <- as.numeric(sf::st_length(seg)); g <- igraph::graph_from_data_frame(data.frame(from, to, weight = len * W[seg_cls], eid = seq_along(seg)), directed = FALSE)
    nodes_xy <- do.call(rbind, lapply(strsplit(igraph::V(g)$name, '_'), as.numeric))
    # shared boundary stretches
    new_lines <- list(); ends <- list(); kept <- 0; routed <- 0
    for (a in seq_len(k - 1)) for (b in (a + 1):k) {
      if (!lengths(sf::st_intersects(cur[[a]], sf::st_buffer(cur[[b]], 0.5)))) next
      li <- lines_of(sf::st_intersection(sf::st_boundary(cur[[a]]), sf::st_buffer(cur[[b]], 0.5))); li <- li[as.numeric(sf::st_length(li)) > 1]; if (!length(li)) next; sh <- lines_of(sf::st_line_merge(sf::st_cast(sf::st_union(sf::st_snap(li, li, 0.5)), "MULTILINESTRING")))
      for (s in seq_along(sh)) { part <- sh[s]; L <- as.numeric(sf::st_length(part)); if (L < 1) next
        m <- sf::st_coordinates(part); p1 <- m[1, 1:2]; p2 <- m[nrow(m), 1:2]; ends[[length(ends) + 1]] <- p1; ends[[length(ends) + 1]] <- p2
        cw <- min(300, max(60, 0.15 * L)); corr <- sf::st_intersection(sf::st_buffer(part, cw), sf::st_buffer(outline, 1))
        ein <- which(lengths(sf::st_within(seg, corr)) > 0); ok <- FALSE
        if (length(ein) >= 1) { sg <- igraph::subgraph.edges(g, igraph::E(g)[eid %in% ein], delete.vertices = TRUE); se <- igraph::E(sg)$eid; dm <- as.numeric(sf::st_distance(sf::st_cast(mid[se], 'POINT'), part)); igraph::E(sg)$weight <- len[se] * W[seg_cls[se]] * (1 + dm / 50); nm <- igraph::V(sg)$name; nxy <- nodes_xy[match(nm, igraph::V(g)$name), , drop = FALSE]
          d1 <- sqrt((nxy[, 1] - p1[1])^2 + (nxy[, 2] - p1[2])^2); d2 <- sqrt((nxy[, 1] - p2[1])^2 + (nxy[, 2] - p2[2])^2); snap <- min(cw, 150)
          if (min(d1) <= snap && min(d2) <= snap && which.min(d1) != which.min(d2)) {
            sp <- suppressWarnings(igraph::shortest_paths(sg, nm[which.min(d1)], nm[which.min(d2)], output = 'epath')$epath[[1]])
            if (length(sp)) { path <- sf::st_union(seg[igraph::E(sg)$eid[sp]]); con <- sf::st_sfc(sf::st_linestring(rbind(p1, nxy[which.min(d1), ])), sf::st_linestring(rbind(nxy[which.min(d2), ], p2)), crs = crs)
              pl <- as.numeric(sf::st_length(path)); if (pl <= 2.5 * L + 2 * snap) { new_lines[[length(new_lines) + 1]] <- c(sf::st_geometry(path), con); ok <- TRUE; routed <- routed + 1 } } } }
        if (!ok) { new_lines[[length(new_lines) + 1]] <- part; kept <- kept + 1 } } }
    net <- sf::st_union(c(sf::st_cast(sf::st_boundary(outline), 'MULTILINESTRING'), do.call(c, lapply(new_lines, function(z) sf::st_cast(sf::st_sfc(z, crs = crs), 'MULTILINESTRING')))))
    ob <- sf::st_cast(sf::st_boundary(outline), 'MULTILINESTRING')
    for (p in ends) { pt <- sf::st_sfc(sf::st_point(p), crs = crs); d <- as.numeric(sf::st_distance(pt, ob)); if (d > 5) next
      np <- sf::st_coordinates(sf::st_cast(sf::st_nearest_points(pt, ob), 'POINT'))[2, 1:2]; v <- np - p; nv <- sqrt(sum(v^2)); if (nv < 1e-6) { tg <- np } else tg <- np + 0.5 * v / nv
      new_lines[[length(new_lines) + 1]] <- sf::st_sfc(sf::st_linestring(rbind(p, tg)), crs = crs) }
    inner <- sf::st_union(do.call(c, lapply(new_lines, function(z) sf::st_cast(sf::st_sfc(z, crs = crs), "MULTILINESTRING")))); faces <- polys(sf::st_difference(outline, sf::st_buffer(inner, 0.02)), crs)
    faces <- c(faces, polys(sf::st_difference(outline, U(faces, crs)), crs))
    curs <- sf::st_sf(dfa_name = gn, geometry = do.call(c, cur)); ix <- sf::st_intersection(sf::st_sf(f = seq_along(faces), geometry = faces), curs); ix$a <- as.numeric(sf::st_area(ix))
    own <- vapply(seq_along(faces), function(f) { s <- ix[ix$f == f, ]; if (!nrow(s)) NA_character_ else s$dfa_name[which.max(s$a)] }, '')
    # an area that wins no face would vanish: faces holding its pieces keep the current split
    split <- rep(FALSE, length(faces)); for (m in setdiff(gn, own)) split[unique(ix$f[ix$dfa_name == m])] <- TRUE
    geo <- setNames(lapply(gn, function(n) { a <- faces[own %in% n & !split]; b <- sf::st_geometry(ix[ix$f %in% which(split) & ix$dfa_name == n, ])
      U(polys(sf::st_union(c(a, b)), crs), crs) }), gn)
    cl <- clean_partition(geo, outline, extra = extra_dfa_names)
    lost <- gn[vapply(gn, function(n) A(cl$areas[[n]]) < 0.01 * A(cur[[n]]), TRUE)]
    for (n in lost) { for (o in setdiff(gn, n)) cl$areas[[o]] <- U(polys(sf::st_difference(cl$areas[[o]], cur[[n]]), crs), crs); cl$areas[[n]] <- cur[[n]] }
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
      max_wp_change_u5 = round(max(abs(wp_p - wp_c))), pieces_prop = pieces, sites_moved_out = length(site_bad), kept_unchanged = paste(lost, collapse = '; '))
    list(row = row, cur = cur4, prop = prop, roads = if (nrow(roads)) sf::st_transform(roads, 4326) else NULL, sites = st, site_bad = site_bad,
         wp = data.frame(health_area = gn, worldpop_now = round(wp_c), worldpop_proposal = round(wp_p)))
  }, error = function(e) { cat('ERROR', D, conditionMessage(e), '\n'); NULL })
  if (!is.null(r)) { saveRDS(r, file.path(OUT, paste0(gsub('[^A-Za-z0-9]+', '_', D), '.rds')))
    cat(sprintf('%-14s routed %3d kept %3d | on-road %5.1f%% -> %5.1f%% | boundary %.1f -> %.1f km | moved %.2f%% of district | max WP change %4.1f%% | sites out %d | %.0fs\n', D,
      r$row$stretches_routed, r$row$stretches_kept, r$row$on_road_now, r$row$on_road_prop, r$row$boundary_km_now, r$row$boundary_km_prop, r$row$area_moved_pct, r$row$max_wp_change_pct, r$row$sites_moved_out, as.numeric(difftime(Sys.time(), t0, units = 'secs')))) } }
cat('DONE\n')
