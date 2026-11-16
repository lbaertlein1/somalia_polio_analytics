# Street-following core, shared by street_follow.R (health areas) and street_follow_teams.R (teams inside one health area). Source after clean_lib.R.
GPKG <- 'data/osm_inputs/cache/geofabrik_somalia-latest.gpkg'
W <- c(motorway = .6, trunk = .6, primary = .6, secondary = .7, tertiary = .8, motorway_link = .7, trunk_link = .7, primary_link = .7, secondary_link = .8, tertiary_link = .9,
       unclassified = 1, residential = 1, living_street = 1, road = 1, service = 1.3, track = 1.2, path = 1.5, pedestrian = 1.4, river = .8, stream = 1, canal = 1, wadi = 1, subdivision = .6, urban_edge = .7)
lines_of <- function(g) { g <- g[!sf::st_is_empty(g)]; if (!length(g)) return(g); g <- tryCatch(sf::st_collection_extract(g, 'LINESTRING'), error = function(e) g[sf::st_dimension(g) == 1]); if (!length(g)) return(g)
  sf::st_cast(sf::st_cast(g, 'MULTILINESTRING', warn = FALSE), 'LINESTRING', warn = FALSE) }
# share of INTERNAL boundary (not the outer edge) within tol m of a street
on_road_share <- function(geoms, roads_u, tol = 3) { b <- sf::st_union(sf::st_boundary(geoms)); ext <- sf::st_boundary(sf::st_union(geoms)); internal <- sf::st_difference(b, sf::st_buffer(ext, 1))
  L <- sum(as.numeric(sf::st_length(internal))); if (L == 0 || is.null(roads_u)) return(c(km = L / 1000, pct = 0)); c(km = L / 1000, pct = 100 * sum(as.numeric(sf::st_length(sf::st_intersection(internal, sf::st_buffer(roads_u, tol))))) / L) }
# Guide lines near an outline (UTM sfc), in that crs: OSM streets/tracks/waterways, plus (when `district`, a district sf row, is given and GUIDES allows)
# city-section (subdivision) boundaries and settlement-extent (urban edge) outlines from the app's ArcGIS layers. GUIDES: comma list of
# street, river (OSM waterway river/stream/canal/wadi), subdivision, urban; 'osm' = street + river (default osm,subdivision,urban).
# Outside towns use GUIDES=river,subdivision,urban: routing through every rural track is very slow and not meaningful.
load_roads <- function(outline, crs, pad = 400, district = NULL) {
  gd <- trimws(strsplit(Sys.getenv('GUIDES', 'osm,subdivision,urban'), ',')[[1]])
  wkt <- sf::st_as_text(sf::st_as_sfc(sf::st_bbox(sf::st_transform(sf::st_buffer(outline, pad), 4326))))
  ln <- sf::st_read(GPKG, layer = 'lines', quiet = TRUE, wkt_filter = wkt); ln$cls <- ifelse(!is.na(ln$highway), ln$highway, ln$waterway); ln <- ln[ln$cls %in% names(W), ]
  if ('osm' %in% gd) gd <- c(gd, 'street', 'river'); riv <- ln$cls %in% c('river', 'stream', 'canal', 'wadi')
  roads <- sf::st_transform(ln[('street' %in% gd & !riv) | ('river' %in% gd & riv), c('cls', 'highway', 'name')], crs)
  poly_lines <- function(p, cls) { if (is.null(p) || !nrow(p)) return(NULL); g <- lines_of(sf::st_boundary(sf::st_geometry(sf::st_transform(sf::st_make_valid(p), crs)))); if (!length(g)) return(NULL)
    sf::st_sf(cls = cls, highway = NA_character_, name = NA_character_, geometry = g) }
  sf::st_geometry(roads) <- 'geometry'
  if (!is.null(district)) {
    if ('subdivision' %in% gd) { s <- tryCatch(fetch_subdivisions_for_district(district), error = function(e) NULL); if (!is.null(s) && nrow(s) >= 2) roads <- rbind(roads, poly_lines(s, 'subdivision')) }
    if ('urban' %in% gd) { e <- tryCatch(fetch_settlement_extents_for_district(district), error = function(e) NULL); roads <- rbind(roads, poly_lines(e, 'urban_edge')) } }
  roads[lengths(sf::st_intersects(roads, sf::st_buffer(outline, 300))) > 0, ] }
# share of internal boundary length within tol m of each guide class group
guide_shares <- function(geoms, roads, tol = 3) { grp <- ifelse(roads$cls == 'subdivision', 'subdivision', ifelse(roads$cls == 'urban_edge', 'urban', ifelse(roads$cls %in% c('river', 'stream', 'canal', 'wadi'), 'river', 'street')))
  vapply(c('street', 'river', 'subdivision', 'urban'), function(g) { r <- roads[grp == g, ]; if (!nrow(r)) 0 else on_road_share(geoms, sf::st_union(r), tol)[['pct']] }, 0) }

# cur: named list of UTM sfc (a partition of outline). Each shared stretch between two areas is replaced by the cheapest street route between its ends,
# inside a corridor (cw_min..cw_max m) around the current line; faces of the new network go to the area holding most of them; clean_partition() tidies.
# Returns list(areas = named list of UTM sfc, routed, kept, lost = areas restored because they would have vanished).
# fixed: areas whose boundaries are not re-routed (their shared stretches are kept as they are).
route_partition <- function(cur, outline, roads, extra = character(), cw_min = 60, cw_max = 300, snap_max = 150, kind = 'ha', fixed = character()) {
  crs <- sf::st_crs(outline); gn <- names(cur); k <- length(gn)
  if (!nrow(roads) || k < 2) return(list(areas = cur, routed = 0, kept = 0, lost = character()))
  seg <- lines_of(sf::st_union(sf::st_geometry(roads))); seg <- seg[as.numeric(sf::st_length(seg)) > 0.05]
  mid <- sf::st_line_sample(seg, sample = 0.5); seg_cls <- roads$cls[sf::st_nearest_feature(sf::st_cast(mid, 'POINT'), roads)]
  xy <- lapply(seg, function(s) { m <- sf::st_coordinates(s); rbind(m[1, 1:2], m[nrow(m), 1:2]) })
  key <- function(p) sprintf('%.1f_%.1f', p[1], p[2]); from <- vapply(xy, function(m) key(m[1, ]), ''); to <- vapply(xy, function(m) key(m[2, ]), '')
  len <- as.numeric(sf::st_length(seg)); g <- igraph::graph_from_data_frame(data.frame(from, to, weight = len * W[seg_cls], eid = seq_along(seg)), directed = FALSE)
  nodes_xy <- do.call(rbind, lapply(strsplit(igraph::V(g)$name, '_'), as.numeric))
  new_lines <- list(); ends <- list(); kept <- 0; routed <- 0
  for (a in seq_len(k - 1)) for (b in (a + 1):k) {
    if (!lengths(sf::st_intersects(cur[[a]], sf::st_buffer(cur[[b]], 0.5)))) next
    li <- lines_of(sf::st_intersection(sf::st_boundary(cur[[a]]), sf::st_buffer(cur[[b]], 0.5))); li <- li[as.numeric(sf::st_length(li)) > 1]; if (!length(li)) next
    sh <- lines_of(sf::st_line_merge(sf::st_cast(sf::st_union(sf::st_snap(li, li, 0.5)), "MULTILINESTRING")))
    for (s in seq_along(sh)) { part <- sh[s]; L <- as.numeric(sf::st_length(part)); if (L < 1) next
      m <- sf::st_coordinates(part); p1 <- m[1, 1:2]; p2 <- m[nrow(m), 1:2]; ends[[length(ends) + 1]] <- p1; ends[[length(ends) + 1]] <- p2
      if (gn[a] %in% fixed || gn[b] %in% fixed) { new_lines[[length(new_lines) + 1]] <- part; kept <- kept + 1; next }
      cw <- min(cw_max, max(cw_min, 0.15 * L)); corr <- sf::st_intersection(sf::st_buffer(part, cw), sf::st_buffer(outline, 1))
      ein <- which(lengths(sf::st_within(seg, corr)) > 0); ok <- FALSE
      if (length(ein) >= 1) { sg <- igraph::subgraph.edges(g, igraph::E(g)[eid %in% ein], delete.vertices = TRUE); se <- igraph::E(sg)$eid; dm <- as.numeric(sf::st_distance(sf::st_cast(mid[se], 'POINT'), part)); igraph::E(sg)$weight <- len[se] * W[seg_cls[se]] * (1 + dm / 50); nm <- igraph::V(sg)$name; nxy <- nodes_xy[match(nm, igraph::V(g)$name), , drop = FALSE]
        d1 <- sqrt((nxy[, 1] - p1[1])^2 + (nxy[, 2] - p1[2])^2); d2 <- sqrt((nxy[, 1] - p2[1])^2 + (nxy[, 2] - p2[2])^2); snap <- min(cw, snap_max)
        if (min(d1) <= snap && min(d2) <= snap && which.min(d1) != which.min(d2)) {
          sp <- suppressWarnings(igraph::shortest_paths(sg, nm[which.min(d1)], nm[which.min(d2)], output = 'epath')$epath[[1]])
          if (length(sp)) { path <- sf::st_union(seg[igraph::E(sg)$eid[sp]]); con <- sf::st_sfc(sf::st_linestring(rbind(p1, nxy[which.min(d1), ])), sf::st_linestring(rbind(nxy[which.min(d2), ], p2)), crs = crs)
            pl <- as.numeric(sf::st_length(path)); if (pl <= 2.5 * L + 2 * snap) { new_lines[[length(new_lines) + 1]] <- c(sf::st_geometry(path), con); ok <- TRUE; routed <- routed + 1 } } } }
      if (!ok) { new_lines[[length(new_lines) + 1]] <- part; kept <- kept + 1 } } }
  if (!length(new_lines)) return(list(areas = cur, routed = 0, kept = 0, lost = character()))
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
  cl <- clean_partition(geo, outline, extra = extra, kind = kind)
  lost <- gn[vapply(gn, function(n) A(cl$areas[[n]]) < 0.01 * A(cur[[n]]), TRUE)]
  for (n in lost) { for (o in setdiff(gn, n)) cl$areas[[o]] <- U(polys(sf::st_difference(cl$areas[[o]], cur[[n]]), crs), crs); cl$areas[[n]] <- cur[[n]] }
  list(areas = cl$areas, routed = routed, kept = kept, lost = lost) }
