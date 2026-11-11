# Shared partition-cleaning code (sourced after global.R). All geometry work in a local UTM zone; output EPSG:4326, valid on plane + sphere.
sf::sf_use_s2(FALSE)
utm_for <- function(x) { c0 <- sf::st_coordinates(sf::st_centroid(sf::st_union(sf::st_transform(x, 4326))))[1, ]; 32600L + as.integer(floor((c0[1] + 180) / 6) + 1) }
polys <- function(g, crs) { g <- sf::st_sfc(g, crs = crs); g <- sf::st_make_valid(g); g <- g[!sf::st_is_empty(g)]; if (!length(g)) return(sf::st_sfc(crs = crs))
  g <- tryCatch(sf::st_collection_extract(g, 'POLYGON'), error = function(e) g[sf::st_dimension(g) == 2]); if (!length(g)) return(sf::st_sfc(crs = crs))
  g <- sf::st_cast(sf::st_cast(g, 'MULTIPOLYGON', warn = FALSE), 'POLYGON', warn = FALSE); g[as.numeric(sf::st_area(g)) > 0.01] }
A  <- function(g) if (length(g)) sum(as.numeric(sf::st_area(g))) else 0
U  <- function(g, crs) { if (!length(g)) return(sf::st_sfc(sf::st_polygon(), crs = crs)); sf::st_union(g) }
empty_g <- function(g) !length(g) || all(sf::st_is_empty(g)) || A(g) == 0
border_len <- function(piece, g) { if (empty_g(g)) return(0); x <- suppressWarnings(sf::st_intersection(sf::st_boundary(piece), sf::st_buffer(g, 0.05))); if (length(x)) sum(as.numeric(sf::st_length(x))) else 0 }

# areas: named list of sfc (UTM); outline: sfc (UTM). extra: names whose scattered parts are legitimate.
clean_partition <- function(areas, outline, extra = character(), kind = 'ha', split_gaps = 0) {   # split_gaps: m2 above which a gap piece is split by nearest area (0 = off)
  crs <- sf::st_crs(outline); nm <- names(areas); k <- length(areas); log <- list()
  L <- function(type, area, m2, to = NA) log[[length(log) + 1]] <<- data.frame(type = type, area = area, m2 = round(m2, 1), to = to)
  before_a <- vapply(areas, A, 0)
  # 1. spill
  for (i in seq_len(k)) { s <- A(polys(sf::st_difference(areas[[i]], outline), crs)); if (s > 0.5) L('spill clipped', nm[i], s); areas[[i]] <- U(polys(sf::st_intersection(areas[[i]], outline), crs), crs) }
  free <- list(); fo <- character()   # free pieces + the area they must NOT go back to ('' = any)
  addf <- function(p, owner) { for (j in seq_along(p)) { free[[length(free) + 1]] <<- p[j]; fo[length(fo) + 1] <<- owner } }
  # 2. overlaps
  for (i in seq_len(k)) for (j in seq_len(k)) if (i < j) { o <- polys(sf::st_intersection(areas[[i]], areas[[j]]), crs)
    if (A(o) > 0.01) { L('overlap', paste(nm[i], '/', nm[j]), A(o)); addf(o, ''); uo <- U(o, crs)
      areas[[i]] <- U(polys(sf::st_difference(areas[[i]], uo), crs), crs); areas[[j]] <- U(polys(sf::st_difference(areas[[j]], uo), crs), crs) } }
  # 3. thin spikes / peninsulas (square-cornered opening)
  for (i in seq_len(k)) { g <- areas[[i]]; a <- A(g); if (a == 0) next
    w <- max(2, min(15, 0.005 * sqrt(a)))
    op <- sf::st_buffer(sf::st_buffer(g, -w, joinStyle = 'MITRE', mitreLimit = 3), w, joinStyle = 'MITRE', mitreLimit = 3)
    sp <- polys(sf::st_difference(g, op), crs); sp <- sp[as.numeric(sf::st_area(sp)) > 0.5]
    for (p in seq_along(sp)) { piece <- sp[p]
      others <- vapply(seq_len(k), function(j) if (j == i) 0 else border_len(piece, areas[[j]]), 0)
      if (max(others) < 0.5) next                                     # touches no other area -> stays
      rest <- polys(sf::st_difference(areas[[i]], piece), crs); ra <- sort(as.numeric(sf::st_area(rest)), decreasing = TRUE)
      if (length(ra) > length(polys(areas[[i]], crs)) && sum(ra[-1]) > 0.01 * a) next   # would cut the area in two -> leave
      L('spike/peninsula', nm[i], A(piece)); addf(piece, nm[i]); areas[[i]] <- U(rest, crs) } }
  # 4. detached islands
  for (i in seq_len(k)) { p <- polys(areas[[i]], crs); if (length(p) < 2) next
    pa <- as.numeric(sf::st_area(p)); m <- which.max(pa); tot <- sum(pa); keep <- rep(TRUE, length(p))
    for (q in setdiff(seq_along(p), m)) {
      others <- vapply(seq_len(k), function(j) if (j == i) 0 else border_len(p[q], areas[[j]]), 0)
      if (max(others) < 0.5) next                                     # exclave touching nobody (island/coast) -> keep
      lim <- if (nm[i] %in% extra) 500 else 0.10 * tot
      if (pa[q] <= lim) { L('island', nm[i], pa[q]); addf(p[q], nm[i]); keep[q] <- FALSE } else L('FLAG large detached part (kept)', nm[i], pa[q]) }
    areas[[i]] <- U(p[keep], crs) }
  # 5. gaps
  g <- polys(sf::st_difference(outline, U(do.call(c, areas), crs)), crs); if (A(g) > 0.5) L('gap filled', '', A(g))
  # large gap pieces (e.g. land a team layout gains when its health area grows) are split between the nearest areas on a 10 m grid
  if (split_gaps > 0 && length(g)) { big <- as.numeric(sf::st_area(g)) > split_gaps
    for (p in which(big)) { cells <- polys(sf::st_intersection(sf::st_make_grid(g[p], cellsize = 10), g[p]), crs)
      live <- which(!vapply(areas, empty_g, TRUE)); nf <- sf::st_nearest_feature(sf::st_centroid(cells), do.call(c, lapply(areas[live], function(z) U(z, crs))))
      for (j in unique(nf)) { w <- live[j]; areas[[w]] <- U(polys(sf::st_union(c(areas[[w]], cells[nf == j])), crs), crs) }
      L('large gap split by nearest area', '', A(g[p])) }
    g <- g[!big] }
  addf(g, '')
  # 6. assign free pieces: longest shared border (not back to the owner it was taken from)
  for (pass in 1:60) { if (!length(free)) break; lf <- list(); lo <- character()
    for (f in seq_along(free)) { piece <- free[[f]]
      bl <- vapply(seq_len(k), function(j) if (nm[j] == fo[f]) 0 else border_len(piece, areas[[j]]), 0)
      if (max(bl) > 0.01) { w <- which.max(bl); areas[[w]] <- U(polys(sf::st_union(areas[[w]], piece), crs), crs)
        if (nzchar(fo[f])) log[[length(log) + 1]] <- data.frame(type = 'moved', area = fo[f], m2 = round(A(piece), 1), to = nm[w]) }
      else { lf[[length(lf) + 1]] <- piece; lo[length(lo) + 1] <- fo[f] } }
    if (length(lf) == length(free)) { for (f in seq_along(lf)) { d <- vapply(seq_len(k), function(j) if (empty_g(areas[[j]])) Inf else as.numeric(sf::st_distance(lf[[f]], areas[[j]])), 0)
        if (nzchar(lo[f]) && all(is.infinite(d[nm != lo[f]]))) w <- match(lo[f], nm) else { d[nm == lo[f]] <- Inf; w <- which.min(d) }
        areas[[w]] <- U(polys(sf::st_union(areas[[w]], lf[[f]]), crs), crs) }; lf <- list() }
    free <- lf; fo <- lo }
  # 7. crumbs
  areas <- lapply(areas, function(g) { p <- polys(g, crs); if (length(p) > 1) p <- p[as.numeric(sf::st_area(p)) > 0.5]; U(p, crs) })
  list(areas = areas, log = if (length(log)) do.call(rbind, log) else NULL, delta = vapply(areas, A, 0) - before_a, before = before_a)
}

# UTM sfc -> 4326, snap-rounded to 1 cm, valid on plane and sphere
finalize_geom <- function(g, crs) {
  g <- sf::st_set_precision(g, 100); g <- polys(sf::st_union(sf::st_make_valid(g)), crs); g <- g[as.numeric(sf::st_area(g)) > 0.5]; g <- sf::st_union(g)
  g4 <- sf::st_transform(g, 4326); sf::sf_use_s2(TRUE); on.exit(sf::sf_use_s2(FALSE))
  if (!all(s2::s2_is_valid(g4))) g4 <- sf::st_as_sfc(s2::s2_rebuild(s2::as_s2_geography(g4, check = FALSE))) |> sf::st_set_crs(4326)
  sf::sf_use_s2(FALSE); g4 <- sf::st_set_precision(g4, 1e8); g4 <- sf::st_make_valid(g4)
  g4 <- tryCatch(sf::st_collection_extract(g4, 'POLYGON'), error = function(e) g4); sf::st_union(g4) }

# Finalize every area, then undo overlaps the per-area sphere repair re-introduced: each overlap goes to the area whose CLEANED (planar)
# shape holds more of it; the other area loses it and is re-finalized. Returns an sfc (4326) in the same order as `areas`.
finalize_partition <- function(areas, crs) {
  F <- lapply(areas, finalize_geom, crs = crs)
  back <- function(g) U(polys(sf::st_transform(g, crs), crs), crs)
  safe_int <- function(a, b) tryCatch(polys(sf::st_intersection(a, b), crs), error = function(e) polys(sf::st_intersection(lwgeom::st_snap_to_grid(a, 0.01), lwgeom::st_snap_to_grid(b, 0.01)), crs))
  for (it in 1:4) { Fu <- lapply(F, back); fixed <- 0
    for (i in seq_along(F)) for (j in seq_along(F)) if (i < j) {
      o <- safe_int(Fu[[i]], Fu[[j]]); if (A(o) < 0.5) next
      uo <- U(o, crs); loser <- if (A(polys(sf::st_intersection(uo, areas[[i]]), crs)) >= A(polys(sf::st_intersection(uo, areas[[j]]), crs))) j else i
      cut <- U(polys(sf::st_difference(Fu[[loser]], sf::st_buffer(uo, 0.005)), crs), crs)
      F[[loser]] <- finalize_geom(cut, crs); Fu[[loser]] <- back(F[[loser]]); fixed <- fixed + 1 }
    if (!fixed) break }
  do.call(c, F) }

partition_metrics <- function(sf4326, outline_utm, crs) {
  x <- sf::st_transform(sf4326, crs); g <- sf::st_geometry(x); u <- U(g, crs); ov <- 0
  m <- sf::st_intersects(g, g); for (i in seq_along(m)) for (j in m[[i]]) if (i < j) ov <- ov + A(polys(sf::st_intersection(g[i], g[j]), crs))
  sf::sf_use_s2(TRUE); s2ok <- all(s2::s2_is_valid(sf4326$geometry)); sf::sf_use_s2(FALSE)
  c(gap = A(polys(sf::st_difference(outline_utm, u), crs)), spill = A(polys(sf::st_difference(u, outline_utm), crs)), overlap = ov,
    s2_valid = s2ok, planar_valid = all(sf::st_is_valid(sf4326)), empty = sum(sf::st_is_empty(sf4326)),
    pieces = sum(vapply(seq_along(g), function(i) length(polys(g[i], crs)), 1L))) }

# % of each area's original size that changed hands (symmetric difference / original area). before, after: named lists of UTM sfc.
change_pct <- function(before, after) vapply(names(before), function(n) { b <- before[[n]]; a <- after[[n]]; ab <- A(b)
  if (ab == 0) return(if (empty_g(a)) 0 else 100); 100 * A(polys(sf::st_sym_difference(U(b, sf::st_crs(b)), U(a, sf::st_crs(b))), sf::st_crs(b))) / ab }, 0)
