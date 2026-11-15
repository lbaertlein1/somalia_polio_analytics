# =============================================================================
# Variable paint grid ("finer grid in towns")
#
# The standard paint grid is one regular square grid over the district
# (make_paint_grid() in mod_initial_health_area_generation.R). The variable
# grid is THE SAME coarse grid -- same origin, same cell size, same cell order
# -- with some of its cells split into k x k fine cells. Every cell sits on
# one fine lattice (cell size cell_m / k): a coarse cell covers k x k lattice
# squares, a fine cell one. Each cell carries
#   row, col  -- lattice position of its south-west corner
#   size      -- lattice squares per side (k for coarse cells, 1 for fine)
# so cell corners are lattice points ("row_col"), shared exactly by every
# cell that touches them -- the paint/refine boundary tracing in
# paint-app.js relies on that (no T-junctions).
#
# The spec is stored per version (mapping_versions.paint_grid_spec). NULL =
# the standard grid, built exactly as before; every version saved before
# this existed has NULL and is untouched. The spec stores the LIST of split
# cells, not the rule that chose them, so a later WorldPop or threshold
# change can never reshuffle the cells under a saved painting.
# =============================================================================

.vg_dist_3857 <- function(district_sf) {
  d <- sf::st_transform(safe_make_valid(district_sf), 3857)
  sf::st_sf(geometry = sf::st_union(sf::st_geometry(d)))
}

#' Decide which coarse cells to split. Returns the spec (a plain list) or
#' NULL when nothing qualifies (the caller then keeps the standard grid).
#'   grid_n      -- the same grid_n the standard grid uses (coarse cell size
#'                  = district max dimension / grid_n, as now)
#'   u5_rast     -- WorldPop U5 raster; coarse cells with at least
#'                  density_km2 U5 per km2 (urban) can be split, densest first
#'   sites_sf    -- coordination sites; a site with another site closer
#'                  than 3 coarse cells splits the cells around it (first
#'                  priority, so close sites can keep their own areas)
#'   fine_m      -- target fine cell size: k = cell_m / fine_m rounded (at
#'                  least 2), so fine cells are cell_m / k
#'   extra_cells -- most cells the split may ADD to the standard grid (the
#'                  standard grid itself stays as large as it is today):
#'                  the highest-priority cells are split until it is used
make_grid_spec <- function(district_sf, grid_n, u5_rast = NULL, sites_sf = NULL,
                           fine_m = 60, density_km2 = 400, extra_cells = 20000) {
  d3857   <- .vg_dist_3857(district_sf)
  bb      <- sf::st_bbox(d3857)
  max_dim <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin)
  cell_m  <- as.numeric(max_dim / grid_n)
  k       <- max(2L, as.integer(round(cell_m / fine_m)))
  if (cell_m / k < 0.75 * fine_m) return(NULL)   # standard cells already about the target size
  ncols   <- as.integer(ceiling((bb$xmax - bb$xmin) / cell_m))
  nrows   <- as.integer(ceiling((bb$ymax - bb$ymin) / cell_m))
  x0 <- as.numeric(bb$xmin); y0 <- as.numeric(bb$ymin)

  # Coarse cells, same order as st_make_grid (bottom row first, left to right)
  full <- sf::st_make_grid(d3857, cellsize = cell_m, what = 'polygons', square = TRUE)
  stopifnot(length(full) == ncols * nrows)
  touches <- lengths(sf::st_intersects(full, d3857)) > 0
  cent    <- suppressWarnings(sf::st_centroid(full))
  inside  <- lengths(sf::st_intersects(cent, d3857)) > 0

  u5 <- numeric(length(full))
  if (!is.null(u5_rast) && any(touches)) {
    v <- tryCatch(exactextractr::exact_extract(raster::raster(u5_rast),
                    sf::st_transform(sf::st_sf(geometry = full[touches]), sf::st_crs(terra::crs(u5_rast))),
                    fun = 'sum', progress = FALSE), error = function(e) rep(0, sum(touches)))
    v[is.na(v)] <- 0; u5[touches] <- v
  }
  dens <- u5 / (cell_m^2 / 1e6)

  rc  <- function(idx) list(r = (idx - 1L) %/% ncols, c = (idx - 1L) %% ncols)
  ring <- function(idx) {          # idx plus its 8 neighbours, inside the grid
    p <- rc(idx); out <- integer()
    for (dr in -1:1) for (dc in -1:1) {
      r <- p$r + dr; c <- p$c + dc
      ok <- r >= 0 & r < nrows & c >= 0 & c < ncols
      out <- c(out, (r * ncols + c + 1L)[ok])
    }
    unique(out)
  }

  # Sites close to another site need fine cells to keep their own areas
  site_cells <- integer()
  if (!is.null(sites_sf) && nrow(sites_sf) > 1) {
    s <- sf::st_transform(sites_sf, 3857); xy <- sf::st_coordinates(s)
    dm <- as.matrix(stats::dist(xy)); diag(dm) <- Inf
    close <- which(apply(dm, 1, min) < 3 * cell_m)
    if (length(close)) {
      cc <- floor((xy[close, 1] - x0) / cell_m); rr <- floor((xy[close, 2] - y0) / cell_m)
      ok <- cc >= 0 & cc < ncols & rr >= 0 & rr < nrows
      site_cells <- unique(as.integer(rr[ok] * ncols + cc[ok] + 1L))
    }
  }

  # Priority: close-site cells first, then urban cells densest first. Each
  # chosen cell is split together with its 8 neighbours (so town edges and
  # the cells around a site are fine too); take as many as the budget
  # allows (largest prefix of the priority list whose split fits).
  urban <- which(touches & dens >= density_km2); urban <- urban[order(-dens[urban])]
  prio  <- unique(c(site_cells[touches[site_cells]], urban))
  if (!length(prio)) return(NULL)
  budget <- floor(extra_cells / (k * k - 1))
  split_of <- function(m) { f <- unique(unlist(lapply(prio[seq_len(m)], ring))); sort(f[touches[f]]) }
  lo <- 0L; hi <- length(prio)
  while (lo < hi) { mid <- (lo + hi + 1L) %/% 2L; if (length(split_of(mid)) <= budget) lo <- mid else hi <- mid - 1L }
  if (lo == 0L) return(NULL)
  fine <- split_of(lo)
  last <- prio[lo]

  list(type = 'aligned_blocks', v = 1L, cell_m = cell_m, k = k, x0 = x0, y0 = y0,
       ncols = ncols, nrows = nrows, fine = as.integer(fine),
       density_km2 = round(dens[last]), fine_m = cell_m / k,
       n_urban = length(urban), n_split_seeds = lo)
}

# A spec read back from the database (jsonlite, simplifyVector = FALSE)
# has lists where vectors were; this makes it the same shape as a fresh one.
normalize_grid_spec <- function(spec) {
  if (is.null(spec) || length(spec) == 0) return(NULL)
  if (!identical(spec$type, 'aligned_blocks')) return(NULL)
  num <- function(x) as.numeric(unlist(x))
  list(type = 'aligned_blocks', v = as.integer(num(spec$v)), cell_m = num(spec$cell_m), k = as.integer(num(spec$k)),
       x0 = num(spec$x0), y0 = num(spec$y0), ncols = as.integer(num(spec$ncols)), nrows = as.integer(num(spec$nrows)),
       fine = as.integer(num(spec$fine)), density_km2 = num(spec$density_km2), fine_m = num(spec$fine_m))
}

# Short signature of a spec, for scene keys: a different grid means
# different cells, so it is a different scene. 'std' = standard grid.
grid_spec_sig <- function(spec) {
  if (is.null(spec)) return('std')
  paste('vg', spec$k, round(spec$cell_m, 3), length(spec$fine), sum(as.numeric(spec$fine)), sep = ':')
}

grid_spec_json <- function(spec) {
  if (is.null(spec)) return(NA_character_)
  as.character(jsonlite::toJSON(spec, auto_unbox = TRUE, digits = NA))
}

#' Build the variable grid from a spec. Same return shape as
#' make_paint_grid() (grid_sf in 4326 with cell_id/row/col/centroids), plus
#' `size` per cell and `lattice` (owner matrix used for neighbours/edges).
build_variable_paint_grid <- function(district_sf, spec) {
  d3857 <- .vg_dist_3857(district_sf)
  k <- spec$k; f <- spec$cell_m / k; nc <- spec$ncols; nr <- spec$nrows
  # The spec must still cover the outline (it can stop covering it if the
  # outline grew since -- e.g. a health area edited after its team layout
  # was saved). NULL = caller falls back to the standard grid, and the
  # saved painting is re-derived from the saved shapes, as for any grid
  # that no longer matches.
  bb0 <- sf::st_bbox(d3857); tol <- 0.01
  if (bb0$xmin < spec$x0 - tol || bb0$ymin < spec$y0 - tol ||
      bb0$xmax > spec$x0 + nc * spec$cell_m + tol || bb0$ymax > spec$y0 + nr * spec$cell_m + tol) {
    message('[variable_grid] saved grid no longer covers the outline -- standard grid')
    return(NULL)
  }
  n_full <- nc * nr
  is_fine <- logical(n_full); is_fine[spec$fine[spec$fine >= 1 & spec$fine <= n_full]] <- TRUE

  idx  <- seq_len(n_full); cr <- (idx - 1L) %/% nc; cc <- (idx - 1L) %% nc
  # coarse cells (not split): kept when their centre is inside, as now
  co <- which(!is_fine)
  co_pts <- sf::st_as_sf(data.frame(x = spec$x0 + (cc[co] + 0.5) * spec$cell_m, y = spec$y0 + (cr[co] + 0.5) * spec$cell_m),
                         coords = c('x', 'y'), crs = 3857)
  co_keep <- co[lengths(sf::st_intersects(co_pts, d3857)) > 0]
  # fine cells: each split cell's k x k sub-cells, kept when their own centre is inside
  fi <- which(is_fine)
  sub <- expand.grid(j = 0:(k - 1L), i = 0:(k - 1L))           # j fastest: row-major within the block
  fdf <- data.frame(parent = rep(fi, each = nrow(sub)), i = rep(sub$i, length(fi)), j = rep(sub$j, length(fi)))
  fdf$row <- cr[fdf$parent] * k + fdf$i; fdf$col <- cc[fdf$parent] * k + fdf$j
  if (nrow(fdf)) {
    f_pts <- sf::st_as_sf(data.frame(x = spec$x0 + (fdf$col + 0.5) * f, y = spec$y0 + (fdf$row + 0.5) * f), coords = c('x', 'y'), crs = 3857)
    fdf <- fdf[lengths(sf::st_intersects(f_pts, d3857)) > 0, , drop = FALSE]
  }
  cells <- rbind(
    data.frame(parent = co_keep, sub = 0L, row = cr[co_keep] * k, col = cc[co_keep] * k, size = k),
    if (nrow(fdf)) data.frame(parent = fdf$parent, sub = fdf$i * k + fdf$j + 1L, row = fdf$row, col = fdf$col, size = 1L)
  )
  cells <- cells[order(cells$parent, cells$sub), , drop = FALSE]
  n <- nrow(cells); if (n == 0) stop('variable grid: no cells inside the district')

  # Cell outlines through EVERY lattice point along their sides, with each
  # lattice coordinate computed once (X[c], Y[r]) -- so a coarse cell and the
  # fine cells beside it share bit-identical vertices and merge cleanly into
  # area shapes (a 4-corner coarse outline left slivers against them).
  # Fine cells: the same 4 corners as before.
  X <- spec$x0 + (0:(nc * k)) * f; Y <- spec$y0 + (0:(nr * k)) * f
  geoms <- lapply(seq_len(n), function(q) {
    r <- cells$row[q]; c <- cells$col[q]; s <- cells$size[q]; t <- seq_len(s) - 1L
    ci <- c(c + t, rep(c + s, s), c + s - t, rep(c, s), c) + 1L       # S, E, N, W sides, closed
    ri <- c(rep(r, s), r + t, rep(r + s, s), r + s - t, r) + 1L
    sf::st_polygon(list(cbind(X[ci], Y[ri])))
  })
  grid_sf <- sf::st_sf(cell_id = seq_len(n), row = as.integer(cells$row), col = as.integer(cells$col),
                       size = as.integer(cells$size), geometry = sf::st_sfc(geoms, crs = 3857))
  cent <- sf::st_as_sf(data.frame(x = spec$x0 + (cells$col + cells$size / 2) * f, y = spec$y0 + (cells$row + cells$size / 2) * f),
                       coords = c('x', 'y'), crs = 3857)
  xy <- sf::st_coordinates(sf::st_transform(cent, 4326))
  grid_sf <- sf::st_transform(grid_sf, 4326)
  grid_sf$centroid_lon <- xy[, 1]; grid_sf$centroid_lat <- xy[, 2]

  # lattice owner matrix: lattice square (r, c) -> cell position (0 = none)
  L <- matrix(0L, nrow = nr * k, ncol = nc * k)
  for (q in seq_len(n)) {
    s <- cells$size[q]; rr <- cells$row[q] + seq_len(s); cl <- cells$col[q] + seq_len(s)
    L[rr, cl] <- q
  }
  bb <- sf::st_bbox(d3857)
  message('[variable_grid] cells: ', n, ' (', sum(cells$size == 1L), ' fine at ', round(f), ' m, ',
          sum(cells$size > 1L), ' coarse at ', round(spec$cell_m), ' m)')
  list(grid_sf = grid_sf, max_dim_m = as.numeric(max(bb$xmax - bb$xmin, bb$ymax - bb$ymin)),
       ncols_full = NULL, nrows_full = NULL, inside_mask = NULL, lattice = L, grid_spec = spec)
}

# Lattice squares on the ring just outside a cell (its 4 sides, plus the 4
# corner squares when diagonal neighbours count). Out-of-lattice squares
# come back as 0 owners.
.vg_ring_owners <- function(L, r0, c0, s, diagonal = TRUE) {
  nr <- nrow(L); nc <- ncol(L)
  rr <- c(rep(r0 - 1L, s), rep(r0 + s, s), r0 + seq_len(s) - 1L, r0 + seq_len(s) - 1L)
  cl <- c(c0 + seq_len(s) - 1L, c0 + seq_len(s) - 1L, rep(c0 - 1L, s), rep(c0 + s, s))
  if (diagonal) { rr <- c(rr, r0 - 1L, r0 - 1L, r0 + s, r0 + s); cl <- c(cl, c0 - 1L, c0 + s, c0 - 1L, c0 + s) }
  ok <- rr >= 0L & rr < nr & cl >= 0L & cl < nc
  out <- integer(length(rr)); out[ok] <- L[cbind(rr[ok] + 1L, cl[ok] + 1L)]
  out
}

# Side-sharing cells, plus diagonal (corner-only) cells of the SAME size when
# allow_diagonal -- as on the standard grid. A corner-only step between a
# fine and a coarse cell would let an area jump from one 60 m cell into a
# 500 m cell touching it at a single point (blocks hanging off a corner).
variable_grid_neighbors <- function(grid_sf, lattice, allow_diagonal = TRUE) {
  n <- nrow(grid_sf); out <- vector('list', n); sz <- grid_sf$size
  for (q in seq_len(n)) {
    s <- sz[q]
    o <- .vg_ring_owners(lattice, grid_sf$row[q], grid_sf$col[q], s, allow_diagonal)
    if (allow_diagonal) {
      corner <- utils::tail(seq_along(o), 4L)
      keep_c <- o[corner] > 0L & sz[pmax(o[corner], 1L)] == s
      o <- c(o[-corner], o[corner][keep_c])
    }
    out[[q]] <- sort(unique(o[o > 0L & o != q]))
  }
  names(out) <- as.character(grid_sf$cell_id)
  out
}

# Same meaning as build_edge_list(): a cell is on the district edge if any
# neighbouring lattice square (8-neighbourhood) has no cell.
variable_grid_edge_list <- function(grid_sf, lattice) {
  n <- nrow(grid_sf)
  flag <- vapply(seq_len(n), function(q)
    any(.vg_ring_owners(lattice, grid_sf$row[q], grid_sf$col[q], grid_sf$size[q], TRUE) == 0L), logical(1))
  out <- as.list(flag); names(out) <- as.character(grid_sf$cell_id)
  out
}
