# =============================================================================
# team_area_helpers.R
#
# The only new generation-side logic Team Areas needs: computing N seed
# points inside a single health area, spread out in proportion to where the
# population actually is. Everything downstream of these seed points (BFS
# propagation, health-area boundary as a hard wall via a grid restricted to
# the health area's own polygon, subdivision penalty, population saturation)
# is handled by the existing initialHealthAreaGenerationServer module — this
# just produces the facility_seed_sf-equivalent input it expects.
#
# This is deliberately self-contained rather than reusing
# initialHealthAreaGenerationServer's internal grid: that module needs
# facility_seed_sf as an input BEFORE it has built its own grid/population
# extraction (seed placement happens early in its scene() pipeline), so
# seed computation can't depend on output that doesn't exist yet. Instead,
# this samples its own coarse population-weighted point grid directly from
# the health area polygon and the WorldPop raster. The result only needs to
# be "reasonable starting locations" — snap_seeds_into_district() (inside
# the main module) places them onto valid cells on ITS finer grid, and the
# actual boundary computation happens there, not here.
#
# Approach: population-weighted Lloyd's algorithm (weighted k-means).
#   1. Sample a coarse grid of candidate points inside the health area.
#   2. Weight each by population (extracted from the WorldPop raster).
#   3. Initial centers: N points sampled with probability proportional to
#      population.
#   4. Assign every candidate point to its nearest current center.
#   5. Recompute each center as the population-weighted centroid of its
#      assigned points.
#   6. Repeat until assignments stop changing or max_iter is reached.
# =============================================================================

#' Population-weighted cluster centers within one health area.
#'
#' @param health_area_sf  sf polygon(s) for a single health area (will be
#'                        unioned to one row if multipart).
#' @param u5_rast         terra SpatRaster of under-5 population (same one
#'                        loaded globally as u5_rast), or NULL to fall back
#'                        to uniform (unweighted) spacing.
#' @param n_teams         integer, number of teams (seed points) to place.
#'                        Minimum 1.
#' @param seed            integer, RNG seed for reproducibility.
#' @param grid_n          integer, resolution of the coarse candidate-point
#'                        grid sampled inside the health area (not the final
#'                        painting grid — just for weighting the clustering).
#' @param max_iter        integer, cap on Lloyd's-algorithm iterations.
#'
#' @return sf POINT object (CRS 3857) with one column `team_name` =
#'   "Team 1", "Team 2", etc. — pass this directly as `facility_seed_sf` to
#'   a second initialHealthAreaGenerationServer instance scoped to this
#'   health area, with `facility_name_col = "team_name"`.
compute_team_area_seeds <- function(health_area_sf, u5_rast, n_teams,
                                    seed = 1, grid_n = 60, max_iter = 15) {
  n_teams <- max(1L, as.integer(round(n_teams)))

  ha <- suppressWarnings(sf::st_make_valid(health_area_sf))
  if (nrow(ha) > 1)
    ha <- sf::st_as_sf(sf::st_union(ha))
  ha_3857 <- sf::st_transform(ha, 3857)

  bbox     <- sf::st_bbox(ha_3857)
  max_dim  <- max(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin)
  cellsize <- max(30, max_dim / grid_n)

  cand_pts <- sf::st_make_grid(ha_3857, cellsize = cellsize, what = "centers")
  cand_sf  <- sf::st_sf(geometry = cand_pts, crs = sf::st_crs(ha_3857))
  inside   <- lengths(sf::st_within(cand_sf, ha_3857)) > 0
  cand_sf  <- cand_sf[inside, ]

  # Very small health areas can yield too few grid points for n_teams —
  # fall back to a denser random sample inside the polygon.
  if (nrow(cand_sf) < max(n_teams * 3L, 10L)) {
    pts     <- sf::st_sample(ha_3857, size = max(n_teams * 20L, 200L), exact = TRUE)
    cand_sf <- sf::st_sf(geometry = pts, crs = sf::st_crs(ha_3857))
  }

  coords  <- sf::st_coordinates(cand_sf)
  n_cand  <- nrow(coords)

  w <- rep(1, n_cand)
  if (!is.null(u5_rast)) {
    cand_ll <- sf::st_transform(cand_sf, sf::st_crs(terra::crs(u5_rast)))
    w_ext <- tryCatch(
      as.numeric(terra::extract(u5_rast, terra::vect(cand_ll))[, 2]),
      error = function(e) NULL
    )
    if (!is.null(w_ext) && length(w_ext) == n_cand) {
      w_ext[is.na(w_ext)] <- 0
      if (sum(w_ext) > 0) w <- w_ext
    }
  }

  # Efficient squared-Euclidean distance between a small set of centers (k)
  # and all candidate points (n) — O(k*n), avoids a full n x n matrix.
  sq_dist_to_centers <- function(centers, pts) {
    cc <- rowSums(centers^2)
    pp <- rowSums(pts^2)
    cp <- centers %*% t(pts)
    outer(cc, pp, "+") - 2 * cp   # k x n
  }

  set.seed(seed)
  n_init     <- min(n_teams, n_cand)
  n_positive <- sum(w > 0)
  if (n_positive >= n_init) {
    init_idx <- sample.int(n_cand, size = n_init, prob = w)
  } else {
    # Fewer population-weighted candidates than seeds needed -- a mostly
    # unpopulated health area with only a couple of small population
    # clusters can easily have fewer positive-weight cells than n_teams.
    # sample.int() errors ("too few positive probabilities") rather than
    # degrading gracefully when asked for more draws than there are
    # positive weights. Take every positively-weighted candidate first,
    # then fill the remaining slots with an UNWEIGHTED draw from the rest
    # of the candidate pool, so every team still gets a starting location
    # somewhere inside the health area rather than crashing the scene.
    # (Indexing via sample.int(length(...)) rather than sample(pool, ...)
    # directly avoids the classic R footgun where sample() on a
    # length-1 vector samples from 1:x instead of treating x as the
    # single candidate.)
    positive_idx   <- which(w > 0)
    remaining_pool <- setdiff(seq_len(n_cand), positive_idx)
    remaining_n    <- n_init - length(positive_idx)
    fill_idx <- remaining_pool[
      sample.int(length(remaining_pool), size = remaining_n,
                 replace = length(remaining_pool) < remaining_n)
    ]
    init_idx <- c(positive_idx, fill_idx)
  }
  centers  <- coords[init_idx, , drop = FALSE]
  if (nrow(centers) < n_teams) {
    extra <- n_teams - nrow(centers)
    pad   <- coords[sample.int(n_cand, extra, replace = TRUE), , drop = FALSE]
    pad   <- pad + matrix(stats::rnorm(extra * 2, sd = cellsize * 0.25), ncol = 2)
    centers <- rbind(centers, pad)
  }

  assign_vec <- rep(1L, n_cand)
  for (iter in seq_len(max_iter)) {
    d          <- sq_dist_to_centers(centers, coords)   # n_teams x n_cand
    new_assign <- apply(d, 2, which.min)
    converged  <- identical(new_assign, assign_vec) && iter > 1L
    assign_vec <- new_assign
    if (converged) break

    for (k in seq_len(n_teams)) {
      idx <- which(assign_vec == k)
      if (length(idx) == 0) next   # empty cluster — leave its center where it was
      wk <- w[idx]
      if (sum(wk) <= 0) wk <- rep(1, length(wk))
      centers[k, 1] <- stats::weighted.mean(coords[idx, 1], wk)
      centers[k, 2] <- stats::weighted.mean(coords[idx, 2], wk)
    }
  }

  pts_sfc <- sf::st_sfc(
    lapply(seq_len(n_teams), function(k) sf::st_point(centers[k, ])),
    crs = 3857
  )
  sf::st_sf(team_name = paste("Team", seq_len(n_teams)), geometry = pts_sfc)
}

#' Number of teams for a health area, from admin-configured generation
#' settings (target_pop_per_team), falling back to a sane default if the
#' setting is missing.
#'
#' @param area_pop     numeric, total under-5 population of the health area.
#' @param campaign_id  integer or NULL — per-campaign override if set.
compute_n_teams <- function(area_pop, campaign_id = NULL) {
  target <- tryCatch(
    db_get_generation_setting(pool, "target_pop_per_team", campaign_id = campaign_id),
    error = function(e) NA_real_
  )
  if (is.na(target) || target <= 0) target <- 400
  max(1L, as.integer(ceiling(area_pop / target)))
}

# =============================================================================
# NOTE: automatic reconciliation (re-rasterizing a stale team-area version
# onto a health area's changed boundary via weighted BFS) was removed —
# the model is deliberately simpler now: a team-area version stays
# permanently pinned to the health-area version it was drawn against
# (based_on_health_area_version_id). Once any team-area version is
# current for a district, the health-area track locks unconditionally —
# there is no admin override that swaps the current health-area version
# out from under existing team-area work. To work on a health area whose
# pinned version isn't current, that health-area version has to be made
# current again (see db_publish_version() / mod_admin_tab_v2.R's District
# review section) — nothing here tries to reconcile a mismatch
# automatically.
# =============================================================================
