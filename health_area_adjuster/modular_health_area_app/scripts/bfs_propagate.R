# =============================================================================
# bfs_propagate.R
# Afghanistan Cluster Boundary App
#
# R wrapper around bfs_propagate_cpp() (bfs_propagate.cpp).
#
# Call signature matches mod_cluster_boundaries_tab.R:
#
#   bfs_propagate(
#     seed_sf        = seed_sf_proj,       # sf POINTS, EPSG:3857, col: dfa_name
#     grid_sf        = grid_sf,            # sf grid polygons, EPSG:3857
#     grid_cents     = grid_cents,         # sf centroids, EPSG:3857
#     neighbors      = neighbors,          # list of 0-indexed int vectors from build_neighbor_list()
#     friction_vals  = friction_vals,      # numeric[n_cells] in [0,1]
#     dfa_names      = placed$cluster_name,# character — order matches seed_sf rows
#     target_pops    = target_pops,        # numeric[n_seeds] — WorldPop-scaled target per cluster
#     district_sf    = district_sf         # sf polygon, EPSG:3857 — for barrier construction
#   )
#
# Returns: character vector of cluster names, length n_cells.
#
# Parameters (same defaults as Somalia):
#   base_step_fric  = 0.0
#   compactness_pen = 0.0
#   subdiv_penalty  = 0.0    (no subdivisions in Afghanistan)
#   pop_sat_pct     = 0.90
#   pop_sat_weight  = 5.0
#   pop_sat_max     = 200.0
#   max_cost_val    = Inf
#   starter_radius_m = 100   (expand seed to all cells within 100m)
# =============================================================================

bfs_propagate <- function(seed_sf,
                           grid_sf,
                           grid_cents,
                           neighbors,
                           friction_vals,
                           dfa_names,
                           target_pops    = NULL,
                           district_sf    = NULL,
                           base_step_fric  = 0.0,
                           compactness_pen = 0.0,
                           subdiv_penalty  = 0.0,
                           pop_sat_pct     = 0.90,
                           pop_sat_weight  = 5.0,
                           pop_sat_max     = 200.0,
                           max_cost_val    = Inf,
                           starter_radius_m = 100.0) {

  n_seeds <- nrow(seed_sf)
  n_cells <- nrow(grid_cents)

  if (n_seeds == 0 || n_cells == 0)
    return(rep(NA_character_, n_cells))

  # ── Ensure 3857 ───────────────────────────────────────────────────────────
  seed_sf    <- sf::st_transform(seed_sf,    3857)
  grid_cents <- sf::st_transform(grid_cents, 3857)
  grid_sf_p  <- sf::st_transform(grid_sf,    3857)

  # ── neighbors: convert from 0-based to 1-based for C++ ───────────────────
  # build_neighbor_list() returns 0-indexed; C++ expects 1-based
  neighbors_1based <- lapply(neighbors, function(x) as.integer(x) + 1L)

  # ── Starter cells per seed (nearest cells within starter_radius_m) ────────
  starter_cells_list <- lapply(seq_len(n_seeds), function(i) {
    pt  <- seed_sf[i, ]
    buf <- sf::st_buffer(pt, dist = starter_radius_m)
    # cells whose centroid falls within buffer
    idx_buf <- which(lengths(sf::st_within(grid_cents, buf)) > 0)
    # always include the single nearest cell as fallback
    idx_near <- sf::st_nearest_feature(pt, grid_cents)
    as.integer(unique(c(idx_near, idx_buf)))  # 1-based (R default)
  })

  # ── Hard barriers: district boundary edges ────────────────────────────────
  # For each cell, find neighbours that are NOT in the same planning area.
  # In Afghanistan the planning area IS the district (or sub-area polygon),
  # so any cell whose neighbour lies outside district_sf is a hard barrier.
  barrier_mat <- vector('list', n_cells)
  for (i in seq_len(n_cells)) barrier_mat[[i]] <- integer(0)

  if (!is.null(district_sf) && nrow(district_sf) > 0) {
    district_p <- sf::st_transform(district_sf, 3857)
    district_p <- sf::st_union(district_p)

    # Which cells are inside the district?
    inside <- lengths(sf::st_within(grid_cents, district_p)) > 0

    for (i in which(inside)) {
      nbrs   <- neighbors_1based[[i]]
      outside <- nbrs[!inside[nbrs]]
      if (length(outside) > 0)
        barrier_mat[[i]] <- as.integer(outside)
    }
  }

  # ── No subdivision barriers in Afghanistan ────────────────────────────────
  subdiv_mat <- vector('list', n_cells)
  for (i in seq_len(n_cells)) subdiv_mat[[i]] <- integer(0)

  # ── Population per cell (WorldPop already on grid_sf) ────────────────────
  use_pop  <- FALSE
  cell_pop <- rep(0.0, n_cells)

  if ('u5_pop' %in% names(grid_sf) &&
      !is.null(target_pops) && length(target_pops) == n_seeds) {
    cell_pop <- as.numeric(grid_sf$u5_pop)
    cell_pop[is.na(cell_pop)] <- 0
    use_pop <- TRUE
  }

  # ── friction_vals safety ──────────────────────────────────────────────────
  if (is.null(friction_vals) || length(friction_vals) != n_cells)
    friction_vals <- rep(0.0, n_cells)
  friction_vals <- as.numeric(friction_vals)
  friction_vals[is.na(friction_vals)] <- 0.0
  friction_vals <- pmin(pmax(friction_vals, 0.0), 1.0)

  # ── target_pop per seed — use provided or equal share ────────────────────
  if (is.null(target_pops) || length(target_pops) != n_seeds) {
    district_total <- sum(cell_pop, na.rm = TRUE)
    target_pops    <- rep(district_total / n_seeds, n_seeds)
  }
  # Use mean target_pop across seeds for the C++ scalar argument
  # (C++ uses one target_pop for all areas — population saturation is
  # relative to this value, so the mean gives a reasonable centre)
  target_pop_scalar <- mean(target_pops, na.rm = TRUE)
  if (!is.finite(target_pop_scalar) || target_pop_scalar <= 0)
    target_pop_scalar <- 1.0

  # ── Call C++ ──────────────────────────────────────────────────────────────
  result <- bfs_propagate_cpp(
    n_cells           = n_cells,
    neighbors         = neighbors_1based,
    cell_friction_raw = friction_vals,
    seed_cells        = vapply(starter_cells_list, function(x) x[[1]], integer(1)),
    starter_cells_list = starter_cells_list,
    barrier_mat       = barrier_mat,
    subdiv_mat        = subdiv_mat,
    subdiv_penalty    = subdiv_penalty,
    base_step_fric    = base_step_fric,
    compactness_pen   = compactness_pen,
    max_cost_val      = max_cost_val,
    use_pop           = use_pop,
    cell_pop          = cell_pop,
    target_pop        = target_pop_scalar,
    pop_sat_pct       = pop_sat_pct,
    pop_sat_weight    = pop_sat_weight,
    pop_sat_max       = pop_sat_max,
    n_areas           = n_seeds
  )

  owner_idx <- result$owner   # 0-based area index, -1 = unassigned

  # ── Map indices back to cluster names ────────────────────────────────────
  assignments <- dfa_names[owner_idx + 1L]   # shift to 1-based
  assignments[owner_idx < 0 | is.na(assignments)] <- NA_character_

  # ── Flood-fill any unassigned cells with nearest assigned neighbour ───────
  unassigned <- which(is.na(assignments))
  if (length(unassigned) > 0) {
    assigned_sf <- grid_cents[!is.na(assignments), ]
    if (nrow(assigned_sf) > 0) {
      nearest <- sf::st_nearest_feature(grid_cents[unassigned, ], assigned_sf)
      assigned_names <- assignments[!is.na(assignments)]
      assignments[unassigned] <- assigned_names[nearest]
    } else {
      assignments[unassigned] <- dfa_names[1]
    }
  }

  as.character(assignments)
}
