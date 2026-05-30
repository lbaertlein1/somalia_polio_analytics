# NOTE: bfs_propagate.cpp must be compiled before this module is used.
# Add this line to global.R (adjust path to match your project layout):
#
#   Rcpp::sourceCpp("path/to/bfs_propagate.cpp")
#
# bfs_propagate_cpp() will then be available in the global environment.

initialHealthAreaGenerationServer <- function(
    id,
    district_sf,           # planning area sf (urban/rural/full) — used for grid
    friction_district_sf = NULL,  # full district sf for friction path lookup;
    # defaults to district_sf when NULL
    grid_n,
    n_dfa = 5,
    seed = 1,
    facility_seed_sf = reactive(NULL),
    facility_name_col = NULL,
    barrier_lines_sf = reactive(NULL),
    barrier_penalty = 1e6,
    subdivision_lines_sf = reactive(NULL),
    subdivision_boundary_penalty = 0.99,
    compactness_penalty = 0,
    allow_diagonal = TRUE,
    max_cost = Inf,
    friction_dir = file.path(getwd(), "data", "friction", "district_standardized"),
    friction_lookup_csv = file.path(
      getwd(), "data", "friction", "district_standardized", "district_friction_index.csv"
    ),
    # WorldPop raster — if provided, enables population saturation penalty
    u5_rast = NULL,
    # Raw friction addition per step (0-1 scale), independent of terrain.
    # Adds a small baseline cost to every move, limiting runaway expansion
    # into empty areas. 0.05 = +5% friction per step.
    base_step_friction = 0.05,
    # Fraction of target_pop at which saturation penalty starts (0-1)
    pop_saturation_pct = 0.75,
    # Rate at which saturation adds to raw friction per unit of excess.
    # At excess = 1.0 (double the target), adds pop_saturation_weight to friction.
    # e.g. 0.4 means at 2x target pop, raw friction increases by 0.4
    pop_saturation_weight = 0.4,
    # Hard cap on how much population saturation can add to raw friction (0-1)
    pop_saturation_max = 0.4
) {
  moduleServer(id, function(input, output, session) {
    
    safe_make_valid <- function(x) {
      if (is.null(x)) return(x)
      suppressWarnings(sf::st_make_valid(x))
    }
    
    std_name <- function(x) {
      x |> tolower() |>
        gsub("[^a-z0-9]+", "_", x = _) |>
        gsub("^_+|_+$", "", x = _)
    }
    
    make_paint_grid <- function(district_sf, grid_n = 150) {
      district_sf   <- safe_make_valid(district_sf)
      district_3857 <- sf::st_transform(district_sf, 3857)
      bbox     <- sf::st_bbox(district_3857)
      max_dim  <- max(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin)
      cellsize <- max_dim / grid_n
      
      raw_grid <- sf::st_make_grid(district_3857, cellsize = cellsize,
                                   what = "polygons", square = TRUE)
      
      # Record full grid dimensions BEFORE filtering — needed for arithmetic
      # neighbour lookup. st_make_grid fills bbox row-by-row (bottom to top),
      # ncols = ceil((xmax-xmin)/cellsize), nrows = ceil((ymax-ymin)/cellsize).
      ncols_full <- ceiling((bbox$xmax - bbox$xmin) / cellsize)
      nrows_full <- ceiling((bbox$ymax - bbox$ymin) / cellsize)
      n_full     <- ncols_full * nrows_full
      
      full_idx <- seq_along(raw_grid)   # 1-based index in full grid
      
      grid_sf_full <- sf::st_sf(
        full_idx = full_idx,
        geometry = raw_grid,
        crs      = sf::st_crs(district_3857)
      )
      
      cent_3857 <- suppressWarnings(sf::st_centroid(grid_sf_full))
      inside    <- lengths(sf::st_within(cent_3857, district_3857)) > 0
      
      grid_sf   <- grid_sf_full[inside, ]
      grid_sf$cell_id  <- seq_len(nrow(grid_sf))
      
      cent_inside <- cent_3857[inside, ]
      coords      <- sf::st_coordinates(sf::st_transform(cent_inside, 4326))
      grid_sf     <- sf::st_transform(grid_sf, 4326)
      grid_sf$centroid_lon <- coords[, 1]
      grid_sf$centroid_lat <- coords[, 2]
      
      list(
        grid_sf     = grid_sf,
        max_dim_m   = as.numeric(max_dim),
        ncols_full  = as.integer(ncols_full),
        nrows_full  = as.integer(nrows_full),
        inside_mask = inside   # logical length n_full; TRUE = cell kept
      )
    }
    
    build_seed_points_outputs <- function(seed_points_sf) {
      seed_pts    <- sf::st_transform(seed_points_sf, 4326)
      seed_coords <- sf::st_coordinates(seed_pts)
      seed_points_df <- data.frame(
        dfa_name = as.character(seed_pts$dfa_name),
        lon = seed_coords[, 1], lat = seed_coords[, 2],
        stringsAsFactors = FALSE
      )
      seed_points_list <- lapply(seq_len(nrow(seed_points_df)), function(i) {
        list(dfa_name = seed_points_df$dfa_name[i],
             lon = unname(seed_points_df$lon[i]),
             lat = unname(seed_points_df$lat[i]))
      })
      list(seed_points_sf = seed_pts, seed_points_df = seed_points_df, seed_points_list = seed_points_list)
    }
    
    build_neighbors_list <- function(grid_sf, allow_diagonal = TRUE,
                                     ncols_full = NULL, inside_mask = NULL) {
      # Fast arithmetic path: for a regular square grid, neighbours are at
      # known index offsets in the full (unfiltered) grid. No spatial ops needed.
      # Falls back to st_touches if grid metadata is unavailable.
      if (!is.null(ncols_full) && !is.null(inside_mask)) {
        n        <- nrow(grid_sf)
        # Map from filtered position (1-based) to full-grid index (1-based)
        full_ids <- grid_sf$full_idx   # stored by make_paint_grid
        # Reverse map: full_idx -> filtered position (0 = not in district)
        max_full <- max(full_ids)
        full_to_pos <- integer(max_full)
        full_to_pos[full_ids] <- seq_len(n)
        
        offsets <- if (isTRUE(allow_diagonal))
          c(-ncols_full - 1L, -ncols_full, -ncols_full + 1L,
            -1L,                             1L,
            ncols_full - 1L,  ncols_full,  ncols_full + 1L)
        else
          c(-ncols_full, -1L, 1L, ncols_full)
        
        neighbors_list <- vector("list", n)
        for (i in seq_len(n)) {
          fi      <- full_ids[i]
          fi_row  <- (fi - 1L) %/% ncols_full
          nbr_fis <- fi + offsets
          # Exclude out-of-bounds and wrap-around (left/right edge)
          valid   <- nbr_fis >= 1L & nbr_fis <= max_full
          # Prevent row wrap: neighbours must be in adjacent rows
          nbr_rows <- (nbr_fis - 1L) %/% ncols_full
          valid    <- valid & abs(nbr_rows - fi_row) <= 1L
          nbr_fis  <- nbr_fis[valid]
          # Keep only cells that are inside the district
          pos <- full_to_pos[nbr_fis]
          neighbors_list[[i]] <- pos[pos > 0L]
        }
        names(neighbors_list) <- as.character(grid_sf$cell_id)
        return(neighbors_list)
      }
      
      # Fallback: spatial st_touches (slow but always correct)
      grid_3857  <- sf::st_transform(grid_sf, 3857)
      touch_list <- if (isTRUE(allow_diagonal)) sf::st_touches(grid_3857) else
        sf::st_relate(grid_3857, grid_3857, pattern = "F***1****")
      neighbors_list <- lapply(touch_list, as.integer)
      names(neighbors_list) <- as.character(grid_sf$cell_id)
      neighbors_list
    }
    
    build_edge_list <- function(grid_sf, district_sf,
                                ncols_full = NULL, inside_mask = NULL) {
      # Fast arithmetic path: a cell is on the district edge if any of its
      # 8 neighbours (in the full grid) is outside the district.
      if (!is.null(ncols_full) && !is.null(inside_mask) &&
          "full_idx" %in% names(grid_sf)) {
        n        <- nrow(grid_sf)
        full_ids <- grid_sf$full_idx
        max_full <- length(inside_mask)
        offsets  <- c(-ncols_full - 1L, -ncols_full, -ncols_full + 1L,
                      -1L,                             1L,
                      ncols_full - 1L,  ncols_full,  ncols_full + 1L)
        edge_flag <- logical(n)
        for (i in seq_len(n)) {
          fi       <- full_ids[i]
          fi_row   <- (fi - 1L) %/% ncols_full
          nbr_fis  <- fi + offsets
          valid    <- nbr_fis >= 1L & nbr_fis <= max_full
          nbr_rows <- (nbr_fis - 1L) %/% ncols_full
          valid    <- valid & abs(nbr_rows - fi_row) <= 1L
          nbr_fis  <- nbr_fis[valid]
          # Edge if any valid neighbour position is outside the district
          # OR if any offset goes out of the full grid bounds (true boundary)
          if (any(!valid) || any(!inside_mask[nbr_fis])) edge_flag[i] <- TRUE
        }
        edge_list <- as.list(edge_flag)
        names(edge_list) <- as.character(grid_sf$cell_id)
        return(edge_list)
      }
      
      # Fallback: spatial st_intersects
      grid_sf_3857  <- sf::st_transform(grid_sf, 3857)
      district_3857 <- sf::st_transform(district_sf, 3857)
      cell_bbox     <- sf::st_bbox(grid_sf_3857[1, ])
      edge_buffer   <- max(cell_bbox["xmax"] - cell_bbox["xmin"],
                           cell_bbox["ymax"] - cell_bbox["ymin"]) * 0.05
      district_boundary_3857 <- sf::st_boundary(district_3857) |>
        sf::st_buffer(edge_buffer)
      edge_flag <- lengths(sf::st_intersects(grid_sf_3857,
                                             district_boundary_3857)) > 0
      edge_list <- as.list(edge_flag)
      names(edge_list) <- as.character(grid_sf$cell_id)
      edge_list
    }
    
    infer_admin_fields <- function(district_sf) {
      nm <- names(district_sf); nm_low <- tolower(nm)
      find_first <- function(candidates) {
        hit <- nm[match(candidates, nm_low, nomatch = 0)]; hit <- hit[hit != ""]
        if (length(hit) == 0) return(NA_character_); hit[1]
      }
      list(
        zone_field     = find_first(c("zone", "state", "admin1", "state_name", "zone_name")),
        region_field   = find_first(c("region", "admin2", "region_name")),
        district_field = find_first(c("district", "admin3", "district_name"))
      )
    }
    
    get_district_friction_path <- function(district_sf, friction_dir, friction_lookup_csv = NULL) {
      stopifnot(nrow(district_sf) == 1)
      fields <- infer_admin_fields(district_sf)
      if (any(is.na(unlist(fields)))) stop("Could not infer zone/region/district field names.")
      zone_val     <- std_name(as.character(district_sf[[fields$zone_field]][1]))
      region_val   <- std_name(as.character(district_sf[[fields$region_field]][1]))
      district_val <- std_name(as.character(district_sf[[fields$district_field]][1]))
      direct_path  <- file.path(friction_dir, paste0(zone_val, "__", region_val, "__", district_val, "__friction_100m.tif"))
      if (file.exists(direct_path)) return(direct_path)
      if (!is.null(friction_lookup_csv) && file.exists(friction_lookup_csv)) {
        idx <- tryCatch(utils::read.csv(friction_lookup_csv, stringsAsFactors = FALSE), error = function(e) NULL)
        if (!is.null(idx) && nrow(idx) > 0) {
          idx_names    <- tolower(names(idx))
          zone_col     <- names(idx)[match(c("zone","state","admin1"), idx_names, nomatch=0)][1]
          region_col   <- names(idx)[match(c("region","admin2"),       idx_names, nomatch=0)][1]
          district_col <- names(idx)[match(c("district","admin3"),     idx_names, nomatch=0)][1]
          file_col     <- names(idx)[match(c("file_name","filename","file","path"), idx_names, nomatch=0)][1]
          if (!any(is.na(c(zone_col, region_col, district_col, file_col)))) {
            idx$zone_std <- std_name(idx[[zone_col]]); idx$region_std <- std_name(idx[[region_col]]); idx$district_std <- std_name(idx[[district_col]])
            hit <- idx |> dplyr::filter(.data$zone_std == zone_val, .data$region_std == region_val, .data$district_std == district_val)
            if (nrow(hit) > 0) {
              candidate <- hit[[file_col]][1]
              if (file.exists(candidate)) return(candidate)
              candidate2 <- file.path(friction_dir, basename(candidate))
              if (file.exists(candidate2)) return(candidate2)
            }
          }
        }
      }
      tif_files <- list.files(friction_dir, pattern = "\\.tif$", full.names = TRUE)
      hit <- tif_files[grepl(paste(zone_val, region_val, district_val, sep="__"), basename(tif_files), fixed=TRUE)]
      if (length(hit) >= 1) return(hit[1])
      stop(paste0("No district friction raster found for: ", zone_val, " / ", region_val, " / ", district_val))
    }
    
    extract_cell_friction_from_raster <- function(grid_sf, district_sf, friction_path) {
      if (!file.exists(friction_path)) stop("Friction raster not found: ", friction_path)
      r     <- terra::rast(friction_path)
      vals  <- as.numeric(terra::extract(r, terra::centroids(terra::vect(sf::st_transform(grid_sf, terra::crs(r)))))[, 2])
      if (all(is.na(vals))) stop("All friction values are NA for: ", friction_path)
      if (anyNA(vals)) vals[is.na(vals)] <- stats::median(vals, na.rm = TRUE)
      vals[!is.finite(vals)] <- stats::median(vals[is.finite(vals)], na.rm = TRUE)
      pmin(pmax(vals, 0), 1)
    }
    
    extract_cell_population <- function(grid_sf, u5_rast) {
      if (is.null(u5_rast)) return(rep(0, nrow(grid_sf)))
      grid_proj <- sf::st_transform(grid_sf, sf::st_crs(terra::crs(u5_rast)))
      vals <- tryCatch(
        exactextractr::exact_extract(x = raster::raster(u5_rast), y = grid_proj, fun = "sum"),
        error = function(e) as.numeric(terra::extract(u5_rast, terra::centroids(terra::vect(grid_proj)))[, 2])
      )
      vals[is.na(vals)] <- 0
      as.numeric(vals)
    }
    
    compute_distance_friction <- function(grid_sf, facility_sf, flat_km = 5, inflect_km = 10, steepness = 2.5) {
      if (is.null(facility_sf) || nrow(facility_sf) == 0) return(rep(0, nrow(grid_sf)))
      grid_cent <- suppressWarnings(sf::st_centroid(sf::st_transform(grid_sf, 3857)))
      dist_km   <- apply(as.matrix(sf::st_distance(grid_cent, sf::st_transform(facility_sf, 3857))), 1, min) / 1000
      penalty   <- 1 / (1 + exp(-(dist_km  - inflect_km) / steepness))
      baseline  <- 1 / (1 + exp(-(flat_km  - inflect_km) / steepness))
      pmax(0, (penalty - baseline) / (1 - baseline))
    }
    
    friction_to_cost <- function(x, min_cost = 1, soft_block_cost = 1000, knee = 0.90, power_low = 2, power_high = 6) {
      x <- pmin(pmax(x, 0), 1)
      ifelse(x <= knee,
             min_cost + ((x / knee) ^ power_low) * 99,
             100 + (((x - knee) / (1 - knee)) ^ power_high) * (soft_block_cost - 100)
      )
    }
    
    build_barrier_crossing_matrix <- function(grid_sf, neighbors_list, barrier_lines_sf) {
      n   <- nrow(grid_sf)
      out <- vector("list", n)
      names(out) <- as.character(grid_sf$cell_id)
      if (is.null(barrier_lines_sf) || nrow(barrier_lines_sf) == 0) {
        for (i in seq_len(n)) out[[i]] <- integer(0)
        return(out)
      }
      
      grid_3857    <- sf::st_transform(grid_sf, 3857)
      cent_3857    <- suppressWarnings(sf::st_centroid(grid_3857))
      barrier_3857 <- safe_make_valid(sf::st_transform(barrier_lines_sf, 3857))
      coords_m     <- sf::st_coordinates(cent_3857)   # matrix, no per-cell extraction
      
      # Build all unique forward edges (i < j) as a two-column matrix
      edge_i <- integer(0); edge_j <- integer(0)
      for (i in seq_len(n)) {
        nbrs <- neighbors_list[[as.character(grid_sf$cell_id[i])]]
        fwd  <- nbrs[nbrs > i]
        if (length(fwd) > 0) {
          edge_i <- c(edge_i, rep(i, length(fwd)))
          edge_j <- c(edge_j, fwd)
        }
      }
      
      if (length(edge_i) == 0) {
        for (i in seq_len(n)) out[[i]] <- integer(0)
        return(out)
      }
      
      # Build all centroid-to-centroid segments as a single sfc — one
      # bulk st_intersects call instead of one per edge
      segs_sfc <- sf::st_sfc(
        mapply(function(i, j) {
          sf::st_linestring(rbind(coords_m[i, , drop = FALSE],
                                  coords_m[j, , drop = FALSE]))
        }, edge_i, edge_j, SIMPLIFY = FALSE),
        crs = sf::st_crs(cent_3857)
      )
      
      hits <- lengths(sf::st_intersects(segs_sfc, barrier_3857)) > 0
      
      # Populate output list (symmetric)
      for (k in which(hits)) {
        i <- edge_i[k]; j <- edge_j[k]
        out[[i]] <- c(out[[i]], j)
        out[[j]] <- c(out[[j]], i)
      }
      for (i in seq_len(n)) {
        if (is.null(out[[i]])) out[[i]] <- integer(0)
        else out[[i]] <- sort(unique(out[[i]]))
      }
      out
    }
    
    get_unique_seed_cells <- function(grid_sf, site_sf) {
      # Ensure both are in the same CRS before computing distances
      grid_cents <- suppressWarnings(sf::st_centroid(grid_sf))
      if (!identical(sf::st_crs(site_sf), sf::st_crs(grid_cents)))
        site_sf <- sf::st_transform(site_sf, sf::st_crs(grid_cents))
      dist_mat <- as.matrix(sf::st_distance(site_sf, grid_cents))
      chosen <- integer(nrow(site_sf)); used <- integer(0)
      for (i in seq_len(nrow(site_sf))) {
        ord  <- order(dist_mat[i, ])
        pick <- ord[which(!(ord %in% used))[1]]
        if (length(pick) == 0 || is.na(pick)) pick <- ord[1]
        chosen[i] <- pick; used <- c(used, pick)
      }
      chosen
    }
    
    make_random_sites <- function(district_sf, n_dfa = 5, seed = 1) {
      set.seed(seed)
      pts <- sf::st_sample(district_sf, size = n_dfa, exact = TRUE)
      sf::st_sf(dfa_name = paste("Health Area", seq_len(n_dfa)), geometry = pts, crs = sf::st_crs(district_sf))
    }
    
    normalize_sites <- function(site_sf, grid_sf, name_col = NULL) {
      stopifnot(!is.null(site_sf), nrow(site_sf) > 0)
      site_sf <- sf::st_as_sf(site_sf) |> safe_make_valid() |> sf::st_transform(sf::st_crs(grid_sf))
      if (is.null(name_col)) {
        if ("dfa_name" %in% names(site_sf))           name_col <- "dfa_name"
        else if ("facility_name" %in% names(site_sf)) name_col <- "facility_name"
      }
      site_sf$dfa_name <- if (is.null(name_col)) paste("Health Area", seq_len(nrow(site_sf))) else as.character(site_sf[[name_col]])
      site_sf
    }
    
    snap_seeds_into_district <- function(site_sf, district_sf, grid_sf, cell_friction_raw = NULL,
                                         inward_buffer_m = 500, max_snap_friction = 0.98) {
      stopifnot(nrow(grid_sf) > 0)
      # Dissolve to single row — planning area sf (rural remainder) may have
      # multiple rows if st_difference produced a multipart result
      if (nrow(district_sf) > 1)
        district_sf <- district_sf |>
          dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
          sf::st_as_sf()
      site_3857     <- sf::st_transform(site_sf, 3857)
      district_3857 <- sf::st_transform(safe_make_valid(district_sf), 3857)
      grid_cent     <- suppressWarnings(sf::st_centroid(sf::st_transform(grid_sf, 3857)))
      district_inner <- suppressWarnings(sf::st_buffer(district_3857, -inward_buffer_m))
      if (nrow(district_inner) == 0 || any(sf::st_is_empty(district_inner)))
        district_inner <- suppressWarnings(sf::st_buffer(district_3857, -250))
      if (nrow(district_inner) == 0 || any(sf::st_is_empty(district_inner)))
        district_inner <- district_3857
      seed_inside   <- lengths(sf::st_within(site_3857, district_3857)) > 0
      candidate_idx <- which(lengths(sf::st_within(grid_cent, district_inner)) > 0)
      if (length(candidate_idx) == 0)
        candidate_idx <- which(lengths(sf::st_within(grid_cent, district_3857)) > 0)
      if (length(candidate_idx) == 0) stop("No candidate grid centroids for seed snapping.")
      if (!is.null(cell_friction_raw) && length(cell_friction_raw) == nrow(grid_sf)) {
        keep <- cell_friction_raw[candidate_idx] <= max_snap_friction
        if (any(keep)) candidate_idx <- candidate_idx[keep]
      }
      candidate_cent <- grid_cent[candidate_idx, ]
      for (i in which(!seed_inside)) {
        d <- as.numeric(sf::st_distance(site_3857[i, ], candidate_cent))
        sf::st_geometry(site_3857)[[i]] <- sf::st_geometry(grid_cent)[[candidate_idx[which.min(d)]]]
      }
      sf::st_transform(site_3857, sf::st_crs(district_sf))
    }
    
    expand_seed_starter_cells <- function(seed_cells, neighbors_list, starter_rings = 1) {
      out <- vector("list", length(seed_cells))
      for (i in seq_along(seed_cells)) {
        visited <- frontier <- unique(seed_cells[i])
        for (r in seq_len(max(1L, starter_rings))) {
          nxt <- setdiff(unique(unlist(neighbors_list[as.character(frontier)], use.names = FALSE)), visited)
          if (length(nxt) == 0) break
          visited <- unique(c(visited, nxt)); frontier <- nxt
        }
        out[[i]] <- sort(unique(visited))
      }
      out
    }
    
    # =========================================================================
    # propagate_assignments
    #
    # Wrapper around the compiled C++ Dijkstra implementation (bfs_propagate_cpp).
    # The C++ function handles the hot loop; this R wrapper handles:
    #   - seed cell expansion (expand_seed_starter_cells)
    #   - converting list-of-names neighbors to indexed form
    #   - mapping integer area indices back to area name strings
    #   - post-BFS flood fill for any unassigned cells
    #
    # All penalties operate in raw friction (0-1) space BEFORE friction_to_cost.
    # =========================================================================
    propagate_assignments <- function(
    grid_sf,
    site_sf,
    neighbors_list,
    cell_friction_raw,
    barrier_crossing_list        = NULL,
    barrier_penalty              = 0,
    subdivision_crossing_list    = NULL,
    subdivision_boundary_penalty = 0,
    compactness_penalty          = 0,
    max_cost                     = Inf,
    starter_rings                = 1,
    cell_pop                     = NULL,
    target_pop                   = NULL,
    base_step_friction           = 0.05,
    pop_saturation_pct           = 0.75,
    pop_saturation_weight        = 0.4,
    pop_saturation_max           = 0.4
    ) {
      stopifnot(nrow(grid_sf) > 0, nrow(site_sf) > 0)
      stopifnot(length(cell_friction_raw) == nrow(grid_sf))
      
      n          <- nrow(grid_sf)
      area_names <- as.character(site_sf$dfa_name)
      n_areas    <- length(area_names)
      seed_cells <- get_unique_seed_cells(grid_sf, site_sf)
      
      use_pop <- !is.null(cell_pop) && !is.null(target_pop) &&
        pop_saturation_weight > 0 && target_pop > 0 &&
        length(cell_pop) == n
      
      # neighbors_list and crossing lists are already positional integer lists
      # keyed by cell_id string — unname them for C++ (which needs plain lists)
      nbrs_indexed    <- unname(neighbors_list)
      barrier_indexed <- if (!is.null(barrier_crossing_list))
        unname(barrier_crossing_list) else vector("list", 0)
      subdiv_indexed  <- if (!is.null(subdivision_crossing_list))
        unname(subdivision_crossing_list) else vector("list", 0)
      
      # ── Expand seed starter cells (same logic as before) ───────────────────
      starter_cells_list <- expand_seed_starter_cells(seed_cells, neighbors_list, starter_rings)
      
      # Filter starters that cross hard barriers away from their seed
      starter_cells_indexed <- lapply(seq_len(n_areas), function(i) {
        starters <- starter_cells_list[[i]]
        if (!is.null(barrier_crossing_list) && length(starters) > 1) {
          keep <- starters[1]
          seed_nbrs <- neighbors_list[[as.character(grid_sf$cell_id[seed_cells[i]])]]
          for (cand in starters[-1]) {
            if (!(cand %in% seed_nbrs &&
                  cand %in% barrier_crossing_list[[seed_cells[i]]])) {
              keep <- c(keep, cand)
            }
          }
          starters <- unique(keep)
        }
        as.integer(starters)
      })
      
      # ── Call C++ BFS ───────────────────────────────────────────────────────
      cpp_result <- bfs_propagate_cpp(
        n_cells           = n,
        neighbors         = nbrs_indexed,
        cell_friction_raw = as.numeric(cell_friction_raw),
        seed_cells        = as.integer(seed_cells),
        starter_cells_list = starter_cells_indexed,
        barrier_mat       = barrier_indexed,
        subdiv_mat        = subdiv_indexed,
        subdiv_penalty    = as.double(subdivision_boundary_penalty),
        base_step_fric    = as.double(base_step_friction),
        compactness_pen   = as.double(compactness_penalty),
        max_cost_val      = as.double(max_cost),
        use_pop           = isTRUE(use_pop),
        cell_pop          = if (use_pop) as.numeric(cell_pop) else numeric(n),
        target_pop        = as.double(if (use_pop) target_pop else 0),
        pop_sat_pct       = as.double(pop_saturation_pct),
        pop_sat_weight    = as.double(pop_saturation_weight),
        pop_sat_max       = as.double(pop_saturation_max),
        n_areas           = as.integer(n_areas)
      )
      
      # Map 0-based area indices back to area name strings
      owner_idx  <- cpp_result$owner      # 0-based, -1 = unassigned
      best_cost  <- cpp_result$best_cost
      
      owner <- ifelse(owner_idx >= 0L, area_names[owner_idx + 1L], NA_character_)
      
      # ── Post-BFS: flood fill any unassigned cells ──────────────────────────
      unassigned <- which(is.na(owner))
      if (length(unassigned) > 0) {
        max_passes <- n
        pass       <- 0L
        while (length(unassigned) > 0 && pass < max_passes) {
          pass     <- pass + 1L
          progress <- FALSE
          for (cell_i in unassigned) {
            nbrs          <- nbrs_indexed[[cell_i]]
            assigned_nbrs <- nbrs[!is.na(owner[nbrs])]
            if (length(assigned_nbrs) > 0) {
              best_nbr      <- assigned_nbrs[which.min(best_cost[assigned_nbrs])]
              owner[cell_i] <- owner[best_nbr]
              progress      <- TRUE
            }
          }
          unassigned <- which(is.na(owner))
          if (!progress) break
        }
        if (length(unassigned) > 0) {
          fallback          <- names(which.max(table(owner[!is.na(owner)])))
          owner[unassigned] <- fallback
          cat("[propagate_assignments] fallback assigned", length(unassigned),
              "disconnected cell(s) to:", fallback, "\n")
        }
      }
      
      list(assignments = owner, cumulative_cost = best_cost,
           seeds_sf = site_sf, seed_cell_id = seed_cells)
    }
    
    make_start_assignment <- function(
    grid_sf, district_sf, neighbors_list, cell_friction_raw,
    barrier_crossing_list = NULL, n_dfa = 5, seed = 1,
    subdivision_crossing_list = NULL, subdivision_boundary_penalty = 0,
    barrier_penalty = 0, compactness_penalty = 0, max_cost = Inf, starter_rings = 1,
    cell_pop = NULL, target_pop = NULL, base_step_friction = 0.05,
    pop_saturation_pct = 0.75, pop_saturation_weight = 0.4, pop_saturation_max = 0.4
    ) {
      propagate_assignments(
        grid_sf = grid_sf,
        site_sf = make_random_sites(district_sf = district_sf, n_dfa = n_dfa, seed = seed),
        neighbors_list = neighbors_list, cell_friction_raw = cell_friction_raw,
        barrier_crossing_list = barrier_crossing_list, barrier_penalty = barrier_penalty,
        subdivision_crossing_list    = subdivision_crossing_list,
        subdivision_boundary_penalty = subdivision_boundary_penalty,
        compactness_penalty = compactness_penalty, max_cost = max_cost, starter_rings = starter_rings,
        cell_pop = cell_pop, target_pop = target_pop, base_step_friction = base_step_friction,
        pop_saturation_pct = pop_saturation_pct, pop_saturation_weight = pop_saturation_weight,
        pop_saturation_max = pop_saturation_max
      )
    }
    
    make_start_assignment_from_sites <- function(
    grid_sf, district_sf, site_sf, neighbors_list, cell_friction_raw,
    barrier_crossing_list = NULL, name_col = NULL,
    subdivision_crossing_list = NULL, subdivision_boundary_penalty = 0,
    barrier_penalty = 0, compactness_penalty = 0, max_cost = Inf, starter_rings = 1,
    cell_pop = NULL, target_pop = NULL, base_step_friction = 0.05,
    pop_saturation_pct = 0.75, pop_saturation_weight = 0.4, pop_saturation_max = 0.4
    ) {
      site_sf <- normalize_sites(site_sf = site_sf, grid_sf = grid_sf, name_col = name_col)
      site_sf <- snap_seeds_into_district(
        site_sf = site_sf, district_sf = district_sf, grid_sf = grid_sf,
        cell_friction_raw = cell_friction_raw, inward_buffer_m = 500, max_snap_friction = 0.98
      )
      propagate_assignments(
        grid_sf = grid_sf, site_sf = site_sf, neighbors_list = neighbors_list,
        cell_friction_raw = cell_friction_raw,
        barrier_crossing_list = barrier_crossing_list, barrier_penalty = barrier_penalty,
        subdivision_crossing_list    = subdivision_crossing_list,
        subdivision_boundary_penalty = subdivision_boundary_penalty,
        compactness_penalty = compactness_penalty, max_cost = max_cost, starter_rings = starter_rings,
        cell_pop = cell_pop, target_pop = target_pop, base_step_friction = base_step_friction,
        pop_saturation_pct = pop_saturation_pct, pop_saturation_weight = pop_saturation_weight,
        pop_saturation_max = pop_saturation_max
      )
    }
    
    scene <- reactive({
      req(district_sf()); req(nrow(district_sf()) > 0); req(grid_n())
      
      .t <- function(label, expr) {
        t0 <- proc.time()[["elapsed"]]
        result <- force(expr)
        cat(sprintf("[timing] %-30s %.2f s\n", label, proc.time()[["elapsed"]] - t0))
        result
      }
      
      seed_val <- if (shiny::is.reactive(seed)) seed() else as.integer(seed)
      
      # Ensure district_sf is in 4326 for JS canvas and downstream ops.
      # make_paint_grid internally reprojects to 3857 for grid building.
      district_sf_value <- safe_make_valid(district_sf())
      if (!identical(sf::st_crs(district_sf_value), sf::st_crs(4326)))
        district_sf_value <- sf::st_transform(district_sf_value, 4326)
      
      grid_info <- .t("grid build", {
        make_paint_grid(district_sf = district_sf_value, grid_n = grid_n())
      })
      grid_sf_value <- grid_info$grid_sf
      cat(sprintf("[timing] grid cells: %d (grid_n=%d, cellsize~%.0f m)\n",
                  nrow(grid_sf_value), grid_n(),
                  grid_info$max_dim_m / grid_n()))
      
      neighbors_list_value <- .t("neighbors list", {
        build_neighbors_list(
          grid_sf        = grid_sf_value,
          allow_diagonal = allow_diagonal,
          ncols_full     = grid_info$ncols_full,
          inside_mask    = grid_info$inside_mask
        )
      })
      
      # Use full district sf for friction path lookup (urban/rural areas
      # don't have zone/region/district columns; full district sf does).
      friction_sf_for_lookup <- if (!is.null(friction_district_sf)) {
        fdsf <- tryCatch(friction_district_sf(), error = function(e) NULL)
        if (!is.null(fdsf) && nrow(fdsf) > 0) fdsf else district_sf_value
      } else district_sf_value
      friction_path_value <- get_district_friction_path(
        district_sf = friction_sf_for_lookup,
        friction_dir = friction_dir,
        friction_lookup_csv = friction_lookup_csv
      )
      
      cell_friction_raw_value <- .t("friction extract", {
        extract_cell_friction_from_raster(
          grid_sf = grid_sf_value, district_sf = district_sf_value,
          friction_path = friction_path_value
        )
      })
      
      facility_sf_value <- facility_seed_sf()
      
      if (!is.null(facility_sf_value) && nrow(facility_sf_value) > 0) {
        cell_friction_raw_value <- .t("distance friction blend", {
          dist_friction_value <- compute_distance_friction(
            grid_sf = grid_sf_value, facility_sf = facility_sf_value,
            flat_km = 5, inflect_km = 10, steepness = 2.5
          )
          pmin(1, 0.6 * cell_friction_raw_value + 0.4 * dist_friction_value)
        })
      }
      
      cell_pop_value <- .t("population extract", {
        extract_cell_population(grid_sf_value, u5_rast)
      })
      total_pop        <- sum(cell_pop_value, na.rm = TRUE)
      n_areas          <- if (!is.null(facility_sf_value) && nrow(facility_sf_value) > 0) nrow(facility_sf_value) else n_dfa
      target_pop_value <- if (total_pop > 0 && n_areas > 0) total_pop / n_areas else NULL
      
      cat(sprintf("[health_area_gen] total_pop=%.0f | n_areas=%d | target_pop=%.0f\n",
                  total_pop, n_areas, target_pop_value %||% 0))
      
      barrier_sf_value <- barrier_lines_sf()
      if (!is.null(barrier_sf_value) && nrow(barrier_sf_value) > 0) {
        barrier_sf_value            <- safe_make_valid(barrier_sf_value)
        barrier_crossing_list_value <- .t("barrier crossing matrix", {
          build_barrier_crossing_matrix(
            grid_sf = grid_sf_value, neighbors_list = neighbors_list_value,
            barrier_lines_sf = barrier_sf_value
          )
        })
      } else {
        barrier_crossing_list_value <- NULL
        cat("[timing] barrier crossing matrix        skipped (no barriers)\n")
      }
      
      subdiv_lines_value <- subdivision_lines_sf()
      if (!is.null(subdiv_lines_value) && nrow(subdiv_lines_value) > 0) {
        subdiv_lines_value              <- safe_make_valid(subdiv_lines_value)
        subdivision_crossing_list_value <- .t("subdivision crossing matrix", {
          build_barrier_crossing_matrix(
            grid_sf = grid_sf_value, neighbors_list = neighbors_list_value,
            barrier_lines_sf = subdiv_lines_value
          )
        })
        cat(sprintf("[health_area_gen] subdivision boundary penalty=%.2f | crossing pairs=%d\n",
                    subdivision_boundary_penalty,
                    sum(vapply(subdivision_crossing_list_value, length, integer(1)))))
      } else {
        subdivision_crossing_list_value <- NULL
        cat("[timing] subdivision crossing matrix    skipped (no subdivisions)\n")
      }
      
      shared_pop_args <- list(
        cell_pop = cell_pop_value, target_pop = target_pop_value,
        base_step_friction = base_step_friction,
        pop_saturation_pct = pop_saturation_pct,
        pop_saturation_weight = pop_saturation_weight,
        pop_saturation_max = pop_saturation_max
      )
      
      if (!is.null(facility_sf_value) && nrow(facility_sf_value) > 0) {
        start_info <- .t("BFS propagation (from sites)", {
          do.call(make_start_assignment_from_sites, c(list(
            grid_sf = grid_sf_value, district_sf = district_sf_value,
            site_sf = facility_sf_value,
            neighbors_list = neighbors_list_value,
            cell_friction_raw = cell_friction_raw_value,
            barrier_crossing_list = barrier_crossing_list_value,
            subdivision_crossing_list    = subdivision_crossing_list_value,
            subdivision_boundary_penalty = subdivision_boundary_penalty,
            name_col = facility_name_col, barrier_penalty = barrier_penalty,
            compactness_penalty = compactness_penalty, max_cost = max_cost,
            starter_rings = 2
          ), shared_pop_args))
        })
      } else {
        start_info <- .t("BFS propagation (random seeds)", {
          do.call(make_start_assignment, c(list(
            grid_sf = grid_sf_value, district_sf = district_sf_value,
            neighbors_list = neighbors_list_value,
            cell_friction_raw = cell_friction_raw_value,
            barrier_crossing_list = barrier_crossing_list_value,
            subdivision_crossing_list    = subdivision_crossing_list_value,
            subdivision_boundary_penalty = subdivision_boundary_penalty,
            n_dfa = n_dfa, seed = seed_val, barrier_penalty = barrier_penalty,
            compactness_penalty = compactness_penalty, max_cost = max_cost,
            starter_rings = 2
          ), shared_pop_args))
        })
      }
      
      seed_outputs <- build_seed_points_outputs(start_info$seeds_sf)
      grid_sf_value$u5_pop <- cell_pop_value
      
      edge_list_value <- .t("edge list", {
        build_edge_list(
          grid_sf_value, district_sf_value,
          ncols_full  = grid_info$ncols_full,
          inside_mask = grid_info$inside_mask
        )
      })
      
      list(
        district_sf                  = district_sf_value,
        grid_sf                      = grid_sf_value,
        initial_assignments          = as.character(start_info$assignments),
        seed_points_sf               = seed_outputs$seed_points_sf,
        seed_points_df               = seed_outputs$seed_points_df,
        seed_points_list             = seed_outputs$seed_points_list,
        neighbors_list               = neighbors_list_value,
        edge_list                    = edge_list_value,
        max_dim_m                    = grid_info$max_dim_m,
        cell_friction                = friction_to_cost(cell_friction_raw_value),
        cell_friction_raw            = cell_friction_raw_value,
        cumulative_cost              = as.numeric(start_info$cumulative_cost),
        seed_cell_id                 = as.integer(start_info$seed_cell_id),
        barrier_crossing_list        = barrier_crossing_list_value,
        subdivision_crossing_list    = subdivision_crossing_list_value,
        friction_path                = friction_path_value
      )
    })
    
    list(
      scene                 = scene,
      district_sf           = reactive(scene()$district_sf),
      grid_sf               = reactive(scene()$grid_sf),
      initial_assignments   = reactive(scene()$initial_assignments),
      seed_points_sf        = reactive(scene()$seed_points_sf),
      seed_points_df        = reactive(scene()$seed_points_df),
      seed_points_list      = reactive(scene()$seed_points_list),
      neighbors_list        = reactive(scene()$neighbors_list),
      edge_list             = reactive(scene()$edge_list),
      max_dim_m             = reactive(scene()$max_dim_m),
      cell_friction         = reactive(scene()$cell_friction),
      cell_friction_raw     = reactive(scene()$cell_friction_raw),
      cumulative_cost       = reactive(scene()$cumulative_cost),
      seed_cell_id          = reactive(scene()$seed_cell_id),
      barrier_crossing_list        = reactive(scene()$barrier_crossing_list),
      subdivision_crossing_list    = reactive(scene()$subdivision_crossing_list),
      friction_path                = reactive(scene()$friction_path)
    )
  })
}