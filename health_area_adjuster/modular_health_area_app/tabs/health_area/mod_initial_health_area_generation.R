initialHealthAreaGenerationServer <- function(
    id,
    district_sf,
    grid_n,
    n_dfa = 5,
    seed = 1,
    facility_seed_sf = reactive(NULL),
<<<<<<< HEAD
    facility_name_col = NULL,
    barrier_lines_sf = reactive(NULL),
    barrier_penalty = 1e6,
    compactness_penalty = 0,
    allow_diagonal = TRUE,
    max_cost = Inf,
    friction_dir = file.path(getwd(), "data", "friction", "district_standardized"),
    friction_lookup_csv = file.path(
      getwd(), "data", "friction", "district_standardized", "district_friction_index.csv"
    )
) {
  moduleServer(id, function(input, output, session) {
    
    safe_make_valid <- function(x) {
      if (is.null(x)) return(x)
      suppressWarnings(sf::st_make_valid(x))
    }
    
    std_name <- function(x) {
      x |>
        tolower() |>
        gsub("[^a-z0-9]+", "_", x = _) |>
        gsub("^_+|_+$", "", x = _)
    }
    
    make_paint_grid <- function(district_sf, grid_n = 150) {
      district_sf <- safe_make_valid(district_sf)
      district_3857 <- sf::st_transform(district_sf, 3857)
      
=======
    facility_name_col = NULL
) {
  moduleServer(id, function(input, output, session) {

    safe_make_valid <- function(x) {
      sf::st_make_valid(x)
    }

    make_paint_grid <- function(district_sf, grid_n = 150) {
      district_sf <- safe_make_valid(district_sf)
      district_3857 <- sf::st_transform(district_sf, 3857)

>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
      bbox <- sf::st_bbox(district_3857)
      width_m <- bbox$xmax - bbox$xmin
      height_m <- bbox$ymax - bbox$ymin
      max_dim <- max(width_m, height_m)
<<<<<<< HEAD
      
      cellsize <- max_dim / grid_n
      
      raw_grid <- sf::st_make_grid(
        district_3857,
        cellsize = cellsize,
        what = "polygons",
        square = TRUE
      )
      
=======

      cellsize <- max_dim / grid_n

      raw_grid <- sf::st_make_grid(
        district_3857,
        cellsize = cellsize,
        what = 'polygons',
        square = TRUE
      )

>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
      grid_sf <- sf::st_sf(
        cell_id = seq_along(raw_grid),
        geometry = raw_grid,
        crs = sf::st_crs(district_3857)
      )
<<<<<<< HEAD
      
      cent_3857 <- suppressWarnings(sf::st_centroid(grid_sf))
      inside <- lengths(sf::st_within(cent_3857, district_3857)) > 0
      
      grid_sf <- grid_sf |>
        dplyr::filter(inside) |>
        dplyr::mutate(cell_id = seq_len(dplyr::n()))
      
      cent_3857 <- cent_3857[inside, ]
      cent_wgs84 <- sf::st_transform(cent_3857, 4326)
      coords <- sf::st_coordinates(cent_wgs84)
      
      grid_sf <- sf::st_transform(grid_sf, 4326)
      grid_sf$centroid_lon <- coords[, 1]
      grid_sf$centroid_lat <- coords[, 2]
      
=======

      cent_3857 <- suppressWarnings(sf::st_centroid(grid_sf))
      inside <- lengths(sf::st_within(cent_3857, district_3857)) > 0

      grid_sf <- grid_sf |>
        dplyr::filter(inside) |>
        dplyr::mutate(cell_id = seq_len(dplyr::n()))

      cent_wgs84 <- sf::st_transform(cent_3857[inside, ], 4326)
      coords <- sf::st_coordinates(cent_wgs84)

      grid_sf <- sf::st_transform(grid_sf, 4326)
      grid_sf$centroid_lon <- coords[, 1]
      grid_sf$centroid_lat <- coords[, 2]

>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
      list(
        grid_sf = grid_sf,
        max_dim_m = as.numeric(max_dim)
      )
    }
<<<<<<< HEAD
    
    build_seed_points_outputs <- function(seed_points_sf) {
      seed_pts <- sf::st_transform(seed_points_sf, 4326)
      seed_coords <- sf::st_coordinates(seed_pts)
      
=======

    make_start_assignment <- function(grid_sf, district_sf, n_dfa = 5, seed = 1) {
      set.seed(seed)

      pts <- sf::st_sample(district_sf, size = n_dfa, exact = TRUE)

      pts_sf <- sf::st_sf(
        dfa_name = paste('Health Area', seq_len(n_dfa)),
        geometry = pts,
        crs = sf::st_crs(district_sf)
      )

      cent <- suppressWarnings(sf::st_centroid(grid_sf))
      idx <- sf::st_nearest_feature(cent, pts_sf)

      list(
        assignments = as.character(pts_sf$dfa_name[idx]),
        seeds_sf = pts_sf
      )
    }

    make_start_assignment_from_sites <- function(grid_sf, site_sf, name_col = NULL) {
      stopifnot(!is.null(grid_sf), nrow(grid_sf) > 0)
      stopifnot(!is.null(site_sf), nrow(site_sf) > 0)

      site_sf <- sf::st_as_sf(site_sf)
      site_sf <- sf::st_transform(site_sf, sf::st_crs(grid_sf))

      if (is.null(name_col)) {
        if ('dfa_name' %in% names(site_sf)) {
          name_col <- 'dfa_name'
        } else if ('facility_name' %in% names(site_sf)) {
          name_col <- 'facility_name'
        }
      }

      if (is.null(name_col)) {
        site_sf$dfa_name <- paste('Health Area', seq_len(nrow(site_sf)))
      } else {
        site_sf$dfa_name <- as.character(site_sf[[name_col]])
      }

      cent <- suppressWarnings(sf::st_centroid(grid_sf))
      idx <- sf::st_nearest_feature(cent, site_sf)

      list(
        assignments = as.character(site_sf$dfa_name[idx]),
        seeds_sf = site_sf
      )
    }

    build_seed_points_outputs <- function(seed_points_sf) {
      seed_pts <- sf::st_transform(seed_points_sf, 4326)
      seed_coords <- sf::st_coordinates(seed_pts)

>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
      seed_points_df <- data.frame(
        dfa_name = as.character(seed_pts$dfa_name),
        lon = seed_coords[, 1],
        lat = seed_coords[, 2],
        stringsAsFactors = FALSE
      )
<<<<<<< HEAD
      
=======

>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
      seed_points_list <- lapply(seq_len(nrow(seed_points_df)), function(i) {
        list(
          dfa_name = seed_points_df$dfa_name[i],
          lon = unname(seed_points_df$lon[i]),
          lat = unname(seed_points_df$lat[i])
        )
      })
<<<<<<< HEAD
      
=======

>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
      list(
        seed_points_sf = seed_pts,
        seed_points_df = seed_points_df,
        seed_points_list = seed_points_list
      )
    }
<<<<<<< HEAD
    
    build_neighbors_list <- function(grid_sf, allow_diagonal = TRUE) {
      grid_3857 <- sf::st_transform(grid_sf, 3857)
      
      if (isTRUE(allow_diagonal)) {
        touch_list <- sf::st_touches(grid_3857)
      } else {
        touch_list <- sf::st_relate(
          grid_3857,
          grid_3857,
          pattern = "F***1****"
        )
      }
      
=======

    build_neighbors_list <- function(grid_sf) {
      touch_list <- sf::st_touches(grid_sf)
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
      neighbors_list <- lapply(touch_list, as.integer)
      names(neighbors_list) <- as.character(grid_sf$cell_id)
      neighbors_list
    }
<<<<<<< HEAD
    
    build_edge_list <- function(grid_sf, district_sf) {
      grid_sf_3857 <- sf::st_transform(grid_sf, 3857)
      district_3857 <- sf::st_transform(district_sf, 3857)
      
      cell_bbox <- sf::st_bbox(grid_sf_3857[1, ])
      cell_w <- as.numeric(cell_bbox["xmax"] - cell_bbox["xmin"])
      cell_h <- as.numeric(cell_bbox["ymax"] - cell_bbox["ymin"])
      edge_buffer <- max(cell_w, cell_h) * 0.05
      
      district_boundary_3857 <- sf::st_boundary(district_3857) |>
        sf::st_buffer(edge_buffer)
      
=======

    build_edge_list <- function(grid_sf, district_sf) {
      grid_sf_3857 <- sf::st_transform(grid_sf, 3857)
      district_3857 <- sf::st_transform(district_sf, 3857)

      cell_bbox <- sf::st_bbox(grid_sf_3857[1, ])
      cell_w <- as.numeric(cell_bbox['xmax'] - cell_bbox['xmin'])
      cell_h <- as.numeric(cell_bbox['ymax'] - cell_bbox['ymin'])
      edge_buffer <- max(cell_w, cell_h) * 0.05

      district_boundary_3857 <- sf::st_boundary(district_3857) |>
        sf::st_buffer(edge_buffer)

>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
      edge_flag <- lengths(sf::st_intersects(grid_sf_3857, district_boundary_3857)) > 0
      edge_list <- as.list(edge_flag)
      names(edge_list) <- as.character(grid_sf$cell_id)
      edge_list
    }
<<<<<<< HEAD
    
    infer_admin_fields <- function(district_sf) {
      nm <- names(district_sf)
      nm_low <- tolower(nm)
      
      find_first <- function(candidates) {
        hit <- nm[match(candidates, nm_low, nomatch = 0)]
        hit <- hit[hit != ""]
        if (length(hit) == 0) return(NA_character_)
        hit[1]
      }
      
      zone_field <- find_first(c(
        "zone", "state", "admin1", "state_name", "zone_name"
      ))
      region_field <- find_first(c(
        "region", "admin2", "region_name"
      ))
      district_field <- find_first(c(
        "district", "admin3", "district_name"
      ))
      
      list(
        zone_field = zone_field,
        region_field = region_field,
        district_field = district_field
      )
    }
    
    get_district_friction_path <- function(district_sf, friction_dir, friction_lookup_csv = NULL) {
      stopifnot(nrow(district_sf) == 1)
      
      fields <- infer_admin_fields(district_sf)
      
      if (any(is.na(unlist(fields)))) {
        stop("Could not infer zone/region/district field names from district_sf.")
      }
      
      zone_val <- std_name(as.character(district_sf[[fields$zone_field]][1]))
      region_val <- std_name(as.character(district_sf[[fields$region_field]][1]))
      district_val <- std_name(as.character(district_sf[[fields$district_field]][1]))
      
      direct_fname <- paste0(zone_val, "__", region_val, "__", district_val, "__friction_100m.tif")
      direct_path <- file.path(friction_dir, direct_fname)
      
      if (file.exists(direct_path)) {
        return(direct_path)
      }
      
      if (!is.null(friction_lookup_csv) && file.exists(friction_lookup_csv)) {
        idx <- tryCatch(
          utils::read.csv(friction_lookup_csv, stringsAsFactors = FALSE),
          error = function(e) NULL
        )
        
        if (!is.null(idx) && nrow(idx) > 0) {
          idx_names <- tolower(names(idx))
          
          zone_col <- names(idx)[match(c("zone", "state", "admin1"), idx_names, nomatch = 0)][1]
          region_col <- names(idx)[match(c("region", "admin2"), idx_names, nomatch = 0)][1]
          district_col <- names(idx)[match(c("district", "admin3"), idx_names, nomatch = 0)][1]
          file_col <- names(idx)[match(c("file_name", "filename", "file", "path"), idx_names, nomatch = 0)][1]
          
          if (!any(is.na(c(zone_col, region_col, district_col, file_col)))) {
            idx$zone_std <- std_name(idx[[zone_col]])
            idx$region_std <- std_name(idx[[region_col]])
            idx$district_std <- std_name(idx[[district_col]])
            
            hit <- idx |>
              dplyr::filter(
                .data$zone_std == zone_val,
                .data$region_std == region_val,
                .data$district_std == district_val
              )
            
            if (nrow(hit) > 0) {
              candidate <- hit[[file_col]][1]
              
              if (file.exists(candidate)) {
                return(candidate)
              }
              
              candidate2 <- file.path(friction_dir, basename(candidate))
              if (file.exists(candidate2)) {
                return(candidate2)
              }
            }
          }
        }
      }
      
      tif_files <- list.files(friction_dir, pattern = "\\.tif$", full.names = TRUE)
      
      target_stub <- paste(zone_val, region_val, district_val, sep = "__")
      hit <- tif_files[grepl(target_stub, basename(tif_files), fixed = TRUE)]
      
      if (length(hit) == 1) {
        return(hit[1])
      }
      
      if (length(hit) > 1) {
        return(hit[1])
      }
      
      stop(
        paste0(
          "No district friction raster found for: ",
          zone_val, " / ", region_val, " / ", district_val
        )
      )
    }
    
    extract_cell_friction_from_raster <- function(grid_sf, district_sf, friction_path) {
      if (!file.exists(friction_path)) {
        stop("Friction raster file does not exist: ", friction_path)
      }
      
      r <- terra::rast(friction_path)
      
      grid_vect <- terra::vect(sf::st_transform(grid_sf, terra::crs(r)))
      cent_vect <- terra::centroids(grid_vect)
      
      vals <- terra::extract(r, cent_vect)[, 2]
      vals <- as.numeric(vals)
      
      if (all(is.na(vals))) {
        stop("All extracted friction values are NA for raster: ", friction_path)
      }
      
      if (anyNA(vals)) {
        med <- stats::median(vals, na.rm = TRUE)
        vals[is.na(vals)] <- med
      }
      
      vals[!is.finite(vals)] <- stats::median(vals[is.finite(vals)], na.rm = TRUE)
      vals <- pmin(pmax(vals, 0), 1)
      
      vals
    }
    
    friction_to_cost <- function(
    x,
    min_cost = 1,
    soft_block_cost = 1000,
    knee = 0.90,
    power_low = 2,
    power_high = 6
    ) {
      x <- pmin(pmax(x, 0), 1)
      
      out <- ifelse(
        x <= knee,
        min_cost + ((x / knee) ^ power_low) * 99,
        100 + (((x - knee) / (1 - knee)) ^ power_high) * (soft_block_cost - 100)
      )
      
      out
    }
    
    build_barrier_crossing_matrix <- function(grid_sf, neighbors_list, barrier_lines_sf) {
      n <- nrow(grid_sf)
      out <- vector("list", n)
      names(out) <- as.character(grid_sf$cell_id)
      
      if (is.null(barrier_lines_sf) || nrow(barrier_lines_sf) == 0) {
        for (i in seq_len(n)) {
          out[[i]] <- integer(0)
        }
        return(out)
      }
      
      grid_3857 <- sf::st_transform(grid_sf, 3857)
      cent_3857 <- suppressWarnings(sf::st_centroid(grid_3857))
      barrier_3857 <- safe_make_valid(sf::st_transform(barrier_lines_sf, 3857))
      
      for (i in seq_len(n)) {
        nbrs <- neighbors_list[[as.character(grid_sf$cell_id[i])]]
        
        if (length(nbrs) == 0) {
          out[[i]] <- integer(0)
          next
        }
        
        crossing_nbrs <- integer(0)
        p1 <- sf::st_coordinates(cent_3857[i, ])
        
        for (j in nbrs) {
          if (j <= i) next
          
          p2 <- sf::st_coordinates(cent_3857[j, ])
          
          seg <- sf::st_sfc(
            sf::st_linestring(rbind(p1, p2)),
            crs = sf::st_crs(cent_3857)
          )
          
          crosses_barrier <- lengths(sf::st_intersects(seg, barrier_3857)) > 0
          
          if (isTRUE(crosses_barrier)) {
            crossing_nbrs <- c(crossing_nbrs, j)
          }
        }
        
        out[[i]] <- crossing_nbrs
      }
      
      for (i in seq_len(n)) {
        nbrs <- out[[i]]
        if (length(nbrs) == 0) next
        for (j in nbrs) {
          out[[j]] <- unique(c(out[[j]], i))
        }
      }
      
      out
    }
    
    get_unique_seed_cells <- function(grid_sf, site_sf) {
      grid_cent <- suppressWarnings(sf::st_centroid(grid_sf))
      dist_mat <- sf::st_distance(site_sf, grid_cent)
      dist_mat <- as.matrix(dist_mat)
      
      chosen <- integer(nrow(site_sf))
      used <- integer(0)
      
      for (i in seq_len(nrow(site_sf))) {
        ord <- order(dist_mat[i, ])
        pick <- ord[which(!(ord %in% used))[1]]
        
        if (length(pick) == 0 || is.na(pick)) {
          pick <- ord[1]
        }
        
        chosen[i] <- pick
        used <- c(used, pick)
      }
      
      chosen
    }
    
    make_random_sites <- function(district_sf, n_dfa = 5, seed = 1) {
      set.seed(seed)
      
      pts <- sf::st_sample(district_sf, size = n_dfa, exact = TRUE)
      
      sf::st_sf(
        dfa_name = paste("Health Area", seq_len(n_dfa)),
        geometry = pts,
        crs = sf::st_crs(district_sf)
      )
    }
    
    normalize_sites <- function(site_sf, grid_sf, name_col = NULL) {
      stopifnot(!is.null(site_sf), nrow(site_sf) > 0)
      
      site_sf <- sf::st_as_sf(site_sf)
      site_sf <- safe_make_valid(site_sf)
      site_sf <- sf::st_transform(site_sf, sf::st_crs(grid_sf))
      
      if (is.null(name_col)) {
        if ("dfa_name" %in% names(site_sf)) {
          name_col <- "dfa_name"
        } else if ("facility_name" %in% names(site_sf)) {
          name_col <- "facility_name"
        }
      }
      
      if (is.null(name_col)) {
        site_sf$dfa_name <- paste("Health Area", seq_len(nrow(site_sf)))
      } else {
        site_sf$dfa_name <- as.character(site_sf[[name_col]])
      }
      
      site_sf
    }
    
    snap_seeds_into_district <- function(
    site_sf,
    district_sf,
    grid_sf,
    cell_friction_raw = NULL,
    inward_buffer_m = 500,
    max_snap_friction = 0.98
    ) {
      stopifnot(nrow(district_sf) == 1)
      stopifnot(nrow(grid_sf) > 0)
      
      site_3857 <- sf::st_transform(site_sf, 3857)
      district_3857 <- sf::st_transform(safe_make_valid(district_sf), 3857)
      grid_3857 <- sf::st_transform(grid_sf, 3857)
      grid_cent <- suppressWarnings(sf::st_centroid(grid_3857))
      
      district_inner <- suppressWarnings(sf::st_buffer(district_3857, -inward_buffer_m))
      
      if (nrow(district_inner) == 0 || any(sf::st_is_empty(district_inner))) {
        district_inner <- suppressWarnings(sf::st_buffer(district_3857, -250))
      }
      if (nrow(district_inner) == 0 || any(sf::st_is_empty(district_inner))) {
        district_inner <- suppressWarnings(sf::st_buffer(district_3857, -100))
      }
      if (nrow(district_inner) == 0 || any(sf::st_is_empty(district_inner))) {
        district_inner <- district_3857
      }
      
      seed_inside <- lengths(sf::st_within(site_3857, district_3857)) > 0
      cent_inside_inner <- lengths(sf::st_within(grid_cent, district_inner)) > 0
      
      candidate_idx <- which(cent_inside_inner)
      
      if (length(candidate_idx) == 0) {
        cent_inside_district <- lengths(sf::st_within(grid_cent, district_3857)) > 0
        candidate_idx <- which(cent_inside_district)
      }
      
      if (length(candidate_idx) == 0) {
        stop("No candidate grid centroids found for seed snapping.")
      }
      
      if (!is.null(cell_friction_raw) && length(cell_friction_raw) == nrow(grid_sf)) {
        keep <- cell_friction_raw[candidate_idx] <= max_snap_friction
        if (any(keep)) {
          candidate_idx <- candidate_idx[keep]
        }
      }
      
      candidate_cent <- grid_cent[candidate_idx, ]
      
      for (i in which(!seed_inside)) {
        d <- as.numeric(sf::st_distance(site_3857[i, ], candidate_cent))
        pick_local <- which.min(d)
        pick_global <- candidate_idx[pick_local]
        sf::st_geometry(site_3857)[[i]] <- sf::st_geometry(grid_cent)[[pick_global]]
      }
      
      sf::st_transform(site_3857, sf::st_crs(district_sf))
    }
    
    expand_seed_starter_cells <- function(seed_cells, neighbors_list, starter_rings = 1) {
      out <- vector("list", length(seed_cells))
      
      for (i in seq_along(seed_cells)) {
        visited <- unique(seed_cells[i])
        frontier <- unique(seed_cells[i])
        
        if (starter_rings > 0) {
          for (r in seq_len(starter_rings)) {
            nxt <- unique(unlist(neighbors_list[as.character(frontier)], use.names = FALSE))
            nxt <- setdiff(nxt, visited)
            
            if (length(nxt) == 0) break
            
            visited <- unique(c(visited, nxt))
            frontier <- nxt
          }
        }
        
        out[[i]] <- sort(unique(visited))
      }
      
      out
    }
    
    propagate_assignments <- function(
    grid_sf,
    site_sf,
    neighbors_list,
    cell_friction,
    cell_population,
    target_pop = NULL,
    barrier_crossing_list = NULL,
    barrier_penalty = 0,
    compactness_penalty = 0,
    max_cost = Inf,
    starter_rings = 1,
    pop_under_penalty = 30,
    pop_under_power = 2,
    pop_over_penalty = 150,
    pop_over_power = 2
    ) {
      stopifnot(nrow(grid_sf) > 0)
      stopifnot(nrow(site_sf) > 0)
      stopifnot(length(cell_friction) == nrow(grid_sf))
      stopifnot(length(cell_population) == nrow(grid_sf))
      
      n <- nrow(grid_sf)
      
      cell_population <- as.numeric(cell_population)
      cell_population[!is.finite(cell_population)] <- 0
      cell_population[cell_population < 0] <- 0
      
      seed_cells <- get_unique_seed_cells(grid_sf, site_sf)
      
      owner <- rep(NA_character_, n)
      best_cost <- rep(Inf, n)
      visited <- rep(FALSE, n)
      
      area_names <- unique(as.character(site_sf$dfa_name))
      owner_pop <- stats::setNames(
        rep(0, length(area_names)),
        area_names
      )
      
      if (is.null(target_pop) || !is.finite(target_pop) || target_pop <= 0) {
        target_pop <- sum(cell_population, na.rm = TRUE) / length(area_names)
      }
      
      queue_cell <- integer(0)
      queue_cost <- numeric(0)
      queue_owner <- character(0)
      
      barrier_hit <- function(i, j) {
        if (is.null(barrier_crossing_list)) return(FALSE)
        j %in% barrier_crossing_list[[i]]
      }
      
      population_adjustment <- function(current_pop, target_pop) {
        if (!is.finite(current_pop)) current_pop <- 0
        if (!is.finite(target_pop) || target_pop <= 0) return(0)
        
        pop_ratio <- current_pop / target_pop
        
        if (pop_ratio <= 1) {
          return(pop_under_penalty * (pop_ratio ^ pop_under_power))
        } else {
          return(
            pop_under_penalty +
              pop_over_penalty * ((pop_ratio - 1) ^ pop_over_power)
          )
        }
      }
      
      starter_cells_list <- expand_seed_starter_cells(
        seed_cells = seed_cells,
        neighbors_list = neighbors_list,
        starter_rings = starter_rings
      )
      
      for (i in seq_len(nrow(site_sf))) {
        owner_i <- as.character(site_sf$dfa_name[i])
        starter_cells <- starter_cells_list[[i]]
        
        if (!is.null(barrier_crossing_list) && length(starter_cells) > 1) {
          keep <- starter_cells[1]
          
          for (cand in starter_cells[-1]) {
            path_ok <- TRUE
            
            nbrs_seed <- neighbors_list[[as.character(seed_cells[i])]]
            if (!(cand %in% nbrs_seed) && cand != seed_cells[i]) {
              path_ok <- TRUE
            } else if (barrier_hit(seed_cells[i], cand)) {
              path_ok <- FALSE
            }
            
            if (path_ok) keep <- c(keep, cand)
          }
          
          starter_cells <- unique(keep)
        }
        
        for (cell_i in starter_cells) {
          prev_owner <- owner[cell_i]
          
          if (!is.na(prev_owner) && prev_owner != owner_i) {
            owner_pop[[prev_owner]] <- owner_pop[[prev_owner]] - cell_population[cell_i]
          }
          
          if (is.infinite(best_cost[cell_i]) || 0 < best_cost[cell_i] || owner[cell_i] != owner_i) {
            best_cost[cell_i] <- 0
            owner[cell_i] <- owner_i
            owner_pop[[owner_i]] <- owner_pop[[owner_i]] + cell_population[cell_i]
            
            queue_cell <- c(queue_cell, cell_i)
            queue_cost <- c(queue_cost, 0)
            queue_owner <- c(queue_owner, owner_i)
          }
        }
      }
      
      while (length(queue_cell) > 0) {
        k <- which.min(queue_cost)[1]
        
        cell_i <- queue_cell[k]
        cost_i <- queue_cost[k]
        owner_i <- queue_owner[k]
        
        queue_cell <- queue_cell[-k]
        queue_cost <- queue_cost[-k]
        queue_owner <- queue_owner[-k]
        
        if (visited[cell_i]) next
        if (owner[cell_i] != owner_i) next
        if (!isTRUE(all.equal(cost_i, best_cost[cell_i])) && cost_i > best_cost[cell_i]) next
        
        visited[cell_i] <- TRUE
        
        nbrs <- neighbors_list[[as.character(grid_sf$cell_id[cell_i])]]
        if (length(nbrs) == 0) next
        
        current_owner_pop <- owner_pop[[owner_i]]
        pop_adjustment <- population_adjustment(
          current_pop = current_owner_pop,
          target_pop = target_pop
        )
        
        for (nbr in nbrs) {
          if (visited[nbr]) next
          if (barrier_hit(cell_i, nbr)) next
          
          local_same_owner_n <- 0
          nbrs2 <- neighbors_list[[as.character(grid_sf$cell_id[nbr])]]
          if (length(nbrs2) > 0) {
            local_same_owner_n <- sum(owner[nbrs2] == owner_i, na.rm = TRUE)
          }
          
          shape_pen <- 0
          if (compactness_penalty > 0 && local_same_owner_n < 2) {
            shape_pen <- compactness_penalty
          }
          
          move_cost <- ((cell_friction[cell_i] + cell_friction[nbr]) / 2) +
            shape_pen +
            pop_adjustment
          
          if (is.finite(barrier_penalty) && barrier_penalty > 0 && barrier_hit(cell_i, nbr)) {
            move_cost <- move_cost + barrier_penalty
          }
          
          new_cost <- best_cost[cell_i] + move_cost
          
          if (new_cost < best_cost[nbr] && new_cost <= max_cost) {
            prev_owner <- owner[nbr]
            
            if (!is.na(prev_owner) && prev_owner != owner_i) {
              owner_pop[[prev_owner]] <- owner_pop[[prev_owner]] - cell_population[nbr]
            }
            
            if (is.na(prev_owner) || prev_owner != owner_i) {
              owner_pop[[owner_i]] <- owner_pop[[owner_i]] + cell_population[nbr]
            }
            
            best_cost[nbr] <- new_cost
            owner[nbr] <- owner_i
            
            queue_cell <- c(queue_cell, nbr)
            queue_cost <- c(queue_cost, new_cost)
            queue_owner <- c(queue_owner, owner_i)
          }
        }
      }
      
      area_population <- data.frame(
        dfa_name = names(owner_pop),
        assigned_pop = as.numeric(owner_pop),
        target_pop = rep(target_pop, length(owner_pop)),
        pop_ratio = as.numeric(owner_pop) / target_pop,
        stringsAsFactors = FALSE
      )
      
      list(
        assignments = owner,
        cumulative_cost = best_cost,
        seeds_sf = site_sf,
        seed_cell_id = seed_cells,
        area_population = area_population,
        target_pop = target_pop
      )
    }
    
    make_start_assignment <- function(
    grid_sf,
    district_sf,
    neighbors_list,
    cell_friction,
    cell_population,
    target_pop = NULL,
    barrier_crossing_list = NULL,
    n_dfa = 5,
    seed = 1,
    barrier_penalty = 0,
    compactness_penalty = 0,
    max_cost = Inf,
    starter_rings = 1
    ) {
      pts_sf <- make_random_sites(
        district_sf = district_sf,
        n_dfa = n_dfa,
        seed = seed
      )
      
      propagate_assignments(
        grid_sf = grid_sf,
        site_sf = pts_sf,
        neighbors_list = neighbors_list,
        cell_friction = cell_friction,
        cell_population = cell_population,
        target_pop = target_pop,
        barrier_crossing_list = barrier_crossing_list,
        barrier_penalty = barrier_penalty,
        compactness_penalty = compactness_penalty,
        max_cost = max_cost,
        starter_rings = starter_rings
      )
    }
    
    make_start_assignment_from_sites <- function(
    grid_sf,
    district_sf,
    site_sf,
    neighbors_list,
    cell_friction,
    cell_population,
    target_pop = NULL,
    cell_friction_raw = NULL,
    barrier_crossing_list = NULL,
    name_col = NULL,
    barrier_penalty = 0,
    compactness_penalty = 0,
    max_cost = Inf,
    starter_rings = 1
    ) {
      site_sf <- normalize_sites(
        site_sf = site_sf,
        grid_sf = grid_sf,
        name_col = name_col
      )
      
      site_sf <- snap_seeds_into_district(
        site_sf = site_sf,
        district_sf = district_sf,
        grid_sf = grid_sf,
        cell_friction_raw = cell_friction_raw,
        inward_buffer_m = 500,
        max_snap_friction = 0.98
      )
      
      propagate_assignments(
        grid_sf = grid_sf,
        site_sf = site_sf,
        neighbors_list = neighbors_list,
        cell_friction = cell_friction,
        cell_population = cell_population,
        target_pop = target_pop,
        barrier_crossing_list = barrier_crossing_list,
        barrier_penalty = barrier_penalty,
        compactness_penalty = compactness_penalty,
        max_cost = max_cost,
        starter_rings = starter_rings
      )
    }
    
=======

>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
    scene <- reactive({
      req(district_sf())
      req(nrow(district_sf()) > 0)
      req(grid_n())
<<<<<<< HEAD
      
      district_sf_value <- safe_make_valid(district_sf())
      
=======

      district_sf_value <- safe_make_valid(district_sf())

>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
      grid_info <- make_paint_grid(
        district_sf = district_sf_value,
        grid_n = grid_n()
      )
<<<<<<< HEAD
      
      grid_sf_value <- grid_info$grid_sf
      
      neighbors_list_value <- build_neighbors_list(
        grid_sf = grid_sf_value,
        allow_diagonal = allow_diagonal
      )
      
      friction_path_value <- get_district_friction_path(
        district_sf = district_sf_value,
        friction_dir = friction_dir,
        friction_lookup_csv = friction_lookup_csv
      )
      
      cell_friction_raw_value <- extract_cell_friction_from_raster(
        grid_sf = grid_sf_value,
        district_sf = district_sf_value,
        friction_path = friction_path_value
      )
      
      cell_friction_value <- friction_to_cost(
        cell_friction_raw_value,
        min_cost = 1,
        soft_block_cost = 1000,
        knee = 0.90,
        power_low = 2,
        power_high = 6
      )
      
      # ----------------------------------------------------------
      # NEW: compute population per grid cell
      # ----------------------------------------------------------
      
      u5_rast <- load_worldpop_u5_raster(
        t_u1_1to4_file = worldpop_t_u1_1to4_file
      )
      
      grid_sf_value$u5_pop <- calculate_grid_cell_population(
        grid_sf_value,
        u5_rast
      )
      
      # ----------------------------------------------------------
      
      barrier_sf_value <- barrier_lines_sf()
      
      if (!is.null(barrier_sf_value) && nrow(barrier_sf_value) > 0) {
        
        barrier_sf_value <- safe_make_valid(barrier_sf_value)
        
        barrier_crossing_list_value <- build_barrier_crossing_matrix(
          grid_sf = grid_sf_value,
          neighbors_list = neighbors_list_value,
          barrier_lines_sf = barrier_sf_value
        )
        
      } else {
        
        barrier_crossing_list_value <- NULL
        
      }
      
      facility_sf_value <- facility_seed_sf()
      
      # ----------------------------------------------------------
      # NEW: compute target population
      # ----------------------------------------------------------
      
      n_seed_areas <- if (
        !is.null(facility_sf_value) &&
        nrow(facility_sf_value) > 0
      ) {
        
        nrow(facility_sf_value)
        
      } else {
        
        n_dfa
        
      }
      
      target_pop_value <-
        sum(grid_sf_value$u5_pop, na.rm = TRUE) /
        n_seed_areas
      
      # ----------------------------------------------------------
      
      if (
        !is.null(facility_sf_value) &&
        nrow(facility_sf_value) > 0
      ) {
        
        start_info <- make_start_assignment_from_sites(
          
          grid_sf = grid_sf_value,
          district_sf = district_sf_value,
          site_sf = facility_sf_value,
          
          neighbors_list = neighbors_list_value,
          
          cell_friction = cell_friction_value,
          
          # NEW
          cell_population = grid_sf_value$u5_pop,
          target_pop = target_pop_value,
          
          barrier_crossing_list = barrier_crossing_list_value,
          
          name_col = facility_name_col,
          
          barrier_penalty = barrier_penalty,
          compactness_penalty = compactness_penalty,
          max_cost = max_cost,
          
          starter_rings = 2
          
        )
        
      } else {
        
        start_info <- make_start_assignment(
          
          grid_sf = grid_sf_value,
          district_sf = district_sf_value,
          
          neighbors_list = neighbors_list_value,
          
          cell_friction = cell_friction_value,
          
          # NEW
          cell_population = grid_sf_value$u5_pop,
          target_pop = target_pop_value,
          
          barrier_crossing_list = barrier_crossing_list_value,
          
          n_dfa = n_dfa,
          seed = seed,
          
          barrier_penalty = barrier_penalty,
          compactness_penalty = compactness_penalty,
          max_cost = max_cost,
          
          starter_rings = 2
          
        )
        
      }
      
      seed_outputs <- build_seed_points_outputs(
        start_info$seeds_sf
      )
      
      list(
        
        district_sf = district_sf_value,
        
        grid_sf = grid_sf_value,
        
        initial_assignments =
          as.character(start_info$assignments),
        
        seed_points_sf =
          seed_outputs$seed_points_sf,
        
        seed_points_df =
          seed_outputs$seed_points_df,
        
        seed_points_list =
          seed_outputs$seed_points_list,
        
        neighbors_list =
          neighbors_list_value,
        
        edge_list =
          build_edge_list(
            grid_sf_value,
            district_sf_value
          ),
        
        max_dim_m =
          grid_info$max_dim_m,
        
        cell_friction =
          cell_friction_value,
        
        cell_friction_raw =
          cell_friction_raw_value,
        
        cumulative_cost =
          as.numeric(start_info$cumulative_cost),
        
        seed_cell_id =
          as.integer(start_info$seed_cell_id),
        
        barrier_crossing_list =
          barrier_crossing_list_value,
        
        friction_path =
          friction_path_value
        
      )
    })
    
=======

      grid_sf_value <- grid_info$grid_sf
      facility_sf_value <- facility_seed_sf()

      if (!is.null(facility_sf_value) && nrow(facility_sf_value) > 0) {
        start_info <- make_start_assignment_from_sites(
          grid_sf = grid_sf_value,
          site_sf = facility_sf_value,
          name_col = facility_name_col
        )
      } else {
        start_info <- make_start_assignment(
          grid_sf = grid_sf_value,
          district_sf = district_sf_value,
          n_dfa = n_dfa,
          seed = seed
        )
      }

      seed_outputs <- build_seed_points_outputs(start_info$seeds_sf)

      list(
        district_sf = district_sf_value,
        grid_sf = grid_sf_value,
        initial_assignments = as.character(start_info$assignments),
        seed_points_sf = seed_outputs$seed_points_sf,
        seed_points_df = seed_outputs$seed_points_df,
        seed_points_list = seed_outputs$seed_points_list,
        neighbors_list = build_neighbors_list(grid_sf_value),
        edge_list = build_edge_list(grid_sf_value, district_sf_value),
        max_dim_m = grid_info$max_dim_m
      )
    })

>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
    list(
      scene = scene,
      district_sf = reactive(scene()$district_sf),
      grid_sf = reactive(scene()$grid_sf),
      initial_assignments = reactive(scene()$initial_assignments),
      seed_points_sf = reactive(scene()$seed_points_sf),
      seed_points_df = reactive(scene()$seed_points_df),
      seed_points_list = reactive(scene()$seed_points_list),
      neighbors_list = reactive(scene()$neighbors_list),
      edge_list = reactive(scene()$edge_list),
<<<<<<< HEAD
      max_dim_m = reactive(scene()$max_dim_m),
      cell_friction = reactive(scene()$cell_friction),
      cell_friction_raw = reactive(scene()$cell_friction_raw),
      cumulative_cost = reactive(scene()$cumulative_cost),
      seed_cell_id = reactive(scene()$seed_cell_id),
      barrier_crossing_list = reactive(scene()$barrier_crossing_list),
      friction_path = reactive(scene()$friction_path)
    )
    })
  }
=======
      max_dim_m = reactive(scene()$max_dim_m)
    )
  })
}
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
