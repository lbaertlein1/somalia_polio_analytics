show_help_modal <- function(session) {
  showModal(
    modalDialog(
      title = tags$span(
        style = 'font-size: 15px; font-weight: 700; color: #0f172a;',
        'Health Area Mapping — How to use this tab'
      ),
      div(
        style = 'font-size: 13px; line-height: 1.7; color: #334155; max-height: 65vh; overflow-y: auto;',
        
        # ── What is a health area ─────────────────────────────────────────────
        div(
          style = 'margin-bottom: 16px;',
          tags$p(
            'A ', tags$strong('Health Area'), ' is the operational area covered by vaccination teams ',
            'supervised from one health facility. Each health area should target approximately ',
            tags$strong('2,000 children under 5'), ' and be covered by ',
            tags$strong('5–6 vaccination teams'), '.'
          )
        ),
        
        tags$hr(style = 'border-color: #e2e8f0; margin: 12px 0;'),
        
        # ── Controls ──────────────────────────────────────────────────────────
        div(
          style = 'margin-bottom: 14px;',
          tags$p(style = 'font-weight: 700; color: #0f172a; margin-bottom: 6px;',
                 'Map controls'),
          tags$ul(
            style = 'margin: 0; padding-left: 18px;',
            tags$li(tags$strong('Left click'), ' — paint cells to the selected health area'),
            tags$li(tags$strong('Right click + drag'), ' — move the map'),
            tags$li(tags$strong('Scroll wheel'), ' — zoom in / out'),
            tags$li(tags$strong('Brush size slider'), ' — adjust the painting brush size')
          )
        ),
        
        tags$hr(style = 'border-color: #e2e8f0; margin: 12px 0;'),
        
        # ── Steps ─────────────────────────────────────────────────────────────
        div(
          style = 'margin-bottom: 14px;',
          tags$p(style = 'font-weight: 700; color: #0f172a; margin-bottom: 10px;',
                 'Suggested steps'),
          
          .help_step('1', 'Mark Unpopulated areas',
                     'Paint areas with no resident population — desert, water bodies, industrial land.'),
          
          .help_step('2', 'Mark Inaccessible areas',
                     'Paint areas vaccination teams cannot reach — insecurity, flooding, impassable terrain.'),
          
          .help_step('3', 'Adjust Health Area boundaries',
                     paste0(
                       'Select a health area from the population table on the right, then paint. ',
                       'Boundaries should follow recognisable features and give teams manageable, ',
                       'well-supervised workloads. Aim for ~2,000 children per area.'
                     )
          ),
          
          .help_step('4', 'Check all cells are assigned',
                     paste0(
                       'Every cell must belong to exactly one category — a Health Area, ',
                       'Inaccessible, or Unpopulated. No gaps or overlaps.'
                     )
          ),
          
          .help_step('5', 'Save',
                     paste0(
                       'Click Save when finished. Saved boundaries carry forward to the ',
                       'Microplan Prep tab where population, teams and supervisors are recorded.'
                     )
          )
        ),
        
        tags$hr(style = 'border-color: #e2e8f0; margin: 12px 0;'),
        
        # ── Population note ───────────────────────────────────────────────────
        tags$p(
          style = 'font-size: 12px; color: #64748b; margin: 0;',
          tags$strong('Population estimates'), ' in the right panel are from WorldPop ',
          '(children under 5). Values can be adjusted in the Microplan Prep tab.'
        )
      ),
      easyClose = TRUE,
      size      = 'm',
      footer    = modalButton('Close')
    )
  )
}

# ── Step item helper ───────────────────────────────────────────────────────────
.help_step <- function(num, title, desc) {
  div(
    style = 'display:flex;align-items:flex-start;gap:12px;margin-bottom:10px;',
    div(
      style = paste0(
        'width:22px;height:22px;border-radius:50%;',
        'background:#0d9488;color:#fff;flex-shrink:0;margin-top:1px;',
        'display:flex;align-items:center;justify-content:center;',
        'font-size:11px;font-weight:700;'
      ),
      num
    ),
    div(
      tags$span(style = 'font-weight:600;color:#0f172a;', title),
      tags$span(style = 'color:#64748b;', paste0(' — ', desc))
    )
  )
}

calc_grid_limits <- function(max_dim_m) {
  min_n_raw <- clamp_num(max_dim_m / 350, 100, 300)
  max_n_raw <- clamp_num(max_dim_m / 120, 140, 350)
  
  min_n <- floor(min_n_raw)
  max_n <- ceiling(max_n_raw)
  
  if (max_n <= min_n) {
    max_n <- min_n + 20
  }
  
  range_n <- max_n - min_n
  step_n <- round(range_n * 0.10)
  step_n <- clamp_num(step_n, 2, 20)
  
  if (step_n <= 5) {
    step_n <- 1
  } else if (step_n <= 10) {
    step_n <- 2
  } else if (step_n <= 20) {
    step_n <- 5
  } else {
    step_n <- 10
  }
  
  default_n <- round(min_n + 0.40 * (max_n - min_n))
  
  list(
    min = as.integer(min_n),
    max = as.integer(max_n),
    value = as.integer(default_n),
    step = as.integer(step_n)
  )
}

calc_brush_limits <- function(max_dim_m) {
  min_b <- round_to_step(clamp_num(max_dim_m * 0.02, 50, 10000), brush_step_m)
  max_b <- round_to_step(clamp_num(max_dim_m * 0.18, 50, 10000), brush_step_m)
  
  if (max_b <= min_b) {
    max_b <- clamp_num(min_b + brush_step_m, 50, 10000)
  }
  
  default_b <- round_to_step((min_b + max_b) / 2, brush_step_m)
  default_b <- clamp_num(default_b, min_b, max_b)
  
  list(
    min = as.integer(min_b),
    max = as.integer(max_b),
    value = as.integer(default_b),
    step = as.integer(brush_step_m)
  )
}

calc_district_max_dim <- function(district_sf) {
  district_3857 <- st_transform(safe_make_valid(district_sf), 3857)
  bbox <- st_bbox(district_3857)
  as.numeric(max(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin))
}

as_geojson_text <- function(x) {
  geojsonsf::sf_geojson(x)
}

make_fill_colors <- function(active_dfa, dfa_names = all_dfa_names) {
  dfa_names <- unique(as.character(dfa_names))
  dfa_names <- c(setdiff(dfa_names, extra_dfa_names), extra_dfa_names)
  
  out <- setNames(rep(nonselected_fill_color, length(dfa_names)), dfa_names)
  
  special_present <- intersect(names(special_fill_colors), dfa_names)
  out[special_present] <- special_fill_colors[special_present]
  
  if (!is.null(active_dfa) && active_dfa %in% dfa_names && !(active_dfa %in% extra_dfa_names)) {
    out[active_dfa] <- selected_fill_color
  }
  
  out
}

make_paint_grid <- function(district_sf, grid_n = 150) {
  district_sf <- safe_make_valid(district_sf)
  district_3857 <- st_transform(district_sf, 3857)
  
  bbox <- st_bbox(district_3857)
  width_m <- bbox$xmax - bbox$xmin
  height_m <- bbox$ymax - bbox$ymin
  max_dim <- max(width_m, height_m)
  
  cellsize <- max_dim / grid_n
  
  raw_grid <- st_make_grid(
    district_3857,
    cellsize = cellsize,
    what = 'polygons',
    square = TRUE
  )
  
  grid_sf <- st_sf(
    cell_id = seq_along(raw_grid),
    geometry = raw_grid,
    crs = st_crs(district_3857)
  )
  
  cent_3857 <- suppressWarnings(st_centroid(grid_sf))
  inside <- lengths(st_within(cent_3857, district_3857)) > 0
  
  grid_sf <- grid_sf |>
    filter(inside) |>
    mutate(cell_id = seq_len(n()))
  
  cent_wgs84 <- st_transform(cent_3857[inside, ], 4326)
  coords <- st_coordinates(cent_wgs84)
  
  grid_sf <- st_transform(grid_sf, 4326)
  grid_sf$centroid_lon <- coords[, 1]
  grid_sf$centroid_lat <- coords[, 2]
  
  list(
    grid_sf = grid_sf,
    max_dim_m = as.numeric(max_dim)
  )
}

make_start_assignment <- function(grid_sf, district_sf, n_dfa = 5, seed = 1) {
  set.seed(seed)
  
  pts <- st_sample(district_sf, size = n_dfa, exact = TRUE)
  
  pts_sf <- st_sf(
    dfa_name = paste('Health Area', seq_len(n_dfa)),
    geometry = pts,
    crs = st_crs(district_sf)
  )
  
  cent <- suppressWarnings(st_centroid(grid_sf))
  idx <- st_nearest_feature(cent, pts_sf)
  
  list(
    assignments = as.character(pts_sf$dfa_name[idx]),
    seeds_sf = pts_sf
  )
}

build_dfa_polygons_from_assignments <- function(grid_sf, assignments, district_sf) {
  stopifnot(length(assignments) == nrow(grid_sf))
  
  # Normalise CRS — grid_sf may be 3857 (planning area path) or 4326
  target_crs <- sf::st_crs(grid_sf)
  if (!identical(sf::st_crs(district_sf), target_crs))
    district_sf <- sf::st_transform(district_sf, target_crs)
  
  out <- grid_sf |>
    mutate(dfa_name = assignments) |>
    dplyr::select(cell_id, centroid_lon, centroid_lat, dfa_name, geometry) |>
    group_by(dfa_name) |>
    summarise(geometry = st_union(geometry), .groups = 'drop')
  
  out <- safe_make_valid(out)
  out <- suppressWarnings(st_intersection(out, district_sf))
  out <- safe_make_valid(out)
  out <- tryCatch(sf::st_collection_extract(out, 'POLYGON'), error = function(e) out)
  out$geometry <- st_cast(out$geometry, 'MULTIPOLYGON', warn = FALSE)
  
  out |>
    dplyr::select(dfa_name, geometry)
}


smooth_dfa_boundaries <- function(dfa_sf, district_sf, iterations = 1) {
  dfa_sf <- safe_make_valid(dfa_sf)
  
  # Normalise CRS
  if (!identical(sf::st_crs(dfa_sf), sf::st_crs(district_sf)))
    district_sf <- sf::st_transform(district_sf, sf::st_crs(dfa_sf))
  
  dfa_sf <- rmapshaper::ms_smooth(
    dfa_sf,
    method = "chaikin",
    iterations = iterations
  )
  
  dfa_sf <- suppressWarnings(sf::st_intersection(dfa_sf, district_sf))
  dfa_sf <- safe_make_valid(dfa_sf)
  dfa_sf <- tryCatch(sf::st_collection_extract(dfa_sf, 'POLYGON'), error = function(e) dfa_sf)
  dfa_sf$geometry <- sf::st_cast(dfa_sf$geometry, "MULTIPOLYGON", warn = FALSE)
  
  dfa_sf
}

make_dfa_label_points <- function(dfa_sf) {
  if (is.null(dfa_sf) || nrow(dfa_sf) == 0) return(NULL)
  
  pts <- suppressWarnings(st_point_on_surface(dfa_sf))
  coords <- st_coordinates(pts)
  
  data.frame(
    dfa_name = dfa_sf$dfa_name,
    lon = coords[, 1],
    lat = coords[, 2],
    stringsAsFactors = FALSE
  )
}

load_worldpop_u5_raster <- function(t_u1_1to4_file) {
  t_path <- path.expand(t_u1_1to4_file)
  
  if (!file.exists(t_path)) {
    stop(paste0('WorldPop raster not found: ', t_path))
  }
  
  u5 <- terra::rast(t_path)
  names(u5) <- 'u5_pop'
  u5
}

calculate_grid_cell_population <- function(grid_sf, u5_rast) {
  if (is.null(grid_sf) || nrow(grid_sf) == 0) return(numeric(0))
  
  grid_proj <- sf::st_transform(grid_sf, sf::st_crs(terra::crs(u5_rast)))
  vals <- exactextractr::exact_extract(
    x = raster::raster(u5_rast),
    y = grid_proj,
    fun = 'sum'
  )
  
  vals[is.na(vals)] <- 0
  as.numeric(vals)
}

make_population_overlay_sf <- function(district_sf, u5_rast, max_dim_cells = Inf) {
  district_vect <- terra::vect(sf::st_transform(district_sf, terra::crs(u5_rast)))
  r_crop <- terra::crop(u5_rast, district_vect, snap = 'out')
  r_mask <- terra::mask(r_crop, district_vect)
  
  vals0 <- terra::values(r_mask)
  if (all(is.na(vals0))) return(NULL)
  
  factor_x <- max(1, ceiling(ncol(r_mask) / max_dim_cells))
  factor_y <- max(1, ceiling(nrow(r_mask) / max_dim_cells))
  fact <- max(factor_x, factor_y)
  
  r_small <- terra::aggregate(r_mask, fact = fact, fun = mean, na.rm = TRUE)
  p <- terra::as.polygons(r_small, na.rm = TRUE)
  names(p) <- 'pop_u5'
  
  pop_sf <- sf::st_as_sf(p)
  pop_sf <- sf::st_transform(pop_sf, 4326)
  pop_sf <- safe_make_valid(pop_sf)
  
  vals <- pop_sf$pop_u5
  vals_non_na <- vals[is.finite(vals) & !is.na(vals)]
  if (length(vals_non_na) == 0) return(NULL)
  
  breaks <- unique(stats::quantile(vals_non_na, probs = seq(0, 1, length.out = 6), na.rm = TRUE))
  if (length(breaks) < 2) {
    breaks <- c(min(vals_non_na, na.rm = TRUE), max(vals_non_na, na.rm = TRUE) + 1e-9)
  }
  
  cols <- pop_palette(max(1, length(breaks) - 1))
  idx <- cut(vals, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  
  fill_color <- rep('#000000', length(vals))
  ok <- !is.na(idx)
  fill_color[ok] <- cols[idx[ok]]
  
  pop_sf$fill_color <- fill_color
  pop_sf
}


make_friction_overlay_sf <- function(
    district_sf,
    friction_rast,
    max_dim_cells = Inf
) {
  cat("\n--- make_friction_overlay_sf running ---\n")
  
  district_vect <- terra::vect(
    sf::st_transform(district_sf, terra::crs(friction_rast))
  )
  
  r_crop <- terra::crop(friction_rast, district_vect, snap = "out")
  r_mask <- terra::mask(r_crop, district_vect)
  
  vals0 <- terra::values(r_mask)
  vals0 <- vals0[is.finite(vals0) & !is.na(vals0)]
  
  if (length(vals0) == 0) return(NULL)
  
  cat("raw masked quantiles:\n")
  print(quantile(vals0, probs = c(0, 0.50, 0.80, 0.93, 0.98, 1), na.rm = TRUE))
  
  factor_x <- max(1, ceiling(ncol(r_mask) / max_dim_cells))
  factor_y <- max(1, ceiling(nrow(r_mask) / max_dim_cells))
  fact <- max(factor_x, factor_y)
  
  cat("aggregation fact:", fact, "\n")
  
  r_small <- terra::aggregate(
    r_mask,
    fact = fact,
    fun = max,
    na.rm = TRUE
  )
  
  vals_small <- terra::values(r_small)
  vals_small <- vals_small[is.finite(vals_small) & !is.na(vals_small)]
  
  if (length(vals_small) == 0) return(NULL)
  
  cat("aggregated quantiles:\n")
  print(quantile(vals_small, probs = c(0, 0.50, 0.80, 0.93, 0.98, 1), na.rm = TRUE))
  
  # Fixed breaks from the viewer that worked better
  breaks_raw <- c(
    0,
    0.000001,
    0.05,
    0.1,
    0.2,
    0.4,
    0.6,
    0.8,
    0.999999,
    1.000001
  )
  
  friction_cols <- c(
    "#FFFFFF",  # 0
    "#440154",
    "#3B528B",
    "#21918C",
    "#5DC863",
    "#FDE725",
    "#FDB863",
    "#E66101",
    "#B2182B"   # 1 / impassable
  )
  
  friction_labels <- c(
    "0 (zero)",
    "0–0.05",
    "0.05–0.1",
    "0.1–0.2",
    "0.2–0.4",
    "0.4–0.6",
    "0.6–0.8",
    "0.8–<1",
    "1 (impassable)"
  )
  
  cat("breaks used:\n")
  print(breaks_raw)
  
  # --------------------------------------------------
  # IMPORTANT: classify raster FIRST, polygonize SECOND
  # --------------------------------------------------
  m <- matrix(
    c(
      breaks_raw[-length(breaks_raw)],
      breaks_raw[-1],
      seq_len(length(breaks_raw) - 1)
    ),
    ncol = 3
  )
  
  r_class <- terra::classify(
    r_small,
    rcl = m,
    include.lowest = TRUE,
    right = FALSE
  )
  
  vals_class <- terra::values(r_class)
  vals_class <- vals_class[is.finite(vals_class) & !is.na(vals_class)]
  
  cat("classified raster counts:\n")
  print(table(vals_class, useNA = "ifany"))
  
  p <- terra::as.polygons(r_class, na.rm = TRUE)
  names(p) <- "friction_class"
  
  friction_sf <- sf::st_as_sf(p)
  friction_sf <- sf::st_transform(friction_sf, 4326)
  friction_sf <- safe_make_valid(friction_sf)
  
  idx <- as.integer(friction_sf$friction_class)
  
  cat("polygon class counts:\n")
  print(table(idx, useNA = "ifany"))
  
  friction_sf$fill_color <- friction_cols[idx]
  friction_sf$friction_label <- friction_labels[idx]
  
  cat("friction polygons:", nrow(friction_sf), "\n")
  
  friction_sf
}
# write_raster_overlay_png <- function(
    #     rast,
#     session,
#     prefix = "raster_overlay",
#     palette = "viridis"
# ) {
#   if (is.null(rast)) {
#     return(NULL)
#   }
#   
#   if (!inherits(rast, "SpatRaster")) {
#     stop("write_raster_overlay_png expects a SpatRaster")
#   }
#   
#   vals <- terra::values(rast, mat = FALSE)
#   vals <- vals[is.finite(vals) & !is.na(vals)]
#   
#   if (length(vals) == 0) {
#     return(NULL)
#   }
#   
#   lower <- min(vals, na.rm = TRUE)
#   upper <- max(vals, na.rm = TRUE)
#   
#   if (!is.finite(upper) || upper <= lower) {
#     upper <- lower + 1e-6
#   }
#   
#   pal_fun <- leaflet::colorNumeric(
#     palette = palette,
#     domain = c(lower, upper),
#     na.color = "transparent"
#   )
#   
#   m <- terra::as.matrix(rast, wide = TRUE)
#   
#   # flip rows so north is at the top in the PNG
#   m <- m[nrow(m):1, , drop = FALSE]
#   
#   hex_mat <- matrix(
#     pal_fun(as.vector(m)),
#     nrow = nrow(m),
#     ncol = ncol(m),
#     byrow = FALSE
#   )
#   
#   rgba <- grDevices::col2rgb(as.vector(hex_mat), alpha = TRUE) / 255
#   
#   img <- array(
#     0,
#     dim = c(nrow(hex_mat), ncol(hex_mat), 4)
#   )
#   
#   for (k in 1:4) {
#     img[, , k] <- matrix(
#       rgba[k, ],
#       nrow = nrow(hex_mat),
#       ncol = ncol(hex_mat),
#       byrow = FALSE
#     )
#   }
#   
#   tf <- tempfile(
#     pattern = paste0(prefix, "_"),
#     fileext = ".png"
#   )
#   
#   png::writePNG(img, target = tf)
#   
#   ext <- terra::ext(rast)
#   
#   bounds <- list(
#     xmin = as.numeric(ext$xmin),
#     ymin = as.numeric(ext$ymin),
#     xmax = as.numeric(ext$xmax),
#     ymax = as.numeric(ext$ymax)
#   )
#   
#   resource_prefix <- paste0(prefix, "_", session$token)
#   
#   shiny::addResourcePath(
#     prefix = resource_prefix,
#     directoryPath = dirname(tf)
#   )
#   
#   list(
#     url = paste0(
#       "/",
#       resource_prefix,
#       "/",
#       basename(tf)
#     ),
#     bounds = bounds,
#     lower = lower,
#     upper = upper
#   )
# }



build_saved_dfa_sf <- function(
    grid_sf,
    assignments,
    district_sf,
    outer_buffer_m = 500
) {
  # Original exact partition
  raw <- build_dfa_polygons_from_assignments(
    grid_sf = grid_sf,
    assignments = assignments,
    district_sf = district_sf
  )
  
  raw <- safe_make_valid(raw)
  district_sf <- safe_make_valid(district_sf)
  
  orig_crs <- sf::st_crs(raw)
  
  raw_proj <- sf::st_transform(raw, 3857)
  district_proj <- sf::st_transform(district_sf, 3857)
  
  # Buffered areas, clipped to district
  buffered <- suppressWarnings(
    sf::st_intersection(
      sf::st_buffer(raw_proj, dist = outer_buffer_m),
      district_proj
    )
  )
  buffered <- safe_make_valid(buffered)
  
  # Area not covered by original raw partition
  fringe <- suppressWarnings(
    sf::st_difference(
      district_proj,
      sf::st_union(raw_proj)
    )
  )
  
  # If no fringe, return raw
  if (length(fringe) == 0 || all(sf::st_is_empty(fringe))) {
    out <- raw_proj
  } else {
    fringe_sf <- sf::st_as_sf(fringe)
    fringe_sf <- fringe_sf[!sf::st_is_empty(fringe_sf), , drop = FALSE]
    
    if (nrow(fringe_sf) == 0) {
      out <- raw_proj
    } else {
      fringe_sf$piece_id <- seq_len(nrow(fringe_sf))
      
      # First try to label fringe by overlap with buffered areas
      ov <- suppressWarnings(
        sf::st_intersection(
          fringe_sf,
          buffered |>
            dplyr::select(dfa_name)
        )
      )
      ov <- safe_make_valid(ov)
      
      if (nrow(ov) > 0) {
        ov$ov_area <- as.numeric(sf::st_area(ov))
        
        labels <- ov |>
          sf::st_drop_geometry() |>
          dplyr::group_by(piece_id) |>
          dplyr::slice_max(ov_area, n = 1, with_ties = FALSE) |>
          dplyr::ungroup() |>
          dplyr::select(piece_id, dfa_name)
        
        fringe_sf <- fringe_sf |>
          dplyr::left_join(labels, by = "piece_id")
      } else {
        fringe_sf$dfa_name <- NA_character_
      }
      
      # Any unlabeled fringe gets assigned to nearest raw area
      if (any(is.na(fringe_sf$dfa_name))) {
        idx <- sf::st_nearest_feature(
          sf::st_point_on_surface(fringe_sf[is.na(fringe_sf$dfa_name), ]),
          raw_proj
        )
        fringe_sf$dfa_name[is.na(fringe_sf$dfa_name)] <- raw_proj$dfa_name[idx]
      }
      
      out <- dplyr::bind_rows(
        raw_proj |>
          dplyr::select(dfa_name),
        fringe_sf |>
          dplyr::select(dfa_name)
      ) |>
        dplyr::group_by(dfa_name) |>
        dplyr::summarise(do_union = TRUE, .groups = "drop")
    }
  }
  
  out <- safe_make_valid(out)
  
  # Final patch for any tiny residual holes
  residual <- suppressWarnings(
    sf::st_difference(
      district_proj,
      sf::st_union(out)
    )
  )
  
  if (!(length(residual) == 0 || all(sf::st_is_empty(residual)))) {
    residual_sf <- sf::st_as_sf(residual)
    residual_sf <- residual_sf[!sf::st_is_empty(residual_sf), , drop = FALSE]
    
    if (nrow(residual_sf) > 0) {
      idx <- sf::st_nearest_feature(
        sf::st_point_on_surface(residual_sf),
        out
      )
      residual_sf$dfa_name <- out$dfa_name[idx]
      
      out <- dplyr::bind_rows(
        out |>
          dplyr::select(dfa_name),
        residual_sf |>
          dplyr::select(dfa_name)
      ) |>
        dplyr::group_by(dfa_name) |>
        dplyr::summarise(do_union = TRUE, .groups = "drop")
    }
  }
  
  out <- safe_make_valid(out)
  # Merge any remaining slivers into main bodies
  out <- out |>
    dplyr::group_by(dfa_name) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
    sf::st_as_sf()
  out <- safe_make_valid(out)
  out <- tryCatch(sf::st_collection_extract(out, 'POLYGON'), error = function(e) out)
  
  out <- suppressWarnings(sf::st_intersection(out, district_proj))
  out <- safe_make_valid(out)
  out <- tryCatch(sf::st_collection_extract(out, 'POLYGON'), error = function(e) out)
  
  # Re-merge after intersection in case it split rows
  out <- out |>
    dplyr::group_by(dfa_name) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
    sf::st_as_sf()
  out <- safe_make_valid(out)
  out <- tryCatch(sf::st_collection_extract(out, 'POLYGON'), error = function(e) out)
  out <- sf::st_cast(out, "MULTIPOLYGON", warn = FALSE)
  
  # Safety check: ensure no dfa_name was silently dropped by st_intersection.
  # If any area is missing, recover it from raw_proj clipped to district.
  expected_names <- unique(raw_proj$dfa_name)
  present_names  <- unique(out$dfa_name)
  missing_names  <- setdiff(expected_names, present_names)
  
  if (length(missing_names) > 0) {
    cat('[build_saved_dfa_sf] recovering dropped areas:', paste(missing_names, collapse = ', '), '\n')
    recovered_raw <- raw_proj[raw_proj$dfa_name %in% missing_names, , drop = FALSE]
    recovered <- tryCatch({
      r <- suppressWarnings(sf::st_intersection(recovered_raw, district_proj))
      r <- safe_make_valid(r)
      r <- tryCatch(sf::st_collection_extract(r, 'POLYGON'), error = function(e) r)
      r |>
        dplyr::group_by(dfa_name) |>
        dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
        sf::st_as_sf() |>
        safe_make_valid()
    }, error = function(e) {
      recovered_raw |>
        dplyr::group_by(dfa_name) |>
        dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
        sf::st_as_sf()
    })
    recovered <- tryCatch(sf::st_collection_extract(recovered, 'POLYGON'), error = function(e) recovered)
    recovered <- sf::st_cast(recovered, 'MULTIPOLYGON', warn = FALSE)
    
    out <- dplyr::bind_rows(
      out |> dplyr::select(dfa_name),
      recovered |> dplyr::select(dfa_name)
    ) |>
      dplyr::group_by(dfa_name) |>
      dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
      sf::st_as_sf() |>
      safe_make_valid()
    out <- tryCatch(sf::st_collection_extract(out, 'POLYGON'), error = function(e) out)
    out <- sf::st_cast(out, 'MULTIPOLYGON', warn = FALSE)
  }
  
  out |>
    dplyr::select(dfa_name) |>
    dplyr::arrange(dfa_name) |>
    sf::st_transform(orig_crs)
}