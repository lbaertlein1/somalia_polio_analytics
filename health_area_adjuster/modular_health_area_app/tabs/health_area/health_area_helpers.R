show_help_modal <- function(session) {
  showModal(
    modalDialog(
      title = 'Health Area Boundary Review for Microplanning',
      div(
        style = '
          max-height: 75vh;
          overflow-y: auto;
          font-size: 14px;
          line-height: 1.5;
        ',
        HTML("
<h3>About this tool</h3>

<p>
This tool is used to review and adjust <b>Health Area boundaries</b> before campaign microplanning.
</p>

<p>
Health Areas were first created by national GIS teams using available data. These initial boundaries are provided as a starting point. District teams are asked to review and adjust the boundaries to reflect how the district will be covered in polio vaccination campaigns.
</p>

<p>
A <b>Health Area</b> represents:
</p>

<ul>
<li>The operational area covered during vaccination campaigns by vaccination teams supervised by a health center</li>
<li>An area typically served by <b>5 to 6 vaccination teams</b></li>
<li>An area targeting approximately <b>2,000 children</b></li>
<li>Health areas should align as closely as possible with the population served by a health center</li>
</ul>

<p>
Accurate Health Area boundaries are important because they support:
</p>

<ul>
<li>Correct allocation of vaccination teams</li>
<li>Manageable workloads and effective supervision</li>
<li>Reliable population estimates</li>
<li>Accountability for coverage and missed children</li>
</ul>

<p>
This tool is intended to be used collaboratively during a district planning meeting prior to health area microplanning.
</p>

<p>
The boundaries finalized here will be used as the <b>foundation for health area microplanning</b>.
</p>

<hr>

<h3>How to use this tool</h3>
<p>
First, select a district on the Introduction tab. Once loaded, select a health area to begin editing. Use the mouse to 'paint' areas according to which health area they belong to. 
</p>
<ul>
<li>Left click to 'paint' which land belongs to the selected Health Area.</li>
<li>To adjust the size of the 'paint brush', adjust the Brush Size slider on the left sidebar.</li>
<li>Right click to move the map.</li>
<li>Use the scroll-wheel to zoom in or out.</li>
<li>When finished with a Health Area, simply select another or click 'Save'.</li>
</ul>

<h4>Step 1. Mark Unpopulated areas</h4>

<p>
Begin by identifying areas where no people live.
</p>

<ul>
<li>Desert or open land</li>
<li>Water bodies</li>
<li>Industrial or restricted land</li>
<li>Other areas with no resident population</li>
</ul>

<p>
Assign these areas to <b>Unpopulated</b>.
</p>

<h4>Step 2. Mark Inaccessible areas</h4>

<p>
Next, identify areas that vaccination teams cannot reach.
</p>

<ul>
<li>Areas affected by insecurity</li>
<li>Flooded areas</li>
<li>Terrain that cannot be safely accessed</li>
<li>Other areas where teams cannot operate</li>
</ul>

<p>
Assign these areas to <b>Inaccessible</b>.
</p>

<h4>Step 3. Review and adjust Health Area boundaries</h4>

<ul>
<li>Follow recognizable features</li>
<li>Ensure boundaries are easy for teams to understand</li>
<li>Ensure vaccination team coverage and supervision are practical</li>
</ul>

<h4>Step 4. Ensure all areas are assigned</h4>

<p>
Every location must belong to exactly one category:
</p>

<ul>
<li>A Health Area</li>
<li>Inaccessible</li>
<li>Unpopulated</li>
</ul>

<p>
There must be no gaps and no overlaps.
</p>

<h4>Step 5. Submit the completed district</h4>

<p>
Submit the finalized boundaries so they can be used for microplanning.
</p>

<hr>

<h3>Population estimates (WorldPop)</h3>

<p>
This tool uses <b>WorldPop population estimates</b> as the starting target population for each Health Area.
</p>

<p>
District teams may adjust these estimates if needed.
</p>

<hr>

<h3>Support</h3>

<p>
If you have questions or encounter problems while using this tool, contact the <b>national data team</b>.
</p>
")
      ),
      easyClose = TRUE,
      size = 'l'
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

  out <- grid_sf |>
    mutate(dfa_name = assignments) |>
    dplyr::select(cell_id, centroid_lon, centroid_lat, dfa_name, geometry) |>
    group_by(dfa_name) |>
    summarise(geometry = st_union(geometry), .groups = 'drop')

  out <- safe_make_valid(out)
  out <- suppressWarnings(st_intersection(out, district_sf))
  out <- safe_make_valid(out)
  out$geometry <- st_cast(out$geometry, 'MULTIPOLYGON', warn = FALSE)

  out |>
    dplyr::select(dfa_name, geometry)
}


smooth_dfa_boundaries <- function(dfa_sf, district_sf, iterations = 1) {
  dfa_sf <- safe_make_valid(dfa_sf)
  
  dfa_sf <- rmapshaper::ms_smooth(
    dfa_sf,
    method = "chaikin",
    iterations = iterations
  )
  
  dfa_sf <- suppressWarnings(sf::st_intersection(dfa_sf, district_sf))
  dfa_sf <- safe_make_valid(dfa_sf)
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
  out <- suppressWarnings(sf::st_intersection(out, district_proj))
  out <- safe_make_valid(out)
  out <- sf::st_cast(out, "MULTIPOLYGON", warn = FALSE)
  
  out |>
    dplyr::select(dfa_name) |>
    dplyr::arrange(dfa_name) |>
    sf::st_transform(orig_crs)
}
