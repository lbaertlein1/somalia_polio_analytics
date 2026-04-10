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
  min_n_raw <- clamp_num(max_dim_m / 350, 100, 200)
  max_n_raw <- clamp_num(max_dim_m / 120, 100, 200)

  min_n <- floor(min_n_raw)
  max_n <- ceiling(max_n_raw)

  if (max_n <= min_n) {
    max_n <- min_n + 20
  }

  range_n <- max_n - min_n
  step_n <- round(range_n * 0.10)
  step_n <- clamp_num(step_n, 2, 25)

  if (step_n <= 5) {
    step_n <- 1
  } else if (step_n <= 10) {
    step_n <- 2
  } else if (step_n <= 20) {
    step_n <- 5
  } else {
    step_n <- 10
  }

  default_n <- round((min_n + max_n) / 2)

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

make_fill_colors <- function(active_dfa) {
  out <- setNames(rep(nonselected_fill_color, length(all_dfa_names)), all_dfa_names)
  out[names(special_fill_colors)] <- special_fill_colors
  if (!is.null(active_dfa) && active_dfa %in% starter_dfa_names) {
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

build_saved_dfa_sf <- function(grid_sf, assignments, district_sf) {
  build_dfa_polygons_from_assignments(
    grid_sf = grid_sf,
    assignments = assignments,
    district_sf = district_sf
  )
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

make_population_overlay_sf <- function(district_sf, u5_rast, max_dim_cells = 120) {
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
