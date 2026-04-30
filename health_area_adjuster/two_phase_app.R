library(shiny)
library(sf)
library(dplyr)
library(jsonlite)
library(geojsonsf)
library(terra)
library(exactextractr)
library(raster)
library(DT)
library(leaflet)
library(htmltools)

# =========================================================
# User config
# =========================================================

districts_file <- "districts_shp.Rds"
health_centers_file <- "health_centers.Rds"
worldpop_t_u1_1to4_file <- "som_u5_population_2025_100m.tif"

default_grid_n <- 100
brush_step_m <- 50

show_pop_default <- FALSE
boundary_only_default <- FALSE

selected_fill_color <- "#FFD400"
nonselected_fill_color <- "#757575"
special_fill_colors <- c(
  "Inaccessible" = "#D7301F",
  "Unpopulated" = "#FFFFFF"
)
special_area_names <- names(special_fill_colors)

pop_palette <- colorRampPalette(c(
  "#feebe2", "#fbb4b9", "#f768a1", "#c51b8a", "#7a0177"
))

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# =========================================================
# Helpers
# =========================================================

safe_make_valid <- function(x) {
  tryCatch(st_make_valid(x), error = function(e) x)
}

as_geojson_text <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(NULL)
  geojsonsf::sf_geojson(x)
}


as_js_point_rows <- function(df, cols = c("lon", "lat")) {
  if (is.null(df) || nrow(df) == 0) return(list())
  
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  keep <- cols[cols %in% names(out)]
  
  if (length(keep) > 0) {
    for (nm in keep) {
      out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
    }
    
    ok <- stats::complete.cases(out[, keep, drop = FALSE])
    for (nm in keep) {
      ok <- ok & is.finite(out[[nm]])
    }
    out <- out[ok, , drop = FALSE]
  }
  
  if (nrow(out) == 0) return(list())
  
  lapply(seq_len(nrow(out)), function(i) {
    as.list(out[i, , drop = FALSE])
  })
}

make_hf_icon <- function() {
  svg <- paste0(
    "data:image/svg+xml;utf8,",
    utils::URLencode(
      '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16"><circle cx="8" cy="8" r="4" fill="white" stroke="black" stroke-width="1.5"/></svg>',
      reserved = TRUE
    )
  )
  
  leaflet::icons(
    iconUrl = svg,
    iconWidth = 16,
    iconHeight = 16,
    iconAnchorX = 8,
    iconAnchorY = 8
  )
}

district_key <- function(zone, region, district) {
  paste(zone %||% "", region %||% "", district %||% "", sep = "|||")
}

show_intro_help_modal <- function(session) {
  showModal(
    modalDialog(
      title = "Health Area Boundary Review for Microplanning",
      div(
        style = "max-height:75vh; overflow-y:auto; font-size:14px; line-height:1.5;",
        HTML("
<h3>Overview</h3>
<p>This tool supports a two-step workflow for campaign microplanning.</p>

<h4>Step 1. Health Center Locations</h4>
<ul>
<li>Review all health center points shown on the map.</li>
<li>Confirm whether each point is in the correct location.</li>
<li>Confirm whether each health center is operational.</li>
<li>Confirm whether each health center will serve as a coordination site for polio campaigns.</li>
<li>Drag points to the correct location if needed.</li>
<li>Add any missing health centers.</li>
</ul>

<h4>Step 2. Health Area Mapping</h4>
<ul>
<li>Initial health areas are generated automatically from the health centers marked as campaign coordination sites.</li>
<li>Users then review and revise those areas manually using the paint tool.</li>
<li>Users should also mark <b>Unpopulated</b> and <b>Inaccessible</b> areas where needed.</li>
</ul>

<h4>Population estimates</h4>
<p>WorldPop U5 population estimates are used as starting estimates for each health area.</p>

<h4>Support</h4>
<p>If you encounter problems while using this tool, contact the national data team.</p>
")
      ),
      easyClose = TRUE,
      size = "l"
    )
  )
}

show_hc_help_modal <- function(session) {
  showModal(
    modalDialog(
      title = "Health Center Location Verification",
      div(
        style = "max-height:75vh; overflow-y:auto; font-size:14px; line-height:1.5;",
        HTML("
<h3>How to use this page</h3>

<ul>
<li>Select a district in the left sidebar.</li>
<li>Review the health center points shown on the map.</li>
<li>Click a point or select a row in the table to review that health center.</li>
<li>Drag the point if the location is incorrect.</li>
<li>Mark whether the health center is operational.</li>
<li>Mark whether it will serve as a coordination site for campaigns.</li>
<li>Add any missing health centers using the Add health center button, then click the map.</li>
<li>When complete, click Generate starting health areas.</li>
</ul>

<p>Only health centers marked as both <b>Operational</b> and <b>Coordination site for campaigns</b> will be used to generate the initial health areas.</p>
")
      ),
      easyClose = TRUE,
      size = "l"
    )
  )
}

show_ha_help_modal <- function(session) {
  showModal(
    modalDialog(
      title = "Health Area Boundary Review for Microplanning",
      div(
        style = "max-height:75vh; overflow-y:auto; font-size:14px; line-height:1.5;",
        HTML("
<h3>About this tool</h3>

<p>This tool is used to review and adjust <b>Health Area boundaries</b> before campaign microplanning.</p>

<p>The initial boundaries shown here are generated from the verified health centers selected as campaign coordination sites in Step 1.</p>

<p>A <b>Health Area</b> represents:</p>

<ul>
<li>The operational area covered during vaccination campaigns by vaccination teams supervised by a health center</li>
<li>An area typically served by <b>5 to 6 vaccination teams</b></li>
<li>An area targeting approximately <b>2,000 children</b></li>
<li>Health areas should align as closely as possible with the population served by a health center</li>
</ul>

<h3>How to use this tool</h3>
<ul>
<li>Left click to paint which land belongs to the selected Health Area.</li>
<li>Use the Brush Size slider to adjust the brush.</li>
<li>Right click to move the map.</li>
<li>Use the scroll wheel to zoom in or out.</li>
<li>Health center points are shown as a reference layer.</li>
</ul>
")
      ),
      easyClose = TRUE,
      size = "l"
    )
  )
}

load_worldpop_u5_raster <- function(path) {
  p <- path.expand(path)
  if (!file.exists(p)) return(NULL)
  r <- terra::rast(p)
  names(r) <- "u5_pop"
  r
}

estimate_u5_population <- function(polygons_sf, u5_rast, name_col = "area_name") {
  if (is.null(polygons_sf) || nrow(polygons_sf) == 0 || is.null(u5_rast)) return(data.frame())
  
  polys <- sf::st_transform(polygons_sf, sf::st_crs(terra::crs(u5_rast)))
  vals <- exactextractr::exact_extract(
    x = raster::raster(u5_rast),
    y = polys,
    fun = "sum"
  )
  
  data.frame(
    area_name = as.character(polys[[name_col]]),
    est_u5_pop = round(vals, 0),
    stringsAsFactors = FALSE
  )
}

make_population_overlay_sf <- function(district_sf, u5_rast, max_dim_cells = 120) {
  if (is.null(u5_rast)) return(NULL)
  
  district_vect <- terra::vect(sf::st_transform(district_sf, terra::crs(u5_rast)))
  r_crop <- terra::crop(u5_rast, district_vect, snap = "out")
  r_mask <- terra::mask(r_crop, district_vect)
  
  vals0 <- terra::values(r_mask)
  if (all(is.na(vals0))) return(NULL)
  
  factor_x <- max(1, ceiling(ncol(r_mask) / max_dim_cells))
  factor_y <- max(1, ceiling(nrow(r_mask) / max_dim_cells))
  fact <- max(factor_x, factor_y)
  
  r_small <- terra::aggregate(r_mask, fact = fact, fun = mean, na.rm = TRUE)
  p <- terra::as.polygons(r_small, na.rm = TRUE)
  names(p) <- "pop_u5"
  
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
  
  fill_color <- rep("#000000", length(vals))
  ok <- !is.na(idx)
  fill_color[ok] <- cols[idx[ok]]
  
  pop_sf$fill_color <- fill_color
  pop_sf
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
    what = "polygons",
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

build_polygons_from_assignments <- function(grid_sf, assignments, district_sf) {
  stopifnot(length(assignments) == nrow(grid_sf))
  
  out <- grid_sf |>
    mutate(area_name = assignments) |>
    dplyr::select(cell_id, centroid_lon, centroid_lat, area_name, geometry) |>
    group_by(area_name) |>
    summarise(geometry = st_union(geometry), .groups = "drop")
  
  out <- safe_make_valid(out)
  out <- suppressWarnings(st_intersection(out, district_sf))
  out <- safe_make_valid(out)
  out$geometry <- st_cast(out$geometry, "MULTIPOLYGON", warn = FALSE)
  out
}

make_area_label_points <- function(area_sf) {
  if (is.null(area_sf) || nrow(area_sf) == 0) return(NULL)
  
  area_sf <- area_sf |>
    filter(!area_name %in% special_area_names)
  
  if (nrow(area_sf) == 0) return(NULL)
  
  pts <- suppressWarnings(st_point_on_surface(area_sf))
  coords <- st_coordinates(pts)
  
  data.frame(
    area_name = area_sf$area_name,
    lon = coords[, 1],
    lat = coords[, 2],
    stringsAsFactors = FALSE
  )
}

clamp_num <- function(x, lo, hi) max(lo, min(hi, x))
round_to_step <- function(x, step) round(x / step) * step

calc_grid_limits <- function(max_dim_m) {
  min_n_raw <- clamp_num(max_dim_m / 350, 100, 200)
  max_n_raw <- clamp_num(max_dim_m / 120, 100, 200)
  
  min_n <- floor(min_n_raw)
  max_n <- ceiling(max_n_raw)
  if (max_n <= min_n) max_n <- min_n + 20
  
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

normalize_health_centers <- function(x) {
  if (is.null(x)) return(NULL)
  
  if (inherits(x, "sf")) {
    x <- safe_make_valid(x)
    if (!inherits(sf::st_geometry(x), "sfc_POINT")) {
      x <- suppressWarnings(sf::st_centroid(x))
    }
  } else {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }
  
  nm <- names(x)
  nml <- tolower(nm)
  
  pick_col <- function(cands) {
    i <- match(cands, nml)
    i <- i[!is.na(i)]
    if (length(i) == 0) return(NULL)
    nm[i[1]]
  }
  
  hc_name_col <- pick_col(c("health_center_name", "hc_name", "facility_name", "dfa_name", "name"))
  district_col <- pick_col(c("district_name", "district"))
  region_col <- pick_col(c("region_name", "region"))
  zone_col <- pick_col(c("zone_name", "zone"))
  lon_col <- pick_col(c("lon", "longitude", "x"))
  lat_col <- pick_col(c("lat", "latitude", "y"))
  
  if (inherits(x, "sf")) {
    coords <- sf::st_coordinates(sf::st_transform(x, 4326))
    x$lon <- coords[, 1]
    x$lat <- coords[, 2]
  } else {
    if (is.null(lon_col) || is.null(lat_col)) {
      stop("health_centers_file must contain sf geometry or lon/lat columns.")
    }
    x$lon <- as.numeric(x[[lon_col]])
    x$lat <- as.numeric(x[[lat_col]])
  }
  
  x$health_center_name <- if (!is.null(hc_name_col)) as.character(x[[hc_name_col]]) else paste("Health Center", seq_len(nrow(x)))
  x$district_name <- if (!is.null(district_col)) as.character(x[[district_col]]) else NA_character_
  x$region_name <- if (!is.null(region_col)) as.character(x[[region_col]]) else NA_character_
  x$zone_name <- if (!is.null(zone_col)) as.character(x[[zone_col]]) else NA_character_
  
  x <- x |>
    mutate(
      hc_id = if ("hc_id" %in% names(.)) as.character(hc_id) else paste0("hc_", seq_len(n())),
      correct_location = if ("correct_location" %in% names(.)) as.logical(correct_location) else TRUE,
      operational = if ("operational" %in% names(.)) as.logical(operational) else TRUE,
      coordination_site = if ("coordination_site" %in% names(.)) as.logical(coordination_site) else TRUE,
      is_new = if ("is_new" %in% names(.)) as.logical(is_new) else FALSE
    ) |>
    dplyr::select(
      hc_id, health_center_name, zone_name, region_name, district_name,
      lon, lat, correct_location, operational, coordination_site, is_new
    )
  
  x
}

health_centers_to_sf <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
}

make_placeholder_health_centers <- function(district_sf, n = 5, seed = 1) {
  set.seed(seed)
  pts <- st_sample(district_sf, size = n, exact = TRUE)
  pts_sf <- st_sf(
    hc_id = paste0("placeholder_", seq_len(n)),
    health_center_name = paste("Health Center", seq_len(n)),
    zone_name = district_sf$zone_name[1] %||% NA_character_,
    region_name = district_sf$region_name[1] %||% NA_character_,
    district_name = district_sf$district_name[1] %||% NA_character_,
    correct_location = TRUE,
    operational = TRUE,
    coordination_site = TRUE,
    is_new = FALSE,
    geometry = pts,
    crs = st_crs(district_sf)
  )
  pts_sf <- st_transform(pts_sf, 4326)
  coords <- st_coordinates(pts_sf)
  pts_sf$lon <- coords[, 1]
  pts_sf$lat <- coords[, 2]
  st_drop_geometry(pts_sf)
}

load_health_centers_file <- function(path) {
  p <- path.expand(path)
  if (!file.exists(p)) return(NULL)
  normalize_health_centers(readRDS(p))
}

get_district_health_centers <- function(hc_df, district_sf) {
  district_name <- as.character(district_sf$district_name[1])
  region_name <- as.character(district_sf$region_name[1])
  zone_name <- as.character(district_sf$zone_name[1])
  
  out <- NULL
  
  if (!is.null(hc_df) && nrow(hc_df) > 0) {
    out <- hc_df |>
      filter(
        (!is.na(district_name) & .data$district_name == district_name) |
          (is.na(.data$district_name) & !is.na(region_name) & .data$region_name == region_name) |
          (is.na(.data$district_name) & is.na(.data$region_name) & !is.na(zone_name) & .data$zone_name == zone_name)
      )
  }
  
  if (is.null(out) || nrow(out) == 0) {
    out <- make_placeholder_health_centers(
      district_sf = district_sf,
      n = 5,
      seed = max(1, sum(utf8ToInt(district_name)))
    )
  }
  
  hc_sf <- health_centers_to_sf(out)
  hc_sf <- st_transform(hc_sf, st_crs(district_sf))
  inside <- lengths(st_within(hc_sf, district_sf)) > 0
  out2 <- st_drop_geometry(hc_sf[inside, ])
  
  if (nrow(out2) == 0) {
    out2 <- make_placeholder_health_centers(
      district_sf = district_sf,
      n = 5,
      seed = max(1, sum(utf8ToInt(district_name)))
    )
  }
  
  out2
}

make_start_assignment_from_sites <- function(grid_sf, site_sf, district_sf) {
  stopifnot(!is.null(site_sf), nrow(site_sf) > 0)
  
  site_sf <- safe_make_valid(site_sf)
  site_sf <- st_transform(site_sf, st_crs(district_sf))
  site_sf <- suppressWarnings(st_intersection(site_sf, district_sf))
  
  if (nrow(site_sf) == 0) {
    stop("No coordination-site health centers fall inside the selected district.")
  }
  
  site_sf$area_name <- make.unique(as.character(site_sf$health_center_name))
  
  cent <- suppressWarnings(st_centroid(grid_sf))
  idx <- st_nearest_feature(cent, site_sf)
  
  list(
    assignments = as.character(site_sf$area_name[idx]),
    seeds_sf = site_sf
  )
}

make_fill_colors <- function(active_area, area_names) {
  out <- setNames(rep(nonselected_fill_color, length(area_names)), area_names)
  overlap_special <- intersect(names(special_fill_colors), names(out))
  out[overlap_special] <- special_fill_colors[overlap_special]
  if (!is.null(active_area) && active_area %in% names(out) && !active_area %in% special_area_names) {
    out[active_area] <- selected_fill_color
  }
  out
}

make_seed_points_df <- function(site_sf) {
  site_sf <- st_transform(site_sf, 4326)
  coords <- st_coordinates(site_sf)
  data.frame(
    area_name = site_sf$area_name,
    health_center_name = site_sf$health_center_name,
    lon = coords[, 1],
    lat = coords[, 2],
    stringsAsFactors = FALSE
  )
}

build_neighbors_and_edges <- function(grid_sf, district_sf) {
  touch_list <- st_touches(grid_sf)
  neighbors_list <- lapply(touch_list, as.integer)
  names(neighbors_list) <- as.character(grid_sf$cell_id)
  
  grid_sf_3857 <- st_transform(grid_sf, 3857)
  district_3857 <- st_transform(district_sf, 3857)
  cell_bbox <- st_bbox(grid_sf_3857[1, ])
  cell_w <- as.numeric(cell_bbox["xmax"] - cell_bbox["xmin"])
  cell_h <- as.numeric(cell_bbox["ymax"] - cell_bbox["ymin"])
  edge_buffer <- max(cell_w, cell_h) * 0.05
  district_boundary_3857 <- st_boundary(district_3857) |> st_buffer(edge_buffer)
  edge_flag <- lengths(st_intersects(grid_sf_3857, district_boundary_3857)) > 0
  edge_list <- as.list(edge_flag)
  names(edge_list) <- as.character(grid_sf$cell_id)
  
  list(
    neighbors_list = neighbors_list,
    edge_list = edge_list
  )
}

get_district_sf <- function(districts_shp, zone, region, district) {
  district_sf <- districts_shp |>
    filter(
      zone_name == zone,
      region_name == region,
      district_name == district
    )
  
  req(nrow(district_sf) >= 1)
  
  district_sf <- district_sf |>
    summarise(
      district_name = dplyr::first(district_name),
      region_name = dplyr::first(region_name),
      zone_name = dplyr::first(zone_name),
      geometry = st_union(geometry),
      .groups = "drop"
    ) |>
    st_as_sf()
  
  safe_make_valid(district_sf)
}

# =========================================================
# Load base data
# =========================================================

districts_path <- path.expand(districts_file)
if (!file.exists(districts_path)) {
  stop(
    paste0(
      "Could not find districts file:\n",
      districts_file,
      "\n\nCheck the path near the top of app.R."
    )
  )
}

districts_shp <- readRDS(districts_path)
districts_shp <- safe_make_valid(districts_shp)

required_cols <- c("zone_name", "region_name", "district_name")
missing_cols <- setdiff(required_cols, names(districts_shp))
if (length(missing_cols) > 0) {
  stop(
    paste0(
      "districts_shp is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  )
}

zone_choices <- sort(unique(as.character(stats::na.omit(districts_shp$zone_name))))
all_health_centers_raw <- tryCatch(load_health_centers_file(health_centers_file), error = function(e) NULL)
u5_worldpop_global <- tryCatch(load_worldpop_u5_raster(worldpop_t_u1_1to4_file), error = function(e) NULL)

# =========================================================
# UI helpers
# =========================================================

app_layout <- function(left_ui, map_ui, right_ui, loading_id = NULL) {
  map_children <- if (is.null(loading_id)) {
    map_ui
  } else {
    tagList(
      map_ui,
      div(id = loading_id, class = "loading_overlay", "Loading")
    )
  }
  
  div(
    class = "app_row",
    div(class = "leftbar", left_ui),
    div(class = "mapwrap", map_children),
    div(class = "rightbar", right_ui)
  )
}

# =========================================================
# UI
# =========================================================

ui <- navbarPage(
  title = "Microplanning Tool",
  id = "nav_page",
  collapsible = TRUE,
  
  header = tagList(
    tags$head(
      tags$style(HTML("
        html, body {
          height: 100%;
          margin: 0;
          padding: 0;
        }
        body > .container-fluid {
          padding-left: 6px;
          padding-right: 6px;
        }
        .tab-content {
          padding-top: 6px;
        }
        .app_row {
          display: flex;
          gap: 6px;
          height: calc(100vh - 115px);
        }
        .leftbar {
          flex: 2;
          min-width: 240px;
          max-width: 340px;
          background: #FAFAFA;
          border: 1px solid #E6E6E6;
          border-radius: 6px;
          padding: 8px;
          overflow-y: auto;
        }
        .mapwrap {
          flex: 8;
          min-width: 0;
          position: relative;
          height: 100%;
        }
        .rightbar {
          flex: 2;
          min-width: 240px;
          max-width: 340px;
          background: #FAFAFA;
          border: 1px solid #E6E6E6;
          border-radius: 6px;
          padding: 8px;
          overflow-y: auto;
        }
        .page_map {
          width: 100%;
          height: 100%;
          min-height: 700px;
          border: 1px solid #E6E6E6;
          border-radius: 6px;
          background: #D9D9D9;
        }
        .page_map .leaflet {
          width: 100%;
          height: 100%;
        }
        .loading_overlay {
          display: none;
          position: absolute;
          inset: 0;
          z-index: 900;
          background: rgba(255,255,255,0.55);
          align-items: center;
          justify-content: center;
          pointer-events: none;
          font-size: 18px;
          font-weight: 600;
          color: #444444;
        }
        .leaflet-container {
          background: #D9D9D9;
        }
        .mini-label {
          font-size: 11px;
          color: #666666;
          margin-bottom: 3px;
        }
        .section-gap {
          margin-top: 8px;
        }
        .slider-row {
          display: flex;
          align-items: center;
          gap: 4px;
          margin-bottom: 8px;
        }
        .slider-row .btn {
          width: 30px;
          min-width: 30px;
          padding: 2px 0;
          font-size: 12px;
        }
        .slider-wrap {
          flex: 1;
        }
        .slider-wrap .form-group {
          margin-bottom: 0;
        }
        .slider-wrap .irs-min,
        .slider-wrap .irs-max,
        .slider-wrap .irs-from,
        .slider-wrap .irs-to,
        .slider-wrap .irs-single,
        .slider-wrap .irs-grid-text,
        .slider-wrap .irs-grid-pol {
          display: none !important;
        }
        .dataTables_wrapper {
          font-size: 11px;
        }
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate,
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter {
          display: none;
        }
        .btn {
          padding: 3px 8px;
          font-size: 12px;
        }
        .shiny-input-container {
          margin-bottom: 6px;
        }
        .control-row {
          display: flex;
          gap: 6px;
          margin-top: 6px;
          margin-bottom: 6px;
        }
        .top-help {
          display: flex;
          justify-content: flex-end;
          margin-bottom: 4px;
        }
        .modal-body {
          font-size: 13px;
          line-height: 1.45;
        }
        .legend-box {
          display: inline-block;
          width: 12px;
          height: 12px;
          border: 1px solid #7F7F7F;
          margin-right: 6px;
          vertical-align: middle;
        }
        .legend-row {
          font-size: 11px;
          line-height: 1.35;
          margin-bottom: 3px;
        }
        .legend-wrap {
          margin-bottom: 8px;
          padding-bottom: 8px;
          border-bottom: 1px solid #E6E6E6;
        }
        .rightbar-title {
          font-size: 12px;
          font-weight: 600;
          margin-bottom: 6px;
          color: #333333;
        }
        .area-map-label {
          background: rgba(255,255,255,0.92);
          border: 1px solid #CCCCCC;
          border-radius: 3px;
          padding: 1px 4px;
          color: #000000;
          font-size: 10px;
          white-space: nowrap;
        }
        .leaflet-tooltip.ha-tooltip {
          background: transparent;
          border: none;
          box-shadow: none;
          padding: 0;
        }
        .leaflet-tooltip.ha-tooltip:before {
          display: none;
        }
        .note-box {
          font-size: 11px;
          color: #555555;
          background: #F7F7F7;
          border: 1px solid #E6E6E6;
          border-radius: 4px;
          padding: 6px;
          margin-bottom: 8px;
        }
        .intro-wrap {
          max-width: 1000px;
          margin: 0 auto;
          padding: 24px 12px 36px 12px;
          font-size: 15px;
          line-height: 1.6;
        }
        .intro-wrap h2, .intro-wrap h3 {
          margin-top: 0;
        }
        .intro-card {
          background: #FAFAFA;
          border: 1px solid #E6E6E6;
          border-radius: 8px;
          padding: 18px;
          margin-bottom: 14px;
        }
      ")),
      tags$script(HTML("
(function() {
  Shiny.addCustomMessageHandler('show_loading', function(msg) {
    var id = msg && msg.id ? msg.id : null;
    if (!id) return;
    var el = document.getElementById(id);
    if (el) el.style.display = 'flex';
  });

  Shiny.addCustomMessageHandler('hide_loading', function(msg) {
    var id = msg && msg.id ? msg.id : null;
    if (!id) return;
    var el = document.getElementById(id);
    if (el) el.style.display = 'none';
  });

  window.haApp = {
    map: null,
    districtLayer: null,
    popLayer: null,
    savedLayer: null,
    facilityLayer: null,
    labelLayer: null,
    brushPreview: null,
    cells: [],
    pendingMsg: null,
    isPainting: false,
    isRightPanning: false,
    handlersBound: false,
    activeDfa: null,
    assignments: {},
    initialAssignments: {},
    dfaColors: {},
    neighbors: {},
    edgeCells: {},
    brushSize: 300,
    boundaryOnly: false,
    baseLayers: {},
    currentBaseLayer: null,
    baseControl: null,

    currentBrushSize: function() {
      return window.haApp.brushSize || 300;
    },

    normalizeRowArray: function(x) {
      if (!x) return [];
      if (Array.isArray(x)) return x;
      if (typeof x === 'object') {
        return Object.keys(x).sort(function(a, b) { return Number(a) - Number(b); }).map(function(k) {
          return x[k];
        });
      }
      return [];
    },

    validLatLng: function(p) {
      if (!p) return false;
      var lat = Number(p.lat);
      var lon = Number(p.lon);
      return Number.isFinite(lat) && Number.isFinite(lon);
    },

    mapContainerReady: function() {
      var el = document.getElementById('ha_map');
      if (!el) return false;
      return el.offsetWidth > 0 && el.offsetHeight > 0;
    },

    ensureMap: function() {
      if (window.haApp.map) return window.haApp.map;
      if (typeof L === 'undefined') return null;
      if (!window.haApp.mapContainerReady()) return null;

      var map = L.map('ha_map', {
        zoomSnap: 0.25,
        preferCanvas: true,
        dragging: false,
        scrollWheelZoom: true
      });

      var osm = L.tileLayer(
        'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
        { maxZoom: 20, attribution: '&copy; OpenStreetMap contributors' }
      );
      var esriImagery = L.tileLayer(
        'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
        { maxZoom: 20, attribution: 'Tiles &copy; Esri' }
      );
      var cartoLight = L.tileLayer(
        'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',
        {
          maxZoom: 20,
          subdomains: 'abcd',
          attribution: '&copy; OpenStreetMap contributors &copy; CARTO'
        }
      );

      window.haApp.baseLayers = {
        'OpenStreetMap': osm,
        'ESRI Satellite': esriImagery,
        'CARTO Light': cartoLight
      };
      window.haApp.currentBaseLayer = osm;
      osm.addTo(map);
      window.haApp.baseControl = L.control.layers(window.haApp.baseLayers, null, {
        collapsed: true,
        position: 'topright'
      }).addTo(map);
      map.on('baselayerchange', function(e) {
        window.haApp.currentBaseLayer = e.layer;
      });

      var container = map.getContainer();
      container.addEventListener('contextmenu', function(e) {
        e.preventDefault();
      });

      window.haApp.map = map;
      window.haApp.bindHandlers();
      return map;
    },

    bindHandlers: function() {
      var app = window.haApp;
      var map = app.map;
      if (!map || app.handlersBound) return;
      app.handlersBound = true;

      var container = map.getContainer();

      map.on('mousemove', function(e) {
        if (app.brushPreview) {
          app.brushPreview.setLatLng(e.latlng);
          app.brushPreview.setRadius(app.currentBrushSize());
        }
        if (app.isPainting) {
          app.paintAt(e.latlng);
        }
      });

      map.on('mousedown', function(e) {
        if (e.originalEvent && e.originalEvent.button === 0) {
          app.paintAt(e.latlng);
        }
      });

      container.addEventListener('mousedown', function(e) {
        if (e.button === 0) {
          app.isPainting = true;
          app.isRightPanning = false;
        } else if (e.button === 2) {
          app.isPainting = false;
          app.isRightPanning = true;
          if (!map.dragging.enabled()) map.dragging.enable();
        }
      });

      container.addEventListener('mouseup', function(e) {
        if (e.button === 0) {
          app.isPainting = false;
        } else if (e.button === 2) {
          app.isRightPanning = false;
          if (map.dragging.enabled()) map.dragging.disable();
        }
      });

      container.addEventListener('mouseleave', function() {
        app.isPainting = false;
        app.isRightPanning = false;
        if (map.dragging.enabled()) map.dragging.disable();
      });

      document.addEventListener('mouseup', function() {
        app.isPainting = false;
        app.isRightPanning = false;
        if (app.map && app.map.dragging.enabled()) app.map.dragging.disable();
      });
    },

    invalidate: function() {
      if (window.haApp.map) {
        setTimeout(function() {
          if (window.haApp.map) window.haApp.map.invalidateSize();
        }, 50);
      }
    },

    fillForDfa: function(dfa) {
      return window.haApp.dfaColors[dfa] || '#757575';
    },

    borderColorForDfa: function(dfa) {
      if (dfa === 'Inaccessible') return '#D7301F';
      if (dfa === 'Unpopulated') return '#FFFFFF';
      if (dfa === window.haApp.activeDfa) return '#FFD400';
      return '#000000';
    },

    isBoundaryCellById: function(id) {
      var app = window.haApp;
      var myDfa = app.assignments[id];
      var nbrs = app.neighbors[id] || [];
      if (app.edgeCells[id]) return true;
      for (var i = 0; i < nbrs.length; i++) {
        var nbrId = String(nbrs[i]);
        if (app.assignments[nbrId] !== myDfa) return true;
      }
      return false;
    },

    styleForCell: function(cell) {
      var app = window.haApp;
      var id = String(cell._cellId);
      var dfa = app.assignments[id];
      var isBoundary = app.isBoundaryCellById(id);
      var isSelectedBoundary = isBoundary && dfa === app.activeDfa;
      var opacity = app.boundaryOnly ? (isSelectedBoundary ? 0.9 : 0.0) : 0.72;
      return {
        color: app.borderColorForDfa(dfa),
        weight: isBoundary ? (isSelectedBoundary ? 0.8 : 0.35) : 0.15,
        opacity: isBoundary ? 1.0 : 0.35,
        fillColor: app.fillForDfa(dfa),
        fillOpacity: opacity,
        smoothFactor: 0,
        interactive: false
      };
    },

    refreshAllStyles: function() {
      window.haApp.cells.forEach(function(cell) {
        cell.setStyle(window.haApp.styleForCell(cell));
      });
    },

    paintAt: function(latlng) {
      var app = window.haApp;
      var map = app.map;
      if (!map || !app.cells.length || !app.activeDfa) return;

      var brushMeters = app.currentBrushSize();
      app.cells.forEach(function(cell) {
        var d = map.distance(latlng, L.latLng(cell._clat, cell._clng));
        if (d <= brushMeters) {
          app.assignments[String(cell._cellId)] = app.activeDfa;
        }
      });
      app.refreshAllStyles();
    },

    clearScene: function() {
      var app = window.haApp;
      if (!app.map) return;
      ['districtLayer', 'popLayer', 'savedLayer', 'facilityLayer', 'labelLayer'].forEach(function(nm) {
        if (app[nm]) {
          app.map.removeLayer(app[nm]);
          app[nm] = null;
        }
      });
      app.cells.forEach(function(cell) {
        app.map.removeLayer(cell);
      });
      app.cells = [];
      if (app.brushPreview) {
        app.map.removeLayer(app.brushPreview);
        app.brushPreview = null;
      }
    },

    renderScene: function(msg) {
      var app = window.haApp;
      var map = app.ensureMap();
      if (!map) return false;

      app.clearScene();
      app.pendingMsg = null;

      app.assignments = msg.currentAssignments || {};
      app.initialAssignments = msg.initialAssignments || {};
      app.dfaColors = msg.dfaColors || {};
      app.activeDfa = msg.activeDfa || null;
      app.neighbors = msg.neighbors || {};
      app.edgeCells = msg.edgeCells || {};
      app.brushSize = msg.brushSize || 300;
      app.boundaryOnly = !!msg.boundaryOnly;

      if (msg.districtGeojson) {
        app.districtLayer = L.geoJSON(JSON.parse(msg.districtGeojson), {
          style: function() {
            return { color: '#333333', weight: 1.5, opacity: 1, fillOpacity: 0 };
          }
        }).addTo(map);
      }

      if (msg.popGeojson) {
        app.popLayer = L.geoJSON(JSON.parse(msg.popGeojson), {
          style: function(feature) {
            return {
              stroke: false,
              fillColor: feature.properties.fill_color || '#000000',
              fillOpacity: 0.35
            };
          },
          interactive: false
        });
        if (msg.showPop) app.popLayer.addTo(map);
      }

      if (msg.gridGeojson) {
        var gj = JSON.parse(msg.gridGeojson);
        gj.features.forEach(function(feature) {
          var geom = feature.geometry;
          if (!geom || geom.type !== 'Polygon' || !geom.coordinates || !geom.coordinates.length) return;
          var coords = geom.coordinates[0].map(function(x) { return [x[1], x[0]]; });
          var cellId = String(feature.properties.cell_id);
          var poly = L.polygon(coords, {
            smoothFactor: 0,
            interactive: false
          }).addTo(map);
          poly._cellId = cellId;
          poly._clng = Number(feature.properties.centroid_lon);
          poly._clat = Number(feature.properties.centroid_lat);
          app.cells.push(poly);
        });
        app.refreshAllStyles();
      }

      if (msg.savedGeojson) {
        app.savedLayer = L.geoJSON(JSON.parse(msg.savedGeojson), {
          style: function(feature) {
            var nm = (feature.properties || {}).area_name;
            return {
              color: '#222222',
              weight: 1,
              opacity: 0.9,
              fillOpacity: 0,
              fillColor: app.fillForDfa(nm)
            };
          },
          interactive: false
        }).addTo(map);
      }

      var facilityPoints = app.normalizeRowArray(msg.facilityPoints);
      if (facilityPoints.length) {
        var facMarkers = [];
        facilityPoints.forEach(function(p) {
          if (!app.validLatLng(p)) return;
          var m = L.circleMarker([Number(p.lat), Number(p.lon)], {
            radius: 4,
            color: 'black',
            weight: 1,
            fillColor: '#FFFFFF',
            fillOpacity: 1
          });
          m.bindTooltip(p.health_center_name || p.area_name || '', {
            permanent: true,
            direction: 'right',
            offset: [8, 0],
            opacity: 1
          });
          facMarkers.push(m);
        });
        app.facilityLayer = L.layerGroup(facMarkers).addTo(map);
      }

      var labelPoints = app.normalizeRowArray(msg.labelPoints);
      if (labelPoints.length) {
        var labelMarkers = [];
        labelPoints.forEach(function(p) {
          if (!app.validLatLng(p)) return;
          var m = L.circleMarker([Number(p.lat), Number(p.lon)], {
            radius: 2,
            color: '#000000',
            weight: 1,
            fillColor: '#000000',
            fillOpacity: 1,
            interactive: false
          });
          m.bindTooltip('<div class=\"area-map-label\">' + (p.area_name || '') + '</div>', {
            permanent: true,
            direction: 'right',
            className: 'ha-tooltip',
            offset: [4, 0]
          });
          labelMarkers.push(m);
        });
        app.labelLayer = L.layerGroup(labelMarkers).addTo(map);
      }

      app.brushPreview = L.circle(map.getCenter(), {
        radius: app.currentBrushSize(),
        color: '#111111',
        weight: 1,
        opacity: 0.7,
        fillOpacity: 0.05,
        interactive: false
      }).addTo(map);

      if (app.districtLayer) {
        var b = app.districtLayer.getBounds();
        if (b && b.isValid()) map.fitBounds(b, { padding: [10, 10] });
      }

      app.invalidate();
      return true;
    },

    maybeRenderPending: function() {
      var app = window.haApp;
      if (!app.pendingMsg) return;
      if (!app.mapContainerReady()) return;
      app.renderScene(app.pendingMsg);
    },

    loadScene: function(msg) {
      var app = window.haApp;
      app.pendingMsg = msg;
      var tries = 0;
      function tryRender() {
        if (!app.pendingMsg) return;
        if (app.renderScene(app.pendingMsg)) return;
        tries += 1;
        if (tries < 40) {
          setTimeout(tryRender, 100);
        }
      }
      tryRender();
    },

    setPopulationVisibility: function(showIt) {
      var app = window.haApp;
      if (!app.map || !app.popLayer) return;
      if (showIt) {
        if (!app.map.hasLayer(app.popLayer)) app.popLayer.addTo(app.map);
      } else {
        if (app.map.hasLayer(app.popLayer)) app.map.removeLayer(app.popLayer);
      }
    },

    setBrushSize: function(v) {
      window.haApp.brushSize = v;
      if (window.haApp.brushPreview) window.haApp.brushPreview.setRadius(v);
    },

    setBoundaryOnly: function(v) {
      window.haApp.boundaryOnly = !!v;
      window.haApp.refreshAllStyles();
    },

    setColorsAndActive: function(colorsObj, activeDfa) {
      window.haApp.dfaColors = colorsObj || {};
      window.haApp.activeDfa = activeDfa || null;
      window.haApp.refreshAllStyles();
    },

    requestAssignments: function() {
      Shiny.setInputValue('ha_assignments', {
        assignments: window.haApp.assignments,
        nonce: Date.now()
      }, { priority: 'event' });
    },

    resetAssignments: function() {
      window.haApp.assignments = Object.assign({}, window.haApp.initialAssignments);
      window.haApp.refreshAllStyles();
      Shiny.setInputValue('ha_assignments', {
        assignments: window.haApp.assignments,
        nonce: Date.now()
      }, { priority: 'event' });
    }
  };

  $(document).on('shown.bs.tab', 'a[data-toggle=tab], a[data-bs-toggle=tab], button[data-bs-toggle=tab]', function() {
    if (window.haApp) {
      window.haApp.maybeRenderPending();
      window.haApp.invalidate();
    }
  });

  $(window).on('resize', function() {
    if (window.haApp) {
      window.haApp.maybeRenderPending();
      window.haApp.invalidate();
    }
  });

  Shiny.addCustomMessageHandler('ha_load_scene', function(msg) {
    window.haApp.loadScene(msg);
  });

  Shiny.addCustomMessageHandler('ha_toggle_population', function(msg) {
    window.haApp.setPopulationVisibility(!!msg.show);
  });

  Shiny.addCustomMessageHandler('ha_set_brush', function(msg) {
    window.haApp.setBrushSize(msg.value);
  });

  Shiny.addCustomMessageHandler('ha_set_boundary_only', function(msg) {
    window.haApp.setBoundaryOnly(msg.value);
  });

  Shiny.addCustomMessageHandler('ha_set_colors', function(msg) {
    window.haApp.setColorsAndActive(msg.colors, msg.activeDfa);
  });

  Shiny.addCustomMessageHandler('ha_request_assignments', function(msg) {
    window.haApp.requestAssignments();
  });

  Shiny.addCustomMessageHandler('ha_reset', function(msg) {
    window.haApp.resetAssignments();
  });

  Shiny.addCustomMessageHandler('ha_render_now', function(msg) {
    if (window.haApp) {
      window.haApp.maybeRenderPending();
      window.haApp.invalidate();
    }
  });
})();
      "))
    )
  ),
  
  div(
    id = "leaflet_loader_wrap",
    style = "position:absolute; left:-9999px; top:-9999px; width:1px; height:1px; overflow:hidden;",
    leafletOutput("leaflet_loader", width = "1px", height = "1px")
  ),
  
  tabPanel(
    "Introduction",
    div(
      class = "intro-wrap",
      div(
        class = "intro-card",
        h2("Health Area Boundary Review for Microplanning"),
        p("This tool supports a two-step process for preparing health area boundaries for campaign microplanning."),
        actionButton("intro_help_btn", "Help / Instructions")
      ),
      div(
        class = "intro-card",
        h3("Step 1. Health Center Locations"),
        p("Review the health center points for a district. For each health center, confirm whether the location is correct, whether it is operational, and whether it will serve as a coordination site for campaigns. Users can drag points to update locations and add missing health centers.")
      ),
      div(
        class = "intro-card",
        h3("Step 2. Health Area Mapping"),
        p("Once Step 1 is complete, initial health areas are generated automatically from the health centers marked as campaign coordination sites. District teams then review and revise those health area boundaries manually.")
      ),
      div(
        class = "intro-card",
        h3("Population estimates"),
        p("WorldPop U5 population estimates are used as starting target population estimates for each health area.")
      )
    )
  ),
  
  tabPanel(
    "(1) Health Center Locations",
    app_layout(
      left_ui = tagList(
        div(class = "top-help", actionButton("hc_help_btn", "Help")),
        selectInput("zone_hc", "Zone", choices = zone_choices),
        uiOutput("region_ui_hc"),
        uiOutput("district_ui_hc"),
        div(class = "section-gap"),
        checkboxInput("show_pop_hc", "Show population layer", value = show_pop_default),
        div(class = "control-row",
            actionButton("add_hc_btn", "Add health center"),
            actionButton("generate_ha_btn", "Generate starting health areas")),
        div(class = "section-gap"),
        div(class = "rightbar-title", "Selected health center"),
        uiOutput("selected_hc_ui"),
        div(class = "control-row",
            actionButton("save_hc_edits_btn", "Apply changes"),
            downloadButton("download_hc_btn", "Download table")),
        div(class = "section-gap note-box",
            "Click 'Add health center' and then click the map to place a new point. Existing points can be dragged directly on the map.")
      ),
      map_ui = div(
        class = "page_map",
        leafletOutput("hc_map", width = "100%", height = "100%")
      ),
      right_ui = tagList(
        div(class = "rightbar-title", "Health centers"),
        DTOutput("hc_table")
      )
    )
  ),
  
  tabPanel(
    "(2) Health Area Mapping",
    app_layout(
      left_ui = tagList(
        div(class = "top-help", actionButton("ha_help_btn", "Help")),
        selectInput("zone_ha", "Zone", choices = zone_choices),
        uiOutput("region_ui_ha"),
        uiOutput("district_ui_ha"),
        div(class = "section-gap"),
        checkboxInput("show_pop_ha", "Show population layer", value = show_pop_default),
        checkboxInput("boundary_only_ha", "Show only active boundary", value = boundary_only_default),
        uiOutput("active_area_ui_ha"),
        div(class = "mini-label", "Brush size"),
        div(
          class = "slider-row",
          actionButton("brush_minus_ha", "-", width = "30px"),
          div(class = "slider-wrap", uiOutput("brush_slider_ui_ha")),
          actionButton("brush_plus_ha", "+", width = "30px")
        ),
        div(class = "control-row",
            actionButton("save_ha_btn", "Save"),
            actionButton("reset_ha_btn", "Reset")),
        div(class = "control-row",
            downloadButton("download_ha_geojson_btn", "GeoJSON"),
            downloadButton("download_ha_rds_btn", "RDS")),
        div(class = "section-gap note-box", textOutput("ha_status_text"))
      ),
      map_ui = div(id = "ha_map", class = "page_map"),
      right_ui = tagList(
        div(class = "rightbar-title", "Legend"),
        uiOutput("legend_ui_ha"),
        div(class = "rightbar-title", "Estimated U5 population"),
        DTOutput("pop_table_ha")
      ),
      loading_id = "loading_overlay_ha"
    )
  )
)

# =========================================================
# Server
# =========================================================

server <- function(input, output, session) {
  rv <- reactiveValues(
    hc_by_key = list(),
    ha_by_key = list(),
    hc_selected_id = NULL,
    syncing = FALSE,
    hc_add_mode = FALSE,
    pending_hc_click = NULL
  )
  
  pending_action <- reactiveVal(NULL)
  
  # -------------------------------------------------------
  # Shared selector syncing
  # -------------------------------------------------------
  
  observeEvent(input$zone_hc, {
    if (isTRUE(rv$syncing)) return()
    rv$syncing <- TRUE
    updateSelectInput(session, "zone_ha", selected = input$zone_hc)
    rv$syncing <- FALSE
  }, ignoreInit = TRUE)
  
  observeEvent(input$zone_ha, {
    if (isTRUE(rv$syncing)) return()
    rv$syncing <- TRUE
    updateSelectInput(session, "zone_hc", selected = input$zone_ha)
    rv$syncing <- FALSE
  }, ignoreInit = TRUE)
  
  output$region_ui_hc <- renderUI({
    req(input$zone_hc)
    regions <- districts_shp |>
      st_drop_geometry() |>
      filter(zone_name == input$zone_hc) |>
      pull(region_name) |>
      unique() |>
      sort()
    selectInput("region_hc", "Region", choices = regions)
  })
  
  output$region_ui_ha <- renderUI({
    req(input$zone_ha)
    regions <- districts_shp |>
      st_drop_geometry() |>
      filter(zone_name == input$zone_ha) |>
      pull(region_name) |>
      unique() |>
      sort()
    selectInput("region_ha", "Region", choices = regions)
  })
  
  observeEvent(input$region_hc, {
    if (isTRUE(rv$syncing)) return()
    rv$syncing <- TRUE
    updateSelectInput(session, "region_ha", selected = input$region_hc)
    rv$syncing <- FALSE
  }, ignoreInit = TRUE)
  
  observeEvent(input$region_ha, {
    if (isTRUE(rv$syncing)) return()
    rv$syncing <- TRUE
    updateSelectInput(session, "region_hc", selected = input$region_ha)
    rv$syncing <- FALSE
  }, ignoreInit = TRUE)
  
  output$district_ui_hc <- renderUI({
    req(input$zone_hc, input$region_hc)
    districts <- districts_shp |>
      st_drop_geometry() |>
      filter(zone_name == input$zone_hc, region_name == input$region_hc) |>
      pull(district_name) |>
      unique() |>
      sort()
    selectInput("district_hc", "District", choices = districts)
  })
  
  output$district_ui_ha <- renderUI({
    req(input$zone_ha, input$region_ha)
    districts <- districts_shp |>
      st_drop_geometry() |>
      filter(zone_name == input$zone_ha, region_name == input$region_ha) |>
      pull(district_name) |>
      unique() |>
      sort()
    selectInput("district_ha", "District", choices = districts)
  })
  
  observeEvent(input$district_hc, {
    if (isTRUE(rv$syncing)) return()
    rv$syncing <- TRUE
    updateSelectInput(session, "district_ha", selected = input$district_hc)
    rv$syncing <- FALSE
  }, ignoreInit = TRUE)
  
  observeEvent(input$district_ha, {
    if (isTRUE(rv$syncing)) return()
    rv$syncing <- TRUE
    updateSelectInput(session, "district_hc", selected = input$district_ha)
    rv$syncing <- FALSE
  }, ignoreInit = TRUE)
  
  # -------------------------------------------------------
  # District reactives
  # -------------------------------------------------------
  
  district_info_hc <- reactive({
    req(input$zone_hc, input$region_hc, input$district_hc)
    district_sf <- get_district_sf(districts_shp, input$zone_hc, input$region_hc, input$district_hc)
    max_dim_m <- calc_district_max_dim(district_sf)
    list(
      key = district_key(input$zone_hc, input$region_hc, input$district_hc),
      district_sf = district_sf,
      grid_limits = calc_grid_limits(max_dim_m),
      brush_limits = calc_brush_limits(max_dim_m)
    )
  })
  
  district_info_ha <- reactive({
    req(input$zone_ha, input$region_ha, input$district_ha)
    district_sf <- get_district_sf(districts_shp, input$zone_ha, input$region_ha, input$district_ha)
    max_dim_m <- calc_district_max_dim(district_sf)
    list(
      key = district_key(input$zone_ha, input$region_ha, input$district_ha),
      district_sf = district_sf,
      grid_limits = calc_grid_limits(max_dim_m),
      brush_limits = calc_brush_limits(max_dim_m)
    )
  })
  
  # -------------------------------------------------------
  # Step 1 state
  # -------------------------------------------------------
  
  observeEvent(district_info_hc(), {
    info <- district_info_hc()
    key <- info$key
    
    if (is.null(rv$hc_by_key[[key]])) {
      rv$hc_by_key[[key]] <- get_district_health_centers(all_health_centers_raw, info$district_sf)
    }
    
    this_df <- rv$hc_by_key[[key]]
    rv$hc_selected_id <- if (!is.null(this_df) && nrow(this_df) > 0) this_df$hc_id[1] else NULL
    rv$hc_add_mode <- FALSE
  }, ignoreInit = FALSE)
  
  current_hc_df <- reactive({
    req(district_info_hc())
    rv$hc_by_key[[district_info_hc()$key]]
  })
  
  selected_hc_row <- reactive({
    df <- current_hc_df()
    req(!is.null(df), nrow(df) > 0, !is.null(rv$hc_selected_id))
    idx <- match(rv$hc_selected_id, df$hc_id)
    req(!is.na(idx))
    df[idx, , drop = FALSE]
  })
  
  output$selected_hc_ui <- renderUI({
    df <- current_hc_df()
    if (is.null(df) || nrow(df) == 0) {
      return(div("No health centers available."))
    }
    
    row <- selected_hc_row()
    
    tagList(
      textInput("hc_name_edit", "Name", value = row$health_center_name[1]),
      checkboxInput("hc_correct_edit", "Correct location", value = isTRUE(row$correct_location[1])),
      checkboxInput("hc_operational_edit", "Operational", value = isTRUE(row$operational[1])),
      checkboxInput("hc_coordination_edit", "Coordination site for campaigns", value = isTRUE(row$coordination_site[1])),
      div(class = "mini-label", paste0("Lon: ", round(row$lon[1], 6), " | Lat: ", round(row$lat[1], 6)))
    )
  })
  
  observeEvent(input$save_hc_edits_btn, {
    req(district_info_hc())
    key <- district_info_hc()$key
    df <- rv$hc_by_key[[key]]
    req(!is.null(df), !is.null(rv$hc_selected_id))
    idx <- match(rv$hc_selected_id, df$hc_id)
    req(!is.na(idx))
    
    df$health_center_name[idx] <- input$hc_name_edit
    df$correct_location[idx] <- isTRUE(input$hc_correct_edit)
    df$operational[idx] <- isTRUE(input$hc_operational_edit)
    df$coordination_site[idx] <- isTRUE(input$hc_coordination_edit)
    
    rv$hc_by_key[[key]] <- df
    showNotification("Health center updated.", type = "message", duration = 3)
  })
  
  observeEvent(input$add_hc_btn, {
    rv$hc_add_mode <- TRUE
    showNotification("Click on the map to add a new health center.", type = "message", duration = 4)
  })
  
  observeEvent(input$hc_map_click, {
    req(isTRUE(rv$hc_add_mode))
    rv$hc_add_mode <- FALSE
    
    rv$pending_hc_click <- list(
      lon = input$hc_map_click$lng,
      lat = input$hc_map_click$lat
    )
    
    showModal(
      modalDialog(
        title = "Add health center",
        textInput("new_hc_name", "Health center name"),
        checkboxInput("new_hc_correct", "Correct location", value = TRUE),
        checkboxInput("new_hc_operational", "Operational", value = TRUE),
        checkboxInput("new_hc_coordination", "Coordination site for campaigns", value = TRUE),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_add_hc", "Add")
        )
      )
    )
  })
  
  observeEvent(input$confirm_add_hc, {
    req(rv$pending_hc_click, district_info_hc())
    req(nzchar(input$new_hc_name))
    
    key <- district_info_hc()$key
    df <- rv$hc_by_key[[key]]
    if (is.null(df)) df <- data.frame()
    
    new_row <- data.frame(
      hc_id = paste0("new_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
      health_center_name = input$new_hc_name,
      zone_name = input$zone_hc,
      region_name = input$region_hc,
      district_name = input$district_hc,
      lon = rv$pending_hc_click$lon,
      lat = rv$pending_hc_click$lat,
      correct_location = isTRUE(input$new_hc_correct),
      operational = isTRUE(input$new_hc_operational),
      coordination_site = isTRUE(input$new_hc_coordination),
      is_new = TRUE,
      stringsAsFactors = FALSE
    )
    
    rv$hc_by_key[[key]] <- bind_rows(df, new_row)
    rv$hc_selected_id <- new_row$hc_id[1]
    rv$pending_hc_click <- NULL
    removeModal()
    
    showNotification("Health center added.", type = "message", duration = 3)
  })
  
  observeEvent(input$hc_map_marker_dragend, {
    req(district_info_hc())
    key <- district_info_hc()$key
    df <- rv$hc_by_key[[key]]
    req(!is.null(df))
    
    drag <- input$hc_map_marker_dragend
    idx <- match(as.character(drag$id), df$hc_id)
    req(!is.na(idx))
    
    df$lon[idx] <- as.numeric(drag$lng)
    df$lat[idx] <- as.numeric(drag$lat)
    df$correct_location[idx] <- TRUE
    rv$hc_by_key[[key]] <- df
    rv$hc_selected_id <- df$hc_id[idx]
  })
  
  observeEvent(input$hc_map_marker_click, {
    rv$hc_selected_id <- input$hc_map_marker_click$id
  })
  
  hc_pop_overlay <- reactive({
    req(district_info_hc())
    if (!isTRUE(input$show_pop_hc)) return(NULL)
    make_population_overlay_sf(district_info_hc()$district_sf, u5_worldpop_global)
  })
  
  output$leaflet_loader <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 45.30, lat = 2.09, zoom = 11)
  })
  
  
  observeEvent(input$nav_page, {
    if (identical(input$nav_page, '(2) Health Area Mapping')) {
      session$sendCustomMessage('ha_render_now', list())
    }
  }, ignoreInit = FALSE)
  
  output$hc_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.25)) |>
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite") |>
      addProviderTiles(providers$CartoDB.Positron, group = "CARTO Light") |>
      addLayersControl(
        baseGroups = c("OpenStreetMap", "ESRI Satellite", "CARTO Light"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  observe({
    req(district_info_hc())
    df <- current_hc_df()
    req(!is.null(df))
    
    district_sf <- st_transform(district_info_hc()$district_sf, 4326)
    hc_sf <- health_centers_to_sf(df)
    pop_sf <- hc_pop_overlay()
    
    proxy <- leafletProxy("hc_map")
    proxy |> clearShapes() |> clearMarkers() |> clearGroup("population")
    
    proxy |> addPolygons(
      data = district_sf,
      color = "#333333",
      weight = 1.5,
      opacity = 1,
      fillOpacity = 0
    )
    
    if (!is.null(pop_sf)) {
      proxy |> addPolygons(
        data = pop_sf,
        fillColor = ~fill_color,
        fillOpacity = 0.35,
        stroke = FALSE,
        group = "population"
      )
    }
    
    if (!is.null(hc_sf) && nrow(hc_sf) > 0) {
      coords <- st_coordinates(st_transform(hc_sf, 4326))
      proxy |> addMarkers(
        lng = coords[, 1],
        lat = coords[, 2],
        layerId = hc_sf$hc_id,
        icon = make_hf_icon(),
        label = hc_sf$health_center_name,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "right",
          offset = c(8, 0),
          textOnly = FALSE,
          style = list(
            "font-size" = "11px",
            "font-weight" = "bold",
            "color" = "#000000",
            "background-color" = "rgba(255,255,255,0.8)",
            "padding" = "2px 4px",
            "border-radius" = "3px"
          )
        ),
        options = markerOptions(draggable = TRUE)
      )
    }
    
    bb <- st_bbox(district_sf)
    proxy |> fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
  })
  
  output$hc_table <- renderDT({
    df <- current_hc_df()
    req(!is.null(df), nrow(df) > 0)
    
    display_df <- df |>
      mutate(
        selected = ifelse(hc_id == rv$hc_selected_id, "●", ""),
        lon = round(lon, 6),
        lat = round(lat, 6)
      ) |>
      dplyr::select(
        selected,
        health_center_name,
        correct_location,
        operational,
        coordination_site,
        lon,
        lat
      )
    
    datatable(
      display_df,
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 14, scrollX = TRUE, dom = "tip")
    )
  })
  
  observeEvent(input$hc_table_rows_selected, {
    df <- current_hc_df()
    req(!is.null(df))
    idx <- input$hc_table_rows_selected
    if (length(idx) == 1 && idx >= 1 && idx <= nrow(df)) {
      rv$hc_selected_id <- df$hc_id[idx]
    }
  })
  
  output$download_hc_btn <- downloadHandler(
    filename = function() {
      paste0("verified_health_centers_", gsub("[^A-Za-z0-9]+", "_", input$district_hc), ".csv")
    },
    content = function(file) {
      write.csv(current_hc_df(), file, row.names = FALSE, na = "")
    }
  )
  
  # -------------------------------------------------------
  # Generate Step 2 state from Step 1
  # -------------------------------------------------------
  
  observeEvent(input$generate_ha_btn, {
    req(district_info_hc())
    key <- district_info_hc()$key
    district_sf <- district_info_hc()$district_sf
    hc_df <- rv$hc_by_key[[key]]
    req(!is.null(hc_df))
    
    sites_df <- hc_df |>
      filter(
        operational %in% TRUE,
        coordination_site %in% TRUE
      )
    
    if (nrow(sites_df) == 0) {
      showNotification(
        "No health centers are currently marked as both operational and coordination sites.",
        type = "error",
        duration = 5
      )
      return()
    }
    
    session$sendCustomMessage("show_loading", list(id = "loading_overlay_ha"))
    
    sites_sf <- health_centers_to_sf(sites_df)
    
    if (is.null(sites_sf) || nrow(sites_sf) == 0) {
      session$sendCustomMessage("hide_loading", list(id = "loading_overlay_ha"))
      showNotification(
        "Could not create coordination-site points for this district.",
        type = "error",
        duration = 5
      )
      return()
    }
    
    grid_info <- make_paint_grid(district_sf, grid_n = default_grid_n)
    
    start_info <- make_start_assignment_from_sites(
      grid_sf = grid_info$grid_sf,
      site_sf = sites_sf,
      district_sf = district_sf
    )
    
    ng <- build_neighbors_and_edges(grid_info$grid_sf, district_sf)
    
    saved_area_sf <- build_polygons_from_assignments(
      grid_sf = grid_info$grid_sf,
      assignments = start_info$assignments,
      district_sf = district_sf
    )
    
    pop_table <- estimate_u5_population(
      saved_area_sf,
      u5_worldpop_global,
      name_col = "area_name"
    )
    
    facility_points_df <- sites_df |>
      dplyr::select(health_center_name, lon, lat)
    
    rv$ha_by_key[[key]] <- list(
      district_sf = district_sf,
      grid_sf = grid_info$grid_sf,
      verified_sites_sf = start_info$seeds_sf,
      initial_assignments = start_info$assignments,
      current_assignments = start_info$assignments,
      saved_area_sf = saved_area_sf,
      neighbors = ng$neighbors_list,
      edges = ng$edge_list,
      seed_points = make_seed_points_df(start_info$seeds_sf),
      facility_points = facility_points_df,
      pop_table = pop_table,
      grid_limits = district_info_hc()$grid_limits,
      brush_limits = district_info_hc()$brush_limits
    )
    
    session$sendCustomMessage("hide_loading", list(id = "loading_overlay_ha"))
    
    updateNavbarPage(session, "nav_page", selected = "(2) Health Area Mapping")
    showNotification(
      "Starting health areas generated from verified coordination-site health centers.",
      type = "message",
      duration = 5
    )
  })
  
  # -------------------------------------------------------
  # Step 2 state
  # -------------------------------------------------------
  
  current_ha_state <- reactive({
    req(district_info_ha())
    rv$ha_by_key[[district_info_ha()$key]]
  })
  
  output$brush_slider_ui_ha <- renderUI({
    info <- district_info_ha()
    sliderInput(
      "brush_m_ha",
      NULL,
      min = info$brush_limits$min,
      max = info$brush_limits$max,
      value = info$brush_limits$value,
      step = info$brush_limits$step,
      width = "100%"
    )
  })
  
  output$ha_status_text <- renderText({
    st <- current_ha_state()
    if (is.null(st)) {
      "No generated starting health areas are available for this district yet. First complete Step 1 and click Generate starting health areas."
    } else {
      paste0(
        "Starting health areas available for ", input$district_ha,
        ". Use the paint tool to review and revise boundaries."
      )
    }
  })
  
  output$active_area_ui_ha <- renderUI({
    st <- current_ha_state()
    if (is.null(st)) {
      return(div(class = "mini-label", "No areas available yet"))
    }
    
    area_names <- unique(c(st$verified_sites_sf$area_name, special_area_names))
    selectInput("active_area_ha", "Health Area", choices = area_names, selected = area_names[1])
  })
  
  current_fill_colors_ha <- reactive({
    st <- current_ha_state()
    req(!is.null(st))
    area_names <- unique(c(st$verified_sites_sf$area_name, special_area_names))
    make_fill_colors(input$active_area_ha, area_names)
  })
  
  output$legend_ui_ha <- renderUI({
    st <- current_ha_state()
    if (is.null(st)) return(div(class = "mini-label", "No legend available yet"))
    
    active_name <- input$active_area_ha %||% st$verified_sites_sf$area_name[1]
    
    div(
      class = "legend-wrap",
      div(
        class = "legend-row",
        HTML(paste0(
          "<span class='legend-box' style='background:", selected_fill_color, ";'></span>",
          htmlEscape(active_name)
        ))
      ),
      div(
        class = "legend-row",
        HTML(paste0(
          "<span class='legend-box' style='background:", nonselected_fill_color, ";'></span>",
          "Other health areas"
        ))
      ),
      div(
        class = "legend-row",
        HTML(paste0(
          "<span class='legend-box' style='background:", special_fill_colors["Inaccessible"], ";'></span>",
          "Inaccessible"
        ))
      ),
      div(
        class = "legend-row",
        HTML(paste0(
          "<span class='legend-box' style='background:", special_fill_colors["Unpopulated"], ";'></span>",
          "Unpopulated"
        ))
      )
    )
  })
  
  output$pop_table_ha <- renderDT({
    st <- current_ha_state()
    req(!is.null(st), !is.null(st$pop_table))
    datatable(
      st$pop_table,
      rownames = FALSE,
      options = list(pageLength = 14, scrollX = TRUE, dom = "tip")
    )
  })
  
  ha_pop_overlay <- reactive({
    req(district_info_ha())
    if (!isTRUE(input$show_pop_ha)) return(NULL)
    make_population_overlay_sf(district_info_ha()$district_sf, u5_worldpop_global)
  })
  
  observe({
    st <- current_ha_state()
    if (is.null(st)) return()
    
    session$sendCustomMessage("show_loading", list(id = "loading_overlay_ha"))
    
    init_named <- as.list(st$initial_assignments)
    names(init_named) <- as.character(st$grid_sf$cell_id)
    
    cur_named <- as.list(st$current_assignments)
    names(cur_named) <- as.character(st$grid_sf$cell_id)
    
    label_pts <- make_area_label_points(st$saved_area_sf)
    
    session$sendCustomMessage(
      "ha_load_scene",
      list(
        districtGeojson = as_geojson_text(st$district_sf),
        gridGeojson = as_geojson_text(st$grid_sf),
        popGeojson = if (!is.null(ha_pop_overlay())) as_geojson_text(ha_pop_overlay()) else NULL,
        showPop = isTRUE(input$show_pop_ha),
        initialAssignments = init_named,
        currentAssignments = cur_named,
        dfaColors = as.list(current_fill_colors_ha()),
        activeDfa = input$active_area_ha,
        neighbors = st$neighbors,
        edgeCells = st$edges,
        brushSize = input$brush_m_ha %||% st$brush_limits$value,
        boundaryOnly = isTRUE(input$boundary_only_ha),
        seedPoints = as_js_point_rows(st$seed_points),
        facilityPoints = as_js_point_rows(st$facility_points),
        savedGeojson = as_geojson_text(st$saved_area_sf),
        labelPoints = if (!is.null(label_pts) && nrow(label_pts) > 0) {
          as_js_point_rows(label_pts)
        } else {
          list()
        }
      )
    )
    
    session$sendCustomMessage("hide_loading", list(id = "loading_overlay_ha"))
  })
  
  observeEvent(input$brush_minus_ha, {
    info <- district_info_ha()
    req(!is.null(input$brush_m_ha))
    updateSliderInput(
      session,
      "brush_m_ha",
      value = clamp_num(input$brush_m_ha - info$brush_limits$step, info$brush_limits$min, info$brush_limits$max)
    )
  })
  
  observeEvent(input$brush_plus_ha, {
    info <- district_info_ha()
    req(!is.null(input$brush_m_ha))
    updateSliderInput(
      session,
      "brush_m_ha",
      value = clamp_num(input$brush_m_ha + info$brush_limits$step, info$brush_limits$min, info$brush_limits$max)
    )
  })
  
  observeEvent(input$brush_m_ha, {
    session$sendCustomMessage("ha_set_brush", list(value = input$brush_m_ha))
  }, ignoreInit = TRUE)
  
  observeEvent(input$boundary_only_ha, {
    session$sendCustomMessage("ha_set_boundary_only", list(value = isTRUE(input$boundary_only_ha)))
  }, ignoreInit = TRUE)
  
  observeEvent(input$active_area_ha, {
    st <- current_ha_state()
    if (is.null(st)) return()
    
    session$sendCustomMessage(
      "ha_set_colors",
      list(
        colors = as.list(current_fill_colors_ha()),
        activeDfa = input$active_area_ha
      )
    )
  }, ignoreInit = TRUE)
  
  recompute_ha_state <- function(key, assignments) {
    st <- rv$ha_by_key[[key]]
    req(!is.null(st))
    
    saved_area_sf <- build_polygons_from_assignments(
      grid_sf = st$grid_sf,
      assignments = assignments,
      district_sf = st$district_sf
    )
    
    pop_table <- estimate_u5_population(saved_area_sf, u5_worldpop_global, name_col = "area_name")
    
    st$current_assignments <- assignments
    st$saved_area_sf <- saved_area_sf
    st$pop_table <- pop_table
    rv$ha_by_key[[key]] <- st
  }
  
  observeEvent(input$save_ha_btn, {
    st <- current_ha_state()
    req(!is.null(st))
    pending_action("save")
    session$sendCustomMessage("ha_request_assignments", list())
  })
  
  observeEvent(input$reset_ha_btn, {
    st <- current_ha_state()
    req(!is.null(st), district_info_ha())
    key <- district_info_ha()$key
    
    st$current_assignments <- st$initial_assignments
    st$saved_area_sf <- build_polygons_from_assignments(
      grid_sf = st$grid_sf,
      assignments = st$initial_assignments,
      district_sf = st$district_sf
    )
    st$pop_table <- estimate_u5_population(st$saved_area_sf, u5_worldpop_global, name_col = "area_name")
    rv$ha_by_key[[key]] <- st
    
    pending_action(NULL)
    session$sendCustomMessage("ha_reset", list())
    
    showNotification("Health area mapping reset to the generated starting boundaries.", type = "message", duration = 4)
  })
  
  observeEvent(input$ha_assignments, {
    payload <- input$ha_assignments
    st <- current_ha_state()
    req(!is.null(st), district_info_ha(), !is.null(payload$assignments))
    
    key <- district_info_ha()$key
    js_assignments <- payload$assignments
    
    ordered_assignments <- vapply(
      as.character(st$grid_sf$cell_id),
      function(id) {
        val <- js_assignments[[id]]
        if (is.null(val) || !nzchar(val)) {
          st$initial_assignments[as.integer(id)]
        } else {
          as.character(val)
        }
      },
      character(1)
    )
    
    if (identical(pending_action(), "save")) {
      recompute_ha_state(key, ordered_assignments)
      pending_action(NULL)
      showNotification("Health area boundaries saved.", type = "message", duration = 4)
    } else {
      st$current_assignments <- ordered_assignments
      rv$ha_by_key[[key]] <- st
    }
  }, ignoreInit = TRUE)
  
  output$download_ha_geojson_btn <- downloadHandler(
    filename = function() {
      paste0("health_areas_", gsub("[^A-Za-z0-9]+", "_", input$district_ha), ".geojson")
    },
    content = function(file) {
      st <- current_ha_state()
      req(!is.null(st), !is.null(st$saved_area_sf))
      sf::st_write(st$saved_area_sf, file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    }
  )
  
  output$download_ha_rds_btn <- downloadHandler(
    filename = function() {
      paste0("health_areas_", gsub("[^A-Za-z0-9]+", "_", input$district_ha), ".Rds")
    },
    content = function(file) {
      st <- current_ha_state()
      req(!is.null(st), !is.null(st$saved_area_sf))
      saveRDS(st$saved_area_sf, file)
    }
  )
  
  # -------------------------------------------------------
  # Help
  # -------------------------------------------------------
  
  observeEvent(input$intro_help_btn, {
    show_intro_help_modal(session)
  })
  
  observeEvent(input$hc_help_btn, {
    show_hc_help_modal(session)
  })
  
  observeEvent(input$ha_help_btn, {
    show_ha_help_modal(session)
  })
}

shinyApp(ui, server)