library(shiny)
library(sf)
library(dplyr)
library(jsonlite)
library(geojsonsf)
library(terra)
library(exactextractr)
library(raster)
library(DT)

# =========================================================
# User config
# =========================================================

districts_file <- "districts_shp.Rds"

# Set these to your actual WorldPop files
# worldpop_f_u1_file   <- "som_agesex_structures_2025_CN_100m_R2024B_v1/som_f_00_2025_CN_100m_R2024B_v1.tif"
# worldpop_m_u1_file   <- "som_agesex_structures_2025_CN_100m_R2024B_v1/som_m_00_2025_CN_100m_R2024B_v1.tif"
# worldpop_f_1to4_file <- "som_agesex_structures_2025_CN_100m_R2024B_v1/som_f_01_2025_CN_100m_R2024B_v1.tif"
# worldpop_m_1to4_file <- "som_agesex_structures_2025_CN_100m_R2024B_v1/som_m_01_2025_CN_100m_R2024B_v1.tif"

worldpop_t_u1_1to4_file   <- "som_u5_population_2025_100m.tif"


default_grid_n <- 100
n_start_dfas <- 5

min_brush_m <- 50
max_brush_m <- 10000
brush_step_m <- 50

show_pop_default <- FALSE
boundary_only_default <- FALSE

starter_dfa_names <- paste("Health Area", seq_len(n_start_dfas))
extra_dfa_names <- c("Inaccessible", "Unpopulated")
all_dfa_names <- c(starter_dfa_names, extra_dfa_names)

selected_fill_color <- "#FFD400"
nonselected_fill_color <- "#757575"
special_fill_colors <- c(
  "Inaccessible" = "#D7301F",
  "Unpopulated" = "#FFFFFF"
)

pop_palette <- colorRampPalette(c(
  "#feebe2", "#fbb4b9", "#fbb4b9", "#c51b8a", "#7a0177"
))

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# =========================================================
# Helpers
# =========================================================

show_help_modal <- function(session) {
  showModal(
    modalDialog(
      title = "Health Area Boundary Review for Microplanning",
      
      div(
        style = "
          max-height: 75vh;
          overflow-y: auto;
          font-size: 14px;
          line-height: 1.5;
        ",
        
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
First, select a district in the left sidebar. Once loaded, select a health area to begin editing. Use the mouse to 'paint' areas according to which health area they belong to. 
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
      size = "l"
    )
  )
}

calculate_grid_cell_population <- function(grid_sf, u5_rast) {
  if (is.null(grid_sf) || nrow(grid_sf) == 0) return(numeric(0))
  
  grid_proj <- sf::st_transform(grid_sf, sf::st_crs(terra::crs(u5_rast)))
  
  vals <- exactextractr::exact_extract(
    x = raster::raster(u5_rast),
    y = grid_proj,
    fun = "sum"
  )
  
  vals[is.na(vals)] <- 0
  as.numeric(vals)
}

safe_make_valid <- function(x) {
  tryCatch(st_make_valid(x), error = function(e) x)
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

load_worldpop_u5_raster <- function(t_u1_1to4_file) {
  t_path  <- path.expand(t_u1_1to4_file)
  
  for (p in c(t_path)) {
    if (!file.exists(p)) stop(paste0("WorldPop raster not found: ", p))
  }

  t_u1_1to4  <- terra::rast(t_path)
  

  u5 <- t_u1_1to4
  names(u5) <- "u5_pop"
  u5
}

estimate_u5_population <- function(polygons_sf, u5_rast, name_col = "dfa_name") {
  if (is.null(polygons_sf) || nrow(polygons_sf) == 0) return(data.frame())

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

make_start_assignment <- function(grid_sf, district_sf, n_dfa = 5, seed = 1) {
  set.seed(seed)

  pts <- st_sample(district_sf, size = n_dfa, exact = TRUE)

  pts_sf <- st_sf(
    dfa_name = paste("Health Area", seq_len(n_dfa)),
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
    summarise(geometry = st_union(geometry), .groups = "drop")

  out <- safe_make_valid(out)
  out <- suppressWarnings(st_intersection(out, district_sf))
  out <- safe_make_valid(out)
  out$geometry <- st_cast(out$geometry, "MULTIPOLYGON", warn = FALSE)

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

clamp_num <- function(x, lo, hi) {
  max(lo, min(hi, x))
}

round_to_step <- function(x, step) {
  round(x / step) * step
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

# u5_worldpop <- load_worldpop_u5_raster(
#   f_u1_file = worldpop_f_u1_file,
#   m_u1_file = worldpop_m_u1_file,
#   f_age1to4_file = worldpop_f_1to4_file,
#   m_age1to4_file = worldpop_m_1to4_file
# )

# =========================================================
# UI
# =========================================================

ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css",
      integrity = "sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY=",
      crossorigin = ""
    ),
    tags$script(
      src = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js",
      integrity = "sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=",
      crossorigin = ""
    ),
    tags$style(HTML("
  html, body {
    height: 100%;
    margin: 0;
    padding: 0;
  }
  .container-fluid {
    padding: 6px;
  }
  #app_row {
    display: flex;
    gap: 6px;
    height: calc(100vh - 12px);
  }
  #leftbar {
    flex: 2;
    min-width: 240px;
    max-width: 340px;
    background: #FAFAFA;
    border: 1px solid #E6E6E6;
    border-radius: 6px;
    padding: 8px;
    overflow-y: auto;
  }
  #mapwrap {
  flex: 8;
  min-width: 0;
  position: relative;
  height: 100%;
}

#paint_map {
  width: 100%;
  height: 100%;
  min-height: 700px;
  border: 1px solid #E6E6E6;
  border-radius: 6px;
  background: #D9D9D9;
}

#loading_overlay {
  display: none;
  position: absolute;
  inset: 0;
  z-index: 900;
  background: rgba(255,255,255,0.55);
  align-items: center;
  justify-content: center;
  pointer-events: none;
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
  .dfa-map-label {
    background: rgba(255,255,255,0.92);
    border: 1px solid #CCCCCC;
    border-radius: 3px;
    padding: 1px 4px;
    color: #000000;
    font-size: 10px;
    white-space: nowrap;
  }
  .leaflet-tooltip.dfa-tooltip {
    background: transparent;
    border: none;
    box-shadow: none;
    padding: 0;
  }
  .leaflet-tooltip.dfa-tooltip:before {
    display: none;
  }
")),
    tags$script(HTML("
    Shiny.addCustomMessageHandler('show_loading', function(msg) {
  var el = document.getElementById('loading_overlay');
  if (el) {
    el.style.display = 'flex';
  }
});

Shiny.addCustomMessageHandler('hide_loading', function(msg) {
  var el = document.getElementById('loading_overlay');
  if (el) {
    el.style.display = 'none';
  }
});
      window.paintApp = {
        map: null,
        districtLayer: null,
        popLayer: null,
        gridLayer: null,
        savedLayer: null,
        seedLayer: null,
        brushPreview: null,
        isPainting: false,
        assignments: {},
        initialAssignments: {},
        dfaColors: {},
        activeDfa: null,
        cellLayers: {},
        centroids: {},
        neighbors: {},
        edgeCells: {},
        brushSize: 300,
        boundaryOnly: false,
        baseLayers: {},
        baseControl: null,
        currentBaseLayer: null,

        currentBrushSize: function() {
          return window.paintApp.brushSize || 300;
        },
        ensureMap: function() {
  if (window.paintApp.map) return;

  window.paintApp.map = L.map('paint_map', {
    zoomSnap: 0.25,
    preferCanvas: true,
    dragging: false
  });

  var osm = L.tileLayer(
    'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
    {
      maxZoom: 20,
      attribution: '&copy; OpenStreetMap contributors'
    }
  );

  var esriImagery = L.tileLayer(
    'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
    {
      maxZoom: 20,
      attribution: 'Tiles &copy; Esri'
    }
  );

  var cartoLight = L.tileLayer(
    'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',
    {
      maxZoom: 20,
      subdomains: 'abcd',
      attribution: '&copy; OpenStreetMap contributors &copy; CARTO'
    }
  );

  var topo = L.tileLayer(
    'https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png',
    {
      maxZoom: 17,
      attribution: 'Map data: &copy; OpenStreetMap contributors, SRTM | Map style: &copy; OpenTopoMap'
    }
  );

  window.paintApp.baseLayers = {
    'OpenStreetMap': osm,
    'ESRI Satellite': esriImagery,
    'CARTO Light': cartoLight,
    'Topo': topo
  };

  window.paintApp.currentBaseLayer = osm;
  window.paintApp.currentBaseLayer.addTo(window.paintApp.map);

  window.paintApp.baseControl = L.control.layers(
    window.paintApp.baseLayers,
    null,
    {
      collapsed: true,
      position: 'topright'
    }
  ).addTo(window.paintApp.map);

  window.paintApp.brushPreview = L.circle([0, 0], {
    radius: window.paintApp.currentBrushSize(),
    color: '#222222',
    weight: 1,
    opacity: 0.7,
    fillOpacity: 0.05,
    interactive: false
  });

  window.paintApp.map.on('baselayerchange', function(e) {
    window.paintApp.currentBaseLayer = e.layer;
  });

  var container = window.paintApp.map.getContainer();

  window.paintApp.isPainting = false;
  window.paintApp.isRightPanning = false;
  window.paintApp.rightPanStart = null;

  L.DomEvent.on(container, 'contextmenu', function(e) {
    L.DomEvent.preventDefault(e);
  });

  window.paintApp.map.on('mousemove', function(e) {
    if (window.paintApp.brushPreview) {
      window.paintApp.brushPreview.setLatLng(e.latlng);
      window.paintApp.brushPreview.setRadius(window.paintApp.currentBrushSize());

      if (!window.paintApp.map.hasLayer(window.paintApp.brushPreview)) {
        window.paintApp.brushPreview.addTo(window.paintApp.map);
      }
    }

    if (window.paintApp.isPainting) {
      window.paintApp.paintAtLatLng(e.latlng);
    }
  });

  L.DomEvent.on(container, 'mousedown', function(e) {
    // left click = paint
    if (e.button === 0) {
      if (!window.paintApp.gridLayer) return;

      window.paintApp.isPainting = true;
      window.paintApp.paintAtLatLng(
        window.paintApp.map.mouseEventToLatLng(e)
      );

      L.DomEvent.preventDefault(e);
      return;
    }

    // right click = start manual pan
    if (e.button === 2) {
      window.paintApp.isRightPanning = true;
      window.paintApp.rightPanStart = {
        x: e.clientX,
        y: e.clientY
      };

      L.DomEvent.preventDefault(e);
      return;
    }
  });

  L.DomEvent.on(document, 'mousemove', function(e) {
    if (!window.paintApp.isRightPanning) return;
    if (!window.paintApp.rightPanStart) return;

    var dx = e.clientX - window.paintApp.rightPanStart.x;
    var dy = e.clientY - window.paintApp.rightPanStart.y;

    window.paintApp.map.panBy([-dx, -dy], {
      animate: false
    });

    window.paintApp.rightPanStart = {
      x: e.clientX,
      y: e.clientY
    };

    L.DomEvent.preventDefault(e);
  });

  L.DomEvent.on(document, 'mouseup', function(e) {
    if (e.button === 0) {
      window.paintApp.isPainting = false;
    }

    if (e.button === 2) {
      window.paintApp.isRightPanning = false;
      window.paintApp.rightPanStart = null;
    }
  });

 setTimeout(function() { window.paintApp.map.invalidateSize(); }, 300);
  setTimeout(function() { window.paintApp.map.invalidateSize(); }, 900);
},

        isBoundaryCell: function(id) {
          id = String(id);
          var myDfa = window.paintApp.assignments[id];
          var nbrs = window.paintApp.neighbors[id] || [];
          var touchesEdge = !!window.paintApp.edgeCells[id];

          if (touchesEdge) return true;

          for (var i = 0; i < nbrs.length; i++) {
            var nbrId = String(nbrs[i]);
            if (window.paintApp.assignments[nbrId] !== myDfa) {
              return true;
            }
          }

          return false;
        },

        fillForDfa: function(dfa) {
          return window.paintApp.dfaColors[dfa] || '#757575';
        },

        borderColorForDfa: function(dfa) {
          if (dfa === 'Inaccessible') return '#D7301F';
          if (dfa === 'Unpopulated') return '#FFFFFF';
          if (dfa === window.paintApp.activeDfa) return '#FFD400';
          return '#000000';
        },

        styleForFeature: function(feature) {
          var id = String(feature.properties.cell_id);
          var dfa = window.paintApp.assignments[id];
        
          var fillColor = window.paintApp.fillForDfa(dfa);
        
          var isBoundary = window.paintApp.isBoundaryCell(id);
        
          var isSelectedBoundary =
            isBoundary &&
            dfa === window.paintApp.activeDfa;
        
          var fillOpacity;
        
          if (window.paintApp.boundaryOnly) {
        
            if (isSelectedBoundary) {
              fillOpacity = 0.9;
            } else {
              fillOpacity = 0.0;
            }
        
          } else {
        
            if (isSelectedBoundary) {
              fillOpacity = 0.85;
            } else {
              fillOpacity = 0.3;
            }
        
          }
        
          return {
            stroke: isSelectedBoundary,
        
            color: window.paintApp.borderColorForDfa(dfa),
        
            weight: isSelectedBoundary ? 0.8 : 0,
        
            opacity: isSelectedBoundary ? 1.0 : 0.0,
        
            fillColor: fillColor,
        
            fillOpacity: fillOpacity
          };
        },

        popStyleForFeature: function(feature) {
          return {
            stroke: false,
            fillColor: feature.properties.fill_color || '#000000',
            fillOpacity: 0.35
          };
        },

        refreshCellsAndNeighbors: function(cellIds) {
          var toUpdate = {};

          for (var i = 0; i < cellIds.length; i++) {
            var id = String(cellIds[i]);
            toUpdate[id] = true;

            var nbrs = window.paintApp.neighbors[id] || [];
            for (var j = 0; j < nbrs.length; j++) {
              toUpdate[String(nbrs[j])] = true;
            }
          }

          Object.keys(toUpdate).forEach(function(id) {
            if (window.paintApp.cellLayers[id]) {
              window.paintApp.cellLayers[id].setStyle(
                window.paintApp.styleForFeature(window.paintApp.cellLayers[id].feature)
              );
            }
          });
        },

        paintCells: function(cellIds) {
          var dfa = window.paintApp.activeDfa;
          if (!dfa || !cellIds || cellIds.length === 0) return;

          for (var i = 0; i < cellIds.length; i++) {
            var id = String(cellIds[i]);
            window.paintApp.assignments[id] = dfa;
          }

          window.paintApp.refreshCellsAndNeighbors(cellIds);
        },

        paintAtLatLng: function(latlng) {
          if (!window.paintApp.gridLayer) return;

          var brushSize = window.paintApp.currentBrushSize();
          var touched = [];

          for (var id in window.paintApp.centroids) {
            var c = window.paintApp.centroids[id];
            var d = window.paintApp.map.distance(
              [latlng.lat, latlng.lng],
              [c.lat, c.lng]
            );
            if (d <= brushSize) {
              touched.push(id);
            }
          }

          window.paintApp.paintCells(touched);
        },

        refreshAllStyles: function() {
          if (!window.paintApp.gridLayer) return;
          window.paintApp.gridLayer.eachLayer(function(layer) {
            layer.setStyle(window.paintApp.styleForFeature(layer.feature));
          });
        },

        clearScene: function() {
          if (!window.paintApp.map) return;

          if (window.paintApp.districtLayer) {
            window.paintApp.map.removeLayer(window.paintApp.districtLayer);
            window.paintApp.districtLayer = null;
          }
          if (window.paintApp.seedLayer) {
            window.paintApp.map.removeLayer(window.paintApp.seedLayer);
            window.paintApp.seedLayer = null;
          }
          if (window.paintApp.popLayer) {
            window.paintApp.map.removeLayer(window.paintApp.popLayer);
            window.paintApp.popLayer = null;
          }
          if (window.paintApp.gridLayer) {
            window.paintApp.map.removeLayer(window.paintApp.gridLayer);
            window.paintApp.gridLayer = null;
          }
          if (window.paintApp.savedLayer) {
            window.paintApp.map.removeLayer(window.paintApp.savedLayer);
            window.paintApp.savedLayer = null;
          }

          window.paintApp.cellLayers = {};
          window.paintApp.centroids = {};
          window.paintApp.neighbors = {};
          window.paintApp.edgeCells = {};
        },

        setPopulationVisibility: function(showIt) {
          if (!window.paintApp.map || !window.paintApp.popLayer) return;
          if (showIt) {
            if (!window.paintApp.map.hasLayer(window.paintApp.popLayer)) {
              window.paintApp.popLayer.addTo(window.paintApp.map);
            }
          } else {
            if (window.paintApp.map.hasLayer(window.paintApp.popLayer)) {
              window.paintApp.map.removeLayer(window.paintApp.popLayer);
            }
          }
        },

        setBrushSize: function(v) {
          window.paintApp.brushSize = v;
          if (window.paintApp.brushPreview) {
            window.paintApp.brushPreview.setRadius(v);
          }
        },

        setBoundaryOnly: function(v) {
          window.paintApp.boundaryOnly = !!v;
          window.paintApp.refreshAllStyles();
        },

        setColorsAndActive: function(colorsObj, activeDfa) {
          window.paintApp.dfaColors = colorsObj || {};
          window.paintApp.activeDfa = activeDfa || null;
          window.paintApp.refreshAllStyles();

          if (window.paintApp.savedLayer) {
            window.paintApp.savedLayer.eachLayer(function(layer) {
              var nm = layer.feature.properties && layer.feature.properties.dfa_name;
              layer.setStyle({
                color: window.paintApp.borderColorForDfa(nm),
                weight: 2.5,
                opacity: 1,
                dashArray: null,
                fill: false
              });
            });
          }
        },

        loadScene: function(msg) {
          window.paintApp.ensureMap();
          window.paintApp.clearScene();

          window.paintApp.initialAssignments = JSON.parse(JSON.stringify(msg.initialAssignments));
          window.paintApp.assignments = JSON.parse(JSON.stringify(msg.initialAssignments));
          window.paintApp.dfaColors = msg.dfaColors || {};
          window.paintApp.activeDfa = msg.activeDfa || null;
          window.paintApp.neighbors = msg.neighbors || {};
          window.paintApp.edgeCells = msg.edgeCells || {};
          window.paintApp.brushSize = msg.brushSize || window.paintApp.brushSize;
          window.paintApp.boundaryOnly = !!msg.boundaryOnly;

          if (window.paintApp.brushPreview) {
            window.paintApp.brushPreview.setRadius(window.paintApp.brushSize);
          }
          
          if (msg.seedPoints && Array.isArray(msg.seedPoints)) {
  window.paintApp.seedLayer = L.layerGroup();

  msg.seedPoints.forEach(function(pt) {
    if (pt.lon == null || pt.lat == null) return;

    L.circleMarker([pt.lat, pt.lon], {
      radius: 4,
      color: '#000000',
      weight: 1,
      opacity: 1,
      fillColor: '#FFFFFF',
      fillOpacity: 1,
      interactive: false
    })
    .bindTooltip(
      '<div class=\"dfa-map-label\">' + pt.dfa_name + '</div>',
      {
        permanent: true,
        direction: 'top',
        offset: [0, -6],
        className: 'dfa-tooltip'
      }
    )
    .addTo(window.paintApp.seedLayer);
  });

  window.paintApp.seedLayer.addTo(window.paintApp.map);
}

          var districtGeo = (typeof msg.districtGeojson === 'string') ? JSON.parse(msg.districtGeojson) : msg.districtGeojson;
          var gridGeo = (typeof msg.gridGeojson === 'string') ? JSON.parse(msg.gridGeojson) : msg.gridGeojson;

          window.paintApp.districtLayer = L.geoJSON(districtGeo, {
            style: {
              color: '#000000',
              weight: 2,
              fill: false,
              opacity: 1
            }
          }).addTo(window.paintApp.map);

          if (msg.popGeojson) {
            var popGeo = (typeof msg.popGeojson === 'string') ? JSON.parse(msg.popGeojson) : msg.popGeojson;
            window.paintApp.popLayer = L.geoJSON(popGeo, {
              style: function(feature) {
                return window.paintApp.popStyleForFeature(feature);
              },
              interactive: false
            });

            if (msg.showPop) {
              window.paintApp.popLayer.addTo(window.paintApp.map);
            }
          }

          window.paintApp.gridLayer = L.geoJSON(gridGeo, {
            style: function(feature) {
              return window.paintApp.styleForFeature(feature);
            },
            onEachFeature: function(feature, layer) {
              var id = String(feature.properties.cell_id);
              window.paintApp.cellLayers[id] = layer;
              window.paintApp.centroids[id] = {
                lng: feature.properties.centroid_lon,
                lat: feature.properties.centroid_lat
              };

              layer.on('click', function(e) {
                window.paintApp.paintAtLatLng(e.latlng);
              });
            }
          }).addTo(window.paintApp.map);

          if (msg.savedGeojson) {
            var savedGeo = (typeof msg.savedGeojson === 'string') ? JSON.parse(msg.savedGeojson) : msg.savedGeojson;

            window.paintApp.savedLayer = L.geoJSON(savedGeo, {
              style: function(feature) {
                var nm = feature.properties && feature.properties.dfa_name;
                return {
                  color: window.paintApp.borderColorForDfa(nm),
                  weight: 2.5,
                  opacity: 1,
                  dashArray: null,
                  fill: false
                };
              }
            }).addTo(window.paintApp.map);
          }

          window.paintApp.map.fitBounds(window.paintApp.districtLayer.getBounds(), { padding: [10, 10] });
          setTimeout(function() { window.paintApp.map.invalidateSize(); }, 150);

          Shiny.setInputValue('paint_map_ready', Date.now(), { priority: 'event' });
        },

        resetAssignments: function() {
          window.paintApp.assignments = JSON.parse(JSON.stringify(window.paintApp.initialAssignments));
          window.paintApp.refreshAllStyles();

          if (window.paintApp.savedLayer) {
            window.paintApp.map.removeLayer(window.paintApp.savedLayer);
            window.paintApp.savedLayer = null;
          }
        },

        emitAssignments: function() {
          Shiny.setInputValue(
            'paint_assignments',
            { assignments: window.paintApp.assignments, nonce: Date.now() },
            { priority: 'event' }
          );
        },

        showSaved: function(geojsonText) {
          if (!window.paintApp.map) return;

          if (window.paintApp.savedLayer) {
            window.paintApp.map.removeLayer(window.paintApp.savedLayer);
            window.paintApp.savedLayer = null;
          }

          if (!geojsonText) return;

          var gj = (typeof geojsonText === 'string') ? JSON.parse(geojsonText) : geojsonText;

          window.paintApp.savedLayer = L.geoJSON(gj, {
            style: function(feature) {
              var nm = feature.properties && feature.properties.dfa_name;
              return {
                color: window.paintApp.borderColorForDfa(nm),
                weight: 2.5,
                opacity: 1,
                dashArray: null,
                fill: false
              };
            }
          }).addTo(window.paintApp.map);
        }
      };

      document.addEventListener('DOMContentLoaded', function() {
        setTimeout(function() {
          window.paintApp.ensureMap();
        }, 100);
      });

      if (window.Shiny) {
        Shiny.addCustomMessageHandler('paint_load_scene', function(msg) {
          window.paintApp.loadScene(msg);
        });

        Shiny.addCustomMessageHandler('paint_reset', function(msg) {
          window.paintApp.resetAssignments();
        });

        Shiny.addCustomMessageHandler('paint_request_assignments', function(msg) {
          window.paintApp.emitAssignments();
        });

        Shiny.addCustomMessageHandler('paint_show_saved', function(msg) {
          window.paintApp.showSaved(msg.geojson);
        });

        Shiny.addCustomMessageHandler('paint_toggle_population', function(msg) {
          window.paintApp.setPopulationVisibility(!!msg.show);
        });

        Shiny.addCustomMessageHandler('paint_set_brush', function(msg) {
          window.paintApp.setBrushSize(msg.value);
        });

        Shiny.addCustomMessageHandler('paint_set_boundary_only', function(msg) {
          window.paintApp.setBoundaryOnly(msg.value);
        });

        Shiny.addCustomMessageHandler('paint_set_colors', function(msg) {
          window.paintApp.setColorsAndActive(msg.colors, msg.activeDfa);
        });
      }
    "))
  ),
  div(
    id = "app_row",
    div(
      id = "leftbar",
      div(
        class = "top-help",
        style = "
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 6px;
  ",
        
        div(
          style = "
      font-size: 16px;
      font-weight: 600;
      color: #333333;
    ",
          "Health Area Boundary Review for Microplanning"
        ),
        
        actionButton(
          "help_btn",
          "?",
          width = "32px"
        )
      ),
      div(class = "mini-label", "Zone"),
      selectInput("zone_select", NULL, choices = zone_choices, selected = zone_choices[1]),
      div(class = "mini-label", "Region"),
      selectInput("region_select", NULL, choices = NULL),
      div(class = "mini-label", "District"),
      selectInput("district_select", NULL, choices = NULL),
      div(class = "section-gap mini-label", "Edit Health Area"),
      selectInput("active_dfa", NULL, choices = all_dfa_names, selected = starter_dfa_names[1]),
      div(class = "mini-label", "Brush Size:"),
      div(
        class = "slider-row",
        actionButton("brush_minus", "-", width = "30px"),
        div(
          class = "slider-wrap",
          sliderInput(
            "brush_m_ui", NULL,
            min = min_brush_m, max = max_brush_m,
            value = 300, step = brush_step_m, width = "100%"
          )
        ),
        actionButton("brush_plus", "+", width = "30px")
      ),
      checkboxInput("show_pop_raster", "Show WorldPop U5 Population", value = show_pop_default),
      checkboxInput("boundary_only", "Boundaries only", value = boundary_only_default),
      div(class = "control-row",
          actionButton("reset_btn", "Reset"),
          actionButton("save_btn", "Save"))
    ),
    div(
      id = "mapwrap",
      div(id = "paint_map"),
      div(
        id = "loading_overlay",
        div(
          style = "
        background: rgba(255,255,255,0.96);
        padding: 12px 18px;
        border: 1px solid #D9D9D9;
        border-radius: 6px;
        box-shadow: 0 1px 6px rgba(0,0,0,0.08);
        font-size: 16px;
        font-weight: 600;
        color: #333333;
      ",
          "Loading district data..."
        )
      )
    ),
    div(
      id = "rightbar",
      div(class = "rightbar-title", "Legend and population"),
      uiOutput("legend_ui"),
      DTOutput("pop_table")
    )
  )
)

# =========================================================
# Server
# =========================================================

server <- function(input, output, session) {
  pending_action <- reactiveVal(NULL)
  
  observe({
    show_help_modal(session)
  })

  rv <- reactiveValues(
    district_sf = NULL,
    district_base_sf = NULL,
    grid_sf = NULL,
    initial_assignments = NULL,
    current_assignments = NULL,
    saved_dfa_sf = NULL,
    neighbors_list = NULL,
    edge_list = NULL,
    pop_overlay_sf = NULL,
    pop_table = NULL,
    max_dim_m = NULL,
    grid_limits = NULL,
    brush_limits = NULL,
    seed_points = NULL
  )

  current_fill_colors <- reactive({
    make_fill_colors(input$active_dfa)
  })
  
  u5_worldpop_rv <- reactiveVal(NULL)
  
  get_u5_worldpop <- function() {
    if (is.null(u5_worldpop_rv())) {
      u5_worldpop_rv(
        load_worldpop_u5_raster(
          t_u1_1to4_file = worldpop_t_u1_1to4_file
        )
      )
    }
    u5_worldpop_rv()
  }
  
  observeEvent(input$district_select, {
    session$sendCustomMessage("show_loading", list())
  }, ignoreInit = TRUE)
  
  observeEvent(input$paint_map_ready, {
    session$sendCustomMessage("hide_loading", list())
  }, ignoreInit = TRUE)
  
  observe({
    session$sendCustomMessage("show_loading", list())
  })

  output$legend_ui <- renderUI({
    selected_name <- input$active_dfa %||% starter_dfa_names[1]
    show_selected <- !(selected_name %in% c("Inaccessible", "Unpopulated"))
    
    raster_cols <- pop_palette(5)
    raster_labels <- c("Low", "", "", "", "High")
    
    tagList(
      div(
        class = "legend-wrap",
        
        if (show_selected) {
          div(
            class = "legend-row",
            tags$span(class = "legend-box", style = paste0("background:", selected_fill_color, ";")),
            tags$span(selected_name)
          )
        },
        
        div(
          class = "legend-row",
          tags$span(class = "legend-box", style = paste0("background:", nonselected_fill_color, ";")),
          tags$span("Other Health Areas")
        ),
        div(
          class = "legend-row",
          tags$span(
            class = "legend-box",
            style = paste0(
              "background:", special_fill_colors[["Inaccessible"]],
              "; border-color:", special_fill_colors[["Inaccessible"]], ";"
            )
          ),
          tags$span("Inaccessible")
        ),
        div(
          class = "legend-row",
          tags$span(class = "legend-box", style = "background:#FFFFFF;"),
          tags$span("Unpopulated")
        ),
        
        if (isTRUE(input$show_pop_raster)) {
          tagList(
            tags$div(
              style = "height:6px;"
            ),
            tags$div(
              class = "mini-label",
              style = "margin-bottom:4px;",
              "WorldPop U5 Population"
            ),
            tags$div(
              style = "display:flex; gap:0; margin-bottom:3px;",
              lapply(raster_cols, function(clr) {
                tags$div(
                  style = paste0(
                    "flex:1; height:10px; background:", clr,
                    "; border-top:1px solid #999999; border-bottom:1px solid #999999;"
                  )
                )
              })
            ),
            tags$div(
              style = "display:flex; justify-content:space-between; font-size:10px; color:#666666;",
              lapply(raster_labels, function(lbl) tags$span(lbl))
            )
          )
        }
      )
    )
  })

  recompute_population_table <- function(assignments) {
    req(!is.null(rv$grid_sf), length(assignments) == nrow(rv$grid_sf))
    req("u5_pop" %in% names(rv$grid_sf))
    
    df <- data.frame(
      area_name = assignments,
      est_u5_pop = rv$grid_sf$u5_pop,
      stringsAsFactors = FALSE
    ) |>
      dplyr::group_by(area_name) |>
      dplyr::summarise(est_u5_pop = round(sum(est_u5_pop, na.rm = TRUE), 0), .groups = "drop")
    
    missing_classes <- setdiff(all_dfa_names, df$area_name)
    if (length(missing_classes) > 0) {
      df <- dplyr::bind_rows(
        df,
        data.frame(
          area_name = missing_classes,
          est_u5_pop = 0,
          stringsAsFactors = FALSE
        )
      )
    }
    
    df <- df |>
      dplyr::mutate(area_name = factor(area_name, levels = all_dfa_names)) |>
      dplyr::arrange(area_name) |>
      dplyr::mutate(area_name = as.character(area_name))
    
    district_total <- round(sum(rv$grid_sf$u5_pop, na.rm = TRUE), 0)
    
    rv$pop_table <- dplyr::bind_rows(
      df,
      data.frame(
        area_name = "District Total",
        est_u5_pop = district_total,
        stringsAsFactors = FALSE
      )
    )
    
    invisible(NULL)
  }

  observeEvent(input$help_btn, {
    show_help_modal(session)
  })

  observeEvent(input$zone_select, {
    req(input$zone_select)
    regions <- districts_shp |>
      filter(zone_name == input$zone_select) |>
      pull(region_name) |>
      as.character() |>
      unique() |>
      sort()
    updateSelectInput(session, "region_select", choices = regions, selected = regions[1])
  }, ignoreInit = FALSE)

  observeEvent(list(input$zone_select, input$region_select), {
    req(input$zone_select, input$region_select)
    dists <- districts_shp |>
      filter(zone_name == input$zone_select, region_name == input$region_select) |>
      pull(district_name) |>
      as.character() |>
      unique() |>
      sort()
    updateSelectInput(session, "district_select", choices = dists, selected = dists[1])
  }, ignoreInit = FALSE)

  district_base <- reactive({
    req(input$zone_select, input$region_select, input$district_select)

    district_sf <- districts_shp |>
      filter(
        zone_name == input$zone_select,
        region_name == input$region_select,
        district_name == input$district_select
      ) |>
      dplyr::select(admin_id, district_name, region_id, region_name, zone_id, zone_name, geometry)

    req(nrow(district_sf) >= 1)

    district_sf <- district_sf |>
      summarise(
        admin_id = dplyr::first(admin_id),
        district_name = dplyr::first(district_name),
        region_id = dplyr::first(region_id),
        region_name = dplyr::first(region_name),
        zone_id = dplyr::first(zone_id),
        zone_name = dplyr::first(zone_name),
        geometry = st_union(geometry),
        .groups = "drop"
      ) |>
      st_as_sf()

    district_sf <- safe_make_valid(district_sf)
    max_dim_m <- calc_district_max_dim(district_sf)

    list(
      district_sf = district_sf,
      max_dim_m = max_dim_m,
      grid_limits = calc_grid_limits(max_dim_m),
      brush_limits = calc_brush_limits(max_dim_m)
    )
  })

  observeEvent(district_base(), {
    db <- district_base()

    rv$district_base_sf <- db$district_sf
    rv$grid_limits <- db$grid_limits
    rv$brush_limits <- db$brush_limits

    updateSliderInput(
      session,
      "brush_m_ui",
      min = db$brush_limits$min,
      max = db$brush_limits$max,
      value = db$brush_limits$value,
      step = db$brush_limits$step
    )
  }, ignoreInit = FALSE)

  observeEvent(input$brush_minus, {
    bl <- rv$brush_limits
    req(!is.null(bl), !is.null(input$brush_m_ui))
    updateSliderInput(session, "brush_m_ui", value = clamp_num(input$brush_m_ui - bl$step, bl$min, bl$max))
  })

  observeEvent(input$brush_plus, {
    bl <- rv$brush_limits
    req(!is.null(bl), !is.null(input$brush_m_ui))
    updateSliderInput(session, "brush_m_ui", value = clamp_num(input$brush_m_ui + bl$step, bl$min, bl$max))
  })

  observeEvent(input$brush_m_ui, {
    session$sendCustomMessage("paint_set_brush", list(value = input$brush_m_ui))
  }, ignoreInit = TRUE)

  observeEvent(input$boundary_only, {
    session$sendCustomMessage("paint_set_boundary_only", list(value = isTRUE(input$boundary_only)))
  }, ignoreInit = TRUE)

  observeEvent(input$show_pop_raster, {
    session$sendCustomMessage("paint_toggle_population", list(show = isTRUE(input$show_pop_raster)))
  }, ignoreInit = TRUE)

  selected_scene <- reactive({
    req(input$district_select)

    db <- district_base()
    district_sf <- db$district_sf

    grid_info <- make_paint_grid(district_sf, grid_n = db$grid_limits$value)
    grid_sf <- grid_info$grid_sf
    req(nrow(grid_sf) > 0)

    district_seed <- sum(utf8ToInt(input$district_select))

    start_info <- make_start_assignment(
      grid_sf = grid_sf,
      district_sf = district_sf,
      n_dfa = n_start_dfas,
      seed = district_seed
    )
    
    initial_assignments <- as.character(start_info$assignments)
    
    seed_pts <- sf::st_transform(start_info$seeds_sf, 4326)
    seed_coords <- sf::st_coordinates(seed_pts)
    seed_points_df <- data.frame(
      dfa_name = seed_pts$dfa_name,
      lon = seed_coords[, 1],
      lat = seed_coords[, 2],
      stringsAsFactors = FALSE
    )

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

    pop_overlay_sf <- NULL
    if (isTRUE(input$show_pop_raster)) {
      pop_overlay_sf <- tryCatch(
        make_population_overlay_sf(district_sf, get_u5_worldpop()),
        error = function(e) NULL
      )
    }
    list(
      district_sf = district_sf,
      grid_sf = grid_sf,
      initial_assignments = initial_assignments,
      neighbors_list = neighbors_list,
      edge_list = edge_list,
      pop_overlay_sf = pop_overlay_sf,
      max_dim_m = grid_info$max_dim_m,
      seed_points = seed_points_df
    )
  })

  observeEvent(selected_scene(), {
    sc <- selected_scene()

    rv$district_sf <- sc$district_sf
    rv$grid_sf <- sc$grid_sf
    if (!"u5_pop" %in% names(rv$grid_sf)) {
      rv$grid_sf$u5_pop <- calculate_grid_cell_population(
        rv$grid_sf,
        get_u5_worldpop()
      )
    }
    
    rv$initial_assignments <- sc$initial_assignments
    rv$current_assignments <- sc$initial_assignments
    rv$saved_dfa_sf <- NULL
    rv$neighbors_list <- sc$neighbors_list
    rv$edge_list <- sc$edge_list
    rv$pop_overlay_sf <- sc$pop_overlay_sf
    rv$pop_table <- NULL
    rv$max_dim_m <- sc$max_dim_m
    rv$seed_points <- sc$seed_points

    if (!is.null(rv$brush_limits)) {
      updateSliderInput(
        session,
        "brush_m_ui",
        min = rv$brush_limits$min,
        max = rv$brush_limits$max,
        value = rv$brush_limits$value,
        step = rv$brush_limits$step
      )
    }

    init_named <- setNames(as.list(sc$initial_assignments), as.character(sc$grid_sf$cell_id))

    pop_geojson <- NULL
    if (!is.null(sc$pop_overlay_sf) && nrow(sc$pop_overlay_sf) > 0) {
      pop_geojson <- as_geojson_text(sc$pop_overlay_sf)
    }

    initial_saved_sf <- build_saved_dfa_sf(
      grid_sf = sc$grid_sf,
      assignments = sc$initial_assignments,
      district_sf = sc$district_sf
    )

    session$sendCustomMessage(
      "paint_load_scene",
      list(
        districtGeojson = as_geojson_text(sc$district_sf),
        gridGeojson = as_geojson_text(sc$grid_sf),
        popGeojson = pop_geojson,
        showPop = isTRUE(input$show_pop_raster),
        initialAssignments = init_named,
        dfaColors = as.list(current_fill_colors()),
        activeDfa = input$active_dfa,
        neighbors = sc$neighbors_list,
        edgeCells = sc$edge_list,
        brushSize = input$brush_m_ui,
        boundaryOnly = isTRUE(input$boundary_only),
        seedPoints = sc$seed_points,
        savedGeojson = as_geojson_text(initial_saved_sf)
      )
    )

    recompute_population_table(sc$initial_assignments)
  }, ignoreInit = FALSE)

  observeEvent(input$save_btn, {
    pending_action("save")
    session$sendCustomMessage("paint_request_assignments", list())
  })

  observeEvent(input$reset_btn, {
    req(!is.null(rv$initial_assignments))

    rv$current_assignments <- rv$initial_assignments
    rv$saved_dfa_sf <- NULL
    pending_action(NULL)

    session$sendCustomMessage("paint_reset", list())
    session$sendCustomMessage(
      "paint_set_colors",
      list(
        colors = as.list(current_fill_colors()),
        activeDfa = input$active_dfa
      )
    )

    recompute_population_table(rv$initial_assignments)
  })

  observeEvent(input$paint_assignments, {
    payload <- input$paint_assignments
    req(!is.null(payload$assignments))
    req(!is.null(rv$grid_sf), !is.null(rv$district_sf), !is.null(rv$initial_assignments))

    js_assignments <- payload$assignments

    ordered_assignments <- vapply(
      as.character(rv$grid_sf$cell_id),
      function(id) {
        val <- js_assignments[[id]]
        if (is.null(val) || !nzchar(val)) {
          rv$initial_assignments[as.integer(id)]
        } else {
          as.character(val)
        }
      },
      character(1)
    )

    rv$current_assignments <- ordered_assignments

    act <- pending_action()

    if (identical(act, "save")) {
      saved <- tryCatch(
        build_saved_dfa_sf(
          grid_sf = rv$grid_sf,
          assignments = ordered_assignments,
          district_sf = rv$district_sf
        ),
        error = function(e) e
      )

      if (inherits(saved, "error")) {
        showNotification(paste("Save failed:", saved$message), type = "error", duration = 8)
        pending_action(NULL)
        return()
      }

      rv$saved_dfa_sf <- saved
      session$sendCustomMessage("paint_show_saved", list(geojson = as_geojson_text(saved)))
    }

    if (identical(act, "save") || identical(act, "refresh")) {
      recompute_population_table(ordered_assignments)
    }

    pending_action(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$active_dfa, {
    req(!is.null(rv$current_assignments), !is.null(rv$grid_sf), !is.null(rv$district_sf))

    session$sendCustomMessage(
      "paint_set_colors",
      list(
        colors = as.list(current_fill_colors()),
        activeDfa = input$active_dfa
      )
    )

    pending_action("save")
    session$sendCustomMessage("paint_request_assignments", list())
  }, ignoreInit = TRUE)

  output$pop_table <- renderDT({
    if (is.null(rv$pop_table) || nrow(rv$pop_table) == 0) {
      return(
        datatable(
          data.frame(Area = character(0), `Estimated U5 population` = numeric(0)),
          options = list(dom = "t", paging = FALSE, ordering = FALSE, searching = FALSE),
          rownames = FALSE
        )
      )
    }

    datatable(
      rv$pop_table |>
        rename(
          Area = area_name,
          `WorldPop U5 Population` = est_u5_pop
        ),
      options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE, searching = FALSE),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
