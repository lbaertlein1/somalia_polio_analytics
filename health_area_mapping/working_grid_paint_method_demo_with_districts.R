# app.R

library(shiny)
library(sf)
library(dplyr)
library(jsonlite)
library(geojsonsf)

# =========================================================
# User config
# =========================================================

districts_file <- file.path(getwd(), "data/districts_shp.Rds")

# Default grid resolution: about 100 x 100 across the district extent
default_grid_n <- 100

# Number of arbitrary starter DFAs
n_start_dfas <- 5

# Brush size defaults in meters
default_brush_m <- 300
min_brush_m <- 50
max_brush_m <- 3000
brush_step_m <- 50

base_palette <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
  "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62",
  "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"
)

# =========================================================
# Helpers
# =========================================================

safe_make_valid <- function(x) {
  tryCatch(st_make_valid(x), error = function(e) x)
}

as_geojson_text <- function(x) {
  geojsonsf::sf_geojson(x)
}

make_palette_for_values <- function(values) {
  vals <- unique(as.character(values))
  vals <- vals[order(vals)]
  cols <- rep(base_palette, length.out = length(vals))
  names(cols) <- vals
  cols
}

# Build regular paint grid in meters using EPSG:3857,
# then transform back to WGS84 for display.
# grid_n = approximate number of cells across the largest district dimension
make_paint_grid <- function(district_sf, grid_n = 100) {
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
  
  grid_sf
}

# Arbitrary starter DFA assignment using nearest random seeds
make_start_assignment <- function(grid_sf, district_sf, n_dfa = 5, seed = 1) {
  set.seed(seed)
  
  pts <- st_sample(district_sf, size = n_dfa, exact = TRUE)
  
  pts_sf <- st_sf(
    dfa_name = paste("DFA", seq_len(n_dfa)),
    geometry = pts,
    crs = st_crs(district_sf)
  )
  
  cent <- suppressWarnings(st_centroid(grid_sf))
  idx <- st_nearest_feature(cent, pts_sf)
  
  list(
    assignments = pts_sf$dfa_name[idx],
    seeds_sf = pts_sf
  )
}

build_dfa_polygons_from_assignments <- function(grid_sf, assignments, district_sf) {
  stopifnot(length(assignments) == nrow(grid_sf))
  
  out <- grid_sf |>
    mutate(dfa_name = assignments) |>
    select(cell_id, centroid_lon, centroid_lat, dfa_name, geometry) |>
    group_by(dfa_name) |>
    summarise(geometry = st_union(geometry), .groups = "drop")
  
  out <- safe_make_valid(out)
  out <- suppressWarnings(st_intersection(out, district_sf))
  out <- safe_make_valid(out)
  out$geometry <- st_cast(out$geometry, "MULTIPOLYGON", warn = FALSE)
  
  out |>
    select(dfa_name, geometry)
}

build_saved_dfa_sf <- function(grid_sf, assignments, district_sf) {
  build_dfa_polygons_from_assignments(
    grid_sf = grid_sf,
    assignments = assignments,
    district_sf = district_sf
  )
}

# =========================================================
# Load districts
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

required_cols <- c("district_name")
missing_cols <- setdiff(required_cols, names(districts_shp))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "districts_shp is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  )
}

district_choices <- sort(unique(as.character(districts_shp$district_name)))

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
        padding: 10px 12px;
      }
      #app_row {
        display: flex;
        gap: 12px;
        height: calc(100vh - 20px);
      }
      #sidebar {
        width: 390px;
        min-width: 390px;
        background: #fafafa;
        border: 1px solid #ddd;
        border-radius: 8px;
        padding: 12px;
        overflow-y: auto;
      }
      #mapwrap {
        flex: 1;
        min-width: 0;
        position: relative;
      }
      #paint_map {
        width: 100%;
        height: 100%;
        min-height: 750px;
        border: 1px solid #ddd;
        border-radius: 8px;
        background: #d9d9d9;
      }
      .leaflet-container {
        background: #d9d9d9;
      }
      .small-note {
        font-size: 12px;
        color: #555;
        line-height: 1.35;
      }
      .legend-row {
        display: flex;
        align-items: center;
        margin-bottom: 6px;
      }
      .legend-swatch {
        width: 16px;
        height: 16px;
        border-radius: 3px;
        margin-right: 8px;
        border: 1px solid rgba(0,0,0,0.25);
      }
      .hint-box {
        margin-top: 8px;
        padding: 8px;
        background: #f3f7fb;
        border-radius: 6px;
        font-size: 12px;
      }
      pre {
        white-space: pre-wrap;
        word-break: break-word;
        max-height: 260px;
        overflow-y: auto;
      }
    ")),
    tags$script(HTML("
      window.paintApp = {
        map: null,
        districtLayer: null,
        gridLayer: null,
        savedLayer: null,
        brushPreview: null,
        isPainting: false,
        assignments: {},
        initialAssignments: {},
        dfaColors: {},
        cellLayers: {},
        centroids: {},
        neighbors: {},
        edgeCells: {},

        currentDfa: function() {
          var el = document.getElementById('active_dfa');
          return el ? el.value : null;
        },

        currentBrushSize: function() {
          var el = document.getElementById('brush_size');
          if (!el) return 300;
          var v = parseFloat(el.value);
          if (isNaN(v)) return 300;
          return v;
        },

        ensureMap: function() {
          if (window.paintApp.map) return;

          window.paintApp.map = L.map('paint_map', {
            zoomSnap: 0.25,
            preferCanvas: true
          });

          L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
            maxZoom: 20,
            attribution: '&copy; OpenStreetMap contributors'
          }).addTo(window.paintApp.map);

          window.paintApp.brushPreview = L.circle([0, 0], {
            radius: window.paintApp.currentBrushSize(),
            color: '#222222',
            weight: 1,
            opacity: 0.7,
            fillOpacity: 0.05,
            interactive: false
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

          window.paintApp.map.on('mousedown', function(e) {
            if (!window.paintApp.gridLayer) return;
            window.paintApp.isPainting = true;
            window.paintApp.map.dragging.disable();
            window.paintApp.paintAtLatLng(e.latlng);
          });

          document.addEventListener('mouseup', function() {
            if (window.paintApp.isPainting) {
              window.paintApp.isPainting = false;
              window.paintApp.map.dragging.enable();
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

        styleForFeature: function(feature) {
          var id = String(feature.properties.cell_id);
          var dfa = window.paintApp.assignments[id];
          var color = window.paintApp.dfaColors[dfa] || '#999999';
          var isBoundary = window.paintApp.isBoundaryCell(id);

          return {
            stroke: false,
            fillColor: color,
            fillOpacity: isBoundary ? 0.45 : 0.20
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
          var dfa = window.paintApp.currentDfa();
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

        loadScene: function(msg) {
          window.paintApp.ensureMap();
          window.paintApp.clearScene();

          window.paintApp.initialAssignments = JSON.parse(JSON.stringify(msg.initialAssignments));
          window.paintApp.assignments = JSON.parse(JSON.stringify(msg.initialAssignments));
          window.paintApp.dfaColors = msg.dfaColors || {};
          window.paintApp.neighbors = msg.neighbors || {};
          window.paintApp.edgeCells = msg.edgeCells || {};

          var districtGeo = (typeof msg.districtGeojson === 'string') ? JSON.parse(msg.districtGeojson) : msg.districtGeojson;
          var gridGeo = (typeof msg.gridGeojson === 'string') ? JSON.parse(msg.gridGeojson) : msg.gridGeojson;

          window.paintApp.districtLayer = L.geoJSON(districtGeo, {
            style: {
              color: '#000000',
              weight: 3,
              fill: false,
              opacity: 1
            }
          }).addTo(window.paintApp.map);

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
              var color = window.paintApp.dfaColors[nm] || '#000000';
              return {
                color: color,
                weight: 4,
                opacity: 1,
                dashArray: '8,6',
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
      }
    "))
  ),
  
  div(
    id = "app_row",
    
    div(
      id = "sidebar",
      h3("DFA paint editor"),
      p(
        class = "small-note",
        "Select a district. The app creates 5 arbitrary starter DFA areas, converts them to a grid-based ownership surface, and lets you repaint cells with a circular brush. Boundary cells update dynamically as painting changes adjacency."
      ),
      
      selectInput(
        "district_select",
        "District",
        choices = district_choices,
        selected = district_choices[1]
      ),
      
      numericInput(
        "grid_n",
        "Grid resolution (cells across largest district dimension)",
        value = default_grid_n,
        min = 20,
        max = 400,
        step = 10
      ),
      
      uiOutput("dfa_selector_ui"),
      
      sliderInput(
        "brush_size",
        "Brush size (meters)",
        min = min_brush_m,
        max = max_brush_m,
        value = default_brush_m,
        step = brush_step_m
      ),
      
      actionButton("reset_btn", "Reset to starting DFA layout"),
      actionButton("save_btn", "Save to multipolygon"),
      
      div(
        class = "hint-box",
        HTML(
          paste0(
            "<b>How to paint</b><br>",
            "Click once to repaint all cells within the circular brush, or hold the mouse down and move to paint continuously.<br><br>",
            "<b>Visual style</b><br>",
            "Interior cells are lighter. Boundary cells, including edges between different DFA areas, are darker and update live as you paint."
          )
        )
      ),
      
      tags$hr(),
      h4("DFA colors"),
      uiOutput("legend_ui"),
      
      tags$hr(),
      strong("Status"),
      verbatimTextOutput("status_text"),
      
      tags$hr(),
      strong("Saved sf preview"),
      verbatimTextOutput("saved_preview")
    ),
    
    div(
      id = "mapwrap",
      tags$div(id = "paint_map")
    )
  )
)

# =========================================================
# Server
# =========================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    district_sf = NULL,
    start_dfa_sf = NULL,
    grid_sf = NULL,
    initial_assignments = NULL,
    current_assignments = NULL,
    saved_dfa_sf = NULL,
    dfa_colors = NULL,
    neighbors_list = NULL,
    edge_list = NULL,
    status = "Choose a district."
  )
  
  selected_scene <- reactive({
    req(input$district_select, input$grid_n)
    
    district_sf <- districts_shp |>
      filter(district_name == input$district_select) |>
      select(admin_id, district_name, region_id, region_name, zone_id, zone_name, geometry)
    
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
    
    grid_sf <- make_paint_grid(district_sf, grid_n = input$grid_n)
    req(nrow(grid_sf) > 0)
    
    district_seed <- sum(utf8ToInt(input$district_select))
    
    start_info <- make_start_assignment(
      grid_sf = grid_sf,
      district_sf = district_sf,
      n_dfa = n_start_dfas,
      seed = district_seed
    )
    
    initial_assignments <- start_info$assignments
    
    start_dfa_sf <- build_dfa_polygons_from_assignments(
      grid_sf = grid_sf,
      assignments = initial_assignments,
      district_sf = district_sf
    )
    
    dfa_colors <- make_palette_for_values(sort(unique(initial_assignments)))
    
    # neighbors for dynamic boundary styling
    touch_list <- st_touches(grid_sf)
    neighbors_list <- lapply(touch_list, as.integer)
    names(neighbors_list) <- as.character(grid_sf$cell_id)
    
    # cells touching district boundary
    edge_flag <- lengths(st_intersects(grid_sf, st_boundary(district_sf))) > 0
    edge_list <- as.list(edge_flag)
    names(edge_list) <- as.character(grid_sf$cell_id)
    
    list(
      district_sf = district_sf,
      start_dfa_sf = start_dfa_sf,
      grid_sf = grid_sf,
      initial_assignments = initial_assignments,
      dfa_colors = dfa_colors,
      neighbors_list = neighbors_list,
      edge_list = edge_list
    )
  })
  
  observeEvent(selected_scene(), {
    sc <- selected_scene()
    
    rv$district_sf <- sc$district_sf
    rv$start_dfa_sf <- sc$start_dfa_sf
    rv$grid_sf <- sc$grid_sf
    rv$initial_assignments <- sc$initial_assignments
    rv$current_assignments <- sc$initial_assignments
    rv$saved_dfa_sf <- NULL
    rv$dfa_colors <- sc$dfa_colors
    rv$neighbors_list <- sc$neighbors_list
    rv$edge_list <- sc$edge_list
    rv$status <- paste0(
      "Loaded district: ", input$district_select,
      ". Grid cells: ", nrow(sc$grid_sf),
      ". Grid resolution setting: ", input$grid_n
    )
    
    district_dfas <- sort(unique(sc$initial_assignments))
    
    updateSelectInput(
      session,
      "active_dfa",
      choices = district_dfas,
      selected = district_dfas[1]
    )
    
    init_named <- setNames(
      as.list(sc$initial_assignments),
      as.character(sc$grid_sf$cell_id)
    )
    
    session$sendCustomMessage(
      "paint_load_scene",
      list(
        districtGeojson = as_geojson_text(sc$district_sf),
        gridGeojson = as_geojson_text(sc$grid_sf),
        initialAssignments = init_named,
        dfaColors = as.list(sc$dfa_colors),
        neighbors = sc$neighbors_list,
        edgeCells = sc$edge_list
      )
    )
  }, ignoreInit = FALSE)
  
  output$dfa_selector_ui <- renderUI({
    sc <- selected_scene()
    district_dfas <- sort(unique(sc$initial_assignments))
    
    selectInput(
      "active_dfa",
      "Active DFA paint class",
      choices = district_dfas,
      selected = district_dfas[1]
    )
  })
  
  output$legend_ui <- renderUI({
    req(rv$dfa_colors)
    tagList(
      lapply(names(rv$dfa_colors), function(nm) {
        div(
          class = "legend-row",
          div(class = "legend-swatch", style = paste0("background:", rv$dfa_colors[[nm]], ";")),
          span(nm)
        )
      })
    )
  })
  
  observeEvent(input$paint_map_ready, {
    rv$status <- paste0(
      "Map ready for district: ", input$district_select,
      ". Brush size: ", input$brush_size, " m.",
      " Grid resolution setting: ", input$grid_n,
      ". Grid cells: ", if (!is.null(rv$grid_sf)) nrow(rv$grid_sf) else NA
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$reset_btn, {
    req(!is.null(rv$initial_assignments))
    rv$current_assignments <- rv$initial_assignments
    rv$saved_dfa_sf <- NULL
    rv$status <- paste0("Reset to starting DFA layout for district: ", input$district_select)
    session$sendCustomMessage("paint_reset", list())
  })
  
  observeEvent(input$save_btn, {
    session$sendCustomMessage("paint_request_assignments", list())
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
    
    saved <- tryCatch(
      build_saved_dfa_sf(
        grid_sf = rv$grid_sf,
        assignments = ordered_assignments,
        district_sf = rv$district_sf
      ),
      error = function(e) e
    )
    
    if (inherits(saved, "error")) {
      rv$status <- paste("Save failed:", saved$message)
      showNotification(
        paste("Save failed:", saved$message),
        type = "error",
        duration = 8
      )
      return()
    }
    
    rv$saved_dfa_sf <- saved
    
    session$sendCustomMessage(
      "paint_show_saved",
      list(
        geojson = as_geojson_text(saved)
      )
    )
    
    rv$status <- sprintf(
      "Saved district %s to %s DFA MULTIPOLYGON geometries.",
      input$district_select, nrow(saved)
    )
  }, ignoreInit = TRUE)
  
  output$status_text <- renderText({
    if (is.null(rv$current_assignments)) return(rv$status)
    
    counts <- table(sort(rv$current_assignments))
    paste0(
      rv$status, "\n\n",
      "Current grid cell counts by DFA:\n",
      paste(names(counts), counts, sep = ": ", collapse = "\n")
    )
  })
  
  output$saved_preview <- renderPrint({
    if (is.null(rv$saved_dfa_sf)) {
      cat("No saved geometry yet. Click 'Save to multipolygon'.\n")
    } else {
      print(rv$saved_dfa_sf)
      cat("\nGeometry type:", paste(unique(as.character(st_geometry_type(rv$saved_dfa_sf))), collapse = ", "), "\n")
      cat("CRS:", st_crs(rv$saved_dfa_sf)$input, "\n")
    }
  })
}

shinyApp(ui, server)