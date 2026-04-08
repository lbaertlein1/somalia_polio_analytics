# app.R

library(shiny)
library(sf)
library(dplyr)
library(jsonlite)
library(geojsonsf)

# =========================================================
# Helpers
# =========================================================

make_demo_district <- function() {
  coords <- matrix(
    c(
      45.280, 2.040,
      45.325, 2.085,
      45.390, 2.075,
      45.430, 2.120,
      45.405, 2.180,
      45.345, 2.210,
      45.295, 2.185,
      45.255, 2.125,
      45.270, 2.075,
      45.280, 2.040
    ),
    byrow = TRUE,
    ncol = 2
  )
  
  st_sf(
    district_name = "Demo Health District",
    geometry = st_sfc(st_polygon(list(coords)), crs = 4326)
  )
}

# For reliability, this version keeps full grid cells whose centroid falls inside the district.
# This avoids fragile geometry collection issues during exact clipping.
make_paint_grid <- function(district_sf, cellsize = 0.0035) {
  raw_grid <- st_make_grid(
    district_sf,
    cellsize = cellsize,
    what = "polygons",
    square = TRUE
  )
  
  grid_sf <- st_sf(
    cell_id = seq_along(raw_grid),
    geometry = raw_grid,
    crs = st_crs(district_sf)
  )
  
  cent <- suppressWarnings(st_centroid(grid_sf))
  inside <- lengths(st_within(cent, district_sf)) > 0
  
  grid_sf |>
    filter(inside) |>
    mutate(cell_id = seq_len(n()))
}

make_seed_points <- function() {
  st_as_sf(
    data.frame(
      dfa_name = c("DFA 1", "DFA 2", "DFA 3", "DFA 4"),
      lon = c(45.295, 45.385, 45.340, 45.285),
      lat = c(2.075, 2.105, 2.185, 2.155)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )
}

initial_assignment_from_seeds <- function(grid_sf, seed_sf) {
  centroids <- suppressWarnings(st_centroid(grid_sf))
  idx <- st_nearest_feature(centroids, seed_sf)
  seed_sf$dfa_name[idx]
}

build_saved_dfa_sf <- function(grid_sf, assignments, district_sf) {
  stopifnot(length(assignments) == nrow(grid_sf))
  
  out <- grid_sf |>
    mutate(dfa_name = assignments) |>
    group_by(dfa_name) |>
    summarise(geometry = st_union(geometry), .groups = "drop")
  
  out$geometry <- st_sfc(
    lapply(out$geometry, function(g) st_make_valid(g)),
    crs = st_crs(grid_sf)
  )
  
  out <- suppressWarnings(st_intersection(out, district_sf))
  
  out$geometry <- st_sfc(
    lapply(out$geometry, function(g) st_make_valid(g)),
    crs = st_crs(grid_sf)
  )
  
  out$geometry <- st_cast(out$geometry, "MULTIPOLYGON", warn = FALSE)
  
  out |>
    select(dfa_name, geometry)
}

# =========================================================
# Demo data
# =========================================================

district_sf <- make_demo_district()
grid_sf <- make_paint_grid(district_sf, cellsize = 0.0035)
seed_sf <- make_seed_points()

dfa_colors <- c(
  "DFA 1" = "#E41A1C",
  "DFA 2" = "#377EB8",
  "DFA 3" = "#4DAF4A",
  "DFA 4" = "#984EA3"
)

initial_assignments <- initial_assignment_from_seeds(grid_sf, seed_sf)

# GeoJSON strings for browser-side Leaflet
district_geojson <- geojsonsf::sf_geojson(district_sf)
grid_geojson <- geojsonsf::sf_geojson(grid_sf)

# Named list for JS
initial_assignments_named <- setNames(
  as.list(initial_assignments),
  as.character(grid_sf$cell_id)
)

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
        width: 340px;
        min-width: 340px;
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
        min-height: 700px;
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
    
    tags$script(HTML(sprintf(
      "
window.paintApp = {
  map: null,
  districtLayer: null,
  gridLayer: null,
  savedLayer: null,
  isPainting: false,
  isLoaded: false,
  initialAssignments: %s,
  assignments: %s,
  dfaColors: %s,
  cellLayers: {},

  currentDfa: function() {
    var el = document.getElementById('active_dfa');
    return el ? el.value : 'DFA 1';
  },

  styleForFeature: function(feature) {
    var id = String(feature.properties.cell_id);
    var dfa = window.paintApp.assignments[id];
    var color = window.paintApp.dfaColors[dfa] || '#999999';
    return {
      color: color,
      weight: 0.4,
      opacity: 0.8,
      fillColor: color,
      fillOpacity: 0.45
    };
  },

  paintLayer: function(layer) {
    if (!layer || !layer.feature || !layer.feature.properties) return;
    var id = String(layer.feature.properties.cell_id);
    var dfa = window.paintApp.currentDfa();
    window.paintApp.assignments[id] = dfa;
    layer.setStyle(window.paintApp.styleForFeature(layer.feature));

    Shiny.setInputValue(
      'paint_last_cell',
      { cell_id: id, dfa_name: dfa, nonce: Date.now() },
      { priority: 'event' }
    );
  },

  refreshAllStyles: function() {
    if (!window.paintApp.gridLayer) return;
    window.paintApp.gridLayer.eachLayer(function(layer) {
      layer.setStyle(window.paintApp.styleForFeature(layer.feature));
    });
  },

  resetAssignments: function() {
    window.paintApp.assignments = JSON.parse(JSON.stringify(window.paintApp.initialAssignments));
    window.paintApp.refreshAllStyles();
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
  },

  clearSaved: function() {
    if (window.paintApp.map && window.paintApp.savedLayer) {
      window.paintApp.map.removeLayer(window.paintApp.savedLayer);
      window.paintApp.savedLayer = null;
    }
  },

  initMap: function() {
    if (window.paintApp.isLoaded) return;
    if (typeof L === 'undefined') return;

    var mapDiv = document.getElementById('paint_map');
    if (!mapDiv) return;

    var districtGeo = JSON.parse(%s);
    var gridGeo = JSON.parse(%s);

    var map = L.map('paint_map', {
      zoomSnap: 0.25
    });

    window.paintApp.map = map;

    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
      maxZoom: 20,
      attribution: '&copy; OpenStreetMap contributors'
    }).addTo(map);

    window.paintApp.districtLayer = L.geoJSON(districtGeo, {
      style: {
        color: '#000000',
        weight: 3,
        fill: false,
        opacity: 1
      }
    }).addTo(map);

    window.paintApp.gridLayer = L.geoJSON(gridGeo, {
      style: function(feature) {
        return window.paintApp.styleForFeature(feature);
      },
      onEachFeature: function(feature, layer) {
        var id = String(feature.properties.cell_id);
        window.paintApp.cellLayers[id] = layer;

        layer.on('mousedown', function(e) {
          window.paintApp.isPainting = true;
          map.dragging.disable();
          window.paintApp.paintLayer(layer);
          if (e.originalEvent) {
            e.originalEvent.preventDefault();
          }
        });

        layer.on('mouseover', function() {
          if (window.paintApp.isPainting) {
            window.paintApp.paintLayer(layer);
          }
        });

        layer.on('click', function() {
          if (!window.paintApp.isPainting) {
            window.paintApp.paintLayer(layer);
          }
        });
      }
    }).addTo(map);

    document.addEventListener('mouseup', function() {
      if (window.paintApp.isPainting) {
        window.paintApp.isPainting = false;
        map.dragging.enable();
      }
    });

    map.fitBounds(window.paintApp.districtLayer.getBounds(), { padding: [10, 10] });

    setTimeout(function() { map.invalidateSize(); }, 200);
    setTimeout(function() { map.invalidateSize(); }, 800);

    window.paintApp.isLoaded = true;

    Shiny.setInputValue('paint_map_ready', Date.now(), { priority: 'event' });
  }
};

document.addEventListener('DOMContentLoaded', function() {
  setTimeout(function() {
    window.paintApp.initMap();
  }, 100);
});

if (window.Shiny) {
  Shiny.addCustomMessageHandler('paint_reset', function(msg) {
    window.paintApp.resetAssignments();
    window.paintApp.clearSaved();
  });

  Shiny.addCustomMessageHandler('paint_request_assignments', function(msg) {
    window.paintApp.emitAssignments();
  });

  Shiny.addCustomMessageHandler('paint_show_saved', function(msg) {
    window.paintApp.showSaved(msg.geojson);
  });

  Shiny.addCustomMessageHandler('paint_clear_saved', function(msg) {
    window.paintApp.clearSaved();
  });
}
",
toJSON(initial_assignments_named, auto_unbox = TRUE),
toJSON(initial_assignments_named, auto_unbox = TRUE),
toJSON(as.list(dfa_colors), auto_unbox = TRUE),
toJSON(district_geojson, auto_unbox = TRUE),
toJSON(grid_geojson, auto_unbox = TRUE)
    )))
  ),

div(
  id = "app_row",
  
  div(
    id = "sidebar",
    h3("Paint-based DFA editor"),
    p(
      class = "small-note",
      "This proof of concept uses a grid as a paint surface on top of a real Leaflet basemap. Each cell belongs to exactly one DFA at a time. Painting over a cell reassigns it."
    ),
    
    selectInput(
      "active_dfa",
      "Active DFA paint class",
      choices = names(dfa_colors),
      selected = "DFA 1"
    ),
    
    actionButton("reset_btn", "Reset to initial assignment"),
    actionButton("save_btn", "Save current painted result"),
    
    div(
      class = "hint-box",
      HTML("<b>How to paint</b><br>Click a cell to repaint it, or hold the mouse down and drag across cells to paint multiple areas.")
    ),
    
    tags$hr(),
    h4("DFA colors"),
    
    lapply(names(dfa_colors), function(nm) {
      div(
        class = "legend-row",
        div(class = "legend-swatch", style = paste0("background:", dfa_colors[[nm]], ";")),
        span(nm)
      )
    }),
    
    tags$hr(),
    strong("Status"),
    verbatimTextOutput("status_text"),
    
    tags$hr(),
    strong("Saved sf preview"),
    p(
      class = "small-note",
      "On save, painted cells are dissolved into one MULTIPOLYGON geometry per DFA."
    ),
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
  
  current_assignments <- reactiveVal(initial_assignments)
  saved_dfa_sf <- reactiveVal(NULL)
  status_rv <- reactiveVal("Waiting for map to initialize.")
  
  observeEvent(input$paint_map_ready, {
    status_rv("Map ready. Select a DFA and click or drag to paint.")
  }, ignoreInit = TRUE)
  
  observeEvent(input$paint_last_cell, {
    info <- input$paint_last_cell
    req(!is.null(info$cell_id), !is.null(info$dfa_name))
    
    cell_id <- suppressWarnings(as.integer(info$cell_id))
    
    if (!is.na(cell_id) && cell_id >= 1 && cell_id <= length(current_assignments())) {
      x <- current_assignments()
      x[cell_id] <- info$dfa_name
      current_assignments(x)
      status_rv(sprintf("Painted cell %s as %s.", cell_id, info$dfa_name))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$reset_btn, {
    current_assignments(initial_assignments)
    saved_dfa_sf(NULL)
    session$sendCustomMessage("paint_reset", list())
    status_rv("Reset complete. Restored initial DFA assignment and cleared saved boundaries.")
  })
  
  observeEvent(input$save_btn, {
    session$sendCustomMessage("paint_request_assignments", list())
  })
  
  observeEvent(input$paint_assignments, {
    payload <- input$paint_assignments
    req(!is.null(payload$assignments))
    
    js_assignments <- payload$assignments
    
    ordered_assignments <- vapply(
      as.character(grid_sf$cell_id),
      function(id) {
        val <- js_assignments[[id]]
        if (is.null(val) || !nzchar(val)) {
          initial_assignments[as.integer(id)]
        } else {
          as.character(val)
        }
      },
      character(1)
    )
    
    current_assignments(ordered_assignments)
    
    saved <- tryCatch(
      build_saved_dfa_sf(
        grid_sf = grid_sf,
        assignments = ordered_assignments,
        district_sf = district_sf
      ),
      error = function(e) e
    )
    
    if (inherits(saved, "error")) {
      status_rv(paste("Save failed:", saved$message))
      showNotification(
        paste("Save failed:", saved$message),
        type = "error",
        duration = 8
      )
      return()
    }
    
    saved_dfa_sf(saved)
    
    session$sendCustomMessage(
      "paint_show_saved",
      list(geojson = geojsonsf::sf_geojson(saved))
    )
    
    status_rv(
      sprintf(
        "Saved %s DFA geometries. Painted cells were dissolved into one MULTIPOLYGON feature per DFA.",
        nrow(saved)
      )
    )
  }, ignoreInit = TRUE)
  
  output$status_text <- renderText({
    counts <- table(factor(current_assignments(), levels = names(dfa_colors)))
    paste0(
      status_rv(), "\n\n",
      "Current cell counts by DFA:\n",
      paste(names(counts), counts, sep = ": ", collapse = "\n")
    )
  })
  
  output$saved_preview <- renderPrint({
    x <- saved_dfa_sf()
    if (is.null(x)) {
      cat("No saved DFA geometry yet. Click 'Save current painted result'.\n")
    } else {
      print(x)
      cat("\nGeometry type:", paste(unique(as.character(st_geometry_type(x))), collapse = ", "), "\n")
      cat("CRS:", st_crs(x)$input, "\n")
    }
  })
}

shinyApp(ui, server)