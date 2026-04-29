# =============================================================================
# mod_orientation_tab.R
# =============================================================================

orientationTabUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    
    column(
      width = 2,
      
      div(class = 'rightbar-title', style = 'margin-top: 4px;', 'Orientation'),
      
      tags$p(
        style = 'font-size: 12px; color: #475569; line-height: 1.6;',
        'Get familiar with the map before moving to facility mapping.'
      ),
      
      tags$ul(
        style = 'font-size: 12px; color: #475569; padding-left: 16px; line-height: 1.8;',
        tags$li('Click anywhere on the map to drop a landmark.'),
        tags$li('Drag a pin to reposition it.'),
        tags$li('Rename landmarks in the table on the right.')
      ),
      
      tags$hr(style = 'margin: 10px 0;'),
      
      uiOutput(ns('landmark_count')),
      
      tags$hr(style = 'margin: 10px 0;'),
      
      actionButton(
        ns('clear_all'), 'Clear all landmarks',
        class = 'btn btn-default btn-sm',
        width = '100%',
        icon  = icon('trash')
      ),
      
      tags$hr(style = 'margin: 10px 0;'),
      
      actionButton(
        ns('submit_landmarks'), 'Submit Landmarks',
        class = 'btn btn-primary btn-sm',
        width = '100%',
        icon  = icon('check-circle')
      ),
      div(
        style = 'font-size: 11px; color: #64748b; margin-top: 5px; line-height: 1.4;',
        'Saves your landmarks to the database.'
      ),
      
      tags$hr(style = 'margin: 10px 0;'),
      
      tags$button(
        id      = ns('continue'),
        class   = 'btn btn-default btn-block',
        type    = 'button',
        style   = 'font-weight: 600; font-size: 13px; height: 36px; width: 100%;',
        onclick = paste0(
          "$('#main_tabs a[data-value=\"tab_health_facility_mapping\"]').tab('show');"
        ),
        'Continue \u2192'
      )
    ),
    
    column(
      width = 7,
      div(
        style = 'height: calc(100vh - 120px); position: relative;',
        leaflet::leafletOutput(ns('map'), width = '100%', height = '100%')
      )
    ),
    
    column(
      width = 3,
      div(class = 'rightbar-title', 'Landmarks'),
      div(
        style = 'font-size: 11px; color: #94a3b8; margin-bottom: 6px;',
        'Click a row to pan to that landmark. Edit the Name column to rename.'
      ),
      div(
        style = 'overflow-y: auto; height: calc(100vh - 170px);',
        rhandsontable::rHandsontableOutput(ns('landmark_table'), height = '100%')
      )
    )
  )
}


# =============================================================================
# Server
# =============================================================================

orientationTabServer <- function(
    id,
    zone,
    region,
    district,
    district_ready,
    submit_stage_fn  = NULL,
    save_snapshot_fn = NULL,   # kept for compatibility, no-op
    restore_r        = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    
    updating_table <- reactiveVal(FALSE)
    
    rv <- reactiveValues(
      landmarks = data.frame(
        landmark_id   = character(0),
        landmark_name = character(0),
        lat           = numeric(0),
        lon           = numeric(0),
        stringsAsFactors = FALSE
      )
    )
    
    selected_id <- reactiveVal(NULL)
    
    # ── District boundary reactive ────────────────────────────────────────────
    district_sf <- reactive({
      req(isTRUE(district_ready()))
      req(zone(), region(), district())
      
      sf <- districts_shp |>
        dplyr::filter(
          zone_name     == zone(),
          region_name   == region(),
          district_name == district()
        ) |>
        dplyr::summarise(
          admin_id      = dplyr::first(admin_id),
          district_name = dplyr::first(district_name),
          region_name   = dplyr::first(region_name),
          zone_name     = dplyr::first(zone_name),
          geometry      = sf::st_union(geometry),
          .groups       = 'drop'
        ) |>
        sf::st_as_sf() |>
        safe_make_valid()
      
      req(nrow(sf) >= 1)
      sf::st_transform(sf, 4326)
    })
    
    # ── Clear landmarks when district changes ─────────────────────────────────
    observeEvent(district(), {
      rv$landmarks <- data.frame(
        landmark_id   = character(0),
        landmark_name = character(0),
        lat           = numeric(0),
        lon           = numeric(0),
        stringsAsFactors = FALSE
      )
      selected_id(NULL)
    }, ignoreInit = TRUE)
    
    # ── Restore ───────────────────────────────────────────────────────────────
    observeEvent(restore_r(), {
      snap <- restore_r()
      if (is.null(snap) || is.null(snap$landmarks)) return()
      rv$landmarks <- snap$landmarks
      showNotification('Landmark state restored.', type = 'message', duration = 2)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # ── Base map ──────────────────────────────────────────────────────────────
    output$map <- leaflet::renderLeaflet({
      req(district_sf())
      
      bbox <- sf::st_bbox(district_sf())
      
      leaflet::leaflet(
        options = leaflet::leafletOptions(zoomSnap = 0.25)
      ) |>
        leaflet::addTiles(group = 'OpenStreetMap') |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery, group = 'ESRI Satellite'
        ) |>
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron, group = 'CARTO Light'
        ) |>
        leaflet::addLayersControl(
          baseGroups = c('OpenStreetMap', 'ESRI Satellite', 'CARTO Light'),
          options    = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::fitBounds(
          lng1 = bbox[['xmin']], lat1 = bbox[['ymin']],
          lng2 = bbox[['xmax']], lat2 = bbox[['ymax']]
        ) |>
        leaflet::addScaleBar(
          position = 'bottomright',
          options  = leaflet::scaleBarOptions(imperial = FALSE, maxWidth = 200)
        )
    })
    # Pre-render map while user is still on intro tab so the district boundary
    # proxy call lands on an already-rendered map when district_sf() resolves.
    outputOptions(output, 'map', suspendWhenHidden = FALSE)
    
    # ── District boundary ─────────────────────────────────────────────────────
    observe({
      req(district_sf())
      leaflet::leafletProxy('map', session = session) |>
        leaflet::clearGroup('district') |>
        leaflet::addPolygons(
          data    = district_sf(),
          group   = 'district',
          color   = '#334155',
          weight  = 2,
          fill    = FALSE,
          opacity = 1
        )
    })
    
    # ── Draw / redraw all landmark markers ────────────────────────────────────
    .redraw_markers <- function() {
      proxy <- leaflet::leafletProxy('map', session = session) |>
        leaflet::clearGroup('landmarks')
      
      lm <- rv$landmarks
      if (nrow(lm) == 0) return()
      
      sel <- selected_id()
      
      for (i in seq_len(nrow(lm))) {
        is_sel   <- !is.null(sel) && identical(lm$landmark_id[i], sel)
        icon_url <- if (is_sel) {
          'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png'
        } else {
          'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png'
        }
        
        proxy <- proxy |>
          leaflet::addMarkers(
            lng     = lm$lon[i],
            lat     = lm$lat[i],
            layerId = lm$landmark_id[i],
            group   = 'landmarks',
            icon    = leaflet::makeIcon(
              iconUrl    = icon_url,
              shadowUrl  = 'https://unpkg.com/leaflet@1.9.4/dist/images/marker-shadow.png',
              iconWidth  = 25, iconHeight  = 41,
              iconAnchorX = 12, iconAnchorY = 41
            ),
            options      = leaflet::markerOptions(draggable = TRUE, riseOnHover = TRUE),
            label        = lm$landmark_name[i],
            labelOptions = leaflet::labelOptions(
              noHide    = TRUE,
              direction = 'right',
              offset    = c(10, 0),
              textsize  = '11px',
              className = 'hf-tooltip'
            )
          )
      }
    }
    
    observe({
      rv$landmarks
      selected_id()
      .redraw_markers()
    })
    
    # ── Left-click on map → add landmark ──────────────────────────────────────
    observeEvent(input$map_click, {
      click <- input$map_click
      req(!is.null(click))
      
      new_id <- paste0('lm_', format(Sys.time(), '%Y%m%d%H%M%S'), '_', sample(1000:9999, 1))
      n      <- nrow(rv$landmarks) + 1L
      
      rv$landmarks <- rbind(rv$landmarks, data.frame(
        landmark_id   = new_id,
        landmark_name = paste('Landmark', n),
        lat           = as.numeric(click$lat),
        lon           = as.numeric(click$lng),
        stringsAsFactors = FALSE
      ))
      
      selected_id(new_id)
    })
    
    # ── Drag → update position ────────────────────────────────────────────────
    observeEvent(input$map_marker_dragend, {
      info <- input$map_marker_dragend
      req(!is.null(info$id))
      
      idx <- which(rv$landmarks$landmark_id == as.character(info$id))
      if (length(idx) != 1) return()
      
      rv$landmarks$lat[idx] <- as.numeric(info$lat)
      rv$landmarks$lon[idx] <- as.numeric(info$lng)
    })
    
    # ── Click pin → select in table ───────────────────────────────────────────
    observeEvent(input$map_marker_click, {
      info <- input$map_marker_click
      req(!is.null(info$id))
      selected_id(as.character(info$id))
    })
    
    # ── Clear all ─────────────────────────────────────────────────────────────
    observeEvent(input$clear_all, {
      rv$landmarks <- data.frame(
        landmark_id   = character(0),
        landmark_name = character(0),
        lat           = numeric(0),
        lon           = numeric(0),
        stringsAsFactors = FALSE
      )
      selected_id(NULL)
    })
    
    # ── Submit landmarks → DB ─────────────────────────────────────────────────
    observeEvent(input$submit_landmarks, {
      if (!is.null(submit_stage_fn)) {
        submit_stage_fn('landmarks', list(landmarks = rv$landmarks))
      } else {
        showNotification('Submit not configured.', type = 'warning', duration = 3)
      }
    }, ignoreInit = TRUE)
    
    # ── Landmark count display ────────────────────────────────────────────────
    output$landmark_count <- renderUI({
      n <- nrow(rv$landmarks)
      div(
        style = 'font-size: 12px; color: #0d9488; font-weight: 600;',
        if (n == 0) {
          span(style = 'color: #94a3b8;', 'No landmarks added yet')
        } else {
          paste0(n, ' landmark', if (n != 1) 's' else '', ' added')
        }
      )
    })
    
    # ── Landmark table ────────────────────────────────────────────────────────
    output$landmark_table <- rhandsontable::renderRHandsontable({
      updating_table(TRUE)
      on.exit(updating_table(FALSE))
      lm <- rv$landmarks
      if (nrow(lm) == 0) return(NULL)
      
      sel_id      <- selected_id()
      sel_row     <- if (!is.null(sel_id)) which(lm$landmark_id == sel_id) else integer(0)
      sel_row_js  <- if (length(sel_row) == 1) sel_row - 1L else -1L
      
      display_df <- data.frame(
        landmark_id_internal = lm$landmark_id,
        Name                 = lm$landmark_name,
        Delete               = seq_len(nrow(lm)),
        stringsAsFactors     = FALSE
      )
      
      highlight_renderer <- htmlwidgets::JS(sprintf("
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          if (row === %d) {
            td.style.background = '#FFF176';
            td.style.fontWeight = '600';
          } else {
            td.style.background = '';
            td.style.fontWeight = '';
          }
        }
      ", sel_row_js))
      
      blank_renderer <- htmlwidgets::JS("
        function(instance, td, row, col, prop, value, cellProperties) {
          td.innerHTML = '';
          td.style.border = 'none';
          td.style.background = 'transparent';
          td.style.padding = '0px';
        }
      ")
      
      delete_renderer <- htmlwidgets::JS(sprintf("
        function(instance, td, row, col, prop, value, cellProperties) {
          td.innerHTML = '<button style=\"font-size:11px;padding:1px 6px;cursor:pointer;' +
            'border:1px solid #ef4444;border-radius:3px;background:#fff;color:#ef4444;\"' +
            ' onclick=\"Shiny.setInputValue(\\'%s\\', ' + value + ', {priority:\\'event\\'})\">✕</button>';
          if (row === %d) { td.style.background = '#FFF176'; }
          else { td.style.background = ''; }
        }
      ", session$ns('delete_row'), sel_row_js))
      
      rhandsontable::rhandsontable(
        display_df,
        rowHeaders = NULL,
        stretchH   = 'all',
        useTypes   = FALSE
      ) |>
        rhandsontable::hot_col('landmark_id_internal', width = 1,   readOnly = TRUE,  renderer = blank_renderer) |>
        rhandsontable::hot_col('Name',                 width = 140, readOnly = FALSE, renderer = highlight_renderer) |>
        rhandsontable::hot_col('Delete',               width = 44,  readOnly = TRUE,  renderer = delete_renderer) |>
        rhandsontable::hot_table(
          highlightRow   = TRUE,
          columnSorting  = FALSE,
          afterSelection = htmlwidgets::JS(sprintf("
            function(r, c, r2, c2) {
              Shiny.setInputValue('%s', r + 1, {priority: 'event'});
            }
          ", session$ns('selected_row')))
        ) |>
        rhandsontable::hot_rows(rowHeights = 28) |>
        rhandsontable::hot_cols(manualColumnResize = TRUE)
    })
    
    # ── Table row click → select + pan map ───────────────────────────────────
    observeEvent(input$selected_row, ignoreInit = TRUE, {
      idx <- as.integer(input$selected_row)
      lm  <- rv$landmarks
      req(!is.na(idx), idx >= 1, idx <= nrow(lm))
      
      sel_id <- lm$landmark_id[idx]
      selected_id(sel_id)
      
      leaflet::leafletProxy('map', session = session) |>
        leaflet::setView(
          lng  = lm$lon[idx],
          lat  = lm$lat[idx],
          zoom = 13
        )
    })
    
    # ── Table name edit → update rv ───────────────────────────────────────────
    observeEvent(input$landmark_table, ignoreInit = TRUE, ignoreNULL = TRUE, {
      if (isTRUE(updating_table())) return()
      
      edited <- rhandsontable::hot_to_r(input$landmark_table)
      req(!is.null(edited), nrow(edited) == nrow(rv$landmarks))
      
      if (!identical(edited$Name, rv$landmarks$landmark_name)) {
        rv$landmarks$landmark_name <- edited$Name
      }
    })
    
    # ── Delete row ────────────────────────────────────────────────────────────
    observeEvent(input$delete_row, ignoreInit = TRUE, ignoreNULL = TRUE, {
      idx <- as.integer(input$delete_row)
      lm  <- rv$landmarks
      req(!is.na(idx), idx >= 1, idx <= nrow(lm))
      
      del_id       <- lm$landmark_id[idx]
      rv$landmarks <- lm[-idx, , drop = FALSE]
      
      if (!is.null(selected_id()) && identical(selected_id(), del_id)) {
        selected_id(NULL)
      }
    })
    
    # ── Return ────────────────────────────────────────────────────────────────
    list(
      landmarks_r           = reactive(rv$landmarks),
      restore_from_snapshot = function(snap) {
        if (!is.null(snap$landmarks)) rv$landmarks <- snap$landmarks
      }
    )
  })
}
