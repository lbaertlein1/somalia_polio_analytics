# =============================================================================
# mod_microplan_tab.R
# Health Area Microplanning Preparation
# =============================================================================

TEAMS_PER_CHILD       <- 400L  # 1 team per N children u5
SUPERVISORS_PER_TEAMS <-   5L  # 1 supervisor per N vaccination teams

microplanTabUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    
    # ── Left sidebar ──────────────────────────────────────────────────────────
    column(
      width = 2,
      
      div(class = 'rightbar-title', style = 'margin-top: 4px;',
          'Microplan Preparation'),
      
      tags$p(
        style = 'font-size: 12px; color: #475569; line-height: 1.6;',
        'Click any health area on the map to enter planning data.'
      ),
      
      tags$ul(
        style = 'font-size: 12px; color: #475569; padding-left: 16px; line-height: 1.8;',
        tags$li('Suggested values are pre-filled from WorldPop estimates.'),
        tags$li('Edit any field as needed.'),
        tags$li('Areas turn teal when marked complete.')
      ),
      
      tags$hr(style = 'margin: 10px 0;'),
      
      uiOutput(ns('progress_summary')),
      
      tags$hr(style = 'margin: 10px 0;'),
      
      downloadButton(
        ns('download_data'), 'Download data',
        class = 'btn btn-default btn-sm',
        style = 'width: 100%;'
      )
    ),
    
    # ── Map panel ─────────────────────────────────────────────────────────────
    column(
      width = 10,
      div(
        style = 'position: relative; height: calc(100vh - 120px);',
        leaflet::leafletOutput(ns('map'), width = '100%', height = '100%'),
        uiOutput(ns('no_data_overlay'))
      )
    )
  )
}


# =============================================================================
# Server
# =============================================================================

microplanTabServer <- function(
    id,
    zone,
    region,
    district,
    district_ready,
    saved_dfa_sf_r,
    pop_table_r,
    facility_data_r  = reactive(NULL),
    save_snapshot_fn = NULL,
    restore_r        = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    
    rv           <- reactiveValues(planning_data = list())
    editing_area <- reactiveVal(NULL)
    
    # ── Clear on district change ──────────────────────────────────────────────
    observeEvent(district(), {
      rv$planning_data <- list()
      editing_area(NULL)
    }, ignoreInit = TRUE)
    
    # ── Session restore ───────────────────────────────────────────────────────
    observeEvent(restore_r(), {
      snap <- restore_r()
      if (is.null(snap) || is.null(snap$planning_data)) return()
      rv$planning_data <- snap$planning_data
      showNotification('Planning data restored.', type = 'message', duration = 2)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # ── Helper: initialise or retrieve area data ──────────────────────────────
    .get_area_data <- function(area_name) {
      if (!is.null(rv$planning_data[[area_name]])) return(rv$planning_data[[area_name]])
      
      pt     <- tryCatch(pop_table_r(), error = function(e) NULL)
      u5_pop <- 0
      if (!is.null(pt) && nrow(pt) > 0) {
        m <- pt[pt$area_name == area_name, , drop = FALSE]
        if (nrow(m) > 0) u5_pop <- as.numeric(m$est_u5_pop[1])
      }
      n_teams <- max(1L, as.integer(ceiling(u5_pop / TEAMS_PER_CHILD)))
      n_sup   <- max(1L, as.integer(ceiling(n_teams / SUPERVISORS_PER_TEAMS)))
      
      list(
        u5_pop        = u5_pop,
        n_teams       = n_teams,
        n_supervisors = n_sup,
        supervisors   = lapply(seq_len(n_sup),
                               function(i) list(name = '', role = '', phone = '', email = '')),
        complete      = FALSE,
        notes         = ''
      )
    }
    
    .is_special <- function(area_name) area_name %in% extra_dfa_names
    
    # ── Progress summary ──────────────────────────────────────────────────────
    output$progress_summary <- renderUI({
      sf_obj <- tryCatch(saved_dfa_sf_r(), error = function(e) NULL)
      if (is.null(sf_obj) || nrow(sf_obj) == 0) {
        return(div(style = 'font-size: 12px; color: #94a3b8;', 'No saved areas yet.'))
      }
      areas   <- setdiff(unique(sf_obj$dfa_name), extra_dfa_names)
      n_total <- length(areas)
      n_done  <- sum(vapply(areas, function(a) isTRUE(rv$planning_data[[a]]$complete), logical(1)))
      color   <- if (n_done == n_total && n_total > 0) '#0d9488' else '#64748b'
      div(
        div(style = paste0('font-size:22px;font-weight:700;color:', color, ';line-height:1;'),
            paste0(n_done, ' / ', n_total)),
        div(style = 'font-size:11px;color:#94a3b8;margin-top:2px;', 'areas complete')
      )
    })
    
    # ── No-data overlay ───────────────────────────────────────────────────────
    output$no_data_overlay <- renderUI({
      sf_obj <- tryCatch(saved_dfa_sf_r(), error = function(e) NULL)
      if (!is.null(sf_obj) && nrow(sf_obj) > 0) return(NULL)
      div(
        style = paste0('position:absolute;inset:0;z-index:500;',
                       'display:flex;align-items:center;justify-content:center;',
                       'background:rgba(248,250,252,0.92);'),
        div(
          style = paste0('text-align:center;padding:32px 40px;background:#fff;',
                         'border-radius:10px;border:1px solid #e2e8f0;',
                         'box-shadow:0 2px 12px rgba(0,0,0,0.06);'),
          div(style = 'font-size:15px;font-weight:600;color:#0f172a;margin-bottom:8px;',
              'Health area boundaries not yet saved'),
          div(style = 'font-size:13px;color:#64748b;',
              'Complete the Health Areas tab and click Save before continuing.')
        )
      )
    })
    
    # ── Base map ──────────────────────────────────────────────────────────────
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.25)) |>
        leaflet::addTiles(group = 'OpenStreetMap') |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = 'ESRI Satellite') |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,  group = 'CARTO Light') |>
        leaflet::addLayersControl(
          baseGroups = c('OpenStreetMap', 'ESRI Satellite', 'CARTO Light'),
          options    = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::addScaleBar(position = 'bottomright',
                             options = leaflet::scaleBarOptions(imperial = FALSE, maxWidth = 200))
    })
    
    outputOptions(output, 'map', suspendWhenHidden = FALSE)
    
    # ── Area color / opacity helpers ──────────────────────────────────────────
    .area_color <- function(area_name) {
      if (identical(area_name, 'Inaccessible')) return('#D7301F')
      if (identical(area_name, 'Unpopulated'))  return('#e2e8f0')
      if (isTRUE(rv$planning_data[[area_name]]$complete)) return('#0d9488')
      '#94a3b8'
    }
    
    .area_opacity <- function(area_name) {
      if (.is_special(area_name)) return(0.45)
      if (isTRUE(rv$planning_data[[area_name]]$complete)) return(0.65)
      0.35
    }
    
    last_fitted_district <- reactiveVal(NULL)
    
    # ── Draw / redraw polygons ────────────────────────────────────────────────
    observe({
      sf_obj <- tryCatch(saved_dfa_sf_r(), error = function(e) NULL)
      force(reactiveValuesToList(rv))  # depend on planning_data for color updates
      
      proxy <- leaflet::leafletProxy('map', session = session) |>
        leaflet::clearShapes() |>
        leaflet::clearMarkers() |>
        leaflet::removeControl('microplan_legend')
      
      if (is.null(sf_obj) || nrow(sf_obj) == 0) return()
      
      # ── Fit bounds on district change ──────────────────────────────────────
      current_district <- district()
      if (!identical(last_fitted_district(), current_district)) {
        bbox <- sf::st_bbox(sf::st_transform(sf_obj, 4326))
        proxy <- proxy |>
          leaflet::fitBounds(
            lng1 = bbox[['xmin']], lat1 = bbox[['ymin']],
            lng2 = bbox[['xmax']], lat2 = bbox[['ymax']]
          )
        last_fitted_district(current_district)
      }
      
      # ── District boundary outline ──────────────────────────────────────────
      district_outline <- tryCatch({
        districts_shp |>
          dplyr::filter(district_name == current_district) |>
          dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
          sf::st_as_sf() |>
          safe_make_valid() |>
          sf::st_transform(4326)
      }, error = function(e) NULL)
      
      if (!is.null(district_outline) && nrow(district_outline) > 0) {
        proxy <- proxy |>
          leaflet::addPolygons(
            data    = district_outline,
            color   = '#1e293b',
            weight  = 2.5,
            fill    = FALSE,
            opacity = 1
          )
      }
      
      # ── Health area polygons ───────────────────────────────────────────────
      for (i in seq_len(nrow(sf_obj))) {
        area_name <- sf_obj$dfa_name[i]
        
        # Extract polygon geometry — handles GEOMETRYCOLLECTION gracefully
        area_geom <- tryCatch(
          sf::st_collection_extract(sf_obj[i, ], 'POLYGON'),
          error = function(e) sf_obj[i, ]
        )
        if (is.null(area_geom) || nrow(area_geom) == 0) next
        
        proxy <- proxy |>
          leaflet::addPolygons(
            data        = area_geom,
            layerId     = area_name,
            group       = 'areas',
            fillColor   = .area_color(area_name),
            fillOpacity = .area_opacity(area_name),
            color       = '#334155',
            weight      = 1.5,
            opacity     = 0.8,
            highlight   = leaflet::highlightOptions(
              weight = 3, color = '#0f172a', fillOpacity = 0.75, bringToFront = TRUE
            ),
            label        = area_name,
            labelOptions = leaflet::labelOptions(
              noHide = TRUE, direction = 'center', textOnly = TRUE,
              style  = list('font-size' = '11px', 'font-weight' = '600',
                            'color' = '#0f172a', 'background' = 'none',
                            'border' = 'none', 'box-shadow' = 'none')
            )
          )
      }
      
      proxy |>
        leaflet::addControl(
          html = paste0(
            '<div style="background:white;padding:8px 10px;border-radius:4px;',
            'font-size:12px;line-height:1.9;border:1px solid #ccc;">',
            '<b>Health Areas</b><br>',
            '<span style="display:inline-block;width:12px;height:12px;background:#0d9488;',
            'border-radius:2px;margin-right:5px;vertical-align:middle;"></span>Complete<br>',
            '<span style="display:inline-block;width:12px;height:12px;background:#94a3b8;',
            'border-radius:2px;margin-right:5px;vertical-align:middle;"></span>Incomplete<br>',
            '<span style="display:inline-block;width:12px;height:12px;background:#D7301F;',
            'border-radius:2px;margin-right:5px;vertical-align:middle;"></span>Inaccessible<br>',
            '<span style="display:inline-block;width:12px;height:12px;background:#e2e8f0;',
            'border:1px solid #ccc;border-radius:2px;margin-right:5px;vertical-align:middle;',
            '"></span>Unpopulated',
            '</div>'
          ),
          position = 'bottomright', layerId = 'microplan_legend'
        )
    })
    
    # ── Map click → open modal ────────────────────────────────────────────────
    observeEvent(input$map_shape_click, {
      area_name <- input$map_shape_click$id
      req(nzchar(area_name %||% ''))
      if (.is_special(area_name)) {
        showNotification(
          paste0('"', area_name, '" — no planning data required for this area.'),
          type = 'message', duration = 3)
        return()
      }
      editing_area(area_name)
      .show_form_modal(area_name)
    })
    
    # ── Planning modal ────────────────────────────────────────────────────────
    .show_form_modal <- function(area_name) {
      d <- .get_area_data(area_name)
      
      showModal(modalDialog(
        title = tags$span(
          tags$span(style = 'font-size:14px;font-weight:600;color:#0f172a;', area_name),
          tags$span(style = 'font-size:12px;color:#94a3b8;margin-left:8px;', 'Health Area Planning')
        ),
        size = 'l', easyClose = FALSE,
        footer = tagList(
          actionButton(session$ns('cancel_modal'), 'Cancel', class = 'btn btn-default'),
          actionButton(session$ns('save_area'), 'Save & Close',
                       class = 'btn btn-primary', style = 'font-weight:600;')
        ),
        
        # Population
        fluidRow(column(12,
                        div(style = 'background:#f8fafc;border-radius:7px;padding:14px 16px;margin-bottom:14px;',
                            div(style = 'font-size:11px;font-weight:700;color:#94a3b8;text-transform:uppercase;letter-spacing:.06em;margin-bottom:8px;',
                                'Population'),
                            fluidRow(column(6,
                                            tags$label('Estimated children under 5',
                                                       style = 'font-size:12px;font-weight:600;color:#334155;'),
                                            div(style = 'font-size:11px;color:#94a3b8;margin-bottom:4px;',
                                                'Suggested from WorldPop — edit if needed'),
                                            numericInput(session$ns('modal_u5_pop'), NULL,
                                                         value = d$u5_pop, min = 0, step = 1, width = '100%')
                            ))
                        )
        )),
        
        # Teams & Supervisors
        fluidRow(column(12,
                        div(style = 'background:#f8fafc;border-radius:7px;padding:14px 16px;margin-bottom:14px;',
                            div(style = 'font-size:11px;font-weight:700;color:#94a3b8;text-transform:uppercase;letter-spacing:.06em;margin-bottom:8px;',
                                'Teams & Supervisors'),
                            fluidRow(
                              column(4,
                                     tags$label('Vaccination teams',
                                                style = 'font-size:12px;font-weight:600;color:#334155;'),
                                     div(style = 'font-size:11px;color:#94a3b8;margin-bottom:4px;',
                                         paste0('Suggested: ', max(1L, ceiling(d$u5_pop / TEAMS_PER_CHILD)))),
                                     numericInput(session$ns('modal_n_teams'), NULL,
                                                  value = d$n_teams, min = 1, step = 1, width = '100%')
                              ),
                              column(4,
                                     tags$label('Supervisors',
                                                style = 'font-size:12px;font-weight:600;color:#334155;'),
                                     div(style = 'font-size:11px;color:#94a3b8;margin-bottom:4px;',
                                         '1 per 5 vaccination teams'),
                                     numericInput(session$ns('modal_n_supervisors'), NULL,
                                                  value = d$n_supervisors, min = 1, step = 1, width = '100%')
                              )
                            )
                        )
        )),
        
        # Supervisor details
        fluidRow(column(12,
                        div(style = 'background:#f8fafc;border-radius:7px;padding:14px 16px;margin-bottom:14px;',
                            div(style = 'font-size:11px;font-weight:700;color:#94a3b8;text-transform:uppercase;letter-spacing:.06em;margin-bottom:8px;',
                                'Supervisor details'),
                            uiOutput(session$ns('supervisor_fields'))
                        )
        )),
        
        # Notes + completeness
        fluidRow(
          column(8,
                 tags$label('Notes (optional)',
                            style = 'font-size:12px;font-weight:600;color:#334155;'),
                 textAreaInput(session$ns('modal_notes'), NULL,
                               value = d$notes %||% '', rows = 2, width = '100%')
          ),
          column(4,
                 div(style = 'padding-top:22px;',
                     checkboxInput(
                       session$ns('mark_complete'),
                       tags$span(style = 'font-size:13px;font-weight:600;color:#0d9488;',
                                 'Mark as complete'),
                       value = isTRUE(d$complete)
                     )
                 )
          )
        )
      ))
    }
    
    # ── Supervisor fields ─────────────────────────────────────────────────────
    output$supervisor_fields <- renderUI({
      n <- max(1L, min(as.integer(input$modal_n_supervisors %||% 1L), 20L))
      
      area_name <- isolate(editing_area())
      existing  <- if (!is.null(area_name))
        isolate(rv$planning_data[[area_name]]$supervisors) %||% list()
      else list()
      
      rows <- lapply(seq_len(n), function(i) {
        prev_name  <- if (i <= length(existing)) existing[[i]]$name  %||% '' else ''
        prev_role  <- if (i <= length(existing)) existing[[i]]$role  %||% '' else ''
        prev_phone <- if (i <= length(existing)) existing[[i]]$phone %||% '' else ''
        prev_email <- if (i <= length(existing)) existing[[i]]$email %||% '' else ''
        
        fluidRow(
          style = 'margin-bottom: 6px;',
          column(1,
                 div(style = paste0(
                   'width:24px;height:24px;border-radius:50%;',
                   'background:#e2e8f0;color:#64748b;',
                   'display:flex;align-items:center;justify-content:center;',
                   'font-size:11px;font-weight:700;margin-top:4px;'), i)
          ),
          column(3,
                 textInput(session$ns(paste0('sup_name_',  i)), NULL,
                           value = prev_name,  placeholder = paste0('Supervisor ', i, ' name'),
                           width = '100%')
          ),
          column(2,
                 textInput(session$ns(paste0('sup_role_',  i)), NULL,
                           value = prev_role,  placeholder = 'Role / title', width = '100%')
          ),
          column(3,
                 textInput(session$ns(paste0('sup_phone_', i)), NULL,
                           value = prev_phone, placeholder = 'Phone', width = '100%')
          ),
          column(3,
                 textInput(session$ns(paste0('sup_email_', i)), NULL,
                           value = prev_email, placeholder = 'Email', width = '100%')
          )
        )
      })
      
      do.call(tagList, rows)
    })
    
    # ── Auto-complete detection ───────────────────────────────────────────────
    observe({
      req(!is.null(input$modal_u5_pop), !is.null(input$modal_n_supervisors))
      n_sup   <- as.integer(input$modal_n_supervisors %||% 0L)
      all_sup <- all(vapply(seq_len(n_sup), function(i) {
        nzchar(trimws(input[[paste0('sup_name_', i)]] %||% ''))
      }, logical(1)))
      auto_complete <- (input$modal_u5_pop  %||% 0) > 0 &&
        (input$modal_n_teams %||% 0) > 0 &&
        n_sup > 0L && all_sup
      if (isTRUE(auto_complete) && !isTRUE(input$mark_complete))
        updateCheckboxInput(session, 'mark_complete', value = TRUE)
    })
    
    # ── Save area data ────────────────────────────────────────────────────────
    .collect_and_save <- function() {
      area_name <- editing_area()
      req(nzchar(area_name %||% ''))
      
      n_sup <- max(1L, as.integer(input$modal_n_supervisors %||% 1L))
      
      supervisors <- lapply(seq_len(n_sup), function(i) {
        list(
          name  = trimws(input[[paste0('sup_name_',  i)]] %||% ''),
          role  = trimws(input[[paste0('sup_role_',  i)]] %||% ''),
          phone = trimws(input[[paste0('sup_phone_', i)]] %||% ''),
          email = trimws(input[[paste0('sup_email_', i)]] %||% '')
        )
      })
      
      rv$planning_data[[area_name]] <- list(
        u5_pop        = as.numeric(input$modal_u5_pop      %||% 0),
        n_teams       = as.integer(input$modal_n_teams     %||% 1L),
        n_supervisors = n_sup,
        supervisors   = supervisors,
        complete      = isTRUE(input$mark_complete),
        notes         = trimws(input$modal_notes %||% '')
      )
      
      if (!is.null(save_snapshot_fn))
        save_snapshot_fn(list(planning_data = rv$planning_data))
    }
    
    observeEvent(input$save_area,    { .collect_and_save(); removeModal(); editing_area(NULL) }, ignoreInit = TRUE)
    observeEvent(input$cancel_modal, { removeModal(); editing_area(NULL) },                      ignoreInit = TRUE)
    
    # ── Download ──────────────────────────────────────────────────────────────
    output$download_data <- downloadHandler(
      filename = function()
        paste0('microplan_', gsub('[^A-Za-z0-9]', '_', district() %||% 'unknown'),
               '_', format(Sys.Date(), '%Y%m%d'), '.zip'),
      content = function(file) {
        di <- sf::st_drop_geometry(districts_shp) |>
          dplyr::filter(district_name == district()) |>
          dplyr::slice(1)
        
        build_district_zip(
          file          = file,
          district_name = district() %||% '',
          zone          = di$zone_name[1]   %||% '',
          region        = di$region_name[1] %||% '',
          saved_dfa_sf  = tryCatch(saved_dfa_sf_r(), error = function(e) NULL),
          planning_data = rv$planning_data,
          facility_data = tryCatch(facility_data_r(), error = function(e) NULL)
        )
      }
    )
    
    # ── Return ────────────────────────────────────────────────────────────────
    list(
      planning_data_r       = reactive(rv$planning_data),
      restore_from_snapshot = function(snap) {
        if (!is.null(snap$planning_data)) rv$planning_data <- snap$planning_data
      }
    )
  })
}
