# =============================================================================
# mod_microplan_tab.R
# =============================================================================

TEAMS_PER_CHILD       <- 400L
SUPERVISORS_PER_TEAMS <-   5L

microplanTabUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 3,
      
      div(class = 'rightbar-title', style = 'margin-top: 4px;', 'Microplan Prep'),
      
      div(
        style = paste0('background:#f0fdf4;border-left:3px solid #0d9488;',
                       'border-radius:0 6px 6px 0;padding:7px 10px;margin-bottom:8px;'),
        tags$p(
          style = 'font-size: 11px; font-weight: 600; color: #0f172a; margin: 0 0 2px;',
          'Complete planning data for each health area'
        ),
        tags$p(
          style = 'font-size: 11px; color: #475569; line-height: 1.5; margin: 0;',
          'Click a health area on the map to open its planning form. ',
          'Areas turn ', tags$strong(style = 'color:#0d9488;', 'teal'), ' when marked complete.'
        )
      ),
      
      tags$p(
        style = 'font-size: 11px; color: #475569; line-height: 1.7; margin-bottom: 8px;',
        tags$strong('For each area, enter:'),
        tags$br(),
        '• Under-5 population (pre-filled from WorldPop)',
        tags$br(),
        '• Number of vaccination teams',
        tags$br(),
        '• Number of supervisors and their contact details',
        tags$br(),
        '• Tick ', tags$strong('Mark as complete'), ' when finished'
      ),
      
      uiOutput(ns('progress_summary')),
      
      tags$hr(style = 'margin: 8px 0;'),
      
      downloadButton(ns('download_data'), 'Download data',
                     class = 'btn btn-default btn-sm', style = 'width: 100%;'),
      
      tags$hr(style = 'margin: 8px 0;'),
      
      actionButton(ns('submit_microplan'), 'Submit Microplan Prep',
                   class = 'btn btn-primary btn-sm', width = '100%',
                   icon = icon('check-circle')),
      div(style = 'font-size: 11px; color: #64748b; margin-top: 4px; line-height: 1.4;',
          'Saves all planning data to the database.')
    ),
    
    column(
      width = 9,
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
    zone, region, district, district_ready,
    saved_dfa_sf_r,
    pop_table_r,
    facility_data_r     = reactive(NULL),
    subdivisions_r      = reactive(NULL),
    planning_area_sf_r  = reactive(NULL),
    submit_stage_fn     = NULL,
    save_snapshot_fn    = NULL,   # kept for compatibility, no-op
    areas_regenerated_r = reactive(0L),
    changed_areas_r     = reactive(character(0)),
    restore_r           = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    
    rv           <- reactiveValues(planning_data = list())
    editing_area <- reactiveVal(NULL)
    
    observeEvent(district(), {
      rv$planning_data <- list()
      editing_area(NULL)
    }, ignoreInit = TRUE)
    
    # SIA coordination sites changed -> health areas regenerated -> clear all planning data
    observeEvent(areas_regenerated_r(), {
      req(areas_regenerated_r() > 0L)
      rv$planning_data <- list()
      showNotification(
        'SIA coordination sites changed — health areas regenerated and planning data cleared.',
        type = 'warning', duration = 5
      )
    }, ignoreInit = TRUE)
    
    # Health area boundaries changed — refresh WorldPop population estimate only.
    # All other submitted data (supervisors, teams, notes, completion) is preserved.
    # Areas with no submitted data are left alone — .get_area_data() will supply
    # fresh defaults from the updated pop_table when their modal is opened.
    observeEvent(changed_areas_r(), {
      changed <- changed_areas_r()
      if (length(changed) == 0) return()
      
      pt <- tryCatch(pop_table_r(), error = function(e) NULL)
      
      n_updated <- 0L
      for (a in changed) {
        existing <- rv$planning_data[[a]]
        if (is.null(existing)) next   # no submitted data — nothing to patch
        
        new_u5_pop <- 0
        if (!is.null(pt) && nrow(pt) > 0) {
          m <- pt[pt$area_name == a, , drop = FALSE]
          if (nrow(m) > 0) new_u5_pop <- as.numeric(m$est_u5_pop[1])
        }
        rv$planning_data[[a]]$u5_pop <- new_u5_pop
        n_updated <- n_updated + 1L
      }
      
      n <- length(changed)
      showNotification(
        paste0(n, ' health area', if (n > 1) 's' else '', ' boundary changed',
               if (n_updated > 0)
                 ' — WorldPop population estimate updated in planning data.'
               else '.'),
        type = 'message', duration = 5
      )
    }, ignoreInit = TRUE)
    
    observeEvent(restore_r(), {
      snap <- restore_r()
      if (is.null(snap) || is.null(snap$planning_data)) return()
      rv$planning_data <- snap$planning_data
      showNotification('Planning data restored.', type = 'message', duration = 2)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
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
    
    output$progress_summary <- renderUI({
      sf_obj <- tryCatch(saved_dfa_sf_r(), error = function(e) NULL)
      if (is.null(sf_obj) || nrow(sf_obj) == 0)
        return(div(style = 'font-size: 12px; color: #94a3b8;', 'No saved areas yet.'))
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
                             options  = leaflet::scaleBarOptions(imperial = FALSE, maxWidth = 200))
    })
    
    outputOptions(output, 'map', suspendWhenHidden = FALSE)
    
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
    
    # Reset on every district change so fitBounds always fires when
    # saved areas become available for the new district.
    observeEvent(district(), {
      last_fitted_district(NULL)
    }, ignoreInit = TRUE)
    
    observe({
      sf_obj <- tryCatch(saved_dfa_sf_r(), error = function(e) NULL)
      force(reactiveValuesToList(rv))
      
      proxy <- leaflet::leafletProxy('map', session = session) |>
        leaflet::clearShapes() |>
        leaflet::clearMarkers() |>
        leaflet::removeControl('microplan_legend')
      
      if (is.null(sf_obj) || nrow(sf_obj) == 0) return()
      
      current_district <- district()
      if (!identical(last_fitted_district(), current_district)) {
        bbox <- sf::st_bbox(sf::st_transform(sf_obj, 4326))
        proxy <- proxy |> leaflet::fitBounds(
          lng1 = bbox[['xmin']], lat1 = bbox[['ymin']],
          lng2 = bbox[['xmax']], lat2 = bbox[['ymax']]
        )
        last_fitted_district(current_district)
      }
      
      district_outline <- tryCatch({
        pa <- tryCatch(planning_area_sf_r(), error = function(e) NULL)
        if (!is.null(pa) && nrow(pa) > 0) {
          out <- sf::st_transform(pa, 4326) |> safe_make_valid()
          tryCatch(sf::st_collection_extract(out, 'POLYGON'), error = function(e) out)
        } else {
          out <- districts_shp |>
            dplyr::filter(district_name == current_district) |>
            dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
            sf::st_as_sf() |> safe_make_valid() |> sf::st_transform(4326)
          tryCatch(sf::st_collection_extract(out, 'POLYGON'), error = function(e) out)
        }
      }, error = function(e) NULL)
      
      if (!is.null(district_outline) && nrow(district_outline) > 0 &&
          any(sf::st_geometry_type(district_outline) %in% c('POLYGON', 'MULTIPOLYGON')))
        proxy <- proxy |> leaflet::addPolygons(
          data = district_outline, color = '#1e293b', weight = 2.5, fill = FALSE, opacity = 1)
      
      for (i in seq_len(nrow(sf_obj))) {
        area_name <- sf_obj$dfa_name[i]
        area_geom <- tryCatch(sf::st_collection_extract(sf_obj[i, ], 'POLYGON'),
                              error = function(e) sf_obj[i, ])
        if (is.null(area_geom) || nrow(area_geom) == 0) next
        # Skip non-polygon geometries (GEOMETRYCOLLECTION slivers, etc.)
        if (!any(sf::st_geometry_type(area_geom) %in% c('POLYGON', 'MULTIPOLYGON'))) next
        
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
              fillOpacity = min(1, .area_opacity(area_name) + 0.2),
              weight = 2.5, bringToFront = TRUE
            ),
            label       = if (.is_special(area_name)) area_name else {
              d <- rv$planning_data[[area_name]]
              if (!is.null(d))
                paste0(area_name, ' | U5: ', round(d$u5_pop), ' | Teams: ', d$n_teams)
              else area_name
            },
            labelOptions = leaflet::labelOptions(
              noHide = FALSE, direction = 'center', textOnly = FALSE,
              style = list('font-size' = '11px', 'font-weight' = '600')
            )
          )
      }
    })
    
    observeEvent(input$map_shape_click, {
      info <- input$map_shape_click
      req(!is.null(info$id))
      area_name <- as.character(info$id)
      if (.is_special(area_name)) return()
      editing_area(area_name)
      .show_area_modal(area_name)
    })
    
    .show_area_modal <- function(area_name) {
      d <- .get_area_data(area_name)
      
      # Look up the current WorldPop estimate from the population table so the
      # subheader always reflects the latest boundary, even if d$u5_pop was
      # manually overridden by the user in a prior edit.
      pt          <- tryCatch(pop_table_r(), error = function(e) NULL)
      wp_estimate <- d$u5_pop   # default: whatever is stored
      if (!is.null(pt) && nrow(pt) > 0) {
        m <- pt[pt$area_name == area_name, , drop = FALSE]
        if (nrow(m) > 0) wp_estimate <- as.numeric(m$est_u5_pop[1])
      }
      wp_hint <- paste0('WorldPop estimate: ', format(round(wp_estimate), big.mark = ','))
      
      showModal(modalDialog(
        title = tags$span(
          tags$span(style = 'font-weight:700;color:#0f172a;', area_name),
          tags$span(style = 'color:#94a3b8;font-size:12px;margin-left:8px;', '— Planning Data')
        ),
        size = 'l', easyClose = FALSE,
        footer = tagList(
          actionButton(session$ns('cancel_modal'), 'Cancel', class = 'btn btn-default'),
          actionButton(session$ns('save_area'),    'Save',   class = 'btn btn-primary',
                       style = 'font-weight:600;')
        ),
        fluidRow(column(12,
                        div(style = 'background:#f8fafc;border-radius:7px;padding:14px 16px;margin-bottom:14px;',
                            div(style = 'font-size:11px;font-weight:700;color:#94a3b8;text-transform:uppercase;
                         letter-spacing:.06em;margin-bottom:10px;', 'Population & Teams'),
                            fluidRow(
                              column(4,
                                     tags$label('U5 Population',
                                                style = 'font-size:12px;font-weight:600;color:#334155;'),
                                     div(style = 'font-size:11px;color:#94a3b8;margin-bottom:4px;',
                                         wp_hint),
                                     numericInput(session$ns('modal_u5_pop'), NULL,
                                                  value = d$u5_pop, min = 0, step = 10, width = '100%')
                              ),
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
        fluidRow(column(12,
                        div(style = 'background:#f8fafc;border-radius:7px;padding:14px 16px;margin-bottom:14px;',
                            div(style = 'font-size:11px;font-weight:700;color:#94a3b8;text-transform:uppercase;
                         letter-spacing:.06em;margin-bottom:8px;', 'Supervisor details'),
                            uiOutput(session$ns('supervisor_fields'))
                        )
        )),
        fluidRow(
          column(8,
                 tags$label('Notes (optional)', style = 'font-size:12px;font-weight:600;color:#334155;'),
                 textAreaInput(session$ns('modal_notes'), NULL,
                               value = d$notes %||% '', rows = 2, width = '100%')
          ),
          column(4,
                 div(style = 'padding-top:22px;',
                     checkboxInput(
                       session$ns('mark_complete'),
                       tags$span(style = 'font-size:13px;font-weight:600;color:#0d9488;', 'Mark as complete'),
                       value = isTRUE(d$complete)
                     )
                 )
          )
        )
      ))
    }
    
    # ── Supervisor draft ───────────────────────────────────────────────────────
    # Stores in-progress supervisor values independently of the Shiny input
    # store, so they survive renderUI refreshes when the count changes.
    supervisor_draft <- reactiveVal(list())
    
    # Initialise from saved data whenever the modal opens for a new area.
    observeEvent(editing_area(), {
      area_name  <- editing_area()
      saved_sups <- if (!is.null(area_name))
        rv$planning_data[[area_name]]$supervisors %||% list()
      else list()
      supervisor_draft(saved_sups)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Snapshot current input values into the draft BEFORE renderUI re-renders.
    # Shiny runs observers before render functions in the same flush cycle,
    # so this always executes first when modal_n_supervisors changes.
    observeEvent(input$modal_n_supervisors, {
      current <- supervisor_draft()
      for (i in seq_len(20L)) {
        nm <- input[[paste0('sup_name_',  i)]]
        ro <- input[[paste0('sup_role_',  i)]]
        ph <- input[[paste0('sup_phone_', i)]]
        em <- input[[paste0('sup_email_', i)]]
        # Only update a slot when at least one of its inputs is known to the server
        if (!is.null(nm) || !is.null(ro) || !is.null(ph) || !is.null(em)) {
          prev <- if (i <= length(current)) current[[i]] else list()
          current[[i]] <- list(
            name  = if (!is.null(nm) && nzchar(nm)) nm else prev$name  %||% '',
            role  = if (!is.null(ro) && nzchar(ro)) ro else prev$role  %||% '',
            phone = if (!is.null(ph) && nzchar(ph)) ph else prev$phone %||% '',
            email = if (!is.null(em) && nzchar(em)) em else prev$email %||% ''
          )
        }
      }
      supervisor_draft(current)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    output$supervisor_fields <- renderUI({
      n     <- max(1L, min(as.integer(input$modal_n_supervisors %||% 1L), 20L))
      draft <- supervisor_draft()
      
      rows <- lapply(seq_len(n), function(i) {
        sup <- if (i <= length(draft)) draft[[i]] else list()
        fluidRow(
          style = 'margin-bottom: 6px;',
          column(1, div(style = paste0(
            'width:24px;height:24px;border-radius:50%;background:#e2e8f0;color:#64748b;',
            'display:flex;align-items:center;justify-content:center;',
            'font-size:11px;font-weight:700;margin-top:4px;'), i)),
          column(3, textInput(session$ns(paste0('sup_name_',  i)), NULL,
                              value = sup$name  %||% '',
                              placeholder = paste0('Supervisor ', i, ' name'), width = '100%')),
          column(2, textInput(session$ns(paste0('sup_role_',  i)), NULL,
                              value = sup$role  %||% '',
                              placeholder = 'Role / title', width = '100%')),
          column(3, textInput(session$ns(paste0('sup_phone_', i)), NULL,
                              value = sup$phone %||% '',
                              placeholder = 'Phone', width = '100%')),
          column(3, textInput(session$ns(paste0('sup_email_', i)), NULL,
                              value = sup$email %||% '',
                              placeholder = 'Email', width = '100%'))
        )
      })
      do.call(tagList, rows)
    })
    
    observe({
      req(!is.null(input$modal_u5_pop), !is.null(input$modal_n_supervisors))
      n_sup   <- as.integer(input$modal_n_supervisors %||% 0L)
      all_sup <- all(vapply(seq_len(n_sup), function(i) {
        nzchar(trimws(input[[paste0('sup_name_', i)]] %||% ''))
      }, logical(1)))
      auto_complete <- (input$modal_u5_pop  %||% 0) > 0 &&
        (input$modal_n_teams %||% 0) > 0 && n_sup > 0L && all_sup
      if (isTRUE(auto_complete) && !isTRUE(input$mark_complete))
        updateCheckboxInput(session, 'mark_complete', value = TRUE)
    })
    
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
    }
    
    observeEvent(input$save_area,    { .collect_and_save(); removeModal(); editing_area(NULL) },
                 ignoreInit = TRUE)
    observeEvent(input$cancel_modal, { removeModal(); editing_area(NULL) },
                 ignoreInit = TRUE)
    
    # ── Submit microplan → DB ─────────────────────────────────────────────────
    observeEvent(input$submit_microplan, {
      if (length(rv$planning_data) == 0) {
        showNotification(
          "No planning data to submit yet. Enter data for at least one health area.",
          type = "warning", duration = 4
        )
        return()
      }
      if (!is.null(submit_stage_fn)) {
        submit_stage_fn('microplan', list(planning_data = rv$planning_data))
      }
    }, ignoreInit = TRUE)
    
    output$download_data <- downloadHandler(
      filename = function()
        paste0('microplan_', gsub('[^A-Za-z0-9]', '_', district() %||% 'unknown'),
               '_', format(Sys.Date(), '%Y%m%d'), '.zip'),
      content = function(file) {
        di <- sf::st_drop_geometry(districts_shp) |>
          dplyr::filter(district_name == district()) |> dplyr::slice(1)
        fac <- tryCatch(facility_data_r(), error = function(e) NULL)
        build_district_download(
          file          = file,
          district_name = district() %||% '',
          zone          = '',
          region        = di$region_name[1] %||% '',
          saved_dfa_sf  = tryCatch(saved_dfa_sf_r(), error = function(e) NULL),
          planning_data = rv$planning_data,
          facility_data = fac
        )
      }
    )
    
    list(
      planning_data_r       = reactive(rv$planning_data),
      restore_from_snapshot = function(snap) {
        if (!is.null(snap$planning_data)) rv$planning_data <- snap$planning_data
      }
    )
  })
}