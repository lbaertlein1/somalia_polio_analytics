# =============================================================================
# mod_export_tab.R
#
# Standalone export page, available to every user (not admin-only — the
# admin panel's old "Download all" / "Print maps" actions moved here
# entirely). Scoped to CURRENT/PUBLISHED data only, same boundary as
# everywhere else a regular user can act: a non-current version (someone's
# draft, or an old superseded one) stays admin-only, browsable from the
# admin panel's District review section, never from here.
#
# Two independent actions:
#   - Boundary export: whole campaign or one district, GeoJSON/SHP/KML,
#     reusing build_campaign_download_v2()/build_district_download_v2()
#     (download_helpers_v2.R).
#   - Printable maps: one district's current health-area + team-area
#     boundaries as a print-ready PDF (printable_export.R) — this already
#     existed as a real, working feature inside the admin panel; moved
#     here unchanged in behavior, just relocated and opened up to every
#     user.
# =============================================================================

exportTabUI <- function(id) {
  ns <- NS(id)
  
  div(
    style = 'padding: 20px 28px; max-width: 1400px;',
    
    tags$h3(style = 'font-size: 18px; font-weight: 700; color: #0f172a; margin: 0 0 3px;',
            'Export'),
    tags$p(style = 'font-size: 13px; color: #64748b; margin: 0 0 20px;',
           'Download current, published boundaries and data. For a specific past version or a draft, an admin can pull it from the Admin panel.'),
    
    div(style = 'max-width: 760px;',
        div(class = 'mini-label', 'Campaign'),
        selectInput(ns('campaign'), NULL, choices = setNames('', 'Select campaign...'), selected = '', width = '100%'),
        
        tags$hr(style = 'margin: 16px 0;'),
        
        tags$h4(style = 'font-size: 14px; font-weight: 700; color: #1e293b; margin: 0 0 10px;', 'Boundary export'),
        
        div(class = 'mini-label', 'Scope'),
        radioButtons(ns('scope'), NULL,
                     choices = c('Whole campaign' = 'campaign', 'Single district' = 'district'),
                     selected = 'campaign', inline = TRUE),
        
        uiOutput(ns('district_picker_ui')),
        
        div(class = 'mini-label', style = 'margin-top: 10px;', 'Format'),
        radioButtons(ns('format'), NULL,
                     choices = c('GeoJSON' = 'geojson', 'Shapefile' = 'shp', 'KML' = 'kml'),
                     selected = 'geojson', inline = TRUE),
        
        actionButton(ns('prepare_download'), 'Prepare download', icon = icon('download'),
                     class = 'btn btn-primary', style = 'margin-top: 8px;')
    ),
    
    tags$hr(style = 'margin: 24px 0;'),
    
    tags$h4(style = 'font-size: 14px; font-weight: 700; color: #1e293b; margin: 0 0 10px;', 'Printable maps'),
    tags$p(style = 'font-size: 12px; color: #64748b; margin: 0 0 14px;',
           'One overview page plus one page per health area, sized for A3/A4 landscape printing, ',
           'each with a population/team summary table.'),
    
    # Sidebar inputs / main-panel preview, same fluidRow(column, column)
    # pattern the app's other map-editing tabs already use (e.g.
    # healthAreaTabUI's controls/map/population columns) rather than a
    # single stacked column -- puts the preview where a user expects to
    # find it, next to (not below) the controls that shape it.
    fluidRow(
      column(width = 5,
             uiOutput(ns('print_district_picker_ui')),
             
             div(class = 'mini-label', style = 'margin-top: 10px;', 'Basemap'),
             radioButtons(ns('print_basemap'), NULL,
                          choices = c('None' = 'none', 'OpenStreetMap' = 'osm', 'Satellite' = 'satellite', 'Topo' = 'topo'),
                          selected = 'none', inline = TRUE),
             
             div(class = 'mini-label', style = 'margin-top: 10px;', 'Basemap zoom level'),
             sliderInput(ns('print_zoom'), NULL, min = 1, max = 19, value = 16, step = 1, width = '100%'),
             
             div(class = 'mini-label', style = 'margin-top: 10px;', 'Page dimensions (inches)'),
             div(style = 'display: flex; gap: 10px; align-items: center;',
                 numericInput(ns('print_width_in'), 'Width', value = 11.69, min = 1, max = 60, step = 0.1, width = '120px'),
                 numericInput(ns('print_height_in'), 'Height', value = 8.27, min = 1, max = 60, step = 0.1, width = '120px')
             ),
             
             checkboxInput(ns('print_show_pop'), 'Include WorldPop population overlay', value = FALSE),
             
             actionButton(ns('prepare_print'), 'Generate PDF', icon = icon('print'),
                          class = 'btn btn-default', style = 'margin-top: 14px;')
      ),
      column(width = 7,
             div(class = 'mini-label', 'Preview (district overview page)'),
             # Aspect ratio of this container is set live from the width/
             # height inputs (see print_preview_container_ui below) so the
             # preview box's own shape actually mirrors the page being
             # printed, not a fixed box the image gets squeezed into
             # regardless of what dimensions are set.
             uiOutput(ns('print_preview_container_ui'))
      )
    )
  )
}

exportTabServer <- function(id, districts_shp) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      campaigns <- tryCatch(db_get_campaigns(pool, active_only = TRUE), error = function(e) NULL)
      if (is.null(campaigns) || nrow(campaigns) == 0) {
        updateSelectInput(session, 'campaign', choices = setNames('', 'No active campaigns'), selected = '')
        return()
      }
      choices <- setNames(as.character(campaigns$campaign_id), campaigns$campaign_name)
      updateSelectInput(session, 'campaign', choices = c(setNames('', 'Select campaign...'), choices), selected = '')
    })
    
    campaign_id <- reactive({
      v <- input$campaign %||% ''
      if (!nzchar(v)) NULL else as.integer(v)
    })
    
    # Only campaign-assigned districts are ever exportable here — same
    # scoping as the intro table, never every district in districts_shp.
    campaign_district_choices <- reactive({
      cid <- campaign_id()
      req(!is.null(cid))
      assigned <- tryCatch(db_get_campaign_districts(pool, cid), error = function(e) NULL)
      if (is.null(assigned) || nrow(assigned) == 0) return(character(0))
      sort(assigned$district_name)
    })
    
    output$district_picker_ui <- renderUI({
      req(identical(input$scope, 'district'))
      choices <- campaign_district_choices()
      tagList(
        div(class = 'mini-label', style = 'margin-top: 10px;', 'District'),
        selectInput(session$ns('district'), NULL,
                    choices = c(setNames('', 'Select district...'), choices), width = '100%')
      )
    })
    
    output$print_district_picker_ui <- renderUI({
      choices <- campaign_district_choices()
      div(class = 'mini-label', 'District',
          selectInput(session$ns('print_district'), NULL,
                      choices = c(setNames('', 'Select district...'), choices), width = '100%')
      )
    })
    
    # ── Boundary export ────────────────────────────────────────────────────
    download_path <- reactiveVal(NULL)
    
    observeEvent(input$prepare_download, {
      cid <- campaign_id()
      req(!is.null(cid))
      fmt <- input$format %||% 'geojson'
      
      if (identical(input$scope, 'district')) {
        dname <- input$district %||% ''
        req(nzchar(dname))
        version <- tryCatch(db_get_shared_version(pool, cid, dname), error = function(e) NULL)
        if (is.null(version)) {
          showNotification('This district has no current health-area map to export.', type = 'warning', duration = 5)
          return()
        }
        dinfo <- districts_shp |> dplyr::filter(district_name == dname)
        zone_val   <- if (nrow(dinfo) > 0) as.character(dinfo$zone_name[1])   else ''
        region_val <- if (nrow(dinfo) > 0) as.character(dinfo$region_name[1]) else ''
        tmp <- tempfile(fileext = '.zip')
        tryCatch({
          withProgress(message = paste('Exporting', dname, '...'), {
            build_district_download_v2(tmp, dname, zone_val, region_val, version,
                                       campaign_id = cid, format = fmt)
          })
          download_path(tmp)
          showModal(modalDialog(
            title = 'Download ready', easyClose = TRUE, footer = modalButton('Close'),
            tags$p(sprintf('%s, %s format.', dname, toupper(fmt))),
            downloadButton(session$ns('download_file'), 'Download zip')
          ))
        }, error = function(e) showNotification(paste('Export failed:', e$message), type = 'error', duration = 6))
      } else {
        tmp <- tempfile(fileext = '.zip')
        tryCatch({
          withProgress(message = 'Exporting whole campaign...', value = 0, {
            build_campaign_download_v2(tmp, cid, format = fmt,
                                       progress_callback = function(i, n, district_name) {
                                         setProgress(value = i / max(n, 1), message = sprintf('District %d of %d: %s', i, n, district_name))
                                       })
          })
          download_path(tmp)
          showModal(modalDialog(
            title = 'Download ready', easyClose = TRUE, footer = modalButton('Close'),
            tags$p(sprintf('Whole campaign, %s format.', toupper(fmt))),
            downloadButton(session$ns('download_file'), 'Download zip')
          ))
        }, error = function(e) showNotification(paste('Export failed:', e$message), type = 'error', duration = 6))
      }
    }, ignoreInit = TRUE)
    
    output$download_file <- downloadHandler(
      filename = function() {
        scope_part <- if (identical(input$scope, 'district')) gsub('[^A-Za-z0-9]', '_', input$district %||% 'district')
        else 'campaign'
        paste0(scope_part, '_', Sys.Date(), '.zip')
      },
      content = function(file) {
        src <- download_path()
        if (!is.null(src) && file.exists(src)) file.copy(src, file)
      },
      contentType = 'application/zip'
    )
    
    # ── Printable maps ──────────────────────────────────────────────────────
    print_path <- reactiveVal(NULL)
    
    observeEvent(input$prepare_print, {
      cid <- campaign_id()
      req(!is.null(cid))
      dname <- input$print_district %||% ''
      req(nzchar(dname))
      
      version <- tryCatch(db_get_shared_version(pool, cid, dname), error = function(e) NULL)
      if (is.null(version)) {
        showNotification('This district has no current health-area map to print.', type = 'warning', duration = 5)
        return()
      }
      cname <- { cdf <- tryCatch(db_get_campaigns(pool, active_only = FALSE), error = function(e) NULL)
      row <- if (!is.null(cdf)) cdf[cdf$campaign_id == cid, , drop = FALSE] else NULL
      if (!is.null(row) && nrow(row) == 1) row$campaign_name[1] else '' }
      
      tmp <- tempfile(fileext = '.pdf')
      tryCatch({
        withProgress(message = paste('Building printable maps for', dname, '...'), {
          build_printable_maps_pdf(tmp, version, dname, campaign_id = cid, campaign_name = cname,
                                   basemap = input$print_basemap %||% 'none',
                                   zoom = input$print_zoom %||% 16,
                                   page_width_in = input$print_width_in %||% 11.69,
                                   page_height_in = input$print_height_in %||% 8.27,
                                   show_pop = isTRUE(input$print_show_pop))
        })
        print_path(tmp)
        showModal(modalDialog(
          title = 'Printable maps ready', easyClose = TRUE, footer = modalButton('Close'),
          tags$p(sprintf('One overview page plus one page per health area for %s.', dname)),
          downloadButton(session$ns('download_print'), 'Download PDF')
        ))
      }, error = function(e) showNotification(paste('Failed to build printable maps:', e$message), type = 'error', duration = 6))
    }, ignoreInit = TRUE)
    
    output$download_print <- downloadHandler(
      filename = function() paste0(gsub('[^A-Za-z0-9]', '_', input$print_district %||% 'district'), '_print.pdf'),
      content = function(file) {
        src <- print_path()
        if (!is.null(src) && file.exists(src)) file.copy(src, file)
      },
      contentType = 'application/pdf'
    )
    
    # ── Printable maps: live preview ────────────────────────────────────────
    # District overview page ONLY, rendered as a PNG -- regenerating the
    # full multi-page PDF (population raster extraction, one detail page
    # per health area, team/IDP tables) on every slider tick would be far
    # too slow to feel live. Debounced 600ms so dragging the zoom slider
    # or typing in the width/height fields doesn't fire a re-render on
    # every intermediate value, only once input settles.
    print_preview_inputs <- reactive({
      list(district = input$print_district %||% '', basemap = input$print_basemap %||% 'none',
           zoom = input$print_zoom %||% 16, width_in = input$print_width_in %||% 11.69,
           height_in = input$print_height_in %||% 8.27, show_pop = isTRUE(input$print_show_pop))
    })
    print_preview_inputs_d <- debounce(print_preview_inputs, 600)
    
    print_preview_path <- reactive({
      vals <- print_preview_inputs_d()
      if (!nzchar(vals$district)) return(NULL)
      cid <- campaign_id()
      if (is.null(cid)) return(NULL)
      version <- tryCatch(db_get_shared_version(pool, cid, vals$district), error = function(e) NULL)
      if (is.null(version)) return(NULL)
      cname <- { cdf <- tryCatch(db_get_campaigns(pool, active_only = FALSE), error = function(e) NULL)
      row <- if (!is.null(cdf)) cdf[cdf$campaign_id == cid, , drop = FALSE] else NULL
      if (!is.null(row) && nrow(row) == 1) row$campaign_name[1] else '' }
      tmp <- tempfile(fileext = '.png')
      ok <- tryCatch({
        build_printable_overview_preview_png(
          tmp, version, vals$district, campaign_id = cid, campaign_name = cname,
          width_in = vals$width_in, height_in = vals$height_in,
          basemap = vals$basemap, zoom = vals$zoom, show_pop = vals$show_pop
        )
        TRUE
      }, error = function(e) {
        cat('[export_debug] preview render failed:', conditionMessage(e), '\n')
        FALSE
      })
      if (!ok) return(NULL)
      tmp
    })
    
    # Preview container -- its CSS aspect-ratio is set live from the
    # width/height inputs (debounced, same reactive the image itself
    # uses) so the box's own shape mirrors the actual page being
    # printed, not a fixed box the image gets squeezed into regardless
    # of the dimensions chosen.
    output$print_preview_container_ui <- renderUI({
      vals <- print_preview_inputs_d()
      ratio <- if (vals$height_in > 0) vals$width_in / vals$height_in else 11.69 / 8.27
      div(style = sprintf(
        'border: 1px solid #e2e8f0; border-radius: 6px; padding: 6px; background: #f8fafc; width: 100%%; aspect-ratio: %s;',
        ratio
      ),
      imageOutput(session$ns('print_preview'), width = '100%', height = '100%')
      )
    })
    
    output$print_preview <- renderImage({
      path <- print_preview_path()
      if (is.null(path) || !file.exists(path))
        return(list(src = '', alt = 'Select a district to preview', width = '100%', height = '100%'))
      # object-fit: contain keeps the image whole (no distortion) if the
      # PNG's own aspect ratio and the container's CSS aspect-ratio
      # above ever diverge by a rounding hair -- both are driven from
      # the same width_in/height_in values, so any mismatch should be
      # negligible, but this is a cheap guarantee against stretching
      # either way.
      list(src = path, contentType = 'image/png', width = '100%', height = '100%',
           style = 'object-fit: contain;', alt = 'Printable map preview')
    }, deleteFile = TRUE)
    
    invisible(NULL)
  })
}