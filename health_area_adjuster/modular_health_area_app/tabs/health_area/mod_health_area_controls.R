healthAreaControlsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # ── Title row ─────────────────────────────────────────────────────────────
    div(
      style = 'display:flex; align-items:center; justify-content:space-between; margin-bottom:8px;',
      div(style = 'font-size:14px; font-weight:600; color:#0f172a;', 'Health Areas'),
      actionButton(ns('help_btn'), '?', width = '28px',
                   style = 'font-size:11px;padding:0;height:24px;line-height:24px;')
    ),
    
    # ── Instructions ──────────────────────────────────────────────────────────
    div(
      style = paste0('background:#f0fdf4;border-left:3px solid #0d9488;',
                     'border-radius:0 6px 6px 0;padding:7px 10px;margin-bottom:8px;'),
      tags$p(
        style = 'font-size: 11px; font-weight: 600; color: #0f172a; margin: 0 0 3px;',
        'About health areas'
      ),
      tags$p(
        style = 'font-size: 11px; color: #475569; line-height: 1.6; margin: 0;',
        'Each health area is coordinated by one outreach coordination site, ideally covering ',
        tags$strong('~2,000 children'), ' with ', tags$strong('~5 outreach teams. '),
        'Boundaries are generated automatically, then adjusted by the group.'
      )
    ),
    
    tags$p(
      style = 'font-size: 11px; color: #475569; line-height: 1.7; margin-bottom: 8px;',
      tags$strong('Suggested approach:'),
      tags$br(),
      '1. Click a row in the table on the right to select a health area.',
      tags$br(),
      '2. Click and drag on the map to paint that area\'s boundaries.',
      tags$br(),
      '3. Start with ', tags$strong('Inaccessible'), ' and ', tags$strong('Unpopulated'), ' areas, then adjust remaining boundaries.',
      tags$br(),
      '4. ', tags$strong('Save'), ' to confirm, then ', tags$strong('Submit'), ' when done.'
    ),
    
    tags$hr(style = 'margin: 6px 0;'),
    
    # ── Brush size ────────────────────────────────────────────────────────────
    div(
      style = paste0(
        'background:#f8fafc;border:1px solid #e2e8f0;border-radius:8px;',
        'padding:10px 12px;margin-bottom:10px;'
      ),
      div(
        style = 'display:flex;align-items:center;justify-content:space-between;margin-bottom:6px;',
        tags$span(
          style = 'font-size:11px;font-weight:700;color:#475569;text-transform:uppercase;letter-spacing:.05em;',
          'Brush Diameter (meters)'
        )
      ),
      div(
        style = 'display:flex;align-items:center;gap:6px;',
        actionButton(
          ns('brush_minus'), '\u2212',
          style = paste0(
            'width:28px;height:28px;padding:0;line-height:28px;text-align:center;',
            'font-size:16px;font-weight:700;border-radius:6px;',
            'background:#fff;border:1px solid #cbd5e1;color:#475569;flex-shrink:0;'
          )
        ),
        div(
          style = 'flex:1;',
          sliderInput(
            ns('brush_m_ui'), NULL,
            min = 100, max = 10000, value = 5000,
            step = 100, width = '100%',
            ticks = FALSE
          )
        ),
        actionButton(
          ns('brush_plus'), '+',
          style = paste0(
            'width:28px;height:28px;padding:0;line-height:28px;text-align:center;',
            'font-size:16px;font-weight:700;border-radius:6px;',
            'background:#fff;border:1px solid #cbd5e1;color:#475569;flex-shrink:0;'
          )
        )
      )
    ),
    
    # ── Overlay options ───────────────────────────────────────────────────────
    div(
      style = 'margin-bottom:8px;',
      checkboxInput(ns('show_pop_raster'),     'Show WorldPop U5 Population', value = show_pop_default),
      checkboxInput(ns('show_friction_raster'), 'Show Friction Surface',       value = FALSE),
      checkboxInput(ns('boundary_only'),        'Boundaries only',             value = boundary_only_default)
    ),
    
    # ── Save / Reset ──────────────────────────────────────────────────────────
    div(
      style = 'display:flex;gap:6px;margin-bottom:8px;',
      actionButton(ns('reset_btn'), 'Reset',
                   class = 'btn btn-default btn-sm', style = 'flex:1;'),
      actionButton(ns('save_btn'),  'Save',
                   class = 'btn btn-default btn-sm', style = 'flex:1;')
    ),
    
    tags$hr(style = 'margin: 6px 0;'),
    
    actionButton(
      ns('submit_btn'), 'Submit Health Areas',
      class = 'btn btn-primary btn-sm',
      width = '100%',
      icon  = icon('check-circle')
    ),
    div(
      style = 'font-size: 11px; color: #64748b; margin-top: 4px; line-height: 1.4;',
      'Saves boundaries to the database.'
    ),
    
    tags$hr(style = 'margin: 8px 0;'),
    
    actionButton(
      ns('continue_btn'), 'Continue \u2192',
      class = 'btn btn-default btn-sm',
      width = '100%',
      style = 'font-weight: 600;'
    )
  )
}


healthAreaControlsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Fixed step for +/- buttons (diameter units)
    BRUSH_STEP <- 100L
    BRUSH_MIN  <- 100L
    BRUSH_MAX  <- 10000L
    
    observeEvent(input$brush_minus, {
      val <- input$brush_m_ui %||% 5000L
      updateSliderInput(session, 'brush_m_ui',
                        value = max(BRUSH_MIN, val - BRUSH_STEP))
    }, ignoreInit = TRUE)
    
    observeEvent(input$brush_plus, {
      val <- input$brush_m_ui %||% 5000L
      updateSliderInput(session, 'brush_m_ui',
                        value = min(BRUSH_MAX, val + BRUSH_STEP))
    }, ignoreInit = TRUE)
    
    # set_brush_limits kept for API compatibility but is now a no-op —
    # the slider has a fixed range independent of district size.
    set_brush_limits <- function(brush_limits) invisible(NULL)
    
    list(
      brush_m              = reactive(input$brush_m_ui),
      show_pop_raster      = reactive(isTRUE(input$show_pop_raster)),
      show_friction_raster = reactive(input$show_friction_raster),
      boundary_only        = reactive(isTRUE(input$boundary_only)),
      help_click           = reactive(input$help_btn),
      save_click           = reactive(input$save_btn),
      submit_click         = reactive(input$submit_btn),
      reset_click          = reactive(input$reset_btn),
      continue_click       = reactive(input$continue_btn),
      brush_minus_click    = reactive(input$brush_minus),
      brush_plus_click     = reactive(input$brush_plus),
      set_brush_limits     = set_brush_limits
    )
  })
}