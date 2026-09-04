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
    
    # ── STEP 1: Painting / STEP 2: Refine Boundaries ─────────────────────
    # Two visually distinct, mutually-exclusive steps: only one group's
    # controls are ever interactive at once, matching which mode the
    # canvas is actually in. Rendered via uiOutput/renderUI (not a static
    # div toggled by shinyjs::show/hide, which silently does nothing if
    # shinyjs::useShinyjs() isn't set up -- renderUI needs no such setup
    # and fails loudly instead of just never appearing).
    uiOutput(ns('paint_step_ui')),

    tags$hr(style = 'margin: 6px 0;'),

    div(
      id = ns('refine_controls'),
      actionButton(
        ns('refine_boundaries_btn'), 'Refine Boundaries',
        class = 'btn btn-default btn-sm', width = '100%',
        icon = icon('draw-polygon')
      ),
      uiOutput(ns('refine_step_ui'))
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
    ns <- session$ns  # needed for renderUI blocks below, which construct namespaced input ids server-side
    
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

    # Drives output$paint_step_ui/output$refine_step_ui below via a plain
    # reactiveVal, rather than toggling visibility with shinyjs::show/hide
    # -- that depends on shinyjs::useShinyjs() having been called in the
    # app's UI, which is easy to miss and fails completely silently (the
    # elements just never appear, with nothing in the logs). renderUI/
    # uiOutput need no such setup and fail loudly if something's wrong.
    vertex_mode_active <- reactiveVal(FALSE)

    set_vertex_mode_ui <- function(in_vertex_mode) {
      vertex_mode_active(isTRUE(in_vertex_mode))
      if (isTRUE(in_vertex_mode)) {
        updateActionButton(session, 'refine_boundaries_btn', label = 'Back to Painting',
                            icon = icon('paintbrush'))
      } else {
        updateActionButton(session, 'refine_boundaries_btn', label = 'Refine Boundaries',
                            icon = icon('draw-polygon'))
      }
    }

    # ── Step 1: Painting ────────────────────────────────────────────────
    # Full controls while painting; collapses to a plain status line while
    # refining -- painting's own Undo/Reset/Save are only ever clickable
    # in painting mode, never while refining, by simply not existing in
    # the DOM at that point (not just being disabled).
    output$paint_step_ui <- renderUI({
      if (isTRUE(vertex_mode_active())) {
        div(
          style = paste0('padding:8px 10px;background:#f1f5f9;border-radius:6px;',
                         'color:#64748b;font-size:12px;text-align:center;'),
          tags$strong('Step 1: Painting'), tags$br(),
          'Currently refining — click "Back to Painting" below to resume.'
        )
      } else {
        tagList(
          div(style = 'font-size:11px;font-weight:700;color:#0f172a;margin-bottom:6px;',
              'STEP 1: PAINTING'),
          div(
            style = 'display:flex;gap:6px;margin-bottom:8px;',
            actionButton(ns('paint_undo_btn'), 'Undo',
                         class = 'btn btn-default btn-sm', style = 'flex:1;',
                         icon = icon('rotate-left')),
            actionButton(ns('reset_btn'), 'Reset',
                         class = 'btn btn-default btn-sm', style = 'flex:1;'),
            actionButton(ns('save_btn'),  'Save',
                         class = 'btn btn-default btn-sm', style = 'flex:1;')
          )
        )
      }
    })

    # ── Step 2: Refine Boundaries ───────────────────────────────────────
    # Everything here (sliders, its own Undo/Reset, Save Refinements) is
    # only ever rendered while actually refining -- symmetric with step 1
    # collapsing away while this is active.
    output$refine_step_ui <- renderUI({
      req(vertex_mode_active())
      tagList(
        div(style = 'font-size:11px;font-weight:700;color:#0f172a;margin:8px 0 6px;',
            'STEP 2: REFINE BOUNDARIES'),
        div(style = 'font-size:11px;color:#475569;', 'Smoothness'),
        sliderInput(ns('vertex_smoothness_ui'), NULL, min = 1, max = 15,
                    value = isolate(input$vertex_smoothness_ui) %||% 2,
                    step = 1, width = '100%', ticks = FALSE),
        div(style = 'font-size:11px;color:#475569;', 'Stiffness'),
        sliderInput(ns('vertex_stiffness_ui'), NULL, min = 1, max = 20,
                    value = isolate(input$vertex_stiffness_ui) %||% 6,
                    step = 1, width = '100%', ticks = FALSE),
        div(
          style = 'display:flex;gap:6px;margin-top:8px;',
          actionButton(ns('refine_undo_btn'), 'Undo',
                       class = 'btn btn-default btn-sm', style = 'flex:1;',
                       icon = icon('rotate-left')),
          actionButton(ns('refine_reset_btn'), 'Reset',
                       class = 'btn btn-default btn-sm', style = 'flex:1;')
        ),
        div(
          style = 'margin-top:6px;',
          actionButton(ns('save_refinements_btn'), 'Save Refinements',
                       class = 'btn btn-default btn-sm', width = '100%')
        )
      )
    })

    list(
      brush_m              = reactive(input$brush_m_ui),
      show_pop_raster      = reactive(isTRUE(input$show_pop_raster)),
      show_friction_raster = reactive(input$show_friction_raster),
      boundary_only        = reactive(isTRUE(input$boundary_only)),
      help_click           = reactive(input$help_btn),
      save_click           = reactive(input$save_btn),
      submit_click         = reactive(input$submit_btn),
      reset_click          = reactive(input$reset_btn),
      paint_undo_click     = reactive(input$paint_undo_btn),
      continue_click       = reactive(input$continue_btn),
      brush_minus_click    = reactive(input$brush_minus),
      brush_plus_click     = reactive(input$brush_plus),
      set_brush_limits     = set_brush_limits,
      refine_boundaries_click = reactive(input$refine_boundaries_btn),
      save_refinements_click  = reactive(input$save_refinements_btn),
      refine_undo_click       = reactive(input$refine_undo_btn),
      refine_reset_click      = reactive(input$refine_reset_btn),
      set_vertex_mode_ui      = set_vertex_mode_ui,
      vertex_smoothness       = reactive(input$vertex_smoothness_ui),
      vertex_stiffness        = reactive(input$vertex_stiffness_ui)
    )
  })
}