teamAreaControlsUI <- function(id) {
  ns <- NS(id)

  tagList(

    # ── Title row ─────────────────────────────────────────────────────────────
    div(
      style = 'display:flex; align-items:center; justify-content:space-between; margin-bottom:8px;',
      div(style = 'font-size:14px; font-weight:600; color:#0f172a;', 'Team Areas'),
      actionButton(ns('help_btn'), '?', width = '28px',
                   style = 'font-size:11px;padding:0;height:24px;line-height:24px;')
    ),

    # ── Instructions ──────────────────────────────────────────────────────────
    div(
      style = paste0('background:#f0fdf4;border-left:3px solid #0d9488;',
                     'border-radius:0 6px 6px 0;padding:7px 10px;margin-bottom:8px;'),
      tags$p(
        style = 'font-size: 11px; font-weight: 600; color: #0f172a; margin: 0 0 3px;',
        'About team areas'
      ),
      tags$p(
        style = 'font-size: 11px; color: #475569; line-height: 1.6; margin: 0;',
        'Each health area is divided into team areas, one per outreach team. ',
        'Boundaries are generated automatically within the health area, then adjusted by the group.'
      )
    ),

    tags$hr(style = 'margin: 6px 0;'),

    # ── Health area selector ─────────────────────────────────────────────────
    div(class = 'mini-label', 'Health area'),
    selectInput(ns('health_area'), NULL, choices = character(0), width = '100%'),
    uiOutput(ns('health_area_status')),

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
            min = 50, max = 5000, value = 1000,
            step = 50, width = '100%',
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

    # ── Undo / Reset / Save ──────────────────────────────────────────────────
    div(
      style = 'display:flex;gap:6px;margin-bottom:8px;',
      actionButton(ns('undo_btn'), 'Undo',
                   class = 'btn btn-default btn-sm', style = 'flex:1;', icon = icon('rotate-left')),
      actionButton(ns('reset_btn'), 'Reset',
                   class = 'btn btn-default btn-sm', style = 'flex:1;'),
      actionButton(ns('save_btn'),  'Save',
                   class = 'btn btn-default btn-sm', style = 'flex:1;')
    ),

    # ── Boundary refinement (vertex editing) ─────────────────────────────
    div(
      id = ns('refine_controls'),
      actionButton(
        ns('refine_boundaries_btn'), 'Refine Boundaries',
        class = 'btn btn-default btn-sm', width = '100%',
        icon = icon('draw-polygon')
      ),
      # Rendered via uiOutput/renderUI rather than a static div toggled by
      # shinyjs::show/hide -- that approach silently does nothing if
      # shinyjs::useShinyjs() isn't set up in the app's UI, which is easy
      # to miss and leaves these permanently invisible with no error.
      # renderUI needs no extra setup and fails loudly (a normal R error)
      # if something's actually wrong, instead of just never appearing.
      uiOutput(ns('refine_sliders_ui')),
      uiOutput(ns('save_refinements_ui'))
    ),

    tags$hr(style = 'margin: 6px 0;'),

    actionButton(
      ns('submit_btn'), 'Submit Team Areas',
      class = 'btn btn-primary btn-sm',
      width = '100%',
      icon  = icon('check-circle')
    ),
    div(
      style = 'font-size: 11px; color: #64748b; margin-top: 4px; line-height: 1.4;',
      'Saves team area boundaries for all health areas worked on so far to the database.'
    )
  )
}


teamAreaControlsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # needed for renderUI blocks below, which construct namespaced input ids server-side

    BRUSH_STEP <- 50L
    BRUSH_MIN  <- 50L
    BRUSH_MAX  <- 5000L

    observeEvent(input$brush_minus, {
      val <- input$brush_m_ui %||% 1000L
      updateSliderInput(session, 'brush_m_ui', value = max(BRUSH_MIN, val - BRUSH_STEP))
    }, ignoreInit = TRUE)

    observeEvent(input$brush_plus, {
      val <- input$brush_m_ui %||% 1000L
      updateSliderInput(session, 'brush_m_ui', value = min(BRUSH_MAX, val + BRUSH_STEP))
    }, ignoreInit = TRUE)

    set_health_area_choices <- function(choices, selected = NULL) {
      updateSelectInput(session, 'health_area', choices = choices,
                        selected = selected %||% (if (length(choices) > 0) choices[[1]] else NULL))
    }

    # Drives output$refine_sliders_ui/output$save_refinements_ui below via
    # a plain reactiveVal, rather than toggling visibility with
    # shinyjs::show/hide -- same fix as mod_health_area_controls.R's
    # identical wiring (shinyjs::show/hide silently does nothing without
    # shinyjs::useShinyjs() set up in the app's UI).
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

    output$refine_sliders_ui <- renderUI({
      req(vertex_mode_active())
      tagList(
        div(style = 'margin-top:8px;font-size:11px;color:#475569;', 'Smoothness'),
        sliderInput(ns('vertex_smoothness_ui'), NULL, min = 1, max = 15,
                    value = isolate(input$vertex_smoothness_ui) %||% 2,
                    step = 1, width = '100%', ticks = FALSE),
        div(style = 'font-size:11px;color:#475569;', 'Stiffness'),
        sliderInput(ns('vertex_stiffness_ui'), NULL, min = 1, max = 20,
                    value = isolate(input$vertex_stiffness_ui) %||% 6,
                    step = 1, width = '100%', ticks = FALSE)
      )
    })

    output$save_refinements_ui <- renderUI({
      req(vertex_mode_active())
      div(
        style = 'margin-top:6px;',
        actionButton(ns('save_refinements_btn'), 'Save Refinements',
                     class = 'btn btn-default btn-sm', width = '100%')
      )
    })

    list(
      health_area           = reactive(input$health_area),
      set_health_area_choices = set_health_area_choices,
      brush_m                = reactive(input$brush_m_ui),
      show_pop_raster        = reactive(isTRUE(input$show_pop_raster)),
      show_friction_raster   = reactive(input$show_friction_raster),
      boundary_only          = reactive(isTRUE(input$boundary_only)),
      help_click              = reactive(input$help_btn),
      undo_click               = reactive(input$undo_btn),
      save_click                = reactive(input$save_btn),
      submit_click               = reactive(input$submit_btn),
      reset_click                 = reactive(input$reset_btn),
      set_status_ui                = function(ui) { output$health_area_status <- renderUI(ui) },
      refine_boundaries_click = reactive(input$refine_boundaries_btn),
      save_refinements_click  = reactive(input$save_refinements_btn),
      set_vertex_mode_ui      = set_vertex_mode_ui,
      vertex_smoothness       = reactive(input$vertex_smoothness_ui),
      vertex_stiffness        = reactive(input$vertex_stiffness_ui)
    )
  })
}
