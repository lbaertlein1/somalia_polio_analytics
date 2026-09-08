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
    uiOutput(ns('about_text_ui')),

    tags$p(
      style = 'font-size: 11px; color: #475569; line-height: 1.7; margin-bottom: 8px;',
      tags$strong('Suggested approach:'),
      tags$br(),
      '1. Select a health area from the intro table\'s team-area drill-down.',
      tags$br(),
      '2. Confirm or adjust the population estimate and team count in the pop-up shown.',
      tags$br(),
      '3. Click and drag on the map to paint each team\'s territory.',
      tags$br(),
      '4. ', tags$strong('Save'), ' to confirm, then ', tags$strong('Submit'), ' when done.',
      tags$br(),
      '5. Use ', tags$strong('Refine Boundaries'), ' to smooth a team\'s edges once its cells are painted.'
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

    # ── STEP 1: Painting / STEP 2: Refine Boundaries ─────────────────────
    # Two visually distinct, mutually-exclusive steps -- same structure as
    # mod_health_area_controls.R's identical wiring.
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


teamAreaControlsServer <- function(id, campaign_id = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # needed for renderUI blocks below, which construct namespaced input ids server-side

    # Admin-configured per campaign (Admin tab -> generation settings),
    # not a fixed app default -- see mod_health_area_controls.R's
    # identical about_text_ui for the full reasoning.
    output$about_text_ui <- renderUI({
      team_target <- tryCatch(db_get_generation_setting(pool, 'target_pop_per_team', campaign_id()),
                              error = function(e) NA_real_)
      pop_text <- if (!is.na(team_target)) sprintf('~%s children', format(team_target, big.mark = ',')) else 'a population target set for this campaign'

      div(
        style = paste0('background:#f0fdf4;border-left:3px solid #0d9488;',
                       'border-radius:0 6px 6px 0;padding:7px 10px;margin-bottom:8px;'),
        tags$p(
          style = 'font-size: 11px; font-weight: 600; color: #0f172a; margin: 0 0 3px;',
          'About team areas'
        ),
        tags$p(
          style = 'font-size: 11px; color: #475569; line-height: 1.6; margin: 0;',
          'Each health area is divided into team areas, one per outreach team, each covering ',
          tags$strong(pop_text), '. Boundaries generate automatically within the health area, ',
          'then are adjusted by the group.'
        )
      )
    })

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

    # Drives output$paint_step_ui/output$refine_step_ui below via a plain
    # reactiveVal, rather than toggling visibility with shinyjs::show/hide
    # -- same fix as mod_health_area_controls.R's identical wiring
    # (shinyjs::show/hide silently does nothing without
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

    # ── Step 1: Painting ────────────────────────────────────────────────
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
    # Smoothness/stiffness tracked in their own reactiveVals -- see
    # mod_health_area_controls.R's identical wiring for why (an
    # update*Input() call sent while this UI isn't currently rendered is
    # silently dropped client-side, so a reset can't rely on the raw
    # input value surviving).
    vertex_smoothness_val <- reactiveVal(2)
    vertex_stiffness_val  <- reactiveVal(6)
    vertex_snap_tolerance_val <- reactiveVal(20)
    observeEvent(input$vertex_smoothness_ui, vertex_smoothness_val(input$vertex_smoothness_ui), ignoreInit = TRUE)
    observeEvent(input$vertex_stiffness_ui,  vertex_stiffness_val(input$vertex_stiffness_ui),  ignoreInit = TRUE)
    observeEvent(input$vertex_snap_tolerance_ui, vertex_snap_tolerance_val(input$vertex_snap_tolerance_ui), ignoreInit = TRUE)

    output$refine_step_ui <- renderUI({
      req(vertex_mode_active())
      tagList(
        div(style = 'font-size:11px;font-weight:700;color:#0f172a;margin:8px 0 6px;',
            'STEP 2: REFINE BOUNDARIES'),
        div(style = 'font-size:11px;color:#475569;', 'Smoothness'),
        sliderInput(ns('vertex_smoothness_ui'), NULL, min = 1, max = 15,
                    value = isolate(vertex_smoothness_val()),
                    step = 1, width = '100%', ticks = FALSE),
        div(style = 'font-size:11px;color:#475569;', 'Stiffness'),
        sliderInput(ns('vertex_stiffness_ui'), NULL, min = 1, max = 20,
                    value = isolate(vertex_stiffness_val()),
                    step = 1, width = '100%', ticks = FALSE),
        div(style = 'font-size:11px;color:#475569;', 'Snap tolerance',
            title = 'How close two boundary points need to be before they merge into one, as a % of the current grid cell size. Lower this for small areas if fine detail (thin necks, close-together points) keeps getting merged away.'),
        sliderInput(ns('vertex_snap_tolerance_ui'), NULL, min = 5, max = 50,
                    value = isolate(vertex_snap_tolerance_val()),
                    step = 5, width = '100%', ticks = FALSE, post = '%'),
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
          actionButton(ns('cleanup_boundaries_btn'), 'Clean Up Boundaries',
                       class = 'btn btn-default btn-sm', width = '100%',
                       icon = icon('broom'),
                       title = 'Pinches off thin peninsulas, absorbs small slivers into a neighbor, closes small gaps, and resolves overlaps -- the same cleanup Save applies automatically, run now so you can see and undo the result before saving.')
        ),
        div(
          style = 'margin-top:6px;',
          actionButton(ns('save_refinements_btn'), 'Save Refinements',
                       class = 'btn btn-default btn-sm', width = '100%')
        )
      )
    })

    # Resets every user-adjustable control back to default -- see
    # mod_health_area_controls.R's identical reset_controls() for why
    # this exists and how the parent tab calls it.
    reset_controls <- function() {
      updateSliderInput(session, 'brush_m_ui', value = 1000)
      updateCheckboxInput(session, 'show_pop_raster', value = show_pop_default)
      updateCheckboxInput(session, 'show_friction_raster', value = FALSE)
      updateCheckboxInput(session, 'boundary_only', value = boundary_only_default)
      vertex_mode_active(FALSE)
      vertex_smoothness_val(2)
      vertex_stiffness_val(6)
      vertex_snap_tolerance_val(20)
    }

    list(
      brush_m                = reactive(input$brush_m_ui),
      show_pop_raster        = reactive(isTRUE(input$show_pop_raster)),
      show_friction_raster   = reactive(input$show_friction_raster),
      boundary_only          = reactive(isTRUE(input$boundary_only)),
      help_click              = reactive(input$help_btn),
      paint_undo_click         = reactive(input$paint_undo_btn),
      save_click                = reactive(input$save_btn),
      submit_click               = reactive(input$submit_btn),
      reset_click                 = reactive(input$reset_btn),
      refine_boundaries_click = reactive(input$refine_boundaries_btn),
      save_refinements_click  = reactive(input$save_refinements_btn),
      refine_undo_click       = reactive(input$refine_undo_btn),
      refine_reset_click      = reactive(input$refine_reset_btn),
      cleanup_boundaries_click = reactive(input$cleanup_boundaries_btn),
      set_vertex_mode_ui      = set_vertex_mode_ui,
      vertex_smoothness       = reactive(input$vertex_smoothness_ui),
      vertex_stiffness        = reactive(input$vertex_stiffness_ui),
      vertex_snap_tolerance   = reactive(input$vertex_snap_tolerance_ui),
      reset_controls          = reset_controls
    )
  })
}
