# =============================================================================
# mod_campaign_scope_controls.R
#
# Structurally identical to healthAreaControlsUI/Server (mod_health_area_
# controls.R) -- same brush slider, same Step 1/Step 2 (paint/refine)
# layout, same smoothness/stiffness/snap-tolerance sliders, same button
# wiring -- just with scope-appropriate labels instead of health-area ones
# ("Submit Health Areas" would be actively misleading here) and without
# the health-area-specific "about_text_ui" (team-count/population-target
# messaging that has nothing to do with scope).
# =============================================================================

campaignScopeControlsUI <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      style = 'display:flex; align-items:center; justify-content:space-between; margin-bottom:8px;',
      div(style = 'font-size:14px; font-weight:600; color:#0f172a;', 'Campaign Scope'),
      actionButton(ns('help_btn'), '?', width = '28px',
                   style = 'font-size:11px;padding:0;height:24px;line-height:24px;')
    ),

    tags$p(
      style = 'font-size: 11px; color: #475569; line-height: 1.7; margin-bottom: 8px;',
      'Paint which parts of this district are actually in scope for this campaign. ',
      'Everything left ', tags$strong('Out of Scope'), ' is excluded from facilities, health areas, ',
      'and every population figure downstream.',
      tags$br(), tags$br(),
      tags$strong('Suggested approach:'), tags$br(),
      '1. Pick "In Scope" or "Out of Scope" below, then click and drag on the map to paint.', tags$br(),
      '2. ', tags$strong('Save'), ' to confirm, then ', tags$strong('Submit'), ' when done.', tags$br(),
      '3. Use ', tags$strong('Refine Boundaries'), ' to smooth the edge once it\u2019s roughly painted.'
    ),

    tags$hr(style = 'margin: 6px 0;'),

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
            min = 100, max = 10000, value = 2000,
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

    div(
      style = 'margin-bottom:8px;',
      radioButtons(ns('active_scope_value'), NULL, inline = TRUE,
                  choices = c('In Scope' = 'In Scope', 'Out of Scope' = 'Out of Scope'),
                  selected = 'In Scope'),
      checkboxInput(ns('show_pop_raster'),     'Show WorldPop U5 Population', value = show_pop_default),
      checkboxInput(ns('boundary_only'),        'Boundaries only',             value = boundary_only_default)
    ),

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
      ns('submit_btn'), 'Submit Campaign Scope',
      class = 'btn btn-primary btn-sm',
      width = '100%',
      icon  = icon('check-circle')
    ),
    div(
      style = 'font-size: 11px; color: #64748b; margin-top: 4px; line-height: 1.4;',
      'Saves scope to the database and continues to Facilities.'
    )
  )
}


campaignScopeControlsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$brush_minus, {
      val <- input$brush_m_ui %||% 2000L
      updateSliderInput(session, 'brush_m_ui', value = max(100, val - 100))
    }, ignoreInit = TRUE)

    observeEvent(input$brush_plus, {
      val <- input$brush_m_ui %||% 2000L
      updateSliderInput(session, 'brush_m_ui', value = min(10000, val + 100))
    }, ignoreInit = TRUE)

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

    output$paint_step_ui <- renderUI({
      if (isTRUE(vertex_mode_active())) {
        div(
          style = paste0('padding:8px 10px;background:#f1f5f9;border-radius:6px;',
                         'color:#64748b;font-size:12px;text-align:center;'),
          tags$strong('Step 1: Painting'), tags$br(),
          'Currently refining \u2014 click "Back to Painting" below to resume.'
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

    vertex_smoothness_val     <- reactiveVal(2)
    vertex_stiffness_val      <- reactiveVal(6)
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
        div(style = 'font-size:11px;color:#475569;', 'Snap tolerance'),
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
                       icon = icon('broom'))
        ),
        div(
          style = 'margin-top:6px;',
          actionButton(ns('save_refinements_btn'), 'Save Refinements',
                       class = 'btn btn-default btn-sm', width = '100%')
        )
      )
    })

    reset_controls <- function() {
      updateSliderInput(session, 'brush_m_ui', value = 2000)
      updateCheckboxInput(session, 'show_pop_raster', value = show_pop_default)
      updateCheckboxInput(session, 'boundary_only', value = boundary_only_default)
      vertex_mode_active(FALSE)
      vertex_smoothness_val(2)
      vertex_stiffness_val(6)
      vertex_snap_tolerance_val(20)
    }

    list(
      brush_m                 = reactive(input$brush_m_ui),
      active_scope_value      = reactive(input$active_scope_value),
      show_pop_raster         = reactive(isTRUE(input$show_pop_raster)),
      show_friction_raster    = reactive(FALSE),   # no friction concept for scope; kept for healthAreaPopulationServer's shared interface
      boundary_only           = reactive(isTRUE(input$boundary_only)),
      help_click              = reactive(input$help_btn),
      save_click               = reactive(input$save_btn),
      submit_click              = reactive(input$submit_btn),
      reset_click                = reactive(input$reset_btn),
      paint_undo_click             = reactive(input$paint_undo_btn),
      brush_minus_click              = reactive(input$brush_minus),
      brush_plus_click                 = reactive(input$brush_plus),
      refine_boundaries_click            = reactive(input$refine_boundaries_btn),
      save_refinements_click               = reactive(input$save_refinements_btn),
      refine_undo_click                      = reactive(input$refine_undo_btn),
      refine_reset_click                       = reactive(input$refine_reset_btn),
      cleanup_boundaries_click                   = reactive(input$cleanup_boundaries_btn),
      set_vertex_mode_ui                           = set_vertex_mode_ui,
      vertex_smoothness                              = reactive(input$vertex_smoothness_ui),
      vertex_stiffness                                 = reactive(input$vertex_stiffness_ui),
      vertex_snap_tolerance                              = reactive(input$vertex_snap_tolerance_ui),
      reset_controls                                       = reset_controls
    )
  })
}
