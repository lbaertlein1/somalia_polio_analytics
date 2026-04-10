healthAreaControlsUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = 'top-help',
      style = '
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 6px;
      ',
      div(
        style = '
          font-size: 16px;
          font-weight: 600;
          color: #333333;
        ',
        'Health Area Boundary Review for Microplanning'
      ),
      actionButton(ns('help_btn'), '?', width = '32px')
    ),
    div(class = 'section-gap mini-label', 'Edit Health Area'),
    selectInput(ns('active_dfa'), NULL, choices = all_dfa_names, selected = starter_dfa_names[1]),
    div(class = 'mini-label', 'Brush Size:'),
    div(
      class = 'slider-row',
      actionButton(ns('brush_minus'), '-', width = '30px'),
      div(
        class = 'slider-wrap',
        sliderInput(
          ns('brush_m_ui'), NULL,
          min = min_brush_m,
          max = max_brush_m,
          value = 300,
          step = brush_step_m,
          width = '100%'
        )
      ),
      actionButton(ns('brush_plus'), '+', width = '30px')
    ),
    checkboxInput(ns('show_pop_raster'), 'Show WorldPop U5 Population', value = show_pop_default),
    checkboxInput(ns('boundary_only'), 'Boundaries only', value = boundary_only_default),
    div(
      class = 'control-row',
      actionButton(ns('reset_btn'), 'Reset'),
      actionButton(ns('save_btn'), 'Save')
    )
  )
}

healthAreaControlsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    set_brush_limits <- function(brush_limits) {
      updateSliderInput(
        session,
        'brush_m_ui',
        min = brush_limits$min,
        max = brush_limits$max,
        value = brush_limits$value,
        step = brush_limits$step
      )
    }

    list(
      active_dfa = reactive(input$active_dfa),
      brush_m = reactive(input$brush_m_ui),
      show_pop_raster = reactive(isTRUE(input$show_pop_raster)),
      boundary_only = reactive(isTRUE(input$boundary_only)),
      help_click = reactive(input$help_btn),
      save_click = reactive(input$save_btn),
      reset_click = reactive(input$reset_btn),
      brush_minus_click = reactive(input$brush_minus),
      brush_plus_click = reactive(input$brush_plus),
      set_brush_limits = set_brush_limits
    )
  })
}
