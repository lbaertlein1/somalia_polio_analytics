healthAreaMapUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(id = ns('paint_map')),
    div(
      id = ns('loading_overlay'),
      div(
        style = '
          background: rgba(255,255,255,0.96);
          padding: 12px 18px;
          border: 1px solid #D9D9D9;
          border-radius: 6px;
          box-shadow: 0 1px 6px rgba(0,0,0,0.08);
          font-size: 16px;
          font-weight: 600;
          color: #333333;
        ',
        'Loading district data...'
      )
    )
  )
}

healthAreaMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    list(
      map_ready = reactive(input$paint_map_ready),
      assignments = reactive(input$paint_assignments),
      map_id = session$ns('paint_map'),
      loading_overlay_id = session$ns('loading_overlay'),
      ready_input_id = session$ns('paint_map_ready'),
      assignments_input_id = session$ns('paint_assignments')
    )
  })
}
