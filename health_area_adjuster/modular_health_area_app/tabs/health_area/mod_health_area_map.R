healthAreaMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(id = ns("paint_map")),
    div(
      id = ns("loading_overlay"),
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
      map_ready            = reactive(input$paint_map_ready),
      assignments          = reactive(input$paint_assignments),
      # New: the vertex-edited boundary GeoJSON emitted by
      # paint-app.js's emitVertexGeojson(). Input id is constructed on
      # the JS side as `${assignmentsInputId}_vertex_geojson` -- since
      # ns() is just string-prefixing, ns("paint_assignments_vertex_geojson")
      # matches that construction exactly.
      vertex_geojson       = reactive(input$paint_assignments_vertex_geojson),
      # Paint-mode undo stack depth, pushed from paint-app.js's own
      # emitUndoCount() -- same pattern as mod_team_area_map.R's identical
      # wiring, added now that Health Areas has its own Undo button.
      undo_count           = reactive(input$paint_undo_count),
      # Refine-mode (vertex) undo stack depth, emitted from
      # paint-app.js's emitVertexUndoCount() via a derived input id
      # (${assignmentsInputId}_vertex_undo_count) rather than a
      # separately-passed explicit id, matching how vertex_geojson above
      # is already wired.
      vertex_undo_count    = reactive(input$paint_assignments_vertex_undo_count),
      map_id               = session$ns("paint_map"),
      loading_overlay_id   = session$ns("loading_overlay"),
      ready_input_id       = session$ns("paint_map_ready"),
      assignments_input_id = session$ns("paint_assignments"),
      undo_count_input_id  = session$ns("paint_undo_count")
    )
  })
}
