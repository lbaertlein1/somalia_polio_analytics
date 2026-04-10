facilityTableUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = 'rightbar-title', 'Facility attributes'),
    DT::DTOutput(ns('facility_table'))
  )
}

facilityTableServer <- function(id, facility_data_r, selected_id_r, on_table_edit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$facility_table <- DT::renderDT({
      df <- facility_data_r()
      req(!is.null(df))
      
      show_df <- df |>
        dplyr::select(
          facility_id,
          facility_name,
          operational,
          ri_services,
          facility_type,
          polio_sia_coordination_site,
          lon,
          lat
        )
      
      DT::datatable(
        show_df,
        rownames = FALSE,
        selection = 'single',
        editable = list(
          target = 'cell',
          disable = list(columns = c(0, 6, 7))
        ),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'tip'
        )
      )
    })
    
    observeEvent(input$facility_table_cell_edit, {
      info <- input$facility_table_cell_edit
      req(!is.null(info$row), !is.null(info$col), !is.null(info$value))
      
      on_table_edit(
        row = info$row,
        col = info$col,
        value = info$value
      )
    })
    
    observeEvent(input$facility_table_rows_selected, {
      sel <- input$facility_table_rows_selected
      df <- facility_data_r()
      req(length(sel) == 1, nrow(df) >= sel[1])
      
      selected_id_r(as.character(df$facility_id[sel[1]]))
    })
  })
}