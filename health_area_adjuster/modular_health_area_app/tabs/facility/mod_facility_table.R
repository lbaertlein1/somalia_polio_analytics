
facilityTableUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "rightbar-title",
      "Facility attributes"
    ),
    rhandsontable::rHandsontableOutput(ns("facility_table"))
  )
}

facilityTableServer <- function(
    id,
    facility_data_r,
    selected_id_r,
    on_data_change
) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ----------------------------
    # Render table
    # ----------------------------
    
    output$facility_table <- renderRHandsontable({
      
      df <- facility_data_r()
      req(!is.null(df))
      
      if (nrow(df) == 0) {
        return(NULL)
      }
      
      display_df <- df |>
        dplyr::select(
          facility_id,
          facility_name,
          facility_type,
          operational,
          ri_services,
          polio_sia_coordination_site
        )
      
      selected_id <- selected_id_r()
      
      selected_row <- which(
        display_df$facility_id == selected_id
      )
      
      rhandsontable(
        display_df,
        rowHeaders = NULL,
        stretchH = "all",
        height = "100%"
      ) |>
        
        hot_col(
          "facility_id",
          readOnly = TRUE
        ) |>
        
        hot_col(
          "facility_name",
          colHeaders = "Name"
        ) |>
        
        hot_col(
          "facility_type",
          colHeaders = "Type",
          type = "dropdown",
          source = c(
            "Health Post",
            "Health Center",
            "Hospital"
          )
        ) |>
        
        hot_col(
          "operational",
          colHeaders = "Operational",
          type = "dropdown",
          source = c(
            "Operational",
            "Not Operational"
          )
        ) |>
        
        hot_col(
          "ri_services",
          colHeaders = "RI Services",
          type = "dropdown",
          source = c(
            "Yes",
            "No"
          )
        ) |>
        
        hot_col(
          "polio_sia_coordination_site",
          colHeaders = "SIA Coordination Site",
          type = "dropdown",
          source = c(
            "Yes",
            "No"
          )
        ) |>
        
        hot_table(
          highlightRow = TRUE
        ) |>
        
        hot_rows(
          rowHeights = 28
        ) |>
        
        hot_context_menu(FALSE) |>
        
        hot_cols(
          manualColumnResize = TRUE
        )
    })
    
    # ----------------------------
    # Push edits back to data
    # ----------------------------
    
    observeEvent(
      input$facility_table,
      ignoreInit = TRUE,
      {
        
        new_df <- hot_to_r(
          input$facility_table
        )
        
        req(!is.null(new_df))
        
        on_data_change(new_df)
      }
    )
    
    # ----------------------------
    # Sync row selection to map
    # ----------------------------
    
    observeEvent(
      input$facility_table_select,
      {
        
        sel <- input$facility_table_select
        
        if (length(sel$r) == 1) {
          
          df <- facility_data_r()
          
          selected_id_r(
            df$facility_id[sel$r]
          )
        }
      }
    )
  })
}