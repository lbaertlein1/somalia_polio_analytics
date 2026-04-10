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
    
    output$facility_table <- renderRHandsontable({
      
      df <- facility_data_r()
      req(!is.null(df))
      
      if (nrow(df) == 0) {
        return(NULL)
      }
      
      display_df <- df |>
        dplyr::transmute(
          facility_id_internal = as.character(facility_id),
          `Facility Name` = facility_name,
          `Facility Type` = facility_type,
          `Operational Status` = operational,
          `RI Services` = ri_services,
          `SIA Coordination Site` = polio_sia_coordination_site
        )
      
      selected_id <- as.character(selected_id_r())
      selected_row <- which(display_df$facility_id_internal == selected_id)
      selected_row_js <- if (length(selected_row) == 1) selected_row - 1 else -1
      
      row_highlight_renderer <- htmlwidgets::JS(
        sprintf(
          "
          function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            if (row === %d) {
              td.style.background = '#FFF176';
              td.style.fontWeight = '600';
            } else {
              td.style.background = '';
              td.style.fontWeight = '';
            }
          }
          ",
          selected_row_js
        )
      )
      
      blank_renderer <- htmlwidgets::JS(
        "
        function(instance, td, row, col, prop, value, cellProperties) {
          td.innerHTML = '';
          td.style.border = 'none';
          td.style.background = '';
          td.style.padding = '0px';
        }
        "
      )
      
      rhandsontable::rhandsontable(
        display_df,
        rowHeaders = NULL,
        stretchH = "none",
        height = "100%",
        width = "100%",
        useTypes = FALSE
      ) |>
        rhandsontable::hot_col(
          "facility_id_internal",
          width = 1,
          readOnly = TRUE,
          renderer = blank_renderer
        ) |>
        rhandsontable::hot_col(
          "Facility Name",
          width = 220,
          renderer = row_highlight_renderer
        ) |>
        rhandsontable::hot_col(
          "Facility Type",
          type = "dropdown",
          source = c("Health Post", "Health Center", "Hospital"),
          width = 140,
          renderer = row_highlight_renderer
        ) |>
        rhandsontable::hot_col(
          "Operational Status",
          type = "dropdown",
          source = c("Operational", "Not Operational"),
          width = 150,
          renderer = row_highlight_renderer
        ) |>
        rhandsontable::hot_col(
          "RI Services",
          type = "dropdown",
          source = c("Yes", "No"),
          width = 110,
          renderer = row_highlight_renderer
        ) |>
        rhandsontable::hot_col(
          "SIA Coordination Site",
          type = "dropdown",
          source = c("Yes", "No"),
          width = 170,
          renderer = row_highlight_renderer
        ) |>
        rhandsontable::hot_table(
          highlightRow = TRUE,
          columnSorting = TRUE,
          colHeaders = c(
            "",
            "Facility Name",
            "Facility Type",
            "Operational Status",
            "RI Services",
            "SIA Coordination Site"
          ),
          afterSelection = htmlwidgets::JS(
            sprintf(
              "
              function(r, c, r2, c2) {
                Shiny.setInputValue('%s', r + 1, {priority: 'event'});
              }
              ",
              session$ns("selected_row")
            )
          )
        ) |>
        rhandsontable::hot_rows(
          rowHeights = 30
        ) |>
        rhandsontable::hot_context_menu(FALSE) |>
        rhandsontable::hot_cols(
          manualColumnResize = TRUE
        )
    })
    
    observeEvent(
      input$facility_table,
      ignoreInit = TRUE,
      {
        edited_display_df <- rhandsontable::hot_to_r(input$facility_table)
        req(!is.null(edited_display_df))
        
        original_df <- facility_data_r()
        req(!is.null(original_df))
        req(nrow(original_df) == nrow(edited_display_df))
        
        updated_df <- original_df |>
          dplyr::mutate(
            facility_name = edited_display_df$`Facility Name`,
            facility_type = edited_display_df$`Facility Type`,
            operational = edited_display_df$`Operational Status`,
            ri_services = edited_display_df$`RI Services`,
            polio_sia_coordination_site = edited_display_df$`SIA Coordination Site`
          )
        
        on_data_change(updated_df)
      }
    )
    
    observeEvent(
      input$selected_row,
      ignoreInit = TRUE,
      {
        row_index <- as.integer(input$selected_row)
        
        df <- facility_data_r()
        req(!is.null(df), nrow(df) > 0)
        req(!is.na(row_index))
        req(row_index >= 1, row_index <= nrow(df))
        
        selected_id_r(as.character(df$facility_id[row_index]))
      }
    )
  })
}