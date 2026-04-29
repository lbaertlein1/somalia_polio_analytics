facilityTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    rhandsontable::rHandsontableOutput(ns("facility_table"), height = "100%")
  )
}

facilityTableServer <- function(id, facility_data_r, selected_id_r, on_data_change) {
  moduleServer(id, function(input, output, session) {
    
    output$facility_table <- renderRHandsontable({
      
      df <- facility_data_r()
      req(!is.null(df), nrow(df) > 0)
      
      display_df <- df |>
        dplyr::transmute(
          facility_id_internal    = as.character(facility_id),
          `Facility Name`         = facility_name,
          `SIA Coordination Site` = polio_sia_coordination_site
        )
      
      selected_id     <- as.character(selected_id_r())
      selected_row    <- which(display_df$facility_id_internal == selected_id)
      selected_row_js <- if (length(selected_row) == 1) selected_row - 1L else -1L
      
      highlight_renderer <- htmlwidgets::JS(sprintf("
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
      ", selected_row_js))
      
      blank_renderer <- htmlwidgets::JS("
        function(instance, td, row, col, prop, value, cellProperties) {
          td.innerHTML = '';
          td.style.border = 'none';
          td.style.background = '';
          td.style.padding = '0px';
        }
      ")
      
      rhandsontable::rhandsontable(
        display_df,
        rowHeaders = NULL,
        stretchH   = "all",
        height     = "100%",
        useTypes   = FALSE
      ) |>
        rhandsontable::hot_col("facility_id_internal",    width = 1,   readOnly = TRUE,  renderer = blank_renderer) |>
        rhandsontable::hot_col("Facility Name",           width = 160, readOnly = FALSE, renderer = highlight_renderer) |>
        rhandsontable::hot_col("SIA Coordination Site",
                               type = "dropdown", source = c("Yes", "No"),
                               width = 130, readOnly = FALSE, renderer = highlight_renderer) |>
        rhandsontable::hot_table(
          highlightRow   = TRUE,
          columnSorting  = TRUE,
          afterSelection = htmlwidgets::JS(sprintf("
            function(r, c, r2, c2) {
              Shiny.setInputValue('%s', r + 1, {priority: 'event'});
            }
          ", session$ns("selected_row")))
        ) |>
        rhandsontable::hot_rows(rowHeights = 30) |>
        rhandsontable::hot_cols(manualColumnResize = TRUE)
    })
    
    # -------------------------------------------------------------------------
    # Write edits back — only on actual change
    # -------------------------------------------------------------------------
    observeEvent(input$facility_table, ignoreInit = TRUE, ignoreNULL = TRUE, {
      edited <- rhandsontable::hot_to_r(input$facility_table)
      req(!is.null(edited))
      
      original <- facility_data_r()
      req(!is.null(original), nrow(original) == nrow(edited))
      
      names_changed <- !identical(edited$`Facility Name`, original$facility_name)
      sia_changed   <- !identical(
        edited$`SIA Coordination Site`,
        original$polio_sia_coordination_site
      )
      if (!names_changed && !sia_changed) return()
      
      updated <- original |>
        dplyr::mutate(
          facility_name               = edited$`Facility Name`,
          polio_sia_coordination_site = edited$`SIA Coordination Site`
        )
      on_data_change(updated)
    })
    
    # -------------------------------------------------------------------------
    # Row click → sync map selection
    # -------------------------------------------------------------------------
    observeEvent(input$selected_row, ignoreInit = TRUE, {
      row_index <- as.integer(input$selected_row)
      df <- facility_data_r()
      req(!is.null(df), nrow(df) > 0)
      req(!is.na(row_index), row_index >= 1, row_index <= nrow(df))
      selected_id_r(as.character(df$facility_id[row_index]))
    })
    
  })
}

# Safe scalar display helper
.na_dash <- function(x) {
  if (length(x) == 0 || is.null(x) || (length(x) == 1 && is.na(x)) || trimws(as.character(x)) == "") "\u2014" else as.character(x)
}

facilityTableServer <- function(id, facility_data_r, selected_id_r, on_data_change) {
  moduleServer(id, function(input, output, session) {
    
    output$facility_table <- renderRHandsontable({
      
      df <- facility_data_r()
      req(!is.null(df), nrow(df) > 0)
      
      display_df <- df |>
        dplyr::transmute(
          facility_id_internal    = as.character(facility_id),
          `Facility Name`         = facility_name,
          `SIA Coordination Site` = polio_sia_coordination_site
          )
      
      selected_id     <- as.character(selected_id_r())
      selected_row    <- which(display_df$facility_id_internal == selected_id)
      selected_row_js <- if (length(selected_row) == 1) selected_row - 1L else -1L
      
      highlight_renderer <- htmlwidgets::JS(sprintf("
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
      ", selected_row_js))
      
      blank_renderer <- htmlwidgets::JS("
        function(instance, td, row, col, prop, value, cellProperties) {
          td.innerHTML = '';
          td.style.border = 'none';
          td.style.background = '';
          td.style.padding = '0px';
        }
      ")
      
      rhandsontable::rhandsontable(
        display_df,
        rowHeaders = NULL,
        stretchH   = "all",
        height     = "100%",
        useTypes   = FALSE
      ) |>
        rhandsontable::hot_col("facility_id_internal",    width = 1,   readOnly = TRUE,  renderer = blank_renderer) |>
        rhandsontable::hot_col("Facility Name",           width = 160, readOnly = FALSE, renderer = highlight_renderer) |>
        rhandsontable::hot_col("SIA Coordination Site",
                               type = "dropdown", source = c("Yes", "No"),
                               width = 130, readOnly = FALSE, renderer = highlight_renderer) |>
        rhandsontable::hot_table(
          highlightRow   = TRUE,
          columnSorting  = TRUE,
          afterSelection = htmlwidgets::JS(sprintf("
            function(r, c, r2, c2) {
              Shiny.setInputValue('%s', r + 1, {priority: 'event'});
            }
          ", session$ns("selected_row")))
        ) |>
        rhandsontable::hot_rows(rowHeights = 30) |>
        rhandsontable::hot_cols(manualColumnResize = TRUE)
    })
    
    # -------------------------------------------------------------------------
    # Write edits back — only on actual change
    # -------------------------------------------------------------------------
    observeEvent(input$facility_table, ignoreInit = TRUE, ignoreNULL = TRUE, {
      edited <- rhandsontable::hot_to_r(input$facility_table)
      req(!is.null(edited))
      
      original <- facility_data_r()
      req(!is.null(original), nrow(original) == nrow(edited))
      
      names_changed <- !identical(edited$`Facility Name`, original$facility_name)
      sia_changed   <- !identical(
        edited$`SIA Coordination Site`,
        original$polio_sia_coordination_site
      )
      if (!names_changed && !sia_changed) return()
      
      updated <- original |>
        dplyr::mutate(
          facility_name               = edited$`Facility Name`,
          polio_sia_coordination_site = edited$`SIA Coordination Site`
        )
      on_data_change(updated)
    })
    
    # -------------------------------------------------------------------------
    # Row click → sync map selection
    # -------------------------------------------------------------------------
    observeEvent(input$selected_row, ignoreInit = TRUE, {
      row_index <- as.integer(input$selected_row)
      df <- facility_data_r()
      req(!is.null(df), nrow(df) > 0)
      req(!is.na(row_index), row_index >= 1, row_index <= nrow(df))
      selected_id_r(as.character(df$facility_id[row_index]))
    })
    
  })
}

# Safe scalar display helper — never redefines %||%
.na_dash <- function(x) {
  if (length(x) == 0 || is.null(x) || (length(x) == 1 && is.na(x)) || trimws(as.character(x)) == "") "\u2014" else as.character(x)
}
