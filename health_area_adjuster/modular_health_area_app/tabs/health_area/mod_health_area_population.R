healthAreaPopulationUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = 'rightbar-title', 'Legend & Population'),
    uiOutput(ns('legend_ui')),
    tags$hr(style = 'margin: 6px 0;'),
    div(style = 'font-size: 11px; color: #666; margin-bottom: 4px;',
        'Click a row to select that health area for editing.'),
    rhandsontable::rHandsontableOutput(ns('pop_table'), height = "100%")
  )
}

healthAreaPopulationServer <- function(
    id,
    active_dfa_rv,
    show_pop_raster,
    show_friction_raster,
    pop_table
) {
  moduleServer(id, function(input, output, session) {
    
    output$pop_table <- rhandsontable::renderRHandsontable({
      df <- pop_table()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      is_total   <- df$area_name == "District Total"
      active     <- active_dfa_rv() %||% ""
      sel_row_js <- (which(df$area_name == active) - 1L)
      sel_row_js <- if (length(sel_row_js) == 1) sel_row_js[1] else -1L
      total_row_js <- sum(!is_total)  # last row, 0-indexed
      
      display_df <- data.frame(
        area_name_internal = df$area_name,
        `Health Area`      = df$area_name,
        `WorldPop U5 Population`      = as.integer(df$est_u5_pop),
        stringsAsFactors   = FALSE,
        check.names        = FALSE
      )
      
      row_renderer <- htmlwidgets::JS(sprintf("
  function(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.color = '#333';
    if (row === %d) {
      td.style.background = '#e0e0e0';
      td.style.fontWeight = '700';
      td.style.borderTop  = '2px solid #999';
    } else if (row === %d) {
      td.style.background = '#FFF176';
      td.style.fontWeight = '600';
    } else {
      td.style.background = '';
      td.style.fontWeight = '';
    }
  }
", total_row_js, sel_row_js))
      
      blank_renderer <- htmlwidgets::JS("
        function(instance, td, row, col, prop, value, cellProperties) {
          td.innerHTML = ''; td.style.border = 'none';
          td.style.background = ''; td.style.padding = '0px';
        }
      ")
      
      rhandsontable::rhandsontable(
        display_df,
        rowHeaders = NULL,
        stretchH   = "all",
        height     = 300,
        useTypes   = FALSE
      ) |>
        rhandsontable::hot_col("area_name_internal", width = 1,   readOnly = TRUE, renderer = blank_renderer) |>
        rhandsontable::hot_col("Health Area",        width = 180, readOnly = TRUE, renderer = row_renderer) |>
        rhandsontable::hot_col("WorldPop U5 Population",        width = 110,  readOnly = TRUE, renderer = row_renderer) |>
        rhandsontable::hot_table(
          highlightRow   = TRUE,
          columnSorting  = FALSE,
          afterSelection = htmlwidgets::JS(sprintf("
            function(r, c, r2, c2) {
              var nRows = this.countRows();
              if (r < nRows - 1) {
                Shiny.setInputValue('%s', r + 1, {priority: 'event'});
              }
            }
          ", session$ns("selected_row")))
        ) |>
        rhandsontable::hot_rows(rowHeights = 28) |>
        rhandsontable::hot_cols(manualColumnResize = TRUE)
    })
    
    # Row click -> update active_dfa_rv (skip District Total row)
    observeEvent(input$selected_row, ignoreInit = TRUE, {
      df <- pop_table()
      req(!is.null(df))
      row_index <- as.integer(input$selected_row)
      req(!is.na(row_index), row_index >= 1, row_index <= nrow(df))
      area <- df$area_name[row_index]
      if (area != "District Total") active_dfa_rv(area)
    })
    
    # Legend
    output$legend_ui <- renderUI({
      selected_name <- active_dfa_rv() %||% starter_dfa_names[1]
      show_selected <- !(selected_name %in% c("Inaccessible", "Unpopulated"))
      
      raster_cols     <- pop_palette(5)
      raster_labels   <- c("Low", "", "", "", "High")
      friction_cols   <- c("#FFFFFF","#440154","#3B528B","#21918C","#5DC863",
                           "#FDE725","#FDB863","#E66101","#B2182B")
      friction_labels <- c("0","0.05","0.1","0.2","0.4","0.6","0.8","<1","1")
      
      tagList(
        div(
          class = "legend-wrap",
          if (show_selected) div(class = "legend-row",
                                 tags$span(class = "legend-box", style = paste0("background:", selected_fill_color, ";")),
                                 tags$span(selected_name)),
          div(class = "legend-row",
              tags$span(class = "legend-box", style = paste0("background:", nonselected_fill_color, ";")),
              tags$span("Other Health Areas")),
          div(class = "legend-row",
              tags$span(class = "legend-box",
                        style = paste0("background:", special_fill_colors[["Inaccessible"]],
                                       "; border-color:", special_fill_colors[["Inaccessible"]], ";")),
              tags$span("Inaccessible")),
          div(class = "legend-row",
              tags$span(class = "legend-box", style = "background:#FFFFFF;"),
              tags$span("Unpopulated")),
          if (isTRUE(show_pop_raster())) tagList(
            tags$div(style = "height:6px;"),
            tags$div(class = "mini-label", style = "margin-bottom:4px;", "WorldPop U5 Population"),
            tags$div(style = "display:flex; gap:0; margin-bottom:3px;",
                     lapply(raster_cols, function(clr) tags$div(style = paste0(
                       "flex:1; height:10px; background:", clr,
                       "; border-top:1px solid #999; border-bottom:1px solid #999;")))),
            tags$div(style = "display:flex; justify-content:space-between; font-size:10px; color:#666;",
                     lapply(raster_labels, tags$span))
          ),
          if (isTRUE(show_friction_raster())) tagList(
            tags$div(style = "height:6px;"),
            tags$div(class = "mini-label", style = "margin-bottom:4px;", "Friction surface"),
            tags$div(style = "display:flex; gap:0; margin-bottom:3px;",
                     lapply(friction_cols, function(clr) tags$div(style = paste0(
                       "flex:1; height:10px; background:", clr,
                       "; border-top:1px solid #999; border-bottom:1px solid #999;")))),
            tags$div(style = "display:flex; justify-content:space-between; font-size:10px; color:#666;",
                     lapply(friction_labels, tags$span)),
            tags$div(class = "mini-label", style = "margin-top:3px; color:#666;",
                     "Low = easier movement, High = harder movement")
          )
        )
      )
    })
  })
}
