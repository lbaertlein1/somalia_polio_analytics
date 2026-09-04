healthAreaPopulationUI <- function(id, name_col_label = "Health Area", allow_rename = FALSE) {
  ns <- NS(id)
  tagList(
    div(class = 'rightbar-title', 'Legend & Population'),
    uiOutput(ns('legend_ui')),
    tags$hr(style = 'margin: 6px 0;'),
    div(style = 'font-size: 11px; color: #666; margin-bottom: 4px;',
        sprintf('Click a row to select that %s for editing.%s', tolower(name_col_label),
                if (allow_rename) ' Click a name to rename it.' else '')),
    rhandsontable::rHandsontableOutput(ns('pop_table'), height = "100%")
  )
}

healthAreaPopulationServer <- function(
    id,
    active_dfa_rv,
    show_pop_raster,
    show_friction_raster,
    pop_table,
    in_vertex_mode = reactive(FALSE),
    # Column header for the name column -- "Health Area" (default) or
    # "Team Name" from Team Areas. allow_rename makes that same column
    # editable and wires on_rename(old_name, new_name) to fire when the
    # user commits an edit; on_rename should return TRUE if the rename
    # was accepted (applied) or FALSE if rejected (e.g. a duplicate),
    # since a rejection needs the table forced back to the authoritative
    # value -- rhandsontable doesn't do that on its own.
    name_col_label = "Health Area",
    allow_rename = FALSE,
    on_rename = NULL
) {
  moduleServer(id, function(input, output, session) {

    # Dependency-only counter to force output$pop_table to re-render after
    # a rejected rename, even though pop_table() itself didn't change --
    # without this the table would visually stick at the rejected,
    # invalid value the user just typed, with no other reactive to
    # naturally clear it.
    table_revision <- reactiveVal(0)

    output$pop_table <- rhandsontable::renderRHandsontable({
      table_revision()
      df <- pop_table()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      is_total   <- df$area_name == "District Total"
      # No row highlighted while refining -- all boundaries are shown
      # together for editing, with no notion of a single "active" area,
      # so nothing in the table should read as selected either.
      active     <- if (isTRUE(in_vertex_mode())) "" else (active_dfa_rv() %||% "")
      sel_row_js <- (which(df$area_name == active) - 1L)
      sel_row_js <- if (length(sel_row_js) == 1) sel_row_js[1] else -1L
      total_row_js <- sum(!is_total)  # last row, 0-indexed
      
      display_df <- data.frame(
        area_name_internal = df$area_name,
        name_display        = df$area_name,
        pop_display          = as.integer(df$est_u5_pop),
        stringsAsFactors   = FALSE,
        check.names        = FALSE
      )
      names(display_df)[names(display_df) == "name_display"] <- name_col_label
      names(display_df)[names(display_df) == "pop_display"]  <- "WorldPop U5 Population"
      
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
        rhandsontable::hot_col(name_col_label,        width = 180, readOnly = !allow_rename || isTRUE(in_vertex_mode()), renderer = row_renderer) |>
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
    
    # Row click -> update active_dfa_rv (skip District Total row). Inert
    # entirely while refining -- clicking the table should do nothing
    # during vertex editing, since there's no single "active" area concept
    # there (every area's boundary is shown and editable at once).
    observeEvent(input$selected_row, ignoreInit = TRUE, {
      req(!isTRUE(in_vertex_mode()))
      df <- pop_table()
      req(!is.null(df))
      row_index <- as.integer(input$selected_row)
      req(!is.na(row_index), row_index >= 1, row_index <= nrow(df))
      area <- df$area_name[row_index]
      if (area != "District Total") active_dfa_rv(area)
    })

    # Inline rename via direct table edit -- only wired when allow_rename
    # is TRUE (Team Areas). This module doesn't validate uniqueness itself
    # (it doesn't own the full set of names); it just forwards old/new to
    # on_rename and forces a re-render if that call rejects it.
    observeEvent(input$pop_table$changes$changes, {
      req(isTRUE(allow_rename), !isTRUE(in_vertex_mode()))
      changes <- input$pop_table$changes$changes
      req(!is.null(changes))
      df <- pop_table()
      req(!is.null(df))
      for (chg in changes) {
        col_idx <- suppressWarnings(as.integer(chg[[2]]))
        if (is.na(col_idx) || col_idx != 1L) next   # only the name column (index 1) is renameable
        row_idx <- suppressWarnings(as.integer(chg[[1]])) + 1L
        if (is.na(row_idx) || row_idx < 1 || row_idx > nrow(df)) next
        if (identical(df$area_name[row_idx], "District Total")) next

        old_name <- as.character(chg[[3]])
        new_name <- trimws(as.character(chg[[4]]))
        if (identical(old_name, new_name) || !nzchar(new_name)) {
          table_revision(table_revision() + 1)
          next
        }
        accepted <- if (!is.null(on_rename)) isTRUE(on_rename(old_name, new_name)) else FALSE
        if (!accepted) table_revision(table_revision() + 1)
      }
    }, ignoreInit = TRUE)
    
    # Legend
    output$legend_ui <- renderUI({
      selected_name <- active_dfa_rv() %||% starter_dfa_names[1]
      # Same rule as the table above: no area reads as "selected" while
      # refining, so the legend's highlighted-area row is hidden entirely
      # rather than frozen at whatever was active before Refine Boundaries
      # was clicked.
      show_selected <- !isTRUE(in_vertex_mode()) &&
        !(selected_name %in% c("Inaccessible", "Unpopulated"))
      
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
