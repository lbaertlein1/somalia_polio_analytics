# =============================================================================
# mod_facility_table.R  —  two-table facility panel
#   Top:    Outreach coordination sites  (uncheck to remove)
#   Bottom: remaining facilities         (check to add as coordination site)
# =============================================================================

facilityTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = paste0('font-size:11px;font-weight:700;color:#0d9488;',
                     'text-transform:uppercase;letter-spacing:.06em;',
                     'margin-bottom:6px;padding-bottom:4px;',
                     'border-bottom:2px solid #0d9488;'),
      'Outreach Coordination Sites'
    ),
    rhandsontable::rHandsontableOutput(ns("sia_table"), width = "100%", height = "auto"),
    uiOutput(ns("sia_empty_msg")),
    
    tags$hr(style = 'margin:14px 0 10px;border-color:#e2e8f0;'),
    
    div(
      style = paste0('font-size:11px;font-weight:700;color:#475569;',
                     'text-transform:uppercase;letter-spacing:.06em;',
                     'margin-bottom:6px;padding-bottom:4px;',
                     'border-bottom:1px solid #cbd5e1;'),
      'All Health Facilities'
    ),
    rhandsontable::rHandsontableOutput(ns("all_table"), width = "100%", height = "auto"),
    uiOutput(ns("all_empty_msg"))
  )
}


# =============================================================================
# Server
# =============================================================================

facilityTableServer <- function(id, facility_data_r, selected_id_r, on_data_change) {
  moduleServer(id, function(input, output, session) {
    
    # ── Shared helpers ────────────────────────────────────────────────────────
    
    .sia_rows <- function(df) {
      df |> dplyr::filter(!is.na(polio_sia_coordination_site) &
                            polio_sia_coordination_site == "Yes")
    }
    .non_sia_rows <- function(df) {
      df |> dplyr::filter(is.na(polio_sia_coordination_site) |
                            polio_sia_coordination_site != "Yes")
    }
    
    .to_display <- function(sub_df, sia_val) {
      sub_df |>
        dplyr::transmute(
          facility_id_internal = as.character(facility_id),
          `Facility Name`      = facility_name,
          `Coord. Site`        = sia_val
        )
    }
    
    .sel_row_js <- function(display_df) {
      sid <- as.character(selected_id_r())
      row <- which(display_df$facility_id_internal == sid)
      if (length(row) == 1L) row[[1L]] - 1L else -1L
    }
    
    .renderers <- function(r_js) {
      list(
        text = htmlwidgets::JS(sprintf("
          function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            td.style.background = (row === %d) ? '#FFF176' : '';
            td.style.fontWeight = (row === %d) ? '600'     : '';
          }", r_js, r_js)),
        checkbox = htmlwidgets::JS(sprintf("
          function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
            td.style.background = (row === %d) ? '#FFF176' : '';
          }", r_js)),
        blank = htmlwidgets::JS("
          function(instance, td, row, col, prop, value, cellProperties) {
            td.innerHTML = '';
            td.style.border = 'none'; td.style.background = '';
            td.style.padding = '0px';
          }")
      )
    }
    
    .build_hot <- function(display_df, after_sel_input_id) {
      r_js <- .sel_row_js(display_df)
      rnd  <- .renderers(r_js)
      rhandsontable::rhandsontable(display_df,
                                   rowHeaders = NULL, stretchH = "all", useTypes = FALSE) |>
        rhandsontable::hot_col("facility_id_internal",
                               width = 1, readOnly = TRUE, renderer = rnd$blank) |>
        rhandsontable::hot_col("Facility Name",
                               width = 160, readOnly = FALSE, renderer = rnd$text) |>
        rhandsontable::hot_col("Coord. Site",
                               type = "checkbox",
                               checkedTemplate   = TRUE,
                               uncheckedTemplate = FALSE,
                               width = 60, readOnly = FALSE, renderer = rnd$checkbox) |>
        rhandsontable::hot_table(
          highlightRow  = TRUE,
          columnSorting = TRUE,
          afterSelection = htmlwidgets::JS(sprintf("
            function(r, c, r2, c2) {
              Shiny.setInputValue('%s', r + 1, {priority: 'event'});
              var hot = this;
              setTimeout(function() {
                var nR = hot.countRows(), nC = hot.countCols();
                for (var i = 0; i < nR; i++) for (var j = 0; j < nC; j++) {
                  var td = hot.getCell(i, j); if (!td) continue;
                  td.style.backgroundColor = (i === r) ? '#FFF176' : '';
                  td.style.fontWeight      = (i === r && j === 1) ? '600' : '';
                }
              }, 0);
            }", after_sel_input_id))
        ) |>
        rhandsontable::hot_rows(rowHeights = 30) |>
        rhandsontable::hot_cols(manualColumnResize = TRUE)
    }
    
    # ── Write-back: apply name + SIA changes from an edited display_df ───────
    # new_sia_status = "Yes"  → look for newly-checked rows   (All  table)
    # new_sia_status = "No"   → look for newly-unchecked rows (SIA  table)
    # Returns early when nothing actually changed (echo suppression).
    
    .apply_edits <- function(edited, original, new_sia_status) {
      req(!is.null(edited), !is.null(original))
      ids <- edited$facility_id_internal
      
      sia_logical <- edited$`Coord. Site`
      # For SIA table: changed_ids are the rows that are now FALSE (unchecked)
      # For All table: changed_ids are the rows that are now TRUE (checked)
      flipped_to <- if (new_sia_status == "No") FALSE else TRUE
      changed_ids <- ids[!is.na(sia_logical) & sia_logical == flipped_to]
      
      # Check for name changes against the corresponding original rows
      orig_sub   <- original[as.character(original$facility_id) %in% ids, , drop = FALSE]
      orig_names <- orig_sub$facility_name[
        match(ids, as.character(orig_sub$facility_id))
      ]
      names_changed <- !identical(edited$`Facility Name`, orig_names)
      
      if (length(changed_ids) == 0 && !names_changed) return()
      
      updated <- original |>
        dplyr::mutate(
          facility_name = ifelse(
            as.character(facility_id) %in% ids,
            edited$`Facility Name`[match(as.character(facility_id), ids)],
            facility_name
          ),
          polio_sia_coordination_site = ifelse(
            as.character(facility_id) %in% changed_ids,
            new_sia_status,
            polio_sia_coordination_site
          )
        )
      on_data_change(updated)
    }
    
    # ── SIA table ─────────────────────────────────────────────────────────────
    
    output$sia_empty_msg <- renderUI({
      df <- facility_data_r()
      if (is.null(df) || nrow(.sia_rows(df)) > 0) return(NULL)
      div(style = 'font-size:11px;color:#94a3b8;padding:6px 2px;font-style:italic;',
          'No outreach coordination sites selected yet.')
    })
    
    output$sia_table <- renderRHandsontable({
      df <- facility_data_r()
      req(!is.null(df))
      sia_df <- .sia_rows(df)
      req(nrow(sia_df) > 0)
      .build_hot(.to_display(sia_df, sia_val = TRUE), session$ns("sia_sel_row"))
    })
    
    observeEvent(input$sia_table, ignoreInit = TRUE, ignoreNULL = TRUE, {
      .apply_edits(
        edited         = rhandsontable::hot_to_r(input$sia_table),
        original       = facility_data_r(),
        new_sia_status = "No"
      )
    })
    
    observeEvent(input$sia_sel_row, ignoreInit = TRUE, {
      idx <- as.integer(input$sia_sel_row)
      df  <- facility_data_r(); req(!is.null(df))
      sub <- .sia_rows(df)
      req(!is.na(idx), idx >= 1L, idx <= nrow(sub))
      selected_id_r(as.character(sub$facility_id[idx]))
    })
    
    # ── All-facilities table ──────────────────────────────────────────────────
    
    output$all_empty_msg <- renderUI({
      df <- facility_data_r()
      if (is.null(df) || nrow(.non_sia_rows(df)) > 0) return(NULL)
      div(style = 'font-size:11px;color:#94a3b8;padding:6px 2px;font-style:italic;',
          'All facilities are outreach coordination sites.')
    })
    
    output$all_table <- renderRHandsontable({
      df <- facility_data_r()
      req(!is.null(df))
      non_sia_df <- .non_sia_rows(df)
      req(nrow(non_sia_df) > 0)
      .build_hot(.to_display(non_sia_df, sia_val = FALSE), session$ns("all_sel_row"))
    })
    
    observeEvent(input$all_table, ignoreInit = TRUE, ignoreNULL = TRUE, {
      .apply_edits(
        edited         = rhandsontable::hot_to_r(input$all_table),
        original       = facility_data_r(),
        new_sia_status = "Yes"
      )
    })
    
    observeEvent(input$all_sel_row, ignoreInit = TRUE, {
      idx <- as.integer(input$all_sel_row)
      df  <- facility_data_r(); req(!is.null(df))
      sub <- .non_sia_rows(df)
      req(!is.na(idx), idx >= 1L, idx <= nrow(sub))
      selected_id_r(as.character(sub$facility_id[idx]))
    })
    
  })
}


.na_dash <- function(x) {
  if (length(x) == 0 || is.null(x) || (length(x) == 1 && is.na(x)) ||
      trimws(as.character(x)) == "") "\u2014" else as.character(x)
}