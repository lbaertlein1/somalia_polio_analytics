facilityTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    rhandsontable::rHandsontableOutput(ns("facility_table"), height = "100%"),
    uiOutput(ns("facility_modal"))
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
          `SIA Coordination Site` = polio_sia_coordination_site,
          `View`                  = as.character(seq_len(dplyr::n())),
          `Edit`                  = odk_edit_link
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
      
      view_renderer <- htmlwidgets::JS(sprintf("
        function(instance, td, row, col, prop, value, cellProperties) {
          td.innerHTML = '<button style=\"font-size:11px;padding:2px 8px;cursor:pointer;' +
            'border:1px solid #1565C0;border-radius:3px;background:#fff;color:#1565C0;\"' +
            ' onclick=\"Shiny.setInputValue(\\'%s\\', ' + value + ', {priority:\\'event\\'})\">' +
            'View</button>';
          if (row === %d) { td.style.background = '#FFF176'; }
          else { td.style.background = ''; }
        }
      ", session$ns("view_row"), selected_row_js))
      
      edit_renderer <- htmlwidgets::JS(sprintf("
        function(instance, td, row, col, prop, value, cellProperties) {
          if (value) {
            td.innerHTML = '<a href=\"' + value + '\" target=\"_blank\"' +
              ' style=\"font-size:11px;color:#c0392b;\">Edit</a>';
          } else {
            td.innerHTML = '<span style=\"font-size:11px;color:#aaa;\">—</span>';
          }
          if (row === %d) { td.style.background = '#FFF176'; }
          else { td.style.background = ''; }
        }
      ", selected_row_js))
      
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
        rhandsontable::hot_col("View", width = 55,  readOnly = TRUE, renderer = view_renderer) |>
        rhandsontable::hot_col("Edit", width = 40,  readOnly = TRUE, renderer = edit_renderer) |>
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
    # View modal — full facility details including type, ownership, etc.
    # -------------------------------------------------------------------------
    observeEvent(input$view_row, ignoreInit = TRUE, ignoreNULL = TRUE, {
      row_index <- as.integer(input$view_row)
      df <- facility_data_r()
      req(!is.null(df), nrow(df) > 0, row_index >= 1, row_index <= nrow(df))
      
      fac <- df[row_index, ]
      is_app_added <- grepl("^app_", fac$facility_id)
      
      showModal(modalDialog(
        title     = fac$facility_name,
        size      = "m",
        easyClose = TRUE,
        footer    = tagList(
          if (!is_app_added && !is.na(fac$odk_edit_link)) {
            tags$a(
              href   = fac$odk_edit_link,
              target = "_blank",
              class  = "btn btn-default",
              icon("pen-to-square"), " Edit in ODK"
            )
          },
          modalButton("Close")
        ),
        tags$table(
          class = "table table-condensed table-striped",
          style = "font-size: 13px; margin-bottom: 0;",
          tags$tbody(
            tags$tr(tags$td(tags$strong("Facility ID")),      tags$td(fac$facility_id)),
            tags$tr(tags$td(tags$strong("Name")),             tags$td(fac$facility_name)),
            tags$tr(tags$td(tags$strong("Type")),             tags$td(.na_dash(fac$facility_type))),
            tags$tr(tags$td(tags$strong("Ownership")),        tags$td(.na_dash(fac$hf_ownership))),
            tags$tr(tags$td(tags$strong("Region")),           tags$td(.na_dash(fac$region))),
            tags$tr(tags$td(tags$strong("District")),         tags$td(.na_dash(fac$district))),
            tags$tr(tags$td(tags$strong("Incharge")),         tags$td(.na_dash(fac$incharge_name))),
            tags$tr(tags$td(tags$strong("Latitude")),         tags$td(round(fac$lat, 5))),
            tags$tr(tags$td(tags$strong("Longitude")),        tags$td(round(fac$lon, 5))),
            tags$tr(tags$td(tags$strong("SIA Coord. Site")),  tags$td(fac$polio_sia_coordination_site)),
            if (is_app_added) {
              tags$tr(
                tags$td(tags$strong("Source")),
                tags$td(tags$span(style = "color:#e67e22;", icon("map-pin"), " App-added"))
              )
            }
          )
        )
      ))
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
