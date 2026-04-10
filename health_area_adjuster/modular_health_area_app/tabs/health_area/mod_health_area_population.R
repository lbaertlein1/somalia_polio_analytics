healthAreaPopulationUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = 'rightbar-title', 'Legend and population'),
    uiOutput(ns('legend_ui')),
    DTOutput(ns('pop_table'))
  )
}

healthAreaPopulationServer <- function(id, active_dfa, show_pop_raster, pop_table) {
  moduleServer(id, function(input, output, session) {
    output$legend_ui <- renderUI({
      selected_name <- active_dfa() %||% starter_dfa_names[1]
      show_selected <- !(selected_name %in% c('Inaccessible', 'Unpopulated'))

      raster_cols <- pop_palette(5)
      raster_labels <- c('Low', '', '', '', 'High')

      tagList(
        div(
          class = 'legend-wrap',
          if (show_selected) {
            div(
              class = 'legend-row',
              tags$span(class = 'legend-box', style = paste0('background:', selected_fill_color, ';')),
              tags$span(selected_name)
            )
          },
          div(
            class = 'legend-row',
            tags$span(class = 'legend-box', style = paste0('background:', nonselected_fill_color, ';')),
            tags$span('Other Health Areas')
          ),
          div(
            class = 'legend-row',
            tags$span(
              class = 'legend-box',
              style = paste0(
                'background:', special_fill_colors[['Inaccessible']],
                '; border-color:', special_fill_colors[['Inaccessible']], ';'
              )
            ),
            tags$span('Inaccessible')
          ),
          div(
            class = 'legend-row',
            tags$span(class = 'legend-box', style = 'background:#FFFFFF;'),
            tags$span('Unpopulated')
          ),
          if (isTRUE(show_pop_raster())) {
            tagList(
              tags$div(style = 'height:6px;'),
              tags$div(class = 'mini-label', style = 'margin-bottom:4px;', 'WorldPop U5 Population'),
              tags$div(
                style = 'display:flex; gap:0; margin-bottom:3px;',
                lapply(raster_cols, function(clr) {
                  tags$div(
                    style = paste0(
                      'flex:1; height:10px; background:', clr,
                      '; border-top:1px solid #999999; border-bottom:1px solid #999999;'
                    )
                  )
                })
              ),
              tags$div(
                style = 'display:flex; justify-content:space-between; font-size:10px; color:#666666;',
                lapply(raster_labels, function(lbl) tags$span(lbl))
              )
            )
          }
        )
      )
    })

    output$pop_table <- renderDT({
      df <- pop_table()
      if (is.null(df) || nrow(df) == 0) {
        return(
          datatable(
            data.frame(Area = character(0), `Estimated U5 population` = numeric(0)),
            options = list(dom = 't', paging = FALSE, ordering = FALSE, searching = FALSE),
            rownames = FALSE
          )
        )
      }

      datatable(
        df |>
          rename(
            Area = area_name,
            `WorldPop U5 Population` = est_u5_pop
          ),
        options = list(dom = 't', paging = FALSE, ordering = FALSE, autoWidth = TRUE, searching = FALSE),
        rownames = FALSE
      )
    })
  })
}
