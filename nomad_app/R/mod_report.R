# R/mod_report.R
# Tab 4 — Field Teams Report

report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "g2",
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Monthly Outreach Trend")
            ),
            div(class = "card-body",
                plotly::plotlyOutput(ns("out_monthly"), height = "280px")
            )
        ),
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Summary Report"),
                span(class = "card-subtitle", "Project coordinator download")
            ),
            div(class = "card-body",
                uiOutput(ns("report_summary")),
                tags$br(),
                downloadButton(ns("dl_report"), "Download Report (.txt)",
                               class = "btn-teal")
            )
        )
    ),
    div(class = "card mb",
        div(class = "card-header",
            span(class = "card-title", "District Performance Summary")
        ),
        div(class = "card-body-flush",
            tableOutput(ns("dist_perf_table"))
        )
    )
  )
}

report_server <- function(id, data, year_filter) {
  moduleServer(id, function(input, output, session) {
    yr <- reactive(year_filter())
    
    output$out_monthly <- plotly::renderPlotly({
      d <- data$out_monthly
      if (is.null(d) || nrow(d) == 0) return(plotly::plot_ly())
      plotly::plot_ly(d, x = ~month_label, y = ~vaccinated, type = "bar",
                      name = "Children vaccinated",
                      marker = list(color = COL_GREEN, opacity = 0.8)) |>
        plotly::add_trace(y = ~sessions, name = "Sessions",
                          type = "scatter", mode = "lines+markers",
                          yaxis = "y2",
                          line  = list(color = COL_TEAL, width = 2),
                          marker = list(color = COL_TEAL, size = 5)) |>
        plotly::layout(
          xaxis  = list(title = "", tickangle = -45),
          yaxis  = list(title = "Children vaccinated"),
          yaxis2 = list(title = "Sessions", overlaying = "y", side = "right"),
          legend = list(orientation = "h", y = -0.3),
          margin = list(b = 100)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    output$report_summary <- renderUI({
      kpi <- data$kpi[[yr()]]
      yr_label <- if (yr() == "all") "All time (Jun 2024 – present)" else yr()
      div(
        div(class = "insight",
            HTML(paste0(
              "<strong>Period:</strong> ", yr_label, "<br>",
              "<strong>Camps enumerated:</strong> ",
              formatC(kpi$camps, format = "d", big.mark = ","), "<br>",
              "<strong>Total population:</strong> ",
              formatC(kpi$pop, format = "d", big.mark = ","), "<br>",
              "<strong>Zero-dose children identified:</strong> ",
              formatC(kpi$zd_total, format = "d", big.mark = ","), "<br>",
              "<strong>Children vaccinated (outreach):</strong> ",
              formatC(kpi$vaccinated, format = "d", big.mark = ","), "<br>",
              "<strong>Outreach sessions:</strong> ", kpi$sessions
            ))
        )
      )
    })
    
    output$dl_report <- downloadHandler(
      filename = function() {
        paste0("nomad_report_", yr(), "_", Sys.Date(), ".txt")
      },
      content = function(file) {
        kpi <- data$kpi[[yr()]]
        yr_label <- if (yr() == "all") "All time" else yr()
        pt <- data$perf_table[[yr()]]
        top_gap <- if (!is.null(pt) && nrow(pt) > 0) {
          pt_s <- pt[order(-pt$gap), ]
          paste(paste0("  ", pt_s$district, ": ", pt_s$gap, " unvaccinated"),
                collapse = "\n")
        } else "  No data"
        
        txt <- paste0(
          "NOMADIC MAPPING — POLIO ANALYTICS\n",
          "Report generated: ", Sys.time(), "\n",
          "Period: ", yr_label, "\n",
          paste(rep("─", 50), collapse = ""), "\n\n",
          "KEY INDICATORS\n",
          "  Camps enumerated:            ",
          formatC(kpi$camps, format = "d", big.mark = ","), "\n",
          "  Total population:            ",
          formatC(kpi$pop, format = "d", big.mark = ","), "\n",
          "  Zero-dose children (0-11mo): ",
          formatC(kpi$zd_011, format = "d", big.mark = ","), "\n",
          "  Zero-dose children (12-59mo):",
          formatC(kpi$zd_1259, format = "d", big.mark = ","), "\n",
          "  Outreach sessions:           ", kpi$sessions, "\n",
          "  Children vaccinated:         ",
          formatC(kpi$vaccinated, format = "d", big.mark = ","), "\n\n",
          "UNMET GAP BY DISTRICT\n",
          top_gap, "\n\n",
          "DATA QUALITY NOTE\n",
          "  Luuq district zero-dose counts flagged as potentially under-reported.\n",
          "  Enumerator district field may contain free-text variants — review crosswalk.\n"
        )
        writeLines(txt, file)
      }
    )
    
    output$dist_perf_table <- renderTable({
      d <- data$perf_table[[yr()]]
      if (is.null(d)) return(NULL)
      d |>
        dplyr::arrange(dplyr::desc(gap)) |>
        dplyr::select(
          District     = district,
          Sessions     = sessions,
          Vaccinated   = vaccinated,
          `ZD identified` = zd_id,
          `Unmet gap`  = gap,
          `Coverage (%)` = cov_pct
        )
    }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%", digits = 1)
  })
}