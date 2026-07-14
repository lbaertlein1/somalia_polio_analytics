# R/mod_access.R — Access & Challenges tab

access_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "g2",
        
        # Distance to HF
        div(class = "card mb",
            div(class = "card-header",
                span(class = "card-title", "Distance to Nearest Health Facility"),
                span(class = "bdg b-org", "Travel time (minutes)")
            ),
            div(class = "card-body",
                plotly::plotlyOutput(ns("hf_bar"), height = "220px"),
                div(style = "margin-top:12px",
                    uiOutput(ns("hf_insight"))
                )
            )
        ),
        
        # Challenges
        div(class = "card mb",
            div(class = "card-header",
                span(class = "card-title", "Service Challenges Reported"),
                span(class = "bdg b-red", "Multi-category mentions")
            ),
            div(class = "card-body",
                plotly::plotlyOutput(ns("challenge_bar"), height = "220px"),
                div(style = "margin-top:12px",
                    uiOutput(ns("access_insight"))
                )
            )
        )
    ),
    
    div(class = "g3",
        
        # Settlement type
        div(class = "card",
            div(class = "card-header", span(class = "card-title", "Settlement Type")),
            div(class = "card-body",
                plotly::plotlyOutput(ns("sett_donut"), height = "220px")
            )
        ),
        
        # Transport mode
        div(class = "card",
            div(class = "card-header", span(class = "card-title", "Transport Mode")),
            div(class = "card-body",
                plotly::plotlyOutput(ns("trans_donut"), height = "220px"),
                div(style = "margin-top:10px", uiOutput(ns("transport_insight")))
            )
        ),
        
        # Route type
        div(class = "card",
            div(class = "card-header", span(class = "card-title", "Migratory Route Type")),
            div(class = "card-body",
                plotly::plotlyOutput(ns("rtype_donut"), height = "220px")
            )
        )
    )
  )
}

access_server <- function(id, data, year_filter, insights = NULL) {
  moduleServer(id, function(input, output, session) {
    yr <- reactive(year_filter())
    
    # ── AI insights ────────────────────────────────────────────────────────────
    if (!is.null(insights)) {
      render_insight(output, "hf_insight",       insights, "access_summary",
                     class = "insight insight-warn")
      render_insight(output, "access_insight",   insights, "access_summary",
                     class = "insight")
      render_insight(output, "transport_insight", insights, "access_summary",
                     class = "insight")
    } else {
      output$hf_insight <- renderUI({
        div(class = "insight insight-warn",
            HTML("<strong>37% of camps \u2265120 minutes</strong> from nearest HF. Fixed-post immunization structurally inaccessible."))
      })
      output$access_insight <- renderUI({
        div(class = "insight",
            HTML("<strong>No nearby HF</strong> and <strong>distance</strong> dominate. Camp-based outreach is the only viable strategy."))
      })
      output$transport_insight <- renderUI({
        div(class = "insight", HTML("<strong>97% livestock/foot only.</strong>"))
      })
    }
    
    donut <- function(labels, values, colors) {
      plotly::plot_ly(
        labels = labels, values = values, type = "pie", hole = 0.62,
        marker = list(colors = colors, line = list(color = "#fff", width = 2)),
        textinfo = "label+percent", hoverinfo = "label+value+percent"
      ) |>
        plotly::layout(
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0, y = -0.12, font = list(size = 10)),
          margin = list(t = 5, b = 5, l = 5, r = 5)
        ) |>
        plotly::config(displayModeBar = FALSE)
    }
    
    output$hf_bar <- plotly::renderPlotly({
      d <- data$hf_access[[yr()]]
      if (is.null(d) || nrow(d) == 0) return(plotly::plotly_empty())
      order  <- c("<30 min", "30\u201360 min", "60\u2013120 min", "120+ min")
      colors <- c("#0d9488", "#0d9488", "#f97316", "#dc2626")
      d <- d[match(order, d$bucket), ]
      d <- d[!is.na(d$bucket), ]
      plotly::plot_ly(d,
                      x = ~bucket, y = ~n, type = "bar",
                      marker = list(color = colors[seq_len(nrow(d))]),
                      text = ~paste0(pct, "%"), textposition = "outside",
                      hovertemplate = "<b>%{x}</b><br>%{y} camps (%{text})<extra></extra>"
      ) |>
        plotly::layout(
          xaxis  = list(title = "", categoryorder = "array", categoryarray = order,
                        tickfont = list(size = 10)),
          yaxis  = list(title = "Camps"),
          margin = list(t = 30, b = 10, l = 40, r = 10)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    output$challenge_bar <- plotly::renderPlotly({
      d <- data$challenges[[yr()]]
      if (is.null(d) || nrow(d) == 0) return(plotly::plotly_empty())
      d <- d[order(d$n), ]
      plotly::plot_ly(d, y = ~challenge, x = ~n, type = "bar", orientation = "h",
                      marker = list(color = "#f97316")
      ) |>
        plotly::layout(
          yaxis  = list(title = "", automargin = TRUE, tickfont = list(size = 10)),
          xaxis  = list(title = "Frequency"),
          margin = list(l = 10, r = 10, t = 5, b = 30)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    output$sett_donut  <- plotly::renderPlotly({
      d <- data$settlement_counts
      if (is.null(d) || nrow(d) == 0)
        d <- data.frame(cat = c("Settlement","Camp","Other"), n = c(3716,215,7))
      cols <- c("Settlement"="#2563eb","Camp"="#d97706","Other"="#9ca3af")
      donut(d$cat, d$n, unname(cols[d$cat]))
    })
    output$trans_donut <- plotly::renderPlotly({
      d <- data$transport_counts
      if (is.null(d) || nrow(d) == 0)
        d <- data.frame(cat = c("Livestock/foot","Bus/vehicle","Other"), n = c(3811,51,76))
      cols <- c("Livestock/foot"="#0d9488","Bus/vehicle"="#3b82f6","Other"="#9ca3af")
      donut(d$cat, d$n, unname(cols[match(d$cat, names(cols))]))
    })
    output$rtype_donut <- plotly::renderPlotly({
      d <- data$route_type_counts
      if (is.null(d) || nrow(d) == 0)
        d <- data.frame(cat = c("Footpath","Highway","Border crossing","Other"), n = c(3460,392,34,52))
      cols <- c("Footpath"="#0d9488","Highway"="#f97316","Border crossing"="#dc2626","Other"="#9ca3af")
      donut(d$cat, d$n, unname(cols[match(d$cat, names(cols))]))
    })
  })
}