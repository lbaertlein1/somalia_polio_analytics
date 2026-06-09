# R/mod_access.R
# Tab 3 — Access & Challenges

access_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "g2",
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Travel Time to Nearest Health Facility")
            ),
            div(class = "card-body", plotly::plotlyOutput(ns("hf_pie"), height = "280px"))
        ),
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Structural Access Insight")
            ),
            div(class = "card-body", uiOutput(ns("access_insight")))
        )
    ),
    div(class = "card mb",
        div(class = "card-header",
            span(class = "card-title", "Top Reported Challenges")
        ),
        div(class = "card-body", plotly::plotlyOutput(ns("challenge_bar"), height = "300px"))
    )
  )
}

access_server <- function(id, data, year_filter) {
  moduleServer(id, function(input, output, session) {
    yr <- reactive(year_filter())
    
    output$hf_pie <- plotly::renderPlotly({
      d <- data$hf_access[[yr()]]
      if (is.null(d) || nrow(d) == 0) return(plotly::plot_ly())
      cols <- c("<30 min" = COL_GREEN, "30–60 min" = COL_TEAL,
                "60–120 min" = COL_ORANGE, "120+ min" = COL_RED)
      d$color <- cols[d$bucket]
      plotly::plot_ly(d, labels = ~bucket, values = ~n, type = "pie",
                      marker = list(colors = ~color),
                      textinfo = "label+percent",
                      hoverinfo = "label+value+percent") |>
        plotly::config(displayModeBar = FALSE)
    })
    
    output$access_insight <- renderUI({
      d <- data$hf_access[[yr()]]
      if (is.null(d) || nrow(d) == 0) return(NULL)
      far <- d$pct[d$bucket == "120+ min"]
      far <- if (length(far) > 0) far[1] else 0
      div(
        div(class = "insight insight-warn",
            HTML(paste0(
              "<strong>", far, "% of camps</strong> are ≥120 min from the ",
              "nearest health facility. Fixed-post immunization cannot reach ",
              "these populations — outreach is structurally required."
            ))
        ),
        tags$br(),
        div(class = "insight",
            HTML(paste0(
              "97% of nomadic populations travel by livestock trails. ",
              "88% use unmarked footpaths. Standard catchment-based ",
              "microplanning does not apply — GPS-guided outreach scheduling ",
              "against the seasonal movement calendar is the only viable approach."
            ))
        )
      )
    })
    
    output$challenge_bar <- plotly::renderPlotly({
      d <- data$challenges[[yr()]]
      if (is.null(d) || nrow(d) == 0) return(plotly::plot_ly())
      d <- d[order(d$n), ]
      plotly::plot_ly(d, y = ~challenge, x = ~n, type = "bar",
                      orientation = "h",
                      marker = list(color = COL_TEAL, opacity = 0.8)) |>
        plotly::layout(
          yaxis  = list(title = ""),
          xaxis  = list(title = "Frequency"),
          margin = list(l = 160)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}