# R/mod_zerodose.R
# Tab 2 — Zero Dose & Vaccination

zerodose_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    # ── Row 1: ZD bar chart + ZD rate table ──────────────────────────────────
    div(class = "g2",
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Zero-Dose Children Identified per District")
            ),
            div(class = "card-body", plotly::plotlyOutput(ns("zd_bar"), height = "280px"))
        ),
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Zero-Dose Rate by District"),
                span(class = "card-subtitle", "% unvaccinated per age group")
            ),
            div(class = "card-body-flush", tableOutput(ns("zd_rate_table")))
        )
    ),
    
    # ── Row 2: Coverage gap map ───────────────────────────────────────────────
    div(class = "card mb",
        div(class = "card-header",
            span(class = "card-title", "Coverage Gap Map"),
            span(class = "card-subtitle",
                 "Red = camps with ZD children  ·  Green = outreach sessions"),
            div(class = "map-controls",
                tags$select(id = ns("gap_yr"),
                            tags$option(value = "all",  "All time"),
                            tags$option(value = "2024", "2024"),
                            tags$option(value = "2025", "2025"),
                            tags$option(value = "2026", "2026")
                ),
                tags$select(id = ns("gap_mo"),
                            tags$option(value = "all", "All months")
                )
            ),
            uiOutput(ns("gap_count_label"))
        ),
        leafletOutput(ns("gap_map"), height = "420px")
    ),
    
    # ── Row 3: ZD identified vs vaccinated + ZD-RI vs ZD-Polio ───────────────
    div(class = "g2",
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Zero-Dose Identified vs Vaccinated by Outreach")
            ),
            div(class = "card-body", plotly::plotlyOutput(ns("gap_bar"), height = "260px"))
        ),
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Zero-Dose RI (excl. Polio) vs Zero-Dose Polio"),
                span(class = "card-subtitle", "Reported during outreach visits")
            ),
            div(class = "card-body", plotly::plotlyOutput(ns("zd_ri_pol"), height = "260px"))
        )
    ),
    
    # ── Row 4: Outreach sessions + gap table ─────────────────────────────────
    div(class = "g2",
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Outreach Sessions by District")
            ),
            div(class = "card-body", plotly::plotlyOutput(ns("out_dist_bar"), height = "260px"))
        ),
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "District Coverage Gap"),
                span(class = "card-subtitle",
                     "ZD identified vs reached by outreach")
            ),
            div(class = "card-body-flush", tableOutput(ns("perf_table")))
        )
    )
  )
}

zerodose_server <- function(id, data, year_filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    yr <- reactive(year_filter())
    
    # ── ZD grouped bar ────────────────────────────────────────────────────────
    output$zd_bar <- plotly::renderPlotly({
      d <- data$zd_by_dist[[yr()]]
      if (is.null(d) || nrow(d) == 0) return(plotly::plot_ly() |> plotly::layout(title = "No data"))
      d <- d[order(-d$z011), ]
      plotly::plot_ly(d, x = ~district, y = ~z011, type = "bar",
                      name = "0–11 months",
                      marker = list(color = COL_RED, opacity = 0.8)) |>
        plotly::add_trace(y = ~z1259, name = "12–59 months",
                          marker = list(color = COL_ORANGE, opacity = 0.8)) |>
        plotly::layout(
          barmode = "group",
          xaxis   = list(title = "", tickangle = -30),
          yaxis   = list(title = "Zero-dose children"),
          legend  = list(orientation = "h", y = -0.25),
          margin  = list(b = 80)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── ZD rate table ─────────────────────────────────────────────────────────
    output$zd_rate_table <- renderTable({
      d <- data$zd_by_dist[[yr()]] |>
        dplyr::select(District = district,
                      `0–11 mo (%)` = rate011,
                      `12–59 mo (%)` = rate1259) |>
        dplyr::arrange(dplyr::desc(`0–11 mo (%)`))
    }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%", digits = 1)
    
    # ── Gap map base ─────────────────────────────────────────────────────────
    output$gap_map <- renderLeaflet({
      leaflet(options = leafletOptions(scrollWheelZoom = FALSE)) |>
        addProviderTiles("CartoDB.Positron") |>
        setView(lng = 42.5, lat = 4.5, zoom = 7)
    })
    
    # Populate month dropdown
    observe({
      months <- sort(unique(data$camp_gps$year))
      all_months <- data$out_monthly |>
        dplyr::filter(!is.na(month_label)) |>
        dplyr::pull(month_label) |> unique() |> sort()
      updateSelectInput(session, "gap_mo",
                        choices = c("All months" = "all",
                                    setNames(all_months, all_months)))
    })
    
    # Reactive gap map update
    observe({
      req(input$gap_yr)
      proxy <- leafletProxy(ns("gap_map"))
      proxy |> clearGroup("zd_pts") |> clearGroup("out_pts")
      
      gap_yr <- input$gap_yr
      gap_mo <- input$gap_mo %||% "all"
      
      # ZD camp points
      cp <- data$camp_gps
      if (gap_yr != "all") cp <- cp[cp$year == as.integer(gap_yr), ]
      cp <- cp[!is.na(cp$lat) & !is.na(cp$lon), ]
      
      if (nrow(cp) > 0) {
        max_zd <- max(cp$zd_0to11 + cp$zd_12to59, 1, na.rm = TRUE)
        proxy |> addCircleMarkers(
          data      = cp,
          lng = ~lon, lat = ~lat,
          radius    = ~pmax(3, pmin(12, 3 + 9 * (zd_0to11 + zd_12to59) / max_zd)),
          fillColor = COL_RED, fillOpacity = 0.65,
          color     = "#991b1b", weight = 0.8,
          label     = ~paste0(district, " | ZD: ", zd_0to11 + zd_12to59),
          group     = "zd_pts"
        )
      }
      
      # Outreach points
      op <- data$out_gps
      if (gap_yr != "all") op <- op[op$year == as.integer(gap_yr), ]
      if (gap_mo != "all") op <- op[op$month_label == gap_mo, ]
      op <- op[!is.na(op$lat) & !is.na(op$lon), ]
      
      if (nrow(op) > 0) {
        proxy |> addCircleMarkers(
          data      = op,
          lng = ~lon, lat = ~lat,
          radius    = 6, fillColor = COL_GREEN, fillOpacity = 0.85,
          color     = "white", weight = 1.5,
          label     = ~paste0("Outreach · ", district,
                              " | Vaccinated: ", vaccinated),
          group     = "out_pts"
        )
      }
    })
    
    output$gap_count_label <- renderUI({
      gap_yr <- input$gap_yr %||% "all"
      cp <- data$camp_gps
      op <- data$out_gps
      if (gap_yr != "all") {
        cp <- cp[cp$year == as.integer(gap_yr), ]
        op <- op[op$year == as.integer(gap_yr), ]
      }
      span(class = "muted",
           paste0(nrow(cp), " ZD camps · ", nrow(op), " outreach sessions"))
    })
    
    # ── Gap bar ───────────────────────────────────────────────────────────────
    output$gap_bar <- plotly::renderPlotly({
      d <- data$perf_table[[yr()]]
      if (is.null(d) || nrow(d) == 0) return(plotly::plot_ly())
      d <- d[order(-d$zd_id), ]
      plotly::plot_ly(d, x = ~district, y = ~zd_id, type = "bar",
                      name = "ZD identified",
                      marker = list(color = COL_RED, opacity = 0.8)) |>
        plotly::add_trace(y = ~vaccinated, name = "Vaccinated (outreach)",
                          marker = list(color = COL_GREEN, opacity = 0.8)) |>
        plotly::layout(
          barmode = "group",
          xaxis   = list(title = "", tickangle = -30),
          yaxis   = list(title = "Children"),
          legend  = list(orientation = "h", y = -0.3),
          margin  = list(b = 80)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── ZD-RI vs ZD-Polio ─────────────────────────────────────────────────────
    output$zd_ri_pol <- plotly::renderPlotly({
      d <- data$zd_ri_pol[[yr()]]
      if (is.null(d) || nrow(d) == 0) return(plotly::plot_ly())
      plotly::plot_ly(d, x = ~district, y = ~ri, type = "bar",
                      name = "ZD RI (excl. polio)",
                      marker = list(color = COL_ORANGE, opacity = 0.8)) |>
        plotly::add_trace(y = ~pol, name = "ZD polio",
                          marker = list(color = COL_RED, opacity = 0.75)) |>
        plotly::layout(
          barmode = "group",
          xaxis   = list(title = "", tickangle = -30),
          yaxis   = list(title = "Children"),
          legend  = list(orientation = "h", y = -0.3),
          margin  = list(b = 80)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── Outreach by district bar ─────────────────────────────────────────────
    output$out_dist_bar <- plotly::renderPlotly({
      d <- data$out_by_dist[[yr()]]
      if (is.null(d) || nrow(d) == 0) return(plotly::plot_ly())
      d <- d[order(-d$vaccinated), ]
      plotly::plot_ly(d, x = ~district, y = ~vaccinated, type = "bar",
                      marker = list(color = COL_TEAL, opacity = 0.85),
                      text = ~vaccinated, textposition = "outside") |>
        plotly::layout(
          xaxis  = list(title = "", tickangle = -30),
          yaxis  = list(title = "Children vaccinated"),
          margin = list(b = 80)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── Performance / gap table ───────────────────────────────────────────────
    output$perf_table <- renderTable({
      d <- data$perf_table[[yr()]]
      if (is.null(d)) return(NULL)
      d |>
        dplyr::arrange(dplyr::desc(gap)) |>
        dplyr::select(District = district,
                      `ZD identified` = zd_id,
                      Vaccinated = vaccinated,
                      `Unmet gap` = gap,
                      `Coverage (%)` = cov_pct)
    }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%", digits = 1)
  })
}