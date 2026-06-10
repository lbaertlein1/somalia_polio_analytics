library(leaflet)
# R/mod_zerodose.R
# Tab 2 — Zero Dose & Vaccination
# Matches reference: horizontal ZD bar, badge rate table, coverage gap map,
# gap table, ZD-RI vs polio chart, outreach by district, monthly trend charts

zerodose_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    # Row 1: ZD horizontal bar + ZD rate table with badges
    div(class = "g2",
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Zero-Dose Children Identified per District")
            ),
            div(class = "card-body",
                plotly::plotlyOutput(ns("zd_bar"), height = "300px"),
                div(style = "margin-top:12px", uiOutput(ns("zd_insight")))
            )
        ),
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Zero-Dose Rate by District"),
                span(class = "card-subtitle", "% unvaccinated per age group")
            ),
            div(class = "card-body-flush",
                uiOutput(ns("zd_rate_table_ui"))
            )
        )
    ),
    
    # Row 2: Coverage gap map
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
    
    # Row 3: Gap table + ZD-RI vs Polio
    div(class = "g2",
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "District Coverage Gap"),
                span(class = "card-subtitle", "ZD identified vs reached by outreach")
            ),
            div(class = "card-body-flush",
                uiOutput(ns("gap_table_ui")),
                div(style = "padding:12px", uiOutput(ns("coverage_insight")))
            )
        ),
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Zero-Dose RI (excl. Polio) vs Zero-Dose Polio"),
                span(class = "card-subtitle", "Reported during outreach visits")
            ),
            div(class = "card-body", plotly::plotlyOutput(ns("zd_ri_pol"), height = "260px"))
        )
    ),
    
    # Row 4: Monthly outreach trend (dual axis)
    div(class = "card mb",
        div(class = "card-header",
            span(class = "card-title", "Outreach Sessions & Children Vaccinated Over Time")
        ),
        div(class = "card-body", plotly::plotlyOutput(ns("out_monthly"), height = "260px"))
    ),
    
    # Row 5: District monthly vaccinated trend
    div(class = "card mb",
        div(class = "card-header",
            span(class = "card-title", "Children Vaccinated by District Over Time")
        ),
        div(class = "card-body", plotly::plotlyOutput(ns("dist_monthly"), height = "260px"))
    )
  )
}

zerodose_server <- function(id, data, year_filter, insights = NULL) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    yr  <- reactive(year_filter())
    
    # ── AI insights ────────────────────────────────────────────────────────────
    if (!is.null(insights)) {
      render_insight(output, "zd_insight",       insights, "zd_summary",
                     class = "insight insight-warn")
      render_insight(output, "coverage_insight", insights, "coverage_gap",
                     class = "insight")
    }
    
    # ── ZD horizontal grouped bar ─────────────────────────────────────────────
    output$zd_bar <- plotly::renderPlotly({
      d <- data$zd_by_dist[[yr()]]
      if (is.null(d) || nrow(d) == 0) return(plotly::plot_ly())
      d <- d[order(d$z011 + d$z1259), ]
      plotly::plot_ly(d,
                      y    = ~district, x = ~z011, type = "bar", orientation = "h",
                      name = "Zero-dose 0–11mo",
                      marker = list(color = "#dc2626", opacity = 0.8)
      ) |>
        plotly::add_trace(x = ~z1259, name = "Zero-dose 12–59mo",
                          marker = list(color = "#f97316", opacity = 0.8)) |>
        plotly::layout(
          barmode = "group",
          yaxis   = list(title = "", automargin = TRUE),
          xaxis   = list(title = "Zero-dose children"),
          legend  = list(orientation = "h", y = -0.15),
          margin  = list(l = 10)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── ZD rate table with badges ─────────────────────────────────────────────
    output$zd_rate_table_ui <- renderUI({
      d <- data$zd_by_dist[[yr()]]
      if (is.null(d) || nrow(d) == 0) return(p("No data"))
      d <- d[order(-d$rate011), ]
      
      badge_class <- function(r, district) {
        if (is.na(district) || district == "Luuq") return("bdg b-gry")
        if (!is.na(r) && r >= 80) "bdg b-red"
        else if (!is.na(r) && r >= 60) "bdg b-org"
        else if (!is.na(r) && r >= 30) "bdg b-blu"
        else "bdg b-grn"
      }
      badge_label <- function(r, district) {
        if (is.na(district) || district == "Luuq") return("Check data")
        if (!is.na(r) && r >= 80) "Critical"
        else if (!is.na(r) && r >= 60) "High"
        else if (!is.na(r) && r >= 30) "Moderate"
        else "Low"
      }
      
      rows <- lapply(seq_len(nrow(d)), function(i) {
        row   <- d[i, ]
        dist  <- as.character(row$district)
        warn  <- if (!is.na(dist) && dist == "Luuq") " \u26a0" else ""
        r0    <- as.numeric(row$rate011)
        r1    <- as.numeric(row$rate1259)
        col   <- if (is.na(dist) || dist == "Luuq") "var(--muted)"
        else if (!is.na(r0) && r0 >= 80) "var(--red)"
        else if (!is.na(r0) && r0 >= 60) "var(--orange)"
        else "var(--text)"
        tags$tr(
          tags$td(style = "font-weight:500", paste0(dist, warn)),
          tags$td(class = "tr mono", row$camps),
          tags$td(class = "tr",
                  tags$span(style = paste0("font-weight:600;color:", col),
                            paste0(r0, "%"))),
          tags$td(class = "tr", paste0(r1, "%")),
          tags$td(class = "tc",
                  tags$span(class = badge_class(r0, dist),
                            badge_label(r0, dist)))
        )
      })
      
      tags$table(class = "zd-tbl",
                 tags$thead(tags$tr(
                   tags$th("District"), tags$th(class="tr","Camps"),
                   tags$th(class="tr","Rate 0–11%"), tags$th(class="tr","Rate 12–59%"),
                   tags$th(class="tc","Status")
                 )),
                 do.call(tags$tbody, rows)
      )
    })
    
    # ── Gap map ───────────────────────────────────────────────────────────────
    output$gap_map <- renderLeaflet({
      leaflet(options = leafletOptions(scrollWheelZoom = FALSE)) |>
        addProviderTiles("CartoDB.Positron") |>
        setView(lng = 42.5, lat = 4.5, zoom = 7)
    })
    
    observe({
      all_months <- as.character(levels(data$out_monthly$month_label))
      updateSelectInput(session, "gap_mo",
                        choices = c("All months" = "all", setNames(all_months, all_months)))
    })
    
    observe({
      req(input$gap_yr)
      proxy  <- leafletProxy(ns("gap_map"))
      proxy |> clearGroup("zd_pts") |> clearGroup("out_pts")
      gap_yr <- input$gap_yr
      gap_mo <- input$gap_mo %||% "all"
      
      cp <- data$camp_gps
      if (gap_yr != "all") cp <- cp[cp$year == as.integer(gap_yr), ]
      cp <- cp[!is.na(cp$lat) & !is.na(cp$lon), ]
      if (nrow(cp) > 0) {
        proxy |> addCircleMarkers(
          data = cp, lng = ~lon, lat = ~lat,
          radius    = ~pmax(3, pmin(10, 3 + log(zd_0to11 + zd_12to59 + 1) * 1.5)),
          fillColor = "#dc2626", fillOpacity = 0.65,
          color = "#991b1b", weight = 0.8, group = "zd_pts",
          label = ~paste0(district, " | ZD: ", zd_0to11 + zd_12to59)
        )
      }
      
      op <- data$out_gps
      if (gap_yr != "all") op <- op[op$year == as.integer(gap_yr), ]
      if (gap_mo != "all") op <- op[!is.na(op$month_label) & as.character(op$month_label) == gap_mo, ]
      op <- op[!is.na(op$lat) & !is.na(op$lon), ]
      if (nrow(op) > 0) {
        proxy |> addCircleMarkers(
          data = op, lng = ~lon, lat = ~lat,
          radius = 6, fillColor = "#22c55e", fillOpacity = 0.85,
          color = "white", weight = 1.5, group = "out_pts",
          label = ~paste0("Outreach · ", district, " | Vaccinated: ", vaccinated)
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
    
    # ── Gap table (HTML, matches reference style) ─────────────────────────────
    output$gap_table_ui <- renderUI({
      d <- data$perf_table[[yr()]]
      if (is.null(d)) return(p("No data"))
      d <- d[order(-d$gap), ]
      
      rows <- lapply(seq_len(nrow(d)), function(i) {
        row      <- d[i, ]
        dist     <- as.character(row$district)
        cov      <- if (is.na(row$cov_pct) || is.null(row$cov_pct)) 0 else as.numeric(row$cov_pct)
        cov_col  <- if (!is.na(cov) && cov >= 80) "var(--green)" else if (!is.na(cov) && cov >= 50) "var(--orange)" else "var(--red)"
        cov_str  <- if (!is.na(dist) && length(dist) == 1 && dist == "Luuq")
          paste0(">", cov, "% \u26a0")
        else paste0(ifelse(is.na(cov), "—", cov), "%")
        gap_val  <- if (is.na(row$gap) || is.null(row$gap)) 0L else as.integer(row$gap)
        gap_html <- if (!is.na(gap_val) && gap_val > 0)
          tags$span(style = "color:var(--red);font-weight:600",
                    format(gap_val, big.mark = ","))
        else tags$span(style = "color:var(--green)", "0")
        tags$tr(
          tags$td(style = "font-weight:500", dist),
          tags$td(class = "tr mono", format(row$zd_id,     big.mark = ",")),
          tags$td(class = "tr mono", format(row$vaccinated, big.mark = ",")),
          tags$td(class = "tr", gap_html),
          tags$td(class = "tr",
                  tags$span(style = paste0("color:", cov_col, ";font-weight:600"),
                            cov_str))
        )
      })
      
      tags$table(class = "zd-tbl",
                 tags$thead(tags$tr(
                   tags$th("District"),
                   tags$th(class = "tr", "ZD identified"),
                   tags$th(class = "tr", "Vaccinated"),
                   tags$th(class = "tr", "Gap"),
                   tags$th(class = "tr", "Coverage %")
                 )),
                 do.call(tags$tbody, rows)
      )
    })
    
    # ── ZD-RI vs ZD-Polio horizontal bar ──────────────────────────────────────
    output$zd_ri_pol <- plotly::renderPlotly({
      d <- data$zd_ri_pol[[yr()]]
      if (is.null(d) || nrow(d) == 0) return(plotly::plot_ly())
      d <- d[order(d$ri + d$pol), ]
      plotly::plot_ly(d,
                      y = ~district, x = ~ri, type = "bar", orientation = "h",
                      name = "Zero-dose RI (excl. polio)",
                      marker = list(color = "rgba(217,119,6,0.75)")
      ) |>
        plotly::add_trace(x = ~pol, name = "Zero-dose polio",
                          marker = list(color = "rgba(220,38,38,0.75)")) |>
        plotly::layout(
          barmode = "group",
          yaxis   = list(title = "", automargin = TRUE),
          xaxis   = list(title = "Children"),
          legend  = list(orientation = "h", y = -0.2),
          margin  = list(l = 10)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── Monthly outreach trend (dual axis — sessions + vaccinated) ────────────
    output$out_monthly <- plotly::renderPlotly({
      d <- data$out_monthly
      if (is.null(d) || nrow(d) == 0) return(plotly::plot_ly())
      month_order <- as.character(levels(d$month_label))
      d$month_label <- as.character(d$month_label)
      plotly::plot_ly(d) |>
        plotly::add_trace(
          x = ~month_label, y = ~sessions,
          type = "scatter", mode = "lines+markers",
          fill = "tozeroy", fillcolor = "rgba(13,148,136,0.1)",
          line = list(color = "#0d9488", width = 2),
          marker = list(size = 5),
          name = "Sessions", yaxis = "y"
        ) |>
        plotly::add_trace(
          x = ~month_label, y = ~vaccinated,
          type = "scatter", mode = "lines+markers",
          line = list(color = "#dc2626", width = 2),
          marker = list(size = 5),
          name = "Vaccinated", yaxis = "y2"
        ) |>
        plotly::layout(
          xaxis  = list(title = "", tickangle = -35,
                        categoryorder = "array",
                        categoryarray = month_order),
          yaxis  = list(title = "Sessions"),
          yaxis2 = list(title = "Vaccinated", overlaying = "y",
                        side = "right", showgrid = FALSE),
          legend = list(orientation = "h", y = -0.25),
          margin = list(b = 80, r = 60)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── District monthly vaccinated line chart ────────────────────────────────
    output$dist_monthly <- plotly::renderPlotly({
      # Per-district monthly data not yet available in pipeline
      # Would require aggregating outreach by district x month in data_pull.R
      plotly::plot_ly() |>
        plotly::layout(
          annotations = list(list(
            text      = "Per-district monthly breakdown coming soon",
            showarrow = FALSE,
            xref      = "paper", yref = "paper",
            x = 0.5, y = 0.5,
            font = list(size = 13, color = "#718096")
          )),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}

# Null-coalescing helper
`%||%` <- function(a, b) if (!is.null(a)) a else b