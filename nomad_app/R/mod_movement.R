library(leaflet)
# R/mod_movement.R — Movement Patterns tab

movement_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "mov-grid",
        
        # ── Left: map ──────────────────────────────────────────────────────────
        div(class = "map-col",
            div(class = "card",
                div(class = "card-header",
                    span(class = "card-title", "District Movement Map"),
                    div(class = "map-controls",
                        checkboxInput(ns("show_camps"), "Nomadic camps visited", value = TRUE),
                        checkboxInput(ns("show_flows"), "Cross-district flows",  value = TRUE),
                        tags$select(id = ns("basemap"),
                                    tags$option(value = "carto-light",   "Light"),
                                    tags$option(value = "carto-dark",    "Dark"),
                                    tags$option(value = "osm",           "OpenStreetMap"),
                                    tags$option(value = "satellite",     "Satellite"),
                                    tags$option(value = "carto-voyager", "Voyager")
                        )
                    )
                ),
                leafletOutput(ns("mov_map"), height = "600px"),
                div(class = "map-legend",
                    div(class = "leg-item",
                        div(style = "background:#0d9488;width:14px;height:14px;border-radius:50%;opacity:.75;display:inline-block;vertical-align:middle;margin-right:5px"),
                        "Destination (size = incoming camps)"
                    ),
                    div(class = "leg-item",
                        div(style = "width:24px;height:3px;background:#d97706;display:inline-block;vertical-align:middle;margin-right:5px"),
                        "Cross-district flow"
                    ),
                    div(class = "leg-item",
                        div(style = "background:#0d9488;width:7px;height:7px;border-radius:50%;opacity:.6;display:inline-block;vertical-align:middle;margin-right:5px"),
                        "Nomadic camps visited"
                    )
                )
            )
        ),
        
        # ── Right: panels ──────────────────────────────────────────────────────
        div(class = "right-col",
            
            # Key finding (AI)
            div(class = "card",
                div(class = "card-header",
                    span(class = "card-title", "Key Finding"),
                    span(class = "card-subtitle", "Movement patterns analysis")
                ),
                div(class = "card-body", uiOutput(ns("key_finding")))
            ),
            
            # Indegree centrality table (HTML, matches reference)
            div(class = "card",
                div(class = "card-header",
                    span(class = "card-title", "Indegree Centrality Ranking"),
                    span(class = "card-subtitle", "Incoming camp volume")
                ),
                div(class = "card-body-flush",
                    uiOutput(ns("indegree_table_ui"))
                )
            ),
            
            # Exit seasons
            div(class = "card",
                div(class = "card-header",
                    span(class = "card-title", "Exit Season"),
                    uiOutput(ns("season_badge"))
                ),
                div(class = "card-body",
                    uiOutput(ns("season_boxes"))
                )
            )
        )
    ),
    
    # ── Bottom: routes + cross-border ─────────────────────────────────────────
    div(class = "g2",
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Top Named Migratory Routes")
            ),
            div(class = "card-body", uiOutput(ns("routes_list")))
        ),
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Cross-Border Context"),
                span(class = "card-subtitle", "Polio transmission risk")
            ),
            div(class = "card-body", uiOutput(ns("cross_border")))
        )
    )
  )
}

movement_server <- function(id, data, year_filter, insights = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    yr <- reactive(year_filter())
    
    # ── Checkbox reactives ────────────────────────────────────────────────────
    show_camps <- reactive({ isTRUE(input$show_camps) })
    show_flows <- reactive({ isTRUE(input$show_flows) })
    
    # ── Base map ──────────────────────────────────────────────────────────────
    output$mov_map <- renderLeaflet({
      lmap <- leaflet(options = leafletOptions(scrollWheelZoom = FALSE)) |>
        addProviderTiles("CartoDB.Positron", layerId = "base") |>
        setView(lng = 42.5, lat = 4.5, zoom = 7)
      
      # District polygons (white fill, grey border)
      if (!is.null(data$districts_geojson)) {
        lmap <- lmap |>
          addPolygons(
            data        = data$districts_geojson,
            fillColor   = "#ffffff", fillOpacity = 0.9,
            color       = "#94a3b8", weight = 0.6, opacity = 0.7,
          )
      }
      
      # State outlines (no fill, thick black)
      if (!is.null(data$states_geojson)) {
        lmap <- lmap |>
          addPolygons(
            data        = data$states_geojson,
            fillColor   = "transparent", fillOpacity = 0,
            color       = "#1a202c", weight = 2.5, opacity = 0.85,
            label       = ~state_name,
            labelOptions = labelOptions(
              noHide    = TRUE, direction = "center", textOnly = TRUE,
              style     = list(
                "font-size"    = "9px",
                "font-weight"  = "700",
                "color"        = "#1a202c",
                "text-shadow"  = "1px 1px 0 rgba(255,255,255,.95),-1px -1px 0 rgba(255,255,255,.95),1px -1px 0 rgba(255,255,255,.95),-1px 1px 0 rgba(255,255,255,.95)",
                "pointer-events" = "none",
                "letter-spacing" = ".07em",
                "text-transform" = "uppercase"
              )
            )
          )
      }
      lmap
    })
    
    # ── Reactive layers ───────────────────────────────────────────────────────
    observe({
      req(yr())
      # Also depend on checkbox state so toggling redraws layers
      sc <- show_camps()
      sf <- show_flows()
      proxy <- leafletProxy(ns("mov_map"))
      proxy |> clearGroup("camps") |> clearGroup("flows") |>
        clearGroup("indegree_circles")
      
      ind <- data$indegree[[yr()]]
      fl  <- data$flows[[yr()]]
      cen <- data$centroids
      
      # Indegree circles + district labels
      if (!is.null(ind) && nrow(ind) > 0 && !is.null(cen) && nrow(cen) > 0) {
        ind_join <- dplyr::left_join(ind, cen, by = "district") |>
          dplyr::filter(!is.na(lat), !is.na(lon), !is.na(district))
        if (nrow(ind_join) > 0) {
          max_c <- max(ind_join$count, 1)
          proxy |> addCircleMarkers(
            data        = ind_join,
            lng = ~lon, lat = ~lat,
            radius      = ~pmax(7, pmin(35, 7 + sqrt(count / max_c) * 28)),
            fillColor   = "#0d9488", fillOpacity = 0.18,
            color       = "#0d9488", weight = 2, opacity = 0.85,
            label       = ~paste0(district, " — Indegree: ", count),
            group       = "indegree_circles"
          )
          # District name labels
          for (i in seq_len(nrow(ind_join))) {
            r <- ind_join[i, ]
            proxy |> addMarkers(
              lng  = r$lon, lat  = r$lat,
              icon = leaflet::makeIcon(iconUrl = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7",
                                       iconWidth = 1, iconHeight = 1),
              label = r$district,
              labelOptions = labelOptions(
                noHide   = TRUE, direction = "top", textOnly = FALSE,
                style    = list(
                  "background"    = "white",
                  "border"        = "1.5px solid #0d9488",
                  "border-radius" = "4px",
                  "padding"       = "2px 6px",
                  "font-size"     = "10px",
                  "font-weight"   = "600",
                  "color"         = "#0f5050",
                  "box-shadow"    = "0 1px 3px rgba(0,0,0,.15)",
                  "white-space"   = "nowrap"
                )
              ),
              group = "indegree_circles"
            )
          }
        }
      }
      
      # Camp GPS dots
      if (sc) {
        gps <- data$camp_gps
        if (!is.null(gps) && nrow(gps) > 0) {
          gps_yr <- if (yr() == "all") gps else gps[gps$year == as.integer(yr()), ]
          gps_yr <- gps_yr[!is.na(gps_yr$lat) & !is.na(gps_yr$lon), ]
          if (nrow(gps_yr) > 0) {
            proxy |> addCircleMarkers(
              data = gps_yr, lng = ~lon, lat = ~lat,
              radius = 2.5, fillColor = "#0d9488", fillOpacity = 0.45,
              stroke = FALSE, group = "camps"
            )
          }
        }
      }
      
      # Flow arrows (cross-district only, count >= 3)
      if (sf && !is.null(fl) && nrow(fl) > 0 &&
          !is.null(cen) && nrow(cen) > 0) {
        cross <- fl[!is.na(fl$prev_c) & !is.na(fl$next_c) &
                      fl$prev_c != fl$next_c & fl$count >= 3, ]
        if (nrow(cross) > 0) {
          max_cnt <- max(cross$count, 1)
          for (i in seq_len(nrow(cross))) {
            from <- cen[cen$district == cross$prev_c[i], ]
            to   <- cen[cen$district == cross$next_c[i], ]
            if (nrow(from) == 0 || nrow(to) == 0) next
            wt   <- 1.5 + log(cross$count[i] + 1) * 0.9
            mid  <- c((from$lat + to$lat) / 2 + (to$lon - from$lon) * 0.05,
                      (from$lon + to$lon) / 2 - (to$lat - from$lat) * 0.05)
            proxy |>
              addPolylines(
                lng = c(from$lon, mid[2], to$lon),
                lat = c(from$lat, mid[1], to$lat),
                color = "#000000", weight = wt + 2, opacity = 0.28,
                group = "flows"
              ) |>
              addPolylines(
                lng = c(from$lon, mid[2], to$lon),
                lat = c(from$lat, mid[1], to$lat),
                color = "#d97706", weight = wt, opacity = 0.92,
                label = paste0(cross$prev_c[i], " \u2192 ", cross$next_c[i],
                               ": ", cross$count[i], " camps"),
                group = "flows"
              )
          }
        }
      }
    })
    
    # ── Basemap switcher ──────────────────────────────────────────────────────
    observeEvent(input$basemap, {
      tile_url <- switch(input$basemap,
                         "carto-light"   = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                         "carto-dark"    = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
                         "osm"           = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                         "satellite"     = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                         "carto-voyager" = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png"
      )
      if (!is.null(tile_url))
        leafletProxy(ns("mov_map")) |>
        removeTiles(layerId = "base") |>
        addTiles(urlTemplate = tile_url, layerId = "base")
    })
    
    # ── Year badge ────────────────────────────────────────────────────────────
    output$yr_badge <- renderUI({
      span(class = "bdg b-tel",
           if (yr() == "all") "All time" else yr())
    })
    
    # ── AI insights (rendered from shared insights rv) ──────────────────────────
    render_insight(output, "key_finding", insights, "movement_key",
                   fallback = "<strong>Movement patterns loading…</strong>",
                   class = "insight")
    
    render_insight(output, "cross_border", insights, "cross_border",
                   fallback = "<strong>Cross-border analysis loading…</strong>",
                   class = "insight")
    
    # ── Indegree table         # ── Indegree table (HTML, matches reference) ──────────────────────────────
    output$indegree_table_ui <- renderUI({
      ind <- data$indegree[[yr()]]
      if (is.null(ind) || nrow(ind) == 0) return(p("No data"))
      ind <- ind[order(-ind$count), ][seq_len(min(10, nrow(ind))), ]
      max_v <- max(ind$count, 1)
      
      rows <- lapply(seq_len(nrow(ind)), function(i) {
        r   <- ind[i, ]
        pct <- round(100 * r$count / max_v)
        tags$tr(
          tags$td(class = "mono", style = "color:var(--muted)", i),
          tags$td(style = "font-weight:500", r$district),
          tags$td(class = "tr mono", r$count),
          tags$td(
            div(style = "display:flex;align-items:center;gap:6px",
                div(style = paste0("width:60px;height:6px;background:#e5e7eb;border-radius:3px;overflow:hidden"),
                    div(style = paste0("width:", pct, "%;height:100%;background:#0d9488;border-radius:3px"))
                ),
                tags$span(style = "font-size:10px;color:var(--muted)",
                          paste0(pct, "%"))
            )
          )
        )
      })
      
      tags$table(class = "zd-tbl",
                 tags$thead(tags$tr(
                   tags$th("#"),
                   tags$th("District"),
                   tags$th(class = "tr", "Indegree"),
                   tags$th("Centrality")
                 )),
                 do.call(tags$tbody, rows)
      )
    })
    
    # ── Season badge ──────────────────────────────────────────────────────────
    output$season_badge <- renderUI({
      span(class = "bdg b-tel",
           if (yr() == "all") "All time" else yr())
    })
    
    # ── Season boxes ──────────────────────────────────────────────────────────
    output$season_boxes <- renderUI({
      s <- data$seasons[[yr()]]
      if (is.null(s)) return(NULL)
      
      seasons_meta <- list(
        list(key = "Deyr",   icon = "\u2614", label = "Deyr",   period = "October \u2013 November", color = COL_RED),
        list(key = "Guul",   icon = "\U0001F331", label = "Guu\u2019l", period = "April \u2013 June",     color = COL_GREEN),
        list(key = "Jilaal", icon = "\u2744",  label = "Jilaal", period = "December \u2013 March",   color = COL_BLUE),
        list(key = "Xaaga",  icon = "\u2600",  label = "Xaaga",  period = "July \u2013 September",   color = COL_ORANGE)
      )
      
      boxes <- lapply(seasons_meta, function(m) {
        vals <- s[[m$key]]
        cnt  <- if (!is.null(vals) && !is.null(vals$count)) vals$count else 0L
        pct  <- if (!is.null(vals) && !is.null(vals$pct))   vals$pct   else 0L
        div(class = "season-box",
            style = paste0("border-left: 3px solid ", m$color),
            div(class = "season-label",
                style = paste0("color:", m$color),
                paste(m$icon, m$label)),
            div(class = "season-period", m$period),
            div(class = "season-value",
                format(cnt, big.mark = ",")),
            div(class = "season-sub", paste0(pct, "% of exits"))
        )
      })
      div(class = "season-grid", tagList(boxes))
    })
    
    # ── Migratory routes ──────────────────────────────────────────────────────
    output$routes_list <- renderUI({
      rt <- data$routes[[yr()]]
      # routes is a list of lists: list(list(n="...", c=123), ...)
      if (is.null(rt) || length(rt) == 0) {
        return(div(class = "muted", "No route data available."))
      }
      # Handle both transposed format and direct list format
      if (is.list(rt) && !is.null(rt[[1]]$n)) {
        names_v  <- sapply(rt, `[[`, "n")
        counts_v <- sapply(rt, `[[`, "c")
      } else {
        return(div(class = "muted", "No route data available."))
      }
      valid    <- !is.na(names_v) & names_v != ""
      names_v  <- names_v[valid]
      counts_v <- counts_v[valid]
      if (length(names_v) == 0) return(div(class = "muted", "No route data available."))
      max_c <- max(counts_v, 1)
      
      items <- mapply(function(n, c, i) {
        pct <- round(100 * c / max_c)
        div(class = "route-row",
            div(class = "route-rank", i),
            div(class = "route-name", n),
            div(class = "route-bar",
                div(class = "route-fill", style = paste0("width:", pct, "%"))
            ),
            div(class = "route-count", c)
        )
      }, names_v, counts_v, seq_along(names_v), SIMPLIFY = FALSE)
      
      do.call(div, c(list(class = "routes-list"), items))
    })
    
  })
}