# R/mod_movement.R
# Tab 1 — Movement Patterns
# Leaflet map (indegree circles + flow arrows + camp dots)
# Key finding box | Indegree table | Seasons | Routes | Cross-border

movement_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # ── Map + right column grid ──────────────────────────────────────────────
    div(class = "mov-grid",
        
        # Left: map
        div(class = "map-col",
            div(class = "card",
                div(class = "card-header",
                    span(class = "card-title", "District Movement Map"),
                    div(class = "map-controls",
                        div(class = "map-toggle",
                            tags$input(type = "checkbox", id = ns("show_camps"),
                                       checked = "checked"),
                            tags$label(`for` = ns("show_camps"), "Nomadic camps visited")
                        ),
                        div(class = "map-toggle",
                            tags$input(type = "checkbox", id = ns("show_flows"),
                                       checked = "checked"),
                            tags$label(`for` = ns("show_flows"), "Cross-district flows")
                        ),
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
                        div(class = "leg-circle",
                            style = "background:#0d9488;width:14px;height:14px;border-radius:50%;opacity:.75"),
                        "Destination (size = incoming camps)"
                    ),
                    div(class = "leg-item",
                        div(style = "width:24px;height:3px;background:#d97706;margin-right:4px"),
                        "Cross-district flow"
                    ),
                    div(class = "leg-item",
                        div(class = "leg-circle",
                            style = "background:#0d9488;width:6px;height:6px;border-radius:50%;opacity:.6"),
                        "Nomadic camps visited"
                    )
                )
            )
        ),
        
        # Right: panels
        div(class = "right-col",
            
            # Key finding
            div(class = "card",
                div(class = "card-header",
                    span(class = "card-title", "Key Finding"),
                    uiOutput(ns("yr_badge"))
                ),
                div(class = "card-body",
                    uiOutput(ns("key_finding_1")),
                    tags$br(),
                    uiOutput(ns("key_finding_2"))
                )
            ),
            
            # Indegree table
            div(class = "card",
                div(class = "card-header",
                    span(class = "card-title", "Top Destination Districts"),
                    span(class = "card-subtitle", "Indegree centrality")
                ),
                div(class = "card-body-flush",
                    tableOutput(ns("indegree_table"))
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
    
    # ── Bottom row: routes + cross-border ─────────────────────────────────────
    div(class = "g2",
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Top Named Migratory Routes")
            ),
            div(class = "card-body", uiOutput(ns("routes_list")))
        ),
        div(class = "card",
            div(class = "card-header",
                span(class = "card-title", "Cross-Border Context")
            ),
            div(class = "card-body",
                uiOutput(ns("cross_border_box"))
            )
        )
    )
  )
}

movement_server <- function(id, data, year_filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    yr <- reactive(year_filter())
    
    # ── Leaflet base map ────────────────────────────────────────────────────
    output$mov_map <- renderLeaflet({
      lmap <- leaflet(options = leafletOptions(scrollWheelZoom = FALSE)) |>
        addProviderTiles("CartoDB.Positron", layerId = "base") |>
        setView(lng = 42.5, lat = 4.5, zoom = 7)
      
      # District polygons
      if (!is.null(data$districts_geojson)) {
        lmap <- lmap |>
          addPolygons(
            data        = data$districts_geojson,
            fillColor   = "#ffffff", fillOpacity = 0.85,
            color       = "#94a3b8", weight = 0.6, opacity = 0.8,
            label       = ~district_name,
            layerId     = ~paste0("dist_", district_name),
            group       = "districts"
          )
        # State outlines — dissolve by state if state field exists
        state_col <- intersect(c("ADM1_EN", "admin1Name_en", "STATE", "Region"),
                               names(data$districts_geojson))[1]
        if (!is.na(state_col)) {
          states_sf <- data$districts_geojson |>
            dplyr::group_by(.data[[state_col]]) |>
            dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
          lmap <- lmap |>
            addPolygons(
              data        = states_sf,
              fillColor   = "transparent", fillOpacity = 0,
              color       = "#1a202c", weight = 2.5, opacity = 0.85,
              label       = ~.data[[state_col]],
              group       = "states"
            )
        }
      }
      lmap
    })
    
    # ── Reactive layer updates ──────────────────────────────────────────────
    observe({
      req(yr())
      proxy <- leafletProxy(ns("mov_map"))
      proxy |> clearGroup("camps") |> clearGroup("flows") |>
        clearGroup("indegree_circles")
      
      ind <- data$indegree[[yr()]]
      fl  <- data$flows[[yr()]]
      cen <- data$centroids
      gps <- data$camp_gps
      
      # Indegree circles
      if (!is.null(ind) && nrow(ind) > 0 && nrow(cen) > 0) {
        ind_join <- dplyr::left_join(ind, cen, by = "district") |>
          dplyr::filter(!is.na(lat), !is.na(lon))
        max_c <- max(ind_join$count, 1)
        proxy |> addCircleMarkers(
          data        = ind_join,
          lng = ~lon, lat = ~lat,
          radius      = ~pmax(6, pmin(30, 6 + 24 * count / max_c)),
          fillColor   = COL_TEAL, fillOpacity = 0.65,
          color       = "white",  weight = 1.5,
          label       = ~paste0(district, ": ", count, " incoming"),
          group       = "indegree_circles"
        )
      }
      
      # Camp GPS dots
      if (!is.null(gps) && nrow(gps) > 0) {
        gps_yr <- if (yr() == "all") gps
        else gps[gps$year == as.integer(yr()), ]
        if (input$show_camps && nrow(gps_yr) > 0) {
          proxy |> addCircleMarkers(
            data      = gps_yr,
            lng = ~lon, lat = ~lat,
            radius    = 3, fillColor = COL_TEAL, fillOpacity = 0.5,
            color     = "white", weight = 0.5, stroke = TRUE,
            label     = ~paste0(district, " | ZD 0-11: ", zd_0to11),
            group     = "camps"
          )
        }
      }
      
      # Flow arrows — drawn as polylines with arrowhead workaround
      if (!is.null(fl) && nrow(fl) > 0 && input$show_flows) {
        cross <- fl[fl$prev_c != fl$next_c & fl$count >= 3, ]
        if (nrow(cross) > 0 && nrow(cen) > 0) {
          for (i in seq_len(nrow(cross))) {
            from <- cen[cen$district == cross$prev_c[i], ]
            to   <- cen[cen$district == cross$next_c[i], ]
            if (nrow(from) == 0 || nrow(to) == 0) next
            wt <- pmax(1, pmin(6, 1 + 5 * cross$count[i] / max(cross$count)))
            proxy |> addPolylines(
              lng   = c(from$lon, to$lon),
              lat   = c(from$lat, to$lat),
              color = "#1a1a1a", weight = wt + 1.5, opacity = 0.3,
              group = "flows"
            ) |> addPolylines(
              lng   = c(from$lon, to$lon),
              lat   = c(from$lat, to$lat),
              color = COL_ORANGE, weight = wt, opacity = 0.85,
              label = paste0(cross$prev_c[i], " → ", cross$next_c[i],
                             ": ", cross$count[i]),
              group = "flows"
            )
          }
        }
      }
    })
    
    # ── Basemap switcher ────────────────────────────────────────────────────
    observeEvent(input$basemap, {
      tile_url <- switch(input$basemap,
                         "carto-light"   = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                         "carto-dark"    = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
                         "osm"           = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                         "satellite"     = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                         "carto-voyager" = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png"
      )
      leafletProxy(ns("mov_map")) |>
        removeTiles(layerId = "base") |>
        addTiles(urlTemplate = tile_url, layerId = "base")
    })
    
    # ── Key finding ─────────────────────────────────────────────────────────
    output$yr_badge <- renderUI({
      span(class = "badge badge-teal",
           if (yr() == "all") "All time" else yr())
    })
    
    output$key_finding_1 <- renderUI({
      fl  <- data$flows[[yr()]]
      if (is.null(fl) || nrow(fl) == 0) return(div("No movement data."))
      total  <- sum(fl$count)
      intra  <- sum(fl$count[fl$prev_c == fl$next_c], na.rm = TRUE)
      intra_pct <- if (total > 0) round(100 * intra / total) else 0
      cross  <- fl[fl$prev_c != fl$next_c & !is.na(fl$prev_c) &
                     !is.na(fl$next_c) & fl$count >= 5, ]
      cross  <- cross[order(-cross$count), ][seq_len(min(3, nrow(cross))), ]
      cross_str <- if (nrow(cross) > 0) {
        paste(paste0("<strong>", cross$prev_c, "↔", cross$next_c,
                     "</strong> (", cross$count, ")"), collapse = ", ")
      } else "No major cross-district flows"
      div(class = "insight",
          HTML(paste0(
            "<strong>", intra_pct, "%+ intra-district movement</strong> — ",
            "camps overwhelmingly cycle within their enumerated district. ",
            "Cross-district flows: ", cross_str, ". ",
            "Microplanning must be district-specific with cross-border alert ",
            "protocols especially for <strong>Dolow</strong> (Ethiopia border)."
          ))
      )
    })
    
    output$key_finding_2 <- renderUI({
      zd  <- data$zd_by_dist[[yr()]]
      if (is.null(zd) || nrow(zd) == 0) return(NULL)
      top <- zd[!is.na(zd$district) & zd$district != "Luuq", ]
      top <- top[order(-top$rate011), ][1, ]
      div(class = "insight insight-warn",
          HTML(if (!is.null(top) && nrow(top) > 0 && !is.na(top$rate011)) {
            paste0("<strong>Highest zero-dose burden: ", top$district,
                   " (", top$rate011, "% of infants unvaccinated)</strong>. ",
                   "Intensify outreach before Deyr (Oct–Nov) dispersal window.")
          } else {
            "<strong>Deyr season (Oct–Nov)</strong> drives most camp exits. Intensify outreach in September."
          })
      )
    })
    
    # ── Indegree table ───────────────────────────────────────────────────────
    output$indegree_table <- renderTable({
      ind <- data$indegree[[yr()]]
      if (is.null(ind) || nrow(ind) == 0) return(NULL)
      ind |>
        dplyr::slice_head(n = 10) |>
        dplyr::mutate(Rank = dplyr::row_number()) |>
        dplyr::select(Rank, District = district, `Incoming camps` = count)
    }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%")
    
    # ── Seasons ─────────────────────────────────────────────────────────────
    output$season_badge <- renderUI({
      span(class = "badge badge-teal",
           if (yr() == "all") "All time" else yr())
    })
    
    output$season_boxes <- renderUI({
      s <- data$seasons[[yr()]]
      make_box <- function(name, color) {
        vals <- s[[name]]
        div(class = "season-box", style = paste0("border-color:", color),
            div(class = "season-label", style = paste0("color:", color), name),
            div(class = "season-period",
                switch(name, Deyr = "Oct–Nov", Guul = "Mar–Jun",
                       Jilaal = "Dec–Feb", Xaaga = "Jul–Sep")),
            div(class = "season-value",
                formatC(vals$count, format = "d", big.mark = ",")),
            div(class = "season-sub", paste0(vals$pct, "% of exits"))
        )
      }
      div(class = "season-grid",
          make_box("Deyr",   COL_RED),
          make_box("Guul",   COL_GREEN),
          make_box("Jilaal", COL_BLUE),
          make_box("Xaaga",  COL_ORANGE)
      )
    })
    
    # ── Routes ──────────────────────────────────────────────────────────────
    output$routes_list <- renderUI({
      rt <- data$routes[[yr()]]
      if (is.null(rt) || length(rt) == 0 || length(rt$route_name) == 0) {
        return(div(class = "muted", "No route data available."))
      }
      names_v <- rt$route_name
      counts_v <- rt$count
      max_c <- max(unlist(counts_v), 1)
      items <- purrr::map2(names_v, counts_v, function(n, c) {
        pct <- round(100 * c / max_c)
        div(class = "route-row",
            div(class = "route-name", n),
            div(class = "route-bar",
                div(class = "route-fill",
                    style = paste0("width:", pct, "%"))
            ),
            div(class = "route-count", c)
        )
      })
      do.call(div, c(list(class = "routes-list"), items))
    })
    
    # ── Cross-border ─────────────────────────────────────────────────────────
    output$cross_border_box <- renderUI({
      cb <- data$cross_border[[yr()]]
      div(
        div(class = "insight",
            HTML(paste0(
              "<strong>Dolow</strong> (Ethiopia border): ",
              "<strong>", formatC(cb$dolow, format = "d", big.mark = ","),
              " camps</strong> enumerated — primary SOM-ETH corridor. ",
              "Waddada Dolow route family carries the highest cross-border volume.<br><br>",
              "<strong>Elwak</strong> (Kenya border): ",
              "<strong>", formatC(cb$elwak, format = "d", big.mark = ","),
              " camps</strong> — SOM-KEN corridor, Elwak↔Baardhere the ",
              "most active cross-district flow."
            ))
        )
      )
    })
  })
}