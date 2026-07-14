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
            color       = "#94a3b8", weight = 0.6, opacity = 0.7
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
      proxy |> clearGroup("camps") |> clearGroup("flows") |> clearGroup("indegree_circles")
      
      ind <- data$indegree[[yr()]]
      fl  <- data$flows[[yr()]]
      cen <- data$centroids
      
      # Indegree circles + district labels
      if (!is.null(ind) && nrow(ind) > 0 && !is.null(cen) && nrow(cen) > 0) {
        
        # Non-mutating copies; normalize names consistently
        cen_norm <- cen
        ind_norm <- ind
        cen_norm$district <- normalise_district(cen_norm$district)
        ind_norm$district <- normalise_district(ind_norm$district)
        
        # Join and collapse to ONE centroid per district; cast coords to numeric
        ind_join <- dplyr::left_join(ind_norm, cen_norm, by = "district") |>
          dplyr::filter(!is.na(lat), !is.na(lon), !is.na(district)) |>
          dplyr::mutate(
            lon = suppressWarnings(as.numeric(lon)),
            lat = suppressWarnings(as.numeric(lat)),
            count = suppressWarnings(as.numeric(count))
          ) |>
          dplyr::filter(!is.na(lon), !is.na(lat), !is.na(count)) |>
          dplyr::group_by(district) |>
          dplyr::summarise(
            lon   = lon[1],
            lat   = lat[1],
            count = sum(count, na.rm = TRUE),
            .groups = "drop"
          )
        
        # ---- Decide which districts to show circles on ----
        # Option A: explicit whitelist (uncomment and set to match your mock-up)
        # show_list <- c("Belet-hawa","Dolow","Elwak","Baardhere","Baidoa","Beledweyne","El Barde","Luuq")
        # ind_show  <- ind_join[dplyr::row_number(ind_join$district) > 0, ] # placeholder
        # ind_show  <- ind_join[ normalise_district(ind_join$district) %in% normalise_district(show_list), ]
        
        # Option B (default): show top-N by indegree count
        N_TOP <- 10L
        ind_show <- ind_join |>
          dplyr::arrange(dplyr::desc(count)) |>
          dplyr::slice_head(n = N_TOP)
        
        if (nrow(ind_show) > 0) {
          max_c <- max(ind_show$count, 1)
          
          # Indegree circle
          proxy |>
            addCircleMarkers(
              data      = ind_show,
              lng       = ~lon, lat = ~lat,
              radius    = ~pmax(10, pmin(40, 10 + sqrt(count / max_c) * 30)),
              fillColor = "#0d9488", fillOpacity = 0.18,
              color     = "#0d9488", weight = 3, opacity = 0.90,
              group     = "indegree_circles",
              label     = ~paste0(district, " — Indegree: ", count)
            )
          
          # District labels anchored at centroid
          for (i in seq_len(nrow(ind_show))) {
            r <- ind_show[i, ]
            proxy |>
              addMarkers(
                lng = r$lon,
                lat = r$lat,
                icon = leaflet::makeIcon(
                  iconUrl   = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7",
                  iconWidth = 1, iconHeight = 1
                ),
                label = r$district,
                labelOptions = labelOptions(
                  noHide = TRUE, direction = "top", textOnly = FALSE,
                  style = list(
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
      
      
      # Camp dots
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
      
      #Cross-district flows (≥3 camps)
      if (sf && !is.null(fl) && nrow(fl) > 0 && !is.null(cen) && nrow(cen) > 0) {
        # Non-mutating copies; normalize names consistently
        cen_norm <- cen
        fl_norm  <- fl
        fl_norm$prev_c <- normalise_district(as.character(fl_norm$prev_c))
        fl_norm$next_c <- normalise_district(as.character(fl_norm$next_c))
        fl_norm$count  <- suppressWarnings(as.numeric(fl_norm$count))
        fl_norm$count[is.na(fl_norm$count)] <- 0
        cen_norm$district <- normalise_district(cen_norm$district)
        
        cross <- fl_norm[
          fl_norm$prev_c != fl_norm$next_c &
            !is.na(fl_norm$prev_c) &
            !is.na(fl_norm$next_c) &
            fl_norm$count >= 3,
        ]
        
        if (nrow(cross) > 0) {
          cen_map <- split(cen_norm, cen_norm$district)
          
          for (i in seq_len(nrow(cross))) {
            from_df <- cen_map[[ cross$prev_c[i] ]]
            to_df   <- cen_map[[ cross$next_c[i] ]]
            
            # Skip if either side missing
            if (is.null(from_df) || is.null(to_df)) next
            
            # Use first row explicitly; cast to numeric
            from_lon <- suppressWarnings(as.numeric(from_df$lon[1]))
            from_lat <- suppressWarnings(as.numeric(from_df$lat[1]))
            to_lon   <- suppressWarnings(as.numeric(to_df$lon[1]))
            to_lat   <- suppressWarnings(as.numeric(to_df$lat[1]))
            if (is.na(from_lon) || is.na(from_lat) || is.na(to_lon) || is.na(to_lat)) next
            
            # Gentle curvature
            mid_lat <- (from_lat + to_lat) / 2 + (to_lon - from_lon) * 0.05
            mid_lon <- (from_lon + to_lon) / 2 - (to_lat - from_lat) * 0.05
            
            # Width scales with volume
            wt <- 1.2 + log(cross$count[i] + 1) * 0.9
            
            # Black outline
            proxy |>
              addPolylines(
                lng = c(from_lon, mid_lon, to_lon),
                lat = c(from_lat, mid_lat, to_lat),
                color  = "#000000",
                weight = wt + 2,
                opacity = 0.25,
                smoothFactor = 1,
                group = "flows"
              )
            
            # Orange arrow
            proxy |>
              addPolylines(
                lng = c(from_lon, mid_lon, to_lon),
                lat = c(from_lat, mid_lat, to_lat),
                color  = "#d97706",
                weight = wt,
                opacity = 0.92,
                smoothFactor = 1,
                label = paste0(cross$prev_c[i], " → ", cross$next_c[i], ": ", cross$count[i], " camps"),
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
      if (!is.null(tile_url)) {
        leafletProxy(ns("mov_map")) |>
          removeTiles(layerId = "base") |>
          addTiles(urlTemplate = tile_url, layerId = "base")
      }
    })
    
    # ── Year badge ────────────────────────────────────────────────────────────
    output$yr_badge <- renderUI({
      span(class = "bdg b-tel", if (yr() == "all") "All time" else yr())
    })
    
    # ── AI insights (rendered from shared insights rv) ────────────────────────
    render_insight(output, "key_finding", insights, "movement_key",
                   fallback = "<strong>Movement patterns loading…</strong>", class = "insight")
    
    render_insight(output, "cross_border", insights, "cross_border",
                   fallback = "<strong>Cross-border analysis loading…</strong>", class = "insight")
    
    # ── Indegree table ──────────────────────────────
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
                tags$span(style = "font-size:10px;color:var(--muted)", paste0(pct, "%"))
            )
          )
        )
      })
      
      tags$table(
        class = "zd-tbl",
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
      span(class = "bdg b-tel", if (yr() == "all") "All time" else yr())
    })
    
    # ── Season boxes ──────────────────────────────────────────────────────────
    output$season_boxes <- renderUI({
      s <- data$seasons[[yr()]]
      if (is.null(s)) return(NULL)
      
      seasons_meta <- list(
        list(key = "Deyr",   icon = "\u2614",  label = "Deyr",   period = "October \u2013 November", color = COL_RED),
        list(key = "Guul",   icon = "\U0001F331", label = "Guu\u2019l", period = "April \u2013 June",  color = COL_GREEN),
        list(key = "Jilaal", icon = "\u2744",  label = "Jilaal", period = "December \u2013 March",  color = COL_BLUE),
        list(key = "Xaaga",  icon = "\u2600",  label = "Xaaga",  period = "July \u2013 September",  color = COL_ORANGE)
      )
      
      boxes <- lapply(seasons_meta, function(m) {
        vals <- s[[m$key]]
        cnt  <- if (!is.null(vals) && !is.null(vals$count)) vals$count else 0L
        pct  <- if (!is.null(vals) && !is.null(vals$pct))   vals$pct   else 0L
        div(class = "season-box",
            style = paste0("border-left: 3px solid ", m$color),
            div(class = "season-label", style = paste0("color:", m$color), paste(m$icon, m$label)),
            div(class = "season-period", m$period),
            div(class = "season-value", format(cnt, big.mark = ",")),
            div(class = "season-sub", paste0(pct, "% of exits"))
        )
      })
      div(class = "season-grid", tagList(boxes))
    })
    
    # ── Migratory routes table (3+ camps) ───────────────────────────────
    output$routes_list <- renderUI({
      # Use flows instead of ODK route_name because route_name is mostly empty
      rt <- data$flows[[yr()]]
      if (is.null(rt) || nrow(rt) == 0) {
        return(div(class = "muted", "No route data available."))
      }
      
      # Normalize and filter
      rt <- rt |>
        dplyr::mutate(
          prev_c = normalise_district(prev_c),
          next_c = normalise_district(next_c)
        ) |>
        dplyr::filter(count >= 3) |>
        dplyr::mutate(route = paste(prev_c, "→", next_c))
      
      # If no surviving routes
      if (nrow(rt) == 0) {
        return(div(class = "muted", "No high-volume migratory routes."))
      }
      
      # Sort by count most traveled first
      rt <- rt |>
        dplyr::arrange(dplyr::desc(count))
      
      max_c <- max(rt$count, 1)
      
      # Build UI rows
      items <- mapply(function(route_label, c, i) {
        pct <- round(100 * c / max_c)
        div(class = "route-row",
            div(class = "route-rank", i),
            div(class = "route-name", route_label),
            div(class = "route-bar",
                div(class = "route-fill", style = paste0("width:", pct, "%"))
            ),
            div(class = "route-count", c)
        )
      }, rt$route, rt$count, seq_len(nrow(rt)), SIMPLIFY = FALSE)
      
      div(class = "routes-list", items)
    })
    
  })  
}     