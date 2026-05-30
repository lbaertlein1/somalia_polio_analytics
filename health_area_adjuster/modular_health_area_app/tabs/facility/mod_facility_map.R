facilityMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = 'position: relative; height: 100%;',
      
      leaflet::leafletOutput(
        ns('map'),
        width = '100%',
        height = '100%'
      ),
      
      div(
        id = ns('loading_overlay'),
        style = "
          position: absolute;
          inset: 0;
          display: flex;
          align-items: center;
          justify-content: center;
          background: rgba(255,255,255,0.75);
          z-index: 1000;
        ",
        div(
          style = '
            background: rgba(255,255,255,0.96);
            padding: 12px 18px;
            border: 1px solid #D9D9D9;
            border-radius: 6px;
            box-shadow: 0 1px 6px rgba(0,0,0,0.08);
            font-size: 16px;
            font-weight: 600;
            color: #333333;
          ',
          'Loading district data...'
        )
      )
    )
  )
}

facilityMapServer <- function(
    id,
    district_sf,
    facility_data_r,
    selected_id_r,
    on_marker_drag,
    on_add_facility,
    adding_facility_r,
    show_buffer = TRUE,
    all_district_densities,
    show_pop_r     = reactive(FALSE),
    landmarks_r    = reactive(NULL),   # data frame: landmark_id, landmark_name, lat, lon
    subdivisions_r = reactive(NULL)
) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # tracks whether the district identity has actually changed
    last_district_key <- reactiveVal(NULL)
    
    show_loading <- function() {
      session$sendCustomMessage(
        'toggle_facility_loading',
        list(
          id = ns('loading_overlay'),
          show = TRUE
        )
      )
    }
    
    hide_loading <- function() {
      session$sendCustomMessage(
        'toggle_facility_loading',
        list(
          id = ns('loading_overlay'),
          show = FALSE
        )
      )
    }
    
    # --------------------------------------------------
    # Show loading only when district identity changes
    # --------------------------------------------------
    
    observe({
      req(district_sf())
      req(nrow(district_sf()) > 0)
      
      new_key <- paste0(
        district_sf()$district_name[[1]],
        '_',
        district_sf()$admin_id[[1]]
      )
      
      if (!identical(last_district_key(), new_key)) {
        last_district_key(new_key)
        cat('[facilityMap] NEW district detected -> show loading\n')
        show_loading()
      }
    })
    
    # --------------------------------------------------
    # Helpers
    # --------------------------------------------------
    
    # REPLACE make_facility_icon with this:
    make_facility_icon <- function(is_selected = FALSE, is_sia = FALSE) {
      icon_url <- if (isTRUE(is_selected)) {
        'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png'
      } else if (isTRUE(is_sia)) {
        'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png'
      } else {
        'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-grey.png'
      }
      
      leaflet::makeIcon(
        iconUrl    = icon_url,
        shadowUrl  = 'https://unpkg.com/leaflet@1.9.4/dist/images/marker-shadow.png',
        iconWidth  = 25, iconHeight  = 41,
        iconAnchorX = 12, iconAnchorY = 41
      )
    }
    
    # REPLACE add_one_marker with this:
    add_one_marker <- function(proxy, row, selected_id) {
      is_selected <- !is.null(selected_id) &&
        nzchar(selected_id) &&
        identical(as.character(row$facility_id[[1]]), as.character(selected_id))
      
      is_sia <- isTRUE(row$polio_sia_coordination_site[[1]] == "Yes")
      
      # Build popup HTML — simple table, no modal
      popup_html <- paste0(
        '<div style="font-size:12px;line-height:1.8;min-width:180px;">',
        '<div style="font-weight:700;font-size:13px;margin-bottom:4px;color:#0f172a;">',
        htmltools::htmlEscape(.na_dash(row$facility_name[[1]])),
        '</div>',
        '<table style="border-collapse:collapse;width:100%;">',
        '<tr><td style="color:#64748b;padding-right:10px;">Type</td><td>',
        htmltools::htmlEscape(.na_dash(row$facility_type[[1]])), '</td></tr>',
        '<tr><td style="color:#64748b;padding-right:10px;">Ownership</td><td>',
        htmltools::htmlEscape(.na_dash(row$hf_ownership[[1]])), '</td></tr>',
        '<tr><td style="color:#64748b;padding-right:10px;">Incharge</td><td>',
        htmltools::htmlEscape(.na_dash(row$incharge_name[[1]])), '</td></tr>',
        '<tr><td style="color:#64748b;padding-right:10px;">Coord. Site</td><td>',
        htmltools::htmlEscape(.na_dash(row$polio_sia_coordination_site[[1]])),
        '</td></tr>',
        '</table></div>'
      )
      
      proxy |>
        leaflet::addMarkers(
          lng     = row$lon[[1]],
          lat     = row$lat[[1]],
          layerId = row$facility_id[[1]],
          group   = 'facilities',
          icon    = make_facility_icon(is_selected, is_sia),
          options = leaflet::markerOptions(draggable = TRUE, riseOnHover = TRUE),
          popup   = popup_html,
          label   = row$facility_name[[1]],
          labelOptions = leaflet::labelOptions(
            noHide    = TRUE,
            direction = 'right',
            offset    = c(10, 0),
            textsize  = '11px',
            className = 'hf-tooltip'
          )
        )
    }
    
    # --------------------------------------------------
    # Allowed area cache
    # --------------------------------------------------
    
    allowed_area_val <- reactiveVal(NULL)
    
    observe({
      req(district_sf())
      req(nrow(district_sf()) > 0)
      
      district_geom <- district_sf() |>
        sf::st_make_valid()
      
      this_density <- suppressWarnings(as.numeric(district_geom$u5_pop_density_km2[[1]]))
      
      buffer_m <- get_allowed_distance_m(
        district_density = this_density
      )
      
      district_proj <- district_geom |>
        sf::st_transform(3857)
      
      allowed_area <- district_proj |>
        sf::st_union() |>
        sf::st_buffer(dist = buffer_m)
      
      allowed_area_val(list(
        geom = allowed_area,
        buffer_m = buffer_m
      ))
      
      cat(
        '[facilityMap] density =', round(this_density, 3),
        '| buffer_km =', round(buffer_m / 1000, 2), '\n'
      )
    })
    
    # --------------------------------------------------
    # Base map
    # --------------------------------------------------
    
    output$map <- renderLeaflet({
      req(district_sf())
      req(nrow(district_sf()) > 0)
      
      bbox <- sf::st_bbox(
        sf::st_transform(district_sf(), 4326)
      )
      
      leaflet::leaflet(
        options = leaflet::leafletOptions(
          zoomSnap = 0.25
        )
      ) |>
        leaflet::addTiles(group = 'OpenStreetMap') |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = 'ESRI Satellite'
        ) |>
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron,
          group = 'CARTO Light'
        ) |>
        leaflet::addProviderTiles(
          leaflet::providers$OpenTopoMap,
          group = 'Topo'
        ) |>
        leaflet::addLayersControl(
          baseGroups = c(
            'OpenStreetMap',
            'ESRI Satellite',
            'CARTO Light',
            'Topo'
          ),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::fitBounds(
          lng1 = bbox[['xmin']],
          lat1 = bbox[['ymin']],
          lng2 = bbox[['xmax']],
          lat2 = bbox[['ymax']]
        ) %>%
        leaflet::addControl(
          html = '
          <div style="background:white;padding:8px 10px;border-radius:4px;
                      font-size:12px;line-height:1.8;border:1px solid #ccc;">
            <b>Facilities</b><br>
            <img src="https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png"
                 height="20"> Outreach Coordination Site<br>
            <img src="https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-grey.png"
                 height="20"> Not a Coordination Site<br>
            <img src="https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png"
                 height="20"> Selected
          </div>',
          position = "bottomright",
          layerId  = "facility_legend"
        ) %>%
        leaflet::addScaleBar(
          position = "bottomright",
          options  = leaflet::scaleBarOptions(imperial = FALSE, maxWidth = 200)
        ) |>
        leaflet::addControl(
          html = '
          <div style="background:white;padding:6px 8px;border-radius:4px;
                      border:1px solid #ccc;font-size:18px;line-height:1;
                      text-align:center;">
            &#8593;<br>
            <span style="font-size:10px;font-weight:600;">N</span>
          </div>',
          position = "bottomright"
        )
    })
    
    outputOptions(output, 'map', suspendWhenHidden = FALSE)
    
    # --------------------------------------------------
    # District boundary
    # --------------------------------------------------
    
    observe({
      req(district_sf())
      
      district_geo <- sf::st_transform(district_sf(), 4326)
      bbox <- sf::st_bbox(district_geo)
      
      leaflet::leafletProxy('map', session = session) |>
        leaflet::clearGroup('district') |>
        leaflet::addPolygons(
          data = district_geo,
          group = 'district',
          color = '#333333',
          weight = 2,
          fill = FALSE,
          opacity = 1
        ) |>
        leaflet::fitBounds(
          lng1 = bbox[['xmin']],
          lat1 = bbox[['ymin']],
          lng2 = bbox[['xmax']],
          lat2 = bbox[['ymax']]
        )
      
      cat('[facilityMap] district boundary drawn\n')
    })
    
    # --------------------------------------------------
    # Optional buffer display
    # --------------------------------------------------
    
    observe({
      if (!show_buffer) return()
      req(allowed_area_val())
      
      leaflet::leafletProxy('map', session = session) |>
        leaflet::clearGroup('buffer') |>
        leaflet::addPolygons(
          data = sf::st_transform(allowed_area_val()$geom, 4326),
          group = 'buffer',
          color = '#2C7FB8',
          weight = 2,
          fill = FALSE,
          dashArray = '6,6',
          opacity = 0.8
        )
    })
    
    # --------------------------------------------------
    # WorldPop U5 population raster overlay
    # --------------------------------------------------
    
    u5_worldpop_cache <- reactiveVal(NULL)
    
    get_u5_worldpop_local <- function() {
      if (is.null(u5_worldpop_cache())) {
        u5_worldpop_cache(load_worldpop_u5_raster(worldpop_t_u1_1to4_file))
      }
      u5_worldpop_cache()
    }
    
    # Rebuild the facilities legend whenever show_pop_r changes,
    # appending the WorldPop colour scale when the layer is on.
    observe({
      raster_cols <- pop_palette(5)
      
      swatches_html <- paste(
        vapply(raster_cols, function(clr) {
          paste0(
            '<div style="flex:1;height:10px;background:', clr,
            ';border-top:1px solid #999;border-bottom:1px solid #999;"></div>'
          )
        }, character(1)),
        collapse = ""
      )
      
      pop_section <- if (isTRUE(show_pop_r())) {
        paste0(
          '<div style="height:6px;"></div>',
          '<div style="font-size:11px;font-weight:600;margin-bottom:4px;">',
          'WorldPop U5 Population</div>',
          '<div style="display:flex;gap:0;margin-bottom:3px;">',
          swatches_html, '</div>',
          '<div style="display:flex;justify-content:space-between;',
          'font-size:10px;color:#666;">',
          '<span>Low</span><span></span><span></span><span></span><span>High</span>',
          '</div>'
        )
      } else {
        ""
      }
      
      legend_html <- paste0(
        '<div style="background:white;padding:8px 10px;border-radius:4px;',
        'font-size:12px;line-height:1.8;border:1px solid #ccc;">',
        '<b>Facilities</b><br>',
        '<img src="https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png"',
        ' height="20"> Outreach Coordination Site<br>',
        '<img src="https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-grey.png"',
        ' height="20"> Not a Coordination Site<br>',
        '<img src="https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png"',
        ' height="20"> Selected<br>',
        '<span style="display:inline-block;width:10px;height:10px;border-radius:50%;',
        'background:#7c3aed;margin-right:6px;vertical-align:middle;"></span>Landmark',
        pop_section,
        '</div>'
      )
      
      leaflet::leafletProxy('map', session = session) |>
        leaflet::removeControl("facility_legend") |>
        leaflet::addControl(
          html     = legend_html,
          position = "bottomright",
          layerId  = "facility_legend"
        )
    })
    
    observe({
      proxy <- leaflet::leafletProxy('map', session = session)
      
      if (!isTRUE(show_pop_r())) {
        proxy |> leaflet::clearGroup('pop_raster')
        return()
      }
      
      req(district_sf())
      req(nrow(district_sf()) > 0)
      
      pop_sf <- tryCatch(
        make_population_overlay_sf(
          district_sf = sf::st_transform(district_sf(), 4326),
          u5_rast     = get_u5_worldpop_local()
        ),
        error = function(e) {
          cat('[facilityMap] population overlay error:', e$message, '\n')
          NULL
        }
      )
      
      if (is.null(pop_sf) || nrow(pop_sf) == 0) {
        showNotification(
          'No population data available for this district.',
          type = 'warning', duration = 4
        )
        return()
      }
      
      proxy |>
        leaflet::clearGroup('pop_raster') |>
        leaflet::addPolygons(
          data        = pop_sf,
          group       = 'pop_raster',
          fillColor   = ~fill_color,
          fillOpacity = 0.6,
          stroke      = FALSE,
          options     = leaflet::pathOptions(interactive = FALSE)
        )
      
      cat('[facilityMap] population overlay drawn, n =', nrow(pop_sf), '\n')
    })
    
    # --------------------------------------------------
    # Facility markers
    # --------------------------------------------------
    
    observe({
      # Explicitly depend on district_sf so this observer re-fires on every
      # district change, even when facility_data stays an empty df (NULL->NULL
      # on rv$odk_sf would otherwise not invalidate the reactive).
      district_sf()
      
      df <- facility_data_r()
      if (is.null(df)) {
        cat('[facilityMap] facility_data_r is NULL\n')
        return()
      }
      
      proxy <- leaflet::leafletProxy('map', session = session) |>
        leaflet::clearGroup('facilities')
      
      if (nrow(df) > 0) {
        selected_id <- selected_id_r()
        
        for (i in seq_len(nrow(df))) {
          row <- df[i, , drop = FALSE]
          proxy <- add_one_marker(proxy, row, selected_id)
        }
      }
      
      cat('[facilityMap] markers drawn, n =', nrow(df), '\n')
      
      later::later(function() {
        hide_loading()
        cat('[facilityMap] loading overlay hidden\n')
      }, delay = 0.3)
    })
    
    # --------------------------------------------------
    # Add facility on map click
    # --------------------------------------------------
    
    observeEvent(input$map_click, {
      click <- input$map_click
      req(!is.null(click))
      
      if (!isTRUE(adding_facility_r())) {
        return()
      }
      
      req(district_sf())
      req(nrow(district_sf()) > 0)
      
      clicked_pt <- sf::st_as_sf(
        data.frame(
          lon = as.numeric(click$lng),
          lat = as.numeric(click$lat)
        ),
        coords = c('lon', 'lat'),
        crs = 4326
      )
      
      district_geom <- tryCatch({
        d <- district_sf() |> sf::st_transform(4326) |> safe_make_valid()
        # s2 on shinyapps.io is strict about degenerate vertices — disable for union
        old_s2 <- sf::sf_use_s2(FALSE)
        on.exit(sf::sf_use_s2(old_s2), add = TRUE)
        sf::st_union(d)
      }, error = function(e) {
        district_sf() |> sf::st_transform(4326) |> sf::st_union()
      })
      
      inside_district <- lengths(sf::st_within(clicked_pt, district_geom)) > 0
      
      cat(
        '[facilityMap] add click at lon =', as.numeric(click$lng),
        'lat =', as.numeric(click$lat),
        '| inside_district =', inside_district, '\n'
      )
      
      if (!isTRUE(inside_district)) {
        showNotification(
          'New facility must be placed inside the district boundary.',
          type = 'error',
          duration = 4
        )
        return()
      }
      
      on_add_facility(
        lat = as.numeric(click$lat),
        lon = as.numeric(click$lng)
      )
    })
    
    # --------------------------------------------------
    # Drag validation
    # --------------------------------------------------
    
    observeEvent(input$map_marker_dragend, {
      info <- input$map_marker_dragend
      req(!is.null(info$id), !is.null(info$lat), !is.null(info$lng))
      req(allowed_area_val())
      
      allowed_area <- allowed_area_val()$geom
      
      dragged_pt <- sf::st_as_sf(
        data.frame(
          lon = as.numeric(info$lng),
          lat = as.numeric(info$lat)
        ),
        coords = c('lon', 'lat'),
        crs = 4326
      ) |>
        sf::st_transform(3857)
      
      is_allowed <- lengths(sf::st_within(dragged_pt, allowed_area)) > 0
      
      cat(
        '[facilityMap] drag facility =', as.character(info$id),
        '| allowed =', is_allowed,
        '| buffer_km =', round(allowed_area_val()$buffer_m / 1000, 2), '\n'
      )
      
      if (isTRUE(is_allowed)) {
        on_marker_drag(
          facility_id = as.character(info$id),
          lat = as.numeric(info$lat),
          lon = as.numeric(info$lng)
        )
      } else {
        showNotification(
          paste0(
            'Location must remain within ',
            round(allowed_area_val()$buffer_m / 1000, 1),
            ' km of the district boundary.'
          ),
          type = 'error',
          duration = 4
        )
        
        df <- facility_data_r()
        req(!is.null(df), nrow(df) > 0)
        
        row <- df[df$facility_id == as.character(info$id), , drop = FALSE]
        
        if (nrow(row) == 1) {
          leaflet::leafletProxy('map', session = session) |>
            leaflet::removeMarker(layerId = as.character(info$id)) |>
            add_one_marker(row = row, selected_id = selected_id_r())
        }
      }
    })
    
    # --------------------------------------------------
    # Marker selection
    # --------------------------------------------------
    
    observeEvent(input$map_marker_click, {
      info <- input$map_marker_click
      req(!is.null(info$id))
      # Ignore clicks on landmark markers (prefixed 'lm_')
      if (!startsWith(as.character(info$id), 'lm_')) {
        selected_id_r(as.character(info$id))
      }
    })
    
    # --------------------------------------------------
    # Landmark dots — carried forward from orientation tab
    # Purple circle + bare text label, no background bubble
    # --------------------------------------------------
    
    observe({
      req(district_sf())   # re-fire when map becomes ready, not just when landmarks change
      proxy <- leaflet::leafletProxy('map', session = session)
      proxy |> leaflet::clearGroup('landmarks')
      
      lm <- landmarks_r()
      if (is.null(lm) || nrow(lm) == 0) return()
      
      for (i in seq_len(nrow(lm))) {
        proxy <- proxy |>
          leaflet::addCircleMarkers(
            lng         = lm$lon[i],
            lat         = lm$lat[i],
            layerId     = paste0('lm_', lm$landmark_id[i]),
            group       = 'landmarks',
            radius      = 5,
            color       = '#7c3aed',
            fillColor   = '#7c3aed',
            fillOpacity = 1,
            weight      = 0,
            options     = leaflet::pathOptions(interactive = FALSE),
            label       = lm$landmark_name[i],
            labelOptions = leaflet::labelOptions(
              noHide    = TRUE,
              direction = 'right',
              offset    = c(8, 0),
              className = 'landmark-label'
            )
          )
      }
    })
    
    # ── Subdivision boundaries ────────────────────────────────────────────────
    # Use observeEvent on both district_sf AND subdivisions_r so the layer
    # redraws when either changes. district_sf() guard ensures the map exists.
    .draw_subdivision_boundaries <- function() {
      proxy <- leaflet::leafletProxy('map', session = session) |>
        leaflet::clearGroup('subdivisions')
      subs <- tryCatch(subdivisions_r(), error = function(e) NULL)
      if (is.null(subs) || nrow(subs) == 0) return()
      proxy |>
        leaflet::addPolygons(
          data        = subs,
          group       = 'subdivisions',
          color       = '#7c3aed',
          weight      = 2,
          dashArray   = '6,4',
          fill        = FALSE,
          opacity     = 0.8,
          label       = subs$subdivision_name,
          labelOptions = leaflet::labelOptions(
            noHide    = TRUE,
            direction = 'center',
            textOnly  = TRUE,
            style     = list('font-size' = '11px', 'font-weight' = '700',
                             'color' = '#7c3aed')
          )
        )
    }
    observeEvent(district_sf(), { .draw_subdivision_boundaries() },
                 ignoreNULL = TRUE, ignoreInit = FALSE)
    observeEvent(subdivisions_r(), { .draw_subdivision_boundaries() },
                 ignoreNULL = FALSE, ignoreInit = FALSE)
  })
}