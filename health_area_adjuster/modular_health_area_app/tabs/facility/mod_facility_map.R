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
    all_district_densities
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
      
      proxy |>
        leaflet::addMarkers(
          lng     = row$lon[[1]],
          lat     = row$lat[[1]],
          layerId = row$facility_id[[1]],
          icon    = make_facility_icon(is_selected, is_sia),
          options = leaflet::markerOptions(draggable = TRUE, riseOnHover = TRUE),
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
    # Density -> allowed distance in meters
    # Least dense = 10 km
    # Most dense = 1 km
    # --------------------------------------------------
    
    get_allowed_distance_m <- function(district_density) {
      district_density <- suppressWarnings(as.numeric(district_density))
      if (length(district_density) == 0) {
        return(5000)
      }
      district_density <- district_density[1]
      
      if (district_density >= 10) {
        return(1000)
      }
      if (district_density >= 1) {
        return(5000)
      }
      return(10000)
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
                 height="20"> SIA Coordination Site<br>
            <img src="https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-grey.png"
                 height="20"> Not a Coordination Site<br>
            <img src="https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png"
                 height="20"> Selected
          </div>',
          position = "bottomright"
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
    # Facility markers
    # --------------------------------------------------
    
    observe({
      df <- facility_data_r()
      if (is.null(df)) {
        cat('[facilityMap] facility_data_r is NULL\n')
        return()
      }
      
      proxy <- leaflet::leafletProxy('map', session = session) |>
        leaflet::clearMarkers()
      
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
      
      district_geom <- district_sf() |>
        sf::st_transform(4326) |>
        sf::st_union()
      
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
      selected_id_r(as.character(info$id))
    })
  })
}

facilityTabUI <- function(id) {
  ns <- NS(id)
  
  tags$script(HTML("
  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
    setTimeout(function() {
      $('.leaflet').each(function() {
        var mapWidget = HTMLWidgets.find('#' + this.id);
        if (mapWidget && mapWidget.getMap) {
          var map = mapWidget.getMap();
          if (map) {
            map.invalidateSize();
          }
        }
      });

      if (window.paintApps) {
        Object.keys(window.paintApps).forEach(function(k) {
          var app = window.paintApps[k];
          if (app && app.map) {
            app.map.invalidateSize();
          }
        });
      }

      window.dispatchEvent(new Event('resize'));
    }, 100);

    setTimeout(function() {
      $('.leaflet').each(function() {
        var mapWidget = HTMLWidgets.find('#' + this.id);
        if (mapWidget && mapWidget.getMap) {
          var map = mapWidget.getMap();
          if (map) {
            map.invalidateSize();
          }
        }
      });

      if (window.paintApps) {
        Object.keys(window.paintApps).forEach(function(k) {
          var app = window.paintApps[k];
          if (app && app.map) {
            app.map.invalidateSize();
          }
        });
      }

      window.dispatchEvent(new Event('resize'));
    }, 400);
  });
"))
  
  tagList(
    div(
      id = ns('app_row'),
      class = 'facility-layout',
      
      div(
        id = ns('leftbar'),
        class = 'facility-leftbar',
        div(
          class = 'rightbar-title',
          'Health Facility Mapping'
        ),
        p('Review the preset health facility points for the selected district.'),
        tags$ul(
          tags$li('Drag each point to the correct location if needed.'),
          tags$li('Edit the facility attributes in the table below.'),
          tags$li('Only facilities marked Yes for SIA Coordination Site will be used in the Health Area Mapping tab.')
        ),
        actionButton(
          ns('add_facility'),
          'Add new facility',
          icon = icon('plus'),
          width = '100%'
        ),
        div(
          style = 'margin-top: 8px; font-size: 12px; color: #666;',
          'Click this button, then click the map to place the new facility.'
        )
      ),
      
      div(
        class = 'facility-main',
        
        div(
          id = ns('mapwrap'),
          class = 'facility-mapwrap',
          facilityMapUI(ns('map'))
        ),
        
        div(
          id = ns('tablewrap'),
          class = 'facility-tablewrap',
          facilityTableUI(ns('table'))
        )
      )
    )
  )
}

facilityTabServer <- function(id, zone, region, district, district_ready) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      facility_sf = NULL
    )
    
    selected_id <- reactiveVal(NULL)
    adding_facility <- reactiveVal(FALSE)
    
    district_base <- reactive({
      req(isTRUE(district_ready()))
      req(zone(), region(), district())
      
      district_sf <- districts_shp |>
        dplyr::filter(
          zone_name == zone(),
          region_name == region(),
          district_name == district()
        ) |>
        dplyr::select(
          admin_id,
          district_name,
          region_id,
          region_name,
          zone_id,
          zone_name,
          u5_pop_density_km2,
          geometry
        )
      
      req(nrow(district_sf) >= 1)
      
      district_sf |>
        dplyr::summarise(
          admin_id = dplyr::first(admin_id),
          district_name = dplyr::first(district_name),
          region_id = dplyr::first(region_id),
          region_name = dplyr::first(region_name),
          zone_id = dplyr::first(zone_id),
          zone_name = dplyr::first(zone_name),
          u5_pop_density_km2 = dplyr::first(u5_pop_density_km2),
          geometry = sf::st_union(geometry),
          .groups = 'drop'
        ) |>
        sf::st_as_sf() |>
        safe_make_valid()
    })
    
    observeEvent(district(), {
      req(isTRUE(district_ready()))
      district_sf <- district_base()
      
      cat('facilityTabServer district changed:', district(), '\n')
      cat('district_base rows:', nrow(district_sf), '\n')
      
      district_seed <- sum(utf8ToInt(district()))
      
      rv$facility_sf <- make_starter_facilities(
        district_sf = district_sf,
        district_name = district(),
        n_facilities = n_start_dfas,
        seed = district_seed
      )
      
      cat('starter facilities created, rows:', nrow(rv$facility_sf), '\n')
      cat('starter facility names:', paste(rv$facility_sf$facility_name, collapse = ', '), '\n')
      cat(
        'starter facility lon range:',
        min(rv$facility_sf$lon), max(rv$facility_sf$lon), '\n'
      )
      cat(
        'starter facility lat range:',
        min(rv$facility_sf$lat), max(rv$facility_sf$lat), '\n'
      )
      
      adding_facility(FALSE)
      
      if (!is.null(rv$facility_sf) && nrow(rv$facility_sf) > 0) {
        selected_id(as.character(rv$facility_sf$facility_id[1]))
      } else {
        selected_id(NULL)
      }
    }, ignoreInit = FALSE)
    
    facility_data <- reactive({
      cat('facility_data reactive called\n')
      out <- facility_sf_to_df(rv$facility_sf)
      cat('facility_data rows:', nrow(out), '\n')
      out
    })
    
    coordination_sites <- reactive({
      df <- facility_data()
      if (nrow(df) == 0) {
        return(df)
      }
      
      df |>
        dplyr::filter(polio_sia_coordination_site == 'Yes')
    })
    
    observeEvent(input$add_facility, {
      adding_facility(TRUE)
      showNotification(
        'Click on the map to place the new facility.',
        type = 'message',
        duration = 3
      )
    })
    
    update_marker_position <- function(facility_id, lat, lon) {
      req(!is.null(rv$facility_sf), nrow(rv$facility_sf) > 0)
      
      idx <- which(rv$facility_sf$facility_id == facility_id)
      req(length(idx) == 1)
      
      rv$facility_sf$lon[idx] <- as.numeric(lon)
      rv$facility_sf$lat[idx] <- as.numeric(lat)
      
      geom <- sf::st_sfc(
        sf::st_point(c(as.numeric(lon), as.numeric(lat))),
        crs = 4326
      )
      rv$facility_sf$geometry[idx] <- geom[[1]]
    }
    
    add_new_facility <- function(lat, lon) {
      req(!is.null(rv$facility_sf))
      
      new_id <- paste0(
        'facility_',
        format(Sys.time(), '%Y%m%d%H%M%S'),
        '_',
        sample(1000:9999, 1)
      )
      
      existing_n <- nrow(rv$facility_sf)
      
      template_row <- rv$facility_sf[1, , drop = FALSE]
      template_row$facility_id <- new_id
      template_row$facility_name <- paste('New Facility', existing_n + 1)
      template_row$facility_type <- 'Health Post'
      template_row$operational <- 'Operational'
      template_row$ri_services <- 'Yes'
      template_row$polio_sia_coordination_site <- 'No'
      template_row$lon <- as.numeric(lon)
      template_row$lat <- as.numeric(lat)
      template_row$geometry <- sf::st_sfc(
        sf::st_point(c(as.numeric(lon), as.numeric(lat))),
        crs = 4326
      )
      
      rv$facility_sf <- rbind(rv$facility_sf, template_row)
      
      selected_id(as.character(new_id))
      adding_facility(FALSE)
      
      showNotification(
        'New facility added.',
        type = 'message',
        duration = 3
      )
    }
    
    update_table_value <- function(row, col, value) {
      req(!is.null(rv$facility_sf), nrow(rv$facility_sf) >= row)
      
      df <- facility_sf_to_df(rv$facility_sf)
      
      editable_names <- c(
        'facility_id',
        'facility_name',
        'operational',
        'ri_services',
        'facility_type',
        'polio_sia_coordination_site',
        'lon',
        'lat'
      )
      
      col_name <- editable_names[col + 1]
      
      if (col_name %in% c('facility_id', 'lon', 'lat')) {
        return()
      }
      
      if (col_name == 'operational') {
        value <- if (value %in% c('Operational', 'Not Operational')) value else df[[col_name]][row]
      }
      
      if (col_name == 'ri_services') {
        value <- if (value %in% c('Yes', 'No')) value else df[[col_name]][row]
      }
      
      if (col_name == 'facility_type') {
        value <- if (value %in% c('Health Post', 'Health Center', 'Hospital')) value else df[[col_name]][row]
      }
      
      if (col_name == 'polio_sia_coordination_site') {
        value <- if (value %in% c('Yes', 'No')) value else df[[col_name]][row]
      }
      
      df[[col_name]][row] <- value
      rv$facility_sf <- facility_df_to_sf(df)
    }
    
    facilityMapServer(
      id = "map",
      district_sf = district_base,
      facility_data_r = facility_data,
      selected_id_r = selected_id,
      on_marker_drag = update_marker_position,
      on_add_facility = add_new_facility,
      adding_facility_r = adding_facility,
      show_buffer = TRUE,
      all_district_densities = all_district_densities
    )
    
    update_facility_data <- function(new_df) {
      req(!is.null(rv$facility_sf), nrow(rv$facility_sf) > 0)
      
      rv$facility_sf <- rv$facility_sf |>
        dplyr::left_join(
          new_df,
          by = "facility_id",
          suffix = c("", ".new")
        ) |>
        dplyr::mutate(
          facility_name = facility_name.new,
          facility_type = facility_type.new,
          operational = operational.new,
          ri_services = ri_services.new,
          polio_sia_coordination_site = polio_sia_coordination_site.new
        ) |>
        dplyr::select(
          -ends_with(".new")
        )
    }
    
    facilityTableServer(
      "table",
      facility_data_r = facility_data,
      selected_id_r = selected_id,
      on_data_change = update_facility_data
    )
    
    list(
      facility_data = facility_data,
      coordination_sites = coordination_sites
    )
  })
}