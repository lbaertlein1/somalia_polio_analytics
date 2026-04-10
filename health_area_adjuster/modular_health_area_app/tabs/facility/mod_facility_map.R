facilityMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = 'position: relative; height: 100%;',
      leaflet::leafletOutput(
        ns('map'),
        width = '100%',
        height = '100%'
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
    
    # --------------------------------------------------
    # Helpers
    # --------------------------------------------------
    
    make_facility_icon <- function(is_selected = FALSE) {
      icon_url <- if (isTRUE(is_selected)) {
        'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png'
      } else {
        'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-black.png'
      }
      
      leaflet::makeIcon(
        iconUrl = icon_url,
        shadowUrl = 'https://unpkg.com/leaflet@1.9.4/dist/images/marker-shadow.png',
        iconWidth = 25,
        iconHeight = 41,
        iconAnchorX = 12,
        iconAnchorY = 41
      )
    }
    
    add_one_marker <- function(proxy, row, selected_id) {
      is_selected <- !is.null(selected_id) &&
        nzchar(selected_id) &&
        identical(as.character(row$facility_id[[1]]), as.character(selected_id))
      
      proxy |>
        leaflet::addMarkers(
          lng = row$lon[[1]],
          lat = row$lat[[1]],
          layerId = row$facility_id[[1]],
          icon = make_facility_icon(is_selected),
          options = leaflet::markerOptions(
            draggable = TRUE,
            riseOnHover = TRUE
          ),
          label = row$facility_name[[1]],
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = 'right',
            offset = c(10, 0),
            textsize = '11px',
            className = 'hf-tooltip'
          )
        )
    }
    
    # --------------------------------------------------
    # Density -> allowed distance in meters
    # Least dense = 10 km
    # Most dense = 1 km
    # --------------------------------------------------
    
    get_allowed_distance_m <- function(
    district_density,
    all_densities
    ) {
      
      district_density <- suppressWarnings(as.numeric(district_density))
      if (length(district_density) == 0) {
        return(5000)
      }
      district_density <- district_density[1]
      
      all_densities <- suppressWarnings(as.numeric(all_densities))
      
      if (length(all_densities) == 0) {
        return(5000)
      }
      
      all_densities <- all_densities[
        is.finite(all_densities) &
          !is.na(all_densities) &
          all_densities > 0
      ]
      
      if (is.na(district_density) || !is.finite(district_density) || district_density <= 0) {
        return(5000)
      }
      
      if (length(all_densities) < 2) {
        return(5000)
      }
      
      q30 <- suppressWarnings(as.numeric(stats::quantile(all_densities, 0.30, na.rm = TRUE, type = 7)))
      q70 <- suppressWarnings(as.numeric(stats::quantile(all_densities, 0.70, na.rm = TRUE, type = 7)))
      
      print(q30)
      print(q70)
      
      if (length(q30) == 0 || length(q70) == 0 || is.na(q30) || is.na(q70) || !is.finite(q30) || !is.finite(q70)) {
        return(5000)
      }
      
      if (district_density >= q70) {
        return(1000)
      }
      
      if (district_density <= q30) {
        return(10000)
      }
      
      return(5000)
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
      
      density_vector <- all_district_densities
      if (is.null(density_vector)) {
        density_vector <- this_density
      }
      
      buffer_m <- get_allowed_distance_m(
        district_density = this_density,
        all_densities = density_vector
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
      req(!is.null(df))
      
      proxy <- leaflet::leafletProxy('map', session = session) |>
        leaflet::clearMarkers()
      
      if (nrow(df) == 0) {
        return()
      }
      
      selected_id <- selected_id_r()
      
      for (i in seq_len(nrow(df))) {
        row <- df[i, , drop = FALSE]
        proxy <- add_one_marker(proxy, row, selected_id)
      }
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