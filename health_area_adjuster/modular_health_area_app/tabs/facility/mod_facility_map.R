facilityMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = 'position: relative; height: 100%;',
      leafletOutput(ns('map'), width = '100%', height = '100%')
    )
  )
}

facilityMapServer <- function(id, district_sf, facility_data_r, selected_id_r, on_marker_drag) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$map <- renderLeaflet({
      req(district_sf())
      req(nrow(district_sf()) > 0)
      
      bbox <- sf::st_bbox(sf::st_transform(district_sf(), 4326))
      
      leaflet::leaflet(
        options = leaflet::leafletOptions(zoomSnap = 0.25)
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
        
        is_selected <- !is.null(selected_id) &&
          nzchar(selected_id) &&
          identical(as.character(row$facility_id[[1]]), selected_id)
        
        icon_url <- if (is_selected) {
          'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png'
        } else {
          'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-black.png'
        }
        
        marker_icon <- leaflet::makeIcon(
          iconUrl = icon_url,
          shadowUrl = 'https://unpkg.com/leaflet@1.9.4/dist/images/marker-shadow.png',
          iconWidth = 25,
          iconHeight = 41,
          iconAnchorX = 12,
          iconAnchorY = 41,
          shadowWidth = 41,
          shadowHeight = 41,
          shadowAnchorX = 12,
          shadowAnchorY = 41
        )
        
        proxy <- proxy |>
          leaflet::addMarkers(
            lng = row$lon[[1]],
            lat = row$lat[[1]],
            layerId = row$facility_id[[1]],
            icon = marker_icon,
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
    })
    
    observeEvent(input$map_marker_dragend, {
      info <- input$map_marker_dragend
      req(!is.null(info$id), !is.null(info$lat), !is.null(info$lng))
      
      on_marker_drag(
        facility_id = as.character(info$id),
        lat = as.numeric(info$lat),
        lon = as.numeric(info$lng)
      )
    })
    
    observeEvent(input$map_marker_click, {
      info <- input$map_marker_click
      req(!is.null(info$id))
      selected_id_r(as.character(info$id))
    })
  })
}