facilityMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    leafletOutput(ns('facility_map'), width = '100%', height = '100%'),
    div(
      id = ns('loading_overlay'),
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
}

facilityMapServer <- function(id, district_sf, facility_data_r, selected_id_r, on_marker_drag) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    cat("facilityMapServer initialized\n")
    
    # ----------------------------
    # Create map
    # ----------------------------
    
    output$facility_map <- renderLeaflet({
      
      cat("renderLeaflet entered\n")
      
      req(district_sf())
      req(nrow(district_sf()) > 0)
      
      bbox <- sf::st_bbox(sf::st_transform(district_sf(), 4326))
      
      leaflet::leaflet(
        options = leaflet::leafletOptions(zoomSnap = 0.25)
      ) |>
        leaflet::addProviderTiles(
          leaflet::providers$OpenStreetMap
        ) |>
        leaflet::fitBounds(
          lng1 = bbox[['xmin']],
          lat1 = bbox[['ymin']],
          lng2 = bbox[['xmax']],
          lat2 = bbox[['ymax']]
        )
    })
    outputOptions(output, "facility_map", suspendWhenHidden = FALSE)
    
    # ----------------------------
    # Draw markers AFTER map exists
    # ----------------------------
    
    observe({
      
      df <- facility_data_r()
      
      cat(
        "facility marker observer rows:",
        if (is.null(df)) "NULL" else nrow(df),
        "\n"
      )
      
      req(df)
      req(nrow(df) > 0)
      
      proxy <- leaflet::leafletProxy(
        "facility_map",
        session = session
      ) |>
        leaflet::clearMarkers()
      
      selected_id <- selected_id_r()
      
      for (i in seq_len(nrow(df))) {
        
        row <- df[i, , drop = FALSE]
        
        cat(
          "adding marker:",
          row$facility_id,
          row$lon,
          row$lat,
          "\n"
        )
        
        is_selected <-
          !is.null(selected_id) &&
          nzchar(selected_id) &&
          identical(
            as.character(row$facility_id[[1]]),
            selected_id
          )
        
        icon_url <- if (is_selected) {
          "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png"
        } else if (identical(row$polio_sia_coordination_site[[1]], "Yes")) {
          "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png"
        } else {
          "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-grey.png"
        }
        
        marker_icon <- leaflet::makeIcon(
          iconUrl = icon_url,
          shadowUrl = "https://unpkg.com/leaflet@1.9.4/dist/images/marker-shadow.png",
          iconWidth = 25,
          iconHeight = 41,
          iconAnchorX = 12,
          iconAnchorY = 41
        )
        
        proxy <- proxy |>
          leaflet::addMarkers(
            lng = row$lon[[1]],
            lat = row$lat[[1]],
            layerId = row$facility_id[[1]],
            icon = marker_icon,
            options = leaflet::markerOptions(
              draggable = TRUE
            ),
            label = row$facility_name[[1]],
            labelOptions = leaflet::labelOptions(
              noHide = TRUE,
              direction = "right",
              offset = c(10, 0),
              className = "hf-tooltip"
            )
          )
      }
      
      cat("all facility markers added\n")
      
    })
    
    # ----------------------------
    # Drag handling
    # ----------------------------
    
    observeEvent(input$facility_map_marker_dragend, {
      
      info <- input$facility_map_marker_dragend
      
      req(info$id)
      
      on_marker_drag(
        facility_id = as.character(info$id),
        lat = as.numeric(info$lat),
        lon = as.numeric(info$lng)
      )
      
    })
    
  })
}
