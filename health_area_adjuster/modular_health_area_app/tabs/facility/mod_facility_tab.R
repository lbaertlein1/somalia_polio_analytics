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
      class = 'app-row',
      div(
        id = ns('leftbar'),
        class = 'app-leftbar',
        div(
          class = 'rightbar-title',
          'Health Facility Mapping'
        ),
        p('Review the preset health facility points for the selected district.'),
        tags$ul(
          tags$li('Drag each point to the correct location if needed.'),
          tags$li('Edit the facility attributes in the table.'),
          tags$li('Only facilities marked Yes for Polio SIA Coordination Site will be used in the Health Area Mapping tab.')
        )
      ),
      div(
        id = ns('mapwrap'),
        class = 'app-mapwrap',
        facilityMapUI(ns('map'))
      ),
      div(
        id = ns('rightbar'),
        class = 'app-rightbar',
        facilityTableUI(ns('table'))
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
      cat('starter facility lon range:',
          min(rv$facility_sf$lon), max(rv$facility_sf$lon), '\n')
      cat('starter facility lat range:',
          min(rv$facility_sf$lat), max(rv$facility_sf$lat), '\n')
      
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
      'map',
      district_sf = district_base,
      facility_data_r = facility_data,
      selected_id_r = selected_id,
      on_marker_drag = update_marker_position
    )
    
    facilityTableServer(
      'table',
      facility_data_r = facility_data,
      selected_id_r = selected_id,
      on_table_edit = update_table_value
    )
    
    list(
      facility_data = facility_data,
      coordination_sites = coordination_sites
    )
  })
}