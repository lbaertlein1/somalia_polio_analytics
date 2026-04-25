# =============================================================================
# facility_tab.R
# Two facility sources:
#   rv$odk_sf  — pulled from ODK Central, refreshable
#   rv$app_sf  — app-added SIA coordination sites, persists across refreshes
# =============================================================================

facilityTabUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # ---- Left sidebar -------------------------------------------------------
    column(
      width = 2,
      
      div(class = 'rightbar-title', 'Health Facility Mapping'),
      p('Facilities are loaded from the MHFL for the selected district.'),
      tags$ul(
        tags$li('Drag a pin to correct its GPS location.'),
        tags$li('Rename a facility in the table.'),
        tags$li('Mark facilities as SIA Coordination Sites in the table.'),
        tags$li('Only Yes facilities are used in Health Area Mapping.')
      ),
      
      uiOutput(ns('odk_status')),
      tags$hr(),
      
      actionButton(ns('refresh_odk'), 'Refresh from ODK', icon = icon('rotate'), width = '100%'),
      div(style = 'margin-top: 6px; font-size: 12px; color: #666;',
          'Re-pulls latest MHFL data. Edits are preserved.'),
      tags$hr(),
      
      uiOutput(ns('add_mhfl_ui')),
      div(style = 'margin-top: 6px; font-size: 12px; color: #666;',
          'Opens MHFL survey in new tab. Click Refresh after submitting.'),
      tags$hr(),
      
      actionButton(
        ns('add_sia_site'), 'Add SIA Coordination Site',
        icon = icon('map-pin'), width = '100%', class = 'btn-warning'
      ),
      div(style = 'margin-top: 6px; font-size: 12px; color: #666;',
          'Click this button, then click the map to place a new site.'),
      tags$hr(),
      
      checkboxInput(
        ns('show_pop_raster'),
        'Show WorldPop U5 Population',
        value = FALSE
      ),
      tags$hr(),
      
      actionButton(ns('submit_facilities'), 'Submit Facility Locations',
                   class = 'btn-primary', width = '100%')
    ),
    
    # ---- Map ----------------------------------------------------------------
    column(
      width = 7,
      div(style = 'height: calc(100vh - 120px);', facilityMapUI(ns('map')))
    ),
    
    # ---- Right sidebar — table ----------------------------------------------
    column(
      width = 3,
      div(class = 'rightbar-title', 'Facilities'),
      uiOutput(ns('sia_counts_card')),
      div(
        style = 'overflow-y: auto; height: 75vh;',
        facilityTableUI(ns('table'))
      )
    )
  )
}


# =============================================================================

facilityTabServer <- function(id, zone, region, district, district_ready, submitted_facilities) {
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(
      odk_sf       = NULL,   # from ODK — refreshable
      app_sf       = NULL,   # app-added SIA sites — persists across refreshes
      odk_loading  = FALSE,
      odk_error    = NULL
    )
    
    selected_id    <- reactiveVal(NULL)
    adding_facility <- reactiveVal(FALSE)
    editing_locked  <- reactiveVal(FALSE)
    
    # -------------------------------------------------------------------------
    # District boundary
    # -------------------------------------------------------------------------
    district_base <- reactive({
      req(isTRUE(district_ready()))
      req(zone(), region(), district())
      
      district_sf <- districts_shp |>
        dplyr::filter(
          zone_name     == zone(),
          region_name   == region(),
          district_name == district()
        ) |>
        dplyr::select(
          admin_id, district_name, region_id, region_name,
          zone_id, zone_name, u5_pop_density_km2, geometry
        )
      
      req(nrow(district_sf) >= 1)
      
      district_sf |>
        dplyr::summarise(
          admin_id           = dplyr::first(admin_id),
          district_name      = dplyr::first(district_name),
          region_id          = dplyr::first(region_id),
          region_name        = dplyr::first(region_name),
          zone_id            = dplyr::first(zone_id),
          zone_name          = dplyr::first(zone_name),
          u5_pop_density_km2 = dplyr::first(u5_pop_density_km2),
          geometry           = sf::st_union(geometry),
          .groups            = 'drop'
        ) |>
        sf::st_as_sf() |>
        safe_make_valid()
    })
    
    # -------------------------------------------------------------------------
    # Combined facility data — ODK + app-added
    # -------------------------------------------------------------------------
    facility_data <- reactive({
      odk <- facility_sf_to_df(rv$odk_sf)
      app <- facility_sf_to_df(rv$app_sf)
      dplyr::bind_rows(odk, app)
    })
    
    combined_sf <- reactive({
      parts <- list(rv$odk_sf, rv$app_sf)
      parts <- Filter(Negate(is.null), parts)
      parts <- Filter(function(x) nrow(x) > 0, parts)
      if (length(parts) == 0) return(NULL)
      do.call(rbind, parts)
    })
    
    coordination_sites <- reactive({
      df <- facility_data()
      if (nrow(df) == 0) return(df)
      df |> dplyr::filter(polio_sia_coordination_site == 'Yes')
    })
    
    # -------------------------------------------------------------------------
    # SIA coordination site count card
    # -------------------------------------------------------------------------
    output$sia_counts_card <- renderUI({
      df <- facility_data()
      
      n_selected    <- sum(df$polio_sia_coordination_site == "Yes", na.rm = TRUE)
      
      # Recommended: ceiling(u5_pop_2025 / 2000), minimum 1
      district_pop <- districts_shp |>
        sf::st_drop_geometry() |>
        dplyr::filter(district_name == district()) |>
        dplyr::pull(u5_pop_2025) |>
        sum(na.rm = TRUE)
      
      n_recommended <- max(1L, ceiling(district_pop / 2000))
      
      count_color <- if (n_selected >= n_recommended) "#388e3c" else "#e53935"
      
      div(
        style = 'margin-bottom: 10px; padding: 8px;
                 border: 1px solid #e0e0e0; border-radius: 4px;
                 background: #fafafa;',
        div(
          style = 'font-weight: 600; font-size: 12px;
                   color: #555; margin-bottom: 6px;',
          'SIA Coordination Sites'
        ),
        div(
          style = 'display: flex; gap: 8px;',
          div(
            style = 'flex: 1; text-align: center; padding: 6px 4px;
                     border: 1px solid #e0e0e0; border-radius: 4px;
                     background: white;',
            div(style = 'font-size: 10px; color: #888; margin-bottom: 2px;',
                'Recommended'),
            div(style = 'font-size: 20px; font-weight: 700; color: #1565C0;',
                n_recommended)
          ),
          div(
            style = paste0(
              'flex: 1; text-align: center; padding: 6px 4px;',
              ' border: 1px solid #e0e0e0; border-radius: 4px;',
              ' background: white;'
            ),
            div(style = 'font-size: 10px; color: #888; margin-bottom: 2px;',
                'Selected'),
            div(style = paste0('font-size: 20px; font-weight: 700; color: ', count_color, ';'),
                n_selected)
          )
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # ODK status badge
    # -------------------------------------------------------------------------
    output$odk_status <- renderUI({
      if (isTRUE(rv$odk_loading)) {
        div(style = 'color: #2196F3; font-size: 12px; margin-bottom: 4px;',
            icon('spinner', class = 'fa-spin'), ' Loading facilities from ODK...')
      } else if (!is.null(rv$odk_error)) {
        div(style = 'color: #e53935; font-size: 12px; margin-bottom: 4px;',
            icon('circle-exclamation'), ' ', rv$odk_error)
      } else if (!is.null(rv$odk_sf)) {
        n_app <- if (!is.null(rv$app_sf)) nrow(rv$app_sf) else 0
        div(style = 'color: #388e3c; font-size: 12px; margin-bottom: 4px;',
            icon('circle-check'), ' ',
            nrow(rv$odk_sf), ' MHFL facilities',
            if (n_app > 0) paste0(' + ', n_app, ' app-added') else NULL
        )
      }
    })
    
    # -------------------------------------------------------------------------
    # Add to MHFL link
    # -------------------------------------------------------------------------
    output$add_mhfl_ui <- renderUI({
      req(zone())
      cfg <- odk_form_config(zone())
      url <- paste0(
        "https://emro.nafundi.com/#/projects/9/forms/",
        cfg$form_id,
        "/submissions/new"
      )
      tags$a(
        href   = url,
        target = "_blank",
        class  = "btn btn-default",
        style  = "width: 100%;",
        icon("plus"), " Add new facility to MHFL"
      )
    })
    
    # -------------------------------------------------------------------------
    # ODK fetch
    # -------------------------------------------------------------------------
    do_odk_fetch <- function(preserve_edits = FALSE) {
      req(zone(), district())
      
      previous_odk   <- if (isTRUE(preserve_edits)) rv$odk_sf else NULL
      rv$odk_loading <- TRUE
      rv$odk_error   <- NULL
      
      tryCatch({
        fresh <- fetch_facilities_odk(
          zone_name     = zone(),
          district_name = district()
        )
        
        if (is.null(fresh) || nrow(fresh) == 0) {
          rv$odk_error <- paste0('No MHFL records found for "', district(), '".')
          rv$odk_sf    <- NULL
        } else {
          rv$odk_sf <- if (!is.null(previous_odk)) {
            merge_odk_with_app_edits(fresh, previous_odk)
          } else {
            fresh
          }
        }
        
        # Set selected to first available facility
        all_ids <- c(
          if (!is.null(rv$odk_sf)) as.character(rv$odk_sf$facility_id) else NULL,
          if (!is.null(rv$app_sf)) as.character(rv$app_sf$facility_id) else NULL
        )
        selected_id(if (length(all_ids) > 0) all_ids[1] else NULL)
        
      }, error = function(e) {
        rv$odk_error <- paste0('ODK fetch failed: ', conditionMessage(e))
        cat('ODK fetch error:', conditionMessage(e), '\n')
      })
      
      rv$odk_loading <- FALSE
    }
    
    observeEvent(district(), {
      req(isTRUE(district_ready()))
      # Clear app-added sites when district changes — they belong to a district
      rv$app_sf <- NULL
      editing_locked(FALSE)
      do_odk_fetch(preserve_edits = FALSE)
    }, ignoreInit = FALSE)
    
    observeEvent(input$refresh_odk, {
      do_odk_fetch(preserve_edits = TRUE)
      showNotification('Facilities refreshed from ODK.', type = 'message', duration = 3)
    })
    
    # -------------------------------------------------------------------------
    # Add SIA coordination site — click-to-place on map
    # -------------------------------------------------------------------------
    observeEvent(input$add_sia_site, {
      adding_facility(TRUE)
      showNotification(
        'Click on the map to place the new SIA coordination site.',
        type = 'message', duration = 4
      )
    })
    
    add_new_sia_site <- function(lat, lon) {
      new_id <- paste0(
        'app_',
        format(Sys.time(), '%Y%m%d%H%M%S'),
        '_',
        sample(1000:9999, 1)
      )
      
      new_row <- data.frame(
        facility_id                 = new_id,
        facility_name               = paste('SIA Site', 
                                            if (!is.null(rv$app_sf)) nrow(rv$app_sf) + 1L else 1L),
        facility_type               = NA_character_,
        hf_ownership                = NA_character_,
        region                      = region(),
        district                    = district(),
        incharge_name               = NA_character_,
        lat                         = as.numeric(lat),
        lon                         = as.numeric(lon),
        polio_sia_coordination_site = "Yes",
        odk_edit_link               = NA_character_,
        stringsAsFactors            = FALSE
      )
      
      new_sf <- sf::st_as_sf(
        new_row,
        coords = c("lon", "lat"),
        crs    = 4326,
        remove = FALSE
      )
      
      rv$app_sf <- if (is.null(rv$app_sf)) {
        new_sf
      } else {
        rbind(rv$app_sf, new_sf)
      }
      
      selected_id(new_id)
      adding_facility(FALSE)
      
      showNotification('SIA coordination site added.', type = 'message', duration = 3)
    }
    
    # -------------------------------------------------------------------------
    # Coordinate update via marker drag — works on both ODK and app facilities
    # -------------------------------------------------------------------------
    update_marker_position <- function(facility_id, lat, lon) {
      new_geom <- sf::st_sfc(
        sf::st_point(c(as.numeric(lon), as.numeric(lat))),
        crs = 4326
      )[[1]]
      
      # Try ODK facilities first
      if (!is.null(rv$odk_sf)) {
        idx <- which(rv$odk_sf$facility_id == facility_id)
        if (length(idx) == 1) {
          rv$odk_sf$lon[idx]      <- as.numeric(lon)
          rv$odk_sf$lat[idx]      <- as.numeric(lat)
          rv$odk_sf$geometry[idx] <- new_geom
          return()
        }
      }
      # Then app facilities
      if (!is.null(rv$app_sf)) {
        idx <- which(rv$app_sf$facility_id == facility_id)
        if (length(idx) == 1) {
          rv$app_sf$lon[idx]      <- as.numeric(lon)
          rv$app_sf$lat[idx]      <- as.numeric(lat)
          rv$app_sf$geometry[idx] <- new_geom
        }
      }
    }
    
    # -------------------------------------------------------------------------
    # Table bulk update — routes edits to the correct source sf
    # -------------------------------------------------------------------------
    update_facility_data <- function(new_df) {
      edits <- new_df |>
        dplyr::select(facility_id, facility_name, polio_sia_coordination_site) |>
        dplyr::distinct(facility_id, .keep_all = TRUE)
      
      apply_edits <- function(sf_obj) {
        if (is.null(sf_obj) || nrow(sf_obj) == 0) return(sf_obj)
        sf_obj |>
          dplyr::left_join(edits, by = "facility_id", suffix = c("", ".new")) |>
          dplyr::mutate(
            facility_name               = dplyr::coalesce(facility_name.new, facility_name),
            polio_sia_coordination_site = dplyr::coalesce(
              polio_sia_coordination_site.new, polio_sia_coordination_site
            )
          ) |>
          dplyr::select(-dplyr::ends_with(".new"))
      }
      
      rv$odk_sf <- apply_edits(rv$odk_sf)
      rv$app_sf <- apply_edits(rv$app_sf)
    }
    
    # -------------------------------------------------------------------------
    # Sub-module wiring
    # -------------------------------------------------------------------------
    facilityMapServer(
      id                     = "map",
      district_sf            = district_base,
      facility_data_r        = facility_data,
      selected_id_r          = selected_id,
      on_marker_drag         = update_marker_position,
      on_add_facility        = add_new_sia_site,
      adding_facility_r      = adding_facility,
      show_buffer            = TRUE,
      all_district_densities = all_district_densities,
      show_pop_r             = reactive(isTRUE(input$show_pop_raster))
    )
    
    facilityTableServer(
      "table",
      facility_data_r = facility_data,
      selected_id_r   = selected_id,
      on_data_change  = update_facility_data
    )
    
    # -------------------------------------------------------------------------
    # Submit
    # -------------------------------------------------------------------------
    observeEvent(input$submit_facilities, {
      df    <- facility_data()
      seeds <- df |> dplyr::filter(polio_sia_coordination_site == "Yes")
      
      if (nrow(seeds) == 0) {
        showNotification(
          "At least one coordination site must be marked Yes before submitting.",
          type = "error", duration = 5
        )
        return()
      }
      
      submitted_facilities(seeds)
      editing_locked(TRUE)
      showNotification(
        "Facility locations submitted successfully.",
        type = "message", duration = 3
      )
    })
    
    list(
      facility_data      = facility_data,
      coordination_sites = coordination_sites
    )
  })
}
