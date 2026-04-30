# =============================================================================
# mod_facility_tab.R
# =============================================================================

facilityTabUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 3,
      
      div(class = 'rightbar-title', 'SIA Coordination Sites'),
      
      div(
        style = paste0('background:#f0fdf4;border-left:3px solid #0d9488;',
                       'border-radius:0 6px 6px 0;padding:7px 10px;margin-bottom:8px;'),
        tags$p(
          style = 'font-size: 11px; font-weight: 600; color: #0f172a; margin: 0 0 2px;',
          'Goal: identify SIA coordination sites'
        ),
        tags$p(
          style = 'font-size: 11px; color: #475569; line-height: 1.5; margin: 0;',
          'The facilities where vaccination teams assemble, collect supplies, and report back. ',
          'One coordination site anchors one health area.'
        )
      ),
      
      tags$p(
        style = 'font-size: 11px; color: #475569; line-height: 1.6; margin-bottom: 5px;',
        'Review the listed facilities. Correct GPS by dragging pins on the map. ',
        'Tick the ', tags$strong('checkbox'), ' in the table to mark a facility as a coordination site.'
      ),
      
      tags$p(
        style = 'font-size: 11px; color: #64748b; line-height: 1.5; margin-bottom: 8px;',
        tags$strong('When selecting, consider: '),
        'location & catchment population; cold storage, electricity & assembly space; ',
        'availability of an SIA coordinator or supervisor.'
      ),
      
      uiOutput(ns('odk_status')),
      
      actionButton(
        ns('add_sia_site'),
        tagList(icon('map-pin'), 'Add Non-Facility Site'),
        width = '100%', class = 'btn-default btn-sm'
      ),
      tags$p(
        style = 'font-size: 11px; color: #94a3b8; margin: 4px 0 8px;',
        'Only if the coordination site is not listed as a health facility. ',
        'Click the button then click the map to place it.'
      ),
      
      checkboxInput(
        ns('show_pop_raster'),
        'Show WorldPop U5 Population',
        value = FALSE
      ),
      
      tags$hr(style = 'margin: 6px 0;'),
      
      actionButton(ns('submit_facilities'), 'Submit',
                   class = 'btn-primary btn-sm', width = '100%',
                   icon = icon('check-circle')),
      div(
        style = 'font-size: 11px; color: #64748b; margin-top: 4px;',
        'Saves coordination site selections to the database.'
      ),
      
      tags$hr(style = 'margin: 8px 0;'),
      
      actionButton(
        ns('continue_to_areas'),
        'Continue →',
        class = 'btn btn-default btn-sm',
        width = '100%',
        style = 'font-weight: 600;'
      )
    ),
    
    column(
      width = 6,
      div(style = 'height: calc(100vh - 120px);', facilityMapUI(ns('map')))
    ),
    
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

facilityTabServer <- function(
    id,
    zone, region, district, district_ready,
    active_tab,
    submitted_facilities,
    submit_stage_fn  = NULL,
    landmarks_r      = reactive(NULL),
    save_snapshot_fn = NULL,   # kept for compatibility, no-op
    restore_r        = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(
      odk_sf       = NULL,
      app_sf       = NULL,
      odk_loading  = FALSE,
      odk_error    = NULL
    )
    
    selected_id     <- reactiveVal(NULL)
    adding_facility <- reactiveVal(FALSE)
    editing_locked  <- reactiveVal(FALSE)
    needs_odk_fetch <- reactiveVal(FALSE)
    pending_restore <- reactiveVal(NULL)
    
    # -------------------------------------------------------------------------
    # Restore
    # -------------------------------------------------------------------------
    observeEvent(restore_r(), {
      snap <- restore_r()
      if (is.null(snap)) return()
      if (!is.null(snap$odk_sf) || !is.null(snap$app_sf)) {
        if (isTRUE(rv$odk_loading)) {
          pending_restore(snap)
        } else {
          if (!is.null(snap$odk_sf)) rv$odk_sf <- snap$odk_sf
          if (!is.null(snap$app_sf)) rv$app_sf  <- snap$app_sf
          showNotification('Facility state restored.', type = 'message', duration = 2)
        }
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
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
      
      result_sf <- district_sf |>
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
      result_sf <- tryCatch(sf::st_collection_extract(result_sf, 'POLYGON'),
                            error = function(e) result_sf)
      result_sf
    })
    
    # -------------------------------------------------------------------------
    # Combined facility data
    # -------------------------------------------------------------------------
    facility_data <- reactive({
      odk <- facility_sf_to_df(rv$odk_sf)
      app <- facility_sf_to_df(rv$app_sf)
      dplyr::bind_rows(odk, app)
    })
    
    combined_sf <- reactive({
      parts <- Filter(Negate(is.null), list(rv$odk_sf, rv$app_sf))
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
      
      n_selected <- sum(df$polio_sia_coordination_site == "Yes", na.rm = TRUE)
      
      district_pop <- districts_shp |>
        sf::st_drop_geometry() |>
        dplyr::filter(district_name == district()) |>
        dplyr::pull(u5_pop_2025) |>
        sum(na.rm = TRUE)
      
      n_recommended <- max(1L, ceiling(district_pop / 2000))
      count_color   <- if (n_selected >= n_recommended) "#388e3c" else "#e53935"
      
      div(
        style = 'margin-bottom: 10px; padding: 8px;
                 border: 1px solid #e0e0e0; border-radius: 4px;
                 background: #fafafa;',
        div(
          style = 'font-weight: 600; font-size: 12px; color: #555; margin-bottom: 6px;',
          'SIA Coordination Sites'
        ),
        div(
          style = 'display: flex; gap: 8px;',
          div(
            style = 'flex: 1; text-align: center; padding: 6px 4px;
                     border: 1px solid #e0e0e0; border-radius: 4px; background: white;',
            div(style = 'font-size: 10px; color: #888; margin-bottom: 2px;', 'Recommended'),
            div(style = 'font-size: 20px; font-weight: 700; color: #1565C0;', n_recommended)
          ),
          div(
            style = paste0(
              'flex: 1; text-align: center; padding: 6px 4px;',
              ' border: 1px solid #e0e0e0; border-radius: 4px; background: white;'
            ),
            div(style = 'font-size: 10px; color: #888; margin-bottom: 2px;', 'Selected'),
            div(
              style = paste0('font-size: 20px; font-weight: 700; color: ', count_color, ';'),
              n_selected
            )
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
            icon('spinner', class = 'fa-spin'), ' Loading facilities...')
      } else if (!is.null(rv$odk_error)) {
        div(style = 'color: #e53935; font-size: 12px; margin-bottom: 4px;',
            icon('circle-exclamation'), ' ', rv$odk_error)
      }
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
          rv$odk_sf <- if (!is.null(previous_odk) && nrow(previous_odk) > 0) {
            merge_odk_with_app_edits(fresh, previous_odk)
          } else {
            fresh
          }
        }
        
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
      
      snap <- pending_restore()
      if (!is.null(snap)) {
        pending_restore(NULL)
        if (!is.null(snap$odk_sf)) rv$odk_sf <- snap$odk_sf
        if (!is.null(snap$app_sf)) rv$app_sf  <- snap$app_sf
        showNotification('Facility state restored.', type = 'message', duration = 2)
      }
    }
    
    # -------------------------------------------------------------------------
    # Deferred ODK fetch — only when facility tab is active
    # -------------------------------------------------------------------------
    observeEvent(district(), {
      req(isTRUE(district_ready()))
      rv$app_sf <- NULL
      editing_locked(FALSE)
      needs_odk_fetch(TRUE)
    }, ignoreInit = FALSE)
    
    observe({
      req(isTRUE(needs_odk_fetch()))
      req(identical(active_tab(), "tab_health_facility_mapping"))
      needs_odk_fetch(FALSE)
      do_odk_fetch(preserve_edits = FALSE)
    })
    
    # -------------------------------------------------------------------------
    # Add SIA coordination site
    # -------------------------------------------------------------------------
    observeEvent(input$add_sia_site, {
      adding_facility(TRUE)
      showNotification(
        'Click on the map to place the new SIA coordination site.',
        type = 'message', duration = 4
      )
    })
    
    add_new_sia_site <- function(lat, lon) {
      new_id <- paste0('app_', format(Sys.time(), '%Y%m%d%H%M%S'), '_', sample(1000:9999, 1))
      
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
      
      new_sf <- sf::st_as_sf(new_row, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
      rv$app_sf <- if (is.null(rv$app_sf)) new_sf else rbind(rv$app_sf, new_sf)
      
      selected_id(new_id)
      adding_facility(FALSE)
      showNotification('SIA coordination site added.', type = 'message', duration = 3)
    }
    
    # -------------------------------------------------------------------------
    # Coordinate update via marker drag
    # -------------------------------------------------------------------------
    update_marker_position <- function(facility_id, lat, lon) {
      new_geom <- sf::st_sfc(sf::st_point(c(as.numeric(lon), as.numeric(lat))), crs = 4326)[[1]]
      
      if (!is.null(rv$odk_sf)) {
        idx <- which(rv$odk_sf$facility_id == facility_id)
        if (length(idx) == 1) {
          rv$odk_sf$lon[idx]      <- as.numeric(lon)
          rv$odk_sf$lat[idx]      <- as.numeric(lat)
          rv$odk_sf$geometry[idx] <- new_geom
          return()
        }
      }
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
    # Table bulk update
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
            facility_name = dplyr::coalesce(facility_name.new, facility_name),
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
    district_sf_for_map <- reactive({
      req(identical(active_tab(), "tab_health_facility_mapping"))
      district_base()
    })
    
    facilityMapServer(
      id                     = "map",
      district_sf            = district_sf_for_map,
      facility_data_r        = facility_data,
      selected_id_r          = selected_id,
      on_marker_drag         = update_marker_position,
      on_add_facility        = add_new_sia_site,
      adding_facility_r      = adding_facility,
      show_buffer            = TRUE,
      all_district_densities = all_district_densities,
      show_pop_r             = reactive(isTRUE(input$show_pop_raster)),
      landmarks_r            = landmarks_r
    )
    
    facilityTableServer(
      "table",
      facility_data_r = facility_data,
      selected_id_r   = selected_id,
      on_data_change  = update_facility_data
    )
    
    # -------------------------------------------------------------------------
    # Continue button — validates SIA sites then navigates
    # -------------------------------------------------------------------------
    observeEvent(input$continue_to_areas, {
      df    <- facility_data()
      seeds <- df |> dplyr::filter(polio_sia_coordination_site == "Yes")
      if (nrow(seeds) == 0) {
        showNotification(
          "Please mark at least one SIA coordination site before continuing.",
          type = "warning", duration = 4
        )
        return()
      }
      session$sendCustomMessage('switch_tab', list(value = 'tab_health_area_mapping'))
    }, ignoreInit = TRUE)
    
    # -------------------------------------------------------------------------
    # Submit — passes coordination sites locally AND writes to DB
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
      
      if (!is.null(submit_stage_fn)) {
        submit_stage_fn('facilities', list(odk_sf = rv$odk_sf, app_sf = rv$app_sf))
      }
    })
    
    # -------------------------------------------------------------------------
    # Return
    # -------------------------------------------------------------------------
    list(
      facility_data      = facility_data,
      coordination_sites = coordination_sites,
      odk_sf_r           = reactive(rv$odk_sf),
      app_sf_r           = reactive(rv$app_sf),
      restore_from_snapshot = function(snap) {
        # Apply directly and cancel the queued ODK fetch so it cannot
        # overwrite the restored state. No pending mechanism needed.
        has_data <- (!is.null(snap$odk_sf) && nrow(snap$odk_sf) > 0) ||
          (!is.null(snap$app_sf)  && nrow(snap$app_sf)  > 0)
        
        if (has_data) {
          needs_odk_fetch(FALSE)
          if (!is.null(snap$odk_sf) && nrow(snap$odk_sf) > 0) rv$odk_sf <- snap$odk_sf
          if (!is.null(snap$app_sf)  && nrow(snap$app_sf)  > 0) rv$app_sf  <- snap$app_sf
          showNotification('Facility locations restored.', type = 'message', duration = 2)
        }
        # If no facility data in snap (stage not yet submitted), needs_odk_fetch
        # stays TRUE and the normal fetch runs when the tab is opened.
      }
    )
  })
}
