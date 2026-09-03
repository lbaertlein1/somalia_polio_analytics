# =============================================================================
# mod_facility_tab.R  (v2)
#
# Only change from v1: IDP settlements added. Fetched automatically per
# district (same deferred-until-tab-active pattern as the existing ODK
# facility fetch), shown as a small read-only list in the sidebar, and
# submitted alongside facilities when the Submit button is clicked.
#
# Deliberately NOT done in this pass: rendering IDP points on the Leaflet
# map itself. That would mean editing mod_facility_map.R, which I haven't
# read this session — after getting burned once already this session by
# editing a shared file (bfs_propagate.cpp) without reading what else
# depended on it first, I'm not repeating that on a 24K-line Leaflet
# module blind. The sidebar list covers the "review" requirement; map
# markers are a reasonable follow-up once mod_facility_map.R is actually
# read and a similar point-layer pattern (it already draws landmarks/
# facilities as points, per paint-app.js's equivalent) can be matched
# properly instead of guessed at.
# =============================================================================

facilityTabUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 3,

      div(class = 'rightbar-title', 'Outreach Coordination Sites'),

      div(
        style = paste0('background:#f0fdf4;border-left:3px solid #0d9488;',
                       'border-radius:0 6px 6px 0;padding:7px 10px;margin-bottom:8px;'),
        tags$p(
          style = 'font-size: 11px; font-weight: 600; color: #0f172a; margin: 0 0 2px;',
          'Goal: identify outreach coordination sites'
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
        'availability of an outreach coordinator or supervisor.'
      ),

      uiOutput(ns('odk_status')),

      actionButton(
        ns('add_outreach_site'),
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

      div(class = 'rightbar-title', style = 'font-size: 12px;', 'IDP Settlements'),
      uiOutput(ns('idp_status')),
      uiOutput(ns('idp_list')),

      tags$hr(style = 'margin: 6px 0;'),

      actionButton(ns('submit_facilities'), 'Submit',
                   class = 'btn-primary btn-sm', width = '100%',
                   icon = icon('check-circle')),
      div(
        style = 'font-size: 11px; color: #64748b; margin-top: 4px;',
        'Saves coordination site selections and IDP settlement data to the database.'
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
      uiOutput(ns('outreach_counts_card')),
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
    submit_stage_fn    = NULL,
    landmarks_r        = reactive(NULL),
    subdivisions_r     = reactive(NULL),
    planning_area_sf_r = reactive(NULL),
    save_snapshot_fn   = NULL,   # kept for compatibility, no-op
    restore_r          = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues(
      odk_sf       = NULL,
      app_sf       = NULL,
      odk_loading  = FALSE,
      odk_error    = NULL,
      idp_sf       = NULL,
      idp_loading  = FALSE,
      idp_error    = NULL
    )

    selected_id     <- reactiveVal(NULL)
    adding_facility <- reactiveVal(FALSE)
    editing_locked  <- reactiveVal(FALSE)
    needs_odk_fetch <- reactiveVal(FALSE)
    needs_idp_fetch <- reactiveVal(FALSE)
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
      if (!is.null(snap$idp_settlements)) {
        needs_idp_fetch(FALSE)
        rv$idp_sf <- snap$idp_settlements
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # -------------------------------------------------------------------------
    # District boundary
    # -------------------------------------------------------------------------
    district_base <- reactive({
      req(isTRUE(district_ready()))
      # Use planning area (whole district in v2 — no more urban/rural split)
      pa <- tryCatch(planning_area_sf_r(), error = function(e) NULL)
      if (!is.null(pa) && nrow(pa) > 0) {
        pa <- sf::st_transform(pa, 3857)
        pa <- tryCatch(sf::st_collection_extract(pa, 'POLYGON'), error = function(e) pa)
        density <- tryCatch({
          districts_shp |>
            sf::st_drop_geometry() |>
            dplyr::filter(district_name == district()) |>
            dplyr::pull(u5_pop_density_km2) |>
            (\(x) x[1])()
        }, error = function(e) NA_real_)
        pa$u5_pop_density_km2 <- density
        pa$district_name      <- district()
        return(pa)
      }
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
    # Outreach coordination site count card
    # -------------------------------------------------------------------------
    planning_area_pop <- reactive({
      pa <- tryCatch(planning_area_sf_r(), error = function(e) NULL)
      if (is.null(pa)) pa <- tryCatch(district_base(), error = function(e) NULL)$district_sf

      if (!is.null(u5_rast) && !is.null(pa)) {
        pop <- tryCatch({
          pa_proj <- sf::st_transform(pa, sf::st_crs(terra::crs(u5_rast)))
          val <- exactextractr::exact_extract(
            raster::raster(u5_rast), pa_proj, fun = "sum"
          )
          round(sum(val, na.rm = TRUE))
        }, error = function(e) NULL)
        if (!is.null(pop) && pop > 0) return(pop)
      }

      districts_shp |>
        sf::st_drop_geometry() |>
        dplyr::filter(district_name == district()) |>
        dplyr::pull(WP_U5) |>
        sum(na.rm = TRUE)
    })

    output$outreach_counts_card <- renderUI({
      df <- facility_data()

      n_selected   <- sum(df$polio_sia_coordination_site == "Yes", na.rm = TRUE)
      planning_pop <- planning_area_pop()
      n_recommended <- max(1L, ceiling(planning_pop / 2000))
      count_color   <- if (n_selected >= n_recommended) "#388e3c" else "#e53935"

      div(
        style = 'margin-bottom: 10px; padding: 8px;
                 border: 1px solid #e0e0e0; border-radius: 4px;
                 background: #fafafa;',
        div(
          style = 'font-weight: 600; font-size: 12px; color: #555; margin-bottom: 6px;',
          'Outreach Coordination Sites'
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
    # IDP status + list
    # -------------------------------------------------------------------------
    output$idp_status <- renderUI({
      if (isTRUE(rv$idp_loading)) {
        div(style = 'color: #2196F3; font-size: 11px; margin-bottom: 4px;',
            icon('spinner', class = 'fa-spin'), ' Loading IDP settlements...')
      } else if (!is.null(rv$idp_error)) {
        div(style = 'color: #94a3b8; font-size: 11px; margin-bottom: 4px;', rv$idp_error)
      } else if (!is.null(rv$idp_sf) && nrow(rv$idp_sf) > 0) {
        div(style = 'color: #0d9488; font-size: 11px; margin-bottom: 4px;',
            sprintf('%d settlement(s) found.', nrow(rv$idp_sf)))
      }
    })

    output$idp_list <- renderUI({
      req(!is.null(rv$idp_sf), nrow(rv$idp_sf) > 0)
      df <- sf::st_drop_geometry(rv$idp_sf)
      tagList(
        div(
          style = 'max-height: 160px; overflow-y: auto; border: 1px solid #e2e8f0; border-radius: 4px; margin-bottom: 6px;',
          lapply(seq_len(nrow(df)), function(i) {
            div(
              style = 'padding: 4px 8px; font-size: 11px; border-bottom: 1px solid #f1f5f9;',
              tags$strong(df$idp_name[i]),
              if (!is.na(df$idp_population[i]))
                tags$span(style = 'color:#64748b;', sprintf('  \u00b7  %s people', format(round(df$idp_population[i]), big.mark = ',')))
            )
          })
        )
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

        if (!is.null(fresh) && nrow(fresh) > 0) {
          pa <- tryCatch(planning_area_sf_r(), error = function(e) NULL)
          if (!is.null(pa) && nrow(pa) > 0) {
            fresh_pts <- sf::st_as_sf(
              fresh[!is.na(fresh$lat) & !is.na(fresh$lon), , drop = FALSE],
              coords = c("lon", "lat"), crs = 4326, remove = FALSE
            ) |> sf::st_transform(sf::st_crs(pa))
            inside <- lengths(sf::st_within(fresh_pts,
                                            sf::st_union(sf::st_make_valid(pa)))) > 0
            fresh <- fresh[!is.na(fresh$lat) & !is.na(fresh$lon), , drop = FALSE][inside, , drop = FALSE]
            cat("[facilityTab] planning area filter: kept", nrow(fresh),
                "of", nrow(fresh_pts), "facilities\n")
          }
        }

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
    # IDP fetch — same deferred-until-active pattern as the ODK fetch above
    # -------------------------------------------------------------------------
    do_idp_fetch <- function() {
      req(district())
      db <- tryCatch(district_base(), error = function(e) NULL)
      req(!is.null(db))

      rv$idp_loading <- TRUE
      rv$idp_error   <- NULL

      tryCatch({
        fresh <- fetch_idp_settlements_for_district(db)
        if (is.null(fresh) || nrow(fresh) == 0) {
          rv$idp_error <- 'No IDP settlements found (or no source configured).'
          rv$idp_sf    <- NULL
        } else {
          rv$idp_sf <- fresh
        }
      }, error = function(e) {
        rv$idp_error <- paste0('IDP fetch failed: ', conditionMessage(e))
        cat('IDP fetch error:', conditionMessage(e), '\n')
      })

      rv$idp_loading <- FALSE
    }

    # -------------------------------------------------------------------------
    # Deferred fetches — only when facility tab is active
    # -------------------------------------------------------------------------
    observeEvent(district(), {
      req(isTRUE(district_ready()))
      rv$app_sf <- NULL
      editing_locked(FALSE)
      needs_odk_fetch(TRUE)
      needs_idp_fetch(TRUE)
    }, ignoreInit = FALSE)

    observeEvent(planning_area_sf_r(), {
      req(isTRUE(district_ready()))
      req(nzchar(district() %||% ''))
      needs_odk_fetch(TRUE)
    }, ignoreInit = TRUE)

    observe({
      req(isTRUE(needs_odk_fetch()))
      req(identical(active_tab(), "tab_health_facility_mapping"))
      needs_odk_fetch(FALSE)
      do_odk_fetch(preserve_edits = FALSE)
    })

    observe({
      req(isTRUE(needs_idp_fetch()))
      req(identical(active_tab(), "tab_health_facility_mapping"))
      needs_idp_fetch(FALSE)
      do_idp_fetch()
    })

    # -------------------------------------------------------------------------
    # Add outreach coordination site
    # -------------------------------------------------------------------------
    observeEvent(input$add_outreach_site, {
      adding_facility(TRUE)
      showNotification(
        'Click on the map to place the new outreach coordination site.',
        type = 'message', duration = 4
      )
    })

    add_new_outreach_site <- function(lat, lon) {
      new_id <- paste0('app_', format(Sys.time(), '%Y%m%d%H%M%S'), '_', sample(1000:9999, 1))

      new_row <- data.frame(
        facility_id                 = new_id,
        facility_name               = paste('Outreach Site',
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
      editing_locked(FALSE)
      showNotification('Outreach coordination site added.', type = 'message', duration = 3)
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
      editing_locked(FALSE)
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
      editing_locked(FALSE)
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
      on_add_facility        = add_new_outreach_site,
      adding_facility_r      = adding_facility,
      show_buffer            = TRUE,
      all_district_densities = all_district_densities,
      show_pop_r             = reactive(isTRUE(input$show_pop_raster)),
      landmarks_r            = landmarks_r,
      subdivisions_r         = subdivisions_r
    )

    facilityTableServer(
      "table",
      facility_data_r = facility_data,
      selected_id_r   = selected_id,
      on_data_change  = update_facility_data
    )

    # -------------------------------------------------------------------------
    # Continue button — warns if unsubmitted edits exist
    # -------------------------------------------------------------------------
    .do_continue_to_areas <- function() {
      session$sendCustomMessage('switch_tab', list(value = 'tab_health_area_mapping'))
    }

    .submit_all <- function() {
      df    <- facility_data()
      seeds <- df |> dplyr::filter(polio_sia_coordination_site == "Yes")
      submitted_facilities(seeds)
      editing_locked(TRUE)
      if (!is.null(submit_stage_fn)) {
        submit_stage_fn('facilities', list(odk_sf = rv$odk_sf, app_sf = rv$app_sf))
        if (!is.null(rv$idp_sf) && nrow(rv$idp_sf) > 0)
          submit_stage_fn('idp', list(idp_settlements = sf::st_drop_geometry(
            dplyr::mutate(rv$idp_sf, lon = sf::st_coordinates(rv$idp_sf)[, 1],
                          lat = sf::st_coordinates(rv$idp_sf)[, 2])
          )))
      }
    }

    .do_submit_and_continue_facilities <- function() {
      .submit_all()
      .do_continue_to_areas()
    }

    observeEvent(input$continue_to_areas, {
      df    <- facility_data()
      seeds <- df |> dplyr::filter(polio_sia_coordination_site == "Yes")
      if (nrow(seeds) == 0) {
        showNotification(
          "Please mark at least one outreach coordination site before continuing.",
          type = "warning", duration = 4
        )
        return()
      }
      if (!isTRUE(editing_locked())) {
        showModal(modalDialog(
          title     = 'Unsaved facility edits',
          size      = 's', easyClose = FALSE, footer = NULL,
          div(style = 'font-size:13px;color:#475569;margin-bottom:16px;',
              'You have unsaved facility selections. Submit them now to keep your work, or continue without saving.'),
          div(style = 'display:flex;gap:8px;justify-content:flex-end;flex-wrap:wrap;',
              actionButton(session$ns('fac_continue_cancel'),        'Cancel',               class = 'btn btn-default'),
              actionButton(session$ns('fac_continue_without_save'),  'Continue without saving', class = 'btn btn-default'),
              actionButton(session$ns('fac_submit_and_continue'),    'Submit & Continue',    class = 'btn btn-primary',
                           style = 'font-weight:600;')
          )
        ))
        return()
      }
      .do_continue_to_areas()
    }, ignoreInit = TRUE)

    observeEvent(input$fac_continue_cancel, {
      removeModal()
    }, ignoreInit = TRUE)

    observeEvent(input$fac_continue_without_save, {
      removeModal()
      .do_continue_to_areas()
    }, ignoreInit = TRUE)

    observeEvent(input$fac_submit_and_continue, {
      removeModal()
      .do_submit_and_continue_facilities()
    }, ignoreInit = TRUE)

    # -------------------------------------------------------------------------
    # Submit — passes coordination sites locally AND writes to DB (both
    # facilities and IDP settlements)
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

      .submit_all()
    })

    # -------------------------------------------------------------------------
    # Return
    # -------------------------------------------------------------------------
    list(
      facility_data      = facility_data,
      coordination_sites = coordination_sites,
      odk_sf_r           = reactive(rv$odk_sf),
      app_sf_r           = reactive(rv$app_sf),
      idp_sf_r           = reactive(rv$idp_sf),
      restore_from_snapshot = function(snap) {
        has_data <- (!is.null(snap$odk_sf) && nrow(snap$odk_sf) > 0) ||
          (!is.null(snap$app_sf)  && nrow(snap$app_sf)  > 0)

        if (has_data) {
          needs_odk_fetch(FALSE)
          if (!is.null(snap$odk_sf) && nrow(snap$odk_sf) > 0) rv$odk_sf <- snap$odk_sf
          if (!is.null(snap$app_sf)  && nrow(snap$app_sf)  > 0) rv$app_sf  <- snap$app_sf
          showNotification('Facility locations restored.', type = 'message', duration = 2)
        }
        if (!is.null(snap$idp_settlements) && nrow(snap$idp_settlements) > 0) {
          needs_idp_fetch(FALSE)
          rv$idp_sf <- sf::st_as_sf(snap$idp_settlements, coords = c('lon', 'lat'), crs = 4326, remove = FALSE)
        }
      }
    )
  })
}
