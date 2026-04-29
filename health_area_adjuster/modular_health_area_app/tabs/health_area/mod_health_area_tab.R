healthAreaTabUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 2, healthAreaControlsUI(ns('controls'))),
    column(width = 7, div(style = 'height: calc(100vh - 120px);', healthAreaMapUI(ns('map')))),
    column(width = 3,
           div(style = 'overflow-y: auto; height: calc(100vh - 120px);',
               healthAreaPopulationUI(ns('population'))))
  )
}

healthAreaTabServer <- function(
    id,
    zone, region, district, district_ready,
    active_tab,
    facility_data,
    coordination_sites  = NULL,
    all_facilities_r    = reactive(NULL),
    landmarks_r         = reactive(NULL),
    submit_stage_fn     = NULL,
    save_snapshot_fn    = NULL,
    restore_r           = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    
    controls      <- healthAreaControlsServer("controls")
    map_mod       <- healthAreaMapServer("map")
    active_dfa_rv <- reactiveVal(starter_dfa_names[1])
    restore_just_applied     <- reactiveVal(FALSE)
    pending_action           <- reactiveVal(NULL)
    pending_restore          <- reactiveVal(NULL)
    areas_regenerated_counter <- reactiveVal(0L)
    changed_areas_rv          <- reactiveVal(character(0))
    
    normalize_dfa_names <- function(x) {
      x <- unique(as.character(x)); x <- x[!is.na(x) & nzchar(x)]
      c(setdiff(x, extra_dfa_names), extra_dfa_names)
    }
    
    # Returns names of health areas whose geometry changed between two saved sf objects.
    .find_changed_areas <- function(old_sf, new_sf) {
      if (is.null(old_sf) || nrow(old_sf) == 0 ||
          is.null(new_sf) || nrow(new_sf) == 0) return(character(0))
      changed <- character(0)
      for (area_name in setdiff(unique(new_sf$dfa_name), extra_dfa_names)) {
        old_geom <- old_sf[old_sf$dfa_name == area_name, ]
        new_geom <- new_sf[new_sf$dfa_name == area_name, ]
        if (nrow(old_geom) == 0) next   # new area — no existing planning data
        equal <- tryCatch(
          isTRUE(sf::st_equals(sf::st_geometry(old_geom),
                               sf::st_geometry(new_geom))[[1]]),
          error = function(e) FALSE
        )
        if (!equal) changed <- c(changed, area_name)
      }
      changed
    }
    
    rv <- reactiveValues(
      district_sf = NULL, district_base_sf = NULL,
      grid_sf = NULL, initial_assignments = NULL, current_assignments = NULL,
      saved_dfa_sf = NULL, neighbors_list = NULL, edge_list = NULL,
      pop_overlay_sf = NULL, friction_overlay_sf = NULL, pop_table = NULL,
      max_dim_m = NULL, grid_limits = NULL, brush_limits = NULL,
      seed_points = NULL, dfa_names = all_dfa_names, friction_path = NULL
    )
    
    observeEvent(controls$help_click(), { show_help_modal(session) })
    
    tab_active <- reactive({ identical(active_tab(), "tab_health_area_mapping") })
    
    current_fill_colors <- reactive({
      make_fill_colors(active_dfa = active_dfa_rv(), dfa_names = rv$dfa_names)
    })
    
    healthAreaPopulationServer(
      "population",
      active_dfa_rv        = active_dfa_rv,
      show_pop_raster      = controls$show_pop_raster,
      show_friction_raster = controls$show_friction_raster,
      pop_table            = reactive(rv$pop_table)
    )
    
    u5_worldpop_rv <- reactiveVal(NULL)
    get_u5_worldpop <- function() {
      if (is.null(u5_worldpop_rv()))
        u5_worldpop_rv(load_worldpop_u5_raster(t_u1_1to4_file = worldpop_t_u1_1to4_file))
      u5_worldpop_rv()
    }
    
    send_paint_message <- function(type, payload = list()) {
      session$sendCustomMessage(type, c(list(
        mapId              = map_mod$map_id,
        loadingOverlayId   = map_mod$loading_overlay_id,
        readyInputId       = map_mod$ready_input_id,
        assignmentsInputId = map_mod$assignments_input_id
      ), payload))
    }
    
    recompute_population_table <- function(assignments) {
      req(!is.null(rv$grid_sf), length(assignments) == nrow(rv$grid_sf))
      req("u5_pop" %in% names(rv$grid_sf))
      df <- data.frame(area_name = assignments, est_u5_pop = rv$grid_sf$u5_pop,
                       stringsAsFactors = FALSE) |>
        dplyr::group_by(area_name) |>
        dplyr::summarise(est_u5_pop = round(sum(est_u5_pop, na.rm = TRUE), 0), .groups = "drop")
      current_names   <- rv$dfa_names %||% all_dfa_names
      missing_classes <- setdiff(current_names, df$area_name)
      if (length(missing_classes) > 0)
        df <- dplyr::bind_rows(df, data.frame(area_name = missing_classes, est_u5_pop = 0,
                                              stringsAsFactors = FALSE))
      df <- df |>
        dplyr::mutate(area_name = factor(area_name, levels = current_names)) |>
        dplyr::arrange(area_name) |>
        dplyr::mutate(area_name = as.character(area_name))
      rv$pop_table <- dplyr::bind_rows(
        df,
        data.frame(area_name = "District Total",
                   est_u5_pop = round(sum(rv$grid_sf$u5_pop, na.rm = TRUE), 0),
                   stringsAsFactors = FALSE)
      )
      invisible(NULL)
    }
    
    district_base <- reactive({
      req(isTRUE(district_ready())); req(zone(), region(), district())
      dsf <- districts_shp |>
        dplyr::filter(zone_name == zone(), region_name == region(), district_name == district()) |>
        dplyr::select(admin_id, district_name, region_id, region_name, zone_id, zone_name, geometry)
      req(nrow(dsf) >= 1)
      dsf <- dsf |>
        dplyr::summarise(
          admin_id = dplyr::first(admin_id), district_name = dplyr::first(district_name),
          region_id = dplyr::first(region_id), region_name = dplyr::first(region_name),
          zone_id = dplyr::first(zone_id), zone_name = dplyr::first(zone_name),
          geometry = sf::st_union(geometry), .groups = "drop"
        ) |> sf::st_as_sf()
      dsf       <- safe_make_valid(dsf)
      max_dim_m <- calc_district_max_dim(dsf)
      list(district_sf = dsf, max_dim_m = max_dim_m,
           grid_limits = calc_grid_limits(max_dim_m), brush_limits = calc_brush_limits(max_dim_m))
    })
    
    observeEvent(district_base(), {
      db <- district_base()
      rv$district_base_sf <- db$district_sf
      rv$grid_limits       <- db$grid_limits
      rv$brush_limits      <- db$brush_limits
      controls$set_brush_limits(db$brush_limits)
    }, ignoreInit = TRUE)
    
    observeEvent(controls$brush_minus_click(), {
      bl <- rv$brush_limits; req(!is.null(bl), !is.null(controls$brush_m()))
      updateSliderInput(session, "controls-brush_m_ui",
                        value = clamp_num(controls$brush_m() - bl$step, bl$min, bl$max))
    })
    observeEvent(controls$brush_plus_click(), {
      bl <- rv$brush_limits; req(!is.null(bl), !is.null(controls$brush_m()))
      updateSliderInput(session, "controls-brush_m_ui",
                        value = clamp_num(controls$brush_m() + bl$step, bl$min, bl$max))
    })
    observeEvent(controls$brush_m(), {
      req(tab_active(), isTRUE(district_ready()))
      send_paint_message("paint_set_brush", list(value = controls$brush_m()))
    }, ignoreInit = TRUE)
    observeEvent(controls$boundary_only(), {
      req(tab_active(), isTRUE(district_ready()))
      send_paint_message("paint_set_boundary_only", list(value = controls$boundary_only()))
    }, ignoreInit = TRUE)
    observeEvent(controls$show_pop_raster(), {
      req(tab_active(), isTRUE(district_ready()))
      if (isTRUE(controls$show_pop_raster()) && is.null(rv$pop_overlay_sf) && !is.null(rv$district_sf)) {
        rv$pop_overlay_sf <- tryCatch(
          make_population_overlay_sf(district_sf = rv$district_sf, u5_rast = get_u5_worldpop()),
          error = function(e) NULL)
      }
      geojson <- if (!is.null(rv$pop_overlay_sf)) as_geojson_text(rv$pop_overlay_sf) else NULL
      send_paint_message("paint_toggle_population",
                         list(show = controls$show_pop_raster(), geojson = geojson))
    }, ignoreInit = TRUE)
    observeEvent(controls$show_friction_raster(), {
      req(tab_active(), isTRUE(district_ready()))
      if (isTRUE(controls$show_friction_raster()) && is.null(rv$friction_overlay_sf) &&
          !is.null(rv$friction_path) && file.exists(rv$friction_path)) {
        rv$friction_overlay_sf <- tryCatch(
          make_friction_overlay_sf(district_sf = rv$district_sf,
                                   friction_rast = terra::rast(rv$friction_path)),
          error = function(e) NULL)
      }
      geojson <- if (!is.null(rv$friction_overlay_sf)) as_geojson_text(rv$friction_overlay_sf) else NULL
      send_paint_message("paint_toggle_friction",
                         list(show = controls$show_friction_raster(), geojson = geojson))
    }, ignoreInit = TRUE)
    
    facility_seed_sf <- reactive({
      if (is.null(facility_data)) return(NULL)
      df <- facility_data(); if (is.null(df) || nrow(df) == 0) return(NULL)
      keep <- rep(TRUE, nrow(df))
      if ("polio_sia_coordination_site" %in% names(df))
        keep <- keep & as.character(df$polio_sia_coordination_site) == "Yes"
      if ("operational" %in% names(df))
        keep <- keep & as.character(df$operational) == "Operational"
      df <- df[keep, , drop = FALSE]; if (nrow(df) == 0) return(NULL)
      req(all(c("lon", "lat") %in% names(df)))
      sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    })
    
    initial_scene <- initialHealthAreaGenerationServer(
      "initial_scene",
      district_sf       = reactive({ req(district_base()); district_base()$district_sf }),
      grid_n            = reactive({ req(district_base()); district_base()$grid_limits$value }),
      n_dfa             = n_start_dfas,
      seed              = reactive({ req(district()); sum(utf8ToInt(district())) })(),
      facility_seed_sf  = facility_seed_sf,
      facility_name_col = "facility_name"
    )
    
    # Deferred: only computes when health area tab is active
    selected_scene <- reactive({
      req(isTRUE(tab_active()))
      req(isTRUE(district_ready())); req(zone(), region(), district())
      sc      <- initial_scene$scene()
      seed_df <- initial_scene$seed_points_df()
      
      dynamic_dfa_names <- if (!is.null(seed_df) && nrow(seed_df) > 0 && "dfa_name" %in% names(seed_df)) {
        normalize_dfa_names(c(as.character(seed_df$dfa_name), extra_dfa_names))
      } else {
        normalize_dfa_names(c(unique(sc$initial_assignments), extra_dfa_names))
      }
      
      pop_overlay_sf <- NULL
      if (isTRUE(isolate(controls$show_pop_raster()))) {
        pop_overlay_sf <- tryCatch(
          make_population_overlay_sf(district_sf = sc$district_sf, u5_rast = get_u5_worldpop()),
          error = function(e) NULL)
      }
      friction_overlay_sf <- NULL
      if (isTRUE(isolate(controls$show_friction_raster())) &&
          !is.null(sc$friction_path) && file.exists(sc$friction_path)) {
        friction_overlay_sf <- tryCatch(
          make_friction_overlay_sf(district_sf = sc$district_sf,
                                   friction_rast = terra::rast(sc$friction_path)),
          error = function(e) NULL)
      }
      list(district_sf = sc$district_sf, grid_sf = sc$grid_sf,
           initial_assignments = sc$initial_assignments,
           neighbors_list = sc$neighbors_list, edge_list = sc$edge_list,
           pop_overlay_sf = pop_overlay_sf, friction_overlay_sf = friction_overlay_sf,
           friction_path = sc$friction_path, max_dim_m = sc$max_dim_m,
           seed_points = sc$seed_points_list, dfa_names = dynamic_dfa_names)
    })
    
    send_current_scene <- function() {
      req(tab_active())
      req(!is.null(rv$district_sf), !is.null(rv$grid_sf), !is.null(rv$current_assignments))
      init_named       <- setNames(as.list(rv$current_assignments), as.character(rv$grid_sf$cell_id))
      pop_geojson      <- if (!is.null(rv$pop_overlay_sf) && nrow(rv$pop_overlay_sf) > 0)
        as_geojson_text(rv$pop_overlay_sf) else NULL
      friction_geojson <- if (!is.null(rv$friction_overlay_sf) && nrow(rv$friction_overlay_sf) > 0)
        as_geojson_text(rv$friction_overlay_sf) else NULL
      saved_sf <- rv$saved_dfa_sf
      if (is.null(saved_sf))
        saved_sf <- build_saved_dfa_sf(grid_sf = rv$grid_sf,
                                       assignments = rv$current_assignments,
                                       district_sf = rv$district_sf)
      facility_pts <- list()
      fac_df <- tryCatch(all_facilities_r(), error = function(e) NULL)
      if (!is.null(fac_df) && nrow(fac_df) > 0) {
        non_sia <- fac_df[!is.na(fac_df$polio_sia_coordination_site) &
                            fac_df$polio_sia_coordination_site != 'Yes', , drop = FALSE]
        if (nrow(non_sia) > 0)
          facility_pts <- lapply(seq_len(nrow(non_sia)), function(i)
            list(lat = non_sia$lat[i], lon = non_sia$lon[i], name = non_sia$facility_name[i]))
      }
      landmark_pts <- list()
      lm_df <- tryCatch(landmarks_r(), error = function(e) NULL)
      if (!is.null(lm_df) && nrow(lm_df) > 0)
        landmark_pts <- lapply(seq_len(nrow(lm_df)), function(i)
          list(lat = lm_df$lat[i], lon = lm_df$lon[i], name = lm_df$landmark_name[i]))
      send_paint_message("show_loading")
      send_paint_message("paint_load_scene", list(
        districtGeojson    = as_geojson_text(rv$district_sf),
        gridGeojson        = as_geojson_text(rv$grid_sf),
        popGeojson         = pop_geojson,
        frictionGeojson    = friction_geojson,
        showPop            = isolate(controls$show_pop_raster()),
        showFriction       = isolate(controls$show_friction_raster()),
        initialAssignments = init_named,
        dfaColors          = as.list(current_fill_colors()),
        activeDfa          = active_dfa_rv(),
        neighbors          = rv$neighbors_list,
        edgeCells          = rv$edge_list,
        brushSize          = controls$brush_m(),
        boundaryOnly       = controls$boundary_only(),
        seedPoints         = rv$seed_points,
        facilityPoints     = facility_pts,
        landmarkPoints     = landmark_pts,
        savedGeojson       = as_geojson_text(saved_sf)
      ))
    }
    
    .apply_restore <- function(snap) {
      restore_just_applied(TRUE)
      if (!is.null(snap$dfa_names)) rv$dfa_names <- snap$dfa_names
      if (!is.null(snap$current_assignments) && length(snap$current_assignments) > 0) {
        ca <- snap$current_assignments
        # Convert named list to ordered character vector if needed
        if (is.list(ca)) {
          cell_ids <- as.character(rv$grid_sf$cell_id)
          ca <- vapply(cell_ids, function(id) {
            val <- ca[[id]]
            if (is.null(val)) rv$initial_assignments[as.integer(id)] else as.character(val)
          }, character(1))
        }
        rv$current_assignments <- as.character(ca)
      } else {
        # current_assignments not stored (older submission) — use initial grid state
        rv$current_assignments <- rv$initial_assignments
      }
      if (!is.null(snap$saved_dfa_sf)) rv$saved_dfa_sf <- snap$saved_dfa_sf
      recompute_population_table(rv$current_assignments)
      if (isTRUE(tab_active())) send_current_scene()
      showNotification('Health area state restored.', type = 'message', duration = 2)
      # Clear immediately — the guard branch in observeEvent(selected_scene()) was
      # designed to fire on a second scene computation during restore, but that firing
      # never occurs in practice (submitted_facilities is set before selected_scene
      # ever runs). Leaving this TRUE permanently blocks all future user-driven
      # regenerations (e.g. submitting new SIA sites).
      restore_just_applied(FALSE)
    }
    
    observeEvent(selected_scene(), {
      sc <- selected_scene()
      if (isTRUE(restore_just_applied())) {
        restore_just_applied(FALSE)
        rv$neighbors_list      <- sc$neighbors_list
        rv$edge_list           <- sc$edge_list
        rv$pop_overlay_sf      <- sc$pop_overlay_sf
        rv$friction_overlay_sf <- sc$friction_overlay_sf
        rv$friction_path       <- sc$friction_path
        rv$max_dim_m           <- sc$max_dim_m
        rv$seed_points         <- sc$seed_points
        recompute_population_table(rv$current_assignments)
        if (tab_active()) send_current_scene()
        return()
      }
      rv$dfa_names   <- sc$dfa_names
      rv$district_sf <- sc$district_sf
      rv$grid_sf     <- sc$grid_sf
      if (!active_dfa_rv() %in% rv$dfa_names) active_dfa_rv(rv$dfa_names[[1]])
      if (!"u5_pop" %in% names(rv$grid_sf) || all(rv$grid_sf$u5_pop == 0, na.rm = TRUE))
        rv$grid_sf$u5_pop <- calculate_grid_cell_population(rv$grid_sf, get_u5_worldpop())
      rv$initial_assignments  <- sc$initial_assignments
      rv$neighbors_list       <- sc$neighbors_list
      rv$edge_list            <- sc$edge_list
      rv$pop_overlay_sf       <- sc$pop_overlay_sf
      rv$friction_overlay_sf  <- sc$friction_overlay_sf
      rv$friction_path        <- sc$friction_path
      if (is.null(pending_restore())) rv$pop_table <- NULL
      rv$max_dim_m            <- sc$max_dim_m
      rv$seed_points          <- sc$seed_points
      snap <- pending_restore()
      if (!is.null(snap)) {
        pending_restore(NULL)
        .apply_restore(snap)
      } else {
        had_areas <- !is.null(rv$saved_dfa_sf)   # were areas already drawn?
        rv$current_assignments <- sc$initial_assignments
        rv$saved_dfa_sf        <- NULL
        recompute_population_table(sc$initial_assignments)
        if (tab_active()) send_current_scene()
        # Signal microplan to clear all planning data (SIA sites changed mid-session)
        if (had_areas) areas_regenerated_counter(areas_regenerated_counter() + 1L)
      }
    }, ignoreInit = FALSE)
    
    observeEvent(restore_r(), {
      snap <- restore_r()
      if (is.null(snap)) return()
      if (!is.null(snap$current_assignments) && length(snap$current_assignments) > 0) {
        if (!is.null(rv$grid_sf)) .apply_restore(snap)
        else pending_restore(snap)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    observeEvent(active_tab(), {
      if (tab_active() && isTRUE(district_ready()) && !is.null(rv$grid_sf))
        send_current_scene()
    }, ignoreInit = TRUE)
    
    observeEvent(map_mod$map_ready(), {
      if (tab_active()) send_paint_message("hide_loading")
    }, ignoreInit = TRUE)
    
    # ── Save (local only) ─────────────────────────────────────────────────────
    observeEvent(controls$save_click(), {
      req(isTRUE(district_ready()), tab_active())
      pending_action("save")
      send_paint_message("paint_request_assignments")
    })
    
    # ── Submit (save + write to DB) ───────────────────────────────────────────
    observeEvent(controls$submit_click(), {
      req(isTRUE(district_ready()), tab_active())
      req(!is.null(rv$grid_sf), !is.null(rv$district_sf))
      pending_action("submit_areas")
      send_paint_message("paint_request_assignments")
    }, ignoreInit = TRUE)
    
    # ── Reset ─────────────────────────────────────────────────────────────────
    observeEvent(controls$reset_click(), {
      req(isTRUE(district_ready()), tab_active())
      req(!is.null(rv$initial_assignments), !is.null(rv$dfa_names), length(rv$dfa_names) > 0)
      selected_dfa <- active_dfa_rv()
      if (is.null(selected_dfa) || !selected_dfa %in% rv$dfa_names) {
        selected_dfa <- rv$dfa_names[[1]]; active_dfa_rv(selected_dfa)
      }
      rv$current_assignments <- rv$initial_assignments
      rv$saved_dfa_sf        <- NULL
      pending_action(NULL)
      send_paint_message("paint_reset")
      send_paint_message("paint_set_colors",
                         list(colors = as.list(current_fill_colors()), activeDfa = selected_dfa))
      recompute_population_table(rv$initial_assignments)
    })
    
    # ── Receive assignments from JS ───────────────────────────────────────────
    observeEvent(map_mod$assignments(), {
      payload <- map_mod$assignments()
      req(!is.null(payload$assignments))
      req(!is.null(rv$grid_sf), !is.null(rv$district_sf), !is.null(rv$initial_assignments))
      
      js_assignments <- payload$assignments
      ordered_assignments <- vapply(as.character(rv$grid_sf$cell_id), function(cell_id) {
        val <- js_assignments[[cell_id]]
        if (is.null(val) || !nzchar(val)) rv$initial_assignments[as.integer(cell_id)]
        else as.character(val)
      }, character(1))
      
      rv$current_assignments <- ordered_assignments
      act <- pending_action()
      
      if (identical(act, "capture")) {
        recompute_population_table(ordered_assignments)
        saved <- tryCatch(
          build_saved_dfa_sf(grid_sf = rv$grid_sf, assignments = ordered_assignments,
                             district_sf = rv$district_sf),
          error = function(e) NULL
        )
        if (!is.null(saved)) {
          rv$saved_dfa_sf <- saved
          send_paint_message("paint_show_saved", list(geojson = as_geojson_text(saved)))
        }
        pending_action(NULL)
        return()
      }
      
      if (identical(act, "save")) {
        saved <- tryCatch(
          build_saved_dfa_sf(grid_sf = rv$grid_sf, assignments = ordered_assignments,
                             district_sf = rv$district_sf),
          error = function(e) e
        )
        if (inherits(saved, "error")) {
          showNotification(paste("Save failed:", saved$message), type = "error", duration = 8)
          pending_action(NULL); return()
        }
        old_saved <- rv$saved_dfa_sf
        rv$saved_dfa_sf <- saved
        send_paint_message("paint_show_saved", list(geojson = as_geojson_text(saved)))
        recompute_population_table(ordered_assignments)
        changed_areas_rv(.find_changed_areas(old_saved, saved))
      }
      
      if (identical(act, "submit_areas")) {
        saved <- tryCatch(
          build_saved_dfa_sf(grid_sf = rv$grid_sf, assignments = ordered_assignments,
                             district_sf = rv$district_sf),
          error = function(e) e
        )
        if (inherits(saved, "error")) {
          showNotification(paste("Submit failed:", saved$message), type = "error", duration = 8)
          pending_action(NULL); return()
        }
        old_saved <- rv$saved_dfa_sf
        rv$saved_dfa_sf <- saved
        send_paint_message("paint_show_saved", list(geojson = as_geojson_text(saved)))
        recompute_population_table(ordered_assignments)
        changed_areas_rv(.find_changed_areas(old_saved, saved))
        if (!is.null(submit_stage_fn)) {
          submit_stage_fn('areas', list(
            saved_dfa_sf        = saved,
            dfa_names           = rv$dfa_names,
            current_assignments = ordered_assignments
          ))
        }
      }
      
      if (identical(act, "refresh"))
        recompute_population_table(ordered_assignments)
      
      pending_action(NULL)
    }, ignoreInit = TRUE)
    
    observeEvent(active_dfa_rv(), {
      req(isTRUE(district_ready()), tab_active())
      req(!is.null(rv$current_assignments), !is.null(rv$grid_sf), !is.null(rv$district_sf))
      send_paint_message("paint_set_colors",
                         list(colors = as.list(current_fill_colors()), activeDfa = active_dfa_rv()))
      pending_action("capture")
      send_paint_message("paint_request_assignments")
    }, ignoreInit = TRUE)
    
    list(
      has_scene             = reactive(!is.null(rv$grid_sf)),
      friction_path         = reactive(rv$friction_path),
      current_assignments_r = reactive(rv$current_assignments),
      saved_dfa_sf_r        = reactive(rv$saved_dfa_sf),
      dfa_names_r           = reactive(rv$dfa_names),
      pop_table_r           = reactive(rv$pop_table),
      areas_regenerated     = areas_regenerated_counter,
      changed_areas         = changed_areas_rv,
      restore_from_snapshot = function(snap) {
        if (!is.null(rv$grid_sf)) .apply_restore(snap)
        else pending_restore(snap)
      }
    )
  })
}
