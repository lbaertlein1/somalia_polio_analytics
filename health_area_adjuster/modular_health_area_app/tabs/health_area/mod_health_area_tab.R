healthAreaTabUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 3, healthAreaControlsUI(ns('controls'))),
    column(width = 6, div(style = 'height: calc(100vh - 120px);', healthAreaMapUI(ns('map')))),
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
    subdivisions_r      = reactive(NULL),
    planning_area_sf_r  = reactive(NULL),
    submit_stage_fn     = NULL,
    save_snapshot_fn    = NULL,
    restore_r           = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    
    controls      <- healthAreaControlsServer("controls")
    map_mod       <- healthAreaMapServer("map")
    active_dfa_rv <- reactiveVal('Inaccessible')
    restore_just_applied     <- reactiveVal(FALSE)
    pending_action           <- reactiveVal(NULL)
    pending_restore          <- reactiveVal(NULL)
    areas_regenerated_counter <- reactiveVal(0L)
    changed_areas_rv          <- reactiveVal(character(0))
    last_scene_key            <- reactiveVal(NULL)
    areas_submitted_to_db     <- reactiveVal(FALSE)   # TRUE after submit, FALSE after any assignment change
    
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
      max_dim_m = NULL, brush_limits = NULL,
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
    
    # Use globally loaded raster from global.R (avoids redundant loading)
    get_u5_worldpop <- function() u5_rast
    
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
      
      current_names <- rv$dfa_names %||% all_dfa_names
      
      # Guard: reject assignments that contain names not in current_names.
      # This happens when a stale JS paint_request_assignments response arrives
      # from the previous scene after the new scene has already been loaded.
      # Bailing out silently keeps the pop table at its last valid state until
      # the next well-formed response arrives.
      known_assigned <- assignments[!is.na(assignments) & nzchar(assignments)]
      unknown        <- setdiff(unique(known_assigned), current_names)
      if (length(unknown) > 0) {
        cat(sprintf('[recompute_pop] skipped — %d unrecognised DFA name(s) in assignments: %s\n',
                    length(unknown), paste(unknown, collapse = ', ')))
        return(invisible(NULL))
      }
      
      df <- data.frame(area_name = assignments, est_u5_pop = rv$grid_sf$u5_pop,
                       stringsAsFactors = FALSE) |>
        dplyr::filter(!is.na(area_name), nzchar(area_name)) |>
        dplyr::group_by(area_name) |>
        dplyr::summarise(est_u5_pop = round(sum(est_u5_pop, na.rm = TRUE), 0), .groups = "drop")
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
      req(isTRUE(district_ready()))
      # Use planning area (urban/rural/full) when provided
      pa <- tryCatch(planning_area_sf_r(), error = function(e) NULL)
      if (!is.null(pa) && nrow(pa) > 0) {
        # max_dim needs 3857 (metres); district_sf must be 4326 for JS canvas
        dsf_3857  <- sf::st_transform(pa, 3857)
        dsf_3857  <- safe_make_valid(dsf_3857)
        dsf_3857  <- tryCatch(sf::st_collection_extract(dsf_3857, 'POLYGON'), error = function(e) dsf_3857)
        max_dim_m <- calc_district_max_dim(dsf_3857)
        dsf_4326  <- sf::st_transform(dsf_3857, 4326)
        return(list(district_sf = dsf_4326, max_dim_m = max_dim_m,
                    brush_limits = calc_brush_limits(max_dim_m)))
      }
      req(zone(), region(), district())
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
      dsf <- safe_make_valid(dsf)
      dsf <- tryCatch(sf::st_collection_extract(dsf, 'POLYGON'), error = function(e) dsf)
      max_dim_m <- calc_district_max_dim(dsf)
      list(district_sf = dsf, max_dim_m = max_dim_m,
           brush_limits = calc_brush_limits(max_dim_m))
    })
    
    observeEvent(district_base(), {
      db <- district_base()
      rv$district_base_sf <- db$district_sf
      rv$brush_limits      <- db$brush_limits
      controls$set_brush_limits(db$brush_limits)
    }, ignoreInit = TRUE)
    
    # Clear saved areas immediately on district change so downstream tabs
    # (microplan) never render stale polygons from the previous district.
    # This fires regardless of which tab is currently visible.
    observeEvent(district(), {
      rv$saved_dfa_sf        <- NULL
      rv$current_assignments <- NULL
      rv$pop_table           <- NULL
      last_scene_key(NULL)   # force full reset next time health areas tab opens
    }, ignoreInit = TRUE)
    
    observeEvent(controls$brush_m(), {
      req(tab_active(), isTRUE(district_ready()))
      # Slider is in diameter; JS expects radius
      send_paint_message("paint_set_brush", list(value = controls$brush_m() / 2))
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
    
    # Full district sf — always the complete district polygon from districts_shp,
    # used for friction path lookup regardless of planning unit.
    full_district_sf_r <- reactive({
      req(district_ready(), zone(), region(), district())
      dsf <- districts_shp |>
        dplyr::filter(zone_name == zone(), region_name == region(),
                      district_name == district()) |>
        dplyr::summarise(
          admin_id = dplyr::first(admin_id), district_name = dplyr::first(district_name),
          region_id = dplyr::first(region_id), region_name = dplyr::first(region_name),
          zone_id = dplyr::first(zone_id), zone_name = dplyr::first(zone_name),
          geometry = sf::st_union(geometry), .groups = "drop"
        ) |> sf::st_as_sf() |> safe_make_valid()
      dsf
    })
    
    # Subdivision boundary lines — derived once per district, used as soft
    # barriers in the health area generation (penalty = 0.99 raw friction).
    subdivision_boundary_lines_r <- reactive({
      subs <- tryCatch(subdivisions_r(), error = function(e) NULL)
      subdivisions_to_boundary_lines(subs)
    })
    
    initial_scene <- initialHealthAreaGenerationServer(
      "initial_scene",
      district_sf                  = reactive({ req(district_base()); district_base()$district_sf }),
      friction_district_sf         = full_district_sf_r,
      grid_n                       = reactive({
        req(district_base())
        max_dim  <- district_base()$max_dim_m
        # Target ~25k cells regardless of district size.
        # Cellsize = max_dim / 160, snapped to nearest 50m, floored at 50m.
        cellsize <- max(50, round(max_dim / 160 / 50) * 50)
        as.integer(round(max_dim / cellsize))
      }),
      n_dfa                        = n_start_dfas,
      seed                         = reactive({ req(district()); sum(utf8ToInt(district())) }),
      facility_seed_sf             = facility_seed_sf,
      facility_name_col            = "facility_name",
      subdivision_lines_sf         = subdivision_boundary_lines_r,
      subdivision_boundary_penalty = 0.99,
      u5_rast                      = u5_rast
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
      # Subdivision boundaries — subdivisions_r() already returns NULL for
      # rural planning units, so subdivision_boundary_lines_r() will be NULL too.
      subdiv_geojson <- tryCatch({
        bl <- subdivision_boundary_lines_r()
        if (!is.null(bl) && nrow(bl) > 0) as_geojson_text(bl) else NULL
      }, error = function(e) NULL)
      
      send_paint_message("show_loading")
      send_paint_message("paint_load_scene", list(
        districtGeojson      = as_geojson_text(rv$district_sf),
        gridGeojson          = as_geojson_text(rv$grid_sf),
        popGeojson           = pop_geojson,
        frictionGeojson      = friction_geojson,
        subdivisionGeojson   = subdiv_geojson,
        showPop              = isolate(controls$show_pop_raster()),
        showFriction         = isolate(controls$show_friction_raster()),
        initialAssignments   = init_named,
        dfaColors            = as.list(current_fill_colors()),
        activeDfa            = active_dfa_rv(),
        neighbors            = rv$neighbors_list,
        edgeCells            = rv$edge_list,
        brushSize            = controls$brush_m() / 2,  # diameter -> radius for JS
        boundaryOnly         = controls$boundary_only(),
        seedPoints           = rv$seed_points,
        facilityPoints       = facility_pts,
        landmarkPoints       = landmark_pts,
        savedGeojson         = as_geojson_text(saved_sf)
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
      
      # Build a key that identifies what DATA drives this scene.
      # It changes when the district or SIA seed locations change, but NOT
      # when the user simply switches tabs (tab_active() flip).
      seed_key <- if (!is.null(sc$seed_points) && length(sc$seed_points) > 0) {
        paste(sort(vapply(sc$seed_points, function(p)
          paste0(round(p$lon %||% 0, 5), ',', round(p$lat %||% 0, 5)),
          character(1)
        )), collapse = '|')
      } else ''
      new_key      <- paste0(isolate(district()), '|', seed_key)
      scene_is_new <- !identical(last_scene_key(), new_key)
      last_scene_key(new_key)
      
      # Always refresh spatial/structural data (cheap; needed by send_current_scene)
      rv$dfa_names           <- sc$dfa_names
      rv$district_sf         <- sc$district_sf
      rv$grid_sf             <- sc$grid_sf
      rv$initial_assignments <- sc$initial_assignments
      rv$neighbors_list      <- sc$neighbors_list
      rv$edge_list           <- sc$edge_list
      rv$pop_overlay_sf      <- sc$pop_overlay_sf
      rv$friction_overlay_sf <- sc$friction_overlay_sf
      rv$friction_path       <- sc$friction_path
      rv$max_dim_m           <- sc$max_dim_m
      rv$seed_points         <- sc$seed_points
      
      if (!active_dfa_rv() %in% rv$dfa_names) active_dfa_rv(rv$dfa_names[[1]])
      if (!"u5_pop" %in% names(rv$grid_sf) || all(rv$grid_sf$u5_pop == 0, na.rm = TRUE))
        rv$grid_sf$u5_pop <- calculate_grid_cell_population(rv$grid_sf, get_u5_worldpop())
      
      snap <- pending_restore()
      if (!is.null(snap)) {
        pending_restore(NULL)
        .apply_restore(snap)
      } else if (scene_is_new) {
        # Genuinely new scene (different district or different SIA sites) — full reset
        had_areas              <- !is.null(rv$saved_dfa_sf)
        rv$current_assignments <- sc$initial_assignments
        rv$saved_dfa_sf        <- NULL
        rv$pop_table           <- NULL
        recompute_population_table(sc$initial_assignments)
        if (tab_active()) send_current_scene()
        if (had_areas) areas_regenerated_counter(areas_regenerated_counter() + 1L)
      } else {
        # Same scene — tab just became visible again; restore painted state as-is.
        # observeEvent(active_tab()) handles the re-send, so just ensure pop table is current.
        recompute_population_table(rv$current_assignments %||% sc$initial_assignments)
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
    
    # ── Continue → microplan ─────────────────────────────────────────────────
    .do_continue_to_microplan <- function() {
      session$sendCustomMessage('switch_tab', list(value = 'tab_microplan'))
    }
    
    .do_submit_and_continue_areas <- function() {
      req(!is.null(rv$grid_sf), !is.null(rv$district_sf))
      pending_action("submit_areas")
      send_paint_message("paint_request_assignments")
      # Navigation happens after assignments come back and submit completes
      # via a one-shot observer below
      areas_continue_after_submit(TRUE)
    }
    
    areas_continue_after_submit <- reactiveVal(FALSE)
    
    observeEvent(areas_submitted_to_db(), {
      if (isTRUE(areas_continue_after_submit()) && isTRUE(areas_submitted_to_db())) {
        areas_continue_after_submit(FALSE)
        .do_continue_to_microplan()
      }
    }, ignoreInit = TRUE)
    
    observeEvent(controls$continue_click(), {
      if (is.null(rv$saved_dfa_sf)) {
        showNotification(
          'Please save your health area boundaries before continuing.',
          type = 'warning', duration = 4
        )
        return()
      }
      if (!isTRUE(areas_submitted_to_db())) {
        showModal(modalDialog(
          title     = 'Unsaved health area edits',
          size      = 's', easyClose = FALSE, footer = NULL,
          div(style = 'font-size:13px;color:#475569;margin-bottom:16px;',
              'Your health area boundaries have not been submitted to the database. Submit them now to keep your work, or continue without saving.'),
          div(style = 'display:flex;gap:8px;justify-content:flex-end;flex-wrap:wrap;',
              actionButton(session$ns('ha_continue_cancel'),       'Cancel',                  class = 'btn btn-default'),
              actionButton(session$ns('ha_continue_without_save'), 'Continue without saving', class = 'btn btn-default'),
              actionButton(session$ns('ha_submit_and_continue'),   'Submit & Continue',       class = 'btn btn-primary',
                           style = 'font-weight:600;')
          )
        ))
        return()
      }
      .do_continue_to_microplan()
    }, ignoreInit = TRUE)
    
    observeEvent(input$ha_continue_cancel, {
      removeModal()
    }, ignoreInit = TRUE)
    
    observeEvent(input$ha_continue_without_save, {
      removeModal()
      .do_continue_to_microplan()
    }, ignoreInit = TRUE)
    
    observeEvent(input$ha_submit_and_continue, {
      removeModal()
      .do_submit_and_continue_areas()
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
      
      # Reject stale JS responses: if any non-empty assignment name is not in
      # rv$dfa_names the JS scene is out of sync with the current R scene.
      # Clear pending_action so the app doesn't get stuck, then wait for the
      # next (fresh) response.
      if (!is.null(rv$dfa_names)) {
        known_assigned <- ordered_assignments[!is.na(ordered_assignments) & nzchar(ordered_assignments)]
        unknown        <- setdiff(unique(known_assigned), rv$dfa_names)
        if (length(unknown) > 0) {
          cat(sprintf('[assignments] stale response (%d unknown name(s)), discarding\n',
                      length(unknown)))
          pending_action(NULL)
          return()
        }
      }
      
      rv$current_assignments <- ordered_assignments
      act <- pending_action()
      
      # Only mark as unsubmitted when user actively saves (paints new boundaries).
      # Passive captures (colour changes, tab focus) should not reset this flag.
      if (identical(act, 'save'))
        areas_submitted_to_db(FALSE)
      
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
          areas_submitted_to_db(TRUE)
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
        # Always restore saved_dfa_sf immediately — the microplan tab reads it
        # directly and must not have to wait for the health area grid to compute.
        if (!is.null(snap$saved_dfa_sf) && nrow(snap$saved_dfa_sf) > 0)
          rv$saved_dfa_sf <- snap$saved_dfa_sf
        
        if (!is.null(rv$grid_sf)) .apply_restore(snap)
        else pending_restore(snap)
      }
    )
  })
}