# =============================================================================
# mod_team_area_tab.R
#
# New in v2 (change #1). Structurally this is a second, nested instance of
# the health-area machinery: same paint-app.js canvas, same BFS engine, but
# seeded WITHIN each health area rather than across the whole district, and
# with the health area's own polygon as the grid boundary (which makes it a
# hard wall automatically — the grid simply doesn't extend past it, the same
# way the district edge works for Health Areas).
#
# One health area is worked on at a time, chosen from a selector built from
# saved_dfa_sf_r() (the finalized health areas from the Health Areas tab).
# Team-area boundaries for each health area visited this session are cached
# locally (per_area) so switching between health areas doesn't lose work;
# "Submit Team Areas" combines everything cached so far into one write.
#
# Undo, brush painting, and the Inaccessible/Unpopulated toggle all reuse
# the same paint-app.js engine and its unified undo stack — no separate
# wiring needed beyond passing an undoCountInputId so the Undo button can
# disable itself when the stack is empty.
#
# Simplification versus mod_health_area_tab.R: this v1 does not replicate
# every edge-case guard the health-area tab has accumulated (stale-response
# detection keyed by a seed hash, "areas_regenerated" notifications on
# facility changes, etc.) — health areas don't change facility seeds live
# the way health areas do from SIA site edits, so most of that class of
# guard doesn't apply here. The core save/submit/restore flow is complete.
# =============================================================================

teamAreaTabUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 3, teamAreaControlsUI(ns('controls'))),
    column(width = 6, div(style = 'height: calc(100vh - 120px);', teamAreaMapUI(ns('map')))),
    column(width = 3,
           div(style = 'overflow-y: auto; height: calc(100vh - 120px);',
               healthAreaPopulationUI(ns('population'))))
  )
}

teamAreaTabServer <- function(
    id,
    district, campaign_id, district_ready,
    active_tab,
    saved_dfa_sf_r    = reactive(NULL),   # finalized health areas from the Health Areas tab
    all_facilities_r  = reactive(NULL),
    landmarks_r       = reactive(NULL),
    submit_stage_fn   = NULL,
    restore_r         = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {

    controls <- teamAreaControlsServer('controls')
    map_mod  <- teamAreaMapServer('map')
    active_team_rv <- reactiveVal(NULL)

    tab_active <- reactive({ identical(active_tab(), 'tab_team_area_mapping') })

    observeEvent(controls$help_click(), { .show_team_area_help_modal() })

    .show_team_area_help_modal <- function() {
      showModal(modalDialog(
        title = tags$span(
          style = 'font-size: 15px; font-weight: 700; color: #0f172a;',
          'Team Area Mapping — How to use this tab'
        ),
        div(
          style = 'font-size: 13px; line-height: 1.7; color: #334155;',
          tags$p(
            'A ', tags$strong('Team Area'), ' is the territory covered by one outreach team, within ',
            'one health area. Pick a health area from the left, then paint team boundaries the same ',
            'way you painted health area boundaries — left click/drag to paint, right click/drag to pan.'
          ),
          tags$p(
            style = 'margin-top: 10px;',
            'Save keeps that health area’s team boundaries locally so you can move to the next ',
            'health area without losing work. Submit Team Areas writes everything saved so far to the ',
            'database.'
          )
        ),
        easyClose = TRUE, footer = modalButton('Close')
      ))
    }

    rv <- reactiveValues(
      district_sf = NULL, grid_sf = NULL, initial_assignments = NULL,
      current_assignments = NULL, saved_team_sf = NULL, team_names = NULL,
      neighbors_list = NULL, edge_list = NULL,
      pop_overlay_sf = NULL, friction_overlay_sf = NULL, pop_table = NULL,
      seed_points = NULL, friction_path = NULL,
      smoothed_team_sf = NULL   # vertex-refined boundary for the CURRENTLY selected health area
    )

    # Cached per-health-area results, accumulated across the session.
    # per_area[[health_area_name]] = list(saved_team_sf, team_names, assignments_named, smoothed_team_sf)
    per_area <- reactiveValues()

    pending_action  <- reactiveVal(NULL)
    last_loaded_key <- reactiveVal(NULL)
    in_vertex_mode  <- reactiveVal(FALSE)

    send_paint_message <- function(type, payload = list()) {
      session$sendCustomMessage(type, c(list(
        mapId              = map_mod$map_id,
        loadingOverlayId   = map_mod$loading_overlay_id,
        readyInputId       = map_mod$ready_input_id,
        assignmentsInputId = map_mod$assignments_input_id,
        undoCountInputId   = map_mod$undo_count_input_id
      ), payload))
    }

    current_fill_colors <- reactive({
      make_fill_colors(active_dfa = active_team_rv(), dfa_names = rv$team_names %||% character(0))
    })

    healthAreaPopulationServer(
      'population',
      active_dfa_rv        = active_team_rv,
      show_pop_raster      = controls$show_pop_raster,
      show_friction_raster = controls$show_friction_raster,
      pop_table            = reactive(rv$pop_table),
      in_vertex_mode       = in_vertex_mode
    )

    recompute_population_table <- function(assignments) {
      req(!is.null(rv$grid_sf), length(assignments) == nrow(rv$grid_sf))
      req('u5_pop' %in% names(rv$grid_sf))
      current_names <- rv$team_names %||% character(0)
      known <- assignments[!is.na(assignments) & nzchar(assignments)]
      unknown <- setdiff(unique(known), current_names)
      if (length(unknown) > 0) return(invisible(NULL))

      df <- data.frame(area_name = assignments, est_u5_pop = rv$grid_sf$u5_pop, stringsAsFactors = FALSE) |>
        dplyr::filter(!is.na(area_name), nzchar(area_name)) |>
        dplyr::group_by(area_name) |>
        dplyr::summarise(est_u5_pop = round(sum(est_u5_pop, na.rm = TRUE), 0), .groups = 'drop')
      missing <- setdiff(current_names, df$area_name)
      if (length(missing) > 0)
        df <- dplyr::bind_rows(df, data.frame(area_name = missing, est_u5_pop = 0, stringsAsFactors = FALSE))
      df <- df |>
        dplyr::mutate(area_name = factor(area_name, levels = current_names)) |>
        dplyr::arrange(area_name) |>
        dplyr::mutate(area_name = as.character(area_name))
      rv$pop_table <- dplyr::bind_rows(
        df,
        data.frame(area_name = 'Health Area Total',
                  est_u5_pop = round(sum(rv$grid_sf$u5_pop, na.rm = TRUE), 0),
                  stringsAsFactors = FALSE)
      )
      invisible(NULL)
    }

    # ── Health area choices ───────────────────────────────────────────────────

    health_area_names <- reactive({
      sf_obj <- tryCatch(saved_dfa_sf_r(), error = function(e) NULL)
      if (is.null(sf_obj) || nrow(sf_obj) == 0) return(character(0))
      setdiff(unique(as.character(sf_obj$dfa_name)), extra_dfa_names)
    })

    observeEvent(health_area_names(), {
      controls$set_health_area_choices(health_area_names())
    })

    output_status <- function() {
      done <- names(reactiveValuesToList(per_area))
      controls$set_status_ui(
        if (length(done) == 0) NULL else
          div(style = 'font-size:11px;color:#0d9488;margin-top:4px;',
              sprintf('Saved: %s', paste(done, collapse = ', ')))
      )
    }
    observe({ output_status() })

    selected_health_area_sf <- reactive({
      req(controls$health_area(), saved_dfa_sf_r())
      sf_obj <- saved_dfa_sf_r()
      sub <- sf_obj[sf_obj$dfa_name == controls$health_area(), , drop = FALSE]
      req(nrow(sub) > 0)
      sub
    })

    # ── Friction lookup context — reuse the parent district ──────────────────

    full_district_sf_r <- reactive({
      req(district_ready(), district())
      districts_shp |>
        dplyr::filter(district_name == district()) |>
        dplyr::summarise(
          admin_id = dplyr::first(admin_id), district_name = dplyr::first(district_name),
          region_id = dplyr::first(region_id), region_name = dplyr::first(region_name),
          zone_id = dplyr::first(zone_id), zone_name = dplyr::first(zone_name),
          geometry = sf::st_union(geometry), .groups = 'drop'
        ) |> sf::st_as_sf() |> safe_make_valid()
    })

    area_population <- reactive({
      ha <- tryCatch(selected_health_area_sf(), error = function(e) NULL)
      req(!is.null(ha))
      if (is.null(u5_rast)) return(0)
      tryCatch({
        ha_proj <- sf::st_transform(ha, sf::st_crs(terra::crs(u5_rast)))
        sum(exactextractr::exact_extract(raster::raster(u5_rast), ha_proj, fun = 'sum'), na.rm = TRUE)
      }, error = function(e) 0)
    })

    n_teams_r <- reactive(compute_n_teams(area_population(), campaign_id = campaign_id()))

    team_seed_sf <- reactive({
      req(selected_health_area_sf(), n_teams_r())
      compute_team_area_seeds(
        health_area_sf = selected_health_area_sf(),
        u5_rast        = u5_rast,
        n_teams        = n_teams_r(),
        seed           = sum(utf8ToInt(controls$health_area() %||% 'x'))
      )
    })

    team_scene_mod <- initialHealthAreaGenerationServer(
      'team_scene',
      district_sf           = reactive({ req(selected_health_area_sf()); selected_health_area_sf() }),
      friction_district_sf  = full_district_sf_r,
      grid_n                = reactive({
        req(selected_health_area_sf())
        ha_3857  <- sf::st_transform(selected_health_area_sf(), 3857)
        bbox     <- sf::st_bbox(ha_3857)
        max_dim  <- max(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin)
        cellsize <- max(30, round(max_dim / 100 / 25) * 25)
        max(20L, as.integer(round(max_dim / cellsize)))
      }),
      n_dfa                 = 1,   # unused — facility_seed_sf is always provided below
      seed                  = reactive(sum(utf8ToInt(controls$health_area() %||% 'x'))),
      facility_seed_sf      = team_seed_sf,
      facility_name_col     = 'team_name',
      subdivision_boundary_penalty = 0.99,
      u5_rast               = u5_rast,
      # Without this, scene() computes (grid build, friction extraction,
      # BFS propagation, team seed placement) the moment anything reads
      # it -- including just being an observeEvent() trigger below --
      # regardless of whether this tab is actually visible. That's why a
      # team-seed-placement crash could surface while still on the
      # Health Area page: this scene was generating silently in the
      # background before the user ever visited Team Areas.
      active                = tab_active
    )

    send_current_scene <- function() {
      req(tab_active())
      req(!is.null(rv$district_sf), !is.null(rv$grid_sf), !is.null(rv$current_assignments))
      init_named <- setNames(as.list(rv$current_assignments), as.character(rv$grid_sf$cell_id))
      saved_sf <- rv$saved_team_sf
      if (is.null(saved_sf))
        saved_sf <- build_saved_dfa_sf(grid_sf = rv$grid_sf, assignments = rv$current_assignments,
                                       district_sf = rv$district_sf)
      facility_pts <- list()
      fac_df <- tryCatch(all_facilities_r(), error = function(e) NULL)
      if (!is.null(fac_df) && nrow(fac_df) > 0)
        facility_pts <- lapply(seq_len(nrow(fac_df)), function(i)
          list(lat = fac_df$lat[i], lon = fac_df$lon[i], name = fac_df$facility_name[i]))
      landmark_pts <- list()
      lm_df <- tryCatch(landmarks_r(), error = function(e) NULL)
      if (!is.null(lm_df) && nrow(lm_df) > 0)
        landmark_pts <- lapply(seq_len(nrow(lm_df)), function(i)
          list(lat = lm_df$lat[i], lon = lm_df$lon[i], name = lm_df$landmark_name[i]))

      send_paint_message('show_loading')
      send_paint_message('paint_load_scene', list(
        districtGeojson    = as_geojson_text(rv$district_sf),
        gridGeojson        = as_geojson_text(rv$grid_sf),
        initialAssignments = init_named,
        dfaColors          = as.list(current_fill_colors()),
        activeDfa          = active_team_rv(),
        neighbors          = rv$neighbors_list,
        edgeCells          = rv$edge_list,
        brushSize          = controls$brush_m() / 2,
        boundaryOnly       = controls$boundary_only(),
        seedPoints         = rv$seed_points,
        facilityPoints     = facility_pts,
        landmarkPoints     = landmark_pts,
        savedGeojson       = as_geojson_text(saved_sf)
      ))
    }

    # ── Load a health area's scene: from cache if visited before, else fresh ──

    observeEvent(list(controls$health_area(), team_scene_mod$scene()), {
      req(nzchar(controls$health_area() %||% ''))
      key <- controls$health_area()
      if (identical(last_loaded_key(), key)) return()

      cached <- if (!is.null(per_area[[key]])) per_area[[key]] else NULL
      sc <- tryCatch(team_scene_mod$scene(), error = function(e) NULL)
      req(!is.null(sc))

      last_loaded_key(key)

      rv$district_sf         <- sc$district_sf
      rv$grid_sf              <- sc$grid_sf
      rv$grid_sf$u5_pop        <- sc$grid_sf$u5_pop %||% rep(0, nrow(sc$grid_sf))
      rv$neighbors_list          <- sc$neighbors_list
      rv$edge_list                <- sc$edge_list
      rv$friction_path              <- sc$friction_path
      rv$seed_points                  <- sc$seed_points_list
      rv$team_names                     <- normalize_dfa_names_team(unique(as.character(sc$initial_assignments)))

      if (!is.null(cached)) {
        rv$initial_assignments <- sc$initial_assignments
        rv$current_assignments <- vapply(as.character(rv$grid_sf$cell_id), function(id) {
          val <- cached$assignments_named[[id]]
          if (is.null(val)) sc$initial_assignments[as.integer(id)] else as.character(val)
        }, character(1))
        rv$saved_team_sf    <- cached$saved_team_sf
        rv$team_names       <- cached$team_names %||% rv$team_names
        rv$smoothed_team_sf <- cached$smoothed_team_sf   # NULL if this health area's team areas were never refined
      } else {
        rv$initial_assignments <- sc$initial_assignments
        rv$current_assignments <- sc$initial_assignments
        rv$saved_team_sf       <- NULL
        rv$smoothed_team_sf    <- NULL
      }
      in_vertex_mode(FALSE)   # switching health areas always drops back to painting

      if (is.null(active_team_rv()) || !active_team_rv() %in% rv$team_names) active_team_rv(rv$team_names[[1]])
      recompute_population_table(rv$current_assignments)
      if (tab_active()) send_current_scene()
    }, ignoreInit = TRUE)

    observeEvent(active_tab(), {
      if (tab_active() && !is.null(rv$grid_sf)) send_current_scene()
    }, ignoreInit = TRUE)

    observeEvent(map_mod$map_ready(), {
      if (tab_active()) send_paint_message('hide_loading')
    }, ignoreInit = TRUE)

    observeEvent(map_mod$undo_count(), {
      shinyjs::toggleState(session$ns('controls-undo_btn'),
                           condition = isTRUE(map_mod$undo_count() > 0))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # ── Controls wiring ────────────────────────────────────────────────────────

    observeEvent(controls$brush_m(), {
      req(tab_active()); send_paint_message('paint_set_brush', list(value = controls$brush_m() / 2))
    }, ignoreInit = TRUE)
    observeEvent(controls$boundary_only(), {
      req(tab_active()); send_paint_message('paint_set_boundary_only', list(value = controls$boundary_only()))
    }, ignoreInit = TRUE)
    observeEvent(controls$undo_click(), {
      req(tab_active()); send_paint_message('paint_undo')
      pending_action('refresh'); send_paint_message('paint_request_assignments')
    }, ignoreInit = TRUE)

    observeEvent(active_team_rv(), {
      req(tab_active(), !is.null(rv$current_assignments))
      send_paint_message('paint_set_colors', list(colors = as.list(current_fill_colors()), activeDfa = active_team_rv()))
      pending_action('capture'); send_paint_message('paint_request_assignments')
    }, ignoreInit = TRUE)

    observeEvent(controls$reset_click(), {
      req(tab_active(), !is.null(rv$initial_assignments))
      rv$current_assignments <- rv$initial_assignments
      rv$saved_team_sf       <- NULL
      send_paint_message('paint_reset')
      send_paint_message('paint_set_colors', list(colors = as.list(current_fill_colors()), activeDfa = active_team_rv()))
      recompute_population_table(rv$initial_assignments)
    }, ignoreInit = TRUE)

    observeEvent(controls$save_click(), {
      req(tab_active()); pending_action('save'); send_paint_message('paint_request_assignments')
    }, ignoreInit = TRUE)

    observeEvent(controls$submit_click(), {
      req(tab_active())
      pending_action('submit_all'); send_paint_message('paint_request_assignments')
    }, ignoreInit = TRUE)

    # ── Refine boundaries (vertex editing) ──────────────────────────────
    observeEvent(controls$refine_boundaries_click(), {
      req(tab_active())
      if (isTRUE(in_vertex_mode())) {
        send_paint_message("paint_exit_vertex_mode")
        in_vertex_mode(FALSE)
      } else {
        req(!is.null(rv$grid_sf))
        send_paint_message("paint_enter_vertex_mode", list(
          smoothness = controls$vertex_smoothness(),
          stiffness  = controls$vertex_stiffness()
        ))
        in_vertex_mode(TRUE)
      }
      controls$set_vertex_mode_ui(in_vertex_mode())
    }, ignoreInit = TRUE)

    # Live re-simplify / re-stiffen while already in vertex mode -- same
    # tradeoff as mod_health_area_tab.R's identical wiring: both discard
    # in-progress manual edits, re-deriving from the cached raw trace.
    observeEvent(controls$vertex_smoothness(), {
      req(isTRUE(in_vertex_mode()))
      send_paint_message("paint_set_vertex_smoothness", list(value = controls$vertex_smoothness()))
    }, ignoreInit = TRUE)

    observeEvent(controls$vertex_stiffness(), {
      req(isTRUE(in_vertex_mode()))
      send_paint_message("paint_set_vertex_stiffness", list(value = controls$vertex_stiffness()))
    }, ignoreInit = TRUE)

    observeEvent(controls$save_refinements_click(), {
      req(isTRUE(in_vertex_mode()))
      pending_action("manual_refine_save")
      send_paint_message("paint_save_vertex_edits")
      send_paint_message("paint_request_vertex_geojson")
    }, ignoreInit = TRUE)

    # Fires for three distinct reasons, distinguished by pending_action() --
    # same pattern as mod_health_area_tab.R's identical wiring:
    #   "manual_refine_save"    -- user explicitly clicked Save Refinements.
    #   "finalize_save"         -- Save was clicked; always runs the CURRENT
    #                              health area's boundary through vertex
    #                              conversion as part of finishing, whether
    #                              or not Refine Boundaries was ever opened.
    #   "finalize_submit_all"   -- same, then combines every cached health
    #                              area (including this freshly-converted
    #                              one) into the actual DB write.
    observeEvent(map_mod$vertex_geojson(), {
      payload <- map_mod$vertex_geojson()
      req(!is.null(payload$geojson))
      act <- pending_action()

      parsed <- tryCatch(geojsonsf::geojson_sf(payload$geojson), error = function(e) e)
      if (inherits(parsed, "error")) {
        showNotification(paste("Could not read boundary:", parsed$message), type = "error", duration = 8)
        pending_action(NULL)
        return()
      }
      sf::st_crs(parsed) <- 4326

      if (identical(act, "manual_refine_save")) {
        rv$saved_team_sf    <- parsed
        rv$smoothed_team_sf <- parsed
        send_paint_message('paint_show_saved', list(geojson = as_geojson_text(parsed)))
        showNotification("Boundary refinements saved.", type = "message", duration = 2)
        pending_action(NULL)
        return()
      }

      if (identical(act, "finalize_save") || identical(act, "finalize_submit_all")) {
        rv$smoothed_team_sf <- parsed

        if (isTRUE(rv$vertex_mode_silently_entered)) {
          send_paint_message("paint_exit_vertex_mode")
          rv$vertex_mode_silently_entered <- FALSE
        }

        .cache_current_area(rv$current_assignments, saved_sf_override = parsed)
        recompute_population_table(rv$current_assignments)

        if (identical(act, "finalize_save")) {
          showNotification(sprintf('Team areas saved for %s.', controls$health_area()), type = 'message', duration = 2)
          pending_action(NULL)
          return()
        }

        all_keys <- names(reactiveValuesToList(per_area))
        if (length(all_keys) == 0) { pending_action(NULL); return() }

        combined_sf <- tryCatch(
          do.call(rbind, lapply(all_keys, function(k) {
            x <- per_area[[k]]$saved_team_sf
            x$health_area <- k
            x
          })),
          error = function(e) NULL
        )
        team_names_by_area <- setNames(lapply(all_keys, function(k) per_area[[k]]$team_names), all_keys)
        assignments_by_area <- setNames(lapply(all_keys, function(k) per_area[[k]]$assignments_named), all_keys)
        smoothed_by_area     <- setNames(lapply(all_keys, function(k) per_area[[k]]$smoothed_team_sf), all_keys)

        if (!is.null(combined_sf) && !is.null(submit_stage_fn)) {
          submit_stage_fn('team_areas', list(
            saved_team_sf             = combined_sf,
            team_names                = team_names_by_area,
            current_team_assignments  = assignments_by_area,
            smoothed_team_sf          = smoothed_by_area
          ))
          showNotification('Team areas submitted.', type = 'message', duration = 3)
        }
        pending_action(NULL)
        return()
      }
    }, ignoreInit = TRUE)

    # saved_sf_override lets callers supply an already-computed boundary
    # (the vertex-converted result) instead of having this function
    # rebuild a fresh raw-grid one -- used by the finalize_save/
    # finalize_submit_all path below, where rebuilding here would
    # silently discard the conversion that was just done.
    .cache_current_area <- function(ordered_assignments, saved_sf_override = NULL) {
      key  <- controls$health_area()
      saved <- if (!is.null(saved_sf_override)) {
        saved_sf_override
      } else {
        tryCatch(
          build_saved_dfa_sf(grid_sf = rv$grid_sf, assignments = ordered_assignments, district_sf = rv$district_sf),
          error = function(e) NULL
        )
      }
      if (is.null(saved)) return(invisible(NULL))
      rv$saved_team_sf <- saved
      per_area[[key]] <- list(
        saved_team_sf     = saved,
        team_names        = rv$team_names,
        assignments_named = setNames(as.list(ordered_assignments), as.character(rv$grid_sf$cell_id)),
        smoothed_team_sf  = rv$smoothed_team_sf   # carries forward whatever refinement exists for THIS health area
      )
      send_paint_message('paint_show_saved', list(geojson = as_geojson_text(saved)))
      invisible(saved)
    }

    observeEvent(map_mod$assignments(), {
      payload <- map_mod$assignments()
      req(!is.null(payload$assignments), !is.null(rv$grid_sf), !is.null(rv$district_sf))

      js_assignments <- payload$assignments
      ordered <- vapply(as.character(rv$grid_sf$cell_id), function(id) {
        val <- js_assignments[[id]]
        if (is.null(val) || !nzchar(val)) rv$initial_assignments[as.integer(id)] else as.character(val)
      }, character(1))

      known <- ordered[!is.na(ordered) & nzchar(ordered)]
      if (length(setdiff(unique(known), rv$team_names)) > 0) { pending_action(NULL); return() }

      rv$current_assignments <- ordered
      act <- pending_action()

      if (identical(act, 'capture') || identical(act, 'refresh')) {
        recompute_population_table(ordered)
        pending_action(NULL); return()
      }

      # Save and Submit both always finish by running the CURRENT health
      # area's boundary through grid -> vertex conversion before anything
      # is cached or written to the DB -- see the vertex_geojson observer
      # above for where finalize_save/finalize_submit_all actually
      # complete. No longer depends on the user having opened Refine
      # Boundaries themselves; if they have (in_vertex_mode() is TRUE),
      # this finalizes their current live edits directly instead.
      if (identical(act, 'save') || identical(act, 'submit_all')) {
        pending_action(if (identical(act, 'save')) 'finalize_save' else 'finalize_submit_all')
        if (!isTRUE(in_vertex_mode())) {
          rv$vertex_mode_silently_entered <- TRUE
          send_paint_message("paint_enter_vertex_mode", list(
            smoothness = isolate(controls$vertex_smoothness()) %||% 2,
            stiffness  = isolate(controls$vertex_stiffness()) %||% 6
          ))
        }
        send_paint_message("paint_save_vertex_edits")
        send_paint_message("paint_request_vertex_geojson")
        return()
      }

      pending_action(NULL)
    }, ignoreInit = TRUE)

    # ── Restore ────────────────────────────────────────────────────────────────

    observeEvent(restore_r(), {
      snap <- restore_r()
      if (is.null(snap) || is.null(snap$current_team_assignments)) return()
      by_area <- snap$current_team_assignments
      saved_sf_all <- snap$saved_team_sf
      smoothed_all <- snap$smoothed_team_sf %||% list()
      for (k in names(by_area)) {
        area_sf <- NULL
        if (!is.null(saved_sf_all) && 'health_area' %in% names(saved_sf_all))
          area_sf <- saved_sf_all[saved_sf_all$health_area == k, , drop = FALSE]
        per_area[[k]] <- list(
          saved_team_sf     = area_sf,
          team_names        = (snap$team_names %||% list())[[k]],
          assignments_named = by_area[[k]],
          smoothed_team_sf  = smoothed_all[[k]]
        )
      }
      last_loaded_key(NULL)   # force reload of the currently selected area from cache
      showNotification('Team area state restored.', type = 'message', duration = 2)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    list(
      has_scene = reactive(!is.null(rv$grid_sf)),
      restore_from_snapshot = function(snap) {
        # Routed through restore_r() above for consistency with other tabs'
        # restore_from_snapshot(snap) call signature.
        if (!is.null(snap$current_team_assignments)) {
          by_area <- snap$current_team_assignments
          saved_sf_all <- snap$saved_team_sf
          smoothed_all <- snap$smoothed_team_sf %||% list()
          for (k in names(by_area)) {
            area_sf <- NULL
            if (!is.null(saved_sf_all) && 'health_area' %in% names(saved_sf_all))
              area_sf <- saved_sf_all[saved_sf_all$health_area == k, , drop = FALSE]
            per_area[[k]] <- list(
              saved_team_sf     = area_sf,
              team_names        = (snap$team_names %||% list())[[k]],
              assignments_named = by_area[[k]],
              smoothed_team_sf  = smoothed_all[[k]]
            )
          }
          last_loaded_key(NULL)
        }
      }
    )
  })
}

# Team-name equivalent of the health-area normalize_dfa_names() helper —
# duplicated locally to avoid depending on mod_health_area_tab.R's private
# closure of the same name.
normalize_dfa_names_team <- function(x) {
  x <- unique(as.character(x)); x <- x[!is.na(x) & nzchar(x)]
  c(setdiff(x, extra_dfa_names), extra_dfa_names)
}
