# =============================================================================
# mod_campaign_scope_tab.R
#
# Campaign Scope stage -- shown between Landmarks and Facilities ONLY for a
# district set to 'partial' in the admin page's Manage Districts modal (see
# mod_admin_tab_v2.R). Same paint-then-refine-with-vertex engine as Health
# Areas and Team Areas -- same paint-app.js canvas (via healthAreaMapServer,
# a thin generic wrapper, reused directly under this tab's own module id),
# same pending_action state machine for save/submit finalizing through a
# grid -> vertex conversion, same Refine Boundaries / smoothness / stiffness
# / snap-tolerance / cleanup mechanics as mod_team_area_tab.R -- this file
# mirrors that one's structure closely, not mod_health_area_tab.R's, since
# team areas are the simpler paint+refine case without facility-seeded BFS
# generation, wave-reveal animation, or a team-targets modal, none of which
# scope has any use for either.
#
# What scope genuinely doesn't need, versus either of those tabs:
#   - No facility-seeded initial generation (initialHealthAreaGenerationServer)
#     -- prefilled instead from urban-area data (real GIS boundary, or the
#     WorldPop density approximation as fallback -- both already built into
#     fetch_urban_areas_for_district()), a simple inside/outside-a-buffered-
#     polygon test, not a BFS propagation.
#   - No wave-reveal animation, no team-targets modal, no per-area rename.
#   - No independent "make current" / staleness handling -- scope is locked
#     into the SAME draft's lifecycle as landmarks/facilities (see
#     scope_locked_at in create_db_v2.R), not a separately-publishable track
#     the way team areas are.
# =============================================================================

IN_SCOPE_NAME  <- "In Scope"
OUT_SCOPE_NAME <- "Out of Scope"
.scope_dfa_names <- c(IN_SCOPE_NAME, OUT_SCOPE_NAME)

campaignScopeTabUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 3, campaignScopeControlsUI(ns('controls'))),
    column(width = 6, div(style = 'height: calc(100vh - 120px);', healthAreaMapUI(ns('map')))),
    column(width = 3,
           div(style = 'overflow-y: auto; height: calc(100vh - 120px);',
               healthAreaPopulationUI(ns('population'), name_col_label = "Scope", allow_rename = FALSE)))
  )
}

campaignScopeTabServer <- function(
    id,
    # MUST be full_district_sf, never planning_area_sf -- scope decides
    # what subset of the FULL district counts, so painting needs the
    # whole extent available to paint over. Passing planning_area_sf
    # here would be circular after server.R's own redefinition of it
    # (which now reads scope's OWN saved output).
    district_sf_r,
    urban_areas_r,
    active_campaign_id_r,
    active_tab,
    landmarks_r      = reactive(NULL),
    subdivisions_r   = reactive(NULL),
    # Whether this district is 'full' or 'partial' scope (see
    # mod_admin_tab_v2.R) -- needed here specifically to gate the
    # auto-submit-from-real-urban-data observer below; a 'full' district
    # never has this tab instantiated as relevant in the first place, but
    # this module doesn't otherwise know that on its own.
    active_mapping_scope_r = reactive('full'),
    submit_stage_fn  = NULL,
    restore_r        = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {

    controls <- campaignScopeControlsServer('controls')
    map_mod  <- healthAreaMapServer('map')
    active_scope_rv <- reactiveVal(IN_SCOPE_NAME)
    observeEvent(controls$active_scope_value(), {
      req(!is.null(controls$active_scope_value()))
      active_scope_rv(controls$active_scope_value())
    }, ignoreInit = TRUE)

    tab_active <- reactive({ identical(active_tab(), 'tab_campaign_scope') })

    # Same as mod_health_area_tab.R's identical reactive -- shown as
    # visual context/reference on the paint canvas while deciding
    # scope, same as it already is for health-area boundaries.
    subdivision_boundary_lines_r <- reactive({
      subs <- tryCatch(subdivisions_r(), error = function(e) NULL)
      subdivisions_to_boundary_lines(subs)
    })

    observeEvent(tab_active(), {
      if (!isTRUE(tab_active())) controls$reset_controls()
    }, ignoreInit = TRUE)

    observeEvent(controls$help_click(), { .show_scope_help_modal() })

    .show_scope_help_modal <- function() {
      showModal(modalDialog(
        title = tags$span(
          style = 'font-size: 15px; font-weight: 700; color: #0f172a;',
          'Campaign Scope -- How to use this tab'
        ),
        div(
          style = 'font-size: 13px; line-height: 1.7; color: #334155;',
          tags$p(
            'Paint which parts of this district are actually ', tags$strong('in scope'), ' for this ',
            'campaign. Pick "In Scope" or "Out of Scope" on the left, then left click/drag to paint, ',
            'right click/drag to pan -- same as Health Areas and Team Areas.'
          ),
          tags$p(
            style = 'margin-top: 10px;',
            tags$strong('Refine Boundaries'), ' switches to editable vertex points along the scope edge -- ',
            'drag individual points for precise adjustments, and use the Smoothness and Stiffness sliders ',
            'to control how closely the line follows them.'
          ),
          tags$p(
            style = 'margin-top: 10px;',
            'Save keeps your scope boundary locally. Submit Campaign Scope writes it to the database and ',
            'continues to Facilities -- everything left "Out of Scope" is excluded from facilities, health ',
            'areas, and every population figure downstream from here on.'
          )
        ),
        easyClose = TRUE, footer = modalButton('Close')
      ))
    }

    rv <- reactiveValues(
      district_sf = NULL, grid_sf = NULL, initial_assignments = NULL,
      current_assignments = NULL, saved_scope_sf = NULL,
      neighbors_list = NULL, edge_list = NULL,
      pop_overlay_sf = NULL, pop_table = NULL,
      smoothed_scope_sf = NULL,
      vertex_mode_silently_entered = FALSE,
      # The exact (non-griditized) buffered urban polygon this session's
      # prefill came from, if any -- NULL for a district with no urban
      # data at all (prefill defaults everything to Out of Scope, so
      # there's no exact source polygon to fall back to). Used at
      # save/submit time to bypass the grid/vertex approximation
      # entirely when nothing has actually been painted differently.
      prefill_buffered_urban_sf = NULL,
      prefill_missing_urban_at_build = FALSE
    )

    pending_action <- reactiveVal(NULL)
    in_vertex_mode <- reactiveVal(FALSE)

    send_paint_message <- function(type, payload = list()) {
      session$sendCustomMessage(type, c(list(
        mapId              = map_mod$map_id,
        loadingOverlayId   = map_mod$loading_overlay_id,
        readyInputId       = map_mod$ready_input_id,
        assignmentsInputId = map_mod$assignments_input_id,
        undoCountInputId   = map_mod$undo_count_input_id
      ), payload))
    }

    # Same highlighting convention as Health/Team Areas -- whichever value
    # is currently active to paint gets the "selected" color, the other
    # gets the neutral one. Neither "In Scope" nor "Out of Scope" is in
    # special_fill_colors (that's for Inaccessible/Unpopulated), so both
    # fall through to the plain active/non-active split.
    current_fill_colors <- reactive({
      make_fill_colors(active_dfa = active_scope_rv(), dfa_names = .scope_dfa_names)
    })

    healthAreaPopulationServer(
      'population',
      active_dfa_rv        = active_scope_rv,
      show_pop_raster      = controls$show_pop_raster,
      show_friction_raster = controls$show_friction_raster,
      pop_table            = reactive(rv$pop_table),
      in_vertex_mode       = in_vertex_mode
    )

    recompute_population_table <- function(assignments) {
      req(!is.null(rv$grid_sf), length(assignments) == nrow(rv$grid_sf))
      req('u5_pop' %in% names(rv$grid_sf))
      df <- data.frame(area_name = assignments, est_u5_pop = rv$grid_sf$u5_pop, stringsAsFactors = FALSE) |>
        dplyr::filter(!is.na(area_name), nzchar(area_name)) |>
        dplyr::group_by(area_name) |>
        dplyr::summarise(est_u5_pop = round(sum(est_u5_pop, na.rm = TRUE), 0), .groups = 'drop')
      missing <- setdiff(.scope_dfa_names, df$area_name)
      if (length(missing) > 0)
        df <- dplyr::bind_rows(df, data.frame(area_name = missing, est_u5_pop = 0, stringsAsFactors = FALSE))
      df <- df |>
        dplyr::mutate(area_name = factor(area_name, levels = .scope_dfa_names)) |>
        dplyr::arrange(area_name) |>
        dplyr::mutate(area_name = as.character(area_name))
      rv$pop_table <- dplyr::bind_rows(
        df,
        data.frame(area_name = 'District Total',
                  est_u5_pop = round(sum(rv$grid_sf$u5_pop, na.rm = TRUE), 0),
                  stringsAsFactors = FALSE)
      )
      invisible(NULL)
    }

    # ── Prefill: a cell's centroid falling inside the (buffered) urban-area
    # polygon starts "In Scope"; everything else starts "Out of Scope" --
    # the safer default when no urban data exists at all is to make the
    # mapper explicitly paint scope in, not accidentally default the whole
    # district to in-scope. Buffered by the same urban_buffer_km setting
    # the pre-Campaign-Scope planning_area_sf computation used, for the
    # same reason: a raw urban-area polygon is usually tighter than where
    # campaign activity actually needs to extend.
    #
    # Returns BOTH the per-cell assignment vector (for painting) AND the
    # exact buffered polygon itself (for saving without grid
    # approximation if the user never actually paints anything -- see
    # rv$prefill_buffered_urban_sf below and its use at save/submit
    # time). The buffer is the only intentional adjustment; griditizing
    # it onto the grid is fine for LIVE PAINTING (the canvas has to be
    # cell-based to be paintable/undo-able at all), but was being baked
    # into the SAVED result even when the user changed nothing, which
    # this specifically avoids.
    .prefill_assignments <- function(grid_sf, urban_sf, campaign_id) {
      if (is.null(urban_sf) || nrow(urban_sf) == 0) {
        return(list(assignments = rep(OUT_SCOPE_NAME, nrow(grid_sf)), buffered_urban_sf = NULL))
      }
      buffer_km <- tryCatch(db_get_generation_setting(pool, 'urban_buffer_km', campaign_id),
                            error = function(e) NA_real_)
      if (is.na(buffer_km)) buffer_km <- 5
      tryCatch({
        urban_3857 <- sf::st_transform(safe_make_valid(urban_sf), 3857)
        buffered   <- sf::st_buffer(sf::st_union(urban_3857), buffer_km * 1000)
        buffered_sf <- sf::st_sf(geometry = buffered, crs = 3857)
        cent_3857  <- sf::st_transform(sf::st_centroid(grid_sf), 3857)
        inside <- lengths(sf::st_within(cent_3857, buffered_sf)) > 0
        list(
          assignments       = ifelse(inside, IN_SCOPE_NAME, OUT_SCOPE_NAME),
          buffered_urban_sf = sf::st_transform(buffered_sf, 4326)
        )
      }, error = function(e) list(assignments = rep(OUT_SCOPE_NAME, nrow(grid_sf)), buffered_urban_sf = NULL))
    }

    # Builds the EXACT (non-griditized) saved-scope polygon from the
    # original buffered urban polygon, for the case the user submits
    # without ever painting a different cell -- clipped to the
    # district (scope logically can't extend past it, same reasoning
    # as the old planning_area_sf's own clip step before this feature
    # existed) with the district's remainder as "Out of Scope".
    .build_exact_scope_from_prefill <- function(district_sf, buffered_urban_sf) {
      tryCatch({
        district_3857 <- sf::st_transform(safe_make_valid(district_sf), 3857)
        buffered_3857 <- sf::st_transform(safe_make_valid(buffered_urban_sf), 3857)
        in_scope_geom <- suppressWarnings(sf::st_intersection(sf::st_geometry(district_3857), sf::st_geometry(buffered_3857)))
        out_scope_geom <- suppressWarnings(sf::st_difference(sf::st_geometry(district_3857), sf::st_geometry(buffered_3857)))
        rows <- list()
        if (length(in_scope_geom) > 0 && !all(sf::st_is_empty(in_scope_geom)))
          rows$in_scope <- sf::st_sf(dfa_name = IN_SCOPE_NAME, geometry = sf::st_union(in_scope_geom), crs = 3857)
        if (length(out_scope_geom) > 0 && !all(sf::st_is_empty(out_scope_geom)))
          rows$out_scope <- sf::st_sf(dfa_name = OUT_SCOPE_NAME, geometry = sf::st_union(out_scope_geom), crs = 3857)
        if (length(rows) == 0) return(NULL)
        combined <- do.call(rbind, rows)
        sf::st_transform(combined, 4326)
      }, error = function(e) NULL)
    }


    # Simple spatial fallback for edge-cell detection -- duplicated from
    # mod_initial_health_area_generation.R's build_edge_list()'s own
    # fallback path (that function is defined inside that module's
    # moduleServer() closure, not callable from outside it); this is the
    # same fallback path it itself uses when it doesn't have the fast
    # arithmetic grid metadata, not a new algorithm.
    .build_edge_list_simple <- function(grid_sf, district_sf) {
      grid_3857     <- sf::st_transform(grid_sf, 3857)
      district_3857 <- sf::st_transform(district_sf, 3857)
      cell_bbox   <- sf::st_bbox(grid_3857[1, ])
      edge_buffer <- max(cell_bbox["xmax"] - cell_bbox["xmin"],
                         cell_bbox["ymax"] - cell_bbox["ymin"]) * 0.05
      district_boundary_3857 <- sf::st_boundary(district_3857) |> sf::st_buffer(edge_buffer)
      edge_flag <- lengths(sf::st_intersects(grid_3857, district_boundary_3857)) > 0
      edge_list <- as.list(edge_flag)
      names(edge_list) <- as.character(grid_sf$cell_id)
      edge_list
    }

    last_scene_key  <- reactiveVal(NULL)
    pending_restore <- reactiveVal(NULL)
    restore_applied <- reactiveVal(FALSE)

    .apply_restore <- function(snap) {
      if (is.null(snap$scope_current_assignments) || is.null(rv$grid_sf)) return(invisible(NULL))
      rv$current_assignments <- vapply(as.character(rv$grid_sf$cell_id), function(id) {
        val <- snap$scope_current_assignments[[id]]
        if (is.null(val)) rv$initial_assignments[as.integer(id)] else as.character(val)
      }, character(1))
      rv$saved_scope_sf    <- snap$scope_saved_dfa_sf
      rv$smoothed_scope_sf <- snap$scope_saved_dfa_sf   # scope has no separate smoothed/presentation geometry
      restore_applied(TRUE)
      showNotification('Campaign scope draft restored.', type = 'message', duration = 2)
      invisible(NULL)
    }

    observeEvent(restore_r(), {
      snap <- restore_r()
      req(!is.null(snap))
      if (!is.null(rv$grid_sf)) .apply_restore(snap) else pending_restore(snap)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Builds the grid once per district (keyed by district_name, not
    # nrow -- two different single-row districts could share an nrow,
    # never a name). No facility-seeded scene() the way Health/Team Areas
    # have -- just make_paint_grid() + the simple urban-buffer prefill
    # above, since scope has no BFS propagation to run.
    observe({
      dsf <- district_sf_r()
      req(!is.null(dsf), nrow(dsf) > 0, "district_name" %in% names(dsf))
      isolate({
      if (isTRUE(restore_applied())) return()
      new_key <- as.character(dsf$district_name[[1]])
      if (identical(last_scene_key(), new_key)) return()
      last_scene_key(new_key)

      # make_paint_grid()'s OWN default cellsize formula has no upper
      # cap -- it only scales cellsize UP to keep total cell count under
      # max_cells (40000), with nothing stopping it from ballooning well
      # past a few hundred meters for a genuinely large district. That's
      # exactly what produced a handful of huge rectangular blocks
      # instead of a grid fine enough to roughly trace a subdivision
      # boundary (confirmed directly against a real district). Passing
      # an explicit grid_n, computed the SAME clamped way mod_health_
      # area_tab.R's own grid_n reactive already does (max_dim/160,
      # clamped to [100m, 500m] regardless of district size), bypasses
      # make_paint_grid()'s uncapped branch entirely and keeps this
      # grid's resolution consistent with the one Health Areas uses.
      max_dim <- calc_district_max_dim(dsf)
      target_cellsize <- max_dim / 160
      cellsize        <- max(100, min(500, target_cellsize))
      grid_n          <- max(10L, as.integer(round(max_dim / cellsize)))

      grid_info <- make_paint_grid(dsf, grid_n = grid_n)
      grid_sf <- grid_info$grid_sf
      grid_sf$u5_pop <- tryCatch(extract_cell_population(grid_sf, u5_rast), error = function(e) rep(0, nrow(grid_sf)))

      urban_now <- tryCatch(urban_areas_r(), error = function(e) NULL)
      prefill <- .prefill_assignments(grid_sf, urban_now, active_campaign_id_r())
      # Tracks whether THIS build had no urban data to work with, purely
      # for the catch-up observer below (urban_areas_r() involves a real
      # ArcGIS network round trip in server.R, on its OWN observer keyed
      # off active_district() -- there's no guarantee that fetch has
      # completed by the time this block runs, and reading it via
      # isolate() here means this block would never notice if it
      # finished late). NULL, not FALSE, specifically so the catch-up
      # observer can tell "never checked yet" apart from "checked and
      # confirmed empty" if that distinction ever matters later.
      rv$prefill_missing_urban_at_build <- is.null(urban_now) || nrow(urban_now) == 0

      rv$district_sf         <- dsf
      rv$grid_sf              <- grid_sf
      rv$neighbors_list        <- build_neighbors_list(grid_sf)
      rv$edge_list              <- .build_edge_list_simple(grid_sf, dsf)
      rv$initial_assignments     <- prefill$assignments
      rv$current_assignments      <- prefill$assignments
      rv$prefill_buffered_urban_sf  <- prefill$buffered_urban_sf
      rv$saved_scope_sf             <- NULL
      rv$smoothed_scope_sf           <- NULL

      pr <- pending_restore()
      if (!is.null(pr)) { .apply_restore(pr); pending_restore(NULL) }

      if (isTRUE(in_vertex_mode())) send_paint_message("paint_exit_vertex_mode")
      in_vertex_mode(FALSE)
      controls$set_vertex_mode_ui(FALSE)

      recompute_population_table(rv$current_assignments)
      if (tab_active()) send_current_scene()
      })
    })

    # Catch-up for the race above: if urban data genuinely wasn't ready
    # yet at grid-build time, but arrives afterward, re-run the prefill
    # against it now -- ONLY if the user hasn't painted anything since
    # (identical(current, initial) -- painting over their own edits
    # would be a real regression, not a fix) and isn't mid-refine.
    # Rebuilds just the prefill/assignments in place rather than the
    # whole grid, since the grid itself doesn't depend on urban data at
    # all.
    observeEvent(urban_areas_r(), {
      req(isTRUE(rv$prefill_missing_urban_at_build))
      req(!is.null(rv$grid_sf), !isTRUE(in_vertex_mode()))
      req(identical(rv$current_assignments, rv$initial_assignments))
      urban_now <- tryCatch(urban_areas_r(), error = function(e) NULL)
      req(!is.null(urban_now), nrow(urban_now) > 0)

      prefill <- .prefill_assignments(rv$grid_sf, urban_now, active_campaign_id_r())
      rv$initial_assignments    <- prefill$assignments
      rv$current_assignments     <- prefill$assignments
      rv$prefill_buffered_urban_sf <- prefill$buffered_urban_sf
      rv$prefill_missing_urban_at_build <- FALSE

      recompute_population_table(rv$current_assignments)
      if (tab_active()) send_current_scene()
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # ── Auto-submit when real (non-approximated) urban-area data exists ──
    # A district set to 'partial' with a REAL admin-configured urban-
    # area boundary (not the WorldPop density approximation, which is
    # a genuine guess and needs a human to review it) never needs a
    # human to paint or confirm anything -- the real boundary IS the
    # scope, directly. This fires silently, independent of which tab
    # the user is actually viewing (most likely they're still on
    # Landmarks, since Campaign Scope comes right after it) -- it is
    # NOT gated on tab_active() the way everything else in this module
    # is. mod_orientation_tab.R's own Continue button is what actually
    # skips PAST this tab when scope is auto-handled (checking the same
    # active_mapping_scope_r + urban_source_is_real signal from
    # server.R) -- this observer's only job is to make sure the real
    # data has actually been written to the DB (and is live via
    # submitted_scope_sf() below) by the time that skip happens.
    observeEvent(list(district_sf_r(), urban_areas_r(), active_mapping_scope_r()), {
      req(identical(active_mapping_scope_r(), 'partial'))
      req(!is.null(rv$district_sf), !is.null(rv$grid_sf))
      urban_now <- tryCatch(urban_areas_r(), error = function(e) NULL)
      req(!is.null(urban_now), nrow(urban_now) > 0, identical(attr(urban_now, 'urban_source'), 'real'))

      # Don't re-auto-submit if scope already has real data -- either
      # from a previous session (restore_r()'s own snapshot) or from
      # THIS session's own already-completed auto-submit.
      already_has_scope <- !is.null(submitted_scope_sf())
      if (!already_has_scope) {
        snap <- tryCatch(restore_r(), error = function(e) NULL)
        already_has_scope <- !is.null(snap$scope_saved_dfa_sf) && nrow(snap$scope_saved_dfa_sf) > 0
      }
      req(!already_has_scope)

      prefill <- .prefill_assignments(rv$grid_sf, urban_now, active_campaign_id_r())
      req(!is.null(prefill$buffered_urban_sf))
      exact_sf <- .build_exact_scope_from_prefill(rv$district_sf, prefill$buffered_urban_sf)
      req(!is.null(exact_sf))

      if (!is.null(submit_stage_fn)) {
        submit_stage_fn('scope', list(
          scope_saved_dfa_sf         = exact_sf,
          scope_dfa_names            = .scope_dfa_names,
          scope_current_assignments  = setNames(as.list(prefill$assignments), as.character(rv$grid_sf$cell_id))
        ))
        submitted_scope_sf(exact_sf)
        # Deliberately NOT incrementing submitted_counter() here -- that
        # drives server.R's auto-advance-to-Facilities navigation, which
        # is correct for a real user clicking Submit from this tab, but
        # would be wrong here: forcibly yanking the user to Facilities
        # the instant a district loads, regardless of which tab they're
        # actually on, would be a real regression, not a convenience.
      }
    }, ignoreInit = TRUE)

    scene_ever_sent       <- reactiveVal(FALSE)
    scene_load_confirmed  <- reactiveVal(FALSE)

    send_current_scene <- function() {
      req(tab_active())
      req(!is.null(rv$district_sf), !is.null(rv$grid_sf), !is.null(rv$current_assignments))
      scene_ever_sent(TRUE)
      scene_load_confirmed(FALSE)

      send_paint_message("paint_exit_vertex_mode")
      if (isTRUE(in_vertex_mode())) {
        in_vertex_mode(FALSE)
        controls$set_vertex_mode_ui(FALSE)
      }

      init_named <- setNames(as.list(rv$current_assignments), as.character(rv$grid_sf$cell_id))
      saved_sf <- rv$saved_scope_sf
      if (is.null(saved_sf))
        saved_sf <- build_saved_dfa_sf(grid_sf = rv$grid_sf, assignments = rv$current_assignments,
                                       district_sf = rv$district_sf)

      landmark_pts <- list()
      lm_df <- tryCatch(landmarks_r(), error = function(e) NULL)
      if (!is.null(lm_df) && nrow(lm_df) > 0)
        landmark_pts <- lapply(seq_len(nrow(lm_df)), function(i)
          list(lat = lm_df$lat[i], lon = lm_df$lon[i], name = lm_df$landmark_name[i]))

      # Subdivisions_r() already returns NULL for rural planning units,
      # so subdivision_boundary_lines_r() will be NULL too -- same
      # pattern as mod_health_area_tab.R's identical construction.
      subdiv_geojson <- tryCatch({
        bl <- subdivision_boundary_lines_r()
        if (!is.null(bl) && nrow(bl) > 0) as_geojson_text(bl) else NULL
      }, error = function(e) NULL)

      send_paint_message('show_loading')
      send_paint_message('paint_load_scene', list(
        districtGeojson      = as_geojson_text(rv$district_sf),
        gridGeojson          = as_geojson_text(rv$grid_sf),
        initialAssignments   = init_named,
        dfaColors            = as.list(current_fill_colors()),
        activeDfa            = active_scope_rv(),
        neighbors            = rv$neighbors_list,
        edgeCells            = rv$edge_list,
        brushSize            = (controls$brush_m() %||% 2000) / 2,
        boundaryOnly         = controls$boundary_only(),
        subdivisionGeojson   = subdiv_geojson,
        landmarkPoints       = landmark_pts,
        savedGeojson         = as_geojson_text(saved_sf)
      ))
    }

    observeEvent(active_tab(), {
      if (tab_active() && !is.null(rv$grid_sf)) send_current_scene()
    }, ignoreInit = TRUE)

    # Gated on scene_ever_sent() to avoid a resend ping-pong -- same
    # reasoning as mod_team_area_tab.R's identical wiring: map_ready()
    # fires every time paint-app.js finishes loadScene(), not just once,
    # so this only ever catches up a scene that never successfully sent
    # in the first place.
    observeEvent(map_mod$map_ready(), {
      if (tab_active()) send_paint_message('hide_loading')
      scene_load_confirmed(TRUE)
      if (tab_active() && !is.null(rv$grid_sf) && !isTRUE(scene_ever_sent())) send_current_scene()
    }, ignoreInit = TRUE)

    observeEvent(map_mod$undo_count(), {
      shinyjs::toggleState(session$ns('controls-paint_undo_btn'),
                           condition = isTRUE(map_mod$undo_count() > 0))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    observeEvent(map_mod$vertex_undo_count(), {
      shinyjs::toggleState(session$ns('controls-refine_undo_btn'),
                           condition = isTRUE(map_mod$vertex_undo_count() > 0))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # ── Controls wiring ───────────────────────────────────────────────────

    observeEvent(controls$brush_m(), {
      req(tab_active()); send_paint_message('paint_set_brush', list(value = controls$brush_m() / 2))
    }, ignoreInit = TRUE)

    observeEvent(controls$boundary_only(), {
      req(tab_active()); send_paint_message('paint_set_boundary_only', list(value = controls$boundary_only()))
    }, ignoreInit = TRUE)

    observeEvent(controls$show_pop_raster(), {
      req(tab_active(), !is.null(rv$district_sf))
      if (isTRUE(controls$show_pop_raster()) && is.null(rv$pop_overlay_sf)) {
        rv$pop_overlay_sf <- tryCatch(
          make_population_overlay_sf(district_sf = rv$district_sf, u5_rast = u5_rast),
          error = function(e) NULL)
      }
      geojson <- if (!is.null(rv$pop_overlay_sf)) as_geojson_text(rv$pop_overlay_sf) else NULL
      send_paint_message("paint_toggle_population",
                         list(show = controls$show_pop_raster(), geojson = geojson))
    }, ignoreInit = TRUE)

    observeEvent(controls$paint_undo_click(), {
      req(tab_active(), !isTRUE(in_vertex_mode())); send_paint_message('paint_undo')
      pending_action('refresh'); send_paint_message('paint_request_assignments')
    }, ignoreInit = TRUE)

    observeEvent(active_scope_rv(), {
      req(tab_active(), !is.null(rv$current_assignments))
      send_paint_message('paint_set_colors', list(colors = as.list(current_fill_colors()), activeDfa = active_scope_rv()))
      pending_action('capture'); send_paint_message('paint_request_assignments')
    }, ignoreInit = TRUE)

    observeEvent(controls$reset_click(), {
      req(tab_active(), !isTRUE(in_vertex_mode()), !is.null(rv$initial_assignments))
      rv$current_assignments <- rv$initial_assignments
      rv$saved_scope_sf       <- NULL
      send_paint_message('paint_reset')
      send_paint_message('paint_set_colors', list(colors = as.list(current_fill_colors()), activeDfa = active_scope_rv()))
      recompute_population_table(rv$initial_assignments)
    }, ignoreInit = TRUE)

    observeEvent(controls$save_click(), {
      req(tab_active(), !isTRUE(in_vertex_mode())); pending_action('save'); send_paint_message('paint_request_assignments')
    }, ignoreInit = TRUE)

    observeEvent(controls$submit_click(), {
      req(tab_active())
      pending_action('submit'); send_paint_message('paint_request_assignments')
    }, ignoreInit = TRUE)

    # ── Refine boundaries (vertex editing) ─────────────────────────────────
    observeEvent(controls$refine_boundaries_click(), {
      req(tab_active())
      if (isTRUE(in_vertex_mode())) {
        send_paint_message("paint_exit_vertex_mode")
        in_vertex_mode(FALSE)
      } else {
        req(!is.null(rv$grid_sf))
        if (!isTRUE(scene_load_confirmed())) {
          showNotification('Boundaries are still loading -- try Refine Boundaries again in a moment.',
                           type = 'warning', duration = 4)
          return()
        }
        send_paint_message("paint_enter_vertex_mode", list(
          smoothness = controls$vertex_smoothness(),
          stiffness  = controls$vertex_stiffness(),
          snapTolerance = controls$vertex_snap_tolerance()
        ))
        in_vertex_mode(TRUE)
      }
      controls$set_vertex_mode_ui(in_vertex_mode())
    }, ignoreInit = TRUE)

    observeEvent(controls$vertex_smoothness(), {
      req(isTRUE(in_vertex_mode()))
      send_paint_message("paint_set_vertex_smoothness", list(value = controls$vertex_smoothness()))
    }, ignoreInit = TRUE)

    observeEvent(controls$vertex_stiffness(), {
      req(isTRUE(in_vertex_mode()))
      send_paint_message("paint_set_vertex_stiffness", list(value = controls$vertex_stiffness()))
    }, ignoreInit = TRUE)

    observeEvent(controls$vertex_snap_tolerance(), {
      req(isTRUE(in_vertex_mode()))
      send_paint_message("paint_set_vertex_snap_tolerance", list(value = controls$vertex_snap_tolerance()))
    }, ignoreInit = TRUE)

    observeEvent(controls$save_refinements_click(), {
      req(isTRUE(in_vertex_mode()))
      pending_action("manual_refine_save")
      send_paint_message("paint_save_vertex_edits")
      send_paint_message("paint_request_vertex_geojson")
    }, ignoreInit = TRUE)

    observeEvent(controls$refine_undo_click(), {
      req(isTRUE(in_vertex_mode()))
      send_paint_message("paint_vertex_undo")
    }, ignoreInit = TRUE)

    observeEvent(controls$refine_reset_click(), {
      req(isTRUE(in_vertex_mode()))
      send_paint_message("paint_reset_vertex_edits")
    }, ignoreInit = TRUE)

    observeEvent(controls$cleanup_boundaries_click(), {
      req(isTRUE(in_vertex_mode()))
      send_paint_message("paint_run_cleanup")
    }, ignoreInit = TRUE)

    observeEvent(map_mod$cleanup_result(), {
      r <- map_mod$cleanup_result()
      req(!is.null(r))
      parts <- c(
        if (isTRUE(r$totalPinched > 0)) sprintf('%d thin peninsula(s) pinched off', r$totalPinched),
        if (isTRUE(r$totalAbsorbed > 0)) sprintf('%d small area(s)/island(s) absorbed into a neighbor', r$totalAbsorbed),
        if (isTRUE(r$totalGaps > 0)) sprintf('%d gap/sliver point(s) closed', r$totalGaps),
        if (isTRUE(r$overlapResolved > 0)) sprintf('%d overlap(s) resolved', r$overlapResolved)
      )
      if (length(parts) == 0 && !isTRUE(r$overlapRolledBack)) {
        showNotification('Clean up: no holes, slivers, overlaps, or thin peninsulas found.', type = 'message', duration = 3)
      } else if (length(parts) > 0) {
        showNotification(paste0('Clean up: ', paste(parts, collapse = ', '), '.'), type = 'message', duration = 4)
      }
      if (isTRUE(r$overlapRolledBack)) {
        showNotification(
          sprintf('Clean up: %d overlap(s) detected but not automatically repaired -- the attempt didn\'t fully resolve them, so it was rolled back. Manual adjustment may be needed.', r$overlapRemaining %||% 0),
          type = 'warning', duration = 8
        )
      }
    }, ignoreInit = TRUE)

    submitted_counter <- reactiveVal(0L)
    # The live source of truth for "what scope was just submitted THIS
    # session" -- health_area_session$restore_snapshot() (server.R's
    # other option) is a snapshot taken once at session ACTIVATION time
    # and never refreshes automatically after a later submit_stage_fn()
    # call writes new data to the DB. Confirmed directly as the actual
    # bug: facility selection kept showing the full district after
    # submitting a smaller scope, because restore_snapshot() still held
    # the pre-submission (NULL scope) snapshot for the rest of the
    # session. server.R's planning_area_sf checks this reactive FIRST,
    # falling back to restore_snapshot() only for the case this doesn't
    # cover -- resuming a draft where scope was already submitted in a
    # PREVIOUS session, before this one's activation snapshot was taken.
    submitted_scope_sf <- reactiveVal(NULL)

    # Same three-reason pending_action dispatch as mod_team_area_tab.R's
    # identical wiring:
    #   "manual_refine_save" -- user explicitly clicked Save Refinements.
    #   "finalize_save"      -- Save was clicked; always runs the current
    #                           boundary through vertex conversion as part
    #                           of finishing, whether or not Refine
    #                           Boundaries was ever opened.
    #   "finalize_submit"    -- same, then writes it via submit_stage_fn.
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
        rv$saved_scope_sf    <- parsed
        rv$smoothed_scope_sf <- parsed
        send_paint_message('paint_show_saved', list(geojson = as_geojson_text(parsed)))
        showNotification("Boundary refinements saved.", type = "message", duration = 2)
        pending_action(NULL)
        return()
      }

      if (identical(act, "finalize_save") || identical(act, "finalize_submit")) {
        rv$smoothed_scope_sf <- parsed

        if (isTRUE(rv$vertex_mode_silently_entered)) {
          send_paint_message("paint_exit_vertex_mode")
          rv$vertex_mode_silently_entered <- FALSE
        }

        .update_current_scope(rv$current_assignments, saved_sf_override = parsed)
        recompute_population_table(rv$current_assignments)

        if (identical(act, "finalize_save")) {
          showNotification('Campaign scope saved.', type = 'message', duration = 2)
          pending_action(NULL)
          return()
        }

        if (!is.null(rv$saved_scope_sf) && !is.null(submit_stage_fn)) {
          submit_stage_fn('scope', list(
            scope_saved_dfa_sf         = rv$saved_scope_sf,
            scope_dfa_names            = .scope_dfa_names,
            scope_current_assignments  = setNames(as.list(rv$current_assignments), as.character(rv$grid_sf$cell_id))
          ))
          showNotification('Campaign scope submitted.', type = 'message', duration = 2)
          submitted_scope_sf(rv$saved_scope_sf)
          submitted_counter(submitted_counter() + 1L)
        }
        pending_action(NULL)
        return()
      }
    }, ignoreInit = TRUE)

    .update_current_scope <- function(ordered_assignments, saved_sf_override = NULL) {
      saved <- if (!is.null(saved_sf_override)) {
        saved_sf_override
      } else {
        tryCatch(
          build_saved_dfa_sf(grid_sf = rv$grid_sf, assignments = ordered_assignments, district_sf = rv$district_sf),
          error = function(e) NULL
        )
      }
      if (is.null(saved)) return(invisible(NULL))
      rv$saved_scope_sf <- saved
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
      if (length(setdiff(unique(known), .scope_dfa_names)) > 0) { pending_action(NULL); return() }

      rv$current_assignments <- ordered
      act <- pending_action()

      if (identical(act, 'capture') || identical(act, 'refresh')) {
        recompute_population_table(ordered)
        pending_action(NULL); return()
      }

      # Save and Submit both always finish by running the current scope
      # boundary through grid -> vertex conversion before anything is
      # cached or written to the DB -- see the vertex_geojson observer
      # above for where finalize_save/finalize_submit actually complete.
      if (identical(act, 'save') || identical(act, 'submit')) {
        # EXCEPT when nothing has actually been painted differently from
        # the original prefill and that prefill came from real urban-
        # area data -- in that specific case, use the EXACT buffered
        # polygon directly instead of ever routing it through the grid
        # at all (see rv$prefill_buffered_urban_sf's own comment for
        # why). The moment the user paints even one cell differently,
        # grid-based editing is unavoidable and the normal vertex-
        # conversion path below applies exactly as it did before this.
        if (identical(ordered, rv$initial_assignments) && !is.null(rv$prefill_buffered_urban_sf)) {
          exact_sf <- .build_exact_scope_from_prefill(rv$district_sf, rv$prefill_buffered_urban_sf)
          if (!is.null(exact_sf)) {
            rv$saved_scope_sf    <- exact_sf
            rv$smoothed_scope_sf <- exact_sf
            send_paint_message('paint_show_saved', list(geojson = as_geojson_text(exact_sf)))
            recompute_population_table(rv$current_assignments)
            if (identical(act, 'save')) {
              showNotification('Campaign scope saved.', type = 'message', duration = 2)
            } else if (!is.null(submit_stage_fn)) {
              submit_stage_fn('scope', list(
                scope_saved_dfa_sf         = exact_sf,
                scope_dfa_names            = .scope_dfa_names,
                scope_current_assignments  = setNames(as.list(rv$current_assignments), as.character(rv$grid_sf$cell_id))
              ))
              showNotification('Campaign scope submitted.', type = 'message', duration = 2)
              submitted_scope_sf(exact_sf)
              submitted_counter(submitted_counter() + 1L)
            }
            pending_action(NULL)
            return()
          }
          # Falls through to the normal grid/vertex path below if
          # building the exact polygon failed for any reason -- an
          # untouched scope still needs to save as SOMETHING rather
          # than silently do nothing.
        }

        pending_action(if (identical(act, 'save')) 'finalize_save' else 'finalize_submit')
        if (!isTRUE(in_vertex_mode())) {
          rv$vertex_mode_silently_entered <- TRUE
          send_paint_message("paint_enter_vertex_mode", list(
            smoothness = isolate(controls$vertex_smoothness()) %||% 2,
            stiffness  = isolate(controls$vertex_stiffness()) %||% 6,
            snapTolerance = isolate(controls$vertex_snap_tolerance()) %||% 20
          ))
        }
        send_paint_message("paint_save_vertex_edits")
        send_paint_message("paint_request_vertex_geojson")
        return()
      }

      pending_action(NULL)
    }, ignoreInit = TRUE)

    list(
      has_scene = reactive(!is.null(rv$grid_sf)),
      # server.R advances past this stage (to Facilities) whenever this
      # counter increments -- a counter, not a bare reactive(input$...),
      # so it can be incremented ourselves right after a successful
      # finalize_submit above (which happens asynchronously, after the
      # vertex-geojson round trip completes, not synchronously with the
      # Submit click itself).
      submitted = reactive(submitted_counter()),
      submitted_scope_sf = reactive(submitted_scope_sf())
    )
  })
}
