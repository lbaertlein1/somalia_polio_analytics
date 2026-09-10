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
# THE CANVAS ITSELF, not just the prefill: campaign_extent_r() (the admin-
# configured "campaign extent" layer, fetch_campaign_extent_for_district()
# in subdivision_helpers.R) is the OUTER boundary scope painting is allowed
# to exist within -- not a hint painted onto a full-district canvas. When
# an extent exists for this district, the grid is built over that extent
# CLIPPED to the district (never larger than the district even if the
# extent itself extends past it), sized to the extent's own dimensions
# (not the district's -- a small extent inside a large district gets a
# fine grid, not one coarsened by the district's own size), and every
# cell starts "In Scope" -- the extent IS the intended campaign area, the
# user paints OUT anything to exclude. When NO extent exists for this
# district, the canvas falls back to the whole district, sized to the
# district's own dimensions, and every cell starts "Out of Scope" --
# genuinely blank, painted in from nothing. There is no longer any
# computed approximation (the WorldPop density approximation this used to
# fall back to was removed) -- an admin either provides a real extent or
# doesn't, and the tab behaves accordingly either way.
#
# What scope genuinely doesn't need, versus either of those tabs:
#   - No facility-seeded initial generation (initialHealthAreaGenerationServer)
#     -- the canvas defaults uniformly (see above), not a BFS propagation.
#   - No wave-reveal animation, no team-targets modal, no per-area rename.
#   - No independent "make current" / staleness handling -- scope is locked
#     into the SAME draft's lifecycle as landmarks/facilities (see
#     scope_locked_at in create_db_v2.R), not a separately-publishable track
#     the way team areas are.
#   - No auto-submit / auto-skip -- even when a real campaign extent exists,
#     this tab is always reachable for a 'partial' district; the extent is
#     a starting point for the user to refine within, never a substitute
#     for their review (see mod_orientation_tab.R's .do_continue_to_facilities()).
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
    campaign_extent_r,
    active_tab,
    landmarks_r      = reactive(NULL),
    subdivisions_r   = reactive(NULL),
    # Context-only overlays, shown the same way subdivisions_r already
    # is -- see server.R's settlement_extents_rv / idp_context_rv.
    settlement_extents_r = reactive(NULL),
    idp_sf_r             = reactive(NULL),
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
            'If a campaign extent has been configured for this district, the map only shows that extent ',
            '(clipped to the district) -- scope can never spread past it. Everything starts "In Scope" in ',
            'that case; paint out anything to exclude. With no extent configured, the whole district is ',
            'shown instead, starting entirely "Out of Scope" -- paint in what belongs.'
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
      district_sf = NULL, canvas_sf = NULL, has_campaign_extent = FALSE,
      grid_sf = NULL, initial_assignments = NULL,
      current_assignments = NULL, saved_scope_sf = NULL,
      neighbors_list = NULL, edge_list = NULL,
      pop_overlay_sf = NULL, pop_table = NULL,
      smoothed_scope_sf = NULL,
      vertex_mode_silently_entered = FALSE,
      extent_missing_at_build = FALSE
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

    # ── Canvas determination: campaign extent (clipped to the district),
    # or the whole district when no extent exists. Returns NULL if
    # building the canvas fails outright (defensive -- e.g. a genuinely
    # invalid extent geometry) so the caller can decide how to fall back.
    #
    # An extent that doesn't actually overlap the district at all (a
    # defensive edge case -- e.g. a stale/wrong URL configured for this
    # campaign) is treated the SAME as no extent: falls back to the
    # whole district, rather than silently producing an empty, unusable
    # canvas.
    .determine_canvas <- function(district_sf, extent_sf) {
      if (is.null(extent_sf) || nrow(extent_sf) == 0) {
        return(list(canvas_sf = district_sf, has_extent = FALSE))
      }
      tryCatch({
        district_3857 <- sf::st_transform(safe_make_valid(district_sf), 3857)
        extent_3857   <- sf::st_transform(safe_make_valid(extent_sf), 3857)
        clipped <- suppressWarnings(sf::st_intersection(sf::st_geometry(district_3857), sf::st_union(sf::st_geometry(extent_3857))))
        if (length(clipped) == 0 || all(sf::st_is_empty(clipped))) {
          return(list(canvas_sf = district_sf, has_extent = FALSE))
        }
        canvas <- sf::st_sf(district_name = district_sf$district_name[[1]], geometry = sf::st_union(clipped), crs = 3857)
        canvas <- sf::st_transform(canvas, 4326)
        list(canvas_sf = canvas, has_extent = TRUE)
      }, error = function(e) list(canvas_sf = district_sf, has_extent = FALSE))
    }

    # Builds the EXACT (non-griditized) saved-scope polygon for the case
    # the user submits without ever painting a different cell --
    # canvas_sf itself tagged with whatever value every cell defaulted
    # to, plus (only when an extent was used, and only if the extent
    # doesn't already cover the whole district) the district's own
    # remainder outside the canvas, always tagged Out of Scope -- that
    # area was never even part of the paintable canvas to begin with,
    # so it was never a candidate for "In Scope" regardless of what
    # anyone paints inside the canvas itself.
    .build_exact_untouched_scope <- function(district_sf, canvas_sf, default_dfa_name) {
      tryCatch({
        district_3857 <- sf::st_transform(safe_make_valid(district_sf), 3857)
        canvas_3857   <- sf::st_transform(safe_make_valid(canvas_sf), 3857)
        rows <- list(
          canvas = sf::st_sf(dfa_name = default_dfa_name, geometry = sf::st_union(sf::st_geometry(canvas_3857)), crs = 3857)
        )
        if (!identical(default_dfa_name, OUT_SCOPE_NAME)) {
          remainder <- suppressWarnings(sf::st_difference(sf::st_geometry(district_3857), sf::st_geometry(canvas_3857)))
          if (length(remainder) > 0 && !all(sf::st_is_empty(remainder))) {
            rows$remainder <- sf::st_sf(dfa_name = OUT_SCOPE_NAME, geometry = sf::st_union(remainder), crs = 3857)
          }
        }
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

    # Builds the grid once per (district, extent-availability) combination
    # -- keyed by district_name plus whether an extent was actually used,
    # not district_name alone, since the catch-up logic below can rebuild
    # the WHOLE canvas (not just reassign cells) if a campaign extent
    # arrives late. No facility-seeded scene() the way Health/Team Areas
    # have -- just make_paint_grid() over whatever canvas_sf resolves to,
    # since scope has no BFS propagation to run.
    .build_scene_for_district <- function(dsf) {
      extent_now <- tryCatch(campaign_extent_r(), error = function(e) NULL)
      canvas_info <- .determine_canvas(dsf, extent_now)
      canvas_sf  <- canvas_info$canvas_sf
      has_extent <- canvas_info$has_extent

      # make_paint_grid()'s OWN default cellsize formula has no upper
      # cap -- it only scales cellsize UP to keep total cell count under
      # max_cells (40000), with nothing stopping it from ballooning well
      # past a few hundred meters for a genuinely large canvas. That's
      # exactly what produced a handful of huge rectangular blocks
      # instead of a grid fine enough to roughly trace a real boundary
      # (confirmed directly against a real district). Passing an
      # explicit grid_n, computed the SAME clamped way mod_health_area_
      # tab.R's own grid_n reactive already does (max_dim/160, clamped
      # to [100m, 500m]), bypasses make_paint_grid()'s uncapped branch
      # entirely -- and computed from canvas_sf's OWN dimensions, not
      # the district's, so a small extent inside a large district gets
      # a fine grid rather than one coarsened by the district's size.
      max_dim <- calc_district_max_dim(canvas_sf)
      target_cellsize <- max_dim / 160
      cellsize        <- max(100, min(500, target_cellsize))
      grid_n          <- max(10L, as.integer(round(max_dim / cellsize)))

      grid_info <- make_paint_grid(canvas_sf, grid_n = grid_n)
      grid_sf <- grid_info$grid_sf
      grid_sf$u5_pop <- tryCatch(extract_cell_population(grid_sf, u5_rast), error = function(e) rep(0, nrow(grid_sf)))

      # Every cell starts In Scope when a real extent was used (the
      # extent IS the intended campaign area -- paint OUT to exclude);
      # every cell starts Out of Scope with no extent at all (genuinely
      # blank -- paint IN what belongs). No per-cell test against the
      # source polygon the way the old urban-buffer prefill needed --
      # canvas_sf already IS exactly the extent (clipped to district),
      # so there's nothing left to test cell-by-cell.
      default_value <- if (has_extent) IN_SCOPE_NAME else OUT_SCOPE_NAME
      default_assignments <- rep(default_value, nrow(grid_sf))

      rv$district_sf          <- dsf
      rv$canvas_sf              <- canvas_sf
      rv$has_campaign_extent      <- has_extent
      rv$grid_sf                    <- grid_sf
      rv$neighbors_list                <- build_neighbors_list(grid_sf)
      rv$edge_list                        <- .build_edge_list_simple(grid_sf, canvas_sf)
      rv$initial_assignments                 <- default_assignments
      rv$current_assignments                    <- default_assignments
      rv$saved_scope_sf                            <- NULL
      rv$smoothed_scope_sf                            <- NULL
      # Tracks whether THIS build had no extent to work with, purely for
      # the catch-up observer below (campaign_extent_r() involves a real
      # ArcGIS network round trip in server.R, on its OWN observer keyed
      # off active_district()/active_campaign_id() -- there's no
      # guarantee that fetch has completed by the time this runs).
      rv$extent_missing_at_build <- !has_extent

      if (isTRUE(in_vertex_mode())) send_paint_message("paint_exit_vertex_mode")
      in_vertex_mode(FALSE)
      controls$set_vertex_mode_ui(FALSE)

      recompute_population_table(rv$current_assignments)
      invisible(NULL)
    }

    observe({
      dsf <- district_sf_r()
      req(!is.null(dsf), nrow(dsf) > 0, "district_name" %in% names(dsf))
      isolate({
      if (isTRUE(restore_applied())) return()
      new_key <- as.character(dsf$district_name[[1]])
      if (identical(last_scene_key(), new_key)) return()
      last_scene_key(new_key)

      .build_scene_for_district(dsf)

      pr <- pending_restore()
      if (!is.null(pr)) { .apply_restore(pr); pending_restore(NULL) }

      if (tab_active()) send_current_scene()
      })
    })

    # Catch-up for the race above: if a campaign extent genuinely wasn't
    # ready yet at grid-build time, but arrives afterward, rebuild the
    # WHOLE canvas against it now -- ONLY if the user hasn't painted
    # anything since (identical(current, initial) -- painting over their
    # own edits would be a real regression, not a fix) and isn't mid-
    # refine. A full rebuild, not just reassigning cells in place, since
    # the extent arriving late changes the canvas SHAPE itself (grid
    # cells, neighbors, edge list all need to change too), not just
    # which value each already-existing cell starts at.
    observeEvent(campaign_extent_r(), {
      req(isTRUE(rv$extent_missing_at_build))
      req(!is.null(rv$district_sf), !isTRUE(in_vertex_mode()))
      req(identical(rv$current_assignments, rv$initial_assignments))
      extent_now <- tryCatch(campaign_extent_r(), error = function(e) NULL)
      req(!is.null(extent_now), nrow(extent_now) > 0)

      .build_scene_for_district(rv$district_sf)
      if (tab_active()) send_current_scene()
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    scene_ever_sent       <- reactiveVal(FALSE)
    scene_load_confirmed  <- reactiveVal(FALSE)

    send_current_scene <- function() {
      req(tab_active())
      req(!is.null(rv$canvas_sf), !is.null(rv$grid_sf), !is.null(rv$current_assignments))
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
                                       district_sf = rv$canvas_sf)

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

      settlement_extents_geojson <- tryCatch({
        se <- settlement_extents_r()
        if (!is.null(se) && nrow(se) > 0) as_geojson_text(se) else NULL
      }, error = function(e) NULL)

      idp_pts <- tryCatch(idp_sf_to_points_list(idp_sf_r()), error = function(e) list())

      send_paint_message('show_loading')
      send_paint_message('paint_load_scene', list(
        # canvas_sf, not rv$district_sf -- the paintable/outer boundary
        # IS the (district-clipped) campaign extent when one exists, per
        # "scope is not allowed to spread outside of extent". Falls back
        # to the whole district when no extent exists, since canvas_sf
        # equals district_sf in that case (see .determine_canvas()).
        districtGeojson      = as_geojson_text(rv$canvas_sf),
        gridGeojson          = as_geojson_text(rv$grid_sf),
        initialAssignments   = init_named,
        dfaColors            = as.list(current_fill_colors()),
        activeDfa            = active_scope_rv(),
        neighbors            = rv$neighbors_list,
        edgeCells            = rv$edge_list,
        brushSize            = (controls$brush_m() %||% 2000) / 2,
        boundaryOnly         = controls$boundary_only(),
        subdivisionGeojson   = subdiv_geojson,
        settlementExtentsGeojson = settlement_extents_geojson,
        landmarkPoints       = landmark_pts,
        idpPoints            = idp_pts,
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

    # Catch-up for settlement_extents_r()/idp_sf_r() arriving AFTER the
    # initial scene already sent -- each is its own independent ArcGIS
    # fetch in server.R (settlement_extents_rv / idp_context_rv), with
    # no guaranteed ordering against this tab's own grid-build. Gated
    # on scene_ever_sent() -- if the scene hasn't sent yet, the
    # upcoming send_current_scene() call will already pick up whatever
    # these currently resolve to, so no separate update is needed. A
    # LIGHTWEIGHT context-layer-only message, not a full resend (which
    # would reset in-progress paint state) -- see paint-app.js's
    # updateContextLayers for the JS side.
    observeEvent(list(settlement_extents_r(), idp_sf_r()), {
      req(isTRUE(scene_ever_sent()), tab_active())
      settlement_extents_geojson <- tryCatch({
        se <- settlement_extents_r()
        if (!is.null(se) && nrow(se) > 0) as_geojson_text(se) else NULL
      }, error = function(e) NULL)
      idp_pts <- tryCatch(idp_sf_to_points_list(idp_sf_r()), error = function(e) list())
      send_paint_message('paint_update_context_layers', list(
        settlementExtentsGeojson = settlement_extents_geojson,
        idpPoints                = idp_pts
      ))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

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
      req(tab_active(), !is.null(rv$canvas_sf))
      if (isTRUE(controls$show_pop_raster()) && is.null(rv$pop_overlay_sf)) {
        rv$pop_overlay_sf <- tryCatch(
          make_population_overlay_sf(district_sf = rv$canvas_sf, u5_rast = u5_rast),
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
      # A syntactically valid but EMPTY FeatureCollection (0 rows) parses
      # without error, but geojsonsf::sf_geojson() on an empty sf object
      # can return a length-0 result instead of a valid (empty) GeoJSON
      # string -- which then crashes jsonlite::toJSON() inside
      # send_paint_message() with "values must be length 1, but FUN(...)
      # result is length 0" (confirmed from an actual production crash,
      # not hypothetical). Catching it here, before it ever reaches
      # as_geojson_text()/send_paint_message(), turns a hard crash into a
      # clear notification and an abandoned save instead.
      if (is.null(parsed) || nrow(parsed) == 0) {
        showNotification("The edited boundary came back empty -- nothing was saved. Try refining the boundary again.", type = "error", duration = 8)
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
          build_saved_dfa_sf(grid_sf = rv$grid_sf, assignments = ordered_assignments, district_sf = rv$canvas_sf),
          error = function(e) NULL
        )
      }
      # Second layer of the same guard the vertex_geojson() observer
      # above already has for its own path into this function -- this
      # one covers the OTHER caller (the assignments() observer, via
      # build_saved_dfa_sf()), which could in principle also produce an
      # empty result. Without this, an empty `saved` would reach
      # send_paint_message()'s as_geojson_text() call below and crash
      # jsonlite::toJSON() the same way (confirmed real failure mode,
      # not hypothetical, for the other call path).
      if (is.null(saved) || nrow(saved) == 0) return(invisible(NULL))
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
        # the original default -- in that specific case, use the EXACT
        # canvas polygon directly (canvas_sf itself, tagged with
        # whatever every cell defaulted to -- In Scope with a real
        # extent, Out of Scope without one) instead of ever routing it
        # through the grid at all. The moment the user paints even one
        # cell differently, grid-based editing is unavoidable and the
        # normal vertex-conversion path below applies exactly as it did
        # before this.
        if (identical(ordered, rv$initial_assignments)) {
          default_dfa_name <- if (isTRUE(rv$has_campaign_extent)) IN_SCOPE_NAME else OUT_SCOPE_NAME
          exact_sf <- .build_exact_untouched_scope(rv$district_sf, rv$canvas_sf, default_dfa_name)
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
