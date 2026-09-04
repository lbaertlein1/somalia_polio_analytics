# =============================================================================
# mod_team_area_tab.R
#
# Structurally this is a second, nested instance of the health-area
# machinery: same paint-app.js canvas, same BFS engine, but seeded WITHIN
# one health area rather than across the whole district, and with the
# health area's own polygon as the grid boundary (which makes it a hard
# wall automatically — the grid simply doesn't extend past it, the same
# way the district edge works for Health Areas).
#
# Team-area versioning rework: each session is scoped to exactly ONE
# health area (health_area_name, chosen externally by the intro table's
# team-area drill-down before this tab is ever reached — see
# mod_intro_tab_v2.R). This replaces the old in-tab health-area dropdown
# and its per_area multi-area cache entirely — under the per-(district,
# campaign, health_area) team-area versioning model
# (team_area_versions — see mod_db_v2.R SECTION 13), a health area's team
# map is its own independently-versioned, independently-publishable unit,
# never bundled with any other health area's team work in the same
# submission.
#
# Undo, brush painting, and the Inaccessible/Unpopulated toggle all reuse
# the same paint-app.js engine and its unified undo stack — no separate
# wiring needed beyond passing an undoCountInputId so the Undo button can
# disable itself when the stack is empty.
#
# Simplification versus mod_health_area_tab.R: this does not replicate
# every edge-case guard the health-area tab has accumulated (stale-response
# detection keyed by a seed hash, "areas_regenerated" notifications on
# facility changes, etc.) — team areas don't change facility seeds live
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
               healthAreaPopulationUI(ns('population'), name_col_label = "Team Name", allow_rename = TRUE)))
  )
}

teamAreaTabServer <- function(
    id,
    district, campaign_id, district_ready,
    active_tab,
    saved_dfa_sf_r    = reactive(NULL),   # finalized health areas from the Health Areas tab
    # Which single health area this session's teams are for -- chosen
    # externally (the intro table's team-area drill-down picks it before
    # this tab is ever reached; see mod_intro_tab_v2.R). Replaces the old
    # in-tab health_area dropdown + per_area multi-area cache entirely --
    # under the new per-(district, campaign, health_area) team-area
    # versioning, a session is always scoped to exactly one health area,
    # never several at once.
    health_area_name  = reactive(NULL),
    all_facilities_r  = reactive(NULL),
    landmarks_r       = reactive(NULL),
    submit_stage_fn   = NULL,
    restore_r         = reactive(NULL),
    # Optional -- per-health-area team-planning overrides from the
    # post-submit modal in mod_health_area_tab.R, keyed by health area
    # name: list(target_pop, requested_teams). Defaults to reactive(NULL),
    # which n_teams_r below already treats the same as "no override for
    # this area" -- falls back to compute_n_teams()'s own recommendation.
    team_targets_r    = reactive(NULL),
    # "Make current" / staleness support -- wired from server.R to
    # teamAreaSession$make_current / teamAreaSession$is_stale
    # (mod_session_manager_v2.R). Both default to safe no-ops/FALSE so a
    # caller that doesn't pass them just never shows the message.
    #
    # Staleness has no automatic fix at this level: a team-area version
    # stays permanently pinned to the health-area version it was drawn
    # against (based_on_health_area_version_id never changes). The only
    # way to make a stale version workable again is to make its pinned
    # health-area version current again -- so this tab's only job when
    # stale is to say that plainly and send the user back to the
    # overview, not to offer a fix it can't actually perform itself.
    make_current_fn   = NULL,
    is_stale_r        = reactive(FALSE)
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
      smoothed_team_sf = NULL   # vertex-refined boundary for this session's one health area
    )

    pending_action  <- reactiveVal(NULL)
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

    # ── Inline team rename (from the population table) ──────────────────────
    # Called by mod_health_area_population.R when the user edits the name
    # column directly in the table. Must return TRUE/FALSE so that module
    # knows whether to force the table back to the old value on rejection.
    .on_team_rename <- function(old_name, new_name) {
      if (old_name %in% extra_dfa_names) {
        showNotification(sprintf('"%s" can\'t be renamed.', old_name), type = "warning", duration = 4)
        return(FALSE)
      }
      if (new_name %in% extra_dfa_names) {
        showNotification(sprintf('"%s" is a reserved name.', new_name), type = "warning", duration = 4)
        return(FALSE)
      }
      if (new_name %in% setdiff(rv$team_names, old_name)) {
        showNotification(sprintf('A team named "%s" already exists.', new_name), type = "error", duration = 4)
        return(FALSE)
      }

      rv$team_names <- normalize_dfa_names_team(
        ifelse(rv$team_names == old_name, new_name, rv$team_names)
      )
      if (!is.null(rv$current_assignments)) {
        rv$current_assignments[rv$current_assignments == old_name] <- new_name
      }
      send_paint_message("paint_rename_area", list(oldName = old_name, newName = new_name))
      recompute_population_table(rv$current_assignments)
      if (identical(active_team_rv(), old_name)) active_team_rv(new_name)
      TRUE
    }

    healthAreaPopulationServer(
      'population',
      active_dfa_rv        = active_team_rv,
      show_pop_raster      = controls$show_pop_raster,
      show_friction_raster = controls$show_friction_raster,
      pop_table            = reactive(rv$pop_table),
      in_vertex_mode       = in_vertex_mode,
      name_col_label       = "Team Name",
      allow_rename         = TRUE,
      on_rename            = .on_team_rename
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

    selected_health_area_sf <- reactive({
      req(health_area_name(), saved_dfa_sf_r())
      sf_obj <- saved_dfa_sf_r()
      sub <- sf_obj[sf_obj$dfa_name == health_area_name(), , drop = FALSE]
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

    # Field Requested Teams (from the post-submit modal in
    # mod_health_area_tab.R) overrides the recommendation when present and
    # valid for the CURRENTLY selected health area; otherwise falls back
    # to compute_n_teams()'s own WorldPop-based recommendation, exactly as
    # before this override existed.
    n_teams_r <- reactive({
      overrides <- tryCatch(team_targets_r(), error = function(e) NULL)
      override  <- overrides[[health_area_name() %||% '']]$requested_teams
      if (!is.null(override) && !is.na(override) && override > 0) {
        as.integer(override)
      } else {
        compute_n_teams(area_population(), campaign_id = campaign_id())
      }
    })

    team_seed_sf <- reactive({
      req(selected_health_area_sf(), n_teams_r())
      compute_team_area_seeds(
        health_area_sf = selected_health_area_sf(),
        u5_rast        = u5_rast,
        n_teams        = n_teams_r(),
        seed           = sum(utf8ToInt(health_area_name() %||% 'x'))
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
      seed                  = reactive(sum(utf8ToInt(health_area_name() %||% 'x'))),
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

    scene_ever_sent <- reactiveVal(FALSE)

    send_current_scene <- function() {
      req(tab_active())
      req(!is.null(rv$district_sf), !is.null(rv$grid_sf), !is.null(rv$current_assignments))
      scene_ever_sent(TRUE)

      # Unconditionally force the JS side back to paint mode before every
      # scene (re)load, regardless of what in_vertex_mode() currently
      # thinks. Belt-and-suspenders against the JS side ever getting
      # stuck in vertex mode with no reactive reflecting it -- e.g. if
      # the silent auto-conversion during Save/Submit enters vertex mode
      # (deliberately without setting in_vertex_mode(), to avoid UI
      # flicker) and its own exit step fails to fire for any reason, nothing
      # else would ever notice or correct the desync. paint_exit_vertex_mode
      # is a safe no-op if already in paint mode -- exitVertexMode() only
      # rasterizes if a vertex engine actually exists, and its layer-restore
      # loop is empty if nothing was ever hidden.
      send_paint_message("paint_exit_vertex_mode")
      if (isTRUE(in_vertex_mode())) {
        in_vertex_mode(FALSE)
        controls$set_vertex_mode_ui(FALSE)
      }

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

    # This session is scoped to exactly one health area for its whole
    # lifetime -- no more switching between health areas mid-session. But
    # the scene's OWN inputs can still legitimately change shortly after
    # this tab first mounts: n_teams_r() depends on team_targets_r()
    # (the field-requested-teams override from mod_health_area_tab.R),
    # and that reactive can settle to its real value on a later tick than
    # this tab's own first scene computation -- team_scene_mod$scene()
    # then correctly re-fires with the right n_teams, and this needs to
    # actually apply that, not just the first thing that happened to
    # compute. Tracked by key (health_area_name + n_teams), same pattern
    # as mod_health_area_tab.R's scene_is_new/last_scene_key, rather than
    # a plain "has this run once" flag -- the latter is exactly what
    # silently locked in whatever n_teams happened to be available on the
    # very first tick, permanently ignoring a correction.
    last_scene_key  <- reactiveVal(NULL)
    pending_restore <- reactiveVal(NULL)
    restore_applied <- reactiveVal(FALSE)

    .apply_restore <- function(snap) {
      if (is.null(snap$current_team_assignments) || is.null(rv$grid_sf)) return(invisible(NULL))
      rv$current_assignments <- vapply(as.character(rv$grid_sf$cell_id), function(id) {
        val <- snap$current_team_assignments[[id]]
        if (is.null(val)) rv$initial_assignments[as.integer(id)] else as.character(val)
      }, character(1))
      rv$saved_team_sf    <- snap$saved_team_sf
      rv$team_names        <- snap$team_names %||% rv$team_names
      rv$smoothed_team_sf     <- snap$smoothed_team_sf
      restore_applied(TRUE)
      showNotification('Team area draft restored.', type = 'message', duration = 2)
      invisible(NULL)
    }

    observeEvent(restore_r(), {
      snap <- restore_r()
      req(!is.null(snap))
      if (!is.null(rv$grid_sf)) .apply_restore(snap) else pending_restore(snap)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(team_scene_mod$scene(), {
      req(nzchar(health_area_name() %||% ''))
      # Once a draft's own saved team layout has been restored, that
      # takes precedence permanently for this session -- a later re-fire
      # of team_scene_mod$scene() (e.g. n_teams_r() settling to a
      # different value) must not wipe restored work back to a fresh
      # regeneration. A draft's own team layout is fixed by what was
      # previously saved, not by recomputing seeds again.
      if (isTRUE(restore_applied())) return()

      sc <- tryCatch(team_scene_mod$scene(), error = function(e) NULL)
      req(!is.null(sc))

      new_key <- paste0(health_area_name(), '|', n_teams_r())
      if (identical(last_scene_key(), new_key)) return()
      last_scene_key(new_key)

      rv$district_sf         <- sc$district_sf
      rv$grid_sf              <- sc$grid_sf
      rv$grid_sf$u5_pop        <- sc$grid_sf$u5_pop %||% rep(0, nrow(sc$grid_sf))
      rv$neighbors_list          <- sc$neighbors_list
      rv$edge_list                <- sc$edge_list
      rv$friction_path              <- sc$friction_path
      rv$seed_points                  <- sc$seed_points_list
      rv$team_names                     <- normalize_dfa_names_team(unique(as.character(sc$initial_assignments)))
      rv$initial_assignments             <- sc$initial_assignments
      rv$current_assignments               <- sc$initial_assignments
      rv$saved_team_sf                       <- NULL
      rv$smoothed_team_sf                      <- NULL

      pr <- pending_restore()
      if (!is.null(pr)) { .apply_restore(pr); pending_restore(NULL) }

      if (isTRUE(in_vertex_mode())) send_paint_message("paint_exit_vertex_mode")
      in_vertex_mode(FALSE)
      controls$set_vertex_mode_ui(FALSE)

      if (is.null(active_team_rv()) || !active_team_rv() %in% rv$team_names) active_team_rv(rv$team_names[[1]])
      recompute_population_table(rv$current_assignments)
      if (tab_active()) send_current_scene()
    }, ignoreInit = TRUE)

    observeEvent(active_tab(), {
      if (tab_active() && !is.null(rv$grid_sf)) send_current_scene()
    }, ignoreInit = TRUE)

    # readyInputId (map_ready()) fires from INSIDE paint-app.js's own
    # loadScene() every time a scene finishes loading -- not just once,
    # at true initial map creation -- so calling send_current_scene()
    # here unconditionally would ping-pong forever (scene loads -> JS
    # signals ready -> this resends the scene -> JS reloads and signals
    # ready again -> ...). Gated on scene_ever_sent() specifically to
    # avoid that: this only ever catches up a scene that never
    # successfully sent in the first place (e.g. paint_load_scene
    # arriving before the map container existed, on this tab's fast
    # direct-from-intro navigation), and does nothing on every
    # subsequent, ordinary "scene finished loading" signal.
    observeEvent(map_mod$map_ready(), {
      if (tab_active()) send_paint_message('hide_loading')
      if (tab_active() && !is.null(rv$grid_sf) && !isTRUE(scene_ever_sent())) send_current_scene()
    }, ignoreInit = TRUE)

    observeEvent(map_mod$undo_count(), {
      shinyjs::toggleState(session$ns('controls-paint_undo_btn'),
                           condition = isTRUE(map_mod$undo_count() > 0))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # Same for the refine-step Undo button -- same fix as
    # mod_health_area_tab.R's identical wiring.
    observeEvent(map_mod$vertex_undo_count(), {
      shinyjs::toggleState(session$ns('controls-refine_undo_btn'),
                           condition = isTRUE(map_mod$vertex_undo_count() > 0))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # ── Controls wiring ────────────────────────────────────────────────────────

    observeEvent(controls$brush_m(), {
      req(tab_active()); send_paint_message('paint_set_brush', list(value = controls$brush_m() / 2))
    }, ignoreInit = TRUE)
    observeEvent(controls$boundary_only(), {
      req(tab_active()); send_paint_message('paint_set_boundary_only', list(value = controls$boundary_only()))
    }, ignoreInit = TRUE)

    # These were entirely missing -- show_pop_raster/show_friction_raster
    # were only ever read by healthAreaPopulationServer for the legend's
    # own text, never actually used to build an overlay and send it to
    # the map. Checking the box updated the legend but nothing else,
    # which is exactly the reported symptom. Same pattern as
    # mod_health_area_tab.R's identical wiring, adapted for this file's
    # names (u5_rast used directly here rather than via a
    # get_u5_worldpop() wrapper, since that wrapper doesn't exist in this
    # file -- Team Areas already references the global u5_rast directly
    # elsewhere, e.g. area_population()).
    observeEvent(controls$show_pop_raster(), {
      req(tab_active(), isTRUE(district_ready()))
      if (isTRUE(controls$show_pop_raster()) && is.null(rv$pop_overlay_sf) && !is.null(rv$district_sf)) {
        rv$pop_overlay_sf <- tryCatch(
          make_population_overlay_sf(district_sf = rv$district_sf, u5_rast = u5_rast),
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

    observeEvent(controls$paint_undo_click(), {
      req(tab_active(), !isTRUE(in_vertex_mode())); send_paint_message('paint_undo')
      pending_action('refresh'); send_paint_message('paint_request_assignments')
    }, ignoreInit = TRUE)

    observeEvent(active_team_rv(), {
      req(tab_active(), !is.null(rv$current_assignments))
      send_paint_message('paint_set_colors', list(colors = as.list(current_fill_colors()), activeDfa = active_team_rv()))
      pending_action('capture'); send_paint_message('paint_request_assignments')
    }, ignoreInit = TRUE)

    observeEvent(controls$reset_click(), {
      req(tab_active(), !isTRUE(in_vertex_mode()), !is.null(rv$initial_assignments))
      rv$current_assignments <- rv$initial_assignments
      rv$saved_team_sf       <- NULL
      send_paint_message('paint_reset')
      send_paint_message('paint_set_colors', list(colors = as.list(current_fill_colors()), activeDfa = active_team_rv()))
      recompute_population_table(rv$initial_assignments)
    }, ignoreInit = TRUE)

    observeEvent(controls$save_click(), {
      req(tab_active(), !isTRUE(in_vertex_mode())); pending_action('save'); send_paint_message('paint_request_assignments')
    }, ignoreInit = TRUE)

    observeEvent(controls$submit_click(), {
      req(tab_active())
      pending_action('submit'); send_paint_message('paint_request_assignments')
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

    # ── Undo / Reset (refining) ──────────────────────────────────────────────
    observeEvent(controls$refine_undo_click(), {
      req(isTRUE(in_vertex_mode()))
      send_paint_message("paint_vertex_undo")
    }, ignoreInit = TRUE)

    observeEvent(controls$refine_reset_click(), {
      req(isTRUE(in_vertex_mode()))
      send_paint_message("paint_reset_vertex_edits")
    }, ignoreInit = TRUE)

    # Fires for three distinct reasons, distinguished by pending_action() --
    # same pattern as mod_health_area_tab.R's identical wiring:
    #   "manual_refine_save" -- user explicitly clicked Save Refinements.
    #   "finalize_save"      -- Save was clicked; always runs this health
    #                           area's boundary through vertex conversion
    #                           as part of finishing, whether or not
    #                           Refine Boundaries was ever opened.
    #   "finalize_submit"    -- same, then writes it to the DB via
    #                           submit_stage_fn -- this session is scoped
    #                           to exactly one health area, so there's no
    #                           more "combine everything cached" step.
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

      if (identical(act, "finalize_save") || identical(act, "finalize_submit")) {
        rv$smoothed_team_sf <- parsed

        if (isTRUE(rv$vertex_mode_silently_entered)) {
          send_paint_message("paint_exit_vertex_mode")
          rv$vertex_mode_silently_entered <- FALSE
        }

        .update_current_area(rv$current_assignments, saved_sf_override = parsed)
        recompute_population_table(rv$current_assignments)

        if (identical(act, "finalize_save")) {
          showNotification(sprintf('Team areas saved for %s.', health_area_name()), type = 'message', duration = 2)
          pending_action(NULL)
          return()
        }

        if (!is.null(rv$saved_team_sf) && !is.null(submit_stage_fn)) {
          submit_stage_fn(list(
            saved_team_sf             = rv$saved_team_sf,
            team_names                = rv$team_names,
            current_team_assignments  = setNames(as.list(rv$current_assignments), as.character(rv$grid_sf$cell_id)),
            smoothed_team_sf          = rv$smoothed_team_sf
          ))
          .show_make_current_prompt()
        }
        pending_action(NULL)
        return()
      }
    }, ignoreInit = TRUE)

    # ── "Make current" prompt — shown right after a team-area submission,
    # scoped to just this one health area, independent of anything else in
    # the district. No lock-gating here (unlike health areas) -- team-area
    # publish never invalidates anything else downstream. make_current_fn
    # (teamAreaSession$make_current) does its own final, authoritative
    # staleness check server-side regardless of what this tab shows; if it
    # refuses (the pinned health-area version stopped being current while
    # this session was open), the notification it raises says so plainly --
    # there's nothing else for this prompt to offer.
    .show_make_current_prompt <- function() {
      if (is.null(make_current_fn)) return(invisible(NULL))
      showModal(modalDialog(
        title = 'Team areas submitted', size = 's', easyClose = FALSE,
        footer = tagList(
          actionButton(session$ns('team_make_current_skip'), 'Not now', class = 'btn btn-default'),
          actionButton(session$ns('team_make_current_confirm'), 'Set as current', class = 'btn btn-primary')
        ),
        div(
          style = 'font-size:13px;color:#475569;line-height:1.6;',
          tags$p('Set this submission as ', tags$strong(health_area_name() %||% 'this health area'),
                "'s current team map?")
        )
      ))
    }

    observeEvent(input$team_make_current_skip, { removeModal() }, ignoreInit = TRUE)

    observeEvent(input$team_make_current_confirm, {
      removeModal()
      if (!is.null(make_current_fn)) make_current_fn()
    }, ignoreInit = TRUE)

    # saved_sf_override lets callers supply an already-computed boundary
    # (the vertex-converted result) instead of having this function
    # rebuild a fresh raw-grid one -- used by the finalize_save/
    # finalize_submit path above, where rebuilding here would silently
    # discard the conversion that was just done.
    .update_current_area <- function(ordered_assignments, saved_sf_override = NULL) {
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
      # above for where finalize_save/finalize_submit actually
      # complete. No longer depends on the user having opened Refine
      # Boundaries themselves; if they have (in_vertex_mode() is TRUE),
      # this finalizes their current live edits directly instead.
      if (identical(act, 'save') || identical(act, 'submit')) {
        pending_action(if (identical(act, 'save')) 'finalize_save' else 'finalize_submit')
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

    # ── Stale-draft handling ─────────────────────────────────────────────────
    # Fires whenever the session-manager layer reports this draft's pinned
    # health-area version is no longer current (checked at draft-open time,
    # AND on any later change to is_stale_r() -- e.g. the health-area
    # version changing while this tab is already open). Applies uniformly
    # whether this draft was ever published or not -- staleness doesn't
    # care about publish state, only about whether the boundary it was
    # drawn against still exists as the district's current one. There is
    # no fix to offer here -- only making the pinned health-area version
    # current again un-stales it -- so this just explains that plainly and
    # sends the user back to the overview rather than leaving them on a
    # tab where nothing they do here can help.
    observeEvent(is_stale_r(), {
      req(isTRUE(is_stale_r()))
      showModal(modalDialog(
        title = 'This team area is based on a previous health-area version',
        size = 'm', easyClose = FALSE,
        footer = actionButton(session$ns('stale_back_to_intro'), 'Back to overview', class = 'btn btn-primary'),
        div(
          style = 'font-size:13px;color:#475569;line-height:1.6;',
          tags$p(
            "The health area this team map was drawn against is no longer this district's current one. ",
            "This team area can't be worked on here until that health-area version is made current again ",
            '(from the Admin panel\u2019s District review section), or you can start a new team-area draft ',
            'against whatever health-area version is current now, from the overview page.'
          )
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$stale_back_to_intro, {
      removeModal()
      session$sendCustomMessage('switch_tab', list(value = 'tab_intro'))
    }, ignoreInit = TRUE)

    list(
      has_scene = reactive(!is.null(rv$grid_sf))
    )
  })
}

# Team-name equivalent of the health-area normalize_dfa_names() helper —
# duplicated locally to avoid depending on mod_health_area_tab.R's private
# closure of the same name. Deliberately does NOT append extra_dfa_names
# (Inaccessible/Unpopulated) the way the health-area version does: those
# are meaningful, user-selectable fallback categories at the district
# level, but a health area's own territory is by definition already the
# accessible/populated selection, so team areas have no legitimate use
# for them — appending them here only ever produced phantom extra
# "teams" with zero cells and zero population.
normalize_dfa_names_team <- function(x) {
  x <- unique(as.character(x)); x <- x[!is.na(x) & nzchar(x)]
  x
}
