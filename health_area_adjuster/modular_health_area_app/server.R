app_server <- function(input, output, session) {

  # ===========================================================================
  # Auth
  # ===========================================================================
  auth <- authServer('auth')

  observeEvent(auth$logged_in, {
    req(isTRUE(auth$logged_in))
    shinyjs::hide('login_screen')
    shinyjs::show('main_app')

    if (auth$role == 'admin') {
      insertTab(
        session = session, inputId = 'main_tabs',
        tabPanel(title = 'Admin', value = 'tab_admin', adminTabUI('admin')),
        position = 'after'
      )
      session$sendCustomMessage('insert_nav_tab', list(value = 'tab_admin', label = 'Admin'))
      adminTabServer('admin', districts_shp = districts_shp, username_r = reactive(auth$username))
    }
  }, ignoreInit = TRUE, once = TRUE)

  actor_role_r <- reactive(auth$role %||% 'user')

  # ===========================================================================
  # Intro tab — the ONLY entry point. Owns the campaign selector and the
  # district-status table; its two drill-down pickers are the sole way a
  # health-area or team-area version ever gets chosen. No more single
  # "selected district" reactive owned by this module, no more auto-resume,
  # no more region-then-district cascading selector -- see
  # mod_intro_tab_v2.R's own header comment for the full reasoning.
  # ===========================================================================
  intro <- introTabServer('intro', districts_shp = districts_shp, username_r = reactive(auth$username),
                          active_tab = reactive(input$main_tabs))

  # ===========================================================================
  # Active district/campaign context — derived from whichever drill-down
  # request most recently fired, not owned by the intro tab itself (it has
  # no single "current district" concept anymore; the table can show many).
  # ===========================================================================
  active_district         <- reactiveVal(NULL)
  active_campaign_id      <- reactiveVal(NULL)
  active_team_health_area <- reactiveVal(NULL)

  # Subdivisions + full district geometry, previously computed once inside
  # introTabServer for its single selected district. Rebuilt here, keyed off
  # active_district(), since the intro tab no longer owns that concept.
  active_district_sf_full <- reactive({
    req(!is.null(active_district()))
    districts_shp |>
      dplyr::filter(district_name == active_district()) |>
      dplyr::summarise(
        district_name = dplyr::first(district_name),
        geometry      = sf::st_union(geometry),
        .groups       = 'drop'
      ) |>
      sf::st_as_sf() |> safe_make_valid() |> sf::st_transform(4326)
  })

  subdivisions_rv <- reactiveVal(NULL)
  observeEvent(active_district(), {
    subdivisions_rv(NULL)
    req(!is.null(active_district()))
    dsf <- tryCatch(active_district_sf_full(), error = function(e) NULL)
    req(!is.null(dsf))
    subdivisions_rv(tryCatch(fetch_subdivisions_for_district(dsf), error = function(e) NULL))
  }, ignoreInit = TRUE)

  zone_r <- reactive({
    req(!is.null(active_district()))
    d <- districts_shp |> dplyr::filter(district_name == active_district())
    if (nrow(d) == 0) '' else as.character(d$zone_name[1]) %||% ''
  })
  region_r <- reactive({
    req(!is.null(active_district()))
    d <- districts_shp |> dplyr::filter(district_name == active_district())
    if (nrow(d) == 0) '' else as.character(d$region_name[1]) %||% ''
  })

  # ===========================================================================
  # Session managers — independently versioned health-area and team-area
  # tracks (mod_session_manager_v2.R). No more auto-detect/branch modal, no
  # standalone Publish button — each is activated by an EXPLICIT version_id
  # from the intro tab's drill-down, and "make current" is offered at
  # submit time inside the tab itself.
  # ===========================================================================
  health_area_session <- healthAreaSessionServer(
    'ha_session', username_r = reactive(auth$username),
    district_r = active_district, campaign_id_r = active_campaign_id
  )

  team_area_session <- teamAreaSessionServer(
    'ta_session', username_r = reactive(auth$username),
    district_r = active_district, campaign_id_r = active_campaign_id,
    health_area_name_r = active_team_health_area
  )

  district_ready <- reactive(isTRUE(health_area_session$active()))

  # ── Health Areas drill-down request -> activate + route to Orientation,
  # the first stage in the sequence. A blank draft has no landmarks/
  # facilities yet; a resumed draft's already-completed stages just show
  # as already done via restore_r(), same as this always worked.
  observeEvent(intro$activate_health_area_request(), {
    req_ev <- intro$activate_health_area_request()
    req(!is.null(req_ev))
    active_district(req_ev$district_name)
    active_campaign_id(intro$campaign_id())
    # Entering health-area work always leaves behind whatever team-area
    # session was active before -- it belonged to a specific health area
    # (possibly in a different district entirely) that's no longer this
    # session's context. Without this, the Team Areas tab would stay
    # unlocked and pointing at stale state until the user happened to
    # revisit it via its own drill-down.
    team_area_session$deactivate()
    active_team_health_area(NULL)
    ok <- health_area_session$activate_version_id(req_ev$version_id)
    req(isTRUE(ok))
    updateTabsetPanel(session, 'main_tabs', selected = 'tab_orientation')
  }, ignoreInit = TRUE)

  # ── Team Areas drill-down request -> silently activate the health-area
  # session to the district's CURRENT version (never a picker for team
  # areas' own purposes -- team areas can only ever be drawn against the
  # current health-area map), activate the team-area session to whatever
  # the user actually chose, route directly to Team Areas.
  observeEvent(intro$activate_team_area_request(), {
    req_ev <- intro$activate_team_area_request()
    req(!is.null(req_ev))
    active_district(req_ev$district_name)
    active_campaign_id(intro$campaign_id())
    active_team_health_area(req_ev$health_area_name)
    ha_ok <- health_area_session$activate_version_id(req_ev$health_area_version_id)
    req(isTRUE(ha_ok))
    ta_ok <- team_area_session$activate_team_version_id(req_ev$team_version_id)
    req(isTRUE(ta_ok))
    updateTabsetPanel(session, 'main_tabs', selected = 'tab_team_area_mapping')
  }, ignoreInit = TRUE)

  # ===========================================================================
  # Orientation
  # ===========================================================================
  orientation <- orientationTabServer(
    'orientation',
    zone               = zone_r,
    region             = region_r,
    district           = active_district,
    district_ready     = district_ready,
    active_tab         = reactive(input$main_tabs),
    submit_stage_fn    = function(stage, data) health_area_session$submit_stage(stage, data),
    restore_r          = health_area_session$restore_snapshot,
    subdivisions_r     = subdivisions_rv,
    planning_area_sf_r = active_district_sf_full
  )

  # ===========================================================================
  # Facility tab
  # ===========================================================================
  submitted_facilities <- reactiveVal(NULL)

  facility <- facilityTabServer(
    'facility',
    zone                 = zone_r,
    region               = region_r,
    district             = active_district,
    district_ready       = district_ready,
    active_tab           = reactive(input$main_tabs),
    submitted_facilities = submitted_facilities,
    submit_stage_fn      = function(stage, data) health_area_session$submit_stage(stage, data),
    landmarks_r          = orientation$landmarks_r,
    subdivisions_r       = subdivisions_rv,
    planning_area_sf_r   = active_district_sf_full,
    restore_r            = health_area_session$restore_snapshot
  )

  # ===========================================================================
  # Health area tab
  # ===========================================================================
  health_area <- healthAreaTabServer(
    'health_area',
    zone                     = zone_r,
    region                   = region_r,
    district                 = active_district,
    district_ready           = district_ready,
    active_tab               = reactive(input$main_tabs),
    facility_data            = submitted_facilities,
    all_facilities_r         = facility$facility_data,
    landmarks_r              = orientation$landmarks_r,
    subdivisions_r           = subdivisions_rv,
    planning_area_sf_r       = active_district_sf_full,
    submit_stage_fn          = function(stage, data) health_area_session$submit_stage(stage, data),
    restore_r                = health_area_session$restore_snapshot,
    campaign_id              = active_campaign_id,
    make_current_fn          = function(actor_role) health_area_session$make_current(actor_role = actor_role),
    is_locked_for_publish_r  = health_area_session$is_locked_for_publish,
    actor_role_r             = actor_role_r
  )

  # ===========================================================================
  # Team area tab — draws team boundaries within ONE health area, chosen by
  # the intro tab's drill-down (active_team_health_area). Note this takes
  # district()/campaign_id() directly rather than a planning_area_sf_r,
  # since its "district_sf" per generation call is actually a single health
  # area's polygon, computed inside the module from saved_dfa_sf_r().
  #
  # saved_dfa_sf_r's source has to cover TWO different paths:
  #   - health_area$saved_dfa_sf_r() -- the health-area tab's own live
  #     rv$saved_dfa_sf, which reflects the most recent in-session edit or
  #     submit. Populated by that tab's selected_scene(), which only runs
  #     while the health-area tab is actually active -- correct there
  #     (deferring expensive grid/BFS work until the tab is visible), but
  #     it means this stays NULL forever on the "straight to team area
  #     mapping" path, since that path deliberately never makes the
  #     health-area tab active at all (see mod_intro_tab_v2.R's header
  #     comment on the Team Areas drill-down).
  #   - health_area_session$restore_snapshot()$saved_dfa_sf -- the DB
  #     snapshot from session activation itself, set the moment
  #     activate_version_id() runs regardless of tab visibility. This is
  #     the fallback for exactly that path.
  # Prefer the live tab value when it exists (most up to date); fall back
  # to the session snapshot only when the tab never loaded one.
  # ===========================================================================
  team_area_saved_dfa_sf_r <- reactive({
    from_tab <- health_area$saved_dfa_sf_r()
    if (!is.null(from_tab)) return(from_tab)
    snap <- health_area_session$restore_snapshot()
    if (is.null(snap)) return(NULL)
    snap$saved_dfa_sf
  })

  team_area <- teamAreaTabServer(
    'team_area',
    district          = active_district,
    campaign_id       = active_campaign_id,
    district_ready    = district_ready,
    active_tab        = reactive(input$main_tabs),
    saved_dfa_sf_r    = team_area_saved_dfa_sf_r,
    health_area_name  = active_team_health_area,
    all_facilities_r  = facility$facility_data,
    landmarks_r       = orientation$landmarks_r,
    submit_stage_fn   = function(data) team_area_session$submit_stage(data),
    restore_r         = team_area_session$restore_snapshot,
    team_targets_r    = health_area$team_targets_r,
    make_current_fn   = function() team_area_session$make_current(),
    is_stale_r        = team_area_session$is_stale
  )

  # microplan tab removed entirely — no server call for it.

  # ===========================================================================
  # Export tab — standalone, available to every user regardless of role or
  # whatever district/session is currently active. Scoped entirely to
  # current/published data (see mod_export_tab.R's own header comment).
  # ===========================================================================
  exportTabServer('export', districts_shp = districts_shp)

  # ===========================================================================
  # Facility restore side-effect — extracting submitted_facilities from a
  # restored health-area snapshot is server.R-specific glue, not something
  # any single tab's own restore_from_snapshot handles (facility tab reads
  # submitted_facilities as a reactiveVal it doesn't own). Each tab's own
  # restore_r() observer (wired above) handles its own restoration; this is
  # purely the cross-tab side-effect the old server.R also had.
  # ===========================================================================
  observeEvent(health_area_session$restore_snapshot(), {
    snap <- health_area_session$restore_snapshot()
    req(!is.null(snap))

    fac_sf <- tryCatch({
      parts <- Filter(Negate(is.null), list(snap$odk_sf, snap$app_sf))
      parts <- Filter(function(x) inherits(x, 'sf') && nrow(x) > 0, parts)
      if (length(parts) == 0) NULL else do.call(rbind, parts)
    }, error = function(e) NULL)

    if (!is.null(fac_sf)) {
      fac_df <- facility_sf_to_df(fac_sf)
      sia    <- fac_df[
        !is.na(fac_df$polio_sia_coordination_site) &
          fac_df$polio_sia_coordination_site == 'Yes', ,
        drop = FALSE
      ]
      if (nrow(sia) > 0) submitted_facilities(sia)
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ===========================================================================
  # Tab locking — Orientation/Facilities/Health Areas need an active
  # health-area session; Team Areas needs its own active team-area session
  # (independent gate, since a user can be mid-Team-Areas without the
  # health-area session having ever been visibly opened -- it was
  # activated silently by the Team Areas drill-down).
  # ===========================================================================
  set_tab_enabled <- function(value, enabled, title = 'Choose a district from the Introduction tab first') {
    session$sendCustomMessage(
      'set_tab_enabled',
      list(value = value, enabled = isTRUE(enabled), title = title)
    )
  }

  observe({
    ready <- isTRUE(district_ready())
    set_tab_enabled('tab_orientation',             ready)
    set_tab_enabled('tab_health_facility_mapping', ready)
    set_tab_enabled('tab_health_area_mapping',     ready)
    set_tab_enabled('tab_team_area_mapping',       isTRUE(team_area_session$active()),
                    title = 'Choose a health area from the Introduction tab first')
  })

  observeEvent(input$main_tabs, {
    locked_on_health_area <- c('tab_orientation', 'tab_health_facility_mapping', 'tab_health_area_mapping')
    if (input$main_tabs %in% locked_on_health_area && !isTRUE(district_ready())) {
      updateTabsetPanel(session, 'main_tabs', selected = 'tab_intro')
      showNotification('Choose a district from the Introduction tab first.', type = 'message', duration = 3)
      return()
    }
    if (identical(input$main_tabs, 'tab_team_area_mapping') && !isTRUE(team_area_session$active())) {
      updateTabsetPanel(session, 'main_tabs', selected = 'tab_intro')
      showNotification('Choose a health area from the Introduction tab first.', type = 'message', duration = 3)
    }
  }, ignoreInit = TRUE)
}
