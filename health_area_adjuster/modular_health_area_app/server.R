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
  full_district_sf <- reactive({
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
    dsf <- tryCatch(full_district_sf(), error = function(e) NULL)
    req(!is.null(dsf))
    # Resolves the admin-configurable subdivisions_url (falling back to
    # the hardcoded ARCGIS_SUBDIVISIONS_URL when no override is saved)
    # the same way fetch_urban_areas_for_district() already resolves its
    # own URL -- this was the actual gap: fetch_subdivisions_for_district()
    # itself takes a url parameter now, but this, its only real caller,
    # was never passing one, so the admin field genuinely did nothing.
    subdiv_url <- tryCatch(db_get_data_source_url(pool, 'subdivisions_url'), error = function(e) NULL)
    if (is.null(subdiv_url) || !nzchar(subdiv_url)) subdiv_url <- ARCGIS_SUBDIVISIONS_URL
    subdivisions_rv(tryCatch(fetch_subdivisions_for_district(dsf, url = subdiv_url), error = function(e) NULL))
  }, ignoreInit = TRUE)

  # Separate from subdivisions_rv above -- that one feeds the intro
  # tab's subdivision-picker UI specifically. This is for URBAN
  # CLASSIFICATION only (planning_area_sf below), fetched from its own
  # admin-configurable URL (fetch_urban_areas_for_district(), which
  # falls back to the same subdivisions layer until an admin sets a
  # distinct one) -- decoupled so replacing one URL in the admin page
  # can never silently change what the other feature uses.
  urban_areas_rv <- reactiveVal(NULL)
  observeEvent(active_district(), {
    urban_areas_rv(NULL)
    req(!is.null(active_district()))
    dsf <- tryCatch(full_district_sf(), error = function(e) NULL)
    req(!is.null(dsf))
    urban_areas_rv(tryCatch(fetch_urban_areas_for_district(dsf), error = function(e) NULL))
  }, ignoreInit = TRUE)

  # Whether the CURRENT (active_campaign_id, active_district) pair is
  # scoped to 'urban_only' -- read live from campaign_districts, per
  # admin-configured setting (see mod_admin_tab_v2.R's Manage Districts
  # modal). 'full' (the only behavior that ever existed before this
  # feature) whenever anything here is missing or fails.
  active_mapping_scope <- reactive({
    cid <- active_campaign_id(); dn <- active_district()
    if (is.null(cid) || is.null(dn)) return('full')
    assigned <- tryCatch(db_get_campaign_districts(pool, cid), error = function(e) NULL)
    if (is.null(assigned) || nrow(assigned) == 0) return('full')
    row <- assigned[assigned$district_name == dn, , drop = FALSE]
    if (nrow(row) == 0) return('full')
    row$mapping_scope[1] %||% 'full'
  })

  # THE planning area actually used everywhere -- landmarks, facilities,
  # health areas, and (transitively, since team-area's own planning area
  # is derived from the health area's saved boundary, not the district
  # directly) team areas too. full_district_sf() unless this district is
  # scoped to urban_only for the active campaign, in which case it's a
  # buffered dissolve of that district's subdivision polygons instead --
  # st_union() first (so multiple disconnected urban clusters/islands
  # stay as one valid multi-part geometry, not merged into a single hull
  # that would wrongly swallow rural land between them), buffered by the
  # admin-configured urban_buffer_km, then clipped back to the district's
  # own boundary (a district's planning area logically can't extend
  # beyond the district itself, even if the buffer alone would push past
  # it). Falls back to full_district_sf() on any failure or missing
  # data -- this is a defensive fallback, not the primary enforcement of
  # "no-subdivision districts can't be set to urban only", which is
  # mod_admin_tab_v2.R's .validate_urban_scope(), applied at the point
  # the admin actually sets the scope.
  planning_area_sf <- reactive({
    full_sf <- full_district_sf()
    req(!is.null(full_sf))
    if (!identical(active_mapping_scope(), 'urban_only')) return(full_sf)

    subs <- urban_areas_rv()
    if (is.null(subs) || nrow(subs) == 0) return(full_sf)

    buffer_km <- tryCatch(db_get_generation_setting(pool, 'urban_buffer_km', active_campaign_id()),
                          error = function(e) NA_real_)
    if (is.na(buffer_km)) buffer_km <- 5

    tryCatch({
      subs_3857   <- sf::st_transform(safe_make_valid(subs), 3857)
      dissolved   <- sf::st_union(subs_3857)
      buffered    <- sf::st_buffer(dissolved, buffer_km * 1000)
      buffered_sf <- sf::st_sf(geometry = buffered, crs = 3857)
      full_3857   <- sf::st_transform(full_sf, 3857)
      clipped_sf  <- suppressWarnings(sf::st_intersection(buffered_sf, sf::st_geometry(full_3857)))
      if (nrow(clipped_sf) == 0 || all(sf::st_is_empty(clipped_sf))) return(full_sf)
      clipped_sf$district_name <- full_sf$district_name[1]
      sf::st_transform(clipped_sf, 4326)
    }, error = function(e) full_sf)
  })

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
  #
  # health_area_locked_to_current tracks whether the CURRENTLY active
  # health-area session already has saved boundaries -- when TRUE,
  # Landmarks and Facilities get locked out entirely (see set_tab_enabled
  # observer and the input$main_tabs safety net below) and navigation
  # skips straight to Health Areas, since those earlier stages are
  # already locked-in inputs the existing boundaries were generated
  # from; letting a user revise them afterward would silently
  # invalidate boundaries that look unchanged. Reset to FALSE whenever a
  # genuinely blank/new-draft request comes through, so this lock never
  # incorrectly carries over from a previously continued district.
  health_area_locked_to_current <- reactiveVal(FALSE)

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
    already_mapped <- isTRUE(req_ev$has_boundaries)
    health_area_locked_to_current(already_mapped)
    updateTabsetPanel(session, 'main_tabs',
                      selected = if (already_mapped) 'tab_health_area_mapping' else 'tab_orientation')
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
    # The health-area version activated here is, by definition, always
    # the district's current/published one (per this observer's own
    # comment above) -- so if the user later navigates to Health Areas
    # from here via the header tab, it should show the same locked,
    # skip-Landmarks-and-Facilities state as arriving there directly.
    health_area_locked_to_current(TRUE)
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
    planning_area_sf_r = planning_area_sf
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
    planning_area_sf_r   = planning_area_sf,
    restore_r            = health_area_session$restore_snapshot,
    campaign_id          = reactive(active_campaign_id())
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
    planning_area_sf_r       = planning_area_sf,
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
    ready  <- isTRUE(district_ready())
    locked <- isTRUE(health_area_locked_to_current())
    set_tab_enabled('tab_orientation',             ready && !locked,
                    title = if (locked) 'This district\'s health areas are already mapped -- Landmarks is locked for this session' else 'Choose a district from the Introduction tab first')
    set_tab_enabled('tab_health_facility_mapping', ready && !locked,
                    title = if (locked) 'This district\'s health areas are already mapped -- Facilities is locked for this session' else 'Choose a district from the Introduction tab first')
    set_tab_enabled('tab_health_area_mapping',     ready)
    set_tab_enabled('tab_team_area_mapping',       isTRUE(team_area_session$active()),
                    title = 'Choose a health area from the Introduction tab first')
  })

  observeEvent(input$main_tabs, {
    cat("[navdebug] input$main_tabs fired:", input$main_tabs,
       "| district_ready:", isTRUE(district_ready()),
       "| health_area_locked_to_current:", isTRUE(health_area_locked_to_current()),
       "| team_area_session$active:", isTRUE(team_area_session$active()), "\n")
    locked_on_health_area <- c('tab_orientation', 'tab_health_facility_mapping', 'tab_health_area_mapping')
    if (input$main_tabs %in% locked_on_health_area && !isTRUE(district_ready())) {
      cat("[navdebug] REDIRECT to tab_intro -- reason: locked_on_health_area tab with district_ready FALSE\n")
      updateTabsetPanel(session, 'main_tabs', selected = 'tab_intro')
      showNotification('Choose a district from the Introduction tab first.', type = 'message', duration = 3)
      return()
    }
    # Same visual lock as set_tab_enabled() above, enforced server-side --
    # e.g. against the browser back button landing directly on a locked
    # tab's URL fragment, which set_tab_enabled()'s own client-side click
    # guard alone wouldn't catch.
    if (input$main_tabs %in% c('tab_orientation', 'tab_health_facility_mapping') &&
        isTRUE(health_area_locked_to_current())) {
      cat("[navdebug] REDIRECT to tab_health_area_mapping -- reason: locked-out tab with health_area_locked_to_current TRUE\n")
      updateTabsetPanel(session, 'main_tabs', selected = 'tab_health_area_mapping')
      showNotification('This district\'s health areas are already mapped -- Landmarks and Facilities are locked for this session.',
                       type = 'message', duration = 4)
      return()
    }
    if (identical(input$main_tabs, 'tab_team_area_mapping') && !isTRUE(team_area_session$active())) {
      cat("[navdebug] REDIRECT to tab_intro -- reason: tab_team_area_mapping with team_area_session inactive\n")
      updateTabsetPanel(session, 'main_tabs', selected = 'tab_intro')
      showNotification('Choose a health area from the Introduction tab first.', type = 'message', duration = 3)
    }
  }, ignoreInit = TRUE)
}
