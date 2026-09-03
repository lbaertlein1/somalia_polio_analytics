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
  
  # ===========================================================================
  # Intro tab
  #
  # v2: no more allowed_districts_r (any authenticated user, any district).
  # Exposes campaign_id() alongside district() — session_mgr keys on both.
  # ===========================================================================
  intro <- introTabServer('intro', districts_shp = districts_shp)
  
  # Subdivisions are still fetched (for the barrier/reference layer during
  # generation) but no longer used to split the district into separate
  # planning units — no more "— Rural" label check needed.
  subdivisions_r   <- intro$subdivisions_r
  planning_area_sf <- intro$planning_area_sf
  
  # ===========================================================================
  # Session manager (v2 — ownership/publish/branch model, no practice mode)
  # ===========================================================================
  session_mgr <- sessionManagerServer(
    id                = 'session_mgr',
    username_r        = reactive(auth$username),
    district_r        = reactive(intro$district()),
    campaign_id_r     = reactive(intro$campaign_id()),
    district_ready_r  = reactive(intro$planning_ready())
  )
  
  # Convenience wrapper — the only path that writes to DB
  submit_fn <- function(stage, data) session_mgr$submit_stage(stage, data)
  
  # ===========================================================================
  # Orientation tab (unchanged)
  # ===========================================================================
  orientation <- orientationTabServer(
    'orientation',
    zone               = intro$zone,
    region             = intro$region,
    district           = intro$district,
    district_ready     = intro$planning_ready,
    active_tab         = reactive(input$main_tabs),
    submit_stage_fn    = submit_fn,
    restore_r          = reactive(NULL),
    subdivisions_r     = subdivisions_r,
    planning_area_sf_r = planning_area_sf
  )
  
  # ===========================================================================
  # Facility tab (v2 — IDP fetch/review/submit added)
  # ===========================================================================
  submitted_facilities <- reactiveVal(NULL)
  
  facility <- facilityTabServer(
    'facility',
    zone                 = intro$zone,
    region               = intro$region,
    district             = intro$district,
    district_ready       = intro$planning_ready,
    active_tab           = reactive(input$main_tabs),
    submitted_facilities = submitted_facilities,
    submit_stage_fn      = submit_fn,
    landmarks_r          = orientation$landmarks_r,
    subdivisions_r       = subdivisions_r,
    planning_area_sf_r   = planning_area_sf,
    restore_r            = reactive(NULL)
  )
  
  # ===========================================================================
  # Health area tab (unchanged — still submits stage "areas")
  # ===========================================================================
  health_area <- healthAreaTabServer(
    'health_area',
    zone               = intro$zone,
    region             = intro$region,
    district           = intro$district,
    district_ready     = intro$planning_ready,
    active_tab         = reactive(input$main_tabs),
    facility_data      = submitted_facilities,
    all_facilities_r   = facility$facility_data,
    landmarks_r        = orientation$landmarks_r,
    subdivisions_r     = subdivisions_r,
    planning_area_sf_r = planning_area_sf,
    submit_stage_fn    = submit_fn,
    restore_r          = reactive(NULL)
  )
  
  # ===========================================================================
  # Team area tab (new) — draws team boundaries within each finalized
  # health area. Note this takes district()/campaign_id() directly rather
  # than a planning_area_sf_r, since its "district_sf" per generation call
  # is actually a single health area's polygon, computed inside the module.
  # ===========================================================================
  team_area <- teamAreaTabServer(
    'team_area',
    district          = intro$district,
    campaign_id       = intro$campaign_id,
    district_ready    = intro$planning_ready,
    active_tab        = reactive(input$main_tabs),
    saved_dfa_sf_r    = health_area$saved_dfa_sf_r,
    all_facilities_r  = facility$facility_data,
    landmarks_r       = orientation$landmarks_r,
    submit_stage_fn   = submit_fn,
    restore_r         = reactive(NULL)
  )
  
  # microplan tab removed entirely — no server call for it.
  
  # ===========================================================================
  # Restore — fires only when session_mgr resolves to a version with
  # existing data (continue own draft / branch from shared / carry forward)
  # ===========================================================================
  observeEvent(session_mgr$restore_snapshot(), {
    snap <- session_mgr$restore_snapshot()
    if (is.null(snap)) return()
    cat('[app_server] restoring from version snapshot\n')
    
    orientation$restore_from_snapshot(snap)
    facility$restore_from_snapshot(snap)
    health_area$restore_from_snapshot(snap)
    team_area$restore_from_snapshot(snap)
    
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
  # Tab locking
  # ===========================================================================
  set_tab_enabled <- function(value, enabled, title = 'Select a campaign and district first') {
    session$sendCustomMessage(
      'set_tab_enabled',
      list(value = value, enabled = isTRUE(enabled), title = title)
    )
  }
  
  observe({
    ready <- isTRUE(intro$planning_ready())
    set_tab_enabled('tab_orientation',             ready)
    set_tab_enabled('tab_health_facility_mapping', ready)
    set_tab_enabled('tab_health_area_mapping',     ready)
    set_tab_enabled('tab_team_area_mapping',       ready)
  })
  
  observeEvent(input$main_tabs, {
    locked <- c('tab_orientation', 'tab_health_facility_mapping',
                'tab_health_area_mapping', 'tab_team_area_mapping')
    if (input$main_tabs %in% locked && !isTRUE(intro$planning_ready())) {
      updateTabsetPanel(session, 'main_tabs', selected = 'tab_intro')
      showNotification('Select a campaign and district on the Introduction tab first.',
                       type = 'message', duration = 3)
    }
  }, ignoreInit = TRUE)
}
