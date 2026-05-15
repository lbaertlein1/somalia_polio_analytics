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
      adminTabServer('admin', districts_shp = districts_shp)
    }
  }, ignoreInit = TRUE, once = TRUE)
  
  # ===========================================================================
  # Session manager
  # ===========================================================================
  session_mgr <- sessionManagerServer(
    id               = 'session_mgr',
    username_r       = reactive(auth$username),
    district_r       = reactive(intro$district()),
    district_ready_r = reactive(intro$district_ready())
  )
  
  # Convenience wrapper — the only path that writes to DB
  submit_fn <- function(stage, data) session_mgr$submit_stage(stage, data)
  
  # ===========================================================================
  # Intro tab
  # ===========================================================================
  intro <- introTabServer(
    'intro',
    districts_shp       = districts_shp,
    allowed_districts_r = reactive(auth$allowed_districts)
  )
  
  # ===========================================================================
  # Orientation tab
  # ===========================================================================
  orientation <- orientationTabServer(
    'orientation',
    zone             = intro$zone,
    region           = intro$region,
    district         = intro$district,
    district_ready   = intro$district_ready,
    active_tab       = reactive(input$main_tabs),
    submit_stage_fn  = submit_fn,
    restore_r        = reactive(NULL)
  )
  
  # ===========================================================================
  # Facility tab
  # ===========================================================================
  submitted_facilities <- reactiveVal(NULL)
  
  facility <- facilityTabServer(
    'facility',
    zone                 = intro$zone,
    region               = intro$region,
    district             = intro$district,
    district_ready       = intro$district_ready,
    active_tab           = reactive(input$main_tabs),
    submitted_facilities = submitted_facilities,
    submit_stage_fn      = submit_fn,
    landmarks_r          = orientation$landmarks_r,
    restore_r            = reactive(NULL)
  )
  
  # ===========================================================================
  # Health area tab
  # ===========================================================================
  health_area <- healthAreaTabServer(
    'health_area',
    zone             = intro$zone,
    region           = intro$region,
    district         = intro$district,
    district_ready   = intro$district_ready,
    active_tab       = reactive(input$main_tabs),
    facility_data    = submitted_facilities,
    all_facilities_r = facility$facility_data,
    landmarks_r      = orientation$landmarks_r,
    submit_stage_fn  = submit_fn,
    restore_r        = reactive(NULL)
  )
  
  # ===========================================================================
  # Microplan tab
  # ===========================================================================
  microplan <- microplanTabServer(
    'microplan',
    zone                = intro$zone,
    region              = intro$region,
    district            = intro$district,
    district_ready      = intro$district_ready,
    saved_dfa_sf_r      = health_area$saved_dfa_sf_r,
    pop_table_r         = health_area$pop_table_r,
    facility_data_r     = facility$facility_data,
    submit_stage_fn     = submit_fn,
    areas_regenerated_r = health_area$areas_regenerated,
    changed_areas_r     = health_area$changed_areas,
    restore_r           = reactive(NULL)
  )
  
  # ===========================================================================
  # Restore — fires only when user clicks "Resume" in session manager modal
  # ===========================================================================
  observeEvent(session_mgr$restore_snapshot(), {
    snap <- session_mgr$restore_snapshot()
    if (is.null(snap)) return()
    cat('[app_server] restoring from submission snapshot\n')
    
    orientation$restore_from_snapshot(snap)
    facility$restore_from_snapshot(snap)
    health_area$restore_from_snapshot(snap)
    microplan$restore_from_snapshot(snap)
    
    # Restore submitted_facilities so the health area module receives the
    # correct SIA coordination sites as seed points for the painted scene.
    # Without this, the scene uses 5 random seeds instead of the saved sites.
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
  set_tab_enabled <- function(value, enabled, title = 'Select a district first') {
    session$sendCustomMessage(
      'set_tab_enabled',
      list(value = value, enabled = isTRUE(enabled), title = title)
    )
  }
  
  observe({
    ready <- isTRUE(intro$district_ready())
    set_tab_enabled('tab_orientation',             ready)
    set_tab_enabled('tab_health_facility_mapping', ready)
    set_tab_enabled('tab_health_area_mapping',     ready)
    set_tab_enabled('tab_microplan',               ready)
  })
  
  observeEvent(input$main_tabs, {
    locked <- c('tab_orientation', 'tab_health_facility_mapping',
                'tab_health_area_mapping', 'tab_microplan')
    if (input$main_tabs %in% locked && !isTRUE(intro$district_ready())) {
      updateTabsetPanel(session, 'main_tabs', selected = 'tab_intro')
      showNotification('Select a district on the Introduction tab first.',
                       type = 'message', duration = 3)
    }
  }, ignoreInit = TRUE)
}