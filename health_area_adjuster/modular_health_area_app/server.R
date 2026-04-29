app_server <- function(input, output, session) {
  
  
  # ===========================================================================
  # Auth
  # ===========================================================================
  auth <- authServer('auth', users_df, user_districts_df)
  
  
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
  # Nav info
  # ===========================================================================
  output$nav_info <- renderUI({
    req(isTRUE(auth$logged_in))
    district_label <- if (
      isTRUE(intro$district_ready()) && nzchar(intro$district() %||% '')
    ) intro$district() else NULL
    initials <- toupper(substr(auth$display_name %||% auth$username %||% '?', 1, 2))
    tagList(
      if (!is.null(district_label)) div(class = 'nav-district-tag', district_label),
      div(class = 'nav-user-av',
          title = paste0(auth$display_name, ' (', auth$role, ')'), initials)
    )
  })
  
  # ===========================================================================
  # Session manager
  # ===========================================================================
  session_mgr <- sessionManagerServer(
    id               = 'session_mgr',
    username_r       = reactive(auth$username),
    district_r       = reactive(intro$district()),
    district_ready_r = reactive(intro$district_ready())
  )
  
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
  # restore_r = reactive(NULL) — restoration handled centrally below
  # ===========================================================================
  orientation <- orientationTabServer(
    'orientation',
    zone             = intro$zone,
    region           = intro$region,
    district         = intro$district,
    district_ready   = intro$district_ready,
    save_snapshot_fn = function(data) session_mgr$save_snapshot(data, trigger = 'auto_orientation'),
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
    submitted_facilities = submitted_facilities,
    landmarks_r          = orientation$landmarks_r,
    save_snapshot_fn     = function(data) session_mgr$save_snapshot(data, trigger = 'auto_facility'),
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
    save_snapshot_fn = function(data) session_mgr$save_snapshot(data, trigger = 'auto_health_area'),
    restore_r        = reactive(NULL)
  )
  
  # ===========================================================================
  # Microplan tab
  # ===========================================================================
  microplan <- microplanTabServer(
    'microplan',
    zone             = intro$zone,
    region           = intro$region,
    district         = intro$district,
    district_ready   = intro$district_ready,
    saved_dfa_sf_r   = health_area$saved_dfa_sf_r,
    pop_table_r      = health_area$pop_table_r,
    facility_data_r  = facility$facility_data,
    save_snapshot_fn = function(data) session_mgr$save_snapshot(data, trigger = 'auto_microplan'),
    restore_r        = reactive(NULL)
  )
  
  # ===========================================================================
  # Manual save — collect full state
  # ===========================================================================
  session_mgr$set_collect_fn(function() {
    list(
      landmarks           = orientation$landmarks_r(),
      odk_sf              = facility$odk_sf_r(),
      app_sf              = facility$app_sf_r(),
      current_assignments = health_area$current_assignments_r(),
      saved_dfa_sf        = health_area$saved_dfa_sf_r(),
      dfa_names           = health_area$dfa_names_r(),
      planning_data       = microplan$planning_data_r()
    )
  })
  
  # ===========================================================================
  # Restore — SINGLE path: fires only when user clicks "Continue"
  # Calls restore_from_snapshot() directly on each module.
  # Modules have restore_r = reactive(NULL) so their internal observers
  # never fire — this is the only restore path.
  # ===========================================================================
  observeEvent(session_mgr$restore_snapshot(), {
    snap <- session_mgr$restore_snapshot()
    if (is.null(snap)) return()
    cat('[app_server] restoring session snapshot\n')
    orientation$restore_from_snapshot(snap)
    facility$restore_from_snapshot(snap)
    health_area$restore_from_snapshot(snap)
    microplan$restore_from_snapshot(snap)
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
  
  # ===========================================================================
  # Debug (remove in production)
  # ===========================================================================
  observe({
    req(health_area$has_scene())
    cat('[app_server] health area scene ready\n')
  })
  observe({
    fp <- health_area$friction_path()
    if (!is.null(fp) && nzchar(fp)) cat('[app_server] friction path:', fp, '\n')
  })
}
