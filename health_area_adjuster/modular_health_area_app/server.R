app_server <- function(input, output, session) {
<<<<<<< HEAD
  intro <- introTabServer(
    'intro',
    districts_shp = districts_shp
  )
  
=======
  intro <- introTabServer('intro', districts_shp = districts_shp)
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
  submitted_facilities <- reactiveVal(NULL)
  
  facility <- facilityTabServer(
    'facility',
    zone = intro$zone,
    region = intro$region,
    district = intro$district,
    district_ready = intro$district_ready,
    submitted_facilities = submitted_facilities
  )
  
<<<<<<< HEAD
  health_area <- healthAreaTabServer(
    'health_area',
=======
  healthAreaTabServer(
    "health_area",
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
    zone = intro$zone,
    region = intro$region,
    district = intro$district,
    district_ready = intro$district_ready,
    active_tab = reactive(input$main_tabs),
    facility_data = submitted_facilities
  )
  
  set_tab_enabled <- function(value, enabled, title = 'Select a District') {
    session$sendCustomMessage(
      'set_tab_enabled',
      list(
        value = value,
        enabled = isTRUE(enabled),
        title = title
      )
    )
  }
  
  observe({
    ready <- isTRUE(intro$district_ready())
    
    set_tab_enabled('tab_health_facility_mapping', ready)
    set_tab_enabled('tab_health_area_mapping', ready)
  })
  
  observeEvent(input$main_tabs, {
    if (
      input$main_tabs %in% c('tab_health_facility_mapping', 'tab_health_area_mapping') &&
      !isTRUE(intro$district_ready())
    ) {
      updateTabsetPanel(
        session,
        'main_tabs',
        selected = 'tab_intro'
      )
      
      showNotification(
        'Select a district on the Introduction tab first.',
        type = 'message',
        duration = 3
      )
    }
  }, ignoreInit = TRUE)
  
<<<<<<< HEAD
  # Optional debug hooks
  observe({
    req(health_area$has_scene())
    cat('[app_server] health area scene ready\n')
  })
  
  observe({
    fp <- health_area$friction_path()
    if (!is.null(fp) && nzchar(fp)) {
      cat('[app_server] friction path:', fp, '\n')
    }
  })
}
=======
  
}
>>>>>>> 87d259b (built out HF -> health area -> area adjustment workflow)
