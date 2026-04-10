app_server <- function(input, output, session) {
  intro <- introTabServer('intro', districts_shp = districts_shp)
  
  facility <- facilityTabServer(
    'facility',
    zone = intro$zone,
    region = intro$region,
    district = intro$district,
    district_ready = intro$district_ready
  )
  
  healthAreaTabServer(
    'health_area',
    zone = intro$zone,
    region = intro$region,
    district = intro$district,
    district_ready = intro$district_ready,
    active_tab = reactive(input$main_tabs)
  )

  set_tab_enabled <- function(value, enabled, title = 'Select a District') {
    session$sendCustomMessage(
      'set_tab_enabled',
      list(value = value, enabled = isTRUE(enabled), title = title)
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
      updateTabsetPanel(session, 'main_tabs', selected = 'tab_intro')
      showNotification('Select a district on the Introduction tab first.', type = 'message', duration = 3)
    }
  }, ignoreInit = TRUE)
}
