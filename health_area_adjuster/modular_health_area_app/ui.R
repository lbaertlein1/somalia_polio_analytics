app_ui <- function() {
  fluidPage(
    tags$head(
      tags$link(
        rel = 'stylesheet',
        href = 'https://unpkg.com/leaflet@1.9.4/dist/leaflet.css',
        integrity = 'sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY=',
        crossorigin = ''
      ),
      tags$script(
        src = 'https://unpkg.com/leaflet@1.9.4/dist/leaflet.js',
        integrity = 'sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=',
        crossorigin = ''
      ),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
      tags$script(src = 'paint-app.js'),
      tags$style(HTML("
        .nav-tabs li.disabled a {
          color: #999999 !important;
          background: #F2F2F2 !important;
          cursor: not-allowed !important;
          pointer-events: auto !important;
        }
        .nav-tabs li.disabled a:hover {
          background: #F2F2F2 !important;
        }
      ")),
      tags$script(HTML("
  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
    setTimeout(function() {
      if (window.paintApps) {
        Object.keys(window.paintApps).forEach(function(k) {
          var app = window.paintApps[k];
          if (app && app.map) {
            app.map.invalidateSize();
          }
        });
      }
      window.dispatchEvent(new Event('resize'));
    }, 100);

    setTimeout(function() {
      if (window.paintApps) {
        Object.keys(window.paintApps).forEach(function(k) {
          var app = window.paintApps[k];
          if (app && app.map) {
            app.map.invalidateSize();
          }
        });
      }
      window.dispatchEvent(new Event('resize'));
    }, 400);
  });
"))
    ),
    tabsetPanel(
      id = 'main_tabs',
      type = 'tabs',
      tabPanel(
        title = 'Introduction',
        value = 'tab_intro',
        introTabUI('intro', zone_choices = zone_choices)
      ),
      tabPanel(
        title = 'Health Facility Mapping',
        value = 'tab_health_facility_mapping',
        facilityTabUI('facility')
      ),
      tabPanel(
        title = 'Health Area Mapping',
        value = 'tab_health_area_mapping',
        healthAreaTabUI('health_area')
      )
    )
  )
}
