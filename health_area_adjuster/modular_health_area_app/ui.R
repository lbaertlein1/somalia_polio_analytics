app_ui <- function() {
  fluidPage(
    shinyjs::useShinyjs(),
    
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
      
      tags$script(HTML("
        $(document).ready(function() {

          // NOTE: the v1 version of this file auto-enabled the intro
          // Continue button here based on the district <select> alone.
          // v2 requires BOTH a campaign and a district, and that's now
          // handled correctly on the R side via shinyjs::toggleState in
          // mod_intro_tab_v2.R (driven by planning_ready(), which checks
          // both). That old watcher is deliberately removed — leaving it
          // in would re-enable the button on district selection alone and
          // fight the correct R-side logic.

          // Header tab click
          $(document).on('click', '.hdr-tab:not(.hdr-tab-locked)', function() {
            var val = $(this).data('tab');
            $('#main_tabs a[data-value=\"' + val + '\"]').tab('show');
          });

          // Sync header active state + invalidate maps
          $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
            var val = $(e.target).data('value');
            $('.hdr-tab').removeClass('hdr-tab-active');
            $('.hdr-tab[data-tab=\"' + val + '\"]').addClass('hdr-tab-active');
            [100, 400].forEach(function(d) {
              setTimeout(function() {
                if (window.paintApps) {
                  Object.keys(window.paintApps).forEach(function(k) {
                    var a = window.paintApps[k];
                    if (a && a.map) a.map.invalidateSize();
                  });
                }
                window.dispatchEvent(new Event('resize'));
              }, d);
            });
          });

          // Lock / unlock header tabs
          Shiny.addCustomMessageHandler('set_tab_enabled', function(msg) {
            var $t = $('.hdr-tab[data-tab=\"' + msg.value + '\"]');
            if (msg.enabled) {
              $t.removeClass('hdr-tab-locked');
              $t.removeAttr('title');
            } else {
              $t.addClass('hdr-tab-locked');
              $t.attr('title', msg.title || 'Select a campaign and district first');
            }
          });

          // Switch to a tab programmatically (used by Continue buttons)
          Shiny.addCustomMessageHandler('switch_tab', function(msg) {
            $('#main_tabs a[data-value=' + msg.value + ']').tab('show');
          });

          // Insert admin tab dynamically
          Shiny.addCustomMessageHandler('insert_nav_tab', function(msg) {
            if ($('.hdr-tab[data-tab=\"' + msg.value + '\"]').length) return;
            var $btn = $('<button class=\"hdr-tab\" data-tab=\"' + msg.value + '\">' + msg.label + '</button>');
            $('#header-tabs').append($btn);
          });

          // Facility loading overlay
          Shiny.addCustomMessageHandler('toggle_facility_loading', function(x) {
            var el = document.getElementById(x.id);
            if (el) el.style.display = x.show ? 'flex' : 'none';
          });

        });
      "))
    ),
    
    authUI('auth'),
    
    shinyjs::hidden(
      div(
        id = 'main_app',
        
        # Header bar
        div(
          id = 'app-header',
          
          div(
            id = 'header-title',
            tags$span('Somalia District Health Area Planning',
                      style = 'font-size: 14px; line-height: 1;'),
            tags$span('Vaccination Campaigns & Outreach',
                      style = 'color: #0d9488; font-size: 14px; line-height: 1;')
          ),
          
          div(
            id = 'header-tabs',
            tags$button(class = 'hdr-tab hdr-tab-active', `data-tab` = 'tab_intro',         'Introduction'),
            tags$button(class = 'hdr-tab hdr-tab-locked', `data-tab` = 'tab_orientation',   'Landmarks'),
            tags$button(class = 'hdr-tab hdr-tab-locked', `data-tab` = 'tab_health_facility_mapping', 'Facilities'),
            tags$button(class = 'hdr-tab hdr-tab-locked', `data-tab` = 'tab_health_area_mapping',     'Health Areas'),
            tags$button(class = 'hdr-tab hdr-tab-locked', `data-tab` = 'tab_team_area_mapping',       'Team Areas')
          )
        ),
        
        tabsetPanel(
          id   = 'main_tabs',
          type = 'tabs',
          
          tabPanel(title = 'Introduction',    value = 'tab_intro',
                   introTabUI('intro')),
          
          tabPanel(title = 'Landmarks',       value = 'tab_orientation',
                   orientationTabUI('orientation')),
          
          tabPanel(title = 'Facilities',      value = 'tab_health_facility_mapping',
                   facilityTabUI('facility')),
          
          tabPanel(title = 'Health Areas',    value = 'tab_health_area_mapping',
                   healthAreaTabUI('health_area')),
          
          tabPanel(title = 'Team Areas',      value = 'tab_team_area_mapping',
                   teamAreaTabUI('team_area'))
        )
      )
    )
  )
}
