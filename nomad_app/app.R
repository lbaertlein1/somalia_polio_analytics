# app.R
# Note: global.R is sourced automatically by Shiny — do not source it here

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600&family=JetBrains+Mono:wght@400;500&display=swap"
    )
  ),
  
  # Header
  div(class = "app-header",
      div(class = "header-logo", "\U0001F3D5"),
      div(class = "header-title", "Nomadic Mapping \u2014 Polio Analytics"),
      div(class = "header-pulled",
          paste0("Data: ", format(nomad_data$pulled_at, "%d %b %Y %H:%M")))
  ),
  
  # Year filter
  div(class = "filter-bar",
      span(class = "filter-label", "Year"),
      div(class = "pill-group",
          tags$button("All time", class = "pill-btn active",  onclick = "setYear('all',  this)"),
          tags$button("2024",     class = "pill-btn",         onclick = "setYear('2024', this)"),
          tags$button("2025",     class = "pill-btn",         onclick = "setYear('2025', this)"),
          tags$button("2026",     class = "pill-btn",         onclick = "setYear('2026', this)")
      ),
      span(id = "date-stamp", class = "date-stamp", "Jun 2024 \u2013 present")
  ),
  
  # KPI bar
  kpi_ui("kpi"),
  
  # Tab bar
  div(class = "tab-bar",
      tags$button("Movement Patterns",       class = "tab-btn active", onclick = "showTab('mov', this)"),
      tags$button("Zero Dose & Vaccination", class = "tab-btn",        onclick = "showTab('zd',  this)"),
      tags$button("Access & Challenges",     class = "tab-btn",        onclick = "showTab('acc', this)"),
      tags$button("Field Teams Report",      class = "tab-btn",        onclick = "showTab('rep', this)")
  ),
  
  # Tab panels
  div(class = "app-wrap",
      div(id = "tab-mov", class = "tab-content active", movement_ui("mov")),
      div(id = "tab-zd",  class = "tab-content",        zerodose_ui("zd")),
      div(id = "tab-acc", class = "tab-content",        access_ui("acc")),
      div(id = "tab-rep", class = "tab-content",        report_ui("rep"))
  ),
  
  # JavaScript
  tags$script(HTML("
    var DATE_LABELS = {
      'all':  'Jun 2024 \u2013 present',
      '2024': 'January \u2013 December 2024',
      '2025': 'January \u2013 December 2025',
      '2026': 'January \u2013 April 2026'
    };

    function showTab(id, btn) {
      document.querySelectorAll('.tab-content').forEach(function(el) {
        el.classList.remove('active');
      });
      document.querySelectorAll('.tab-btn').forEach(function(el) {
        el.classList.remove('active');
      });
      document.getElementById('tab-' + id).classList.add('active');
      btn.classList.add('active');
      // Invalidate all Leaflet maps so they redraw correctly
      setTimeout(function() {
        window.dispatchEvent(new Event('resize'));
        if (window.HTMLWidgets) {
          document.querySelectorAll('.leaflet').forEach(function(el) {
            var map = el._leaflet_map;
            if (map) map.invalidateSize();
          });
        }
      }, 150);
    }

    function setYear(yr, btn) {
      document.querySelectorAll('.pill-btn').forEach(function(el) {
        el.classList.remove('active');
      });
      btn.classList.add('active');
      document.getElementById('date-stamp').textContent = DATE_LABELS[yr];
      Shiny.setInputValue('year_filter', yr, {priority: 'event'});
    }

    // File download handler from Shiny server
    Shiny.addCustomMessageHandler('download_file', function(msg) {
      var blob = new Blob([msg.content], {type: 'text/plain'});
      var a = document.createElement('a');
      a.href = URL.createObjectURL(blob);
      a.download = msg.filename;
      a.click();
    });

    // Set initial year value so all outputs render on load
    Shiny.addCustomMessageHandler('init_year', function(msg) {
      Shiny.setInputValue('year_filter', 'all', {priority: 'event'});
    });
  "))
)

server <- function(input, output, session) {
  
  # Fire initial year value so all outputs render immediately on load
  observe({
    session$sendCustomMessage("init_year", list())
  })
  
  year_r <- reactive({
    if (is.null(input$year_filter) || input$year_filter == "") "all"
    else input$year_filter
  })
  
  # Generate all AI insights on load / year change
  insights <- generate_all_insights(nomad_data, year_r, session)
  
  kpi_server(     "kpi", nomad_data, year_r)
  movement_server("mov", nomad_data, year_r, insights)
  zerodose_server("zd",  nomad_data, year_r, insights)
  access_server(  "acc", nomad_data, year_r, insights)
  report_server(  "rep", nomad_data, year_r, insights)
}

shinyApp(ui, server)