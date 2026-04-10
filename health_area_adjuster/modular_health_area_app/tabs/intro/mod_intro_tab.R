introTabUI <- function(id, zone_choices) {
  ns <- NS(id)
  
  tagList(
    div(
      style = 'padding: 20px; max-width: 720px;',
      h3('Introduction'),
      p('Select a district to begin.'),
      tags$hr(),
      div(class = 'mini-label', 'Zone'),
      selectInput(
        ns('zone'),
        NULL,
        choices = c(setNames('', ''), zone_choices),
        selected = ''
      ),
      div(class = 'mini-label', 'Region'),
      selectInput(
        ns('region'),
        NULL,
        choices = setNames('', ''),
        selected = ''
      ),
      div(class = 'mini-label', 'District'),
      selectInput(
        ns('district'),
        NULL,
        choices = setNames('', ''),
        selected = ''
      ),
      tags$div(
        style = 'margin-top: 12px; color: #666666; font-size: 13px;',
        'After selecting a district, the Health Facility Mapping and Health Area Mapping tabs will become available.'
      )
    )
  )
}

introTabServer <- function(id, districts_shp) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$zone, {
      if (!nzchar(input$zone %||% '')) {
        updateSelectInput(session, 'region', choices = setNames('', ''), selected = '')
        updateSelectInput(session, 'district', choices = setNames('', ''), selected = '')
        return()
      }
      
      regions <- districts_shp |>
        dplyr::filter(zone_name == input$zone) |>
        dplyr::pull(region_name) |>
        as.character() |>
        unique() |>
        sort()
      
      updateSelectInput(
        session,
        'region',
        choices = c(setNames('', ''), regions),
        selected = ''
      )
      
      updateSelectInput(
        session,
        'district',
        choices = setNames('', ''),
        selected = ''
      )
    }, ignoreInit = FALSE)
    
    observeEvent(list(input$zone, input$region), {
      if (!nzchar(input$zone %||% '') || !nzchar(input$region %||% '')) {
        updateSelectInput(session, 'district', choices = setNames('', ''), selected = '')
        return()
      }
      
      dists <- districts_shp |>
        dplyr::filter(zone_name == input$zone, region_name == input$region) |>
        dplyr::pull(district_name) |>
        as.character() |>
        unique() |>
        sort()
      
      updateSelectInput(
        session,
        'district',
        choices = c(setNames('', ''), dists),
        selected = ''
      )
    }, ignoreInit = FALSE)
    
    list(
      zone = reactive(input$zone),
      region = reactive(input$region),
      district = reactive(input$district),
      district_ready = reactive(nzchar(input$district %||% ''))
    )
  })
}