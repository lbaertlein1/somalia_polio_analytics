facilityTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns('facility_wrap'),
      style = 'padding: 20px;',
      h3('Health Facility Mapping'),
      p('Instructions to locate all health facilities in the district will be added here later.')
    )
  )
}

facilityTabServer <- function(id, district_name) {
  moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
