# setwd(file.path(getwd(), "health_area_adjuster/modular_health_area_app"))

source('global.R', local = TRUE)
source('ui.R', local = TRUE)
source('server.R', local = TRUE)

shinyApp(ui = app_ui(), server = app_server)
