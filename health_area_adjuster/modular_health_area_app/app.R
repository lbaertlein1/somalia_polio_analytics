# setwd(file.path(getwd(), "health_area_adjuster", "modular_health_area_app"))
# renv::install(file.path(getwd(), "bfsprop"))
# renv::snapshot()

# options(repos = c(
#   ropensci = "https://ropensci.r-universe.dev",
#   CRAN     = "https://cran.r-project.org"
# ))
source('global.R')
shinyApp(ui = app_ui(), server = app_server)