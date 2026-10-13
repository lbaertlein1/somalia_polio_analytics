options(repos = c(
  ropensci = "https://ropensci.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))
library(rsconnect)

rsconnect::deployApp(
  appDir      = ".",
  appName     = "nomad_app_v2",
  account     = "emro-polio-analytics",
  forceUpdate = TRUE
)
