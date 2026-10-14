# =============================================================================
# deploy.R -- publish the app to shinyapps.io (account emro-polio-analytics)
#
# Run from the repo-root R project (the library the app runs with locally):
#   source('vaccine_buddy/deploy.R')
# rsconnect works out the packages from the app's code and the versions
# installed in that library.
#
# Only what the running app needs is uploaded: no scripts/ (they run on the
# VM or by hand). .env is included, as for the health area adjuster -- the
# app reads its database and settings from it.
# =============================================================================

app_dir <- if (file.exists('vaccine_buddy/app.R')) 'vaccine_buddy' else '.'
app_files <- c(
  'app.R', 'global.R', '.env',
  list.files(file.path(app_dir, 'R'), full.names = FALSE) |> (\(f) file.path('R', f))(),
  list.files(file.path(app_dir, 'www'), recursive = TRUE) |> (\(f) file.path('www', f))(),
  'data/som_u5_population_2026_100m.tif'
)
missing <- app_files[!file.exists(file.path(app_dir, app_files))]
if (length(missing)) stop('Missing before deploy: ', paste(missing, collapse = ', '))

rsconnect::deployApp(
  appDir      = app_dir,
  appFiles    = app_files,
  appName     = 'vaccine_buddy',
  account     = 'emro-polio-analytics',
  server      = 'shinyapps.io',
  forceUpdate = TRUE,
  lint        = FALSE
)
