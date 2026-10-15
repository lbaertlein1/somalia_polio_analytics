# =============================================================================
# deploy.R -- publish the app to shinyapps.io (account emro-polio-analytics)
#
# Run from the repo-root R project (the library the app runs with locally):
#   source('vaccine_buddy/deploy.R')
# rsconnect works out the packages from the app's code and the versions
# installed in that library.
#
# Only what the running app needs is uploaded: no scripts/ (they run on the
# VM or by hand). The app reads its settings from .env, so a copy is
# uploaded -- without the database superuser password, the first-admin
# password or the ODK login, which only scripts and the VM job use. The
# upload is assembled in a temporary folder that is deleted afterwards;
# your own .env is not changed.
# =============================================================================

app_dir <- if (file.exists('vaccine_buddy/app.R')) 'vaccine_buddy' else '.'
app_files <- c(
  'app.R', 'global.R',
  file.path('R', list.files(file.path(app_dir, 'R'))),
  file.path('www', list.files(file.path(app_dir, 'www'), recursive = TRUE)),
  'data/som_u5_population_2026_100m.tif',
  # district boundaries, for districts with no health areas drawn
  file.path('data/boundary_sets', list.files(file.path(app_dir, 'data/boundary_sets'), pattern = '^districts_shp[.]Rds$', recursive = TRUE))
)
missing <- app_files[!file.exists(file.path(app_dir, app_files))]
if (length(missing) || !file.exists(file.path(app_dir, '.env')))
  stop('Missing before deploy: ', paste(c(missing, if (!file.exists(file.path(app_dir, '.env'))) '.env'), collapse = ', '))

stage <- file.path(tempdir(), 'vaccine_buddy_deploy')
unlink(stage, recursive = TRUE)
for (f in app_files) {
  dir.create(dirname(file.path(stage, f)), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(app_dir, f), file.path(stage, f))
}
env <- readLines(file.path(app_dir, '.env'), warn = FALSE)
env <- env[!grepl('^(POSTGRES_PASSWORD|ADMIN_PASSWORD|ODK_USERNAME|ODK_PASSWORD)=', env)]
writeLines(env, file.path(stage, '.env'))

tryCatch(
  rsconnect::deployApp(
    appDir      = stage,
    appFiles    = c(app_files, '.env'),
    appName     = 'vaccine_buddy',
    account     = 'emro-polio-analytics',
    server      = 'shinyapps.io',
    forceUpdate = TRUE,
    lint        = FALSE,
    launch.browser = FALSE
  ),
  finally = unlink(stage, recursive = TRUE)
)
