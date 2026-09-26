# =============================================================================
# replace_u5_population_file.R
#
# Builds the app's new under-5 population file from WorldPop:
# R2025A, Somalia 2026, 100m constrained, both sexes, age 0 + age 1-4
# (som_t_00 + som_t_01). Downloads the two files (~15 MB each) into
# data/worldpop_fresh/ if they are not there yet.
#
# Writes data/som_u5_population_2026_100m.tif (plus a _SOURCE.txt note).
# The old 2025 file is left untouched. Point the app and the friction build
# at the new file (see the message at the end).
#
# Run from the app folder. DRY_RUN = TRUE downloads and checks, writes nothing.
# =============================================================================

suppressPackageStartupMessages(library(terra))

DRY_RUN <- FALSE

YEAR      <- 2026
OLD_FILE  <- 'data/som_u5_population_2025_100m.tif'
NEW_FILE  <- sprintf('data/som_u5_population_%d_100m.tif', YEAR)
FRESH_DIR <- 'data/worldpop_fresh'
BASE_URL  <- sprintf(paste0('https://worldpop-public-data.soton.ac.uk/GIS/AgeSex_structures/',
                            'Global_2015_2030/R2025A/%d/SOM/v1/100m/constrained/'), YEAR)
NAMES     <- sprintf(c('som_t_00_%d_CN_100m_R2025A_v1.tif',    # both sexes, age 0 (under 1)
                       'som_t_01_%d_CN_100m_R2025A_v1.tif'),   # both sexes, age 1-4
                     YEAR)

fmt <- function(x) format(round(x), big.mark = ',')

if (file.exists(NEW_FILE)) stop('Already exists: ', NEW_FILE)

# --- Download --------------------------------------------------------------
dir.create(FRESH_DIR, recursive = TRUE, showWarnings = FALSE)
files <- file.path(FRESH_DIR, NAMES)
options(timeout = max(600, getOption('timeout')))
for (i in seq_along(files)) if (!file.exists(files[i])) {
  message('Downloading ', NAMES[i], ' ...')
  download.file(paste0(BASE_URL, NAMES[i]), files[i], mode = 'wb', quiet = TRUE)
}

# --- Sum and check -----------------------------------------------------------
fresh <- app(rast(files), sum)          # NA only where both ages are NA
names(fresh) <- 'u5_pop'
g <- global(fresh, c('sum', 'max', 'notNA'), na.rm = TRUE)
message(sprintf('%d U5 (R2025A): national total %s | cells with data %s | largest cell %.1f',
                YEAR, fmt(g$sum), fmt(g$notNA), g$max))

if (file.exists(OLD_FILE)) {
  old <- rast(OLD_FILE)
  if (!compareGeom(old, fresh, stopOnError = FALSE))
    stop('The ', YEAR, ' grid differs from the current app file -- stopping.')
  old_total <- global(old, 'sum', na.rm = TRUE)[[1]]
  message(sprintf('Current app file (2025) total: %s | %d / 2025 ratio %.3f',
                  fmt(old_total), YEAR, g$sum / old_total))
}
stopifnot(g$sum > 3.3e6, g$sum < 4.2e6)   # sanity: UN WPP 0-4 for Somalia is ~3.6-3.8M

# --- Write -------------------------------------------------------------------
if (DRY_RUN) {
  message('DRY RUN -- would write ', NEW_FILE)
} else {
  writeRaster(fresh, NEW_FILE, datatype = 'FLT4S', gdal = c('COMPRESS=DEFLATE'))
  chk <- global(rast(NEW_FILE), 'sum', na.rm = TRUE)[[1]]
  stopifnot(abs(chk / g$sum - 1) < 1e-6)
  writeLines(c(
    basename(NEW_FILE),
    paste('Created:', format(Sys.time(), '%Y-%m-%d %H:%M')),
    sprintf('Source: WorldPop Global2 R2025A, SOM %d, 100m constrained, som_t_00 + som_t_01 (both sexes, age 0-4)', YEAR),
    paste('URL:', BASE_URL),
    paste('National total:', fmt(chk))
  ), sub('\\.tif$', '_SOURCE.txt', NEW_FILE))
  message('Written ', NEW_FILE, ' (national total ', fmt(chk), ').',
          '\nNow change the population path to "', NEW_FILE, '" in:',
          '\n  global.R                            worldpop_t_u1_1to4_file',
          '\n  build_national_friction_surface.R   worldpop_file',
          '\n  check_u5_population.R               APP_FILE')
}