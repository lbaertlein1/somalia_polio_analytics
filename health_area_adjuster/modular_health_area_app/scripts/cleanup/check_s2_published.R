suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 220, warn = -1)
T <- DBI::dbGetQuery(pool, "SELECT team_version_id tv, district_name d, saved_team_sf::text s, smoothed_team_sf::text m FROM team_area_versions WHERE campaign_id = 10 AND is_shared AND archived_at IS NULL ORDER BY 1")
sf::sf_use_s2(TRUE)
for (i in seq_len(nrow(T))) for (col in c('s', 'm')) { x <- geojsonsf::geojson_sf(T[[col]][i])
  r <- s2::s2_is_valid_detail(x$geometry); bad <- which(!r$is_valid)
  if (length(bad)) cat(sprintf('tv %d %s %s: %s\n', T$tv[i], T$d[i], col, paste(sprintf('%s -> %s', x$dfa_name[bad], substr(r$reason[bad], 1, 90)), collapse = ' || '))) }
cat('checked', nrow(T), 'published team layouts\n')
