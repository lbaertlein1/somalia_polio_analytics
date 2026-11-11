# Unpublish every published team layout of district D (campaign 10) with the app's own db_unshare_team_area_version(); this clears
# db_district_has_locked_team_areas() so a new health-area version can be published. Layouts are kept (owners still see them).
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 220); S <- Sys.getenv('WORK'); if (!nzchar(S)) stop('set WORK to a working folder')
D <- Sys.getenv('D'); if (!nzchar(D)) stop('set D to the district name')  # writes a CSV of what was unpublished to WORK
cat('locked before:', db_district_has_locked_team_areas(pool, 10L, D), '\n')
T <- DBI::dbGetQuery(pool, "SELECT team_version_id, health_area_name, owner_username, version_number, based_on_health_area_version_id hv, shared_at
  FROM team_area_versions WHERE campaign_id = 10 AND district_name = $1 AND is_shared AND archived_at IS NULL ORDER BY health_area_name", list(D))
cat('published team layouts:', nrow(T), '\n'); print(T, row.names = FALSE)
write.csv(T, file.path(S, sprintf('%s_unpublished_team_layouts_%s.csv', D, format(Sys.time(), '%Y%m%d_%H%M%S'))), row.names = FALSE)
for (tv in T$team_version_id) db_unshare_team_area_version(pool, tv)
cat('locked after:', db_district_has_locked_team_areas(pool, 10L, D), '\n')
print(DBI::dbGetQuery(pool, "SELECT version_id, owner_username, version_number, is_shared FROM mapping_versions WHERE campaign_id = 10 AND district_name = $1 AND is_shared AND archived_at IS NULL", list(D)))
