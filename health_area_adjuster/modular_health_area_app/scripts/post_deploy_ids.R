# Run ONCE, right after deploying the health-area / team / district ID code.
#
# Fills IDs on anything saved by the previous app version (which didn't set
# them), then makes the team-area ID columns required. Safe to re-run: it
# only touches rows still missing IDs.
#
#   Rscript scripts/post_deploy_ids.R      (from the app directory; needs .env
#                                           with DB_* and POSTGRES_PASSWORD)

dotenv::load_dot_env('.env')
suppressMessages({ library(sf); source('helpers/mod_db_v2.R') })
sf::sf_use_s2(FALSE)

con <- DBI::dbConnect(RPostgres::Postgres(), host = Sys.getenv('DB_HOST'), port = as.integer(Sys.getenv('DB_PORT', '5432')),
                      dbname = Sys.getenv('DB_NAME'), user = 'postgres', password = Sys.getenv('POSTGRES_PASSWORD'),
                      sslmode = Sys.getenv('DB_SSL', 'require'), bigint = 'integer')

# 1. Health area IDs (+ coordination site facility_id), oldest first so a
#    branch inherits its parent's IDs.
V <- DBI::dbGetQuery(con, "SELECT version_id, branched_from_id FROM mapping_versions
                           WHERE dfa_names IS NOT NULL AND health_area_ids IS NULL ORDER BY version_id")
for (i in seq_len(nrow(V))) {
  if (!is.na(V$branched_from_id[i]))
    DBI::dbExecute(con, "UPDATE mapping_versions c SET health_area_ids = p.health_area_ids FROM mapping_versions p
                         WHERE c.version_id = $1 AND p.version_id = $2 AND c.health_area_ids IS NULL",
                   list(V$version_id[i], V$branched_from_id[i]))
  .resolve_health_area_ids(con, V$version_id[i])
}
cat('health area versions given IDs:', nrow(V), '\n')

# 2. Team areas: health_area_id / facility_id from their pinned version.
for (v in DBI::dbGetQuery(con, "SELECT DISTINCT based_on_health_area_version_id v FROM team_area_versions
                                WHERE health_area_id IS NULL OR facility_id IS NULL")$v)
  .sync_team_area_ids(con, v)

# 3. Team IDs, oldest first (a branch inherits its parent's).
T <- DBI::dbGetQuery(con, "SELECT team_version_id v, branched_from_id p FROM team_area_versions
                           WHERE team_names IS NOT NULL AND team_ids IS NULL ORDER BY 1")
for (i in seq_len(nrow(T))) {
  if (!is.na(T$p[i]))
    DBI::dbExecute(con, "UPDATE team_area_versions c SET team_ids = p.team_ids FROM team_area_versions p
                         WHERE c.team_version_id = $1 AND p.team_version_id = $2", list(T$v[i], T$p[i]))
  .resolve_team_ids(con, T$v[i])
}
cat('team area versions given team IDs:', nrow(T), '\n')

# 4. Require the team-area IDs from now on.
left <- DBI::dbGetQuery(con, "SELECT team_version_id, district_name, health_area_name FROM team_area_versions
                              WHERE health_area_id IS NULL OR facility_id IS NULL")
if (nrow(left) > 0) {
  print(left)
  stop('These team-area versions still have no health area / facility ID -- fix them, then re-run.')
}
DBI::dbExecute(con, "ALTER TABLE team_area_versions ALTER COLUMN health_area_id SET NOT NULL")
DBI::dbExecute(con, "ALTER TABLE team_area_versions ALTER COLUMN facility_id    SET NOT NULL")
DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_tav_ha_id ON team_area_versions(health_area_id, campaign_id)")
cat('team-area health_area_id / facility_id are now required.\n')
DBI::dbDisconnect(con)
