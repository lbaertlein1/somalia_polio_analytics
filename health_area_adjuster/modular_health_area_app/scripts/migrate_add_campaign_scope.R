# =============================================================================
# migrate_add_campaign_scope.R
#
# Adds the campaign-scope columns to mapping_versions on an already-
# existing database (create_db_v2.R's own ALTER TABLE only runs when
# create_db_v2.R itself is executed, not automatically on app startup --
# same gap as seed_stratified_settings.R and seed_urban_approx_setting.R
# earlier this session). Safe to run against a live database and safe
# to re-run: every ADD COLUMN uses IF NOT EXISTS, so a second run is a
# clean no-op.
#
# Does NOT touch any existing campaign_districts.mapping_scope values --
# any row already storing the old 'urban_only' value is renamed to
# 'partial' below in a SEPARATE, explicit UPDATE (also idempotent -- a
# second run affects zero rows once the first has renamed them all).
# =============================================================================

library(DBI)
library(RPostgres)
library(dotenv)

dotenv::load_dot_env('.env_v2')
DB_HOST <- Sys.getenv('DB_HOST', '23.239.19.115')
DB_PORT <- as.integer(Sys.getenv('DB_PORT', '5432'))
DB_NAME <- Sys.getenv('DB_NAME', 'somalia_health_areas_v2')
POSTGRES_PASSWORD <- Sys.getenv('POSTGRES_PASSWORD', '')
if (!nzchar(POSTGRES_PASSWORD)) stop('POSTGRES_PASSWORD is not set. Add it to .env_v2 before running this script.')

con <- dbConnect(RPostgres::Postgres(), host = DB_HOST, port = DB_PORT, dbname = DB_NAME,
                 user = 'postgres', password = POSTGRES_PASSWORD, sslmode = 'require')
on.exit(dbDisconnect(con), add = TRUE)

cat('=== Adding campaign-scope columns to mapping_versions ===\n')
dbExecute(con, "
  ALTER TABLE mapping_versions
    ADD COLUMN IF NOT EXISTS scope_saved_dfa_sf         JSONB,
    ADD COLUMN IF NOT EXISTS scope_dfa_names            JSONB,
    ADD COLUMN IF NOT EXISTS scope_current_assignments  JSONB,
    ADD COLUMN IF NOT EXISTS scope_locked_at            TIMESTAMPTZ,
    ADD COLUMN IF NOT EXISTS has_scope                  BOOLEAN NOT NULL DEFAULT FALSE
")
cat('Done.\n\n')

cat('=== Renaming campaign_districts.mapping_scope: urban_only -> partial ===\n')
before <- dbGetQuery(con, "SELECT mapping_scope, COUNT(*) AS n FROM campaign_districts GROUP BY mapping_scope")
cat('Before:\n'); print(before)

n <- dbExecute(con, "UPDATE campaign_districts SET mapping_scope = 'partial' WHERE mapping_scope = 'urban_only'")
cat(sprintf('Renamed %d row(s).\n\n', n))

after <- dbGetQuery(con, "SELECT mapping_scope, COUNT(*) AS n FROM campaign_districts GROUP BY mapping_scope")
cat('After:\n'); print(after)

