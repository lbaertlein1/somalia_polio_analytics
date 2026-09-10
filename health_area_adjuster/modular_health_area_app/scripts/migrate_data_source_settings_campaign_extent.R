# =============================================================================
# migrate_data_source_settings_campaign_extent.R
#
# Migrates data_source_settings from a global-only (setting_key PRIMARY KEY)
# table to the (setting_key, campaign_id) pattern generation_settings already
# uses -- needed so campaign_extent_url can have a different value per
# campaign, while subdivisions_url/idp_settlements_url/settlement_extents_url
# stay global-only (their lookups simply never find a campaign-specific row,
# which is fine -- the lookup falls back to the global row).
#
# Also renames any existing 'urban_areas_url' row to 'campaign_extent_url'
# (the concept it now represents), and fixes a real, separate bug found
# along the way: the OLD setting_key CHECK constraint never actually
# included 'urban_areas_url' at all -- it silently should have rejected
# every save of that setting since the feature was built. This migration's
# new CHECK constraint is correct from the start.
#
# Safe to run against a live database and safe to re-run: every step uses
# IF NOT EXISTS / IF EXISTS or is naturally idempotent.
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

cat('=== Before ===\n')
before <- tryCatch(dbGetQuery(con, "SELECT setting_key, setting_value FROM data_source_settings"),
                   error = function(e) { cat('(table does not exist yet -- CREATE TABLE below will make it)\n'); NULL })
if (!is.null(before)) print(before)
cat('\n')

cat('=== Migrating schema ===\n')

# If the table doesn't exist at all yet, just create it fresh in the new
# shape -- covers a database that hasn't run create_db_v2.R's own
# (already-updated) CREATE TABLE for this yet either.
table_exists <- tryCatch({
  dbGetQuery(con, "SELECT 1 FROM information_schema.tables WHERE table_name = 'data_source_settings'")
  nrow(dbGetQuery(con, "SELECT 1 FROM information_schema.tables WHERE table_name = 'data_source_settings'")) > 0
}, error = function(e) FALSE)

if (!table_exists) {
  dbExecute(con, "
    CREATE TABLE data_source_settings (
      id             SERIAL PRIMARY KEY,
      setting_key    TEXT        NOT NULL
                      CHECK (setting_key IN ('subdivisions_url', 'idp_settlements_url',
                                             'campaign_extent_url', 'settlement_extents_url')),
      campaign_id    INTEGER REFERENCES campaigns(campaign_id),
      setting_value   TEXT        NOT NULL,
      updated_by       TEXT REFERENCES users(username),
      updated_at        TIMESTAMPTZ NOT NULL DEFAULT NOW()
    )
  ")
  dbExecute(con, "CREATE UNIQUE INDEX idx_dss_global_unique ON data_source_settings(setting_key) WHERE campaign_id IS NULL")
  dbExecute(con, "CREATE UNIQUE INDEX idx_dss_campaign_unique ON data_source_settings(setting_key, campaign_id) WHERE campaign_id IS NOT NULL")
  cat('Created data_source_settings fresh (did not exist before).\n')
} else {
  # Table exists in the OLD shape (setting_key TEXT PRIMARY KEY, no
  # campaign_id) -- migrate it in place.
  dbExecute(con, "ALTER TABLE data_source_settings ADD COLUMN IF NOT EXISTS campaign_id INTEGER REFERENCES campaigns(campaign_id)")
  dbExecute(con, "ALTER TABLE data_source_settings ADD COLUMN IF NOT EXISTS id SERIAL")

  # Drop the old setting_key-only primary key (its name is Postgres's
  # default "<table>_pkey" unless someone renamed it) and the old CHECK
  # constraint, if either still exist under their default names.
  dbExecute(con, "ALTER TABLE data_source_settings DROP CONSTRAINT IF EXISTS data_source_settings_pkey")
  old_check <- dbGetQuery(con, "
    SELECT conname FROM pg_constraint
    WHERE conrelid = 'data_source_settings'::regclass AND contype = 'c'
  ")
  for (cn in old_check$conname) {
    dbExecute(con, sprintf("ALTER TABLE data_source_settings DROP CONSTRAINT IF EXISTS %s", cn))
  }

  dbExecute(con, "ALTER TABLE data_source_settings ADD PRIMARY KEY (id)")
  dbExecute(con, "
    ALTER TABLE data_source_settings ADD CONSTRAINT data_source_settings_setting_key_check
      CHECK (setting_key IN ('subdivisions_url', 'idp_settlements_url',
                             'campaign_extent_url', 'settlement_extents_url'))
  ")
  dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS idx_dss_global_unique ON data_source_settings(setting_key) WHERE campaign_id IS NULL")
  dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS idx_dss_campaign_unique ON data_source_settings(setting_key, campaign_id) WHERE campaign_id IS NOT NULL")
  cat('Migrated existing data_source_settings to the (setting_key, campaign_id) shape.\n')
}

cat('\n=== Renaming urban_areas_url -> campaign_extent_url ===\n')
n <- tryCatch(
  dbExecute(con, "UPDATE data_source_settings SET setting_key = 'campaign_extent_url' WHERE setting_key = 'urban_areas_url'"),
  error = function(e) { cat('(no urban_areas_url row existed -- nothing to rename: ', e$message, ')\n', sep=''); 0 }
)
cat(sprintf('Renamed %d row(s).\n', n))

cat('\n=== After ===\n')
after <- dbGetQuery(con, "SELECT setting_key, campaign_id, setting_value FROM data_source_settings")
print(after)
