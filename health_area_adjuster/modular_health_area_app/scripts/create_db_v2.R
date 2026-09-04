library(DBI)
library(RPostgres)
library(bcrypt)
library(dotenv)

# =============================================================================
# create_db_v2.R
#
# Fresh schema for the versioning/ownership rewrite. This is NOT a migration
# of create_db.R — it stands up a new database from scratch. Legacy data
# (final/actual health areas from the old district_submissions table) gets
# carried over later via a separate, one-time migration script once this
# schema is live and confirmed working.
#
# Key differences from the old schema:
#   - No more user_districts / district-based access scoping. Any
#     authenticated user can create a version for any district.
#   - No more is_practice. Practice/Actual mode is gone entirely.
#   - Primary unit of work is a "version" owned by a user, scoped to a
#     (district, campaign) pair. Versions are never overwritten by anyone
#     but their owner, and only while unpublished.
#   - Publishing sets is_shared = TRUE and unshares whatever was previously
#     shared for that same (district_name, campaign_id) — enforced by
#     db_publish_version() in mod_db_v2.R, not by a DB constraint, since a
#     partial-index UNIQUE would fight the "publish is a transaction" model.
#   - branched_from_id is used identically for "branch from a shared version"
#     and "carry forward from a prior campaign" — same mechanism.
#   - Facilities/boundary snapshots are locked at version creation (or first
#     facilities-stage submit) and never silently refreshed.
#   - microplanning and its planning_data column are gone.
#
#   - TEAM AREAS ARE NOW A SEPARATE, INDEPENDENTLY-VERSIONED TABLE
#     (team_area_versions), NOT columns on mapping_versions. This is a
#     deliberate rework, not an oversight — health areas are meaningless
#     without a whole-district submission, but team areas need to be
#     publishable one health area at a time, independent of every other
#     health area in the district and independent of the health-area
#     track's own version history. Each team_area_versions row is
#     PERMANENTLY pinned to the SPECIFIC mapping_versions row (health-area
#     version) it was drawn against via based_on_health_area_version_id —
#     there is no reconciliation onto a different boundary. A team-area
#     version can only ever be published as current while its pinned
#     health-area version IS the district's current one; if it isn't, the
#     health-area version has to be made current again (a normal publish,
#     nothing automatic) before that team-area work can resume.
#   - Once ANY health area in a district has a current (is_shared=TRUE)
#     team-area version, the WHOLE district's health-area track locks
#     UNCONDITIONALLY against publishing a different version — no admin
#     override. Regular users can still paint/submit/save health-area
#     drafts freely; only db_publish_version() is gated. To publish a
#     different health-area version, the current team-area version(s)
#     must first be unshared (an existing admin action in District
#     review) — a deliberate, explicit step, not an automatic swap. See
#     db_district_has_locked_team_areas() in mod_db_v2.R.
#   - campaign_districts is new: campaigns no longer implicitly apply to
#     every district. A district must be explicitly assigned to a
#     campaign (an ongoing admin action, not fixed at campaign creation)
#     before it appears anywhere in the app for that campaign.
# =============================================================================


# =============================================================================
# CONFIGURATION
#
# Reads from .env_v2 (same directory as this script). Copy the template,
# fill in the values, do not commit it.
#
#   DB_HOST            — Postgres host (same server as the current app)
#   DB_PORT             — defaults to 5432 if unset
#   DB_NAME              — name for the new database (must not already exist)
#   POSTGRES_PASSWORD     — superuser password, required
#   SHINY_APP_PASSWORD     — only read if role shiny_app doesn't exist yet
#   ADMIN_PASSWORD           — optional; skip to add an admin login later
# =============================================================================

dotenv::load_dot_env('.env_v2')

DB_HOST            <- Sys.getenv('DB_HOST', '23.239.19.115')
DB_PORT             <- as.integer(Sys.getenv('DB_PORT', '5432'))
DB_NAME              <- Sys.getenv('DB_NAME', 'somalia_health_areas_v2')
POSTGRES_PASSWORD     <- Sys.getenv('POSTGRES_PASSWORD', '')
SHINY_APP_PASSWORD     <- Sys.getenv('SHINY_APP_PASSWORD', '')
ADMIN_PASSWORD           <- Sys.getenv('ADMIN_PASSWORD', '')

if (!nzchar(POSTGRES_PASSWORD)) {
  stop('POSTGRES_PASSWORD is not set. Add it to .env_v2 before running this script.')
}


# =============================================================================
# Step 1: Connect as postgres superuser, create the database + app role
# =============================================================================

con_admin <- dbConnect(
  RPostgres::Postgres(),
  host     = DB_HOST,
  port     = DB_PORT,
  dbname   = 'postgres',
  user     = 'postgres',
  password = POSTGRES_PASSWORD,
  sslmode  = 'require'
)

db_exists <- dbGetQuery(con_admin,
  "SELECT 1 FROM pg_database WHERE datname = $1", list(DB_NAME))
if (nrow(db_exists) == 0) {
  dbExecute(con_admin, sprintf("CREATE DATABASE %s", DB_NAME))
  cat("Database", DB_NAME, "created.\n")
} else {
  cat("Database", DB_NAME, "already exists — skipping.\n")
}

role_exists <- dbGetQuery(con_admin,
  "SELECT 1 FROM pg_roles WHERE rolname = 'shiny_app'")
if (nrow(role_exists) == 0) {
  if (!nzchar(SHINY_APP_PASSWORD)) {
    dbDisconnect(con_admin)
    stop('Role shiny_app does not exist. Set SHINY_APP_PASSWORD and rerun to create it.')
  }
  dbExecute(con_admin, sprintf("CREATE USER shiny_app WITH PASSWORD '%s'", SHINY_APP_PASSWORD))
  cat("Role shiny_app created.\n")
} else {
  cat("Role shiny_app already exists — skipping.\n")
}

dbExecute(con_admin, sprintf("GRANT CONNECT ON DATABASE %s TO shiny_app", DB_NAME))
dbDisconnect(con_admin)
cat("Step 1 complete.\n\n")


# =============================================================================
# Step 2: Connect to new database and set up schema
# =============================================================================

con <- dbConnect(
  RPostgres::Postgres(),
  host     = DB_HOST,
  port     = DB_PORT,
  dbname   = DB_NAME,
  user     = 'postgres',
  password = POSTGRES_PASSWORD,
  sslmode  = 'require'
)

dbExecute(con, "GRANT USAGE ON SCHEMA public TO shiny_app")
dbExecute(con, "GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO shiny_app")
dbExecute(con, "GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO shiny_app")
dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO shiny_app")
dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT USAGE, SELECT ON SEQUENCES TO shiny_app")
cat("Schema privileges granted.\n")


# =============================================================================
# Step 3: Create tables
# =============================================================================

statements <- list(

  # ── Users (simplified — no more district scoping) ──────────────────────────
  "CREATE TABLE IF NOT EXISTS users (
    username      TEXT PRIMARY KEY,
    password      TEXT        NOT NULL,
    display_name  TEXT        NOT NULL,
    role          TEXT        NOT NULL DEFAULT 'user'
                              CHECK (role IN ('admin', 'user')),
    created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at    TIMESTAMPTZ NOT NULL DEFAULT NOW()
  )",

  # ── Campaigns — admin-created and shared across all users ──────────────────
  "CREATE TABLE IF NOT EXISTS campaigns (
    campaign_id   SERIAL PRIMARY KEY,
    campaign_name TEXT        NOT NULL,
    description   TEXT,
    created_by    TEXT        NOT NULL REFERENCES users(username),
    created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    is_active     BOOLEAN     NOT NULL DEFAULT TRUE
  )",

  # ── Mapping versions — the core rework ──────────────────────────────────────
  "CREATE TABLE IF NOT EXISTS mapping_versions (
    version_id          SERIAL PRIMARY KEY,

    -- identity / ownership
    owner_username       TEXT    NOT NULL REFERENCES users(username),
    campaign_id           INTEGER NOT NULL REFERENCES campaigns(campaign_id),
    district_name         TEXT    NOT NULL,

    -- lineage
    version_number        INTEGER NOT NULL,   -- human-readable, per (owner, campaign, district)
    branched_from_id       INTEGER REFERENCES mapping_versions(version_id),
      -- set when created via 'branch from a shared version' or
      -- 'carry forward from a prior campaign' (same mechanism — the row
      -- pointed to may belong to a different campaign_id)

    -- publish state
    is_shared             BOOLEAN     NOT NULL DEFAULT FALSE,
    shared_at              TIMESTAMPTZ,
      -- editing a shared version always forks a new row rather than
      -- mutating this one in place — enforced in mod_db_v2.R, not here

    -- lifecycle
    created_at             TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_updated_at         TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    archived_at             TIMESTAMPTZ,   -- soft-delete only; owners' own
                                          -- versions are never hard-deleted

    -- snapshot fields, locked at version creation (or first facilities submit)
    facilities_locked_at   TIMESTAMPTZ,
    odk_sf                 JSONB,       -- locked MHFL registry snapshot
    app_sf                  JSONB,       -- locked user-added facility sites
    boundary_locked_at      TIMESTAMPTZ,
    district_boundary_sf    JSONB,       -- locked district/subdivision geometry

    -- working (gridded) geometry — health areas ONLY. Team areas live in
    -- team_area_versions now (see below), not here.
    saved_dfa_sf             JSONB,       -- health area polygons (raw grid)
    dfa_names                 JSONB,
    current_assignments       JSONB,
    team_targets                JSONB,       -- per-health-area field target
      -- population / requested team count from the post-submit modal
      -- (mod_health_area_tab.R's .show_team_targets_modal()). Purely
      -- informational + the requested-teams override Team Areas reads —
      -- never used to compute anything about health-area boundaries
      -- themselves.

    -- presentation (smoothed/snapped) geometry, generated at submit
    smoothed_dfa_sf                JSONB,
    smoothing_generated_at          TIMESTAMPTZ,

    -- point layers
    landmarks                        JSONB,
    idp_settlements                   JSONB,

    -- progress flags (mirrors old has_* columns) — has_team_areas is gone;
    -- team-area progress is now read from team_area_versions directly
    -- (e.g. count of distinct health_area_name with is_shared=TRUE for
    -- this district+campaign) rather than a flag mirrored onto this row.
    has_landmarks       BOOLEAN NOT NULL DEFAULT FALSE,
    has_facilities       BOOLEAN NOT NULL DEFAULT FALSE,
    has_idp                BOOLEAN NOT NULL DEFAULT FALSE,
    has_health_areas        BOOLEAN NOT NULL DEFAULT FALSE,

    submitted_by                TEXT,   -- display convenience, mirrors owner_username

    UNIQUE (owner_username, campaign_id, district_name, version_number)
  )",

  "CREATE INDEX IF NOT EXISTS idx_mv_owner    ON mapping_versions(owner_username)",
  "CREATE INDEX IF NOT EXISTS idx_mv_district ON mapping_versions(district_name)",
  "CREATE INDEX IF NOT EXISTS idx_mv_shared
     ON mapping_versions(district_name, campaign_id)
     WHERE is_shared = TRUE AND archived_at IS NULL",

  # ── Team area versions — independently versioned/publishable per health
  # area, never bundled with the health-area track. PERMANENTLY pinned to
  # the specific health-area version it was drawn against
  # (based_on_health_area_version_id) -- this is the fact
  # db_check_team_area_staleness() checks: "is what I'm pinned to still
  # the district's current health-area version?" There is no
  # reconciliation onto a different boundary -- a version whose pinned
  # health-area version isn't current simply can't be published as
  # current until that health-area version is made current again.
  "CREATE TABLE IF NOT EXISTS team_area_versions (
    team_version_id      SERIAL PRIMARY KEY,

    -- identity / ownership / scope
    owner_username         TEXT    NOT NULL REFERENCES users(username),
    campaign_id              INTEGER NOT NULL REFERENCES campaigns(campaign_id),
    district_name             TEXT    NOT NULL,
    health_area_name           TEXT    NOT NULL,
      -- matches a name in the pinned health-area version's dfa_names

    -- the health-area boundary this team layout was drawn against. Team
    -- areas can ONLY ever be drawn against a district's CURRENT
    -- health-area version at the time work begins -- there is no
    -- branch/carry-forward choice at this level, unlike mapping_versions.
    based_on_health_area_version_id INTEGER NOT NULL
      REFERENCES mapping_versions(version_id),

    -- lineage
    version_number             INTEGER NOT NULL,  -- per (owner, campaign, district, health_area_name)
    branched_from_id             INTEGER REFERENCES team_area_versions(team_version_id),

    -- publish state — at most one is_shared=TRUE row per (district_name,
    -- campaign_id, health_area_name), enforced by db_publish_team_area()
    -- in mod_db_v2.R (same transaction pattern as db_publish_version()),
    -- not by a DB constraint.
    is_shared                       BOOLEAN     NOT NULL DEFAULT FALSE,
    shared_at                         TIMESTAMPTZ,

    -- lifecycle
    created_at                          TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_updated_at                       TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    archived_at                             TIMESTAMPTZ,

    -- working (gridded) geometry
    saved_team_sf                             JSONB,
    team_names                                  JSONB,
    current_team_assignments                      JSONB,

    -- presentation (smoothed/snapped) geometry
    smoothed_team_sf                                JSONB,
    smoothing_generated_at                            TIMESTAMPTZ,

    submitted_by                                        TEXT,

    UNIQUE (owner_username, campaign_id, district_name, health_area_name, version_number)
  )",

  "CREATE INDEX IF NOT EXISTS idx_tav_owner ON team_area_versions(owner_username)",
  "CREATE INDEX IF NOT EXISTS idx_tav_district_ha
     ON team_area_versions(district_name, campaign_id, health_area_name)",
  "CREATE INDEX IF NOT EXISTS idx_tav_shared
     ON team_area_versions(district_name, campaign_id, health_area_name)
     WHERE is_shared = TRUE AND archived_at IS NULL",
  # Drives db_check_team_area_staleness() / the auto-unpublish-on-override
  # sweep: "which team-area versions are pinned to health-area version X".
  "CREATE INDEX IF NOT EXISTS idx_tav_based_on
     ON team_area_versions(based_on_health_area_version_id)",

  # ── Campaign-district assignment — campaigns no longer implicitly apply
  # to every district. An ongoing, revisitable admin action (not fixed at
  # campaign creation) — see mod_admin_tab_v2.R's "Manage districts" per
  # campaign. Newly-assigned districts are offered bundled carry-forward
  # (health + team areas together, from the most recent prior campaign
  # that district was published in) at assignment time.
  "CREATE TABLE IF NOT EXISTS campaign_districts (
    campaign_id    INTEGER NOT NULL REFERENCES campaigns(campaign_id),
    district_name  TEXT    NOT NULL,
    added_at       TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    added_by       TEXT REFERENCES users(username),
    PRIMARY KEY (campaign_id, district_name)
  )",

  "CREATE INDEX IF NOT EXISTS idx_cd_district ON campaign_districts(district_name)",

  # ── Admin-configurable generation settings ──────────────────────────────────
  # campaign_id is nullable (NULL = global default), so it can't be part of a
  # PRIMARY KEY (PK columns are implicitly NOT NULL). Uniqueness is enforced
  # with two partial indexes instead — one for global rows, one per-campaign.
  "CREATE TABLE IF NOT EXISTS generation_settings (
    id             SERIAL PRIMARY KEY,
    setting_key    TEXT        NOT NULL,
    campaign_id    INTEGER REFERENCES campaigns(campaign_id),
      -- NULL = global default; non-null = per-campaign override
    setting_value  NUMERIC     NOT NULL,
    description    TEXT,
    updated_by     TEXT REFERENCES users(username),
    updated_at     TIMESTAMPTZ NOT NULL DEFAULT NOW()
  )",

  "CREATE UNIQUE INDEX IF NOT EXISTS idx_gs_global_unique
     ON generation_settings(setting_key) WHERE campaign_id IS NULL",
  "CREATE UNIQUE INDEX IF NOT EXISTS idx_gs_campaign_unique
     ON generation_settings(setting_key, campaign_id) WHERE campaign_id IS NOT NULL",

  # ── Admin-configurable external data source URLs ────────────────────────────
  # Health-facility (ODK/Kobo) endpoints stay hardcoded in facility_helpers.R /
  # .env — deliberately NOT included here.
  "CREATE TABLE IF NOT EXISTS data_source_settings (
    setting_key    TEXT PRIMARY KEY
                    CHECK (setting_key IN ('subdivisions_url', 'idp_settlements_url')),
    setting_value   TEXT        NOT NULL,
    updated_by       TEXT REFERENCES users(username),
    updated_at        TIMESTAMPTZ NOT NULL DEFAULT NOW()
  )"
)

cat("Creating tables...\n")
for (stmt in statements) {
  label <- substr(trimws(gsub('\n', ' ', stmt)), 1, 70)
  tryCatch({
    dbExecute(con, stmt)
    cat(" OK :", label, "\n")
  }, error = function(e) {
    cat("ERR :", label, "\n    ->", e$message, "\n")
  })
}

# Grant permissions to shiny_app on newly created tables
dbExecute(con, "GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO shiny_app")
dbExecute(con, "GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO shiny_app")


# =============================================================================
# Step 4: Default generation settings, and an admin user if ADMIN_PASSWORD
# was set
# =============================================================================

if (nzchar(ADMIN_PASSWORD)) {
  tryCatch({
    hashed_admin <- bcrypt::hashpw(ADMIN_PASSWORD)
    DBI::dbExecute(con,
                   "INSERT INTO users (username, password, display_name, role)
       VALUES ($1, $2, 'Admin', 'admin')
       ON CONFLICT (username) DO NOTHING",
                   list('admin', hashed_admin)
    )
    cat("\nAdmin user seeded.\n")
  }, error = function(e) cat("Admin user error:", e$message, "\n"))
} else {
  cat("\nADMIN_PASSWORD not set — no admin user created. Add one later with:\n")
  cat("  DBI::dbExecute(con, \"INSERT INTO users (username, password, display_name, role)\n")
  cat("    VALUES ('admin', $1, 'Admin', 'admin')\", list(bcrypt::hashpw('<password>')))\n")
}

# Global default generation settings — values mirror the constants that were
# previously hardcoded in global.R / mod_initial_health_area_generation.R
# defaults. campaign_id left NULL = applies to every campaign unless a
# campaign-specific row overrides it.
default_settings <- list(
  list(key = 'target_pop_per_health_area', val = 2000,  desc = 'Target under-5 population per health area'),
  list(key = 'target_pop_per_team',        val = 400,   desc = 'Target under-5 population per team area'),
  list(key = 'n_start_dfas',               val = 5,     desc = 'Default number of starter health areas shown before facility-based seeding'),
  list(key = 'pop_sat_pct',                val = 1.0,   desc = 'Population saturation threshold (fraction of target before penalty kicks in)'),
  list(key = 'pop_sat_weight',             val = 0.5,   desc = 'Weight applied to population saturation penalty'),
  list(key = 'pop_sat_max',                val = 0.3,   desc = 'Cap on population saturation penalty'),
  list(key = 'subdivision_boundary_penalty', val = 0.99, desc = 'Soft friction penalty for crossing a subdivision boundary during generation')
)

for (s in default_settings) {
  tryCatch(
    DBI::dbExecute(con, "
      INSERT INTO generation_settings (setting_key, campaign_id, setting_value, description)
      VALUES ($1, NULL, $2, $3)
      ON CONFLICT (setting_key) WHERE campaign_id IS NULL DO NOTHING
    ", list(s$key, s$val, s$desc)),
    error = function(e) cat("ERR seeding setting", s$key, ":", e$message, "\n")
  )
}
cat("Default generation settings seeded.\n")


# =============================================================================
# Step 5: Verify
# =============================================================================

cat("\nTables:\n")
print(dbGetQuery(con, "
  SELECT table_name FROM information_schema.tables
  WHERE table_schema = 'public' ORDER BY table_name
"))

cat("\nUsers:\n")
users_check <- dbGetQuery(con, "SELECT username, display_name, role FROM users")
if (nrow(users_check) == 0) cat("(none)\n") else print(users_check)

cat("\nGeneration settings:\n")
print(dbGetQuery(con, "SELECT setting_key, campaign_id, setting_value FROM generation_settings ORDER BY setting_key"))

dbDisconnect(con)
cat("\nDone. Database ready.\n")
