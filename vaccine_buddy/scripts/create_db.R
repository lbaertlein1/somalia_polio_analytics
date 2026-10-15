# =============================================================================
# create_db.R
#
# One-time setup of the vaccine_buddy database, on the same Postgres server
# as the health area adjuster (somalia_health_areas_v2). Same pattern as
# health_area_adjuster/modular_health_area_app/scripts/create_db_v2.R:
# connect as postgres, create the database, grant the existing shiny_app
# role, then create tables. Every statement is idempotent, so rerunning
# after adding a table/column is safe.
#
# This app only READS the adjuster database (campaigns, published health
# areas / team areas, IDP sites, facilities). Everything this app owns --
# supervisor contacts, saved analysis runs, missed places and their
# follow-up status, and a log of what was sent to whom -- lives here.
#
# Run from the vaccine_buddy/ folder:  source('scripts/create_db.R')
# Needs in .env: VB_DB_HOST, VB_DB_PORT, VB_DB_NAME, POSTGRES_PASSWORD
# Optional: ADMIN_PASSWORD -- seeds an 'admin' login for this app (its own
# accounts, separate from the adjuster's). Add more with scripts/add_user.R.
# =============================================================================

library(DBI)
library(RPostgres)
library(dotenv)

if (file.exists('.env')) dotenv::load_dot_env('.env')

DB_HOST           <- Sys.getenv('VB_DB_HOST', '23.239.19.115')
DB_PORT           <- as.integer(Sys.getenv('VB_DB_PORT', '5432'))
DB_NAME           <- Sys.getenv('VB_DB_NAME', 'vaccine_buddy')
POSTGRES_PASSWORD <- Sys.getenv('POSTGRES_PASSWORD', '')

if (!nzchar(POSTGRES_PASSWORD)) stop('POSTGRES_PASSWORD is not set. Add it to .env before running this script.')

# --- Step 1: database + role grant ------------------------------------------
con_admin <- dbConnect(RPostgres::Postgres(), host = DB_HOST, port = DB_PORT, dbname = 'postgres',
                       user = 'postgres', password = POSTGRES_PASSWORD, sslmode = 'require')

if (nrow(dbGetQuery(con_admin, 'SELECT 1 FROM pg_database WHERE datname = $1', list(DB_NAME))) == 0) {
  dbExecute(con_admin, sprintf('CREATE DATABASE %s', DB_NAME))
  cat('Database', DB_NAME, 'created.\n')
} else {
  cat('Database', DB_NAME, 'already exists -- skipping.\n')
}
if (nrow(dbGetQuery(con_admin, "SELECT 1 FROM pg_roles WHERE rolname = 'shiny_app'")) == 0) {
  dbDisconnect(con_admin)
  stop('Role shiny_app does not exist -- run the adjuster create_db_v2.R first (it creates the role).')
}
dbExecute(con_admin, sprintf('GRANT CONNECT ON DATABASE %s TO shiny_app', DB_NAME))
dbDisconnect(con_admin)

# --- Step 2: tables -----------------------------------------------------------
con <- dbConnect(RPostgres::Postgres(), host = DB_HOST, port = DB_PORT, dbname = DB_NAME,
                 user = 'postgres', password = POSTGRES_PASSWORD, sslmode = 'require')

dbExecute(con, 'GRANT USAGE ON SCHEMA public TO shiny_app')
dbExecute(con, 'ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO shiny_app')
dbExecute(con, 'ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT USAGE, SELECT ON SEQUENCES TO shiny_app')

statements <- list(
  # This app's own logins (same shape as the adjuster's users table, but
  # separate accounts). Passwords are bcrypt hashes.
  "CREATE TABLE IF NOT EXISTS users (
    username      TEXT PRIMARY KEY,
    password      TEXT        NOT NULL,
    display_name  TEXT        NOT NULL,
    role          TEXT        NOT NULL DEFAULT 'user' CHECK (role IN ('admin', 'user')),
    created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at    TIMESTAMPTZ NOT NULL DEFAULT NOW()
  )",
  # Where each user last was -- campaign, district, days chosen -- opened
  # again at sign-in
  "ALTER TABLE users ADD COLUMN IF NOT EXISTS last_district TEXT",
  "ALTER TABLE users ADD COLUMN IF NOT EXISTS last_campaign_id INTEGER",
  "ALTER TABLE users ADD COLUMN IF NOT EXISTS last_date_from DATE",
  "ALTER TABLE users ADD COLUMN IF NOT EXISTS last_date_to DATE",

  # App-wide settings changed on the Admin page; key 'analysis_params' holds
  # the analysis settings that differ from DEFAULT_PARAMS in global.R.
  "CREATE TABLE IF NOT EXISTS app_settings (
    key         TEXT PRIMARY KEY,
    value       JSONB       NOT NULL,
    updated_by  TEXT,
    updated_at  TIMESTAMPTZ NOT NULL DEFAULT NOW()
  )",

  # Each user's work in progress on the Find tab, per campaign and district,
  # saved automatically so they pick up where they left off: `base` is the
  # search (written once per search), `edits` what they've changed since
  # (places excluded, painted or adjusted; the tab they were on). Both are
  # gzip-compressed R objects (vb_save_work() in R/vb_db.R).
  "CREATE TABLE IF NOT EXISTS work_sessions (
    username       TEXT    NOT NULL,
    campaign_id    INTEGER NOT NULL,
    district_name  TEXT    NOT NULL,
    base           BYTEA,
    edits          BYTEA,
    updated_at     TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (username, campaign_id, district_name)
  )",

  # Each team's role in a campaign, as set by an admin: in a health area
  # (health_area_name), in none (e.g. Surge teams) or ignored (e.g. FETP
  # monitors). Teams not listed follow their name (team_roles() in analysis.R).
  "CREATE TABLE IF NOT EXISTS team_roles (
    campaign_id       INTEGER NOT NULL,
    team_name         TEXT    NOT NULL,
    role              TEXT    NOT NULL CHECK (role IN ('area', 'no_area', 'ignore')),
    health_area_name  TEXT,
    updated_by        TEXT,
    updated_at        TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (campaign_id, team_name)
  )",

  # Vaccine Buddy data from ODK Central, kept up to date by odk_sync()
  # (R/odk.R; hourly via scripts/sync_odk.R, or "Check for new data" in the
  # app). One row per carrier upload, and one per log row -- keyed by
  # device + entry id, so re-uploaded rows are stored once.
  "CREATE TABLE IF NOT EXISTS vb_submissions (
    submission_id    TEXT PRIMARY KEY,
    device_id        TEXT,
    team_name        TEXT,
    campaign_name    TEXT,
    submission_date  TIMESTAMPTZ,
    stored_at        TIMESTAMPTZ NOT NULL DEFAULT NOW()
  )",
  "CREATE INDEX IF NOT EXISTS idx_vbs_date ON vb_submissions(submission_date)",
  "CREATE TABLE IF NOT EXISTS vb_entries (
    device_id        TEXT NOT NULL,
    entry_id         TEXT NOT NULL,
    submission_id    TEXT NOT NULL REFERENCES vb_submissions(submission_id),
    ts               TIMESTAMPTZ,
    lon              DOUBLE PRECISION,
    lat              DOUBLE PRECISION,
    accuracy_m       DOUBLE PRECISION,
    lid_open         BOOLEAN,
    temp_c           DOUBLE PRECISION,
    speed_kph        DOUBLE PRECISION,
    battery_percent  DOUBLE PRECISION,
    PRIMARY KEY (device_id, entry_id)
  )",
  "CREATE INDEX IF NOT EXISTS idx_vbe_ts ON vb_entries(ts)",
  # "Check for new data" in the app only adds a row here; the VM's
  # every-minute job (scripts/sync_odk.R --if-requested) claims it, runs the
  # update and marks it done. The app never calls ODK itself.
  "CREATE TABLE IF NOT EXISTS vb_sync_requests (
    id            SERIAL PRIMARY KEY,
    requested_by  TEXT,
    requested_at  TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    picked_at     TIMESTAMPTZ,      -- the VM started it
    done_at       TIMESTAMPTZ,
    status        TEXT,             -- ok | error
    message       TEXT
  )",
  "CREATE TABLE IF NOT EXISTS vb_sync_log (
    id             SERIAL PRIMARY KEY,
    started_at     TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    finished_at    TIMESTAMPTZ,
    since          TIMESTAMPTZ,      -- NULL = full pull
    triggered_by   TEXT,             -- 'cron' or the app user
    status         TEXT,             -- ok | error
    n_uploads      INTEGER,
    n_new_entries  INTEGER,
    message        TEXT
  )",

  # Local place names (mosques, schools, shops, area names) recorded by
  # campaign monitors in ODK project 11, for labelling missed places --
  # R/place_names.R. One row per (form, submission + field); each form is
  # re-pulled only when ODK reports new submissions for it.
  "CREATE TABLE IF NOT EXISTS vb_place_names (
    source       TEXT NOT NULL,      -- 'odk:<project>:<form id>'
    source_ref   TEXT NOT NULL,
    name         TEXT NOT NULL,
    lon          DOUBLE PRECISION NOT NULL,
    lat          DOUBLE PRECISION NOT NULL,
    observed_at  TIMESTAMPTZ,
    PRIMARY KEY (source, source_ref)
  )",
  "CREATE INDEX IF NOT EXISTS idx_vpn_lonlat ON vb_place_names(lon, lat)",
  "CREATE TABLE IF NOT EXISTS vb_place_sources (
    fid              TEXT PRIMARY KEY,
    last_submission  TEXT,
    pulled_at        TIMESTAMPTZ
  )",

  # campaign_id / district_name / health_area_name refer to the adjuster
  # database (a different database, so no FK). Contacts are per campaign
  # because health-area names can change between campaigns; the app has a
  # "copy from previous campaign" action. Rows are deactivated, never
  # deleted, so the send log below keeps pointing at real people.
  "CREATE TABLE IF NOT EXISTS supervisors (
    supervisor_id     SERIAL PRIMARY KEY,
    campaign_id       INTEGER     NOT NULL,
    district_name     TEXT        NOT NULL,
    health_area_name  TEXT        NOT NULL,
    supervisor_name   TEXT        NOT NULL,
    phone             TEXT        NOT NULL,
    is_active         BOOLEAN     NOT NULL DEFAULT TRUE,
    notes             TEXT,
    updated_by        TEXT,
    updated_at        TIMESTAMPTZ NOT NULL DEFAULT NOW()
  )",
  "CREATE INDEX IF NOT EXISTS idx_sup_campaign ON supervisors(campaign_id)",
  # A phone is unique per health area among *active* supervisors only:
  # removed ones are kept (inactive) for the send log, and must not stop
  # the same number being entered again. Replaces the table's original
  # UNIQUE constraint, which counted inactive rows too.
  "DO $$ DECLARE c text; BEGIN
     FOR c IN SELECT conname FROM pg_constraint WHERE conrelid = 'supervisors'::regclass AND contype = 'u' LOOP
       EXECUTE format('ALTER TABLE supervisors DROP CONSTRAINT %I', c);
     END LOOP;
   END $$",
  "CREATE UNIQUE INDEX IF NOT EXISTS idx_sup_active_phone ON supervisors(campaign_id, district_name, health_area_name, phone)
     WHERE is_active",

  # One saved analysis: which Vaccine Buddy data window, with which
  # cleaning/classification parameters.
  "CREATE TABLE IF NOT EXISTS analysis_runs (
    run_id           SERIAL PRIMARY KEY,
    campaign_id      INTEGER     NOT NULL,
    campaign_name    TEXT,
    data_from        DATE        NOT NULL,
    data_to          DATE        NOT NULL,
    params           JSONB       NOT NULL,
    cleaning_report  JSONB,
    n_pings_used     INTEGER,
    n_devices        INTEGER,
    notes            TEXT,
    created_by       TEXT        NOT NULL,
    created_at       TIMESTAMPTZ NOT NULL DEFAULT NOW()
  )",
  "CREATE INDEX IF NOT EXISTS idx_runs_campaign ON analysis_runs(campaign_id, created_at DESC)",
  # Each saved list is for one district of a campaign
  "ALTER TABLE analysis_runs ADD COLUMN IF NOT EXISTS district_name TEXT",

  # Coverage per team area, plus one health-area total row per health area
  # (team_area_name = '').
  "CREATE TABLE IF NOT EXISTS area_coverage (
    run_id            INTEGER NOT NULL REFERENCES analysis_runs(run_id) ON DELETE CASCADE,
    district_name     TEXT    NOT NULL,
    health_area_name  TEXT    NOT NULL,
    team_area_name    TEXT    NOT NULL DEFAULT '',
    populated_cells   INTEGER,
    visited_cells     INTEGER,
    u5_pop            NUMERIC,
    u5_pop_visited    NUMERIC,
    n_places          INTEGER,
    n_fixes           INTEGER,   -- usable carrier GPS fixes in the health area
    n_devices         INTEGER,
    teams             TEXT,      -- team names that logged there
    has_data          BOOLEAN,   -- FALSE: no carriers there, not analysed
    PRIMARY KEY (run_id, district_name, health_area_name, team_area_name)
  )",

  # Missed places: contiguous groups of populated-but-unvisited grid cells.
  # lon/lat is the navigation target (the most populated cell in the
  # place); geom is the place outline as GeoJSON. status is updated from
  # the supervisor's page.
  "CREATE TABLE IF NOT EXISTS missed_places (
    place_id          SERIAL PRIMARY KEY,
    run_id            INTEGER NOT NULL REFERENCES analysis_runs(run_id) ON DELETE CASCADE,
    district_name     TEXT    NOT NULL,
    health_area_name  TEXT    NOT NULL,
    team_area_name    TEXT,      -- adjuster team area, if mapped
    assigned_team     TEXT,      -- team the place is for: team area, else nearest carrier's team
    team_source       TEXT,      -- 'team area' | 'nearest carrier'
    team_dist_m       INTEGER,   -- nearest carrier: how close its track came
    place_no          INTEGER NOT NULL,
    lon               DOUBLE PRECISION NOT NULL,
    lat               DOUBLE PRECISION NOT NULL,
    n_cells           INTEGER,
    u5_pop            NUMERIC,
    label             TEXT,
    near_idp          TEXT,
    geom              JSONB,
    status            TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open', 'done', 'unreachable', 'no_children')),
    status_at         TIMESTAMPTZ,
    status_note       TEXT
  )",
  "CREATE INDEX IF NOT EXISTS idx_places_area ON missed_places(run_id, district_name, health_area_name)",
  # 'no_children' (no children live there) added after the table was made
  "ALTER TABLE missed_places DROP CONSTRAINT IF EXISTS missed_places_status_check",
  "ALTER TABLE missed_places ADD CONSTRAINT missed_places_status_check
     CHECK (status IN ('open', 'done', 'unreachable', 'no_children'))",

  # One link per (run, supervisor). The token in the link is the only
  # credential the supervisor page needs. name/phone/message are copied
  # in as sent, so the record survives later contact edits.
  "CREATE TABLE IF NOT EXISTS dispatches (
    dispatch_id       SERIAL PRIMARY KEY,
    run_id            INTEGER NOT NULL REFERENCES analysis_runs(run_id) ON DELETE CASCADE,
    supervisor_id     INTEGER NOT NULL REFERENCES supervisors(supervisor_id),
    token             TEXT    NOT NULL UNIQUE,
    supervisor_name   TEXT,
    phone             TEXT,
    message           TEXT    NOT NULL,
    created_by        TEXT,
    created_at        TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    send_clicked_at   TIMESTAMPTZ,
    first_opened_at   TIMESTAMPTZ,
    last_opened_at    TIMESTAMPTZ,
    n_opens           INTEGER NOT NULL DEFAULT 0,
    UNIQUE (run_id, supervisor_id)
  )",

  # The Google My Maps map made (by hand, from the app's KML file) for each
  # health area of a saved list; its share link is what's sent when
  # sending "with a Google My Maps link".
  "CREATE TABLE IF NOT EXISTS mymaps_links (
    run_id            INTEGER NOT NULL REFERENCES analysis_runs(run_id) ON DELETE CASCADE,
    health_area_name  TEXT    NOT NULL,
    url               TEXT    NOT NULL,
    updated_by        TEXT,
    updated_at        TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (run_id, health_area_name)
  )",

  # Each health area's map to download (PDF), made once per saved list when
  # the operator includes it in the messages; supervisors download it from
  # their link (?t=<token>&pdf=1).
  "CREATE TABLE IF NOT EXISTS area_pdfs (
    run_id            INTEGER NOT NULL REFERENCES analysis_runs(run_id) ON DELETE CASCADE,
    health_area_name  TEXT    NOT NULL,
    page_size         TEXT,
    basemap           TEXT,
    pdf               BYTEA   NOT NULL,
    created_by        TEXT,
    created_at        TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (run_id, health_area_name)
  )",

  "CREATE TABLE IF NOT EXISTS place_status_log (
    id           SERIAL PRIMARY KEY,
    place_id     INTEGER NOT NULL REFERENCES missed_places(place_id) ON DELETE CASCADE,
    status       TEXT    NOT NULL,
    note         TEXT,
    changed_via  TEXT,
    changed_at   TIMESTAMPTZ NOT NULL DEFAULT NOW()
  )"
)

for (stmt in statements) {
  label <- substr(trimws(gsub('\\s+', ' ', stmt)), 1, 70)
  tryCatch({ dbExecute(con, stmt); cat(' OK :', label, '\n') },
           error = function(e) cat('ERR :', label, '\n    ->', conditionMessage(e), '\n'))
}

dbExecute(con, 'GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO shiny_app')
dbExecute(con, 'GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO shiny_app')

ADMIN_PASSWORD <- Sys.getenv('ADMIN_PASSWORD', '')
if (nzchar(ADMIN_PASSWORD)) {
  dbExecute(con, "INSERT INTO users (username, password, display_name, role) VALUES ('admin', $1, 'Admin', 'admin')
                  ON CONFLICT (username) DO NOTHING", list(bcrypt::hashpw(ADMIN_PASSWORD)))
  cat("Login 'admin' ready (if it already existed, its password was left unchanged).
")
} else {
  cat("ADMIN_PASSWORD not set -- no login created. Add one with scripts/add_user.R.
")
}

print(dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' ORDER BY 1"))
dbDisconnect(con)
cat('\nDone. Database ready.\n')

