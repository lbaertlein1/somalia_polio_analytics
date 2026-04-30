library(DBI)
library(RPostgres)

# =============================================================================
# Step 1: Connect as postgres superuser and create the database
# =============================================================================

con_admin <- dbConnect(
  RPostgres::Postgres(),
  host     = '23.239.19.115',
  port     = 5432,
  dbname   = 'postgres',
  user     = 'postgres',
  password = '',
  sslmode  = 'require'
)

# Create the database
dbExecute(con_admin, "CREATE DATABASE somalia_health_areas")
cat("Database created.\n")

# Create app user if it doesn't already exist
# (skip if shiny_app already exists from your other app)
tryCatch({
  dbExecute(con_admin, "CREATE USER shiny_app WITH PASSWORD '%123Somalia2026'")
  cat("User shiny_app created.\n")
}, error = function(e) {
  cat("shiny_app user already exists — skipping creation.\n")
})

# Grant connect privilege
dbExecute(con_admin, "GRANT CONNECT ON DATABASE somalia_health_areas TO shiny_app")
cat("Connect privilege granted.\n")

dbDisconnect(con_admin)
cat("Step 1 complete.\n\n")


# =============================================================================
# Step 2: Connect to new database and create schema
# =============================================================================

con <- dbConnect(
  RPostgres::Postgres(),
  host     = '23.239.19.115',
  port     = 5432,
  dbname   = 'somalia_health_areas',
  user     = 'postgres',
  password = '',
  sslmode  = 'require'
)

# Grant schema privileges to shiny_app
dbExecute(con, "GRANT USAGE ON SCHEMA public TO shiny_app")
dbExecute(con, "GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO shiny_app")
dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO shiny_app")
cat("Schema privileges granted.\n")


# =============================================================================
# Step 3: Create tables
# =============================================================================

statements <- list(
  
  # ── Users ──────────────────────────────────────────────────────────────────
  "CREATE TABLE users (
    username      TEXT PRIMARY KEY,
    password      TEXT        NOT NULL,
    display_name  TEXT        NOT NULL,
    role          TEXT        NOT NULL DEFAULT 'user'
                              CHECK (role IN ('admin', 'user')),
    created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at    TIMESTAMPTZ NOT NULL DEFAULT NOW()
  )",
  
  "CREATE TABLE user_districts (
    id            SERIAL PRIMARY KEY,
    username      TEXT NOT NULL REFERENCES users(username) ON DELETE CASCADE,
    district_name TEXT NOT NULL,
    UNIQUE (username, district_name)
  )",
  
  "CREATE INDEX idx_user_districts_username ON user_districts(username)",
  
  # Seed admin user — change this password before going live
  "INSERT INTO users (username, password, display_name, role)
   VALUES ('admin', 'admin123', 'Admin', 'admin')",
  
  # ── Sessions ───────────────────────────────────────────────────────────────
  "CREATE TABLE sessions (
    session_id    TEXT PRIMARY KEY,
    username      TEXT NOT NULL REFERENCES users(username) ON DELETE CASCADE,
    district_name TEXT NOT NULL,
    started_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    saved_at      TIMESTAMPTZ NOT NULL DEFAULT NOW()
  )",
  
  "CREATE INDEX idx_sessions_username ON sessions(username)",
  "CREATE INDEX idx_sessions_district ON sessions(district_name)",
  
  # ── Session snapshots ──────────────────────────────────────────────────────
  "CREATE TABLE session_snapshots (
    snapshot_id          SERIAL PRIMARY KEY,
    session_id           TEXT NOT NULL REFERENCES sessions(session_id) ON DELETE CASCADE,
    snapshot_at          TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    trigger              TEXT NOT NULL DEFAULT 'manual',
    odk_sf               JSONB,
    app_sf               JSONB,
    landmarks            JSONB,
    current_assignments  JSONB,
    saved_dfa_sf         JSONB,
    dfa_names            JSONB,
    planning_data        JSONB
  )",
  
  "CREATE INDEX idx_snapshots_session ON session_snapshots(session_id)",
  "CREATE INDEX idx_snapshots_time    ON session_snapshots(snapshot_at DESC)",
  
  # Latest snapshot per session — used by admin progress table
  "CREATE VIEW latest_snapshots AS
    SELECT DISTINCT ON (session_id)
      snapshot_id, session_id, snapshot_at, trigger,
      odk_sf, app_sf, landmarks,
      current_assignments, saved_dfa_sf, dfa_names,
      planning_data
    FROM session_snapshots
    ORDER BY session_id, snapshot_at DESC",
  
  # ── Health area plans ──────────────────────────────────────────────────────
  "CREATE TABLE health_area_plans (
    id              SERIAL PRIMARY KEY,
    session_id      TEXT NOT NULL REFERENCES sessions(session_id) ON DELETE CASCADE,
    district_name   TEXT NOT NULL,
    area_name       TEXT NOT NULL,
    uid             TEXT NOT NULL,
    u5_pop          NUMERIC,
    n_teams         INTEGER,
    n_supervisors   INTEGER,
    complete        BOOLEAN NOT NULL DEFAULT FALSE,
    notes           TEXT,
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE (session_id, area_name)
  )",
  
  "CREATE INDEX idx_plans_session  ON health_area_plans(session_id)",
  "CREATE INDEX idx_plans_district ON health_area_plans(district_name)",
  "CREATE INDEX idx_plans_complete ON health_area_plans(complete)",
  
  "CREATE TABLE supervisors (
    id                SERIAL PRIMARY KEY,
    health_area_plan  INTEGER NOT NULL REFERENCES health_area_plans(id) ON DELETE CASCADE,
    supervisor_number INTEGER NOT NULL,
    name              TEXT,
    role              TEXT,
    phone             TEXT,
    email             TEXT
  )",
  
  "CREATE INDEX idx_supervisors_plan ON supervisors(health_area_plan)"
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


# =============================================================================
# Step 4: Verify
# =============================================================================

cat("\nTables created:\n")
tables <- dbGetQuery(con, "
  SELECT table_name, table_type
  FROM information_schema.tables
  WHERE table_schema = 'public'
  ORDER BY table_type, table_name
")
print(tables)

cat("\nAdmin user seeded:\n")
print(dbGetQuery(con, "SELECT username, display_name, role FROM users"))

dbDisconnect(con)
cat("\nDone. Database ready.\n")

