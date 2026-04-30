library(DBI)
library(RPostgres)
library(bcrypt)

# =============================================================================
# CONFIGURATION — set these before running
# =============================================================================

DB_HOST          <- '23.239.19.115'
DB_PORT          <- 5432
POSTGRES_PASSWORD <- ''               # postgres superuser password
SHINY_APP_PASSWORD <- 'CHANGE_ME'     # password for shiny_app user
ADMIN_PASSWORD   <- 'CHANGE_ME'       # password for the admin app user

# =============================================================================
# Step 1: Connect as postgres superuser and create database + app user
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

tryCatch(
  { dbExecute(con_admin, "CREATE DATABASE somalia_health_areas"); cat("Database created.\n") },
  error = function(e) cat("Database already exists — skipping.\n")
)

tryCatch(
  { dbExecute(con_admin, sprintf("CREATE USER shiny_app WITH PASSWORD '%s'", SHINY_APP_PASSWORD))
    cat("User shiny_app created.\n") },
  error = function(e) cat("shiny_app user already exists — skipping creation.\n")
)

dbExecute(con_admin, "GRANT CONNECT ON DATABASE somalia_health_areas TO shiny_app")
cat("Connect privilege granted.\n")

dbDisconnect(con_admin)
cat("Step 1 complete.\n\n")


# =============================================================================
# Step 2: Connect to new database and set up schema
# =============================================================================

con <- dbConnect(
  RPostgres::Postgres(),
  host     = DB_HOST,
  port     = DB_PORT,
  dbname   = 'somalia_health_areas',
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
  
  # ── Users ──────────────────────────────────────────────────────────────────
  "CREATE TABLE IF NOT EXISTS users (
    username      TEXT PRIMARY KEY,
    password      TEXT        NOT NULL,
    display_name  TEXT        NOT NULL,
    role          TEXT        NOT NULL DEFAULT 'user'
                              CHECK (role IN ('admin', 'user')),
    created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at    TIMESTAMPTZ NOT NULL DEFAULT NOW()
  )",
  
  "CREATE TABLE IF NOT EXISTS user_districts (
    id            SERIAL PRIMARY KEY,
    username      TEXT NOT NULL REFERENCES users(username) ON DELETE CASCADE,
    district_name TEXT NOT NULL,
    UNIQUE (username, district_name)
  )",
  
  "CREATE INDEX IF NOT EXISTS idx_user_districts_username ON user_districts(username)",
  
  # ── District submissions ───────────────────────────────────────────────────
  "CREATE TABLE IF NOT EXISTS district_submissions (
    district_name      TEXT        PRIMARY KEY,
    submitted_by       TEXT,
    first_submitted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_submitted_at  TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    landmarks          JSONB,
    odk_sf             JSONB,
    app_sf             JSONB,
    saved_dfa_sf       JSONB,
    dfa_names          JSONB,
    current_assignments JSONB,
    planning_data      JSONB,
    has_landmarks      BOOLEAN NOT NULL DEFAULT FALSE,
    has_facilities     BOOLEAN NOT NULL DEFAULT FALSE,
    has_areas          BOOLEAN NOT NULL DEFAULT FALSE,
    has_microplan      BOOLEAN NOT NULL DEFAULT FALSE
  )",
  
  "CREATE INDEX IF NOT EXISTS idx_district_submissions_last_submitted
     ON district_submissions (last_submitted_at DESC)"
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
# Step 4: Seed admin user
# =============================================================================

tryCatch({
  hashed_admin <- bcrypt::hashpw(ADMIN_PASSWORD)
  DBI::dbExecute(con,
                 "INSERT INTO users (username, password, display_name, role)
     VALUES ($1, $2, 'Admin', 'admin')
     ON CONFLICT (username) DO NOTHING",
                 list('admin', hashed_admin)
  )
  cat("\nAdmin user seeded (password hashed).\n")
}, error = function(e) cat("Admin user error:", e$message, "\n"))


# =============================================================================
# Step 5: Verify
# =============================================================================

cat("\nTables:\n")
print(dbGetQuery(con, "
  SELECT table_name FROM information_schema.tables
  WHERE table_schema = 'public' ORDER BY table_name
"))

cat("\nUsers:\n")
print(dbGetQuery(con, "SELECT username, display_name, role FROM users"))

dbDisconnect(con)
cat("\nDone. Database ready.\n")
