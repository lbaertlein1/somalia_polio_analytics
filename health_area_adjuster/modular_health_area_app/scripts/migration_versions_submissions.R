# =============================================================================
# run_migration_versioned_submissions.R
#
# Verification only — SQL migration already applied directly via psql.
# Confirms all expected columns and indexes are present.
# =============================================================================

library(DBI)
library(RPostgres)

if (file.exists(".env")) readRenviron(".env")

cat("Connecting to:", Sys.getenv("DB_HOST"), "/", Sys.getenv("DB_NAME"), "\n")

conn <- tryCatch(
  DBI::dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("DB_HOST"),
    port     = as.integer(Sys.getenv("DB_PORT", "5432")),
    dbname   = Sys.getenv("DB_NAME"),
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    sslmode  = Sys.getenv("DB_SSL", "require")
  ),
  error = function(e) { cat("Connection FAILED:", e$message, "\n"); NULL }
)

if (is.null(conn)) stop("Aborting — could not connect.")
cat("Connected.\n\n")

# =============================================================================
# 1. Check columns
# =============================================================================

cat("--- Columns on district_submissions ---\n")
cols <- DBI::dbGetQuery(conn,
                        "SELECT column_name, data_type, is_nullable, column_default
   FROM information_schema.columns
   WHERE table_name = 'district_submissions'
   ORDER BY ordinal_position"
)
print(cols)

expected_cols <- c("district_name", "version", "is_current", "is_practice",
                   "archived_at", "restored_from_version",
                   "submitted_by", "first_submitted_at", "last_submitted_at",
                   "landmarks", "odk_sf", "app_sf", "saved_dfa_sf", "dfa_names",
                   "current_assignments", "planning_data",
                   "has_landmarks", "has_facilities", "has_areas", "has_microplan")

missing <- setdiff(expected_cols, cols$column_name)
if (length(missing) == 0) {
  cat("\n[OK] All expected columns present.\n")
} else {
  cat("\n[WARN] Missing columns:", paste(missing, collapse = ", "), "\n")
}

# =============================================================================
# 2. Check indexes
# =============================================================================

cat("\n--- Indexes on district_submissions ---\n")
idx <- DBI::dbGetQuery(conn,
                       "SELECT indexname, indexdef
   FROM pg_indexes
   WHERE tablename = 'district_submissions'
   ORDER BY indexname"
)
print(idx)

expected_idx <- c("district_submissions_pkey", "idx_ds_current", "idx_ds_history")
missing_idx  <- setdiff(expected_idx, idx$indexname)
if (length(missing_idx) == 0) {
  cat("\n[OK] All expected indexes present.\n")
} else {
  cat("\n[WARN] Missing indexes:", paste(missing_idx, collapse = ", "), "\n")
}

# =============================================================================
# 3. Check primary key
# =============================================================================

cat("\n--- Primary key columns ---\n")
pk <- DBI::dbGetQuery(conn,
                      "SELECT kcu.column_name
   FROM information_schema.table_constraints tc
   JOIN information_schema.key_column_usage kcu
     ON tc.constraint_name = kcu.constraint_name
   WHERE tc.table_name = 'district_submissions'
     AND tc.constraint_type = 'PRIMARY KEY'
   ORDER BY kcu.ordinal_position"
)
cat("PK columns:", paste(pk$column_name, collapse = ", "), "\n")

expected_pk <- c("district_name", "is_practice", "version")
if (identical(sort(pk$column_name), sort(expected_pk))) {
  cat("[OK] Primary key is correct.\n")
} else {
  cat("[WARN] Primary key does not match expected (district_name, is_practice, version).\n")
}

# =============================================================================
# 4. Spot-check existing rows
# =============================================================================

cat("\n--- Row check ---\n")
rows <- DBI::dbGetQuery(conn,
                        "SELECT district_name, version, is_current, is_practice,
          first_submitted_at, archived_at
   FROM district_submissions
   ORDER BY district_name, is_practice, version
   LIMIT 10"
)
if (nrow(rows) == 0) {
  cat("No rows found (table is empty).\n")
} else {
  print(rows)
}

DBI::dbDisconnect(conn)
cat("\nDisconnected.\n")

