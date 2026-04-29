# =============================================================================
# mod_db.R  —  Database connection + all read/write helpers
# =============================================================================

library(pool)
library(DBI)
library(RPostgres)

# =============================================================================
# Connection pool
# =============================================================================

db_connect <- function() {
  pool::dbPool(
    drv      = RPostgres::Postgres(),
    host     = Sys.getenv('DB_HOST'),
    port     = as.integer(Sys.getenv('DB_PORT', '5432')),
    dbname   = Sys.getenv('DB_NAME'),
    user     = Sys.getenv('DB_USER'),
    password = Sys.getenv('DB_PASSWORD'),
    sslmode  = Sys.getenv('DB_SSL', 'require'),
    minSize  = 1L,
    maxSize  = 5L,
    idleTimeout = 300000L
  )
}

# =============================================================================
# Helper: safe parameterised query
# =============================================================================

.db_query <- function(pool, sql, params = list()) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  DBI::dbGetQuery(conn, DBI::sqlInterpolate(conn, sql, .dots = params))
}

.db_execute <- function(pool, sql, params = list()) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  DBI::dbExecute(conn, DBI::sqlInterpolate(conn, sql, .dots = params))
}


# =============================================================================
# SECTION 1: Users
# =============================================================================

db_get_users <- function(pool) {
  .db_query(pool, "SELECT username, password, display_name, role FROM users ORDER BY username")
}

db_get_user_districts <- function(pool, username = NULL) {
  if (is.null(username)) {
    .db_query(pool, "SELECT username, district_name FROM user_districts")
  } else {
    .db_query(pool, "SELECT district_name FROM user_districts WHERE username = ?u",
              list(u = username))
  }
}

db_validate_credentials <- function(pool, uname, pword) {
  row <- .db_query(pool,
                   "SELECT username, display_name, role FROM users
     WHERE username = ?u AND password = ?p",
                   list(u = uname, p = pword))
  if (nrow(row) == 0) return(NULL)
  row[1L, ]
}

db_get_allowed_districts <- function(pool, username, role) {
  if (role == 'admin') return('ALL')
  res <- db_get_user_districts(pool, username)
  res$district_name
}

db_upsert_user <- function(pool, username, password, display_name, role) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  DBI::dbExecute(conn, "
    INSERT INTO users (username, password, display_name, role, updated_at)
    VALUES ($1, $2, $3, $4, NOW())
    ON CONFLICT (username) DO UPDATE SET
      password     = EXCLUDED.password,
      display_name = EXCLUDED.display_name,
      role         = EXCLUDED.role,
      updated_at   = NOW()
  ", list(username, password, display_name, role))
}

db_delete_user <- function(pool, username) {
  .db_execute(pool, "DELETE FROM users WHERE username = ?u", list(u = username))
}

db_set_user_districts <- function(pool, username, district_names) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  DBI::dbExecute(conn, "DELETE FROM user_districts WHERE username = $1", list(username))
  if (length(district_names) > 0) {
    rows <- data.frame(username = username, district_name = district_names,
                       stringsAsFactors = FALSE)
    DBI::dbWriteTable(conn, 'user_districts', rows, append = TRUE, row.names = FALSE)
  }
  invisible(NULL)
}


# =============================================================================
# SECTION 2: District submissions
#
# One row per district. Each tab's "Submit" button upserts its stage columns.
# Other stage columns are preserved via COALESCE.
#
# SQL migration — run once on your DB:
#
#   CREATE TABLE IF NOT EXISTS district_submissions (
#     district_name      TEXT PRIMARY KEY,
#     submitted_by       TEXT,
#     first_submitted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
#     last_submitted_at  TIMESTAMPTZ NOT NULL DEFAULT NOW(),
#     landmarks          JSONB,
#     odk_sf             JSONB,
#     app_sf             JSONB,
#     saved_dfa_sf       JSONB,
#     dfa_names          JSONB,
#     planning_data      JSONB,
#     has_landmarks      BOOLEAN NOT NULL DEFAULT FALSE,
#     has_facilities     BOOLEAN NOT NULL DEFAULT FALSE,
#     has_areas          BOOLEAN NOT NULL DEFAULT FALSE,
#     has_microplan      BOOLEAN NOT NULL DEFAULT FALSE
#   );
#
# =============================================================================

# JSON helpers (shared by both submission functions)
.to_json_for_db <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (inherits(x, 'sf') || inherits(x, 'sfc')) {
    # Store raw GeoJSON — GeoJSON is valid JSON so the JSONB column accepts it
    # directly as a JSON *object*, not a JSON *string*.
    # Wrapping with jsonlite::toJSON() would produce a JSON-encoded string value,
    # which PostgreSQL stores differently and which geojson_sf() cannot parse on
    # retrieval (it receives a quoted, escaped string, not a bare {…} object).
    tryCatch(
      geojsonsf::sf_geojson(x),
      error = function(e) NA_character_
    )
  } else {
    tryCatch(
      jsonlite::toJSON(x, auto_unbox = TRUE, null = 'null'),
      error = function(e) NA_character_
    )
  }
}

.from_json_sf_db <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(geojsonsf::geojson_sf(x), error = function(e) NULL)
}

# Deserialise a JSON column back to a named list (planning_data, etc.)
.from_json_db <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
}

# Deserialise a JSON column back to a data.frame (landmarks)
.from_json_df_db <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(
    as.data.frame(jsonlite::fromJSON(x, simplifyVector = TRUE, simplifyDataFrame = TRUE),
                  stringsAsFactors = FALSE),
    error = function(e) NULL
  )
}

# Deserialise a JSON column back to a plain character/numeric vector
.from_json_vec_db <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(jsonlite::fromJSON(x, simplifyVector = TRUE), error = function(e) NULL)
}


#' Get the most recent submission for a district.
#' Returns NULL if no submission exists, otherwise a list with:
#'   $district_name, $submitted_by, $first_submitted_at, $last_submitted_at,
#'   $has_landmarks, $has_facilities, $has_areas, $has_microplan,
#'   $snap  (list matching restore snapshot structure)
db_get_district_submission <- function(pool, district_name) {
  rows <- tryCatch(
    .db_query(pool, "
      SELECT district_name, submitted_by, first_submitted_at, last_submitted_at,
             landmarks, odk_sf, app_sf, saved_dfa_sf, dfa_names,
             current_assignments, planning_data,
             has_landmarks, has_facilities, has_areas, has_microplan
      FROM district_submissions
      WHERE district_name = ?d
    ", list(d = district_name)),
    error = function(e) { cat('[db] get_district_submission error:', e$message, '\n'); NULL }
  )
  
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  
  r <- rows[1, ]
  list(
    district_name      = r$district_name,
    submitted_by       = r$submitted_by,
    first_submitted_at = r$first_submitted_at,
    last_submitted_at  = r$last_submitted_at,
    has_landmarks      = isTRUE(r$has_landmarks),
    has_facilities     = isTRUE(r$has_facilities),
    has_areas          = isTRUE(r$has_areas),
    has_microplan      = isTRUE(r$has_microplan),
    snap = list(
      landmarks           = .from_json_df_db(r$landmarks),      # data.frame
      odk_sf              = .from_json_sf_db(r$odk_sf),         # sf
      app_sf              = .from_json_sf_db(r$app_sf),         # sf
      saved_dfa_sf        = .from_json_sf_db(r$saved_dfa_sf),   # sf
      dfa_names           = .from_json_vec_db(r$dfa_names),     # character vector
      current_assignments = .from_json_vec_db(r$current_assignments), # character vector
      planning_data       = .from_json_db(r$planning_data)      # nested list
    )
  )
}


#' Write one stage of progress to district_submissions.
#' stage = "landmarks" | "facilities" | "areas" | "microplan"
#' data  = list with stage-specific fields (others passed as NULL, preserved by COALESCE)
db_submit_stage <- function(pool, district_name, username, stage, data) {
  landmarks_json     <- if (stage == "landmarks")  .to_json_for_db(data$landmarks)    else NA_character_
  odk_sf_json        <- if (stage == "facilities") .to_json_for_db(data$odk_sf)       else NA_character_
  app_sf_json        <- if (stage == "facilities") .to_json_for_db(data$app_sf)       else NA_character_
  saved_dfa_sf_json       <- if (stage == "areas") .to_json_for_db(data$saved_dfa_sf)       else NA_character_
  dfa_names_json          <- if (stage == "areas") .to_json_for_db(data$dfa_names)          else NA_character_
  current_assignments_json <- if (stage == "areas") .to_json_for_db(data$current_assignments) else NA_character_
  planning_data_json      <- if (stage == "microplan") .to_json_for_db(data$planning_data)   else NA_character_
  
  has_landmarks  <- stage == "landmarks"
  has_facilities <- stage == "facilities"
  has_areas      <- stage == "areas"
  has_microplan  <- stage == "microplan"
  
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  
  tryCatch(
    DBI::dbExecute(conn, "
      INSERT INTO district_submissions (
        district_name, submitted_by, first_submitted_at, last_submitted_at,
        landmarks, odk_sf, app_sf, saved_dfa_sf, dfa_names,
        current_assignments, planning_data,
        has_landmarks, has_facilities, has_areas, has_microplan
      ) VALUES ($1,$2,NOW(),NOW(),$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13)
      ON CONFLICT (district_name) DO UPDATE SET
        submitted_by       = EXCLUDED.submitted_by,
        last_submitted_at  = NOW(),
        landmarks           = COALESCE(EXCLUDED.landmarks,           district_submissions.landmarks),
        odk_sf              = COALESCE(EXCLUDED.odk_sf,              district_submissions.odk_sf),
        app_sf              = COALESCE(EXCLUDED.app_sf,              district_submissions.app_sf),
        saved_dfa_sf        = COALESCE(EXCLUDED.saved_dfa_sf,        district_submissions.saved_dfa_sf),
        dfa_names           = COALESCE(EXCLUDED.dfa_names,           district_submissions.dfa_names),
        current_assignments = COALESCE(EXCLUDED.current_assignments, district_submissions.current_assignments),
        planning_data       = COALESCE(EXCLUDED.planning_data,       district_submissions.planning_data),
        has_landmarks  = district_submissions.has_landmarks  OR EXCLUDED.has_landmarks,
        has_facilities = district_submissions.has_facilities OR EXCLUDED.has_facilities,
        has_areas      = district_submissions.has_areas      OR EXCLUDED.has_areas,
        has_microplan  = district_submissions.has_microplan  OR EXCLUDED.has_microplan,
        first_submitted_at = LEAST(district_submissions.first_submitted_at, NOW())
    ", list(
      district_name, username,
      landmarks_json, odk_sf_json, app_sf_json,
      saved_dfa_sf_json, dfa_names_json, current_assignments_json, planning_data_json,
      has_landmarks, has_facilities, has_areas, has_microplan
    )),
    error = function(e) {
      cat('[db] submit_stage error (', stage, '):', e$message, '\n')
      stop(e)
    }
  )
  
  invisible(NULL)
}


#' Get all submitted districts for the admin progress table.
#' Returns a data frame with stage flags only (no blob columns).
db_get_all_submissions <- function(pool) {
  tryCatch(
    .db_query(pool, "
      SELECT district_name, submitted_by, first_submitted_at, last_submitted_at,
             has_landmarks, has_facilities, has_areas, has_microplan
      FROM district_submissions
      ORDER BY last_submitted_at DESC NULLS LAST
    "),
    error = function(e) { cat('[db] get_all_submissions error:', e$message, '\n'); NULL }
  )
}


#' Get full submission data for a single district (for admin Review modal).
db_get_submission_for_review <- function(pool, district_name) {
  db_get_district_submission(pool, district_name)
}


#' Delete a district's submission entirely (used by the Reject button in admin).
db_delete_district_submission <- function(pool, district_name) {
  .db_execute(pool,
              "DELETE FROM district_submissions WHERE district_name = ?d",
              list(d = district_name))
  invisible(NULL)
}
