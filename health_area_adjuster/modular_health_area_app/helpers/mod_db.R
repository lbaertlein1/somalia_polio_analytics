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
  .db_query(pool, "SELECT username, password, display_name, role, temp_password FROM users ORDER BY username")
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
                   "SELECT username, display_name, role, password FROM users
                    WHERE username = ?u",
                   list(u = uname))
  if (nrow(row) == 0) return(NULL)
  stored <- row$password[1L]
  valid <- if (startsWith(stored, '$2')) {
    tryCatch(bcrypt::checkpw(pword, stored), error = function(e) FALSE)
  } else {
    identical(pword, stored)
  }
  if (!isTRUE(valid)) return(NULL)
  row[1L, c('username', 'display_name', 'role')]
}

db_get_allowed_districts <- function(pool, username, role) {
  if (role == 'admin') return('ALL')
  res <- db_get_user_districts(pool, username)
  res$district_name
}

db_upsert_user <- function(pool, username, password, display_name, role) {
  if (nzchar(password %||% '')) {
    hashed <- bcrypt::hashpw(password)
    conn   <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(conn))
    DBI::dbExecute(conn, "
      INSERT INTO users (username, password, temp_password, display_name, role, updated_at)
      VALUES ($1, $2, $3, $4, $5, NOW())
      ON CONFLICT (username) DO UPDATE SET
        password      = EXCLUDED.password,
        temp_password = EXCLUDED.temp_password,
        display_name  = EXCLUDED.display_name,
        role          = EXCLUDED.role,
        updated_at    = NOW()
    ", list(username, hashed, password, display_name, role))
  } else {
    .db_execute(pool, "
      UPDATE users SET display_name = ?dn, role = ?r, updated_at = NOW()
      WHERE username = ?u
    ", list(dn = display_name, r = role, u = username))
  }
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
# SECTION 2: District submissions (versioned)
#
# Schema (district_name, is_practice, version) is the primary key.
# is_current = TRUE marks the active version for a given district+mode.
# archived_at is stamped when a version is superseded (start new or restore).
# restored_from_version records which version a restore operation copied from.
#
# SQL migration: see migration_versioned_submissions.sql
# =============================================================================

# --- JSON helpers -------------------------------------------------------------

.to_json_for_db <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (inherits(x, 'sf') || inherits(x, 'sfc')) {
    tryCatch(geojsonsf::sf_geojson(x), error = function(e) NA_character_)
  } else {
    tryCatch(jsonlite::toJSON(x, auto_unbox = TRUE, null = 'null'),
             error = function(e) NA_character_)
  }
}

.from_json_sf_db <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(geojsonsf::geojson_sf(x), error = function(e) NULL)
}

.from_json_db <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
}

.from_json_df_db <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(
    as.data.frame(jsonlite::fromJSON(x, simplifyVector = TRUE, simplifyDataFrame = TRUE),
                  stringsAsFactors = FALSE),
    error = function(e) NULL
  )
}

.from_json_vec_db <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(jsonlite::fromJSON(x, simplifyVector = TRUE), error = function(e) NULL)
}

# --- Row deserialiser (shared by all fetch functions) ------------------------

.parse_submission_row <- function(r) {
  list(
    district_name         = r$district_name,
    submitted_by          = r$submitted_by,
    version               = r$version,
    is_practice           = isTRUE(r$is_practice),
    is_current            = isTRUE(r$is_current),
    first_submitted_at    = r$first_submitted_at,
    last_submitted_at     = r$last_submitted_at,
    archived_at           = r$archived_at,
    restored_from_version = r$restored_from_version,
    has_landmarks         = isTRUE(r$has_landmarks),
    has_facilities        = isTRUE(r$has_facilities),
    has_areas             = isTRUE(r$has_areas),
    has_microplan         = isTRUE(r$has_microplan),
    snap = list(
      landmarks           = .from_json_df_db(r$landmarks),
      odk_sf              = .from_json_sf_db(r$odk_sf),
      app_sf              = .from_json_sf_db(r$app_sf),
      saved_dfa_sf        = .from_json_sf_db(r$saved_dfa_sf),
      dfa_names           = .from_json_vec_db(r$dfa_names),
      current_assignments = .from_json_vec_db(r$current_assignments),
      planning_data       = .from_json_db(r$planning_data)
    )
  )
}

# --- Fetch current version ---------------------------------------------------

#' Get the current (active) submission for a district+mode.
#' Returns NULL if none exists.
db_get_district_submission <- function(pool, district_name, is_practice = FALSE) {
  rows <- tryCatch(
    .db_query(pool, "
      SELECT district_name, submitted_by, version, is_practice, is_current,
             first_submitted_at, last_submitted_at, archived_at,
             restored_from_version,
             landmarks, odk_sf, app_sf, saved_dfa_sf, dfa_names,
             current_assignments, planning_data,
             has_landmarks, has_facilities, has_areas, has_microplan
      FROM district_submissions
      WHERE district_name = ?d
        AND is_practice   = ?p
        AND is_current    = TRUE
    ", list(d = district_name, p = is_practice)),
    error = function(e) { cat('[db] get_district_submission error:', e$message, '\n'); NULL }
  )
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  .parse_submission_row(rows[1, ])
}

# --- Fetch all versions (metadata only, no blobs) ----------------------------

#' Get version history for a district+mode (admin use).
#' Returns a data frame ordered newest first. No blob columns.
db_get_submission_versions <- function(pool, district_name, is_practice = FALSE) {
  tryCatch(
    .db_query(pool, "
      SELECT district_name, version, is_practice, is_current,
             submitted_by, first_submitted_at, last_submitted_at,
             archived_at, restored_from_version,
             has_landmarks, has_facilities, has_areas, has_microplan
      FROM district_submissions
      WHERE district_name = ?d
        AND is_practice   = ?p
      ORDER BY version DESC
    ", list(d = district_name, p = is_practice)),
    error = function(e) { cat('[db] get_submission_versions error:', e$message, '\n'); NULL }
  )
}

# --- Fetch a specific version (full data, for restore/review) ----------------

#' Get a specific version of a submission including blob data.
db_get_submission_version <- function(pool, district_name, version, is_practice = FALSE) {
  rows <- tryCatch(
    .db_query(pool, "
      SELECT district_name, submitted_by, version, is_practice, is_current,
             first_submitted_at, last_submitted_at, archived_at,
             restored_from_version,
             landmarks, odk_sf, app_sf, saved_dfa_sf, dfa_names,
             current_assignments, planning_data,
             has_landmarks, has_facilities, has_areas, has_microplan
      FROM district_submissions
      WHERE district_name = ?d
        AND is_practice   = ?p
        AND version       = ?v
    ", list(d = district_name, p = is_practice, v = as.integer(version))),
    error = function(e) { cat('[db] get_submission_version error:', e$message, '\n'); NULL }
  )
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  .parse_submission_row(rows[1, ])
}

# --- Submit stage (upsert into current version) ------------------------------

#' Write one stage of progress to the current version row.
#' Creates the first version (v1) if none exists for this district+mode.
#' stage = "landmarks" | "facilities" | "areas" | "microplan"
db_submit_stage <- function(pool, district_name, username, stage, data,
                            is_practice = FALSE) {
  
  landmarks_json           <- if (stage == "landmarks")  .to_json_for_db(data$landmarks)            else NA_character_
  odk_sf_json              <- if (stage == "facilities") .to_json_for_db(data$odk_sf)               else NA_character_
  app_sf_json              <- if (stage == "facilities") .to_json_for_db(data$app_sf)               else NA_character_
  saved_dfa_sf_json        <- if (stage == "areas")      .to_json_for_db(data$saved_dfa_sf)         else NA_character_
  dfa_names_json           <- if (stage == "areas")      .to_json_for_db(data$dfa_names)            else NA_character_
  current_assignments_json <- if (stage == "areas")      .to_json_for_db(data$current_assignments)  else NA_character_
  planning_data_json       <- if (stage == "microplan")  .to_json_for_db(data$planning_data)        else NA_character_
  
  has_landmarks  <- stage == "landmarks"
  has_facilities <- stage == "facilities"
  has_areas      <- stage == "areas"
  has_microplan  <- stage == "microplan"
  
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  
  # Check if a current version already exists — update it; otherwise insert v1
  cur <- DBI::dbGetQuery(conn,
                         "SELECT version FROM district_submissions
     WHERE district_name = $1 AND is_practice = $2 AND is_current = TRUE",
                         list(district_name, is_practice)
  )
  
  if (nrow(cur) > 0) {
    # UPDATE the existing current row in place
    tryCatch(
      DBI::dbExecute(conn, "
        UPDATE district_submissions SET
          submitted_by        = $3,
          last_submitted_at   = NOW(),
          landmarks           = COALESCE($4,  landmarks),
          odk_sf              = COALESCE($5,  odk_sf),
          app_sf              = COALESCE($6,  app_sf),
          saved_dfa_sf        = COALESCE($7,  saved_dfa_sf),
          dfa_names           = COALESCE($8,  dfa_names),
          current_assignments = COALESCE($9,  current_assignments),
          planning_data       = COALESCE($10, planning_data),
          has_landmarks  = has_landmarks  OR $11,
          has_facilities = has_facilities OR $12,
          has_areas      = has_areas      OR $13,
          has_microplan  = has_microplan  OR $14
        WHERE district_name = $1 AND is_practice = $2 AND is_current = TRUE
      ", list(
        district_name, is_practice,
        username,
        landmarks_json, odk_sf_json, app_sf_json,
        saved_dfa_sf_json, dfa_names_json, current_assignments_json, planning_data_json,
        has_landmarks, has_facilities, has_areas, has_microplan
      )),
      error = function(e) {
        cat('[db] submit_stage UPDATE error (', stage, '):', e$message, '\n')
        stop(e)
      }
    )
  } else {
    # INSERT first version for this district+mode
    next_ver <- tryCatch({
      res <- DBI::dbGetQuery(conn,
                             "SELECT COALESCE(MAX(version), 0) + 1 AS nv FROM district_submissions
         WHERE district_name = $1 AND is_practice = $2",
                             list(district_name, is_practice))
      as.integer(res$nv[1])
    }, error = function(e) 1L)
    
    tryCatch(
      DBI::dbExecute(conn, "
        INSERT INTO district_submissions (
          district_name, is_practice, version, is_current,
          submitted_by, first_submitted_at, last_submitted_at,
          landmarks, odk_sf, app_sf, saved_dfa_sf, dfa_names,
          current_assignments, planning_data,
          has_landmarks, has_facilities, has_areas, has_microplan
        ) VALUES ($1,$2,$3,TRUE,$4,NOW(),NOW(),$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15)
      ", list(
        district_name, is_practice, next_ver,
        username,
        landmarks_json, odk_sf_json, app_sf_json,
        saved_dfa_sf_json, dfa_names_json, current_assignments_json, planning_data_json,
        has_landmarks, has_facilities, has_areas, has_microplan
      )),
      error = function(e) {
        cat('[db] submit_stage INSERT error (', stage, '):', e$message, '\n')
        stop(e)
      }
    )
  }
  
  invisible(NULL)
}

# --- Archive current version (called on "Start new") -------------------------

#' Mark the current version as archived, stamping archived_at = NOW().
#' Returns the version number that was archived, or NULL if nothing was current.
db_archive_current_submission <- function(pool, district_name, is_practice = FALSE) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  
  rows <- DBI::dbGetQuery(conn,
                          "UPDATE district_submissions
     SET is_current = FALSE, archived_at = NOW()
     WHERE district_name = $1 AND is_practice = $2 AND is_current = TRUE
     RETURNING version",
                          list(district_name, is_practice)
  )
  
  if (nrow(rows) == 0) return(NULL)
  as.integer(rows$version[1])
}

# --- Restore an archived version (admin action) ------------------------------

#' Archive the current version (if any), then insert a new current version
#' copied from the requested archived version.
#' The new row gets first_submitted_at = NOW() (restore date) and
#' restored_from_version = source version.
db_restore_submission_version <- function(pool, district_name, version,
                                          is_practice = FALSE) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  
  tryCatch({
    # Archive whatever is currently active
    DBI::dbExecute(conn,
                   "UPDATE district_submissions
       SET is_current = FALSE, archived_at = NOW()
       WHERE district_name = $1 AND is_practice = $2 AND is_current = TRUE",
                   list(district_name, is_practice)
    )
    
    # Next version number
    res     <- DBI::dbGetQuery(conn,
                               "SELECT COALESCE(MAX(version), 0) + 1 AS nv FROM district_submissions
       WHERE district_name = $1 AND is_practice = $2",
                               list(district_name, is_practice))
    new_ver <- as.integer(res$nv[1])
    
    # Insert new current row as copy of the requested version
    DBI::dbExecute(conn, "
      INSERT INTO district_submissions (
        district_name, is_practice, version, is_current,
        submitted_by, first_submitted_at, last_submitted_at,
        restored_from_version,
        landmarks, odk_sf, app_sf, saved_dfa_sf, dfa_names,
        current_assignments, planning_data,
        has_landmarks, has_facilities, has_areas, has_microplan
      )
      SELECT
        district_name, is_practice, $3, TRUE,
        submitted_by, NOW(), NOW(),
        $4,
        landmarks, odk_sf, app_sf, saved_dfa_sf, dfa_names,
        current_assignments, planning_data,
        has_landmarks, has_facilities, has_areas, has_microplan
      FROM district_submissions
      WHERE district_name = $1 AND is_practice = $2 AND version = $5
    ", list(district_name, is_practice, new_ver, as.integer(version), as.integer(version)))
    
    invisible(new_ver)
  }, error = function(e) {
    cat('[db] restore_submission_version error:', e$message, '\n')
    stop(e)
  })
}

# --- Get all current submissions (admin progress table) ----------------------

#' Returns metadata for all current (active) submissions.
#' is_practice parameter controls which chain to show.
db_get_all_submissions <- function(pool, is_practice = FALSE) {
  tryCatch(
    .db_query(pool, "
      SELECT district_name, submitted_by, version,
             first_submitted_at, last_submitted_at,
             has_landmarks, has_facilities, has_areas, has_microplan,
             is_practice
      FROM district_submissions
      WHERE is_current  = TRUE
        AND is_practice = ?p
      ORDER BY last_submitted_at DESC NULLS LAST
    ", list(p = is_practice)),
    error = function(e) { cat('[db] get_all_submissions error:', e$message, '\n'); NULL }
  )
}

# --- Review helpers ----------------------------------------------------------

db_get_submission_for_review <- function(pool, district_name,
                                         is_practice = FALSE, version = NULL) {
  if (is.null(version)) {
    db_get_district_submission(pool, district_name, is_practice)
  } else {
    db_get_submission_version(pool, district_name, version, is_practice)
  }
}

# --- Delete (admin reject — removes ALL versions for district+mode) ----------

db_delete_district_submission <- function(pool, district_name, is_practice = FALSE) {
  .db_execute(pool,
              "DELETE FROM district_submissions
     WHERE district_name = ?d AND is_practice = ?p",
              list(d = district_name, p = is_practice))
  invisible(NULL)
}