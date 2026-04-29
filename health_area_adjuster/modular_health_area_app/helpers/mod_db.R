# =============================================================================
# mod_db.R  —  Database connection + all read/write helpers
#
# Uses the pool package for connection pooling (safe for concurrent Shiny users).
# All functions return the same data structures as the Phase 1 file-based code
# so module logic requires minimal changes.
#
# Credentials are read from environment variables — never hardcode them.
# Set in shinyapps.io dashboard, or locally via a .env file + dotenv::load_dot_env().
#
# Phase 6 migration checklist:
#   [x] mod_db.R  (this file)
#   [ ] mod_auth.R         — replace validate_credentials() + district lookup
#   [ ] mod_session_manager.R — replace .read_sessions() + .write_session()
#   [ ] mod_admin_tab.R    — replace .read_users() + .write_users() etc.
# =============================================================================

library(pool)
library(DBI)
library(RPostgres)

# =============================================================================
# Connection pool (created once at startup in global.R)
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
    # Pool sizing — shinyapps.io free tier is single-instance
    minSize  = 1L,
    maxSize  = 5L,
    idleTimeout = 300000L  # 5 minutes
  )
}

# Call this in global.R:
#   pool <- db_connect()
#
# And register cleanup on app stop:
#   onStop(function() pool::poolClose(pool))


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
# Replaces: users_df, user_districts_df, .read_users(), .write_users()
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
  # Replace all assignments for this user
  DBI::dbExecute(conn, "DELETE FROM user_districts WHERE username = $1", list(username))
  if (length(district_names) > 0) {
    rows <- data.frame(username = username, district_name = district_names,
                       stringsAsFactors = FALSE)
    DBI::dbWriteTable(conn, 'user_districts', rows, append = TRUE, row.names = FALSE)
  }
  invisible(NULL)
}


# =============================================================================
# SECTION 2: Sessions
# Replaces: .read_sessions(), .write_session(), file-based .rds logic
# =============================================================================

db_list_sessions <- function(pool, username, district_name) {
  .db_query(pool,
    "SELECT session_id, username, district_name, started_at, saved_at
     FROM sessions
     WHERE username = ?u AND district_name = ?d
     ORDER BY started_at DESC
     LIMIT ?lim",
    list(u = username, d = district_name, lim = SESSION_MAX_SAVED))
}

db_create_session <- function(pool, session_id, username, district_name) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  DBI::dbExecute(conn, "
    INSERT INTO sessions (session_id, username, district_name, started_at, saved_at)
    VALUES ($1, $2, $3, NOW(), NOW())
    ON CONFLICT (session_id) DO UPDATE SET saved_at = NOW()
  ", list(session_id, username, district_name))
}

db_update_session_saved_at <- function(pool, session_id) {
  .db_execute(pool,
    "UPDATE sessions SET saved_at = NOW() WHERE session_id = ?sid",
    list(sid = session_id))
}

db_prune_old_sessions <- function(pool, username, district_name) {
  # Keep only the most recent SESSION_MAX_SAVED sessions
  .db_execute(pool, "
    DELETE FROM sessions
    WHERE username = ?u AND district_name = ?d
      AND session_id NOT IN (
        SELECT session_id FROM sessions
        WHERE username = ?u AND district_name = ?d
        ORDER BY started_at DESC
        LIMIT ?lim
      )
  ", list(u = username, d = district_name, lim = SESSION_MAX_SAVED))
}


# =============================================================================
# SECTION 3: Snapshots
# Replaces: history list inside .rds session files
# =============================================================================

db_save_snapshot <- function(pool, session_id, trigger, snapshot_data) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  # Serialise sf objects to GeoJSON text; other objects to JSON
  .to_json <- function(x) {
    if (is.null(x)) return(NA_character_)
    if (inherits(x, 'sf') || inherits(x, 'sfc')) {
      tryCatch(
        jsonlite::toJSON(geojsonsf::sf_geojson(x), auto_unbox = TRUE),
        error = function(e) NA_character_
      )
    } else {
      tryCatch(
        jsonlite::toJSON(x, auto_unbox = TRUE, null = 'null'),
        error = function(e) NA_character_
      )
    }
  }

  DBI::dbExecute(conn, "
    INSERT INTO session_snapshots
      (session_id, snapshot_at, trigger,
       odk_sf, app_sf, landmarks,
       current_assignments, saved_dfa_sf, dfa_names,
       planning_data)
    VALUES ($1, NOW(), $2, $3, $4, $5, $6, $7, $8, $9)
  ", list(
    session_id,
    trigger,
    .to_json(snapshot_data$odk_sf),
    .to_json(snapshot_data$app_sf),
    .to_json(snapshot_data$landmarks),
    .to_json(snapshot_data$current_assignments),
    .to_json(snapshot_data$saved_dfa_sf),
    .to_json(snapshot_data$dfa_names),
    .to_json(snapshot_data$planning_data)
  ))

  # Prune old snapshots beyond cap
  .db_execute(pool, "
    DELETE FROM session_snapshots
    WHERE session_id = ?sid
      AND snapshot_id NOT IN (
        SELECT snapshot_id FROM session_snapshots
        WHERE session_id = ?sid
        ORDER BY snapshot_at DESC
        LIMIT ?lim
      )
  ", list(sid = session_id, lim = SESSION_MAX_HISTORY))

  invisible(NULL)
}

db_get_snapshots <- function(pool, session_id) {
  rows <- .db_query(pool, "
    SELECT snapshot_id, snapshot_at, trigger,
           odk_sf, app_sf, landmarks,
           current_assignments, saved_dfa_sf, dfa_names,
           planning_data
    FROM session_snapshots
    WHERE session_id = ?sid
    ORDER BY snapshot_at ASC
  ", list(sid = session_id))

  if (nrow(rows) == 0) return(list())

  .from_json_sf <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NULL)
    tryCatch(geojsonsf::geojson_sf(x), error = function(e) NULL)
  }
  .from_json <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NULL)
    tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
  }

  lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    list(
      snapshot_at         = r$snapshot_at,
      trigger             = r$trigger,
      odk_sf              = .from_json_sf(r$odk_sf),
      app_sf              = .from_json_sf(r$app_sf),
      landmarks           = .from_json(r$landmarks),
      current_assignments = .from_json(r$current_assignments),
      saved_dfa_sf        = .from_json_sf(r$saved_dfa_sf),
      dfa_names           = .from_json(r$dfa_names),
      planning_data       = .from_json(r$planning_data)
    )
  })
}


# =============================================================================
# SECTION 4: Progress (admin)
# Replaces: .load_progress() in mod_admin_tab.R
# =============================================================================

db_get_progress <- function(pool) {
  .db_query(pool, "
    SELECT
      s.session_id,
      s.username,
      s.district_name,
      s.saved_at,
      s.started_at,
      sn.planning_data,
      sn.saved_dfa_sf
    FROM sessions s
    JOIN latest_snapshots sn USING (session_id)
    ORDER BY s.saved_at DESC
  ")
}


# =============================================================================
# SECTION 5: Health area plans (denormalised, queryable)
# Called from mod_microplan_tab.R whenever planning data is saved.
# Writes to health_area_plans + supervisors tables directly.
# =============================================================================

#' Upsert all health area plans for a session.
#' Replaces all existing rows for this session_id then re-inserts.
#'
#' @param pool        DB connection pool
#' @param session_id  character — current session ID
#' @param district_name character
#' @param planning_data named list — same structure as rv$planning_data
#'   list(
#'     "Health Area 1" = list(
#'       u5_pop=4200, n_teams=11, n_supervisors=3, complete=TRUE, notes='',
#'       supervisors=list(
#'         list(name='X', role='Y', phone='Z', email='W'), ...
#'       )
#'     ), ...
#'   )

db_upsert_health_area_plans <- function(pool, session_id, district_name, planning_data) {
  if (length(planning_data) == 0) return(invisible(NULL))

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  # Wrap in a transaction so partial writes don't leave inconsistent state
  DBI::dbWithTransaction(conn, {

    # Delete existing plans for this session (supervisors cascade)
    DBI::dbExecute(conn,
      "DELETE FROM health_area_plans WHERE session_id = $1",
      list(session_id)
    )

    slug <- gsub('[^A-Za-z0-9]', '_', tolower(trimws(district_name)))

    for (area_name in names(planning_data)) {
      d   <- planning_data[[area_name]]
      uid <- paste0(slug, '__',
                    gsub('[^A-Za-z0-9]', '_', tolower(trimws(area_name))))

      # Insert health area plan row
      plan_id <- DBI::dbGetQuery(conn, "
        INSERT INTO health_area_plans
          (session_id, district_name, area_name, uid,
           u5_pop, n_teams, n_supervisors, complete, notes, updated_at)
        VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, NOW())
        RETURNING id
      ", list(
        session_id,
        district_name,
        area_name,
        uid,
        as.numeric(d$u5_pop        %||% NA),
        as.integer(d$n_teams       %||% NA),
        as.integer(d$n_supervisors %||% NA),
        isTRUE(d$complete),
        trimws(d$notes %||% '')
      ))$id

      # Insert supervisor rows
      sups <- d$supervisors %||% list()
      for (i in seq_along(sups)) {
        s <- sups[[i]] %||% list()
        DBI::dbExecute(conn, "
          INSERT INTO supervisors
            (health_area_plan, supervisor_number, name, role, phone, email)
          VALUES ($1, $2, $3, $4, $5, $6)
        ", list(
          plan_id,
          as.integer(i),
          trimws(s$name  %||% ''),
          trimws(s$role  %||% ''),
          trimws(s$phone %||% ''),
          trimws(s$email %||% '')
        ))
      }
    }
  })

  invisible(NULL)
}
