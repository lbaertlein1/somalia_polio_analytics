# =============================================================================
# mod_db_v2.R  —  Database connection + all read/write helpers (v2 schema)
#
# Replaces mod_db.R. Built fresh against create_db_v2.R rather than adapted
# in place — the ownership/publish/branch model is a different paradigm from
# the old single-lineage-per-district version chain, not a variation of it.
#
# Core concepts:
#   - Every row in mapping_versions is OWNED by a user. Users are never
#     scoped to specific districts.
#   - A user's own versions are always editable by them, published or not.
#     Editing a version that is_shared = TRUE forks a NEW row instead of
#     mutating the shared one in place (db_submit_stage_v2 handles this).
#   - Publishing (db_publish_version) is a transaction: unshare whatever was
#     previously shared for that (district_name, campaign_id), then mark the
#     new version shared. Never more than one shared version per
#     district+campaign at a time.
#   - "Branch from a shared version" and "carry forward from a prior
#     campaign" are the same underlying operation (db_branch_version) — the
#     source version's campaign_id may differ from the new version's.
#   - Facilities/boundary snapshots are locked once (at first facilities-
#     stage submit, or explicit refresh) and never silently re-fetched.
#   - No practice/actual mode. No user_districts / district scoping.
# =============================================================================

library(pool)
library(DBI)
library(RPostgres)


# =============================================================================
# Connection pool (unchanged from mod_db.R)
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
# SECTION 1: Users  (district scoping removed — any user, any district)
# =============================================================================

db_get_users <- function(pool) {
  .db_query(pool, "SELECT username, password, display_name, role FROM users ORDER BY username")
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

db_upsert_user <- function(pool, username, password, display_name, role) {
  if (nzchar(password %||% '')) {
    hashed <- bcrypt::hashpw(password)
    conn   <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(conn))
    DBI::dbExecute(conn, "
      INSERT INTO users (username, password, display_name, role, updated_at)
      VALUES ($1, $2, $3, $4, NOW())
      ON CONFLICT (username) DO UPDATE SET
        password      = EXCLUDED.password,
        display_name  = EXCLUDED.display_name,
        role          = EXCLUDED.role,
        updated_at    = NOW()
    ", list(username, hashed, display_name, role))
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


# =============================================================================
# SECTION 2: Campaigns  (admin-created, shared across all users)
# =============================================================================

db_get_campaigns <- function(pool, active_only = TRUE) {
  sql <- "SELECT campaign_id, campaign_name, description, created_by, created_at, is_active
          FROM campaigns"
  if (active_only) sql <- paste(sql, "WHERE is_active = TRUE")
  sql <- paste(sql, "ORDER BY created_at DESC")
  tryCatch(.db_query(pool, sql), error = function(e) { cat('[db] get_campaigns error:', e$message, '\n'); NULL })
}

db_create_campaign <- function(pool, campaign_name, description, created_by) {
  tryCatch({
    row <- .db_query(pool, "
      INSERT INTO campaigns (campaign_name, description, created_by)
      VALUES (?n, ?d, ?u)
      RETURNING campaign_id
    ", list(n = campaign_name, d = description %||% NA_character_, u = created_by))
    as.integer(row$campaign_id[1])
  }, error = function(e) { cat('[db] create_campaign error:', e$message, '\n'); NULL })
}

db_set_campaign_active <- function(pool, campaign_id, is_active) {
  .db_execute(pool, "UPDATE campaigns SET is_active = ?a WHERE campaign_id = ?c",
              list(a = is_active, c = as.integer(campaign_id)))
}


# =============================================================================
# SECTION 3: JSON helpers  (unchanged from mod_db.R)
# =============================================================================

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

.from_json_vec_db <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(jsonlite::fromJSON(x, simplifyVector = TRUE), error = function(e) NULL)
}

.from_json_df_db <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(
    as.data.frame(jsonlite::fromJSON(x, simplifyVector = TRUE, simplifyDataFrame = TRUE),
                  stringsAsFactors = FALSE),
    error = function(e) NULL
  )
}


# =============================================================================
# SECTION 4: Row (de)serialisers
# =============================================================================

# Metadata-only column list (no blobs) — for pickers/history tables
.mv_meta_cols <- paste(
  "version_id, owner_username, campaign_id, district_name, version_number,",
  "branched_from_id, is_shared, shared_at, created_at, last_updated_at,",
  "archived_at, facilities_locked_at, boundary_locked_at,",
  "has_landmarks, has_facilities, has_idp, has_health_areas, has_team_areas,",
  "submitted_by"
)

# Full column list (includes blobs) — for edit/restore/branch
.mv_full_cols <- paste(
  .mv_meta_cols, ",",
  "odk_sf, app_sf, district_boundary_sf,",
  "saved_dfa_sf, dfa_names, current_assignments,",
  "saved_team_sf, team_names, current_team_assignments,",
  "smoothed_dfa_sf, smoothed_team_sf, smoothing_generated_at,",
  "landmarks, idp_settlements"
)

.parse_mv_row <- function(r) {
  list(
    version_id           = as.integer(r$version_id),
    owner_username        = r$owner_username,
    campaign_id            = as.integer(r$campaign_id),
    district_name           = r$district_name,
    version_number            = as.integer(r$version_number),
    branched_from_id            = if (is.na(r$branched_from_id)) NULL else as.integer(r$branched_from_id),
    is_shared                     = isTRUE(r$is_shared),
    shared_at                      = r$shared_at,
    created_at                      = r$created_at,
    last_updated_at                   = r$last_updated_at,
    archived_at                        = r$archived_at,
    facilities_locked_at                 = r$facilities_locked_at,
    boundary_locked_at                     = r$boundary_locked_at,
    has_landmarks                            = isTRUE(r$has_landmarks),
    has_facilities                             = isTRUE(r$has_facilities),
    has_idp                                      = isTRUE(r$has_idp),
    has_health_areas                               = isTRUE(r$has_health_areas),
    has_team_areas                                   = isTRUE(r$has_team_areas),
    submitted_by                                       = r$submitted_by,
    snap = list(
      odk_sf                = .from_json_sf_db(r$odk_sf %||% NA_character_),
      app_sf                 = .from_json_sf_db(r$app_sf %||% NA_character_),
      district_boundary_sf     = .from_json_sf_db(r$district_boundary_sf %||% NA_character_),
      saved_dfa_sf                = .from_json_sf_db(r$saved_dfa_sf %||% NA_character_),
      dfa_names                     = .from_json_vec_db(r$dfa_names %||% NA_character_),
      current_assignments             = .from_json_vec_db(r$current_assignments %||% NA_character_),
      saved_team_sf                     = .from_json_sf_db(r$saved_team_sf %||% NA_character_),
      team_names                          = .from_json_vec_db(r$team_names %||% NA_character_),
      current_team_assignments              = .from_json_vec_db(r$current_team_assignments %||% NA_character_),
      smoothed_dfa_sf                         = .from_json_sf_db(r$smoothed_dfa_sf %||% NA_character_),
      smoothed_team_sf                          = .from_json_sf_db(r$smoothed_team_sf %||% NA_character_),
      landmarks                                   = .from_json_df_db(r$landmarks %||% NA_character_),
      idp_settlements                               = .from_json_df_db(r$idp_settlements %||% NA_character_)
    )
  )
}


# =============================================================================
# SECTION 5: Fetch versions
# =============================================================================

#' The current user's own unpublished-or-published draft for a district+campaign,
#' if one exists. A user has at most one row per (owner, campaign, district)
#' that they are actively editing — the most recently updated, non-archived one.
db_get_active_draft <- function(pool, owner_username, campaign_id, district_name) {
  rows <- tryCatch(
    .db_query(pool, sprintf("
      SELECT %s FROM mapping_versions
      WHERE owner_username = ?u AND campaign_id = ?c AND district_name = ?d
        AND archived_at IS NULL
      ORDER BY last_updated_at DESC
      LIMIT 1
    ", .mv_full_cols), list(u = owner_username, c = as.integer(campaign_id), d = district_name)),
    error = function(e) { cat('[db] get_active_draft error:', e$message, '\n'); NULL }
  )
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  .parse_mv_row(rows[1, ])
}

#' The current shared (published) version for a district+campaign, if any.
#' At most one such row can exist at a time — enforced by db_publish_version().
db_get_shared_version <- function(pool, campaign_id, district_name) {
  rows <- tryCatch(
    .db_query(pool, sprintf("
      SELECT %s FROM mapping_versions
      WHERE campaign_id = ?c AND district_name = ?d
        AND is_shared = TRUE AND archived_at IS NULL
      LIMIT 1
    ", .mv_full_cols), list(c = as.integer(campaign_id), d = district_name)),
    error = function(e) { cat('[db] get_shared_version error:', e$message, '\n'); NULL }
  )
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  .parse_mv_row(rows[1, ])
}

#' All non-blob rows a user owns for a district (across campaigns) — for the
#' "continue my draft" picker. Metadata only.
db_get_owner_versions <- function(pool, owner_username, district_name = NULL) {
  if (is.null(district_name)) {
    .db_query(pool, sprintf("
      SELECT %s FROM mapping_versions
      WHERE owner_username = ?u AND archived_at IS NULL
      ORDER BY last_updated_at DESC
    ", .mv_meta_cols), list(u = owner_username))
  } else {
    .db_query(pool, sprintf("
      SELECT %s FROM mapping_versions
      WHERE owner_username = ?u AND district_name = ?d AND archived_at IS NULL
      ORDER BY version_number DESC
    ", .mv_meta_cols), list(u = owner_username, d = district_name))
  }
}

#' All shared/published versions across all users for a district — the
#' "branch from a shared version" picker source. campaign_id filter optional
#' (omit to support "carry forward from a prior campaign").
db_get_shareable_versions <- function(pool, district_name, campaign_id = NULL) {
  if (is.null(campaign_id)) {
    .db_query(pool, sprintf("
      SELECT %s FROM mapping_versions
      WHERE district_name = ?d AND is_shared = TRUE AND archived_at IS NULL
      ORDER BY shared_at DESC
    ", .mv_meta_cols), list(d = district_name))
  } else {
    .db_query(pool, sprintf("
      SELECT %s FROM mapping_versions
      WHERE district_name = ?d AND campaign_id = ?c
        AND is_shared = TRUE AND archived_at IS NULL
      ORDER BY shared_at DESC
    ", .mv_meta_cols), list(d = district_name, c = as.integer(campaign_id)))
  }
}

#' Full version-history table for a district+campaign (admin review). No blobs.
db_get_version_history <- function(pool, district_name, campaign_id = NULL) {
  if (is.null(campaign_id)) {
    .db_query(pool, sprintf("
      SELECT %s FROM mapping_versions
      WHERE district_name = ?d
      ORDER BY created_at DESC
    ", .mv_meta_cols), list(d = district_name))
  } else {
    .db_query(pool, sprintf("
      SELECT %s FROM mapping_versions
      WHERE district_name = ?d AND campaign_id = ?c
      ORDER BY created_at DESC
    ", .mv_meta_cols), list(d = district_name, c = as.integer(campaign_id)))
  }
}

#' A single version by id, full data (admin restore / branch source lookup).
db_get_version_by_id <- function(pool, version_id) {
  rows <- tryCatch(
    .db_query(pool, sprintf("
      SELECT %s FROM mapping_versions WHERE version_id = ?v
    ", .mv_full_cols), list(v = as.integer(version_id))),
    error = function(e) { cat('[db] get_version_by_id error:', e$message, '\n'); NULL }
  )
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  .parse_mv_row(rows[1, ])
}


# =============================================================================
# SECTION 6: Create / branch / carry-forward
# =============================================================================

.next_version_number <- function(conn, owner_username, campaign_id, district_name) {
  res <- DBI::dbGetQuery(conn, "
    SELECT COALESCE(MAX(version_number), 0) + 1 AS nv FROM mapping_versions
    WHERE owner_username = $1 AND campaign_id = $2 AND district_name = $3
  ", list(owner_username, as.integer(campaign_id), district_name))
  as.integer(res$nv[1])
}

#' Start a brand-new, empty draft (no branched_from_id) — "start blank".
db_create_blank_version <- function(pool, owner_username, campaign_id, district_name) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  tryCatch({
    ver <- .next_version_number(conn, owner_username, campaign_id, district_name)
    row <- DBI::dbGetQuery(conn, "
      INSERT INTO mapping_versions
        (owner_username, campaign_id, district_name, version_number, submitted_by)
      VALUES ($1, $2, $3, $4, $1)
      RETURNING version_id
    ", list(owner_username, as.integer(campaign_id), district_name, ver))
    as.integer(row$version_id[1])
  }, error = function(e) { cat('[db] create_blank_version error:', e$message, '\n'); NULL })
}

#' Branch from a shared version (same mechanism for "carry forward from a
#' prior campaign" — source_version_id's campaign_id may differ from
#' target campaign_id). Copies all working/snapshot geometry into a new
#' row owned by owner_username. The source row is never touched.
db_branch_version <- function(pool, source_version_id, owner_username,
                              target_campaign_id, district_name) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  tryCatch({
    ver <- .next_version_number(conn, owner_username, target_campaign_id, district_name)
    row <- DBI::dbGetQuery(conn, "
      INSERT INTO mapping_versions (
        owner_username, campaign_id, district_name, version_number, branched_from_id,
        submitted_by,
        facilities_locked_at, odk_sf, app_sf,
        boundary_locked_at, district_boundary_sf,
        saved_dfa_sf, dfa_names, current_assignments,
        saved_team_sf, team_names, current_team_assignments,
        smoothed_dfa_sf, smoothed_team_sf, smoothing_generated_at,
        landmarks, idp_settlements,
        has_landmarks, has_facilities, has_idp, has_health_areas, has_team_areas
      )
      SELECT
        $2, $3, $4, $5, version_id,
        $2,
        facilities_locked_at, odk_sf, app_sf,
        boundary_locked_at, district_boundary_sf,
        saved_dfa_sf, dfa_names, current_assignments,
        saved_team_sf, team_names, current_team_assignments,
        smoothed_dfa_sf, smoothed_team_sf, smoothing_generated_at,
        landmarks, idp_settlements,
        has_landmarks, has_facilities, has_idp, has_health_areas, has_team_areas
      FROM mapping_versions
      WHERE version_id = $1
      RETURNING version_id
    ", list(as.integer(source_version_id), owner_username, as.integer(target_campaign_id),
             district_name, ver))
    if (nrow(row) == 0) stop('source version not found')
    as.integer(row$version_id[1])
  }, error = function(e) { cat('[db] branch_version error:', e$message, '\n'); NULL })
}


# =============================================================================
# SECTION 7: Submit stage  (upsert-in-place for unpublished drafts;
#                           forks a new version if the target is shared)
# =============================================================================

#' Write one stage of progress. If version_id points at a row the caller owns
#' and that is NOT shared, updates it in place (matches old submit_stage
#' upsert behavior). If the row IS shared, forks a new version first (per
#' "editing after publish always creates a new version") and writes to that
#' instead — the new version_id is returned so the caller can update its
#' session state.
#'
#' stage = "landmarks" | "facilities" | "idp" | "areas" | "team_areas"
#' (kept as "areas" — not "health_areas" — to match the stage string
#' mod_health_area_tab.R already sends; only the DB columns are named
#' has_health_areas/saved_dfa_sf etc.)
db_submit_stage_v2 <- function(pool, version_id, username, stage, data) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  cur <- DBI::dbGetQuery(conn,
    "SELECT version_id, owner_username, campaign_id, district_name, is_shared
     FROM mapping_versions WHERE version_id = $1",
    list(as.integer(version_id)))
  if (nrow(cur) == 0) stop('version not found')

  target_id <- as.integer(cur$version_id[1])

  # Editing a shared version forks a new one first
  if (isTRUE(cur$is_shared[1])) {
    target_id <- db_branch_version(
      pool, source_version_id = target_id, owner_username = cur$owner_username[1],
      target_campaign_id = cur$campaign_id[1], district_name = cur$district_name[1]
    )
    if (is.null(target_id)) stop('failed to fork version for edit')
  }

  landmarks_json       <- if (stage == "landmarks")     .to_json_for_db(data$landmarks)             else NA_character_
  odk_sf_json          <- if (stage == "facilities")    .to_json_for_db(data$odk_sf)                else NA_character_
  app_sf_json          <- if (stage == "facilities")    .to_json_for_db(data$app_sf)                else NA_character_
  idp_json             <- if (stage == "idp")           .to_json_for_db(data$idp_settlements)       else NA_character_
  saved_dfa_sf_json    <- if (stage == "areas")  .to_json_for_db(data$saved_dfa_sf)          else NA_character_
  dfa_names_json       <- if (stage == "areas")  .to_json_for_db(data$dfa_names)             else NA_character_
  cur_assign_json      <- if (stage == "areas")  .to_json_for_db(data$current_assignments)   else NA_character_
  smoothed_dfa_json    <- if (stage == "areas")  .to_json_for_db(data$smoothed_dfa_sf)        else NA_character_
  saved_team_sf_json   <- if (stage == "team_areas")    .to_json_for_db(data$saved_team_sf)         else NA_character_
  team_names_json      <- if (stage == "team_areas")    .to_json_for_db(data$team_names)            else NA_character_
  cur_team_assign_json <- if (stage == "team_areas")    .to_json_for_db(data$current_team_assignments) else NA_character_
  smoothed_team_json   <- if (stage == "team_areas")    .to_json_for_db(data$smoothed_team_sf)       else NA_character_

  has_landmarks    <- stage == "landmarks"
  has_facilities   <- stage == "facilities"
  has_idp          <- stage == "idp"
  has_health_areas <- stage == "areas"
  has_team_areas   <- stage == "team_areas"

  smoothing_stamp <- stage %in% c("areas", "team_areas")

  tryCatch({
    DBI::dbExecute(conn, sprintf("
      UPDATE mapping_versions SET
        submitted_by              = $2,
        last_updated_at            = NOW(),
        landmarks                   = COALESCE($3,  landmarks),
        odk_sf                       = COALESCE($4,  odk_sf),
        app_sf                        = COALESCE($5,  app_sf),
        idp_settlements                 = COALESCE($6,  idp_settlements),
        saved_dfa_sf                     = COALESCE($7,  saved_dfa_sf),
        dfa_names                          = COALESCE($8,  dfa_names),
        current_assignments                  = COALESCE($9,  current_assignments),
        smoothed_dfa_sf                        = COALESCE($10, smoothed_dfa_sf),
        saved_team_sf                            = COALESCE($11, saved_team_sf),
        team_names                                 = COALESCE($12, team_names),
        current_team_assignments                     = COALESCE($13, current_team_assignments),
        smoothed_team_sf                               = COALESCE($14, smoothed_team_sf),
        %s
        has_landmarks    = has_landmarks    OR $15,
        has_facilities   = has_facilities   OR $16,
        has_idp          = has_idp          OR $17,
        has_health_areas = has_health_areas OR $18,
        has_team_areas   = has_team_areas   OR $19,
        facilities_locked_at = CASE WHEN $16 AND facilities_locked_at IS NULL THEN NOW() ELSE facilities_locked_at END,
        boundary_locked_at   = CASE WHEN $16 AND boundary_locked_at   IS NULL THEN NOW() ELSE boundary_locked_at   END
      WHERE version_id = $1
    ", if (smoothing_stamp) "smoothing_generated_at = NOW()," else ""),
      list(
        target_id, username,
        landmarks_json, odk_sf_json, app_sf_json, idp_json,
        saved_dfa_sf_json, dfa_names_json, cur_assign_json, smoothed_dfa_json,
        saved_team_sf_json, team_names_json, cur_team_assign_json, smoothed_team_json,
        has_landmarks, has_facilities, has_idp, has_health_areas, has_team_areas
      )
    )
  }, error = function(e) {
    cat('[db] submit_stage_v2 UPDATE error (', stage, '):', e$message, '\n')
    stop(e)
  })

  invisible(target_id)
}

#' Explicit "Refresh from source" — updates the locked facility/boundary
#' snapshot fields and re-stamps the lock timestamps. Never called
#' automatically; the UI must warn the user first since this can invalidate
#' already-drawn boundaries.
db_refresh_snapshot <- function(pool, version_id, odk_sf = NULL, app_sf = NULL,
                                district_boundary_sf = NULL) {
  .db_execute(pool, "
    UPDATE mapping_versions SET
      odk_sf                = COALESCE(?o, odk_sf),
      app_sf                 = COALESCE(?a, app_sf),
      district_boundary_sf     = COALESCE(?b, district_boundary_sf),
      facilities_locked_at       = CASE WHEN ?o IS NOT NULL OR ?a IS NOT NULL THEN NOW() ELSE facilities_locked_at END,
      boundary_locked_at           = CASE WHEN ?b IS NOT NULL THEN NOW() ELSE boundary_locked_at END,
      last_updated_at                = NOW()
    WHERE version_id = ?v
  ", list(
    o = .to_json_for_db(odk_sf), a = .to_json_for_db(app_sf),
    b = .to_json_for_db(district_boundary_sf), v = as.integer(version_id)
  ))
}


# =============================================================================
# SECTION 8: Publish  (transaction — unshare previous, share new)
# =============================================================================

#' Publish a version as the current shared version for its (district,
#' campaign). Atomically unshares whatever was previously shared for that
#' same pair. Never mutates the version being published beyond its share
#' flag/timestamp.
db_publish_version <- function(pool, version_id) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch({
    DBI::dbBegin(conn)

    target <- DBI::dbGetQuery(conn,
      "SELECT version_id, campaign_id, district_name FROM mapping_versions WHERE version_id = $1",
      list(as.integer(version_id)))
    if (nrow(target) == 0) stop('version not found')

    # Unshare whatever is currently shared for this district+campaign
    DBI::dbExecute(conn, "
      UPDATE mapping_versions
      SET is_shared = FALSE
      WHERE district_name = $1 AND campaign_id = $2
        AND is_shared = TRUE AND version_id != $3
    ", list(target$district_name[1], target$campaign_id[1], as.integer(version_id)))

    # Share the target
    DBI::dbExecute(conn, "
      UPDATE mapping_versions
      SET is_shared = TRUE, shared_at = NOW(), last_updated_at = NOW()
      WHERE version_id = $1
    ", list(as.integer(version_id)))

    DBI::dbCommit(conn)
    invisible(TRUE)
  }, error = function(e) {
    tryCatch(DBI::dbRollback(conn), error = function(e2) NULL)
    cat('[db] publish_version error:', e$message, '\n')
    stop(e)
  })
}


# =============================================================================
# SECTION 9: Archive / delete / unshare  (admin)
# =============================================================================

db_archive_version <- function(pool, version_id) {
  .db_execute(pool, "UPDATE mapping_versions SET archived_at = NOW() WHERE version_id = ?v",
              list(v = as.integer(version_id)))
}

db_delete_version <- function(pool, version_id) {
  .db_execute(pool, "DELETE FROM mapping_versions WHERE version_id = ?v",
              list(v = as.integer(version_id)))
}

#' Unshare a version WITHOUT publishing a replacement — leaves the
#' district+campaign with no current shared version at all. Distinct from
#' db_publish_version, which always shares something new. This is the
#' admin action for "this shouldn't be the official version anymore" when
#' there isn't yet a better one to put in its place. The version itself
#' (and its owner's access to it) is untouched — only is_shared changes.
db_unshare_version <- function(pool, version_id) {
  .db_execute(pool, "
    UPDATE mapping_versions SET is_shared = FALSE, last_updated_at = NOW()
    WHERE version_id = ?v
  ", list(v = as.integer(version_id)))
}


# =============================================================================
# SECTION 10: Admin progress table  (all shared versions, one row per district)
# =============================================================================

db_get_campaign_progress <- function(pool, campaign_id) {
  tryCatch(
    .db_query(pool, sprintf("
      SELECT %s FROM mapping_versions
      WHERE campaign_id = ?c AND is_shared = TRUE AND archived_at IS NULL
      ORDER BY district_name
    ", .mv_meta_cols), list(c = as.integer(campaign_id))),
    error = function(e) { cat('[db] get_campaign_progress error:', e$message, '\n'); NULL }
  )
}


# =============================================================================
# SECTION 11: Generation settings  (global default + optional per-campaign override)
# =============================================================================

#' Effective value for a setting: per-campaign override if present, else
#' global default. Returns NA if neither exists.
db_get_generation_setting <- function(pool, setting_key, campaign_id = NULL) {
  if (!is.null(campaign_id)) {
    row <- .db_query(pool, "
      SELECT setting_value FROM generation_settings
      WHERE setting_key = ?k AND campaign_id = ?c
    ", list(k = setting_key, c = as.integer(campaign_id)))
    if (nrow(row) > 0) return(as.numeric(row$setting_value[1]))
  }
  row <- .db_query(pool, "
    SELECT setting_value FROM generation_settings
    WHERE setting_key = ?k AND campaign_id IS NULL
  ", list(k = setting_key))
  if (nrow(row) == 0) return(NA_real_)
  as.numeric(row$setting_value[1])
}

db_get_all_generation_settings <- function(pool, campaign_id = NULL) {
  .db_query(pool, "
    SELECT setting_key, campaign_id, setting_value, description, updated_by, updated_at
    FROM generation_settings
    ORDER BY setting_key, campaign_id NULLS FIRST
  ")
}

db_set_generation_setting <- function(pool, setting_key, setting_value, updated_by,
                                      campaign_id = NULL, description = NULL) {
  if (is.null(campaign_id)) {
    .db_execute(pool, "
      INSERT INTO generation_settings (setting_key, campaign_id, setting_value, description, updated_by, updated_at)
      VALUES (?k, NULL, ?v, ?d, ?u, NOW())
      ON CONFLICT (setting_key) WHERE campaign_id IS NULL DO UPDATE SET
        setting_value = EXCLUDED.setting_value,
        description   = COALESCE(EXCLUDED.description, generation_settings.description),
        updated_by    = EXCLUDED.updated_by,
        updated_at    = NOW()
    ", list(k = setting_key, v = as.numeric(setting_value),
            d = description %||% NA_character_, u = updated_by))
  } else {
    .db_execute(pool, "
      INSERT INTO generation_settings (setting_key, campaign_id, setting_value, description, updated_by, updated_at)
      VALUES (?k, ?c, ?v, ?d, ?u, NOW())
      ON CONFLICT (setting_key, campaign_id) WHERE campaign_id IS NOT NULL DO UPDATE SET
        setting_value = EXCLUDED.setting_value,
        description   = COALESCE(EXCLUDED.description, generation_settings.description),
        updated_by    = EXCLUDED.updated_by,
        updated_at    = NOW()
    ", list(k = setting_key, c = as.integer(campaign_id), v = as.numeric(setting_value),
            d = description %||% NA_character_, u = updated_by))
  }
}


# =============================================================================
# SECTION 12: Admin-configurable data source URLs (subdivisions, IDP only —
#             health-facility ODK/Kobo endpoints stay hardcoded per config)
# =============================================================================

db_get_data_source_url <- function(pool, setting_key) {
  row <- .db_query(pool, "SELECT setting_value FROM data_source_settings WHERE setting_key = ?k",
                   list(k = setting_key))
  if (nrow(row) == 0) return(NULL)
  row$setting_value[1]
}

db_set_data_source_url <- function(pool, setting_key, url, updated_by) {
  .db_execute(pool, "
    INSERT INTO data_source_settings (setting_key, setting_value, updated_by, updated_at)
    VALUES (?k, ?v, ?u, NOW())
    ON CONFLICT (setting_key) DO UPDATE SET
      setting_value = EXCLUDED.setting_value,
      updated_by    = EXCLUDED.updated_by,
      updated_at    = NOW()
  ", list(k = setting_key, v = url, u = updated_by))
}
