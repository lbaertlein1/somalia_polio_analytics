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
#
#   - TEAM AREAS ARE A SEPARATE TABLE (team_area_versions), SECTION 13+
#     below — not columns on mapping_versions. Independently versioned and
#     published per (owner, campaign, district, health_area_name), pinned
#     to a specific health-area version via based_on_health_area_version_id.
#     Team areas can ONLY ever be drawn against a district's CURRENT
#     health-area version — no branch/carry-forward choice at that level,
#     unlike mapping_versions. See db_get_active_team_draft() /
#     db_create_team_area_draft().
#   - LOCKING: once any health area in a district has a current
#     (is_shared=TRUE) team-area version, db_publish_version() refuses
#     UNCONDITIONALLY for the WHOLE district — no admin override. To
#     publish a different health-area version, the current team-area
#     version(s) must first be unshared (db_unshare_team_area_version()),
#     an explicit admin action, not an automatic swap. See
#     db_district_has_locked_team_areas().
#   - STALENESS: a team-area version is stale when its
#     based_on_health_area_version_id no longer matches the district's
#     current shared mapping_versions.version_id. Checked uniformly for
#     published AND unpublished/draft rows — see
#     db_check_team_area_staleness() — both at draft-open time (caller's
#     responsibility) and again, authoritatively, inside
#     db_publish_team_area() itself. There is no reconciliation: a stale
#     version simply can't be published as current until its pinned
#     health-area version is made current again.
#   - CAMPAIGN_DISTRICTS: campaigns no longer implicitly apply to every
#     district — see SECTION 14.
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
  "has_landmarks, has_facilities, has_idp, has_health_areas,",
  "submitted_by"
)

# Full column list (includes blobs) — for edit/restore/branch
.mv_full_cols <- paste(
  .mv_meta_cols, ",",
  "odk_sf, app_sf, district_boundary_sf,",
  "saved_dfa_sf, dfa_names, current_assignments, team_targets,",
  "smoothed_dfa_sf, smoothing_generated_at,",
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
    submitted_by                                       = r$submitted_by,
    snap = list(
      odk_sf                = .from_json_sf_db(r$odk_sf %||% NA_character_),
      app_sf                 = .from_json_sf_db(r$app_sf %||% NA_character_),
      district_boundary_sf     = .from_json_sf_db(r$district_boundary_sf %||% NA_character_),
      saved_dfa_sf                = .from_json_sf_db(r$saved_dfa_sf %||% NA_character_),
      dfa_names                     = .from_json_vec_db(r$dfa_names %||% NA_character_),
      current_assignments             = .from_json_vec_db(r$current_assignments %||% NA_character_),
      team_targets                       = .from_json_db(r$team_targets %||% NA_character_),
      smoothed_dfa_sf                    = .from_json_sf_db(r$smoothed_dfa_sf %||% NA_character_),
      landmarks                            = .from_json_df_db(r$landmarks %||% NA_character_),
      idp_settlements                        = .from_json_df_db(r$idp_settlements %||% NA_character_)
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
#'
#' HEALTH-AREA-ONLY. Team-area versions have their own, separate branch
#' function (db_branch_team_area_version(), SECTION 13) — branching a
#' health-area version never copies team-area data, since none lives here
#' anymore, and a health-area branch says nothing about what should happen
#' to any team-area versions pinned to the ORIGINAL version (that's the
#' locking/staleness mechanism, not something a branch handles).
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
        saved_dfa_sf, dfa_names, current_assignments, team_targets,
        smoothed_dfa_sf, smoothing_generated_at,
        landmarks, idp_settlements,
        has_landmarks, has_facilities, has_idp, has_health_areas
      )
      SELECT
        $2, $3, $4, $5, version_id,
        $2,
        facilities_locked_at, odk_sf, app_sf,
        boundary_locked_at, district_boundary_sf,
        saved_dfa_sf, dfa_names, current_assignments, team_targets,
        smoothed_dfa_sf, smoothing_generated_at,
        landmarks, idp_settlements,
        has_landmarks, has_facilities, has_idp, has_health_areas
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
#' stage = "landmarks" | "facilities" | "idp" | "areas"
#' (kept as "areas" — not "health_areas" — to match the stage string
#' mod_health_area_tab.R already sends; only the DB columns are named
#' has_health_areas/saved_dfa_sf etc.)
#'
#' HEALTH-AREA-ONLY (and the landmarks/facilities/idp stages that live on
#' the same row). Team-area submission is a separate function,
#' db_submit_team_area_stage() (SECTION 13) — it was never a "stage" of a
#' mapping_versions row, so it doesn't belong in this stage %in% c(...)
#' dispatch even though the old schema stored it there.
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
  team_targets_json    <- if (stage == "areas")  .to_json_for_db(data$team_targets)          else NA_character_
  smoothed_dfa_json    <- if (stage == "areas")  .to_json_for_db(data$smoothed_dfa_sf)        else NA_character_

  has_landmarks    <- stage == "landmarks"
  has_facilities   <- stage == "facilities"
  has_idp          <- stage == "idp"
  has_health_areas <- stage == "areas"

  smoothing_stamp <- stage == "areas"

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
        team_targets                           = COALESCE($10, team_targets),
        smoothed_dfa_sf                          = COALESCE($11, smoothed_dfa_sf),
        %s
        has_landmarks    = has_landmarks    OR $12,
        has_facilities   = has_facilities   OR $13,
        has_idp          = has_idp          OR $14,
        has_health_areas = has_health_areas OR $15,
        facilities_locked_at = CASE WHEN $13 AND facilities_locked_at IS NULL THEN NOW() ELSE facilities_locked_at END,
        boundary_locked_at   = CASE WHEN $13 AND boundary_locked_at   IS NULL THEN NOW() ELSE boundary_locked_at   END
      WHERE version_id = $1
    ", if (smoothing_stamp) "smoothing_generated_at = NOW()," else ""),
      list(
        target_id, username,
        landmarks_json, odk_sf_json, app_sf_json, idp_json,
        saved_dfa_sf_json, dfa_names_json, cur_assign_json, team_targets_json, smoothed_dfa_json,
        has_landmarks, has_facilities, has_idp, has_health_areas
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
#'
#' Refuses UNCONDITIONALLY when the district is locked (see
#' db_district_has_locked_team_areas()) — no admin override. Publishing a
#' different health-area version while any team-area version is current
#' would leave that team-area work pinned to a boundary nobody can see or
#' reach anymore. To publish a different health-area version, the current
#' team-area version(s) must first be unshared (an explicit admin action,
#' not something this function does automatically). The lock check runs
#' on the SAME connection already checked out for this transaction (not
#' via db_district_has_locked_team_areas(), which checks out its own) —
#' avoids holding two pool connections for one publish call.
#'
#' actor_role is accepted but no longer changes this function's
#' behavior — kept only so existing callers that still pass it don't need
#' to change.
db_publish_version <- function(pool, version_id, actor_role = 'user') {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch({
    DBI::dbBegin(conn)

    target <- DBI::dbGetQuery(conn,
      "SELECT version_id, campaign_id, district_name FROM mapping_versions WHERE version_id = $1",
      list(as.integer(version_id)))
    if (nrow(target) == 0) stop('version not found')

    locked <- DBI::dbGetQuery(conn, "
      SELECT 1 FROM team_area_versions
      WHERE campaign_id = $1 AND district_name = $2
        AND is_shared = TRUE AND archived_at IS NULL
      LIMIT 1
    ", list(target$campaign_id[1], target$district_name[1]))
    if (nrow(locked) > 0) {
      stop(paste0(
        'This district is locked: at least one health area has a published team-area map. ',
        'Unshare the current team-area version(s) first (District review in the Admin panel) ',
        'before publishing a different health-area version.'
      ))
    }

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

#' Whether ANY health area in this (district, campaign) has a current
#' (is_shared=TRUE, not archived) team-area version — the trigger for
#' locking the health-area track against regular-user publishing. Standalone,
#' pool-level version (its own connection checkout) for callers outside an
#' open transaction — e.g. the submit-flow UI checking whether to even show
#' a "make current" option, before the user has clicked anything.
#' db_publish_version() does its own equivalent check inline instead of
#' calling this, to avoid holding two pool connections for one publish call.
#'
#' Deliberately district-wide, not per-health-area: promoting a different
#' health-area version could invalidate team-area work downstream even for
#' health areas whose own shape didn't change, so every health area in the
#' district locks together, not just the one(s) with current team areas.
db_district_has_locked_team_areas <- function(pool, campaign_id, district_name) {
  row <- tryCatch(
    .db_query(pool, "
      SELECT 1 AS x FROM team_area_versions
      WHERE campaign_id = ?c AND district_name = ?d
        AND is_shared = TRUE AND archived_at IS NULL
      LIMIT 1
    ", list(c = as.integer(campaign_id), d = district_name)),
    error = function(e) { cat('[db] district_has_locked_team_areas error:', e$message, '\n'); NULL }
  )
  !is.null(row) && nrow(row) > 0
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

#' One row per district with a current health-area version for this
#' campaign, plus how many of its health areas ALSO have a current
#' team-area version (team_areas_mapped_count out of length(dfa_names)).
#' The join's subquery columns are deliberately renamed (tav_district_name,
#' not district_name) to avoid ambiguity against mapping_versions' own
#' district_name/campaign_id in the unqualified .mv_meta_cols select list.
db_get_campaign_progress <- function(pool, campaign_id) {
  tryCatch(
    .db_query(pool, sprintf("
      SELECT %s, dfa_names, COALESCE(tav.team_area_count, 0) AS team_areas_mapped_count
      FROM mapping_versions
      LEFT JOIN (
        SELECT district_name AS tav_district_name, campaign_id AS tav_campaign_id,
               COUNT(DISTINCT health_area_name) AS team_area_count
        FROM team_area_versions
        WHERE is_shared = TRUE AND archived_at IS NULL
        GROUP BY district_name, campaign_id
      ) tav ON tav.tav_district_name = mapping_versions.district_name
           AND tav.tav_campaign_id   = mapping_versions.campaign_id
      WHERE mapping_versions.campaign_id = ?c
        AND mapping_versions.is_shared = TRUE AND mapping_versions.archived_at IS NULL
      ORDER BY mapping_versions.district_name
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


# =============================================================================
# SECTION 13: Team area versions
#
# Independently versioned/publishable per (owner, campaign, district,
# health_area_name), pinned to the specific health-area version they were
# drawn against via based_on_health_area_version_id. Mirrors mapping_versions'
# ownership/fork-on-edit/publish pattern closely, with three deliberate
# differences:
#   - Publish scope is (district, campaign, health_area_name), not
#     (district, campaign) -- multiple health areas in the same district can
#     each have their own independently-current team-area version.
#   - No branch/carry-forward choice at draft-creation time -- team areas
#     are only ever drawn against a district's CURRENT health-area version,
#     so "start a draft" always means db_create_team_area_draft(), pinned to
#     whatever's current right now. (Carry-forward DOES still apply at the
#     campaign-district-assignment level -- see db_carry_forward_district_
#     to_campaign() in SECTION 14 -- but that's an admin bulk action copying
#     an already-published pair forward, not a per-user choice at this level.)
#   - db_publish_team_area() does a final, authoritative staleness check
#     before sharing -- publishing a version pinned to a health-area
#     boundary that's no longer current is always refused, published or not.
# =============================================================================

.tav_meta_cols <- paste(
  "team_version_id, owner_username, campaign_id, district_name, health_area_name,",
  "based_on_health_area_version_id, version_number, branched_from_id,",
  "is_shared, shared_at, created_at, last_updated_at, archived_at, submitted_by"
)

.tav_full_cols <- paste(
  .tav_meta_cols, ",",
  "saved_team_sf, team_names, current_team_assignments,",
  "smoothed_team_sf, smoothing_generated_at"
)

.parse_tav_row <- function(r) {
  list(
    team_version_id                 = as.integer(r$team_version_id),
    owner_username                   = r$owner_username,
    campaign_id                       = as.integer(r$campaign_id),
    district_name                      = r$district_name,
    health_area_name                    = r$health_area_name,
    based_on_health_area_version_id       = as.integer(r$based_on_health_area_version_id),
    version_number                          = as.integer(r$version_number),
    branched_from_id                          = if (is.na(r$branched_from_id)) NULL else as.integer(r$branched_from_id),
    is_shared                                     = isTRUE(r$is_shared),
    shared_at                                       = r$shared_at,
    created_at                                        = r$created_at,
    last_updated_at                                     = r$last_updated_at,
    archived_at                                           = r$archived_at,
    submitted_by                                            = r$submitted_by,
    snap = list(
      saved_team_sf                = .from_json_sf_db(r$saved_team_sf %||% NA_character_),
      team_names                     = .from_json_vec_db(r$team_names %||% NA_character_),
      current_team_assignments         = .from_json_vec_db(r$current_team_assignments %||% NA_character_),
      smoothed_team_sf                    = .from_json_sf_db(r$smoothed_team_sf %||% NA_character_)
    )
  )
}

.next_team_version_number <- function(conn, owner_username, campaign_id, district_name, health_area_name) {
  res <- DBI::dbGetQuery(conn, "
    SELECT COALESCE(MAX(version_number), 0) + 1 AS nv FROM team_area_versions
    WHERE owner_username = $1 AND campaign_id = $2 AND district_name = $3 AND health_area_name = $4
  ", list(owner_username, as.integer(campaign_id), district_name, health_area_name))
  as.integer(res$nv[1])
}

#' Whether a team-area version's pinned health-area version is still the
#' district's current shared one. TRUE = stale. Checked uniformly for
#' published AND unpublished/draft rows — an in-progress draft built on a
#' boundary that's no longer current is just as stale as a published one.
#' There is no reconciliation: a stale version simply can't be published
#' as current until its pinned health-area version is made current again.
#' Returns NA if the team-area version itself doesn't exist.
db_check_team_area_staleness <- function(pool, team_version_id) {
  row <- tryCatch(
    .db_query(pool, "
      SELECT tav.based_on_health_area_version_id,
             mv_current.version_id AS current_health_area_version_id
      FROM team_area_versions tav
      LEFT JOIN mapping_versions mv_current
        ON mv_current.district_name = tav.district_name
       AND mv_current.campaign_id   = tav.campaign_id
       AND mv_current.is_shared     = TRUE
       AND mv_current.archived_at  IS NULL
      WHERE tav.team_version_id = ?v
    ", list(v = as.integer(team_version_id))),
    error = function(e) { cat('[db] check_team_area_staleness error:', e$message, '\n'); NULL }
  )
  if (is.null(row) || nrow(row) == 0) return(NA)
  if (is.na(row$current_health_area_version_id[1])) return(TRUE)
  !identical(as.integer(row$based_on_health_area_version_id[1]),
            as.integer(row$current_health_area_version_id[1]))
}

#' Start a brand-new team-area draft for one health area, pinned to
#' based_on_version_id — the district's CURRENT health-area version at the
#' time work begins. Callers must have already confirmed this is current;
#' there is no branch/carry-forward choice at this level, unlike health
#' areas — a team-area draft always starts from whatever's current right now.
db_create_team_area_draft <- function(pool, owner_username, campaign_id, district_name,
                                      health_area_name, based_on_version_id) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  tryCatch({
    ver <- .next_team_version_number(conn, owner_username, campaign_id, district_name, health_area_name)
    row <- DBI::dbGetQuery(conn, "
      INSERT INTO team_area_versions
        (owner_username, campaign_id, district_name, health_area_name,
         based_on_health_area_version_id, version_number, submitted_by)
      VALUES ($1, $2, $3, $4, $5, $6, $1)
      RETURNING team_version_id
    ", list(owner_username, as.integer(campaign_id), district_name, health_area_name,
             as.integer(based_on_version_id), ver))
    as.integer(row$team_version_id[1])
  }, error = function(e) { cat('[db] create_team_area_draft error:', e$message, '\n'); NULL })
}

#' Plain branch/fork of a team-area version — used for the normal "editing
#' a shared version forks a new one" rule. Keeps the SAME
#' based_on_health_area_version_id as the source; a branch never changes
#' what boundary it's pinned to — there is no other way to change that
#' pin, since team-area versions stay permanently tied to the health-area
#' version they were drawn against.
db_branch_team_area_version <- function(pool, source_team_version_id, owner_username) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))
  tryCatch({
    src <- DBI::dbGetQuery(conn, "
      SELECT campaign_id, district_name, health_area_name
      FROM team_area_versions WHERE team_version_id = $1
    ", list(as.integer(source_team_version_id)))
    if (nrow(src) == 0) stop('source team-area version not found')

    ver <- .next_team_version_number(conn, owner_username, src$campaign_id[1],
                                     src$district_name[1], src$health_area_name[1])
    row <- DBI::dbGetQuery(conn, "
      INSERT INTO team_area_versions (
        owner_username, campaign_id, district_name, health_area_name,
        based_on_health_area_version_id, version_number, branched_from_id, submitted_by,
        saved_team_sf, team_names, current_team_assignments,
        smoothed_team_sf, smoothing_generated_at
      )
      SELECT
        $2, campaign_id, district_name, health_area_name,
        based_on_health_area_version_id, $3, team_version_id, $2,
        saved_team_sf, team_names, current_team_assignments,
        smoothed_team_sf, smoothing_generated_at
      FROM team_area_versions
      WHERE team_version_id = $1
      RETURNING team_version_id
    ", list(as.integer(source_team_version_id), owner_username, ver))
    if (nrow(row) == 0) stop('branch failed')
    as.integer(row$team_version_id[1])
  }, error = function(e) { cat('[db] branch_team_area_version error:', e$message, '\n'); NULL })
}

#' Write team-area progress. If team_version_id points at a row the caller
#' owns and that is NOT shared, updates it in place. If the row IS shared,
#' forks a new version first (same "editing after publish creates a new
#' version" rule as db_submit_stage_v2) and writes to that instead — the
#' new team_version_id is returned so the caller can follow it. Unlike
#' db_submit_stage_v2 there's no multi-stage dispatch — team areas have
#' only ever the one kind of progress to write.
db_submit_team_area_stage <- function(pool, team_version_id, username, data) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  cur <- DBI::dbGetQuery(conn,
    "SELECT team_version_id, owner_username, is_shared FROM team_area_versions WHERE team_version_id = $1",
    list(as.integer(team_version_id)))
  if (nrow(cur) == 0) stop('team-area version not found')

  target_id <- as.integer(cur$team_version_id[1])

  if (isTRUE(cur$is_shared[1])) {
    target_id <- db_branch_team_area_version(pool, target_id, cur$owner_username[1])
    if (is.null(target_id)) stop('failed to fork team-area version for edit')
  }

  tryCatch({
    DBI::dbExecute(conn, "
      UPDATE team_area_versions SET
        submitted_by              = $2,
        last_updated_at            = NOW(),
        saved_team_sf                = COALESCE($3, saved_team_sf),
        team_names                     = COALESCE($4, team_names),
        current_team_assignments         = COALESCE($5, current_team_assignments),
        smoothed_team_sf                   = COALESCE($6, smoothed_team_sf),
        smoothing_generated_at               = CASE WHEN $6 IS NOT NULL THEN NOW() ELSE smoothing_generated_at END
      WHERE team_version_id = $1
    ", list(
      target_id, username,
      .to_json_for_db(data$saved_team_sf), .to_json_for_db(data$team_names),
      .to_json_for_db(data$current_team_assignments), .to_json_for_db(data$smoothed_team_sf)
    ))
  }, error = function(e) {
    cat('[db] submit_team_area_stage UPDATE error:', e$message, '\n')
    stop(e)
  })

  invisible(target_id)
}

#' The current user's own active (non-archived) draft for one health area,
#' if one exists.
db_get_active_team_draft <- function(pool, owner_username, campaign_id, district_name, health_area_name) {
  rows <- tryCatch(
    .db_query(pool, sprintf("
      SELECT %s FROM team_area_versions
      WHERE owner_username = ?u AND campaign_id = ?c AND district_name = ?d AND health_area_name = ?h
        AND archived_at IS NULL
      ORDER BY last_updated_at DESC
      LIMIT 1
    ", .tav_full_cols), list(u = owner_username, c = as.integer(campaign_id),
                             d = district_name, h = health_area_name)),
    error = function(e) { cat('[db] get_active_team_draft error:', e$message, '\n'); NULL }
  )
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  .parse_tav_row(rows[1, ])
}

#' The current shared (published) team-area version for one health area, if any.
db_get_current_team_area_version <- function(pool, campaign_id, district_name, health_area_name) {
  rows <- tryCatch(
    .db_query(pool, sprintf("
      SELECT %s FROM team_area_versions
      WHERE campaign_id = ?c AND district_name = ?d AND health_area_name = ?h
        AND is_shared = TRUE AND archived_at IS NULL
      LIMIT 1
    ", .tav_full_cols), list(c = as.integer(campaign_id), d = district_name, h = health_area_name)),
    error = function(e) { cat('[db] get_current_team_area_version error:', e$message, '\n'); NULL }
  )
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  .parse_tav_row(rows[1, ])
}

#' A user's own version history for one health area (metadata only) — for
#' the "your other drafts" dropdown in the team-area picker modal.
db_get_owner_team_versions <- function(pool, owner_username, campaign_id, district_name, health_area_name) {
  .db_query(pool, sprintf("
    SELECT %s FROM team_area_versions
    WHERE owner_username = ?u AND campaign_id = ?c AND district_name = ?d AND health_area_name = ?h
      AND archived_at IS NULL
    ORDER BY version_number DESC
  ", .tav_meta_cols), list(u = owner_username, c = as.integer(campaign_id),
                           d = district_name, h = health_area_name))
}

#' Full team-area version history for a district, optionally scoped to one
#' campaign and/or one health area (either or both NULL = no filter on that
#' dimension — NULL campaign_id means "all campaigns", matching
#' db_get_version_history()'s identical pattern; NULL health_area_name
#' means "every health area" — for the admin district-review section's
#' "team areas grouped by health area" view).
db_get_team_area_version_history <- function(pool, district_name, campaign_id = NULL, health_area_name = NULL) {
  where_clauses <- c("district_name = ?d")
  params <- list(d = district_name)
  if (!is.null(campaign_id)) {
    where_clauses <- c(where_clauses, "campaign_id = ?c")
    params$c <- as.integer(campaign_id)
  }
  if (!is.null(health_area_name)) {
    where_clauses <- c(where_clauses, "health_area_name = ?h")
    params$h <- health_area_name
  }
  .db_query(pool, sprintf("
    SELECT %s FROM team_area_versions
    WHERE %s
    ORDER BY health_area_name, created_at DESC
  ", .tav_meta_cols, paste(where_clauses, collapse = " AND ")), params)
}

#' A single team-area version by id, full data.
db_get_team_area_version_by_id <- function(pool, team_version_id) {
  rows <- tryCatch(
    .db_query(pool, sprintf("
      SELECT %s FROM team_area_versions WHERE team_version_id = ?v
    ", .tav_full_cols), list(v = as.integer(team_version_id))),
    error = function(e) { cat('[db] get_team_area_version_by_id error:', e$message, '\n'); NULL }
  )
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  .parse_tav_row(rows[1, ])
}

#' Publish a team-area version as the current shared version for its
#' (district, campaign, health_area_name). Same unshare-then-share
#' transaction pattern as db_publish_version(). Also does a FINAL,
#' authoritative staleness check right before sharing — refuses if the
#' pinned health-area version is no longer current, even if the caller
#' already checked this at draft-open time (a long editing session could
#' see the health-area version change underneath it while the user was
#' working). This IS the admin "restore" mechanism too — restoring an old
#' version is just publishing it; if it's stale, it's refused exactly
#' like any other stale publish attempt. There is no reconciliation —
#' making the pinned health-area version current again is the only way
#' to un-stale it. No role-gating here — unlike health-area publish,
#' team-area publish never invalidates anything else downstream.
db_publish_team_area <- function(pool, team_version_id) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch({
    DBI::dbBegin(conn)

    target <- DBI::dbGetQuery(conn, "
      SELECT team_version_id, campaign_id, district_name, health_area_name,
             based_on_health_area_version_id
      FROM team_area_versions WHERE team_version_id = $1
    ", list(as.integer(team_version_id)))
    if (nrow(target) == 0) stop('team-area version not found')

    current_ha <- DBI::dbGetQuery(conn, "
      SELECT version_id FROM mapping_versions
      WHERE district_name = $1 AND campaign_id = $2 AND is_shared = TRUE AND archived_at IS NULL
    ", list(target$district_name[1], target$campaign_id[1]))

    if (nrow(current_ha) == 0 ||
        !identical(as.integer(current_ha$version_id[1]), as.integer(target$based_on_health_area_version_id[1]))) {
      stop(paste0(
        'This team-area version is pinned to a health-area boundary that is no ',
        'longer current. Make that health-area version current again before publishing.'
      ))
    }

    DBI::dbExecute(conn, "
      UPDATE team_area_versions
      SET is_shared = FALSE
      WHERE district_name = $1 AND campaign_id = $2 AND health_area_name = $3
        AND is_shared = TRUE AND team_version_id != $4
    ", list(target$district_name[1], target$campaign_id[1], target$health_area_name[1],
             as.integer(team_version_id)))

    DBI::dbExecute(conn, "
      UPDATE team_area_versions
      SET is_shared = TRUE, shared_at = NOW(), last_updated_at = NOW()
      WHERE team_version_id = $1
    ", list(as.integer(team_version_id)))

    DBI::dbCommit(conn)
    invisible(TRUE)
  }, error = function(e) {
    tryCatch(DBI::dbRollback(conn), error = function(e2) NULL)
    cat('[db] publish_team_area error:', e$message, '\n')
    stop(e)
  })
}

db_unshare_team_area_version <- function(pool, team_version_id) {
  .db_execute(pool, "
    UPDATE team_area_versions SET is_shared = FALSE, last_updated_at = NOW()
    WHERE team_version_id = ?v
  ", list(v = as.integer(team_version_id)))
}

db_archive_team_area_version <- function(pool, team_version_id) {
  .db_execute(pool, "UPDATE team_area_versions SET archived_at = NOW() WHERE team_version_id = ?v",
              list(v = as.integer(team_version_id)))
}

db_delete_team_area_version <- function(pool, team_version_id) {
  .db_execute(pool, "DELETE FROM team_area_versions WHERE team_version_id = ?v",
              list(v = as.integer(team_version_id)))
}

#' Per-health-area team-area status for one district+campaign — one row per
#' health area name (from the current health-area version's dfa_names,
#' excluding Inaccessible/Unpopulated), with whether it has a current
#' team-area version and who/when. Used by both the intro table's
#' team-area drill-down and the admin district-review section. Empty
#' data frame (not NULL) if there's no current health-area version yet, or
#' it has no real health areas — callers can treat both the same way
#' ("nothing to show") without an extra is.null() check.
db_get_district_team_area_summary <- function(pool, campaign_id, district_name) {
  empty <- data.frame(health_area_name = character(0), has_current = logical(0),
                      owner_username = character(0), shared_at = as.POSIXct(character(0)))

  current_ha <- db_get_shared_version(pool, campaign_id, district_name)
  if (is.null(current_ha) || is.null(current_ha$snap$dfa_names)) return(empty)

  ha_names <- setdiff(unlist(current_ha$snap$dfa_names), c('Inaccessible', 'Unpopulated'))
  if (length(ha_names) == 0) return(empty)

  current_teams <- tryCatch(
    .db_query(pool, "
      SELECT health_area_name, owner_username, shared_at
      FROM team_area_versions
      WHERE campaign_id = ?c AND district_name = ?d AND is_shared = TRUE AND archived_at IS NULL
    ", list(c = as.integer(campaign_id), d = district_name)),
    error = function(e) NULL
  )

  out <- data.frame(health_area_name = ha_names, stringsAsFactors = FALSE)
  if (!is.null(current_teams) && nrow(current_teams) > 0) {
    out <- merge(out, current_teams, by = 'health_area_name', all.x = TRUE)
  } else {
    out$owner_username <- NA_character_
    out$shared_at <- as.POSIXct(NA)
  }
  out$has_current <- !is.na(out$owner_username)
  out[order(out$health_area_name), ]
}


# =============================================================================
# SECTION 14: Campaign-district assignment
#
# Campaigns no longer implicitly apply to every district — a district must
# be explicitly assigned before it appears anywhere in the app for that
# campaign. An ongoing, revisitable admin action (not fixed at campaign
# creation) — see mod_admin_tab_v2.R's "Manage districts" per campaign.
# =============================================================================

#' Districts currently assigned to a campaign.
db_get_campaign_districts <- function(pool, campaign_id) {
  tryCatch(
    .db_query(pool, "
      SELECT district_name, added_at, added_by FROM campaign_districts
      WHERE campaign_id = ?c ORDER BY district_name
    ", list(c = as.integer(campaign_id))),
    error = function(e) { cat('[db] get_campaign_districts error:', e$message, '\n'); NULL }
  )
}

#' Assign one district to a campaign. No-op (ON CONFLICT DO NOTHING) if
#' already assigned — the carry-forward prompt is only offered for
#' districts genuinely NEW to the campaign (see mod_admin_tab_v2.R), so
#' re-assigning an already-assigned district should never re-trigger it.
db_assign_district_to_campaign <- function(pool, campaign_id, district_name, added_by) {
  .db_execute(pool, "
    INSERT INTO campaign_districts (campaign_id, district_name, added_by)
    VALUES (?c, ?d, ?u)
    ON CONFLICT (campaign_id, district_name) DO NOTHING
  ", list(c = as.integer(campaign_id), d = district_name, u = added_by))
}

db_remove_district_from_campaign <- function(pool, campaign_id, district_name) {
  .db_execute(pool, "
    DELETE FROM campaign_districts WHERE campaign_id = ?c AND district_name = ?d
  ", list(c = as.integer(campaign_id), d = district_name))
}

#' Bundled carry-forward for one district newly assigned to
#' target_campaign_id: finds the most recently PUBLISHED health-area
#' version for that district in any OTHER campaign, branches it into
#' target_campaign_id, publishes the branch immediately (this IS an
#' explicit admin action — it deliberately bypasses the normal submit-time
#' "make current" prompt, since bulk-assigning districts to a campaign is
#' inherently a bulk publish decision, not an individual user's
#' incremental submission). Then does the same for every health area that
#' had a current team-area version under that source health-area version —
#' each copied via a plain SQL INSERT...SELECT (mirroring db_branch_version's
#' own pattern) rather than a fetch-then-reinsert round-trip through R,
#' and each published too. Staleness never arises here — the health-area
#' boundary is copied unchanged from its source, not generated fresh, so
#' nothing about it could have diverged from what any pinned team-area
#' version already assumes.
#'
#' Returns NULL if the district has no prior published health-area version
#' in any other campaign — nothing to carry forward; the caller should
#' simply not call this (district starts blank in the new campaign) rather
#' than treat NULL as an error.
db_carry_forward_district_to_campaign <- function(pool, target_campaign_id, district_name, admin_username) {
  source_ha <- tryCatch({
    rows <- db_get_shareable_versions(pool, district_name, campaign_id = NULL)
    if (is.null(rows) || nrow(rows) == 0) return(NULL)
    rows <- rows[rows$campaign_id != target_campaign_id, , drop = FALSE]
    if (nrow(rows) == 0) return(NULL)
    rows[order(rows$shared_at, decreasing = TRUE), ][1, ]
  }, error = function(e) NULL)
  if (is.null(source_ha)) return(NULL)

  new_ha_id <- db_branch_version(
    pool, source_version_id = source_ha$version_id, owner_username = admin_username,
    target_campaign_id = target_campaign_id, district_name = district_name
  )
  if (is.null(new_ha_id)) stop('failed to carry forward health-area version')
  db_publish_version(pool, new_ha_id, actor_role = 'admin')

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  source_team_ids <- tryCatch(
    DBI::dbGetQuery(conn, "
      SELECT team_version_id FROM team_area_versions
      WHERE based_on_health_area_version_id = $1 AND is_shared = TRUE AND archived_at IS NULL
    ", list(as.integer(source_ha$version_id)))$team_version_id,
    error = function(e) integer(0)
  )

  carried_team_ids <- integer(0)
  for (src_id in source_team_ids) {
    ha_name <- tryCatch(
      DBI::dbGetQuery(conn, "SELECT health_area_name FROM team_area_versions WHERE team_version_id = $1",
                      list(as.integer(src_id)))$health_area_name[1],
      error = function(e) NULL
    )
    if (is.null(ha_name)) next
    ver <- .next_team_version_number(conn, admin_username, target_campaign_id, district_name, ha_name)
    new_row <- tryCatch(
      DBI::dbGetQuery(conn, "
        INSERT INTO team_area_versions (
          owner_username, campaign_id, district_name, health_area_name,
          based_on_health_area_version_id, version_number, branched_from_id, submitted_by,
          saved_team_sf, team_names, current_team_assignments,
          smoothed_team_sf, smoothing_generated_at
        )
        SELECT
          $2, $3, $4, health_area_name,
          $5, $6, team_version_id, $2,
          saved_team_sf, team_names, current_team_assignments,
          smoothed_team_sf, smoothing_generated_at
        FROM team_area_versions
        WHERE team_version_id = $1
        RETURNING team_version_id
      ", list(as.integer(src_id), admin_username, as.integer(target_campaign_id), district_name,
               as.integer(new_ha_id), ver)),
      error = function(e) { cat('[db] carry_forward team-area copy error:', e$message, '\n'); NULL }
    )
    if (!is.null(new_row) && nrow(new_row) > 0) {
      new_team_id <- as.integer(new_row$team_version_id[1])
      db_publish_team_area(pool, new_team_id)
      carried_team_ids <- c(carried_team_ids, new_team_id)
    }
  }

  list(health_area_version_id = new_ha_id, team_area_version_ids = carried_team_ids)
}
