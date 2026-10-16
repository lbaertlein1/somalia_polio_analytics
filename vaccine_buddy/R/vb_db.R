# =============================================================================
# vb_db.R -- this app's own database (schema: scripts/create_db.R)
# =============================================================================

vb_connect <- function() {
  pool::dbPool(
    drv      = RPostgres::Postgres(),
    host     = env_first(c('VB_DB_HOST', 'DB_HOST')),
    port     = as.integer(env_first(c('VB_DB_PORT', 'DB_PORT'), '5432')),
    dbname   = env_first('VB_DB_NAME', 'vaccine_buddy'),
    user     = env_first(c('VB_DB_USER', 'DB_USER')),
    password = env_first(c('VB_DB_PASSWORD', 'DB_PASSWORD')),
    sslmode  = env_first(c('VB_DB_SSL', 'DB_SSL'), 'require'),
    minSize = 1L, maxSize = 5L, idleTimeout = 300000L
  )
}

# --- Logins (this app's own accounts; scripts/add_user.R adds them) -----------

#' All logins (no password hashes).
vb_list_users <- function() {
  .q('SELECT username, display_name, role, created_at FROM users ORDER BY username')
}

#' Add a login, or update an existing one's name and role. password NULL
#' keeps the current password (only allowed for existing users).
vb_save_user <- function(username, display_name, role, password = NULL) {
  stopifnot(role %in% c('admin', 'user'))
  if (is.null(password)) {
    n <- .x('UPDATE users SET display_name = $1, role = $2, updated_at = NOW() WHERE username = $3',
            list(display_name, role, username))
    if (n == 0) stop('a new user needs a password')
  } else {
    .x('INSERT INTO users (username, password, display_name, role) VALUES ($1, $2, $3, $4)
        ON CONFLICT (username) DO UPDATE SET password = EXCLUDED.password, display_name = EXCLUDED.display_name,
                                             role = EXCLUDED.role, updated_at = NOW()',
       list(username, bcrypt::hashpw(password), display_name, role))
  }
  invisible(TRUE)
}

vb_delete_user <- function(username) .x('DELETE FROM users WHERE username = $1', list(username))

#' Same check as the adjuster's db_validate_credentials(), against this
#' app's users table. Returns the user's row (username, display_name,
#' role) or NULL.
vb_validate_credentials <- function(uname, pword) {
  row <- tryCatch(
    DBI::dbGetQuery(vb_pool, 'SELECT username, display_name, role, password FROM users WHERE username = $1', list(uname)),
    error = function(e) { message('[auth] ', conditionMessage(e)); NULL })
  if (is.null(row) || nrow(row) != 1) return(NULL)
  if (!isTRUE(tryCatch(bcrypt::checkpw(pword, row$password[1]), error = function(e) FALSE))) return(NULL)
  row[1, c('username', 'display_name', 'role')]
}

.q <- function(sql, params = NULL) DBI::dbGetQuery(vb_pool, sql, params = params)
.x <- function(sql, params = NULL) DBI::dbExecute(vb_pool, sql, params = params)

#' Where a user last was: campaign, district and days chosen (NAs if never,
#' or the columns are missing).
vb_get_last_view <- function(username) {
  d <- tryCatch(.q('SELECT last_campaign_id, last_district, last_date_from, last_date_to FROM users WHERE username = $1', list(username)),
                error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(list(campaign_id = NA_integer_, district_name = NA_character_, date_from = as.Date(NA), date_to = as.Date(NA)))
  list(campaign_id = d$last_campaign_id[1], district_name = d$last_district[1],
       date_from = as.Date(d$last_date_from[1]), date_to = as.Date(d$last_date_to[1]))
}
vb_set_last_view <- function(username, campaign_id, district_name, date_from, date_to) {
  tryCatch(.x('UPDATE users SET last_campaign_id = $1, last_district = $2, last_date_from = $3, last_date_to = $4 WHERE username = $5',
              list(as.integer(campaign_id), district_name, as.character(date_from), as.character(date_to), username)),
           error = function(e) NULL)
}

# --- Analysis settings (Admin page) --------------------------------------------

#' DEFAULT_PARAMS with the admins' changes (app_settings 'analysis_params':
#' only the settings in PARAM_UI that differ from the defaults are stored).
vb_get_params <- function() {
  v <- tryCatch(.q("SELECT value::text AS v FROM app_settings WHERE key = 'analysis_params'")$v, error = function(e) character(0))
  saved <- if (length(v)) tryCatch(jsonlite::fromJSON(v[1]), error = function(e) list()) else list()
  saved <- saved[intersect(names(saved), names(PARAM_UI))]
  utils::modifyList(DEFAULT_PARAMS, saved)
}

vb_save_params <- function(params, user) {
  keys <- intersect(names(params), names(PARAM_UI))
  changed <- params[keys][!mapply(identical, params[keys], DEFAULT_PARAMS[keys])]
  json <- if (length(changed)) as.character(jsonlite::toJSON(changed, auto_unbox = TRUE)) else '{}'
  .x("INSERT INTO app_settings (key, value, updated_by) VALUES ('analysis_params', $1::jsonb, $2)
      ON CONFLICT (key) DO UPDATE SET value = EXCLUDED.value, updated_by = EXCLUDED.updated_by, updated_at = NOW()",
     list(json, user))
}

# --- Work in progress, saved automatically (work_sessions) ----------------------

.pack   <- function(x) blob::new_blob(list(memCompress(serialize(x, NULL), 'gzip')))
.unpack <- function(r) if (is.null(r) || length(r) == 0) NULL else unserialize(memDecompress(r, 'gzip'))

#' Save a user's work on one campaign + district. `base` (the search) and
#' `edits` are R objects; only those given are written, the other is kept.
vb_save_work <- function(username, campaign_id, district_name, base = NULL, edits = NULL) {
  parts <- Filter(Negate(is.null), list(base = base, edits = edits))
  if (length(parts) == 0) return(invisible(0))
  cols <- names(parts)
  .x(sprintf('INSERT INTO work_sessions (username, campaign_id, district_name, %s) VALUES ($1, $2, $3, %s)
              ON CONFLICT (username, campaign_id, district_name) DO UPDATE SET %s, updated_at = NOW()',
             paste(cols, collapse = ', '), paste0('$', 3 + seq_along(cols), collapse = ', '),
             paste(sprintf('%s = EXCLUDED.%s', cols, cols), collapse = ', ')),
     c(list(username, as.integer(campaign_id), district_name), unname(lapply(parts, .pack))))
}

#' list(base, edits, updated_at), or NULL if nothing is saved.
vb_load_work <- function(username, campaign_id, district_name) {
  d <- .q('SELECT base, edits, updated_at FROM work_sessions WHERE username = $1 AND campaign_id = $2 AND district_name = $3',
          list(username, as.integer(campaign_id), district_name))
  if (nrow(d) == 0) return(NULL)
  list(base = .unpack(d$base[[1]]), edits = .unpack(d$edits[[1]]), updated_at = d$updated_at[1])
}

# --- Team roles per campaign (Admin page; team_roles() in analysis.R) ----------

#' The admins' choices for a campaign's teams: team_name, role ('auto' --
#' follows its name -- | 'area' | 'no_area' | 'ignore'), health_area_name
#' (role 'area'), district_name (set by hand, else NA). Teams not listed
#' follow their name, in the district where they logged most GPS.
vb_get_team_roles <- function(campaign_id) {
  .q('SELECT team_name, role, health_area_name, district_name FROM team_roles WHERE campaign_id = $1', list(as.integer(campaign_id)))
}

#' role NULL: back to following its name, in its usual district.
vb_set_team_role <- function(campaign_id, team_name, role, health_area_name = NA_character_, district_name = NA_character_, user) {
  if (is.null(role)) return(.x('DELETE FROM team_roles WHERE campaign_id = $1 AND team_name = $2', list(as.integer(campaign_id), team_name)))
  .x('INSERT INTO team_roles (campaign_id, team_name, role, health_area_name, district_name, updated_by) VALUES ($1, $2, $3, $4, $5, $6)
      ON CONFLICT (campaign_id, team_name) DO UPDATE SET role = EXCLUDED.role, health_area_name = EXCLUDED.health_area_name,
        district_name = EXCLUDED.district_name, updated_by = EXCLUDED.updated_by, updated_at = NOW()',
     list(as.integer(campaign_id), team_name, role, health_area_name, district_name, user))
}

#' Every team that logged GPS between two dates, with its fixes counted per
#' ~1 km square (lon/lat to 2 decimals): team_name, lon, lat, n.
vb_team_squares <- function(date_from, date_to) {
  .q('SELECT s.team_name, round(e.lon::numeric, 2)::float8 AS lon, round(e.lat::numeric, 2)::float8 AS lat, COUNT(*)::int AS n
      FROM vb_entries e JOIN vb_submissions s USING (submission_id)
      WHERE e.ts >= $1 AND e.ts < $2 AND e.lon IS NOT NULL AND e.lat IS NOT NULL AND s.team_name IS NOT NULL
      GROUP BY 1, 2, 3',
     list(as.POSIXct(as.Date(date_from), tz = ODK_TZ), as.POSIXct(as.Date(date_to) + 1, tz = ODK_TZ)))
}

# --- Vaccine Buddy data (stored by odk_sync(), R/odk.R) -------------------------

#' Stored GPS fixes with their team, for the dates (in ODK_TZ) analysed,
#' within `bbox` (an sf bbox) plus ~2 km.
vb_load_entries <- function(date_from, date_to, bbox) {
  from <- as.POSIXct(as.Date(date_from), tz = ODK_TZ)
  to   <- as.POSIXct(as.Date(date_to) + 1, tz = ODK_TZ)
  d <- .q('SELECT e.device_id, e.entry_id, e.ts, e.lon, e.lat, e.accuracy_m, e.lid_open, s.team_name
           FROM vb_entries e JOIN vb_submissions s USING (submission_id)
           WHERE e.ts >= $1 AND e.ts < $2 AND e.lon BETWEEN $3 AND $4 AND e.lat BETWEEN $5 AND $6',
          list(from, to, bbox[['xmin']] - 0.02, bbox[['xmax']] + 0.02, bbox[['ymin']] - 0.02, bbox[['ymax']] + 0.02))
  d$ts <- lubridate::with_tz(d$ts, ODK_TZ)
  d
}

#' Local place names (R/place_names.R) within a bounding box, plus 1 km.
vb_load_place_names <- function(bbox) {
  .q('SELECT name, lon, lat FROM vb_place_names WHERE lon BETWEEN $1 AND $2 AND lat BETWEEN $3 AND $4',
     list(bbox[['xmin']] - 0.01, bbox[['xmax']] + 0.01, bbox[['ymin']] - 0.01, bbox[['ymax']] + 0.01))
}

#' When the data was last brought up to date, and what's stored.
vb_sync_status <- function() {
  last_ok <- .q("SELECT finished_at, triggered_by, n_new_entries FROM vb_sync_log
                 WHERE status = 'ok' ORDER BY finished_at DESC LIMIT 1")
  last_err <- .q("SELECT finished_at, message FROM vb_sync_log WHERE status = 'error'
                  AND finished_at > COALESCE((SELECT max(finished_at) FROM vb_sync_log WHERE status = 'ok'), '-infinity')
                  ORDER BY finished_at DESC LIMIT 1")
  stored <- .q('SELECT COUNT(*)::int AS n, max(ts) AS latest FROM vb_entries')
  list(last_ok = last_ok, last_err = last_err, n_rows = stored$n, latest_ts = stored$latest,
       pending = vb_pending_request())
}

# --- Update requests (the app asks; the VM's every-minute job does the work) ---

SYNC_REQUEST_STALE_MIN <- 15   # a request the VM hasn't finished by then no longer blocks new ones

#' The open (not finished, not stale) update request, if any: one row with
#' requested_by, requested_at, picked_at.
vb_pending_request <- function() {
  .q(sprintf("SELECT requested_by, requested_at, picked_at FROM vb_sync_requests
              WHERE done_at IS NULL AND requested_at > NOW() - INTERVAL '%d minutes'
              ORDER BY requested_at DESC LIMIT 1", SYNC_REQUEST_STALE_MIN))
}

#' Ask the VM for an update now. Returns 'requested', or 'already' if one
#' is already waiting or running (repeated clicks, other users).
vb_request_sync <- function(user) {
  n <- .x(sprintf("INSERT INTO vb_sync_requests (requested_by)
                   SELECT $1 WHERE NOT EXISTS (
                     SELECT 1 FROM vb_sync_requests
                     WHERE done_at IS NULL AND requested_at > NOW() - INTERVAL '%d minutes')", SYNC_REQUEST_STALE_MIN),
          list(user))
  if (n == 1) 'requested' else 'already'
}

#' Whether the last request went unanswered (the VM job may not be running).
vb_last_request_stale <- function() {
  r <- .q(sprintf("SELECT 1 FROM vb_sync_requests WHERE done_at IS NULL
                   AND requested_at <= NOW() - INTERVAL '%d minutes'
                   AND requested_at > NOW() - INTERVAL '1 day'
                   AND id = (SELECT max(id) FROM vb_sync_requests)", SYNC_REQUEST_STALE_MIN))
  nrow(r) > 0
}

# --- Supervisors -------------------------------------------------------------
# One supervisor (name + phone) per health area of a campaign's district.
# Each health area keeps one row: edits change it in place, and clearing
# it switches it off (is_active), so messages already sent still point at
# a real row.

#' The district's supervisors, one per health area: supervisor_id,
#' health_area_name, supervisor_name, phone.
vb_get_supervisors <- function(campaign_id, district_name) {
  .q("SELECT DISTINCT ON (health_area_name) supervisor_id, district_name, health_area_name, supervisor_name, phone
      FROM supervisors WHERE campaign_id = $1 AND district_name = $2 AND is_active
      ORDER BY health_area_name, updated_at DESC", list(as.integer(campaign_id), district_name))
}

#' Set the supervisor of each health area in `df` (health_area_name,
#' supervisor_name, phone); an empty name and phone removes it.
vb_set_supervisors <- function(campaign_id, district_name, df, user) {
  cid <- as.integer(campaign_id)
  pool::poolWithTransaction(vb_pool, function(con) {
    for (i in seq_len(nrow(df))) {
      # the phone in international form, as the WhatsApp link needs it (the
      # Supervisors table then shows it that way)
      h <- df$health_area_name[i]; name <- trimws(df$supervisor_name[i]); phone <- normalize_phone(trimws(df$phone[i]))
      on <- nzchar(name) || nzchar(phone)
      # the health area's row: one that already has this phone (so a phone
      # never clashes with an older row of the health area), else its
      # active one, else its latest
      id <- DBI::dbGetQuery(con, 'SELECT supervisor_id FROM supervisors WHERE campaign_id = $1 AND district_name = $2
                                  AND health_area_name = $3 ORDER BY (phone = $4) DESC, is_active DESC, updated_at DESC LIMIT 1',
                            list(cid, district_name, h, phone))$supervisor_id
      if (length(id)) {
        DBI::dbExecute(con, 'UPDATE supervisors SET supervisor_name = $1, phone = $2, is_active = $3, updated_by = $4,
                               updated_at = NOW() WHERE supervisor_id = $5', list(name, phone, on, user, id))
        # any older rows of the health area (from before one per health area) off
        DBI::dbExecute(con, 'UPDATE supervisors SET is_active = FALSE WHERE campaign_id = $1 AND district_name = $2
                             AND health_area_name = $3 AND supervisor_id <> $4 AND is_active', list(cid, district_name, h, id))
      } else if (on) {
        DBI::dbExecute(con, 'INSERT INTO supervisors (campaign_id, district_name, health_area_name, supervisor_name, phone, updated_by)
                             VALUES ($1, $2, $3, $4, $5, $6)', list(cid, district_name, h, name, phone, user))
      }
    }
  })
}

#' Campaigns with supervisors for a district: campaign_id, n.
vb_supervisor_campaigns <- function(district_name) {
  .q('SELECT campaign_id, COUNT(DISTINCT health_area_name)::int AS n FROM supervisors
      WHERE district_name = $1 AND is_active GROUP BY campaign_id', list(district_name))
}

#' Copy a district's supervisors from an earlier campaign (one per health area).
vb_copy_supervisors <- function(from_campaign_id, to_campaign_id, district_name, user) {
  d <- vb_get_supervisors(from_campaign_id, district_name)
  vb_set_supervisors(to_campaign_id, district_name, d[, c('health_area_name', 'supervisor_name', 'phone')], user)
  nrow(d)
}

# --- Analysis runs -----------------------------------------------------------

#' Save one analysis (run row + coverage + places) in a single transaction.
vb_save_run <- function(campaign_id, campaign_name, district_name, data_from, data_to, params, report, pings, result, user) {
  pool::poolWithTransaction(vb_pool, function(con) {
    run_id <- DBI::dbGetQuery(con, '
      INSERT INTO analysis_runs (campaign_id, campaign_name, district_name, data_from, data_to, params, cleaning_report,
                                 n_pings_used, n_devices, created_by)
      VALUES ($1, $2, $3, $4, $5, $6::jsonb, $7::jsonb, $8, $9, $10) RETURNING run_id',
      list(as.integer(campaign_id), campaign_name, district_name, as.character(data_from), as.character(data_to),
           as.character(jsonlite::toJSON(params, auto_unbox = TRUE)), as.character(jsonlite::toJSON(report)),
           sum(!pings$moving_fast), length(unique(pings$device_id)), user))$run_id

    cov <- result$coverage
    cov <- data.frame(run_id = run_id, district_name = cov$district_name, health_area_name = cov$health_area,
                      team_area_name = cov$team_area, populated_cells = cov$populated_cells,
                      visited_cells = cov$visited_cells, u5_pop = cov$u5_pop,
                      u5_pop_visited = cov$u5_pop_visited, n_places = cov$n_places,
                      n_fixes = cov$n_fixes, n_devices = cov$n_devices, teams = cov$teams, has_data = cov$has_data)
    DBI::dbAppendTable(con, 'area_coverage', cov)

    p <- result$places
    if (!is.null(p) && nrow(p) > 0) {
      geom <- vapply(seq_len(nrow(p)), function(i) geojsonsf::sfc_geojson(p$outline[i]), character(1))
      pl <- data.frame(run_id = run_id, district_name = p$district_name, health_area_name = p$health_area,
                       team_area_name = p$team_area, assigned_team = p$assigned_team, team_source = p$team_source,
                       team_dist_m = p$team_dist_m, place_no = p$place_no, lon = p$lon, lat = p$lat,
                       n_cells = p$n_cells, u5_pop = round(p$u5_pop, 1), label = p$label, near_idp = p$near_idp,
                       geom = geom)
      DBI::dbAppendTable(con, 'missed_places', pl)
    }
    run_id
  })
}

vb_get_runs <- function(campaign_id, district_name) {
  .q("SELECT r.run_id, r.data_from, r.data_to, r.created_by, r.created_at,
             (SELECT COUNT(*)::int FROM missed_places p WHERE p.run_id = r.run_id) AS n_places
      FROM analysis_runs r WHERE r.campaign_id = $1 AND r.district_name = $2
      ORDER BY r.created_at DESC", list(as.integer(campaign_id), district_name))
}

vb_get_run <- function(run_id) .q('SELECT * FROM analysis_runs WHERE run_id = $1', list(as.integer(run_id)))

vb_get_coverage <- function(run_id) .q('SELECT * FROM area_coverage WHERE run_id = $1', list(as.integer(run_id)))

#' Places for a run (optionally one health area), as sf points with an
#' `outline` polygon column.
vb_get_places <- function(run_id, district_name = NULL, health_area_name = NULL) {
  sql <- 'SELECT place_id, district_name, health_area_name, team_area_name, assigned_team, team_source, team_dist_m, place_no, lon, lat, n_cells, u5_pop,
                 label, near_idp, geom::text AS geom, status, status_at, status_note
          FROM missed_places WHERE run_id = $1'
  params <- list(as.integer(run_id))
  if (!is.null(health_area_name)) {
    sql <- paste(sql, 'AND district_name = $2 AND health_area_name = $3')
    params <- c(params, list(district_name, health_area_name))
  }
  d <- .q(paste(sql, 'ORDER BY district_name, health_area_name, place_no'), params)
  if (nrow(d) == 0) return(NULL)
  out <- sf::st_as_sf(d, coords = c('lon', 'lat'), crs = 4326, remove = FALSE)
  out$outline <- sf::st_cast(do.call(c, lapply(d$geom, function(g) sf::st_geometry(geojsonsf::geojson_sf(g)))), 'MULTIPOLYGON')
  out$geom <- NULL
  out
}

vb_set_place_status <- function(place_id, status, via, note = NA_character_) {
  pool::poolWithTransaction(vb_pool, function(con) {
    DBI::dbExecute(con, 'UPDATE missed_places SET status = $1, status_at = NOW(), status_note = $2 WHERE place_id = $3',
                   list(status, note, as.integer(place_id)))
    DBI::dbExecute(con, 'INSERT INTO place_status_log (place_id, status, note, changed_via) VALUES ($1, $2, $3, $4)',
                   list(as.integer(place_id), status, note, via))
  })
}

# --- Google My Maps links (one map per health area of a saved list) ----------

#' health_area_name, url (none if the table is missing)
vb_get_mymaps <- function(run_id) {
  tryCatch(.q('SELECT health_area_name, url FROM mymaps_links WHERE run_id = $1', list(as.integer(run_id))),
           error = function(e) data.frame(health_area_name = character(0), url = character(0)))
}

#' url '' removes the link.
vb_set_mymaps <- function(run_id, health_area_name, url, user) {
  if (!nzchar(url)) return(.x('DELETE FROM mymaps_links WHERE run_id = $1 AND health_area_name = $2',
                              list(as.integer(run_id), health_area_name)))
  .x('INSERT INTO mymaps_links (run_id, health_area_name, url, updated_by) VALUES ($1, $2, $3, $4)
      ON CONFLICT (run_id, health_area_name) DO UPDATE SET url = EXCLUDED.url, updated_by = EXCLUDED.updated_by, updated_at = NOW()',
     list(as.integer(run_id), health_area_name, url, user))
}

# --- Maps to download (PDF, one per health area of a saved list) -------------

#' health_area_name, page_size, basemap of the maps made (none if the table is missing)
vb_pdf_areas <- function(run_id) {
  tryCatch(.q('SELECT health_area_name, page_size, basemap, tracks FROM area_pdfs WHERE run_id = $1', list(as.integer(run_id))),
           error = function(e) data.frame(health_area_name = character(0), page_size = character(0), basemap = character(0), tracks = logical(0)))
}

vb_save_pdf <- function(run_id, health_area_name, file, page_size, basemap, tracks, user) {
  raw <- readBin(file, 'raw', file.size(file))
  .x('INSERT INTO area_pdfs (run_id, health_area_name, page_size, basemap, tracks, pdf, created_by) VALUES ($1, $2, $3, $4, $5, $6, $7)
      ON CONFLICT (run_id, health_area_name) DO UPDATE SET page_size = EXCLUDED.page_size, basemap = EXCLUDED.basemap,
        tracks = EXCLUDED.tracks, pdf = EXCLUDED.pdf, created_by = EXCLUDED.created_by, created_at = NOW()',
     list(as.integer(run_id), health_area_name, page_size, basemap, isTRUE(tracks), blob::new_blob(list(raw)), user))
}

#' The PDF's bytes, or NULL.
vb_get_pdf <- function(run_id, health_area_name) {
  d <- .q('SELECT pdf FROM area_pdfs WHERE run_id = $1 AND health_area_name = $2', list(as.integer(run_id), health_area_name))
  if (nrow(d) == 0) NULL else d$pdf[[1]]
}

# --- Dispatches ---------------------------------------------------------------

#' Create (or refresh) the dispatch for each supervisor of a run. One
#' already sent is left as it was sent. `msgs` has supervisor_id,
#' supervisor_name, phone, token, message.
vb_upsert_dispatches <- function(run_id, msgs, user) {
  for (i in seq_len(nrow(msgs))) {
    .x('INSERT INTO dispatches (run_id, supervisor_id, token, supervisor_name, phone, message, created_by)
        VALUES ($1, $2, $3, $4, $5, $6, $7)
        ON CONFLICT (run_id, supervisor_id) DO UPDATE SET
          supervisor_name = EXCLUDED.supervisor_name, phone = EXCLUDED.phone, message = EXCLUDED.message
        WHERE dispatches.send_clicked_at IS NULL',
       list(as.integer(run_id), as.integer(msgs$supervisor_id[i]), msgs$token[i], msgs$supervisor_name[i],
            msgs$phone[i], msgs$message[i], user))
  }
}

#' Dispatches of a run with their health area's place counts: n (places),
#' closed (done, unreachable or no children), u5 (estimated under-5s in those places).
#' Supervisors removed since are left out, unless they were already sent one.
vb_get_dispatches <- function(run_id) {
  .q("SELECT d.*, s.district_name, s.health_area_name,
             COALESCE(p.n, 0) AS n, COALESCE(p.closed, 0) AS closed, COALESCE(p.u5, 0) AS u5
      FROM dispatches d
      JOIN supervisors s USING (supervisor_id)
      LEFT JOIN (SELECT district_name, health_area_name, COUNT(*)::int AS n,
                        SUM((status <> 'open')::int)::int AS closed, SUM(u5_pop) AS u5
                 FROM missed_places WHERE run_id = $1 GROUP BY 1, 2) p
        ON p.district_name = s.district_name AND p.health_area_name = s.health_area_name
      WHERE d.run_id = $1 AND (s.is_active OR d.send_clicked_at IS NOT NULL)
      ORDER BY s.district_name, s.health_area_name, d.supervisor_name", list(as.integer(run_id)))
}

vb_mark_sent <- function(dispatch_id) {
  .x('UPDATE dispatches SET send_clicked_at = NOW() WHERE dispatch_id = $1', list(as.integer(dispatch_id)))
}

#' Everything the supervisor page needs from its token. (Opens are no
#' longer counted; the n_opens / *_opened_at columns are left unused.)
vb_open_dispatch <- function(token) {
  d <- .q('SELECT dispatch_id, run_id, supervisor_id, supervisor_name, phone FROM dispatches WHERE token = $1', list(token))
  if (nrow(d) != 1) return(NULL)
  s <- .q('SELECT s.district_name, s.health_area_name, s.campaign_id, r.data_from, r.data_to, r.campaign_name,
                  r.params::text AS params_json
           FROM supervisors s, analysis_runs r WHERE s.supervisor_id = $1 AND r.run_id = $2',
          list(d$supervisor_id, d$run_id))
  # The health area's own teams (comma-separated), for their GPS tracks
  s$teams <- .q("SELECT teams FROM area_coverage WHERE run_id = $1 AND district_name = $2
                 AND health_area_name = $3 AND team_area_name = ''",
                list(d$run_id, s$district_name, s$health_area_name))$teams[1] %||% ''
  newer <- .q('SELECT token FROM dispatches WHERE supervisor_id = $1 AND run_id > $2 ORDER BY run_id DESC LIMIT 1',
              list(d$supervisor_id, d$run_id))
  c(as.list(d), as.list(s), list(newer_token = if (nrow(newer)) newer$token[1] else NULL))
}
