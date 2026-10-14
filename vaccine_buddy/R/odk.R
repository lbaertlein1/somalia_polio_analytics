# =============================================================================
# odk.R -- keep the vaccine_buddy database in step with ODK Central
#
# Vaccine Buddy telemetry (ODK form vaccine_buddy_events_aug2025_nopv2_snid):
# one submission per carrier upload (device_id, team_name, ...) with a
# repeat group `entry` holding the log rows (timestamp, gps_point,
# gps_accuracy_m, lid_open, temp_c, speed_kph, battery_percent).
#
# odk_sync() pulls only uploads received since the newest one already
# stored (with an hour's overlap) and inserts them into vb_submissions /
# vb_entries. A log row is keyed by (device_id, entry_id), so carriers
# re-uploading the same rows -- ~12% of rows in Aug 2026 -- are stored
# once. Runs hourly from cron (scripts/sync_odk.R) and on demand from the
# app's "Check for new data" button; a Postgres advisory lock keeps the
# two from running at once.
#
# Only needs DBI, RPostgres, ruODK and lubridate, so the cron job can run
# on the database VM without the app's spatial packages.
# =============================================================================

SYNC_LOCK_ID <- 7311001   # pg advisory lock key for odk_sync()
SOMALIA_BBOX <- c(xmin = 40.9, ymin = -1.8, xmax = 51.5, ymax = 12.1)

odk_setup <- function() {
  ruODK::ru_setup(
    url = Sys.getenv('ODK_URL', 'https://emro.nafundi.com'),
    pid = Sys.getenv('ODK_PID', '32'),
    fid = Sys.getenv('ODK_FID', 'vaccine_buddy_events_aug2025_nopv2_snid'),
    un  = Sys.getenv('ODK_USERNAME'),
    pw  = Sys.getenv('ODK_PASSWORD'),
    tz  = 'UTC',
    verbose = FALSE
  )
}

#' Raw ruODK tables for uploads received at or after `since` (POSIXct, or
#' NULL for everything). The repeat table filters on its parent's
#' submission date via $root (ODK Central 2023.2+).
odk_pull_since <- function(since = NULL) {
  odk_setup()
  f <- if (is.null(since)) NULL else format(since, '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')
  main  <- ruODK::odata_submission_get(table = 'Submissions', wkt = FALSE, download = FALSE,
                                       filter = if (!is.null(f)) sprintf('__system/submissionDate ge %s', f))
  entry <- ruODK::odata_submission_get(table = 'Submissions.entry', wkt = FALSE, download = FALSE,
                                       filter = if (!is.null(f)) sprintf('$root/Submissions/__system/submissionDate ge %s', f))
  list(main = main, entry = entry)
}

# In the repeat table ruODK leaves the geopoint as its GeoJSON parts:
# gps_point_coordinates_1 (lon), _2 (lat). The _longitude/_latitude form
# (ruODK's naming for a top-level geopoint) is accepted in case the form
# changes.
.entry_lonlat <- function(entry) {
  for (cols in list(c('gps_point_coordinates_1', 'gps_point_coordinates_2'),
                    c('gps_point_longitude', 'gps_point_latitude'))) {
    if (all(cols %in% names(entry)))
      return(list(lon = as.numeric(entry[[cols[1]]]), lat = as.numeric(entry[[cols[2]]])))
  }
  if (nrow(entry) == 0) return(list(lon = numeric(0), lat = numeric(0)))
  stop('No GPS columns found in the ODK entry table. Columns: ', paste(names(entry), collapse = ', '))
}

.col <- function(df, name, as = as.character) if (name %in% names(df)) as(df[[name]]) else as(rep(NA, nrow(df)))

#' ruODK tables -> rows for vb_submissions and vb_entries.
odk_to_rows <- function(raw) {
  m <- raw$main; e <- raw$entry
  subs <- data.frame(
    submission_id   = .col(m, 'id'),
    device_id       = .col(m, 'device_id'),
    team_name       = .col(m, 'team_name'),
    campaign_name   = .col(m, 'campaign_name'),
    submission_date = lubridate::ymd_hms(.col(m, 'system_submission_date'), quiet = TRUE),
    stringsAsFactors = FALSE)
  if (nrow(e) == 0) return(list(subs = subs, entries = NULL))

  ll <- .entry_lonlat(e)
  sub_id <- .col(e, 'submissions_id')
  entries <- data.frame(
    device_id       = subs$device_id[match(sub_id, subs$submission_id)],
    entry_id        = ifelse(is.na(.col(e, 'entry_id')), .col(e, 'id'), .col(e, 'entry_id')),
    submission_id   = sub_id,
    ts              = lubridate::ymd_hms(.col(e, 'timestamp'), quiet = TRUE),
    lon             = ll$lon,
    lat             = ll$lat,
    accuracy_m      = .col(e, 'gps_accuracy_m', as.numeric),
    lid_open        = .col(e, 'lid_open') %in% c('y', 'yes', '1', 'TRUE'),
    temp_c          = .col(e, 'temp_c', as.numeric),
    speed_kph       = .col(e, 'speed_kph', as.numeric),
    battery_percent = .col(e, 'battery_percent', as.numeric),
    stringsAsFactors = FALSE)
  # Entries whose upload isn't in this pull arrive with it next time
  entries <- entries[!is.na(entries$device_id) & !is.na(entries$entry_id), ]
  entries <- entries[!duplicated(entries[, c('device_id', 'entry_id')]), ]
  list(subs = subs, entries = entries)
}

# Insert rows, skipping ones already stored; returns the number inserted.
.insert_new <- function(con, table, df) {
  if (is.null(df) || nrow(df) == 0) return(0L)
  tmp <- paste0('tmp_', table)
  DBI::dbWriteTable(con, tmp, df, temporary = TRUE, overwrite = TRUE)
  cols <- paste(DBI::dbQuoteIdentifier(con, names(df)), collapse = ', ')
  n <- DBI::dbExecute(con, sprintf('INSERT INTO %s (%s) SELECT %s FROM %s ON CONFLICT DO NOTHING', table, cols, cols, tmp))
  DBI::dbExecute(con, sprintf('DROP TABLE %s', tmp))
  as.integer(n)
}

#' Bring the database up to date with ODK. `con` is a plain DBI connection
#' to the vaccine_buddy database. full = TRUE re-pulls everything (new rows
#' are still only added once). Returns list(status, n_uploads,
#' n_new_entries, message).
odk_sync <- function(con, triggered_by = 'manual', full = FALSE) {
  got_lock <- DBI::dbGetQuery(con, 'SELECT pg_try_advisory_lock($1) AS ok', list(SYNC_LOCK_ID))$ok
  if (!isTRUE(got_lock))
    return(list(status = 'busy', n_uploads = 0L, n_new_entries = 0L,
                message = 'An update is already running (the hourly job or another user). Try again in a few minutes.'))
  on.exit(DBI::dbExecute(con, 'SELECT pg_advisory_unlock($1)', list(SYNC_LOCK_ID)), add = TRUE)

  last <- DBI::dbGetQuery(con, 'SELECT max(submission_date) AS last FROM vb_submissions')$last
  since <- if (full || length(last) == 0 || is.na(last)) NULL else last - 3600
  log_id <- DBI::dbGetQuery(con, 'INSERT INTO vb_sync_log (since, triggered_by) VALUES ($1, $2) RETURNING id',
                            list(if (is.null(since)) as.POSIXct(NA) else since, triggered_by))$id

  res <- tryCatch({
    rows <- odk_to_rows(odk_pull_since(since))
    DBI::dbWithTransaction(con, {
      n_subs <- .insert_new(con, 'vb_submissions', rows$subs)
      n_ent  <- .insert_new(con, 'vb_entries', rows$entries)
    })
    # Local place names from campaign monitoring (R/place_names.R); a
    # failure here doesn't fail the Vaccine Buddy update.
    places_msg <- tryCatch(place_names_sync(con), error = function(e) paste('Place names not updated:', conditionMessage(e)))
    list(status = 'ok', n_uploads = n_subs, n_new_entries = n_ent,
         message = sprintf('%s new uploads, %s new log rows. %s', format(n_subs, big.mark = ','), format(n_ent, big.mark = ','), places_msg))
  }, error = function(e) list(status = 'error', n_uploads = 0L, n_new_entries = 0L, message = conditionMessage(e)))

  DBI::dbExecute(con, 'UPDATE vb_sync_log SET finished_at = NOW(), status = $1, n_uploads = $2, n_new_entries = $3, message = $4
                       WHERE id = $5', list(res$status, res$n_uploads, res$n_new_entries, res$message, log_id))
  res
}
