# =============================================================================
# sync_odk.R -- copy new Vaccine Buddy uploads from ODK Central into the
# vaccine_buddy database. Runs on the VM only; the app never calls ODK.
#
#   Rscript scripts/sync_odk.R                  hourly update
#   Rscript scripts/sync_odk.R --if-requested   every minute: runs only if
#                                               someone clicked "Check for
#                                               new data" in the app
#   Rscript scripts/sync_odk.R --full           re-pull everything
#
# Run from the vaccine_buddy/ folder (it reads .env, R/odk.R and
# R/place_names.R -- the update also refreshes local place names from
# campaign monitoring forms). Crontab lines: scripts/crontab.txt. Needs R
# packages DBI, RPostgres, ruODK and lubridate (not the app's spatial
# packages). Each update is recorded in vb_sync_log; exits non-zero on error.
# =============================================================================

if (file.exists('.env')) readRenviron('.env')
args <- commandArgs(TRUE)

env_first <- function(keys, default = '') {
  for (k in keys) { v <- Sys.getenv(k); if (nzchar(v)) return(v) }
  default
}

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = env_first(c('VB_DB_HOST', 'DB_HOST')),
  port     = as.integer(env_first(c('VB_DB_PORT', 'DB_PORT'), '5432')),
  dbname   = env_first('VB_DB_NAME', 'vaccine_buddy'),
  user     = env_first(c('VB_DB_USER', 'DB_USER')),
  password = env_first(c('VB_DB_PASSWORD', 'DB_PASSWORD')),
  sslmode  = env_first(c('VB_DB_SSL', 'DB_SSL'), 'require')
)

# Every-minute mode: claim the oldest waiting request, or stop here (fast,
# without loading ruODK) if there is none.
request <- NULL
if ('--if-requested' %in% args) {
  request <- DBI::dbGetQuery(con, "
    UPDATE vb_sync_requests SET picked_at = NOW()
    WHERE id = (SELECT id FROM vb_sync_requests WHERE picked_at IS NULL AND done_at IS NULL
                ORDER BY requested_at LIMIT 1 FOR UPDATE SKIP LOCKED)
    RETURNING id, requested_by")
  if (nrow(request) == 0) { DBI::dbDisconnect(con); quit(save = 'no', status = 0) }
}

source('R/odk.R')
source('R/place_names.R')
res <- odk_sync(con, triggered_by = if (!is.null(request)) request$requested_by[1] else 'cron', full = '--full' %in% args)

if (!is.null(request)) {
  if (res$status == 'busy') {
    # The hourly update is running right now: hand the request back so the
    # next minute's run picks it up again.
    DBI::dbExecute(con, 'UPDATE vb_sync_requests SET picked_at = NULL WHERE id = $1', list(request$id[1]))
  } else {
    DBI::dbExecute(con, 'UPDATE vb_sync_requests SET done_at = NOW(), status = $1, message = $2 WHERE id = $3',
                   list(res$status, res$message, request$id[1]))
  }
}
DBI::dbDisconnect(con)

cat(format(Sys.time(), '%Y-%m-%d %H:%M:%S'), if (!is.null(request)) 'requested:' else 'hourly:', res$status, res$message, '\n')
if (res$status == 'error') quit(status = 1)
