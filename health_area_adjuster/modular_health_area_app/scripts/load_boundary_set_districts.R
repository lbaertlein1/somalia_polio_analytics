# =============================================================================
# load_boundary_set_districts.R
#
# Fills boundary_set_districts: one row per (boundary set, district) with a
# shape_key. Two sets give a district the SAME shape_key only when its
# boundary didn't change between them -- that's what carry-forward checks
# (db_get_carry_forward_source() in mod_db_v2.R): a district can only be
# carried forward from a campaign whose boundary for it has the same key.
#
#   * the first set gets new keys:            "<set>:<DIST_UID>"
#   * a later set inherits the previous set's key for every district the
#     comparison report marks 'same' (IoU > 0.999), and gets a new key for
#     every district marked CHANGED/ADDED.
#
# When a new boundary set is loaded in future: run setup_boundary_sets.R
# for it, then add it to SETS below with prev = the set before it.
#
# Needs boundary_sets to exist (migrate_boundary_sets.R). Safe to re-run.
# Run from the project root that holds data/boundary_sets/ -- point
# ENV_FILE at the app's .env_v2.
# =============================================================================

DRY_RUN <- TRUE

ENV_FILE <- '.env_v2'                  # e.g. 'health_area_adjuster/modular_health_area_app/.env_v2'
SETS_DIR <- 'data/boundary_sets'

SETS <- list(
  list(set = '2026a', prev = NA),
  list(set = '2026b', prev = '2026a')
)

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dotenv)
  library(sf)
})

if (!file.exists(ENV_FILE)) stop('ENV_FILE not found: ', ENV_FILE)
dotenv::load_dot_env(ENV_FILE)
DB_HOST           <- Sys.getenv('DB_HOST', '23.239.19.115')
DB_PORT           <- as.integer(Sys.getenv('DB_PORT', '5432'))
DB_NAME           <- Sys.getenv('DB_NAME', 'somalia_health_areas_v2')
POSTGRES_PASSWORD <- Sys.getenv('POSTGRES_PASSWORD', '')
if (!nzchar(POSTGRES_PASSWORD)) stop('POSTGRES_PASSWORD is not set')
if (!grepl('_v2$', DB_NAME)) stop('DB_NAME is "', DB_NAME, '" -- this must run against the v2 database')

# --- build rows ---------------------------------------------------------------
keys <- list()   # set -> named vector DIST_UID -> shape_key
rows <- list()
for (s in SETS) {
  f <- file.path(SETS_DIR, s$set, 'districts_shp.Rds')
  if (!file.exists(f)) stop('Not found: ', f)
  d <- sf::st_drop_geometry(readRDS(f))
  if (anyDuplicated(d$DIST_UID) || anyDuplicated(d$DISP_L2)) stop(s$set, ': DIST_UID or DISP_L2 not unique')

  k <- setNames(paste0(s$set, ':', d$DIST_UID), d$DIST_UID)
  n_inherited <- 0
  if (!is.na(s$prev)) {
    cmp_file <- file.path(SETS_DIR, sprintf('compare_%s_vs_%s.csv', s$prev, s$set))
    if (!file.exists(cmp_file)) stop('Comparison report not found: ', cmp_file)
    cmp  <- read.csv(cmp_file, stringsAsFactors = FALSE)
    same <- intersect(cmp$DIST_UID[cmp$status == 'same'], names(keys[[s$prev]]))
    k[same] <- keys[[s$prev]][same]
    n_inherited <- length(same)
  }
  keys[[s$set]] <- k
  rows[[s$set]] <- data.frame(set_label = s$set, district_name = d$DISP_L2,
                              dist_uid = d$DIST_UID, shape_key = unname(k[d$DIST_UID]))
  message(sprintf('%s: %d districts, %d unchanged from %s, %d new/changed shapes',
                  s$set, nrow(d), n_inherited, ifelse(is.na(s$prev), '-', s$prev), nrow(d) - n_inherited))
}
all_rows <- do.call(rbind, rows)

# --- write --------------------------------------------------------------------
con <- dbConnect(RPostgres::Postgres(), host = DB_HOST, port = DB_PORT, dbname = DB_NAME,
                 user = 'postgres', password = POSTGRES_PASSWORD, sslmode = 'require')
on.exit(dbDisconnect(con), add = TRUE)

missing_sets <- setdiff(unique(all_rows$set_label),
                        tryCatch(dbGetQuery(con, 'SELECT set_label FROM boundary_sets')$set_label,
                                 error = function(e) character(0)))
if (length(missing_sets)) stop('Not in boundary_sets (run migrate_boundary_sets.R first): ',
                               paste(missing_sets, collapse = ', '))

if (DRY_RUN) {
  message('\nDRY RUN -- nothing written. ', nrow(all_rows), ' rows would be loaded. Set DRY_RUN <- FALSE.')
} else {
  dbBegin(con)
  tryCatch({
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS boundary_set_districts (
        set_label     TEXT NOT NULL REFERENCES boundary_sets(set_label),
        district_name TEXT NOT NULL,
        dist_uid      TEXT NOT NULL,
        shape_key     TEXT NOT NULL,
        PRIMARY KEY (set_label, district_name)
      )")
    for (i in seq_len(nrow(all_rows)))
      dbExecute(con, "
        INSERT INTO boundary_set_districts (set_label, district_name, dist_uid, shape_key)
        VALUES ($1, $2, $3, $4)
        ON CONFLICT (set_label, district_name)
        DO UPDATE SET dist_uid = EXCLUDED.dist_uid, shape_key = EXCLUDED.shape_key",
        unname(as.list(all_rows[i, ])))
    dbCommit(con)
    message('\nDone: ', nrow(all_rows), ' rows loaded into boundary_set_districts.')
  }, error = function(e) {
    try(dbRollback(con), silent = TRUE)
    stop('Load failed and was rolled back: ', conditionMessage(e))
  })
}
