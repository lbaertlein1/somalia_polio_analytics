# =============================================================================
# sync_mhfl_mirror.R -- command-line wrapper around sync_mhfl_mirror()
# (odk_forms/mhfl_mirror.R). The Admin panel's "Update facilities now"
# button runs the same function; use this for a cron job on the VM if
# in-app runs ever get too slow.
#
#   Rscript odk_forms/sync_mhfl_mirror.R [--dry-run]     (from the app folder)
#
# Example crontab line (nightly 02:00):
#   0 2 * * * cd /path/to/modular_health_area_app && Rscript odk_forms/sync_mhfl_mirror.R >> mhfl_sync.log 2>&1
# =============================================================================

if (file.exists(".env")) readRenviron(".env")
source("odk_forms/central_helpers.R")
source("odk_forms/mhfl_mirror.R")

invisible(sync_mhfl_mirror(dry_run = "--dry-run" %in% commandArgs(TRUE)))
