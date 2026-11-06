# =============================================================================
# sync_sites.R -- command-line wrapper: sync IDP and/or nomadic sites into
# the project-36 "sites" list (see sites_sync.R, idp_source.R, nomad_source.R).
#
#   Rscript odk_forms/sync_sites.R idp|nomad|all [--dry-run] [--idp-url=URL] [--review=FILE.csv]
#
# --review writes the records the sync would load (no PII) to a CSV.
# The IDP URL defaults to the one saved in Admin > Data source URLs is not
# readable here without the DB, so pass --idp-url or set IDP_SETTLEMENTS_URL.
# =============================================================================

if (file.exists(".env")) readRenviron(".env")
suppressMessages(library(dplyr))
for (f in c("central_helpers.R", "sites_sync.R", "idp_source.R", "nomad_source.R")) source(file.path("odk_forms", f))

args    <- commandArgs(TRUE)
what    <- if (length(args) && !startsWith(args[1], "--")) args[1] else "all"
dry_run <- "--dry-run" %in% args
opt     <- function(name) { a <- grep(paste0("^--", name, "="), args, value = TRUE); if (length(a)) sub("^[^=]+=", "", a[1]) else NA }
review  <- opt("review")
idp_url <- opt("idp-url"); if (is.na(idp_url)) idp_url <- Sys.getenv("IDP_SETTLEMENTS_URL",
  "https://services.arcgis.com/5T5nSi527N4F7luB/arcgis/rest/services/IDP_Site_Master_List___10_Nov_2025/FeatureServer/0/query")

out <- list()
if (what %in% c("idp", "all")) {
  recs <- read_idp_master_list(idp_url)
  out$idp <- recs
  invisible(sync_sites_from_source(recs, IDP_SOURCE_KEY, dry_run = dry_run))
}
if (what %in% c("nomad", "all")) {
  recs <- read_nomad_camps(existing_sites = central_entities(MICROPLAN_PROJECT, "sites"))
  out$nomad <- recs
  invisible(sync_sites_from_source(recs, NOMAD_SOURCE_KEY, dry_run = dry_run))
}
if (!is.na(review)) {
  write.csv(dplyr::bind_rows(out, .id = "sync"), review, row.names = FALSE, na = "")
  cat("wrote", review, "\n")
}
