# =============================================================================
# mhfl_mirror.R
#
# sync_mhfl_mirror(): one-way mirror of the MHFL (entity list
# master_hf_registry) from project 31 "Somalia - Master HF Registry" into
# project 36 "Somalia - Polio SIA Microplanning", so the site forms there can
# pick facilities from a live entity list without project-36 users needing
# access to project 31.
#
#   - same entity list name, so the forms' master_hf_registry.csv links to it
#   - entity UUIDs are unique server-wide, so mirror entities get their own
#     UUIDs; each carries the project-31 UUID in property `mhfl_uuid`. The
#     site forms store THAT value as linked_facility_id, so a site's facility
#     link means the same facility in both projects.
#   - new MHFL properties are added to the mirror automatically
#   - new facilities created, changed ones overwritten, removed ones deleted
#
# The mirror is READ-ONLY by convention: no project-36 form may update it --
# any such edit is overwritten on the next run. Facility corrections go
# through project 31's own forms.
#
# Called from the Admin panel ("Update facilities now") and from the CLI
# wrapper sync_mhfl_mirror.R (for a cron job if in-app runs get slow).
# Needs central_helpers.R sourced first.
# =============================================================================

sync_mhfl_mirror <- function(dry_run = FALSE, log = function(...) cat(..., "\n")) {
  DATASET <- "master_hf_registry"
  t0 <- Sys.time()

  # ---- source --------------------------------------------------------------
  src_meta  <- central_json(sprintf("/projects/%d/datasets/%s", MHFL_PROJECT, DATASET))
  src_props <- vapply(src_meta$properties, function(p) p$name, "")
  src <- central_entities(MHFL_PROJECT, DATASET)
  if (is.null(src) || nrow(src) == 0) stop("Source MHFL returned no entities -- refusing to sync (would empty the mirror).")
  src$mhfl_uuid <- src$`__id`
  mirror_props <- c(src_props, "mhfl_uuid")

  # ---- target --------------------------------------------------------------
  added_props <- if (!dry_run) central_ensure_dataset(MICROPLAN_PROJECT, DATASET, mirror_props) else character(0)
  if (length(added_props)) log("added properties:", paste(added_props, collapse = ", "))
  tgt <- central_entities(MICROPLAN_PROJECT, DATASET)
  if (is.null(tgt) || nrow(tgt) == 0 || !"mhfl_uuid" %in% names(tgt)) {
    tgt <- data.frame(`__id` = character(0), mhfl_uuid = character(0), label = character(0), check.names = FALSE)
  }

  # Compare on label + every source property, keyed by the project-31 UUID
  # (missing target column = "").
  fingerprint <- function(df) {
    if (nrow(df) == 0) return(setNames(character(0), character(0)))
    cols <- c("label", src_props)
    for (c in setdiff(cols, names(df))) df[[c]] <- ""
    setNames(do.call(paste, c(df[cols], sep = "\x1f")), df$mhfl_uuid)
  }
  src_fp <- fingerprint(src); tgt_fp <- fingerprint(tgt)
  tgt_id <- setNames(tgt$`__id`, tgt$mhfl_uuid)   # project-31 UUID -> mirror UUID

  to_create <- setdiff(names(src_fp), names(tgt_fp))
  to_delete <- setdiff(names(tgt_fp), names(src_fp))
  common    <- intersect(names(src_fp), names(tgt_fp))
  to_update <- common[src_fp[common] != tgt_fp[common]]
  dupes     <- tgt$`__id`[duplicated(tgt$mhfl_uuid)]   # e.g. from an interrupted run

  res <- list(source = nrow(src), mirror_before = nrow(tgt), created = length(to_create),
              updated = length(to_update), deleted = length(to_delete) + length(dupes),
              dry_run = dry_run)
  log(sprintf("source %d | mirror %d | create %d | update %d | delete %d | duplicates %d%s",
              nrow(src), nrow(tgt), length(to_create), length(to_update), length(to_delete),
              length(dupes), if (dry_run) " (dry run)" else ""))

  # Guard: a sudden mass deletion more likely means a broken source read.
  if (length(to_delete) > 0.2 * max(nrow(tgt), 1) && nrow(tgt) > 50) {
    stop("Would delete ", length(to_delete), " of ", nrow(tgt), " mirrored facilities -- aborting; check the source.")
  }

  if (!dry_run) {
    entity_payload <- function(key) {
      row <- src[src$mhfl_uuid == key, , drop = FALSE]
      list(label = row$label, data = as.list(row[1, mirror_props, drop = FALSE]))
    }
    entity_path <- function(id) sprintf("/projects/%d/datasets/%s/entities/%s", MICROPLAN_PROJECT, DATASET, id)

    if (length(to_create)) {
      central_bulk_create(MICROPLAN_PROJECT, DATASET, lapply(to_create, entity_payload),
                          source_name = paste("MHFL mirror sync", format(t0, "%Y-%m-%d %H:%M")))
    }
    for (key in to_update) {
      central_stop_unless(central_call(paste0(entity_path(tgt_id[[key]]), "?force=true"), "PATCH",
                                       jsonlite::toJSON(entity_payload(key), auto_unbox = TRUE)),
                          what = paste("update", key))
    }
    for (id in c(unname(tgt_id[to_delete]), dupes)) {
      central_stop_unless(central_call(entity_path(id), "DELETE"), what = paste("delete", id))
    }
  }

  res$seconds <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  log(sprintf("done in %.1fs", res$seconds))
  res
}
