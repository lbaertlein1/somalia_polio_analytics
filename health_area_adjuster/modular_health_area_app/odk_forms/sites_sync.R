# =============================================================================
# sites_sync.R
#
# Shared engine for syncing an external source (IDP master list, Nomad
# Mapping Survey) into the project-36 "sites" entity list.
#
# Rule: LOCAL EDITS WIN. The sync only creates sites, or changes sites it
# still owns -- i.e. last_edited_by starts with "sync:". Once a person edits
# or removes a site (app or form), the sync never touches it again, and a
# removed site is never re-added.
#
# Source records missing from the source are retired by the sync itself
# (status "removed", removal_reason "not_in_source", last_edited_by still
# "sync: ...") so they come back automatically if they reappear.
#
# Needs central_helpers.R sourced first.
# =============================================================================

SYNC_COMPARE_PROPS <- c("site_type", "site_name", "geometry", "region_code", "district_code",
                        "district_name", "u5_population", "last_confirmed_date", "notes", "status",
                        "removal_reason")

# Assign district_code / district_name / region_code by point-in-polygon
# against the app's district boundaries. `pts`: data frame with lat, lon.
assign_districts <- function(pts, districts_path = "data/districts_shp.Rds") {
  d <- readRDS(districts_path) |> sf::st_transform(4326) |> sf::st_make_valid()
  s2_was <- sf::sf_use_s2(); suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(s2_was)))
  p <- sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  hit <- suppressMessages(sf::st_intersects(p, d))
  i <- vapply(hit, function(x) if (length(x)) x[1] else NA_integer_, 1L)
  pts$district_code <- d$DIST_UID[i]
  pts$district_name <- d$district_name[i]
  pts$region_code   <- d$REG_CODE[i]
  pts
}

# `records`: one row per source record, with columns source_id, site_type,
# site_name, lat, lon, district_code, district_name, region_code,
# u5_population, last_confirmed_date, notes. All become strings in Central.
sync_sites_from_source <- function(records, source_key, dry_run = FALSE,
                                   log = function(...) cat(..., "\n"), max_retire_share = 0.2) {
  t0 <- Sys.time()
  stopifnot(!anyDuplicated(records$source_id), !anyNA(records$source_id))
  who <- paste0("sync: ", source_key)
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

  fmt <- function(x) ifelse(is.na(x), "", as.character(x))
  rec <- data.frame(
    source_id = records$source_id, site_type = records$site_type, site_name = fmt(records$site_name),
    geometry = sprintf("%.6f %.6f 0 0", records$lat, records$lon),
    region_code = fmt(records$region_code), district_code = fmt(records$district_code),
    district_name = fmt(records$district_name),
    u5_population = fmt(ifelse(is.na(records$u5_population), NA, round(records$u5_population))),
    last_confirmed_date = fmt(records$last_confirmed_date), notes = fmt(records$notes),
    status = "active", removal_reason = "", stringsAsFactors = FALSE
  )

  ex <- central_entities(MICROPLAN_PROJECT, "sites")
  if (is.null(ex)) ex <- data.frame()
  for (c in c("__id", "source", "source_id", "last_edited_by", SYNC_COMPARE_PROPS)) if (!c %in% names(ex)) ex[[c]] <- character(nrow(ex))
  ex <- ex[ex$source == source_key, , drop = FALSE]
  owned <- startsWith(ex$last_edited_by, "sync:")

  in_src    <- ex$source_id %in% rec$source_id
  to_create <- rec[!rec$source_id %in% ex$source_id, , drop = FALSE]
  locked    <- ex[!owned & in_src, , drop = FALSE]                       # edited by a person -> left alone
  both      <- ex[owned & in_src, , drop = FALSE]
  retire    <- ex[owned & !in_src & ex$status != "removed", , drop = FALSE]

  key <- function(df) do.call(paste, c(df[SYNC_COMPARE_PROPS], sep = "\x1f"))
  new_vals  <- rec[match(both$source_id, rec$source_id), , drop = FALSE]
  changed   <- key(both) != key(new_vals)
  to_update <- new_vals[changed, , drop = FALSE]
  to_update$`__id` <- both$`__id`[changed]

  res <- list(source = source_key, records = nrow(rec), existing = nrow(ex), created = nrow(to_create),
              updated = nrow(to_update), retired = nrow(retire), locally_edited = nrow(locked),
              dry_run = dry_run)
  log(sprintf("[%s] source %d | in registry %d | create %d | update %d | retire %d | locally edited (skipped) %d%s",
              source_key, nrow(rec), nrow(ex), nrow(to_create), nrow(to_update), nrow(retire), nrow(locked),
              if (dry_run) " (dry run)" else ""))

  n_active_owned <- sum(owned & ex$status != "removed")
  if (nrow(retire) > max_retire_share * max(n_active_owned, 1) && n_active_owned > 50) {
    stop("Would retire ", nrow(retire), " of ", n_active_owned, " synced ", source_key,
         " sites -- aborting; check the source.")
  }

  if (!dry_run) {
    payload_data <- function(r) c(as.list(r[1, SYNC_COMPARE_PROPS, drop = FALSE]),
                                  list(last_edited_by = who, last_edited_at = now,
                                       source = source_key, source_id = r$source_id))
    label_of <- function(r) {
      type_lbl <- c(idp = "IDP settlement", nomadic = "Nomadic settlement")[[r$site_type]]
      sprintf("%s - %s (%s)", r$site_name, type_lbl, r$district_name)
    }
    if (nrow(to_create)) {
      central_bulk_create(MICROPLAN_PROJECT, "sites",
                          lapply(seq_len(nrow(to_create)), function(i) {
                            r <- to_create[i, ]; list(label = label_of(r), data = payload_data(r))
                          }), source_name = paste(who, format(t0, "%Y-%m-%d %H:%M")))
    }
    patch <- function(id, body) {
      central_stop_unless(central_call(sprintf("/projects/%d/datasets/sites/entities/%s?force=true",
                                               MICROPLAN_PROJECT, id), "PATCH",
                                       jsonlite::toJSON(body, auto_unbox = TRUE)), what = paste("update", id))
    }
    for (i in seq_len(nrow(to_update))) {
      r <- to_update[i, ]; patch(r$`__id`, list(label = label_of(r), data = payload_data(r)))
    }
    for (i in seq_len(nrow(retire))) {
      patch(retire$`__id`[i], list(data = list(status = "removed", removal_reason = "not_in_source",
                                               last_edited_by = who, last_edited_at = now)))
    }
  }

  res$seconds <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  log(sprintf("[%s] done in %.1fs", source_key, res$seconds))
  res
}
