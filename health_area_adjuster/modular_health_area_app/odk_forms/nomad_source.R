# =============================================================================
# nomad_source.R
#
# read_nomad_camps(): Nomad Mapping Survey (ODK project 22, form
# nomad_mapping_survey_gedo) cleaned and clustered into nomadic CAMPS -- one
# registry site per place, not per group -- as records for
# sync_sites_from_source() (sites_sync.R).
#
#   1. Clean: group_code normalised (case, separators); rejected submissions
#      dropped; U5 = 0-11 + 12-59 months (NA if it exceeds the total).
#      Contact/PII fields are never read.
#   2. Each submission = one camp sighting. group_code is NOT an identity:
#      it is a clan/lineage code shared by many camps (e.g. ELWAK-GR-002 on
#      160 submissions, 122 settlement names) and spelled inconsistently.
#   3. Present today: recorded within 12 months AND arrival <= today <=
#      expected departure. Departure > 2 years ahead or before arrival =
#      unknown -> present if recorded within the last 6 months.
#   4. Present sightings within NOMAD_CLUSTER_M (500 m, single linkage) form
#      one camp: centroid location, most common settlement name, U5 summed
#      over the group codes there (each code once, at its latest visit).
#   5. Stable IDs: a camp within 500 m of an existing nomadic site keeps that
#      site's source_id. If that site is not sync-owned (entered or edited by
#      a person) the camp is dropped -- the person's site already covers it.
#      Otherwise a new source_id is minted.
#   6. District by point-in-polygon on the app's district boundaries.
# =============================================================================

NOMAD_SOURCE_KEY <- "nomad_mapping_survey"
NOMAD_PROJECT    <- 22L
NOMAD_FORM       <- "nomad_mapping_survey_gedo"
NOMAD_CLUSTER_M  <- 500

read_nomad_survey <- function() {
  sel <- paste(c("__id", "__system/submissionDate", "__system/reviewState", "group_profile/today_date",
                 "community_level/group/group_code", "community_level/movt_type", "community_level/gps",
                 "community_level/camp_settlement_name", "community_level/camp_first_month",
                 "community_level/camp_last_month", "polio/camp_tot_pop",
                 "polio/camp_pop_0to11_months", "polio/camp_pop_12to59_months"), collapse = ",")
  rows <- list(); skip <- 0
  repeat {
    r <- central_call(sprintf("/projects/%d/forms/%s.svc/Submissions?%%24select=%s&%%24top=1000&%%24skip=%d",
                              NOMAD_PROJECT, NOMAD_FORM, utils::URLencode(sel, reserved = TRUE), skip))
    central_stop_unless(r, what = "nomad survey OData")
    v <- jsonlite::fromJSON(r$body, flatten = TRUE)$value
    if (length(v) == 0 || nrow(v) == 0) break
    rows[[length(rows) + 1]] <- v; skip <- skip + 1000
  }
  d <- dplyr::bind_rows(rows)
  names(d) <- sub("^.*[.]", "", names(d))
  d
}

read_nomad_camps <- function(existing_sites = NULL, today = Sys.Date(), log = function(...) cat(..., "\n")) {
  d <- read_nomad_survey()
  log(sprintf("[nomad] %d submissions", nrow(d)))

  # ---- 1. clean ----------------------------------------------------------------
  d <- d[is.na(d$reviewState) | d$reviewState != "rejected", , drop = FALSE]
  # Test submissions (e.g. a "Test" camp in Hodan with codes from Baidoa,
  # Belet Weyne and Dolow).
  is_test <- grepl("\\btest", tolower(paste(d$camp_settlement_name, d$group_code)))
  if (any(is_test)) log(sprintf("[nomad] dropped %d test submissions", sum(is_test)))
  d <- d[!is_test, , drop = FALSE]
  d$group <- gsub("^-+|-+$", "", gsub("[^A-Z0-9]+", "-", toupper(trimws(d$group_code))))
  d$lon <- vapply(d$coordinates, function(x) if (length(x) >= 2) x[[1]] else NA_real_, 0)
  d$lat <- vapply(d$coordinates, function(x) if (length(x) >= 2) x[[2]] else NA_real_, 0)
  d$rec_date <- as.Date(d$today_date)
  d$first    <- as.Date(d$camp_first_month)
  d$last     <- as.Date(d$camp_last_month)
  u5 <- d$camp_pop_0to11_months + d$camp_pop_12to59_months
  d$u5 <- ifelse(!is.na(u5) & !is.na(d$camp_tot_pop) & u5 > d$camp_tot_pop, NA, u5)
  d <- d[!is.na(d$lat) & !is.na(d$lon) & d$lat != 0 & !is.na(d$rec_date), , drop = FALSE]
  d$group[!nzchar(d$group)] <- paste0("UNCODED-", d$`__id`[!nzchar(d$group)])
  # Training submissions: GPS in Mogadishu (Banadir) from enumerators of
  # Gedo / Hiraan / Bay districts, 30 Jul - 1 Aug 2026. The survey does not
  # operate in Banadir, so any point there is training/test data.
  in_banadir <- assign_districts(d[, c("lat", "lon")])$region_code %in% "BAN"
  if (any(in_banadir)) log(sprintf("[nomad] dropped %d submissions with GPS in Banadir (training)", sum(in_banadir)))
  d <- d[!in_banadir, , drop = FALSE]

  # ---- 2-3. each submission = one camp sighting; keep those present today --------
  # Group codes are clan/lineage codes shared by many camps (and spelled
  # inconsistently), so they are NOT used as identities -- only as notes.
  plausible <- !is.na(d$first) & !is.na(d$last) & d$last >= d$first & d$last <= today + 730
  recent12  <- d$rec_date >= today - 365
  present   <- recent12 & ifelse(plausible, d$first <= today & today <= d$last, d$rec_date >= today - 182)
  present[is.na(present)] <- FALSE
  log(sprintf("[nomad] %d submissions describe a camp present today (%d with unusable departure date)",
              sum(present), sum(present & !plausible)))
  g <- d[present, , drop = FALSE]
  if (nrow(g) == 0) return(data.frame())

  # ---- 4. cluster sightings into camps ------------------------------------------------
  pts <- sf::st_as_sf(g, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |> sf::st_transform(32638)
  xy  <- sf::st_coordinates(pts)
  g$camp <- if (nrow(g) > 1) stats::cutree(stats::hclust(stats::dist(xy), method = "single"), h = NOMAD_CLUSTER_M) else 1L

  camps <- do.call(rbind, lapply(split(g, g$camp), function(c) {
    # Repeat visits: within a camp, a group code counts once (its latest visit).
    c <- c[order(c$rec_date, c$submissionDate, decreasing = TRUE), , drop = FALSE]
    c <- c[!duplicated(c$group), , drop = FALSE]
    nm <- trimws(c$camp_settlement_name); nm <- nm[!is.na(nm) & nzchar(nm)]
    name <- if (length(nm)) names(sort(table(tools::toTitleCase(tolower(nm))), decreasing = TRUE))[1] else "Nomadic camp"
    codes <- sort(unique(c$group[!startsWith(c$group, "UNCODED-")]))
    mv <- sort(unique(stats::na.omit(c$movt_type)))
    data.frame(
      site_name = name, lat = mean(c$lat), lon = mean(c$lon),
      u5_population = if (all(is.na(c$u5))) NA else sum(c$u5, na.rm = TRUE),
      last_confirmed_date = format(max(c$rec_date), "%Y-%m-%d"),
      notes = sprintf("%d nomadic group(s)%s%s. Type of movement (survey): %s. Expected to stay until %s. Nomad Mapping Survey.",
                      nrow(c), if (length(codes)) paste0(": ", paste(utils::head(codes, 6), collapse = ", ")) else "",
                      if (length(codes) > 6) sprintf(" +%d more", length(codes) - 6) else "",
                      if (length(mv)) paste(tools::toTitleCase(mv), collapse = "/") else "unknown",
                      if (any(!is.na(c$last) & c$last <= today + 730)) format(max(c$last[c$last <= today + 730], na.rm = TRUE), "%d %b %Y") else "unknown"),
      stringsAsFactors = FALSE)
  }))
  log(sprintf("[nomad] %d camps from %d present sightings", nrow(camps), nrow(g)))

  # ---- 5. stable IDs: match to existing nomadic sites within 500 m ------------------
  camps$source_id <- NA_character_
  keep <- rep(TRUE, nrow(camps))
  ex <- existing_sites
  if (!is.null(ex) && nrow(ex) > 0) ex <- ex[ex$site_type == "nomadic" & nzchar(ex$geometry), , drop = FALSE]
  if (!is.null(ex) && nrow(ex) > 0) {
    eg <- do.call(rbind, strsplit(ex$geometry, " "))
    ex_xy <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(
      data.frame(lat = as.numeric(eg[, 1]), lon = as.numeric(eg[, 2])), coords = c("lon", "lat"), crs = 4326), 32638))
    c_xy <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(camps, coords = c("lon", "lat"), crs = 4326), 32638))
    dm <- sqrt(outer(c_xy[, 1], ex_xy[, 1], "-")^2 + outer(c_xy[, 2], ex_xy[, 2], "-")^2)
    pairs <- which(dm <= NOMAD_CLUSTER_M, arr.ind = TRUE)
    pairs <- pairs[order(dm[pairs]), , drop = FALSE]
    used_c <- used_e <- integer(0)
    for (k in seq_len(nrow(pairs))) {
      ci <- pairs[k, 1]; ei <- pairs[k, 2]
      if (ci %in% used_c || ei %in% used_e) next
      used_c <- c(used_c, ci); used_e <- c(used_e, ei)
      if (ex$source[ei] == NOMAD_SOURCE_KEY && startsWith(ex$last_edited_by[ei], "sync:")) {
        camps$source_id[ci] <- ex$source_id[ei]
      } else if (ex$source[ei] == NOMAD_SOURCE_KEY) {
        camps$source_id[ci] <- ex$source_id[ei]       # locally edited synced site: engine skips it
      } else {
        keep[ci] <- FALSE                             # a person's own site already covers this camp
      }
    }
    if (any(!keep)) log(sprintf("[nomad] %d camps already covered by sites entered by people", sum(!keep)))
  }
  camps <- camps[keep, , drop = FALSE]
  taken <- if (!is.null(existing_sites)) existing_sites$source_id else character(0)
  for (i in which(is.na(camps$source_id))) {
    repeat {
      id <- paste0("nomad-", paste(sample(c(0:9, letters[1:6]), 10, TRUE), collapse = ""))
      if (!id %in% c(taken, camps$source_id)) break
    }
    camps$source_id[i] <- id
  }

  # ---- 6. district ---------------------------------------------------------------------
  camps$site_type <- "nomadic"
  camps <- assign_districts(camps)
  outside <- is.na(camps$district_code)
  if (any(outside)) log(sprintf("[nomad] skipped %d camps outside all district boundaries", sum(outside)))
  camps[!outside, , drop = FALSE]
}
