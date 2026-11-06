# =============================================================================
# import_fixed_hc.R
#
# One-off bulk load of data/manual_data/Fixed HC.xlsx (Banadir fixed
# vaccination sites, all at health facilities) into the ODK Central "sites"
# entity list in project 36 "Somalia - Polio SIA Microplanning". Matching is
# against the MHFL in project 31.
#
#   Step 1 (default): clean + match to the live MHFL (master_hf_registry),
#           write odk_forms/review/fixed_hc_matches.csv for review. Nothing
#           is sent to Central.
#   Step 2: Rscript odk_forms/import_fixed_hc.R --upload
#           uploads rows whose match_status is "matched" (after any manual
#           edits to the review CSV -- set match_status / mhfl_id there to
#           override) and skips sites already in the list (same
#           linked_facility_id + site_type), so re-running is safe.
#
# Credentials: ODK_USERNAME / ODK_PASSWORD in the app's .env.
# Run from the app folder.
# =============================================================================

suppressMessages({ library(dplyr); library(stringdist) })
readRenviron(".env")

source("odk_forms/central_helpers.R")

SRC_FILE <- "data/manual_data/Fixed HC.xlsx"
REVIEW   <- "odk_forms/review/fixed_hc_matches.csv"
upload   <- "--upload" %in% commandArgs(TRUE)

# ---- name normalisation ------------------------------------------------------
# Splits a facility name into a facility-kind token (hc / hosp) and a core
# name, so "Kalkaal HC" and "Kalkaal Hospital" stay distinct while
# "Gubadley Health Center" matches "Gubadley H.C.".
.norm <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\bh\\s*c\\b|\\bhealth\\s*cent(er|re)\\b|\\bhealth\\s*post\\b|\\bmch\\b", " hc ", x)
  x <- gsub("\\bhosp(ital)?\\b", " hosp ", x)
  x <- gsub("\\bsc\\b", " ", x)          # "Halane Health Center SC" (stabilisation centre)
  gsub("\\s+", " ", trimws(x))
}
.kind <- function(n) ifelse(grepl("\\bhosp\\b", n), "hosp", ifelse(grepl("\\bhc\\b", n), "hc", "other"))
.core <- function(n) {
  n <- gsub("\\bjanaral\\b", "general", n)   # Somali spelling of "General" (Daud)
  n <- gsub("\\b(hc|hosp|district|community|general|regional|private|specialist|speciali[sz]ed|national|mother and child)\\b", " ", n)
  gsub("(.)\\1+", "\\1", gsub("\\s+", "", n))   # also collapse doubled letters (Dayniile/Deyniile)
}

# ---- 1. read + clean ---------------------------------------------------------
src <- readxl::read_excel(SRC_FILE, col_types = "text")
src <- setNames(src[, 1:3], c("sn", "district_raw", "site_name")) |>   # headers carry stray spaces
  tidyr::fill(district_raw) |>
  mutate(site_name = gsub("\\s+", " ", trimws(site_name)),
         site_name = gsub("\\b(hc|Hc)\\b", "HC", site_name),
         district_raw = trimws(district_raw))

# District -> DIST_UID, matched on official + alternative names within Banadir.
dist <- sf::st_drop_geometry(readRDS("data/districts_shp.Rds")) |> filter(REG_CODE == "BAN")
dist_names <- do.call(rbind, lapply(seq_len(nrow(dist)), function(i) {
  nm <- c(dist$district_name[i], dist$NAME_L2[i], trimws(strsplit(dist$ALTNAME_L2[i] %||% "", ";")[[1]]))
  data.frame(DIST_UID = dist$DIST_UID[i], district_name = dist$district_name[i], alias = nm[nzchar(nm)])
}))
dist_key <- function(x) gsub("(.)\\1+", "\\1", gsub("[^a-z]", "", tolower(x)))
dm <- unique(src$district_raw)
d_score <- stringdistmatrix(dist_key(dm), dist_key(dist_names$alias), method = "jw", p = 0.1)
d_best  <- apply(d_score, 1, which.min)
district_map <- data.frame(district_raw = dm, DIST_UID = dist_names$DIST_UID[d_best],
                           district_name = dist_names$district_name[d_best],
                           district_dist = round(apply(d_score, 1, min), 3))
print(district_map, row.names = FALSE)
if (any(district_map$district_dist > 0.15)) stop("Unclear district match -- check the table above.")

src <- left_join(src, district_map, by = "district_raw")

# ---- 2. match to live MHFL -----------------------------------------------------
# Matched against project 31 (the source of truth), so mhfl_id is the
# ORIGINAL MHFL UUID -- the same value the forms store via the mirror's
# mhfl_uuid property.
mhfl <- central_entities(MHFL_PROJECT, "master_hf_registry") |>
  filter(grepl("^BAN-", district_code)) |>
  transmute(mhfl_id = `__id`, mhfl_name = facility_name, mhfl_alt = as.character(facility_name_alt),
            mhfl_type = facility_type, mhfl_district = district_code,
            lat = as.numeric(gps_latitude), lon = as.numeric(gps_longitude))

cand <- bind_rows(
  mhfl |> mutate(nm = mhfl_name),
  mhfl |> filter(!is.na(mhfl_alt), nzchar(mhfl_alt)) |> mutate(nm = mhfl_alt)
) |> mutate(n = .norm(nm), kind = .kind(n), core = .core(n))

src <- src |> mutate(n = .norm(site_name), kind = .kind(n), core = .core(n))

best_match <- function(i) {
  s <- src[i, ]
  sc <- cand |>
    mutate(dist = stringdist(s$core, core, method = "jw", p = 0.1),
           # one name is the other plus a suffix ("Kalkaal Hospital" vs
           # "Kalkaal Hospital Darjiinka") -- close, but not exact
           dist = ifelse(nchar(s$core) >= 4 & (startsWith(core, s$core) | startsWith(s$core, core)),
                         pmin(dist, 0.05), dist),
           same_district = mhfl_district == s$DIST_UID,
           kind_ok = kind == s$kind | s$kind == "other" | kind == "other",
           # same district and compatible kind preferred; small penalties otherwise
           score = dist + ifelse(same_district, 0, 0.08) + ifelse(kind_ok, 0, 0.15)) |>
    arrange(score) |> distinct(mhfl_id, .keep_all = TRUE)
  b <- sc[1, ]; runner <- sc[2, ]
  status <- if (b$score <= 0.08 && b$kind_ok) "matched" else if (b$score <= 0.22) "review" else "unmatched"
  data.frame(mhfl_id = b$mhfl_id, mhfl_name = b$mhfl_name, mhfl_type = b$mhfl_type,
             mhfl_district = b$mhfl_district, lat = b$lat, lon = b$lon,
             score = round(b$score, 3),
             next_best = sprintf("%s [%s] %.3f", runner$mhfl_name, runner$mhfl_district, runner$score),
             match_status = status)
}

if (!upload) {
  res <- bind_cols(src |> select(sn, district_raw, DIST_UID, district_name, site_name),
                   do.call(rbind, lapply(seq_len(nrow(src)), best_match)))
  # Two source rows claiming the same facility -> both to review.
  claimed <- ifelse(res$match_status == "unmatched", NA, res$mhfl_id)
  dup <- !is.na(claimed) & (claimed %in% claimed[duplicated(claimed, incomparables = NA)])
  res$match_status[dup] <- "review"
  res$note <- ifelse(dup, "same MHFL facility as another row", "")
  res$note <- ifelse(res$mhfl_district != res$DIST_UID & res$match_status != "unmatched",
                     trimws(paste(res$note, "MHFL district differs")), res$note)
  dir.create(dirname(REVIEW), showWarnings = FALSE)
  write.csv(res, REVIEW, row.names = FALSE, na = "")
  cat("\nwrote", REVIEW, "\n"); print(table(res$match_status))
  quit(save = "no")
}

# ---- 3. upload (--upload) --------------------------------------------------------
res <- read.csv(REVIEW, stringsAsFactors = FALSE) |> filter(match_status == "matched")
mhfl_by_id <- mhfl |> select(mhfl_id, lat, lon, mhfl_district)
res <- res |> select(-lat, -lon, -mhfl_district) |> left_join(mhfl_by_id, by = "mhfl_id")
stopifnot(!anyNA(res$lat), !anyDuplicated(res$mhfl_id))

central_ensure_dataset(MICROPLAN_PROJECT, "sites", SITE_PROPS)
existing <- central_entities(MICROPLAN_PROJECT, "sites")
if (!is.null(existing) && nrow(existing) > 0) {
  done <- existing$linked_facility_id[existing$site_type == "fixed"]
  res <- res |> filter(!mhfl_id %in% done)
}
cat(nrow(res), "sites to upload\n"); if (nrow(res) == 0) quit(save = "no")

now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
entities <- lapply(seq_len(nrow(res)), function(i) {
  r <- res[i, ]
  list(label = sprintf("%s - Fixed vaccination site (%s)", r$site_name, r$district_name),
       data = list(
         region_code = "BAN", district_code = r$DIST_UID, district_name = r$district_name,
         site_type = "fixed", site_subtype = "fixed_health_facility", mhfl_match = "listed",
         linked_facility_id = r$mhfl_id, site_name = r$site_name,
         geometry = sprintf("%.6f %.6f 0 0", r$lat, r$lon), status = "active",
         notes = "", last_edited_by = "bulk import (Fixed HC.xlsx)", last_edited_at = now,
         source = "bulk_fixed_hc_xlsx", source_id = as.character(r$sn)))
})
central_bulk_create(MICROPLAN_PROJECT, "sites", entities, source_name = "Fixed HC.xlsx")
cat("uploaded", length(entities), "sites to project", MICROPLAN_PROJECT, "\n")
