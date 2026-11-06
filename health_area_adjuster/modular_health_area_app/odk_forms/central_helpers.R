# =============================================================================
# central_helpers.R
#
# Minimal ODK Central REST helpers shared by the site-registry scripts
# (sync_mhfl_mirror.R, import_*.R).
#
# Requests go through the curl CLI rather than httr2: on this server httr2
# request bodies arrived empty (Central then stores blank forms / rejects
# entity JSON). Every curl argument is quoted -- system2 does not quote, so
# "Content-Type: application/json" would otherwise split at the space.
#
# Credentials: ODK_USERNAME / ODK_PASSWORD (app .env).
# =============================================================================

CENTRAL_URL       <- "https://emro.nafundi.com/v1"
MHFL_PROJECT      <- 31L   # "Somalia - Master HF Registry" (source of truth for facilities)
MICROPLAN_PROJECT <- 36L   # "Somalia - Polio SIA Microplanning" (sites + MHFL mirror)

# Properties of the "sites" entity list (matches build_site_forms.R's save_to
# columns, plus source_id for bulk loads).
SITE_PROPS <- c("region_code", "district_code", "district_name", "site_type", "site_subtype",
                "subtype_other", "mhfl_match", "linked_facility_id", "site_name", "geometry",
                "status", "removal_reason", "u5_population", "last_confirmed_date", "crossing_name",
                "neighbouring_country", "notes", "last_edited_by", "last_edited_at",
                "source", "source_id")

central_call <- function(path, method = "GET", json = NULL, body_file = NULL,
                         content_type = "application/json") {
  # Response body goes to a file and is read back as raw UTF-8 bytes: piping
  # it through system2's stdout mangles non-ASCII text on Windows.
  out_file <- tempfile(); on.exit(unlink(out_file), add = TRUE)
  args <- c("-s", "-u", paste0(Sys.getenv("ODK_USERNAME"), ":", Sys.getenv("ODK_PASSWORD")),
            "-X", method, "-o", out_file, "-w", "%{http_code}")
  if (!is.null(json)) {
    body_file <- tempfile(fileext = ".json"); on.exit(unlink(body_file), add = TRUE)
    writeLines(json, body_file, useBytes = TRUE)
  }
  if (!is.null(body_file)) {
    args <- c(args, "-H", paste("Content-Type:", content_type), "--data-binary", paste0("@", body_file))
  }
  quote_type <- if (.Platform$OS.type == "windows") "cmd" else "sh"   # sh: single quotes, so a "$" in the password survives
  out <- system2("curl", shQuote(c(args, paste0(CENTRAL_URL, path)), type = quote_type), stdout = TRUE)
  body <- if (file.exists(out_file) && file.size(out_file) > 0) {
    rawToChar(readBin(out_file, "raw", file.size(out_file)))
  } else ""
  Encoding(body) <- "UTF-8"
  list(status = as.integer(utils::tail(out, 1)), body = body)
}

central_stop_unless <- function(r, ok = 200, what = "") {
  if (!r$status %in% ok) stop(what, " -> HTTP ", r$status, ": ", substr(r$body, 1, 500), call. = FALSE)
  invisible(r)
}

central_json <- function(path) {
  r <- central_stop_unless(central_call(path), what = paste("GET", path))
  jsonlite::fromJSON(r$body, simplifyVector = FALSE)
}

# All entities of a list as a character data frame ("" for empty, never NA).
central_entities <- function(project, dataset) {
  r <- central_call(sprintf("/projects/%d/datasets/%s/entities.csv", project, dataset))
  if (r$status == 404) return(NULL)
  central_stop_unless(r, what = paste("entities", dataset))
  read.csv(text = r$body, check.names = FALSE, colClasses = "character",
           na.strings = character(0), stringsAsFactors = FALSE)
}

# Create the entity list and any missing properties (idempotent).
central_ensure_dataset <- function(project, dataset, properties) {
  r <- central_call(sprintf("/projects/%d/datasets/%s", project, dataset))
  if (r$status == 404) {
    central_stop_unless(central_call(sprintf("/projects/%d/datasets", project), "POST",
                                     jsonlite::toJSON(list(name = dataset), auto_unbox = TRUE)),
                        what = paste("create dataset", dataset))
    have <- character(0)
  } else {
    central_stop_unless(r, what = paste("dataset", dataset))
    have <- vapply(jsonlite::fromJSON(r$body, simplifyVector = FALSE)$properties,
                   function(p) p$name, "")
  }
  for (p in setdiff(properties, have)) {
    central_stop_unless(central_call(sprintf("/projects/%d/datasets/%s/properties", project, dataset), "POST",
                                     jsonlite::toJSON(list(name = p), auto_unbox = TRUE)),
                        what = paste("add property", p))
  }
  invisible(setdiff(properties, have))
}

# Bulk-create entities in chunks. `entities`: list of list(uuid=, label=, data=list(...)).
central_bulk_create <- function(project, dataset, entities, source_name, chunk = 500) {
  for (i in seq(1, length(entities), by = chunk)) {
    part <- entities[i:min(i + chunk - 1, length(entities))]
    body <- jsonlite::toJSON(list(source = list(name = source_name, size = length(part)),
                                  entities = part), auto_unbox = TRUE, null = "null")
    central_stop_unless(central_call(sprintf("/projects/%d/datasets/%s/entities", project, dataset),
                                     "POST", body), what = "bulk create")
  }
  invisible(length(entities))
}
