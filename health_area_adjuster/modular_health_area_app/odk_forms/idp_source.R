# =============================================================================
# idp_source.R
#
# read_idp_master_list(url): IDP Site Master List (ArcGIS FeatureServer
# layer -- the URL set in Admin > Data source URLs, idp_settlements_url) as
# records for sync_sites_from_source() (sites_sync.R).
#
#   source_id      Temporary_ (cons_ID_...), falling back to OCHA CCCM / DTM ID
#   site_name      Settlement
#   u5_population  Harmonised individuals x IDP_U5_SHARE (0.20, WorldPop
#                  national U5 share 20.4%) -- the list has no U5 figure
#   last_confirmed Data_sourc date (e.g. 01-Jun-24), else 1st of Month_Year
#   district       point-in-polygon on the app's district boundaries
#   notes          location type (Urban/Rural), source, individuals, IDs
# =============================================================================

IDP_SOURCE_KEY <- "idp_master_list"
IDP_U5_SHARE   <- 0.20

read_idp_master_list <- function(url, log = function(...) cat(..., "\n")) {
  layer <- sub("/query/?$", "", url)
  rows <- list(); offset <- 0
  repeat {
    resp <- httr::GET(paste0(layer, "/query"), httr::timeout(60), query = list(
      where = "1=1", outFields = "*", returnGeometry = "false", f = "json",
      orderByFields = "FID", resultOffset = offset, resultRecordCount = 1000))
    httr::stop_for_status(resp)
    js <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
    if (!is.null(js$error)) stop("ArcGIS error: ", js$error$message)
    a <- js$features$attributes
    if (is.null(a) || nrow(a) == 0) break
    rows[[length(rows) + 1]] <- a; offset <- offset + nrow(a)
    if (!isTRUE(js$exceededTransferLimit)) break
  }
  x <- do.call(rbind, rows)
  log(sprintf("[idp] %d records from master list", nrow(x)))

  blank <- function(v) is.na(v) | !nzchar(trimws(v))
  sid <- ifelse(!blank(x$Temporary_), trimws(x$Temporary_),
                ifelse(!blank(x$OCHA_CCCM_), trimws(x$OCHA_CCCM_), trimws(x$DTM_ID)))

  dt <- as.Date(x$Data_sourc, format = "%d-%b-%y")
  dt2 <- as.Date(paste0("01-", x$Month_Year), format = "%d-%b-%y")
  dt[is.na(dt)] <- dt2[is.na(dt)]

  indiv <- suppressWarnings(as.numeric(x$Harmonised))
  ids <- trimws(paste(ifelse(blank(x$OCHA_CCCM_), "", x$OCHA_CCCM_), ifelse(blank(x$DTM_ID), "", x$DTM_ID)))
  notes <- paste0(
    ifelse(blank(x$Location_t), "", paste0(trimws(x$Location_t), ". ")),
    "IDP master list: ", trimws(x$Source),
    ifelse(is.na(indiv), "", sprintf("; %s individuals (U5 = 20%%)", format(indiv, big.mark = ","))),
    ifelse(is.na(dt), "", paste0("; data ", format(dt, "%b %Y"))),
    ifelse(nzchar(ids), paste0("; ", gsub("\\s+", " / ", ids)), ""))

  out <- data.frame(
    source_id = sid, site_type = "idp",
    site_name = ifelse(blank(x$Settlement), x$Settleme_1, trimws(x$Settlement)),
    lat = suppressWarnings(as.numeric(x$GPS_Latitu)), lon = suppressWarnings(as.numeric(x$GPS_Longit)),
    u5_population = round(indiv * IDP_U5_SHARE),
    last_confirmed_date = ifelse(is.na(dt), NA, format(dt, "%Y-%m-%d")),
    notes = notes, stringsAsFactors = FALSE
  )

  # Drop unusable rows, reporting each reason.
  bad_gps <- is.na(out$lat) | is.na(out$lon) | out$lat == 0 | out$lon == 0
  no_id   <- blank(out$source_id)
  dup_id  <- duplicated(out$source_id) & !no_id
  if (any(bad_gps)) log(sprintf("[idp] skipped %d without usable GPS", sum(bad_gps)))
  if (any(no_id))   log(sprintf("[idp] skipped %d without any site ID", sum(no_id & !bad_gps)))
  if (any(dup_id))  log(sprintf("[idp] skipped %d duplicate site IDs (kept first)", sum(dup_id & !bad_gps & !no_id)))
  out <- out[!(bad_gps | no_id | dup_id), , drop = FALSE]

  out <- assign_districts(out)
  outside <- is.na(out$district_code)
  if (any(outside)) log(sprintf("[idp] skipped %d outside all district boundaries", sum(outside)))
  out[!outside, , drop = FALSE]
}
