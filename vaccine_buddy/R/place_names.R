# =============================================================================
# place_names.R -- local place names for describing missed places
#
# Health facility and IDP site names are often not what people on the
# ground navigate by. 3rd-party campaign monitoring (ODK project 11: LQAS
# and PCIM forms, 2020 onwards) records, for every monitored cluster, a GPS
# point (location_cl) and the village / place name the monitor wrote down
# there -- mosques, Quran schools, shops, local area names. Those are
# stored in vb_place_names and used first when labelling a missed place
# ("150 m N of Daaru-taqwa").
#
# place_names_sync() runs with every Vaccine Buddy update (odk_sync()). It
# re-pulls a monitoring form only when ODK reports new submissions for it
# (vb_place_sources keeps each form's last_submission), so after the first
# run it costs one form-list request.
# =============================================================================

PLACES_PID <- function() as.integer(Sys.getenv('ODK_PLACES_PID', '11'))

# Names that aren't places
.junk_name <- function(x) {
  y <- tolower(trimws(x))
  is.na(x) | nchar(gsub('[^[:alpha:]]', '', y)) < 3 |
    y %in% c('n/a', 'na', 'none', 'unknown', 'test', 'no', 'yes', 'other', 'village', 'settlement') |
    grepl('^#|^[0-9[:punct:] ]+$', y)
}

#' One monitoring form's named points: data.frame(source_ref, name, lon,
#' lat, observed_at), or NULL if the form has no cluster GPS + place name.
.form_place_names <- function(fid) {
  d <- ruODK::odata_submission_get(pid = PLACES_PID(), fid = fid, wkt = FALSE, download = FALSE, verbose = FALSE)
  if (nrow(d) == 0) return(NULL)
  lon_c <- grep('location_cl_(longitude|coordinates_1)$', names(d), value = TRUE)[1]
  if (is.na(lon_c)) return(NULL)
  lat_c <- sub('longitude$', 'latitude', sub('coordinates_1$', 'coordinates_2', lon_c))
  name_cs <- grep('village_name_begin$|village_name_end$|village_name$', names(d), value = TRUE)
  if (length(name_cs) == 0) return(NULL)
  when <- if ('submission_time' %in% names(d)) d$submission_time else as.POSIXct(NA)
  out <- do.call(rbind, lapply(name_cs, function(nc) data.frame(
    source_ref  = paste(d$id, nc, sep = '|'),
    name        = trimws(gsub('[[:space:]]+', ' ', as.character(d[[nc]]))),
    lon         = suppressWarnings(as.numeric(d[[lon_c]])),
    lat         = suppressWarnings(as.numeric(d[[lat_c]])),
    observed_at = as.POSIXct(when),
    stringsAsFactors = FALSE)))
  b <- SOMALIA_BBOX
  out[!.junk_name(out$name) & !is.na(out$lon) & !is.na(out$lat) &
        out$lon >= b['xmin'] & out$lon <= b['xmax'] & out$lat >= b['ymin'] & out$lat <= b['ymax'], ]
}

#' Bring vb_place_names up to date with project 11. Returns a one-line
#' summary; errors in one form don't stop the others.
place_names_sync <- function(con) {
  forms <- ruODK::form_list(pid = PLACES_PID())
  forms <- forms[forms$submissions > 0, ]
  known <- DBI::dbGetQuery(con, 'SELECT fid, last_submission FROM vb_place_sources')
  stale <- forms[is.na(match(paste(forms$fid, forms$last_submission), paste(known$fid, known$last_submission))), ]
  n_forms <- 0L; n_names <- 0L; failed <- character(0)
  for (i in seq_len(nrow(stale))) {
    fid <- stale$fid[i]
    pts <- tryCatch(.form_place_names(fid), error = function(e) { failed <<- c(failed, fid); NULL })
    if (fid %in% failed) next
    source <- paste0('odk:', PLACES_PID(), ':', fid)
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, 'DELETE FROM vb_place_names WHERE source = $1', list(source))
      if (!is.null(pts) && nrow(pts) > 0) n_names <- n_names + .insert_new(con, 'vb_place_names', cbind(source = source, pts))
      DBI::dbExecute(con, 'INSERT INTO vb_place_sources (fid, last_submission, pulled_at) VALUES ($1, $2, NOW())
                           ON CONFLICT (fid) DO UPDATE SET last_submission = EXCLUDED.last_submission, pulled_at = NOW()',
                     list(fid, as.character(stale$last_submission[i])))
    })
    n_forms <- n_forms + 1L
  }
  sprintf('Place names: %d monitoring forms updated (%s names)%s.', n_forms, format(n_names, big.mark = ','),
          if (length(failed)) sprintf(', %d failed: %s', length(failed), paste(failed, collapse = ', ')) else '')
}
