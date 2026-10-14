# =============================================================================
# clean.R -- stored Vaccine Buddy log rows -> clean, per-device GPS fixes
#
# Input: vb_load_entries() rows (device_id, entry_id, ts, lon, lat,
# accuracy_m, lid_open, team_name). Re-uploaded rows were already dropped
# when stored (one row per device + entry id; see R/odk.R).
#
# Steps (each logged in the returned report with rows removed):
#   1. optionally drop teams matching exclude_teams (e.g. FETP monitors)
#   2. keep working hours only -- carriers log all night wherever they are
#      stored, which would otherwise mark that neighbourhood as visited
#   3. drop rows with no GPS fix or a fix outside Somalia
#   4. drop fixes with reported accuracy worse than max_accuracy_m
#   5. drop exact duplicates (same device, time and position)
#   6. drop GPS spikes: a fix reached AND left faster than spike_kmh
# Then flags each remaining fix's speed from the previous one, so analysis
# can ignore driving (walk_max_kmh).
#
# Lid-open/close and temperature rows usually have no GPS fix of their own,
# so they drop out in step 3 -- visits are measured from the GPS track only.
# =============================================================================

# Distance (m) and speed (km/h) from each fix to the previous one of the
# same device. Expects rows sorted by device_id, ts.
.add_steps <- function(d) {
  n <- nrow(d)
  prev <- c(NA_integer_, seq_len(n - 1))
  same_dev <- c(FALSE, d$device_id[-1] == d$device_id[-n])
  pts <- sf::st_as_sf(d[, c('lon', 'lat')], coords = c('lon', 'lat'), crs = 4326)
  dist <- rep(NA_real_, n)
  if (n > 1) dist[-1] <- as.numeric(sf::st_distance(pts[-1, ], pts[-n, ], by_element = TRUE))
  gap_s <- c(NA_real_, as.numeric(difftime(d$ts[-1], d$ts[-n], units = 'secs')))
  dist[!same_dev] <- NA; gap_s[!same_dev] <- NA
  d$step_m   <- dist
  d$gap_min  <- gap_s / 60
  d$speed_kmh <- ifelse(!is.na(gap_s) & gap_s > 0, dist / gap_s * 3.6, NA_real_)
  d
}

clean_pings <- function(d, params) {
  report <- data.frame(step = character(0), removed = integer(0), remaining = integer(0))
  log_step <- function(d, step, before) {
    report[nrow(report) + 1, ] <<- list(step, before - nrow(d), nrow(d))
    d
  }
  report[1, ] <- list('Log rows on the selected dates (re-uploads already removed)', 0L, nrow(d))

  if (nzchar(params$exclude_teams %||% '')) {
    n <- nrow(d); d <- d[!grepl(params$exclude_teams, d$team_name %||% '', ignore.case = TRUE), ]
    d <- log_step(d, sprintf('Teams matching "%s" (not vaccination teams)', params$exclude_teams), n)
  }

  h <- lubridate::hour(d$ts) + lubridate::minute(d$ts) / 60
  n <- nrow(d); d <- d[h >= params$work_start_h & h < params$work_end_h, ]
  d <- log_step(d, sprintf('Outside working hours (%02d:00-%02d:00)', params$work_start_h, params$work_end_h), n)

  b <- SOMALIA_BBOX
  n <- nrow(d); d <- d[!is.na(d$lon) & !is.na(d$lat) & d$lon >= b['xmin'] & d$lon <= b['xmax'] & d$lat >= b['ymin'] & d$lat <= b['ymax'], ]
  d <- log_step(d, 'No GPS fix (lid/temperature rows) or fix outside Somalia', n)

  n <- nrow(d); d <- d[is.na(d$accuracy_m) | d$accuracy_m <= params$max_accuracy_m, ]
  d <- log_step(d, sprintf('GPS accuracy worse than %s m', params$max_accuracy_m), n)

  n <- nrow(d); d <- dplyr::distinct(d, device_id, ts, lon, lat, .keep_all = TRUE)
  d <- log_step(d, 'Duplicate fixes (same device, time and position)', n)

  d <- d[order(d$device_id, d$ts), ]
  if (nrow(d) > 1) {
    d <- .add_steps(d)
    speed_out <- c(d$speed_kmh[-1], NA)
    speed_out[c(d$device_id[-1] != d$device_id[-nrow(d)], TRUE)] <- NA
    spike <- !is.na(d$speed_kmh) & !is.na(speed_out) & d$speed_kmh > params$spike_kmh & speed_out > params$spike_kmh
    n <- nrow(d); d <- d[!spike, ]
    d <- log_step(d, sprintf('GPS spikes (jump faster than %s km/h and back)', params$spike_kmh), n)
    d <- .add_steps(d)
  } else {
    d$step_m <- d$gap_min <- d$speed_kmh <- rep(NA_real_, nrow(d))
  }

  d$moving_fast <- !is.na(d$speed_kmh) & d$speed_kmh > params$walk_max_kmh &
    !is.na(d$gap_min) & d$gap_min <= params$max_gap_min
  report[nrow(report) + 1, ] <- list(sprintf('Moving faster than %s km/h (kept, but not counted as visits)', params$walk_max_kmh),
                                     sum(d$moving_fast), sum(!d$moving_fast))
  rownames(d) <- NULL
  list(pings = d, report = report)
}
