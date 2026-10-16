# =============================================================================
# messages.R -- WhatsApp text, links and phone numbers
#
# Sending is click-to-send: https://wa.me/<number>?text=<message> opens
# WhatsApp (app, desktop or web) with the message filled in; the sender
# presses send. No WhatsApp Business API account needed.
#
# A health area can have dozens of missed places, so the message itself
# stays short -- totals, a line per team area, the top few places -- and
# links to the supervisor page, which has the full list and map.
# =============================================================================

#' Digits only, in international format for Somalia (+252) unless the
#' number already has a country code.
normalize_phone <- function(x) {
  d <- gsub('[^0-9]', '', as.character(x))
  d <- sub('^00', '', d)
  d <- ifelse(grepl('^0', d), paste0('252', sub('^0+', '', d)), d)
  ifelse(nchar(d) <= 9 & nchar(d) >= 7, paste0('252', d), d)
}

wa_link <- function(phone, text) {
  paste0('https://wa.me/', if (is.null(phone)) '' else normalize_phone(phone),
         '?text=', utils::URLencode(text, reserved = TRUE))
}

# Opens Google Maps navigation to the point (the app on phones).
gmaps_nav <- function(lat, lon) sprintf('https://www.google.com/maps/dir/?api=1&destination=%.6f,%.6f', lat, lon)

supervisor_link <- function(token) paste0(sub('/?$', '/', APP_PUBLIC_URL), '?t=', token)

new_token <- function() openssl::rand_bytes(12) |> as.character() |> paste(collapse = '')

# Vaccine Buddy team names as logged ("TABELAHA _HC_T5") made readable
# ("TABELAHA HC T5"); adjuster team-area names pass through unchanged.
team_label <- function(x) ifelse(is.na(x), 'No team', trimws(gsub('[[:space:]]+', ' ', gsub('_', ' ', x))))

.places_n <- function(n) paste(n, ifelse(n == 1, 'place', 'places'))

# WorldPop counts are estimates: show 2 significant figures ("~1,100").
.fmt_n <- function(x) formatC(ifelse(x < 10, round(x), signif(x, 2)), big.mark = ',', format = 'd')

#' places: that health area's missed places (data.frame / sf). Supervisors
#' and teams never see coverage figures or child numbers -- only the missed
#' places, most children first (the numbers are for the admin side only).
#' The operator chooses what goes in: the links come first -- the health
#' area's Google My Maps map (map_link, sending with My Maps), its map to
#' download (pdf_link), the supervisor app (link; NULL leaves it out) --
#' then, with_list, the missed places with a directions link, most
#' children first: all of them, or the first max_list (e.g. the top 5).
#' `attached`: the operator attaches the map (PDF) by hand -- the message
#' says so after the greeting.
supervisor_message <- function(supervisor_name, district, health_area, data_to, places, link = NULL, max_list = Inf,
                               map_link = NULL, pdf_link = NULL, with_list = TRUE, attached = FALSE) {
  head <- c(
    sprintf('Missed places \u2013 %s, %s', health_area, district),
    sprintf('From vaccination team GPS up to %s', format(as.Date(data_to), '%d %b')),
    ''
  )
  if (is.null(places) || nrow(places) == 0) {
    return(paste(c(head, sprintf('Hello %s, no missed places were found in your area. Thank you!', supervisor_name)),
                 collapse = '\n'))
  }
  p <- as.data.frame(sf::st_drop_geometry(places))
  top <- utils::head(p[order(-p$u5_pop), ], max_list)   # most children first
  # Whose place it is: the mapped team area, else the team that passed closest
  team_line <- ifelse(is.na(top$assigned_team), '',
                      ifelse(top$team_source %in% 'team area', sprintf('\nTeam area: %s', team_label(top$assigned_team)),
                             sprintf('\nTeam that passed closest: %s', team_label(top$assigned_team))))

  paste(c(
    head,
    sprintf('Hello %s, based on Vaccine Buddy data, there %s %s in your area that vaccination teams did not visit.',
            supervisor_name, if (nrow(p) == 1) 'is' else 'are', .places_n(nrow(p))),
    if (attached) 'The map of these missed places is attached (PDF).',
    if (!is.null(map_link)) c('', '*Map of all missed places:*', map_link),
    if (!is.null(pdf_link)) c('', '*Map to download and print (PDF):*', pdf_link),
    if (!is.null(link)) c('', '*Assign the places to your teams:*', link),
    if (with_list) c('', '*Missed places* (tap a link for directions)',
                     sprintf('%d. %s%s\n%s', top$place_no, top$label, team_line, gmaps_nav(top$lat, top$lon)),
                     if (nrow(p) > nrow(top)) sprintf('...and %d more', nrow(p) - nrow(top)))
  ), collapse = '\n')
}

#' A team's list, for the supervisor to forward to that team leader.
#' `team` is the team area name, or NULL for places the supervisor picked.
#' `link` opens the supervisor page on just these places (map + mark done).
team_message <- function(team, health_area, places, link) {
  p <- places[order(places$place_no), ]
  paste(c(
    if (is.null(team)) sprintf('Missed places for your team – %s', health_area)
    else sprintf('Missed places – %s, %s', team_label(team), health_area),
    'Please go back to each place and vaccinate every child under 5. Tap a link for directions:',
    '',
    sprintf('%d. %s\n%s', p$place_no, p$label, gmaps_nav(p$lat, p$lon)),
    '',
    'Map of all places (mark each one done when finished):',
    link
  ), collapse = '\n')
}
