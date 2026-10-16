# =============================================================================
# map_helpers.R -- map pieces shared by the admin and supervisor maps
# =============================================================================

# Health facilities: a red cross, the usual health symbol
.cross_svg <- paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 18 18">',
  '<path d="M6 1h6v5h5v6h-5v5H6v-5H1V6h5z" fill="#dc2626" stroke="#ffffff" stroke-width="1.5"/></svg>')
FACILITY_ICON <- leaflet::makeIcon(
  iconUrl = paste0('data:image/svg+xml;charset=UTF-8,', utils::URLencode(.cross_svg, reserved = TRUE)),
  iconWidth = 18, iconHeight = 18, iconAnchorX = 9, iconAnchorY = 9)

#' Health facilities as red crosses, names always shown (label_class) or on
#' hover (label_class = NULL).
add_facilities <- function(map, facilities, label_class = 'lm-label', group = NULL) {
  if (is.null(facilities) || nrow(facilities) == 0) return(map)
  leaflet::addMarkers(map, data = facilities, icon = FACILITY_ICON, group = group, label = ~facility_name,
                      labelOptions = if (!is.null(label_class))
                        leaflet::labelOptions(noHide = TRUE, direction = 'right', textOnly = TRUE, className = label_class, offset = c(8, 0)))
}

# Vaccination team GPS tracks, the same on every map (app, supervisor and
# team pages, printed maps, My Maps): colours neither satellite imagery nor
# street maps use -- cyan for other teams, magenta for the teams in focus
# (the chosen health area's, or your own) -- points with a dark outline so
# they read on light and dark backgrounds, thin lines a shade darker.
TRACK_POINT <- '#22d3ee'
TRACK_LINE  <- '#0891b2'
TRACK_MINE_POINT <- '#e91ee0'
TRACK_MINE_LINE  <- '#c026d3'
TRACK_EDGE <- '#1e293b'

#' clean_pings() points as track lines (sf LINESTRING, lon/lat), or NULL. A
#' new line starts wherever the device changes or there's a long gap / jump.
track_lines <- function(pg, params) {
  if (is.null(pg) || nrow(pg) < 2) return(NULL)
  pg <- pg[order(pg$device_id, pg$ts), ]
  brk <- c(TRUE, pg$device_id[-1] != pg$device_id[-nrow(pg)]) | is.na(pg$gap_min) |
    pg$gap_min > params$max_gap_min | pg$step_m > params$max_link_m
  pg$seg <- cumsum(brk)
  segs <- pg[pg$seg %in% pg$seg[duplicated(pg$seg)], c('seg', 'lon', 'lat')]
  if (nrow(segs) == 0) return(NULL)
  sf::st_set_crs(sfheaders::sf_linestring(segs, x = 'lon', y = 'lat', linestring_id = 'seg'), 4326)
}

#' Add clean_pings() points as tracks (lines and points) to a leaflet map or
#' proxy, in the given colours (blue by default).
add_tracks <- function(map, pg, params, group = 'tracks', pane = NULL, max_points = 40000,
                       line_color = TRACK_LINE, point_color = TRACK_POINT) {
  if (is.null(pg) || nrow(pg) == 0) return(map)
  lines <- track_lines(pg, params)
  opts <- if (is.null(pane)) leaflet::pathOptions() else leaflet::pathOptions(pane = pane)
  if (!is.null(lines))
    map <- leaflet::addPolylines(map, data = lines, group = group, color = line_color, weight = 1.5, opacity = 0.8, options = opts)
  pts <- if (nrow(pg) > max_points) pg[round(seq(1, nrow(pg), length.out = max_points)), ] else pg
  leaflet::addCircleMarkers(map, data = pts, lng = ~lon, lat = ~lat, group = group, radius = 2.5,
                            color = TRACK_EDGE, weight = 0.8, opacity = 0.9,
                            fillColor = point_color, fillOpacity = 0.95, options = opts,
                            popup = ~sprintf('%s<br>%s', team_label(team_name), format(ts, '%a %d %b %H:%M')))
}
