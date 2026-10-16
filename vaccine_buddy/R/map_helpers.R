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

# Vaccination team GPS tracks: small blue points with light route lines
TRACK_POINT <- '#2563eb'
TRACK_LINE  <- '#93c5fd'
# ...and orange for the teams in focus (the chosen health area's, or your own)
TRACK_MINE_POINT <- '#ea580c'
TRACK_MINE_LINE  <- '#fdba74'

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
    map <- leaflet::addPolylines(map, data = lines, group = group, color = line_color, weight = 1.5, opacity = 0.7, options = opts)
  pts <- if (nrow(pg) > max_points) pg[round(seq(1, nrow(pg), length.out = max_points)), ] else pg
  leaflet::addCircleMarkers(map, data = pts, lng = ~lon, lat = ~lat, group = group, radius = 1.8, stroke = FALSE,
                            fillColor = point_color, fillOpacity = 0.7, options = opts,
                            popup = ~sprintf('%s<br>%s', team_label(team_name), format(ts, '%a %d %b %H:%M')))
}
