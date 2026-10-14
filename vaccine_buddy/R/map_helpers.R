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
