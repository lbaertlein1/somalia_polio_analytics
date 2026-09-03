# =============================================================================
# printable_export.R
#
# Printable field maps: one district overview page + one page per health
# area, each with a title block, scale bar, north arrow, and legend, sized
# for A3/A4 landscape printing. Uses the SMOOTHED presentation geometry
# (smoothed_dfa_sf / smoothed_team_sf) rather than the raw grid-stepped
# polygons — those look unprofessional and confusing on a printed field map.
# Falls back to raw saved_dfa_sf/saved_team_sf if smoothing hasn't been
# generated yet for a version (e.g. never submitted).
#
# Uses tmap in "plot" mode (static rendering), matching the tmap v4 syntax
# already used elsewhere in this codebase (view_friction_surface.R) —
# tm_title/tm_scalebar/tm_compass/fill.scale/fill.legend, not the older
# tmap v3 tm_layout(title=...)/tm_raster(palette=...) style.
#
# Multi-page PDF is built the standard base-R way: open one pdf() device,
# print() each tmap object in turn, close the device. No external
# merge/zip tooling needed for the PDF itself.
# =============================================================================

library(tmap)

.PRINT_PAGE_SIZES_IN <- list(
  a4_landscape = c(width = 11.69, height = 8.27),
  a3_landscape = c(width = 16.54, height = 11.69)
)

#' Build a multi-page printable PDF for one version (one district).
#'
#' @param file           output PDF path
#' @param version        parsed version list (as from db_get_version_by_id() /
#'                        db_get_shared_version()) — must have $snap with at
#'                        least smoothed_dfa_sf or saved_dfa_sf.
#' @param district_name  character, for title blocks
#' @param campaign_name  character, for title blocks (optional)
#' @param paper          "a4_landscape" or "a3_landscape"
build_printable_maps_pdf <- function(file, version, district_name,
                                     campaign_name = '', paper = 'a4_landscape') {

  if (!requireNamespace('tmap', quietly = TRUE))
    stop('The tmap package is required for printable export.')

  tmap::tmap_mode('plot')

  snap <- version$snap %||% list()
  ha_sf   <- snap$smoothed_dfa_sf  %||% snap$saved_dfa_sf
  team_sf <- snap$smoothed_team_sf %||% snap$saved_team_sf
  district_sf <- snap$district_boundary_sf

  if (is.null(ha_sf) || nrow(ha_sf) == 0)
    stop('No health area geometry available for this version — nothing to print.')

  ha_sf <- sf::st_transform(sf::st_make_valid(ha_sf), 4326)
  if (!is.null(team_sf) && nrow(team_sf) > 0) team_sf <- sf::st_transform(sf::st_make_valid(team_sf), 4326)
  if (!is.null(district_sf) && nrow(district_sf) > 0) district_sf <- sf::st_transform(sf::st_make_valid(district_sf), 4326)

  # Coordination sites — combine locked ODK snapshot + user-added sites,
  # filtered to those actually marked as coordination sites.
  fac_sf <- NULL
  fac_parts <- Filter(function(x) inherits(x, 'sf') && nrow(x) > 0, list(snap$odk_sf, snap$app_sf))
  if (length(fac_parts) > 0) {
    combined <- do.call(rbind, fac_parts)
    if ('polio_sia_coordination_site' %in% names(combined))
      combined <- combined[!is.na(combined$polio_sia_coordination_site) &
                             combined$polio_sia_coordination_site == 'Yes', , drop = FALSE]
    if (nrow(combined) > 0) fac_sf <- sf::st_transform(combined, 4326)
  }

  # IDP settlements — stored as a plain data frame with lon/lat
  idp_sf <- NULL
  if (!is.null(snap$idp_settlements) && nrow(snap$idp_settlements) > 0 &&
      all(c('lon', 'lat') %in% names(snap$idp_settlements))) {
    idp_sf <- sf::st_as_sf(snap$idp_settlements, coords = c('lon', 'lat'), crs = 4326, remove = FALSE)
  }

  size <- .PRINT_PAGE_SIZES_IN[[paper]] %||% .PRINT_PAGE_SIZES_IN$a4_landscape

  grDevices::pdf(file, width = unname(size['width']), height = unname(size['height']))
  on.exit(grDevices::dev.off(), add = TRUE)

  print(.district_overview_map(ha_sf, district_sf, fac_sf, idp_sf, district_name, campaign_name))

  area_names <- setdiff(unique(as.character(ha_sf$dfa_name)), extra_dfa_names)
  for (nm in area_names) {
    ha_one   <- ha_sf[ha_sf$dfa_name == nm, , drop = FALSE]
    team_one <- if (!is.null(team_sf) && 'health_area' %in% names(team_sf))
      team_sf[team_sf$health_area == nm, , drop = FALSE] else NULL
    fac_one  <- .points_within(fac_sf, ha_one)
    idp_one  <- .points_within(idp_sf, ha_one)
    print(.health_area_detail_map(ha_one, team_one, fac_one, idp_one, nm, district_name))
  }

  invisible(NULL)
}

.points_within <- function(pts_sf, poly_sf) {
  if (is.null(pts_sf) || nrow(pts_sf) == 0 || is.null(poly_sf) || nrow(poly_sf) == 0) return(NULL)
  inside <- lengths(sf::st_within(pts_sf, sf::st_union(poly_sf))) > 0
  if (!any(inside)) return(NULL)
  pts_sf[inside, , drop = FALSE]
}

.district_overview_map <- function(ha_sf, district_sf, fac_sf, idp_sf, district_name, campaign_name) {
  m <- tmap::tm_shape(ha_sf) +
    tmap::tm_fill(
      fill = 'dfa_name',
      fill.scale = tmap::tm_scale_categorical(),
      fill.legend = tmap::tm_legend(title = 'Health Area')
    ) +
    tmap::tm_borders(col = 'black', lwd = 0.8)

  if (!is.null(district_sf) && nrow(district_sf) > 0)
    m <- m + tmap::tm_shape(district_sf) + tmap::tm_borders(col = 'black', lwd = 2)

  if (!is.null(fac_sf) && nrow(fac_sf) > 0)
    m <- m + tmap::tm_shape(fac_sf) +
      tmap::tm_symbols(fill = '#0d9488', size = 0.3, shape = 21, col = 'white', col_alpha = 1)

  if (!is.null(idp_sf) && nrow(idp_sf) > 0)
    m <- m + tmap::tm_shape(idp_sf) +
      tmap::tm_symbols(fill = '#d95f0e', size = 0.25, shape = 24, col = 'white', col_alpha = 1)

  title <- paste0(district_name, if (nzchar(campaign_name)) paste0(' — ', campaign_name) else '')
  m + tmap::tm_title(title) + tmap::tm_scalebar() + tmap::tm_compass(position = c('right', 'top'))
}

.health_area_detail_map <- function(ha_one, team_one, fac_one, idp_one, area_name, district_name) {
  m <- tmap::tm_shape(ha_one) +
    tmap::tm_fill(fill = '#f0fdfa') +
    tmap::tm_borders(col = 'black', lwd = 2)

  if (!is.null(team_one) && nrow(team_one) > 0)
    m <- m + tmap::tm_shape(team_one) +
      tmap::tm_fill(
        fill = 'dfa_name',
        fill.scale = tmap::tm_scale_categorical(),
        fill.legend = tmap::tm_legend(title = 'Team Area'),
        fill_alpha = 0.5
      ) +
      tmap::tm_borders(col = '#334155', lwd = 0.6)

  if (!is.null(fac_one) && nrow(fac_one) > 0)
    m <- m + tmap::tm_shape(fac_one) +
      tmap::tm_symbols(fill = '#0d9488', size = 0.4, shape = 21, col = 'white', col_alpha = 1) +
      tmap::tm_text('facility_name', size = 0.5, ymod = 0.8)

  if (!is.null(idp_one) && nrow(idp_one) > 0)
    m <- m + tmap::tm_shape(idp_one) +
      tmap::tm_symbols(fill = '#d95f0e', size = 0.35, shape = 24, col = 'white', col_alpha = 1)

  m + tmap::tm_title(paste0(district_name, ' — ', area_name)) +
    tmap::tm_scalebar() + tmap::tm_compass(position = c('right', 'top'))
}
