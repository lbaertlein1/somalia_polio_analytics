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
# print() each tmap object (or table page) in turn, close the device. No
# external merge/zip tooling needed for the PDF itself.
#
# Page sequence: overview map, overview summary table (one row per health
# area), then per health area: detail map, team summary table (one row
# per team, only if that health area has a current team-area version).
#
# Basemap: optional OSM or satellite tiles under the vector layers, via
# tmap's tm_basemap() (which uses the maptiles package to fetch tiles for
# the plotted extent in "plot" mode — requires internet access at print
# time and the maptiles package installed; falls back to no basemap with
# a warning if maptiles isn't available, rather than failing the whole
# export).
#
# Population figures (WorldPop columns in both tables) are computed by
# summing u5_rast over each polygon (exactextractr::exact_extract, area-
# weighted at pixel boundaries) — NOT the same computation path as the
# app's own paint-grid population overlay, but the correct one here since
# these are the final, smoothed presentation polygons, not grid cells.
# =============================================================================

library(tmap)
library(gridExtra)

.PRINT_PAGE_SIZES_IN <- list(
  a4_landscape = c(width = 11.69, height = 8.27),
  a3_landscape = c(width = 16.54, height = 11.69)
)

.PRINT_BASEMAP_PROVIDERS <- list(
  osm       = 'OpenStreetMap',
  satellite = 'Esri.WorldImagery'
)

#' Build a multi-page printable PDF for one version (one district).
#'
#' @param file           output PDF path
#' @param version        parsed HEALTH-AREA version list (as from
#'                        db_get_version_by_id() / db_get_shared_version())
#'                        — must have $snap with at least smoothed_dfa_sf
#'                        or saved_dfa_sf. Does NOT carry team-area data
#'                        anymore — that's fetched separately below, one
#'                        current team_area_versions row per health area.
#' @param district_name  character, for title blocks
#' @param campaign_id    integer — needed to look up each health area's
#'                        current team-area version independently, and
#'                        for the default-teams fallback (compute_n_teams's
#'                        campaign-specific generation-setting lookup).
#' @param campaign_name  character, for title blocks (optional)
#' @param paper          "a4_landscape" or "a3_landscape"
#' @param basemap        "none" (default), "osm", or "satellite" — tile
#'                        layer under the vector layers on every map page.
#' @param u5_rast         terra SpatRaster, global under-5 population
#'                        raster, for the WorldPop population columns in
#'                        both summary tables. Defaults to the app's own
#'                        global u5_rast if not supplied. NULL (or a
#'                        failed lookup) shows those columns as "N/A"
#'                        rather than failing the export.
build_printable_maps_pdf <- function(file, version, district_name, campaign_id,
                                     campaign_name = '', paper = 'a4_landscape',
                                     basemap = 'none', u5_rast = NULL) {

  if (!requireNamespace('tmap', quietly = TRUE))
    stop('The tmap package is required for printable export.')
  if (!requireNamespace('gridExtra', quietly = TRUE))
    stop('The gridExtra package is required for printable export tables.')

  tmap::tmap_mode('plot')

  if (is.null(u5_rast))
    u5_rast <- tryCatch(get('u5_rast', envir = .GlobalEnv), error = function(e) NULL)

  snap <- version$snap %||% list()
  ha_sf <- snap$smoothed_dfa_sf %||% snap$saved_dfa_sf
  district_sf <- snap$district_boundary_sf
  team_targets <- snap$team_targets %||% list()

  # Team areas: combine every health area's CURRENT team-area version —
  # there is no single "the district's team areas" field on the
  # health-area version's own snapshot anymore, only per-health-area ones.
  # health_area column matches what the per-page lookup below
  # (team_sf$health_area == nm) already expects.
  team_sf <- tryCatch({
    ha_names <- setdiff(unlist(snap$dfa_names %||% list()), extra_dfa_names)
    parts <- lapply(ha_names, function(han) {
      tv <- db_get_current_team_area_version(pool, campaign_id, district_name, han)
      if (is.null(tv)) return(NULL)
      geom <- tv$snap$smoothed_team_sf %||% tv$snap$saved_team_sf
      if (is.null(geom) || nrow(geom) == 0) return(NULL)
      geom$health_area <- han
      geom
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0) NULL else do.call(rbind, parts)
  }, error = function(e) NULL)

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
  basemap_provider <- .resolve_basemap(basemap)

  grDevices::pdf(file, width = unname(size['width']), height = unname(size['height']))
  on.exit(grDevices::dev.off(), add = TRUE)

  print(.district_overview_map(ha_sf, district_sf, fac_sf, idp_sf, district_name, campaign_name, basemap_provider))

  summary_df <- .build_health_area_summary_table(ha_sf, team_targets, u5_rast, campaign_id)
  .print_table_page(summary_df, paste0(district_name, ' \u2014 Health Area Summary'))

  area_names <- setdiff(unique(as.character(ha_sf$dfa_name)), extra_dfa_names)
  for (nm in area_names) {
    ha_one   <- ha_sf[ha_sf$dfa_name == nm, , drop = FALSE]
    team_one <- if (!is.null(team_sf) && 'health_area' %in% names(team_sf))
      team_sf[team_sf$health_area == nm, , drop = FALSE] else NULL
    fac_one  <- .points_within(fac_sf, ha_one)
    idp_one  <- .points_within(idp_sf, ha_one)
    print(.health_area_detail_map(ha_one, team_one, fac_one, idp_one, nm, district_name, basemap_provider))

    if (!is.null(team_one) && nrow(team_one) > 0) {
      field_target <- .unwrap_num(team_targets[[nm]]$target_pop)
      team_df <- .build_team_summary_table(team_one, field_target, u5_rast)
      if (!is.null(team_df)) .print_table_page(team_df, paste0(district_name, ' \u2014 ', nm, ' \u2014 Team Summary'))
    }
  }

  invisible(NULL)
}

.points_within <- function(pts_sf, poly_sf) {
  if (is.null(pts_sf) || nrow(pts_sf) == 0 || is.null(poly_sf) || nrow(poly_sf) == 0) return(NULL)
  inside <- lengths(sf::st_within(pts_sf, sf::st_union(poly_sf))) > 0
  if (!any(inside)) return(NULL)
  pts_sf[inside, , drop = FALSE]
}

#' Total u5_rast population within each row of poly_sf (one value per
#' row, same order). NA for every row if u5_rast is unavailable or
#' extraction fails, rather than erroring the whole export over a
#' missing/corrupt raster.
.polygon_u5_population <- function(poly_sf, u5_rast) {
  n <- if (is.null(poly_sf)) 0 else nrow(poly_sf)
  if (is.null(u5_rast) || n == 0) return(rep(NA_real_, n))
  if (!requireNamespace('exactextractr', quietly = TRUE)) return(rep(NA_real_, n))
  tryCatch({
    poly_ll <- sf::st_transform(poly_sf, sf::st_crs(terra::crs(u5_rast)))
    as.numeric(exactextractr::exact_extract(u5_rast, poly_ll, 'sum', progress = FALSE))
  }, error = function(e) rep(NA_real_, n))
}

.fmt_pop <- function(x) ifelse(is.na(x), 'N/A', format(round(x), big.mark = ','))
.fmt_field <- function(x) ifelse(is.na(x), '\u2013', format(round(x), big.mark = ','))

#' team_targets comes from the DB via .from_json_db() (mod_db_v2.R), which
#' deliberately uses jsonlite's simplifyVector = FALSE to preserve its
#' named-list-keyed-by-health-area-name structure -- but that means every
#' scalar value inside it (target_pop, requested_teams) comes back
#' wrapped in its own length-1 list rather than as a plain number (e.g.
#' list(1114), not 1114). round()/is.na() on that wrapped form throws
#' "non-numeric argument to mathematical function" -- unwrap explicitly
#' before doing any arithmetic on it.
.unwrap_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (is.list(x)) x <- if (length(x) == 0) NA else x[[1]]
  if (is.null(x) || length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

#' One row per health area: name, WorldPop population, field target
#' population (from the post-submit team-targets modal, mod_health_area_
#' tab.R), and number of teams -- the field-requested count when set,
#' otherwise the same recommended default compute_n_teams() would offer
#' in-app, so this always shows one concrete number rather than a blank.
.build_health_area_summary_table <- function(ha_sf, team_targets, u5_rast, campaign_id) {
  area_names <- setdiff(unique(as.character(ha_sf$dfa_name)), extra_dfa_names)
  rows <- lapply(area_names, function(nm) {
    poly    <- ha_sf[ha_sf$dfa_name == nm, , drop = FALSE]
    wp_vals <- .polygon_u5_population(poly, u5_rast)
    wp_val  <- if (all(is.na(wp_vals))) NA_real_ else sum(wp_vals, na.rm = TRUE)

    tgt          <- team_targets[[nm]]
    field_pop    <- .unwrap_num(tgt$target_pop)
    req_teams    <- .unwrap_num(tgt$requested_teams)
    n_teams <- if (!is.na(req_teams) && req_teams > 0) as.integer(req_teams)
               else tryCatch(compute_n_teams(wp_val, campaign_id = campaign_id), error = function(e) NA_integer_)

    data.frame(
      `Health Area`             = nm,
      `Target Pop (WorldPop)`   = .fmt_pop(wp_val),
      `Target Pop (Field)`       = .fmt_field(field_pop),
      `Number of Teams`           = if (is.na(n_teams)) '\u2013' else as.character(n_teams),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' One row per team within a single health area: team name, WorldPop
#' population (from that team's own drawn polygon), and a field target
#' population -- there is no per-team field figure in the data model
#' (only a whole-health-area one), so this is the health area's own
#' field target divided evenly across its teams, shown as each team's
#' implied share. NA (blank) if the health area itself has no field
#' target set.
.build_team_summary_table <- function(team_one, health_area_field_target, u5_rast) {
  if (is.null(team_one) || nrow(team_one) == 0) return(NULL)
  team_names <- setdiff(unique(as.character(team_one$dfa_name)), extra_dfa_names)
  if (length(team_names) == 0) return(NULL)
  n_teams <- length(team_names)
  per_team_field <- if (!is.na(health_area_field_target) && n_teams > 0)
    health_area_field_target / n_teams else NA_real_

  rows <- lapply(team_names, function(tn) {
    poly    <- team_one[team_one$dfa_name == tn, , drop = FALSE]
    wp_vals <- .polygon_u5_population(poly, u5_rast)
    wp_val  <- if (all(is.na(wp_vals))) NA_real_ else sum(wp_vals, na.rm = TRUE)
    data.frame(
      Team                     = tn,
      `Target Pop (WorldPop)` = .fmt_pop(wp_val),
      `Target Pop (Field)`     = .fmt_field(per_team_field),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Renders a data frame as its own PDF page (title + table), via the
#' current grDevices::pdf() device already open in build_printable_maps_
#' pdf() -- grid.newpage()/grid.arrange() draw directly to that device,
#' the same way print()-ing a tmap object does for the map pages.
.print_table_page <- function(df, title) {
  grid::grid.newpage()
  tt   <- gridExtra::ttheme_default(base_size = 9)
  tbl  <- gridExtra::tableGrob(df, rows = NULL, theme = tt)
  ttl  <- grid::textGrob(title, gp = grid::gpar(fontsize = 15, fontface = 'bold'))
  gridExtra::grid.arrange(ttl, tbl, ncol = 1, heights = grid::unit(c(1, 1), c('cm', 'null')))
}

#' Resolves a basemap = "none"|"osm"|"satellite" choice to a tmap
#' provider string, or NULL if unavailable (no basemap requested, or
#' the maptiles package tmap needs for static-mode tile fetching isn't
#' installed -- warns rather than failing the whole export).
.resolve_basemap <- function(basemap) {
  if (is.null(basemap) || identical(basemap, 'none')) return(NULL)
  provider <- .PRINT_BASEMAP_PROVIDERS[[basemap]]
  if (is.null(provider)) return(NULL)
  if (!requireNamespace('maptiles', quietly = TRUE)) {
    warning('maptiles package not installed -- printing without a basemap.')
    return(NULL)
  }
  provider
}

.district_overview_map <- function(ha_sf, district_sf, fac_sf, idp_sf, district_name, campaign_name, basemap_provider = NULL) {
  # fill_alpha lets a basemap show through underneath -- fully opaque
  # fill would defeat the point of adding one. Only lowered when a
  # basemap is actually in play; unchanged (opaque) otherwise.
  fill_alpha <- if (!is.null(basemap_provider)) 0.55 else 1

  m <- tmap::tm_shape(ha_sf)
  if (!is.null(basemap_provider)) m <- m + tmap::tm_basemap(basemap_provider)
  m <- m +
    tmap::tm_fill(
      fill = 'dfa_name',
      fill.scale = tmap::tm_scale_categorical(),
      fill.legend = tmap::tm_legend(title = 'Health Area'),
      fill_alpha = fill_alpha
    ) +
    tmap::tm_borders(col = 'black', lwd = 0.8)

  if (!is.null(district_sf) && nrow(district_sf) > 0)
    m <- m + tmap::tm_shape(district_sf) + tmap::tm_borders(col = 'black', lwd = 2)

  if (!is.null(fac_sf) && nrow(fac_sf) > 0)
    m <- m + tmap::tm_shape(fac_sf) +
      tmap::tm_symbols(fill = '#0d9488', size = 0.3, shape = 21, col = 'white', col_alpha = 1) +
      tmap::tm_text('facility_name', size = 0.4, ymod = 0.7)

  if (!is.null(idp_sf) && nrow(idp_sf) > 0)
    m <- m + tmap::tm_shape(idp_sf) +
      tmap::tm_symbols(fill = '#d95f0e', size = 0.25, shape = 24, col = 'white', col_alpha = 1)

  title <- paste0(district_name, if (nzchar(campaign_name)) paste0(' — ', campaign_name) else '')
  m + tmap::tm_title(title) + tmap::tm_scalebar() + tmap::tm_compass(position = c('right', 'top'))
}

.health_area_detail_map <- function(ha_one, team_one, fac_one, idp_one, area_name, district_name, basemap_provider = NULL) {
  fill_alpha_base <- if (!is.null(basemap_provider)) 0.35 else 1
  fill_alpha_team <- if (!is.null(basemap_provider)) 0.4  else 0.5

  m <- tmap::tm_shape(ha_one)
  if (!is.null(basemap_provider)) m <- m + tmap::tm_basemap(basemap_provider)
  m <- m +
    tmap::tm_fill(fill = '#f0fdfa', fill_alpha = fill_alpha_base) +
    tmap::tm_borders(col = 'black', lwd = 2)

  if (!is.null(team_one) && nrow(team_one) > 0)
    m <- m + tmap::tm_shape(team_one) +
      tmap::tm_fill(
        fill = 'dfa_name',
        fill.scale = tmap::tm_scale_categorical(),
        fill.legend = tmap::tm_legend(title = 'Team Area'),
        fill_alpha = fill_alpha_team
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
