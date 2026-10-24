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
library(grid)
library(png)

# GPEI logo, used in the new page-header chrome (.draw_page_header()) --
# same data/ convention the app's own u5_rast raster uses. Loaded once,
# lazily, the first time a page header actually needs it (not at source()
# time), and cached in this variable so a multi-page export doesn't re-
# read the same PNG off disk once per page.
.GPEI_LOGO_PATH <- 'data/gpei_logo.png'
.gpei_logo_cache <- NULL
.get_gpei_logo <- function() {
  if (!is.null(.gpei_logo_cache)) return(.gpei_logo_cache)
  if (!file.exists(.GPEI_LOGO_PATH)) return(NULL)
  img <- tryCatch(png::readPNG(.GPEI_LOGO_PATH), error = function(e) NULL)
  .gpei_logo_cache <<- img
  img
}

# GPEI brand colors, sampled directly from the logo -- used for the new
# page-header chrome (banner text, big-number stat card) and nowhere
# else; every existing map layer color (facilities, IDP, subdivisions,
# etc.) is deliberately left alone, per "content stays matching the
# app's current design" -- this is a layout/branding pass only, not a
# recolor of the actual map data.
.GPEI_ORANGE <- '#F5821F'
.GPEI_BLUE   <- '#4E6DB3'

.PRINT_PAGE_SIZES_IN <- list(
  a4_landscape = c(width = 11.69, height = 8.27),
  a3_landscape = c(width = 16.54, height = 11.69)
)

.PRINT_BASEMAP_PROVIDERS <- list(
  osm       = 'OpenStreetMap',
  satellite = 'Esri.WorldImagery',
  topo      = 'OpenTopoMap'
)

# basemap may be several choices (the export tab's checkbox group): every
# map page is printed once per basemap in the same PDF, since field teams
# usually need more than one (street names from OSM, actual rooftops/
# settlement footprint from imagery). Nothing selected means no basemap.
.expand_basemaps <- function(basemap) {
  basemap <- unique(as.character(basemap %||% character(0)))
  basemap <- basemap[nzchar(basemap)]
  if (length(basemap) == 0) 'none' else basemap
}

# Subdivision styling: dashed purple, same as the in-app maps
# (paint-app.js's subdivisionLayer). Drawn UNDER the health area and
# district borders, so where a subdivision boundary follows a health area
# boundary the health area border is what shows. Thicker than the old 1px
# line, which was too faint to see on a printed page.
.SUBDIV_COL   <- '#7c3aed'
.SUBDIV_LWD   <- 2.5

# Point symbol sizes (tmap size units) -- bumped up from the original
# 0.3/0.25 overview and 0.4/0.35 detail sizes after review feedback that
# they were hard to spot on a printed page.
.PRINT_PT_SIZE <- list(
  overview = c(fac = 0.7, idp = 0.6),
  detail   = c(fac = 0.85, idp = 0.75)
)

# Legend items for a map page. Built per basemap, since the health/team
# area border swatches must match the neon-blue borders the map switches
# to on satellite/topo (see .district_overview_map()). pch matches the
# map's own symbol shapes (21 circle -> 19, 24 triangle -> 17).
.legend_items <- function(basemap, team = FALSE, fac = TRUE, idp = TRUE, subdiv = TRUE, urban = TRUE) {
  # Only layers actually drawn on the page get a legend entry -- each
  # flag says whether that page has any of that layer.
  high_contrast <- basemap %in% c('satellite', 'topo')
  items <- list(
    if (fac)    list(swatch_col = '#0d9488', shape = 'point', pch = 19, label = 'SIA Coordination Site'),
    if (idp)    list(swatch_col = '#FF1744', shape = 'point', pch = 17, label = 'IDP Settlement'),
                list(swatch_col = if (high_contrast) '#00e5ff' else 'black', shape = 'line', label = 'Health Area Boundary'),
    if (team)   list(swatch_col = if (high_contrast) '#00e5ff' else '#334155', shape = 'line', label = 'Team Area Boundary'),
    if (subdiv) list(swatch_col = .SUBDIV_COL, shape = 'line', lwd = .SUBDIV_LWD, lty = 'dashed', label = 'Subdivision'),
    if (urban)  list(swatch_col = '#0d9488', shape = 'line', lty = 'dashed', label = 'Urban Extent')
  )
  Filter(Negate(is.null), items)
}
.has_rows <- function(x) !is.null(x) && nrow(x) > 0

# Subdivision lines (drawn right after the health area/district borders)
# and their name labels (drawn later, above the other line layers). Split
# in two because they belong at different points in the layer stack.
# label_min_frac skips labelling slivers -- on a health area detail page
# the subdivisions are clipped to the health area, and a thin clipped
# edge piece doesn't merit a name sitting on top of it.
.subdivision_lines <- function(subdiv_sf) {
  tmap::tm_shape(subdiv_sf) +
    tmap::tm_borders(col = .SUBDIV_COL, lwd = .SUBDIV_LWD, lty = 'dashed')
}
.subdivision_labels <- function(subdiv_sf, label_min_frac = 0) {
  if (!'subdivision_name' %in% names(subdiv_sf)) return(NULL)
  lbl <- subdiv_sf[!is.na(subdiv_sf$subdivision_name) & nzchar(subdiv_sf$subdivision_name), , drop = FALSE]
  if (nrow(lbl) > 0 && label_min_frac > 0) {
    a <- as.numeric(sf::st_area(lbl))
    lbl <- lbl[a >= label_min_frac * sum(a), , drop = FALSE]
  }
  if (nrow(lbl) == 0) return(NULL)
  tmap::tm_shape(lbl) +
    tmap::tm_text('subdivision_name', size = 0.65, col = '#5b21b6', fontface = 'bold.italic',
                  options = tmap::opt_tm_text(shadow = TRUE))
}

# Callout-box label: dark text on a white box, for the labels a reader
# looks for first (health area names on the overview, coordination site
# names on detail pages). Dark text on every basemap -- the box supplies
# the contrast, so the neon-blue satellite text color isn't needed here.
.callout_text <- function(text_col_name, size, ymod = 0) {
  tmap::tm_text(text_col_name, size = size, col = '#0f172a', fontface = 'bold', ymod = ymod,
                bgcol = 'white', bgcol_alpha = 0.9,
                options = tmap::opt_tm_text(bg.padding = 0.25))
}

# Higher than tm_basemap()'s own auto-detected zoom, for sharper export
# imagery -- worth adjusting after seeing a real export at typical
# health-area/district extents, since too high a zoom for a large extent
# just means more tiles fetched without a visible sharpness gain.
.PRINT_BASEMAP_ZOOM <- 16

# Automatic tile zoom for one map page: the lowest zoom whose tiles still
# give at least one tile pixel per pixel of the rendered map (150 dpi,
# see .print_chrome_page()), from the extent the frame will actually
# show. A single fixed zoom can't suit every page -- z16 is right for a
# city health area but means thousands of tiles for the overview of a
# large rural district (slow, and against OSM's tile usage policy).
.auto_basemap_zoom <- function(sf_obj, frame_w_in, frame_h_in, dpi = 150) {
  bb <- sf::st_bbox(sf::st_transform(sf_obj, 4326))
  lon_span <- as.numeric(bb['xmax'] - bb['xmin'])
  lat_span <- as.numeric(bb['ymax'] - bb['ymin'])
  # A tall shape in a wide frame shows more longitude than its own span.
  lon_span <- max(lon_span, lat_span * frame_w_in / frame_h_in)
  if (!is.finite(lon_span) || lon_span <= 0) return(.PRINT_BASEMAP_ZOOM)
  z <- ceiling(log2(frame_w_in * dpi * 360 / (256 * lon_span)))
  as.integer(min(max(z, 5), 18))
}

# Map frame size in inches for a page -- page minus the 0.25in margins,
# times the map row's share of the height (.PRINT_ROW_HEIGHTS).
.map_frame_in <- function(page_width_in, page_height_in) {
  c(w = page_width_in - 0.5, h = (page_height_in - 0.5) * .PRINT_ROW_HEIGHTS[['map']])
}

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
#' @param basemap        character vector of "osm"/"satellite"/"topo" (or "none") —
#'                        every map page is printed once per basemap given.
#' @param u5_rast         terra SpatRaster, global under-5 population
#'                        raster, for the WorldPop population columns in
#'                        both summary tables. Defaults to the app's own
#'                        global u5_rast if not supplied. NULL (or a
#'                        failed lookup) shows those columns as "N/A"
#'                        rather than failing the export.
#' @param show_pop        logical, default FALSE — overlay the WorldPop
#'                        population raster on every map page, same
#'                        source (make_population_overlay_sf(), reused
#'                        directly rather than re-implemented) and same
#'                        0.5 fill opacity the in-app map itself uses
#'                        (paint-app.js's popStyleForFeature). Silently
#'                        skipped if u5_rast/district_sf aren't available
#'                        rather than failing the whole export.
# Shared data-prep for one district's export -- everything needed to
# build the district overview page (and, for the full PDF caller, every
# per-health-area page after it) that does NOT depend on page size,
# basemap zoom, or which pages actually get printed. Extracted out of
# build_printable_maps_pdf() so the new fast preview function
# (build_printable_overview_preview_png()) can share this exact logic
# rather than duplicating it -- two copies of this data-fetching path
# drifting apart over time would be a much worse outcome than one
# shared function with two thin callers.
.prepare_export_context <- function(version, district_name, campaign_id, u5_rast = NULL, show_pop = FALSE) {
  if (is.null(u5_rast))
    u5_rast <- tryCatch(get('u5_rast', envir = .GlobalEnv), error = function(e) NULL)

  snap <- version$snap %||% list()
  ha_sf <- snap$smoothed_dfa_sf %||% snap$saved_dfa_sf
  # snap$district_boundary_sf is never written by the app (see the same
  # note in download_helpers_v2.R), so it's NULL for real versions -- and
  # without a district shape every context layer that needs one
  # (subdivisions, urban extent, population overlay, district outline)
  # was silently skipped. Same districts_shp lookup the boundary download
  # uses.
  district_sf <- snap$district_boundary_sf
  if (is.null(district_sf) || nrow(district_sf) == 0)
    district_sf <- tryCatch(
      districts_shp_for_campaign(campaign_id) |> dplyr::filter(district_name == !!district_name) |>
        dplyr::group_by(district_name) |> dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
        sf::st_as_sf() |> safe_make_valid() |> sf::st_transform(4326),
      error = function(e) NULL)
  team_targets <- snap$team_targets %||% list()
  # Computed-at-submit WorldPop per health area (mapping_versions.dfa_
  # worldpop_pop) -- read here so .build_health_area_summary_table()
  # can use it instead of re-extracting from the raster.
  dfa_worldpop_pop <- snap$dfa_worldpop_pop %||% list()
  cat('[export_debug] checkpoint: snap/ha_sf/district_sf unpacked\n')

  # Team areas: combine every health area's CURRENT team-area version —
  # there is no single "the district's team areas" field on the
  # health-area version's own snapshot anymore, only per-health-area ones.
  # health_area column matches what the per-page lookup below
  # (team_sf$health_area == nm) already expects.
  team_field_overrides_by_ha <- list()
  team_worldpop_by_ha <- list()
  team_sf <- tryCatch({
    ha_names <- setdiff(unlist(snap$dfa_names %||% list()), extra_dfa_names)
    parts <- lapply(ha_names, function(han) {
      tv <- db_get_current_team_area_version(pool, campaign_id, district_name, han)
      if (is.null(tv)) return(NULL)
      geom <- tv$snap$smoothed_team_sf %||% tv$snap$saved_team_sf
      if (is.null(geom) || nrow(geom) == 0) return(NULL)
      geom$health_area <- han
      # Per-team field-verified populations, persisted alongside the
      # team boundaries themselves (see mod_team_area_tab.R's own
      # submit_stage_fn call) -- used by .build_team_summary_table()
      # below in place of naively dividing the health area's field
      # target equally across teams.
      team_field_overrides_by_ha[[han]] <<- tv$snap$team_field_pop_overrides %||% list()
      # Computed-at-submit WorldPop per team (team_area_versions.team_
      # worldpop_pop) -- same reasoning as dfa_worldpop_pop above.
      team_worldpop_by_ha[[han]] <<- tv$snap$team_worldpop_pop %||% list()
      geom
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0) NULL else do.call(rbind, parts)
  }, error = function(e) NULL)
  cat(sprintf('[export_debug] checkpoint: team_sf built | is.null=%s | nrow=%s\n',
              is.null(team_sf), if (is.null(team_sf)) 'NA' else nrow(team_sf)))

  if (is.null(ha_sf) || nrow(ha_sf) == 0)
    stop('No health area geometry available for this version — nothing to print.')

  ha_sf <- sf::st_transform(sf::st_make_valid(ha_sf), 4326)
  if (!is.null(team_sf) && nrow(team_sf) > 0) team_sf <- sf::st_transform(sf::st_make_valid(team_sf), 4326)
  if (!is.null(district_sf) && nrow(district_sf) > 0) district_sf <- sf::st_transform(sf::st_make_valid(district_sf), 4326)
  cat(sprintf('[export_debug] checkpoint: geometry transformed | ha_sf nrow=%d | district_sf is.null=%s\n',
              nrow(ha_sf), is.null(district_sf)))

  # Coordination sites — combine locked ODK snapshot + user-added sites,
  # filtered to those actually marked as coordination sites.
  fac_sf <- NULL
  fac_parts <- Filter(function(x) inherits(x, 'sf') && nrow(x) > 0, list(snap$odk_sf, snap$app_sf))
  if (length(fac_parts) > 0) {
    combined <- do.call(rbind, fac_parts)
    cat(sprintf('[export_debug] FAC RAW: nrow before coordination-site filter=%d | has polio_sia_coordination_site col=%s | names=%s\n',
                nrow(combined), 'polio_sia_coordination_site' %in% names(combined),
                paste(names(combined), collapse = ', ')))
    # NOTE: this filter is a no-op (every facility passes through
    # unfiltered) whenever the column is missing from combined -- the
    # cat() above will show exactly that if it's ever the case, rather
    # than silently showing every facility with no visible sign why.
    if ('polio_sia_coordination_site' %in% names(combined))
      combined <- combined[!is.na(combined$polio_sia_coordination_site) &
                             combined$polio_sia_coordination_site == 'Yes', , drop = FALSE]
    cat(sprintf('[export_debug] FAC RAW: nrow after coordination-site filter=%d\n', nrow(combined)))
    if (nrow(combined) > 0) fac_sf <- sf::st_transform(combined, 4326)
  }
  cat(sprintf('[export_debug] checkpoint: fac_sf built | is.null=%s | nrow=%s\n',
              is.null(fac_sf), if (is.null(fac_sf)) 'NA' else nrow(fac_sf)))

  # IDP settlements — stored as a plain data frame with lon/lat
  idp_sf <- NULL
  cat(sprintf('[export_debug] IDP RAW: is.null(snap$idp_settlements)=%s | class=%s | nrow=%s | names=%s\n',
              is.null(snap$idp_settlements),
              paste(class(snap$idp_settlements), collapse = '/'),
              if (is.null(snap$idp_settlements)) 'NA' else nrow(snap$idp_settlements),
              if (is.null(snap$idp_settlements)) 'NA' else paste(names(snap$idp_settlements), collapse = ', ')))
  if (!is.null(snap$idp_settlements) && nrow(snap$idp_settlements) > 0 &&
      all(c('lon', 'lat') %in% names(snap$idp_settlements))) {
    idp_sf <- sf::st_as_sf(snap$idp_settlements, coords = c('lon', 'lat'), crs = 4326, remove = FALSE)
  }
  cat(sprintf('[export_debug] checkpoint: idp_sf built | is.null=%s | nrow=%s\n',
              is.null(idp_sf), if (is.null(idp_sf)) 'NA' else nrow(idp_sf)))

  # Population overlay -- reuses the app's own make_population_overlay_sf()
  # (health_area_helpers.R) directly, so the export gets the identical
  # quantile-bucketed colors the in-app map shows, not a second
  # implementation of that bucketing logic. Computed once at district
  # extent; each health-area detail page clips it to that page's own
  # extent below.
  pop_sf <- NULL
  if (isTRUE(show_pop) && !is.null(district_sf) && nrow(district_sf) > 0 && !is.null(u5_rast)) {
    pop_sf <- tryCatch(make_population_overlay_sf(district_sf = district_sf, u5_rast = u5_rast),
                       error = function(e) NULL)
  }
  cat(sprintf('[export_debug] checkpoint: pop_sf built | is.null=%s | nrow=%s\n',
              is.null(pop_sf), if (is.null(pop_sf)) 'NA' else nrow(pop_sf)))

  # Campaign extent layer -- previously only used for the population
  # summary tables' urban/rural split; now ALSO drawn as a map context
  # layer (see .district_overview_map()/.health_area_detail_map()'s own
  # comments), matching "urban extents ... same as all the other maps".
  # fetch_campaign_extent_for_district() itself already handles the
  # admin-configurable, per-campaign URL and falls back to no data (NULL)
  # rather than erroring, so this only needs its own tryCatch as a second
  # layer of defense against something unexpected in the fetch/cache path.
  urban_sf <- if (!is.null(district_sf) && nrow(district_sf) > 0)
    tryCatch(fetch_campaign_extent_for_district(district_sf, campaign_id), error = function(e) {
      cat('[export_debug] fetch_campaign_extent_for_district ERRORED:', conditionMessage(e), '\n')
      NULL
    }) else NULL
  cat(sprintf('[export_debug] checkpoint: urban_sf fetched | is.null=%s | nrow=%s\n',
              is.null(urban_sf), if (is.null(urban_sf)) 'NA' else nrow(urban_sf)))

  # Subdivisions -- context-only reference layer, same as every other
  # map in the app already shows.
  subdiv_sf <- if (!is.null(district_sf) && nrow(district_sf) > 0)
    tryCatch(fetch_subdivisions_for_district(district_sf), error = function(e) {
      cat('[export_debug] fetch_subdivisions_for_district ERRORED:', conditionMessage(e), '\n')
      NULL
    }) else NULL
  cat(sprintf('[export_debug] checkpoint: subdiv_sf fetched | is.null=%s | nrow=%s\n',
              is.null(subdiv_sf), if (is.null(subdiv_sf)) 'NA' else nrow(subdiv_sf)))

  # Zone/region for the header subtitle -- snap$district_boundary_sf
  # carries no zone/region attributes of its own (it's boundary
  # geometry only), so this looks them up from districts_shp directly
  # by name, the same source mod_facility_tab.R's own district_base()
  # already trusts for this district's identity.
  zr <- tryCatch(
    districts_shp_for_campaign(campaign_id) |> sf::st_drop_geometry() |>
      dplyr::filter(district_name == !!district_name) |> dplyr::slice(1),
    error = function(e) NULL
  )
  zone_val   <- zr$zone_name %||% ''
  region_val <- zr$region_name %||% ''
  cat(sprintf('[export_debug] checkpoint: zone/region resolved | zone="%s" | region="%s"\n', zone_val, region_val))

  area_names <- setdiff(unique(as.character(ha_sf$dfa_name)), extra_dfa_names)
  cat(sprintf('[export_debug] checkpoint: area_names = %s\n', paste(area_names, collapse = ', ')))

  # Per-health-area raw stats, computed ONCE up front -- reused both to
  # sum into the district overview's own cards and per-area inside a
  # full PDF's page loop, rather than recomputing the same exact_extract()
  # call twice for every health area.
  ha_stats_list <- setNames(lapply(area_names, function(nm) {
    cat(sprintf('[export_debug] checkpoint: computing ha_stats for "%s"\n', nm))
    poly <- ha_sf[ha_sf$dfa_name == nm, , drop = FALSE]
    team_one <- if (!is.null(team_sf) && 'health_area' %in% names(team_sf))
      team_sf[team_sf$health_area == nm, , drop = FALSE] else NULL
    st <- .compute_ha_stats(nm, poly, team_one, team_targets, u5_rast, campaign_id, dfa_worldpop_pop[[nm]])
    cat(sprintf('[export_debug] checkpoint: ha_stats for "%s" done | wp_pop=%s | field_pop=%s | n_teams=%s\n',
                nm, st$wp_pop %||% NA, st$field_pop %||% NA, st$n_teams %||% NA))
    st
  }), area_names)
  cat('[export_debug] checkpoint: ha_stats_list complete for all areas\n')

  list(ha_sf = ha_sf, district_sf = district_sf, team_sf = team_sf, team_targets = team_targets,
       fac_sf = fac_sf, idp_sf = idp_sf, pop_sf = pop_sf, urban_sf = urban_sf, subdiv_sf = subdiv_sf,
       zone_val = zone_val, region_val = region_val, area_names = area_names,
       ha_stats_list = ha_stats_list, u5_rast = u5_rast,
       team_field_overrides_by_ha = team_field_overrides_by_ha,
       dfa_worldpop_pop = dfa_worldpop_pop, team_worldpop_by_ha = team_worldpop_by_ha)
}

build_printable_maps_pdf <- function(file, version, district_name, campaign_id,
                                     campaign_name = '', paper = 'a4_landscape',
                                     basemap = 'none', u5_rast = NULL, show_pop = FALSE,
                                     zoom = NULL,
                                     page_width_in = NULL, page_height_in = NULL,
                                     progress_fn = NULL) {
  # zoom: NULL = automatic per page (.auto_basemap_zoom()), else a fixed
  # tile zoom for every page. progress_fn(done, total, label), if given,
  # is called as each page is drawn. Returns the number of pages.

  if (!requireNamespace('tmap', quietly = TRUE))
    stop('The tmap package is required for printable export.')
  if (!requireNamespace('gridExtra', quietly = TRUE))
    stop('The gridExtra package is required for printable export tables.')

  tmap::tmap_mode('plot')

  ctx <- .prepare_export_context(version, district_name, campaign_id, u5_rast, show_pop)
  ha_sf <- ctx$ha_sf; district_sf <- ctx$district_sf; team_sf <- ctx$team_sf
  team_targets <- ctx$team_targets; fac_sf <- ctx$fac_sf; idp_sf <- ctx$idp_sf
  pop_sf <- ctx$pop_sf; urban_sf <- ctx$urban_sf; subdiv_sf <- ctx$subdiv_sf
  zone_val <- ctx$zone_val; region_val <- ctx$region_val
  area_names <- ctx$area_names; ha_stats_list <- ctx$ha_stats_list
  u5_rast <- ctx$u5_rast
  team_field_overrides_by_ha <- ctx$team_field_overrides_by_ha
  dfa_worldpop_pop <- ctx$dfa_worldpop_pop
  team_worldpop_by_ha <- ctx$team_worldpop_by_ha

  # page_width_in/page_height_in (free-form, in inches) override the
  # named paper preset when given -- the preset stays as the default
  # for callers that don't care to customize (e.g. any older caller
  # still passing only `paper`).
  size <- if (!is.null(page_width_in) && !is.null(page_height_in))
    c(width = page_width_in, height = page_height_in)
  else .PRINT_PAGE_SIZES_IN[[paper]] %||% .PRINT_PAGE_SIZES_IN$a4_landscape
  basemaps <- .expand_basemaps(basemap)
  frame <- .map_frame_in(unname(size['width']), unname(size['height']))
  zoom_for <- function(shape) if (is.null(zoom)) .auto_basemap_zoom(shape, frame[['w']], frame[['h']]) else zoom

  # Page count for progress: overview + detail map per basemap, summary
  # table, and a team table per health area that has team areas (an
  # estimate if a long table splits across pages).
  n_team_tables <- sum(vapply(area_names, function(nm)
    !is.null(team_sf) && 'health_area' %in% names(team_sf) && any(team_sf$health_area == nm), logical(1)))
  n_pages_est <- length(basemaps) * (1 + length(area_names)) + 1 + n_team_tables
  pages_done <- 0L
  page_done <- function(label) {
    pages_done <<- pages_done + 1L
    if (!is.null(progress_fn)) progress_fn(pages_done, n_pages_est, label)
  }

  grDevices::pdf(file, width = unname(size['width']), height = unname(size['height']))
  on.exit(grDevices::dev.off(), add = TRUE)
  cat('[export_debug] checkpoint: pdf device opened\n')

  # District overview page -- IDP count is the WHOLE district's IDP
  # points (not per-area), wp_pop/field_pop/n_teams are sums across
  # every health area's own stats above.
  idp_count_district <- if (is.null(idp_sf)) 0 else nrow(idp_sf)
  district_wp_pop    <- sum(vapply(ha_stats_list, function(s) s$wp_pop %||% 0, numeric(1)), na.rm = TRUE)
  district_field_pop <- {
    fp <- vapply(ha_stats_list, function(s) s$field_pop %||% NA_real_, numeric(1))
    if (all(is.na(fp))) NA_real_ else sum(fp, na.rm = TRUE)
  }
  district_n_teams <- sum(vapply(ha_stats_list, function(s) s$n_teams %||% 0, numeric(1)), na.rm = TRUE)
  cat('[export_debug] checkpoint: district-level card values computed\n')
  overview_cards <- .build_stat_cards(
    idp_count_district, district_wp_pop, district_field_pop, district_n_teams,
    extra_first = list(value = as.character(length(area_names)), label = 'Health Areas')
  )
  # One overview page per selected basemap.
  for (bm in basemaps) {
    cat(sprintf('[export_debug] checkpoint: about to build .district_overview_map() tmap object (basemap=%s)\n', bm))
    overview_tm <- .district_overview_map(ha_sf, district_sf, fac_sf, idp_sf, district_name, campaign_name, .resolve_basemap(bm), bm, pop_sf, subdiv_sf, urban_sf, zoom_for(ha_sf))
    cat('[export_debug] checkpoint: .district_overview_map() tmap object built OK -- about to print via .print_chrome_page()\n')
    .print_chrome_page(
      overview_tm,
      title = district_name, campaign_name = campaign_name, zone = zone_val, region = region_val, district = district_name,
      cards = overview_cards, basemap = bm,
      legend_items = .legend_items(bm, fac = .has_rows(fac_sf), idp = .has_rows(idp_sf),
                                   subdiv = .has_rows(subdiv_sf), urban = .has_rows(urban_sf)),
      page_width_in = unname(size['width']), page_height_in = unname(size['height'])
    )
    page_done(paste('Overview map', if (length(basemaps) > 1) paste0('(', bm, ')') else ''))
    cat('[export_debug] checkpoint: district overview page printed OK\n')
  }

  # Same header/footer chrome as the map pages, for every table page.
  table_chrome <- list(campaign_name = campaign_name, zone = zone_val, region = region_val, district = district_name,
                       basemap = basemaps, page_width_in = unname(size['width']), page_height_in = unname(size['height']))

  summary_df <- .build_health_area_summary_table(ha_sf, team_targets, u5_rast, campaign_id, urban_sf, idp_sf, dfa_worldpop_pop)
  cat('[export_debug] checkpoint: summary_df built OK\n')
  .print_table_page(summary_df, paste0(district_name, ' \u2014 Health Area Summary'), table_chrome)
  page_done('Health area summary table')
  cat('[export_debug] checkpoint: summary table page printed OK\n')

  for (nm in area_names) {
    cat(sprintf('[export_debug] === starting detail page loop for "%s" ===\n', nm))
    ha_one   <- ha_sf[ha_sf$dfa_name == nm, , drop = FALSE]
    team_one <- if (!is.null(team_sf) && 'health_area' %in% names(team_sf))
      team_sf[team_sf$health_area == nm, , drop = FALSE] else NULL
    fac_one  <- .points_within(fac_sf, ha_one)
    idp_one  <- .points_within(idp_sf, ha_one)
    cat(sprintf('[export_debug] checkpoint "%s": ha_one/team_one/fac_one/idp_one clipped\n', nm))
    # Clipped to this health area's own extent, same tryCatch-guarded
    # pattern as everything else pop_sf touches -- a clip failure (e.g.
    # a still-invalid geometry st_make_valid didn't fully resolve) means
    # this one page just prints without the overlay, not that the whole
    # export fails.
    pop_one <- if (!is.null(pop_sf) && nrow(pop_sf) > 0)
      tryCatch(sf::st_intersection(pop_sf, sf::st_geometry(ha_one) |> sf::st_union()), error = function(e) NULL)
      else NULL
    cat(sprintf('[export_debug] checkpoint "%s": pop_one clipped\n', nm))
    # Same clip pattern as pop_one above -- subdivisions/urban extent
    # shown as context on this health area's own map, not the whole
    # district's worth.
    subdiv_one <- if (!is.null(subdiv_sf) && nrow(subdiv_sf) > 0)
      tryCatch(sf::st_intersection(subdiv_sf, sf::st_geometry(ha_one) |> sf::st_union()), error = function(e) NULL)
      else NULL
    cat(sprintf('[export_debug] checkpoint "%s": subdiv_one clipped\n', nm))
    urban_one <- if (!is.null(urban_sf) && nrow(urban_sf) > 0)
      tryCatch(sf::st_intersection(urban_sf, sf::st_geometry(ha_one) |> sf::st_union()), error = function(e) NULL)
      else NULL
    cat(sprintf('[export_debug] checkpoint "%s": urban_one clipped\n', nm))

    st <- ha_stats_list[[nm]]
    ha_cards <- .build_stat_cards(
      if (is.null(idp_one)) 0 else nrow(idp_one), st$wp_pop, st$field_pop, st$n_teams
    )
    for (bm in basemaps) {
      cat(sprintf('[export_debug] checkpoint "%s": about to build .health_area_detail_map() tmap object (basemap=%s)\n', nm, bm))
      detail_tm <- .health_area_detail_map(ha_one, team_one, fac_one, idp_one, nm, district_name, .resolve_basemap(bm), bm, pop_one, subdiv_one, urban_one, zoom_for(ha_one))
      cat(sprintf('[export_debug] checkpoint "%s": .health_area_detail_map() tmap object built OK -- about to print\n', nm))
      .print_chrome_page(
        detail_tm,
        title = paste0(district_name, ' \u2014 ', nm), campaign_name = campaign_name, zone = zone_val, region = region_val, district = district_name,
        cards = ha_cards, basemap = bm,
        legend_items = .legend_items(bm, team = .has_rows(team_one), fac = .has_rows(fac_one), idp = .has_rows(idp_one),
                                     subdiv = .has_rows(subdiv_one), urban = .has_rows(urban_one)),
        page_width_in = unname(size['width']), page_height_in = unname(size['height'])
      )
      page_done(paste(nm, if (length(basemaps) > 1) paste0('(', bm, ')') else ''))
      cat(sprintf('[export_debug] checkpoint "%s": detail page printed OK\n', nm))
    }

    if (!is.null(team_one) && nrow(team_one) > 0) {
      field_target <- .unwrap_num(team_targets[[nm]]$target_pop)
      team_df <- .build_team_summary_table(team_one, field_target, u5_rast, urban_sf, team_field_overrides_by_ha[[nm]], team_worldpop_by_ha[[nm]])
      if (!is.null(team_df)) .print_table_page(team_df, paste0(district_name, ' \u2014 ', nm, ' \u2014 Team Summary'), table_chrome)
      page_done(paste(nm, 'team table'))
      cat(sprintf('[export_debug] checkpoint "%s": team summary table done\n', nm))
    }

    cat(sprintf('[export_debug] === finished detail page loop for "%s" ===\n', nm))
  }

  cat('[export_debug] checkpoint: ALL PAGES COMPLETE -- returning\n')
  invisible(pages_done)
}

#' Fast single-page preview: just the district overview page (map +
#' header/cards/legend/footer chrome), rendered to a PNG instead of the
#' full multi-page PDF. Built for the export tab's live preview -- as
#' page width/height/zoom/basemap controls change, regenerating the
#' FULL PDF (population raster extraction, every health area's own
#' detail page, team/IDP tables) on every change would be far too slow
#' to feel live; this skips straight to just the one page that's
#' actually shown, reusing .prepare_export_context() so the underlying
#' data-fetch logic can never drift from what the real PDF export uses.
#'
#' @param file            output PNG path
#' @param version,district_name,campaign_id,campaign_name,u5_rast,show_pop
#'                        same meaning as build_printable_maps_pdf()
#' @param width_in,height_in  page dimensions in inches (free-form, not
#'                        a named paper preset -- this is specifically
#'                        for the export tab's adjustable-dimensions UI)
#' @param basemap         same as build_printable_maps_pdf(); only the first is previewed
#' @param zoom             basemap tile zoom level (only matters when
#'                        basemap != "none")
#' @param png_res          PNG resolution in DPI -- lower than a real
#'                        print export's implicit resolution, since
#'                        this is an on-screen preview, not the final
#'                        deliverable; keeps preview generation fast.
build_printable_overview_preview_png <- function(file, version, district_name, campaign_id,
                                                 campaign_name = '', width_in = 11.69, height_in = 8.27,
                                                 basemap = 'none', zoom = NULL,
                                                 u5_rast = NULL, show_pop = FALSE, png_res = 100) {
  if (!requireNamespace('tmap', quietly = TRUE))
    stop('The tmap package is required for printable export.')

  tmap::tmap_mode('plot')

  ctx <- .prepare_export_context(version, district_name, campaign_id, u5_rast, show_pop)
  # Several basemaps selected -> preview the first one's overview page only.
  basemap <- .expand_basemaps(basemap)[1]
  basemap_provider <- .resolve_basemap(basemap)

  idp_count_district <- if (is.null(ctx$idp_sf)) 0 else nrow(ctx$idp_sf)
  district_wp_pop    <- sum(vapply(ctx$ha_stats_list, function(s) s$wp_pop %||% 0, numeric(1)), na.rm = TRUE)
  district_field_pop <- {
    fp <- vapply(ctx$ha_stats_list, function(s) s$field_pop %||% NA_real_, numeric(1))
    if (all(is.na(fp))) NA_real_ else sum(fp, na.rm = TRUE)
  }
  district_n_teams <- sum(vapply(ctx$ha_stats_list, function(s) s$n_teams %||% 0, numeric(1)), na.rm = TRUE)
  overview_cards <- .build_stat_cards(
    idp_count_district, district_wp_pop, district_field_pop, district_n_teams,
    extra_first = list(value = as.character(length(ctx$area_names)), label = 'Health Areas')
  )
  overview_legend <- .legend_items(basemap, fac = .has_rows(ctx$fac_sf), idp = .has_rows(ctx$idp_sf),
                                   subdiv = .has_rows(ctx$subdiv_sf), urban = .has_rows(ctx$urban_sf))
  if (is.null(zoom)) {
    frame <- .map_frame_in(width_in, height_in)
    zoom <- .auto_basemap_zoom(ctx$ha_sf, frame[['w']], frame[['h']])
  }
  overview_tm <- .district_overview_map(ctx$ha_sf, ctx$district_sf, ctx$fac_sf, ctx$idp_sf, district_name, campaign_name,
                                        basemap_provider, basemap, ctx$pop_sf, ctx$subdiv_sf, ctx$urban_sf, zoom)

  grDevices::png(file, width = width_in, height = height_in, units = 'in', res = png_res)
  on.exit(grDevices::dev.off(), add = TRUE)
  .print_chrome_page(
    overview_tm,
    title = district_name, campaign_name = campaign_name, zone = ctx$zone_val, region = ctx$region_val, district = district_name,
    cards = overview_cards, legend_items = overview_legend, basemap = basemap,
    page_width_in = width_in, page_height_in = height_in
  )
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

#' Same total as .polygon_u5_population(), split into urban/rural
#' components using the district's urban-areas layer (fetch_urban_areas_
#' for_district(), subdivision_helpers.R). Returns a data.frame with one
#' row per poly_sf row: total, urban, rural (urban + rural == total by
#' construction -- rural is computed as total minus the urban-clipped
#' extraction, not as a second, separate extraction against the
#' rural remainder geometry, so the two can't drift apart by a few
#' population units the way two independent raster sums risked).
#'
#' Reporting-layer only, same as every other consumer of this urban/rural
#' split this session -- does not feed back into the BFS boundary-
#' generation algorithm's own target-pop math.
#'
#' urban_sf may be NULL (no urban-areas data available for this district,
#' or the layer fetch failed) -- every row's urban component is then 0
#' and rural equals the full total, which is the same as saying "treat
#' this whole area as rural" until real urban-areas data exists for it,
#' rather than fail the whole table.
.polygon_u5_population_urban_rural <- function(poly_sf, u5_rast, urban_sf) {
  n <- if (is.null(poly_sf)) 0 else nrow(poly_sf)
  total <- .polygon_u5_population(poly_sf, u5_rast)
  if (n == 0) return(data.frame(total = numeric(0), urban = numeric(0), rural = numeric(0)))

  if (is.null(urban_sf) || nrow(urban_sf) == 0) {
    return(data.frame(total = total, urban = rep(0, n), rural = total))
  }

  # Each row's urban extraction is computed with its OWN separate
  # .polygon_u5_population() call, deliberately not batched into one
  # multi-row extraction. poly_sf can genuinely have more than one row
  # for what's conceptually a single health area (e.g. separate ring
  # parts after an island got pinched off during boundary editing) --
  # if a batched call errored on one row with no urban overlap at all
  # (an empty placeholder polygon), the whole call's tryCatch would zero
  # out urban population for EVERY row in that call, not just the
  # offending one, silently misreporting a genuinely mixed health area
  # as entirely rural. Per-row calls make that impossible by
  # construction -- one row's failure can only ever produce NA for that
  # row.
  urban_union <- tryCatch(
    sf::st_union(safe_make_valid(sf::st_transform(urban_sf, sf::st_crs(poly_sf)))),
    error = function(e) NULL
  )
  urban_vals <- vapply(seq_len(n), function(i) {
    if (is.null(urban_union)) return(NA_real_)
    tryCatch({
      one_clip <- suppressWarnings(sf::st_intersection(sf::st_geometry(poly_sf)[i], urban_union))
      one_geom <- if (length(one_clip) == 0 || sf::st_is_empty(one_clip[[1]])) sf::st_polygon() else one_clip[[1]]
      one_sf   <- sf::st_sf(geometry = sf::st_sfc(list(one_geom), crs = sf::st_crs(poly_sf)))
      .polygon_u5_population(one_sf, u5_rast)[1]
    }, error = function(e) NA_real_)
  }, numeric(1))

  urban_vals[is.na(urban_vals)] <- 0
  # Never let a rounding/geometry-edge artifact push urban above total or
  # rural below zero -- pmin/pmax clamp is a defensive floor, not the
  # primary correctness mechanism (rural = total - urban already
  # guarantees they sum correctly; this only guards the individual
  # components' own sign/bound).
  urban_vals <- pmin(pmax(urban_vals, 0), total)
  data.frame(total = total, urban = urban_vals, rural = total - urban_vals)
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
#' population, WorldPop-recommended team count, and field-requested team
#' count -- all four shown separately (never merged into one "whichever
#' is set" number), matching the Team Planning Targets modal's own
#' columns (mod_health_area_tab.R) so a person sees the identical
#' figures whether they're looking at the modal or this table. Also
#' splits population and recommended-team-count into urban/rural
#' components (reporting only -- see compute_n_teams()'s own doc comment
#' for why this doesn't change what generation actually targets).
#' urban_sf may be NULL (no urban-areas layer available for this
#' district) -- every area's urban component is then 0, rural equals the
#' unstratified total, rather than failing the whole table.
.build_health_area_summary_table <- function(ha_sf, team_targets, u5_rast, campaign_id, urban_sf = NULL, idp_sf = NULL, stored_worldpop = NULL) {
  area_names <- setdiff(unique(as.character(ha_sf$dfa_name)), extra_dfa_names)
  rows <- lapply(area_names, function(nm) {
    poly    <- ha_sf[ha_sf$dfa_name == nm, , drop = FALSE]
    # Prefer the COMPUTED-AT-SUBMIT value (stored_worldpop, from
    # mapping_versions.dfa_worldpop_pop) over live raster extraction --
    # re-extracting WorldPop from the raster on every single read
    # (every export, every popup open) was real, avoidable cost for a
    # number that only changes when the boundary itself changes.
    # Live extraction is kept ONLY as a fallback for a version
    # submitted before this was computed and stored (stored_worldpop[[nm]]
    # missing/NA in that case), not the default path anymore.
    wp_val <- .unwrap_num(stored_worldpop[[nm]])
    if (is.na(wp_val)) {
      # Urban/rural split removed from this table's output -- not
      # requested, only the combined total is shown here now. Still
      # computed via .polygon_u5_population_urban_rural() rather than
      # the plain .polygon_u5_population() so the WorldPop total itself
      # stays identical to what that function already produces
      # elsewhere (the per-team table), not a second, potentially-
      # drifting computation.
      split  <- .polygon_u5_population_urban_rural(poly, u5_rast, urban_sf)
      wp_val <- sum(split$total, na.rm = TRUE)
    }

    idp_count <- if (is.null(idp_sf) || nrow(idp_sf) == 0) 0 else {
      one <- tryCatch(.points_within(idp_sf, poly), error = function(e) NULL)
      if (is.null(one)) 0 else nrow(one)
    }

    tgt             <- team_targets[[nm]]
    field_pop       <- .unwrap_num(tgt$target_pop)
    req_teams       <- .unwrap_num(tgt$requested_teams)
    # Recommended Teams is deliberately a single, non-stratified figure
    # -- a stratified urban/rural version of this was tried and
    # reverted (see mod_facility_tab.R's outreach_counts_card for the
    # full reasoning).
    recommended_teams <- tryCatch(compute_n_teams(wp_val, campaign_id = campaign_id), error = function(e) NA_integer_)

    data.frame(
      `Health Area`             = nm,
      `Target Pop (WorldPop)`   = .fmt_pop(wp_val),
      `Target Pop (Field)`       = .fmt_field(field_pop),
      `IDP Settlements`          = as.character(idp_count),
      `Recommended Teams`        = if (is.na(recommended_teams)) '\u2013' else as.character(as.integer(recommended_teams)),
      `Field Requested Teams`    = if (is.na(req_teams) || req_teams <= 0) '\u2013' else as.character(as.integer(req_teams)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Raw-numeric version of one health area's card stats (WorldPop pop,
# field target pop, team count) -- separate from .build_health_area_
# summary_table() above because that function returns pre-formatted
# DISPLAY STRINGS (via .fmt_pop()/.fmt_field(), e.g. "19,114" or "–"),
# not usable for the district-level SUM this feeds into card values.
# n_teams prefers the ACTUAL count of submitted team-area polygons for
# this health area when one exists, falling back to the recommended
# (population-based) count only when it doesn't -- per explicit
# instruction, not the recommended count unconditionally.
# stored_wp is the computed-at-submit WorldPop figure (mapping_versions.
# dfa_worldpop_pop), preferred for the same reasons as in .build_health_
# area_summary_table() -- and so the cards and the summary table always
# show the same number. Live extraction is only the fallback for older
# versions submitted before that column existed.
.compute_ha_stats <- function(nm, poly, team_one, team_targets, u5_rast, campaign_id, stored_wp = NULL) {
  wp_val <- .unwrap_num(stored_wp)
  if (is.na(wp_val)) wp_val <- tryCatch({
    split <- .polygon_u5_population_urban_rural(poly, u5_rast, NULL)
    sum(split$total, na.rm = TRUE)
  }, error = function(e) NA_real_)
  tgt <- team_targets[[nm]]
  field_pop <- .unwrap_num(tgt$target_pop)
  actual_teams <- if (!is.null(team_one) && nrow(team_one) > 0 && 'dfa_name' %in% names(team_one)) {
    length(setdiff(unique(as.character(team_one$dfa_name)), extra_dfa_names))
  } else NA_integer_
  recommended_teams <- tryCatch(as.integer(compute_n_teams(wp_val, campaign_id = campaign_id)), error = function(e) NA_integer_)
  n_teams <- if (!is.na(actual_teams) && actual_teams > 0) actual_teams else recommended_teams
  list(wp_pop = wp_val, field_pop = field_pop, n_teams = n_teams)
}

# Card list for one page -- shared value/label formatting so it can
# never drift between the district overview and health-area detail
# pages. extra_first is the one card unique to the overview page
# (# health areas); detail pages omit it.
.build_stat_cards <- function(idp_count, wp_pop, field_pop, n_teams, extra_first = NULL) {
  # Field pop is the primary value whenever it's actually set; WorldPop
  # is the fallback primary value when it isn't, and becomes a small
  # reference subtext underneath ONLY when field pop is what's actually
  # shown as the big number -- not shown as "X / Y", which reads as a
  # fraction rather than "here's the number, and here's where it came
  # from / how it compares."
  field_available <- !is.na(field_pop)
  pop_value   <- if (field_available) .fmt_field(field_pop) else .fmt_pop(wp_pop)
  pop_subtext <- if (field_available) paste0('WorldPop est: ', .fmt_pop(wp_pop)) else NULL
  cards <- list()
  if (!is.null(extra_first)) cards[[length(cards) + 1]] <- extra_first
  cards[[length(cards) + 1]] <- list(value = as.character(idp_count), label = 'IDP Settlements')
  cards[[length(cards) + 1]] <- list(value = pop_value, label = 'Target Population', subtext = pop_subtext)
  cards[[length(cards) + 1]] <- list(value = as.character(n_teams %||% '\u2013'), label = 'Teams')
  cards
}


#' One row per team within a single health area: team name, WorldPop
#' population (from that team's own drawn polygon), and a field target
#' population -- there is no per-team field figure in the data model
#' (only a whole-health-area one), so this is the health area's own
#' field target divided evenly across its teams, shown as each team's
#' implied share. NA (blank) if the health area itself has no field
#' target set. Also splits each team's own population into urban/rural
#' components, same urban_sf/reporting-only caveat as the health-area
#' table above.
.build_team_summary_table <- function(team_one, health_area_field_target, u5_rast, urban_sf = NULL, team_field_overrides = NULL, stored_worldpop = NULL) {
  if (is.null(team_one) || nrow(team_one) == 0) return(NULL)
  team_names <- setdiff(unique(as.character(team_one$dfa_name)), extra_dfa_names)
  if (length(team_names) == 0) return(NULL)

  # WorldPop computed for every team FIRST, before any field-population
  # fallback logic -- needed because the fallback below is proportional
  # to each team's own WorldPop share, not an even split, so every
  # team's figure has to exist before any one team's share can be
  # calculated.
  #
  # Prefers the COMPUTED-AT-SUBMIT value (stored_worldpop, from
  # team_area_versions.team_worldpop_pop) over live raster extraction --
  # re-extracting on every single read was real, avoidable cost for a
  # number that only changes when the boundary itself changes. Live
  # extraction is kept ONLY as a fallback for a version submitted before
  # this was computed and stored.
  wp_vals <- vapply(team_names, function(tn) {
    stored <- .unwrap_num(stored_worldpop[[tn]])
    if (!is.na(stored)) return(stored)
    poly  <- team_one[team_one$dfa_name == tn, , drop = FALSE]
    # Urban/rural split not shown in this table's output -- only the
    # combined total, same as .build_health_area_summary_table(). Still
    # computed via .polygon_u5_population_urban_rural() rather than the
    # plain .polygon_u5_population() so the WorldPop total itself stays
    # identical either way.
    split <- .polygon_u5_population_urban_rural(poly, u5_rast, urban_sf)
    sum(split$total, na.rm = TRUE)
  }, numeric(1))
  names(wp_vals) <- team_names
  total_wp <- sum(wp_vals, na.rm = TRUE)

  # Fallback for teams that don't have their OWN field-verified
  # population entered (team_field_overrides, persisted from
  # mod_team_area_tab.R's per-team Field Population column): each
  # team's share of the health area's field target IN THE SAME
  # PROPORTION as its own share of the health area's total WorldPop --
  # e.g. a team with 40% of the WorldPop total gets 40% of the field
  # target, not container-share (n_teams-way) split. Explicit instruction,
  # not a guess -- an even split was flagged as wrong specifically
  # because it doesn't reflect how population is actually distributed
  # across a health area's teams. Falls back to an even split only if
  # total_wp is 0 (every team came back with zero WorldPop, so a
  # proportional share is undefined -- avoids a divide-by-zero, not a
  # silent return to the old behavior in the normal case).
  per_team_field_fallback <- setNames(rep(NA_real_, length(team_names)), team_names)
  if (!is.na(health_area_field_target)) {
    if (total_wp > 0) {
      per_team_field_fallback <- (wp_vals / total_wp) * health_area_field_target
    } else if (length(team_names) > 0) {
      per_team_field_fallback <- setNames(rep(health_area_field_target / length(team_names), length(team_names)), team_names)
    }
  }

  rows <- lapply(team_names, function(tn) {
    wp_val <- wp_vals[[tn]]
    # .unwrap_num(), not plain as.numeric() -- team_field_overrides comes
    # from the same .from_json_db()-parsed, simplifyVector=FALSE JSON
    # structure as team_targets (see .unwrap_num()'s own docstring
    # above), so a per-team value here can arrive wrapped in its own
    # length-1 list (e.g. list(500), not 500). Plain as.numeric() on
    # that wrapped form -- or on NULL, the common case where a team has
    # no override at all -- either mis-evaluates or produces a length-
    # zero result, and the is.na() check right below then throws
    # "missing value where TRUE/FALSE needed" or "argument is of length
    # zero" depending on which. Confirmed as the actual cause of a real
    # production error, not hypothetical.
    own_field <- .unwrap_num(team_field_overrides[[tn]])
    field_val <- if (!is.na(own_field) && own_field >= 0) own_field else per_team_field_fallback[[tn]]
    data.frame(
      Team                     = tn,
      `Target Pop (WorldPop)` = .fmt_pop(wp_val),
      `Target Pop (Field)`     = .fmt_field(field_val),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Renders a data frame as table page(s) in the current pdf() device,
#' with the same header band and footer as the map pages. chrome is
#' list(campaign_name, zone, region, district, basemap, page_width_in,
#' page_height_in). Long tables are split across as many pages as needed
#' ("(1 of 2)" in the title) rather than running off the bottom.
#'
#' Drawn with grid.draw() on a page this function opens itself -- NOT
#' gridExtra::grid.arrange(), which opens its own new page on top of an
#' explicit grid.newpage() and left a blank page before every table.
.print_table_page <- function(df, title, chrome) {
  margin_in <- 0.25
  page_w <- chrome$page_width_in; page_h <- chrome$page_height_in
  body_h_in <- (page_h - 2 * margin_in) * (.PRINT_ROW_HEIGHTS[['map']] + .PRINT_ROW_HEIGHTS[['strip']])
  # ~0.3in per row at base_size 10 (text + cell padding), one row's worth
  # reserved for the column header.
  rows_per_page <- max(5L, as.integer(floor(body_h_in / 0.3)) - 1L)
  chunks <- split(seq_len(nrow(df)), ceiling(seq_len(nrow(df)) / rows_per_page))

  tt <- gridExtra::ttheme_default(
    base_size = 10,
    core    = list(bg_params = list(fill = c('white', '#f8fafc'), col = '#e2e8f0')),
    colhead = list(bg_params = list(fill = '#e2e8f0', col = '#cbd5e1'))
  )
  for (k in seq_along(chunks)) {
    page_title <- if (length(chunks) > 1) sprintf('%s (%d of %d)', title, k, length(chunks)) else title
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(x = margin_in / page_w, y = margin_in / page_h,
                                      width = 1 - 2 * margin_in / page_w, height = 1 - 2 * margin_in / page_h,
                                      just = c('left', 'bottom')))
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 4, heights = grid::unit(.PRINT_ROW_HEIGHTS, 'npc'))))
    .draw_header_band(page_title, chrome$campaign_name, chrome$zone, chrome$region, chrome$district)
    grid::pushViewport(grid::viewport(layout.pos.row = 2:3))
    tbl <- gridExtra::tableGrob(df[chunks[[k]], , drop = FALSE], rows = NULL, theme = tt)
    # Top-aligned under the header rather than centred in the body area.
    grid::pushViewport(grid::viewport(y = grid::unit(1, 'npc') - grid::unit(0.1, 'in'), just = 'top',
                                      width = sum(tbl$widths), height = sum(tbl$heights)))
    grid::grid.draw(tbl)
    grid::upViewport(2)
    .draw_footer_band(chrome$basemap)
    grid::upViewport(2)
  }
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

# =============================================================================
# Page chrome: header band, cards+legend strip, footer band -- drawn
# OUTSIDE the map's own plot area (never overlaid on top of it, per
# explicit instruction), each in its own thin grid row so the map row
# gets the large majority of the page. See .print_chrome_page() below
# for how these three combine with the map into one page.
# =============================================================================

# Row heights as fractions of the page, in top-to-bottom order:
# header, map, cards/legend strip, footer. Map is deliberately the large
# majority -- everything else is "extra info" that must never come at
# its expense, per explicit instruction.
.PRINT_ROW_HEIGHTS <- c(header = 0.085, map = 0.775, strip = 0.095, footer = 0.045)

.draw_header_band <- function(title, campaign_name, zone, region, district) {
  grid::pushViewport(grid::viewport(layout.pos.row = 1))
  # Big title, left-aligned, vertically centered in its own left column
  # so the logo (drawn into a separate right column below) never
  # competes for the same horizontal space.
  grid::pushViewport(grid::viewport(x = 0, width = 0.62, just = 'left',
                                    layout = grid::grid.layout(nrow = 2, heights = c(0.62, 0.38))))
  grid::pushViewport(grid::viewport(layout.pos.row = 1))
  grid::grid.text(title, x = 0.02, just = 'left', gp = grid::gpar(fontsize = 20, fontface = 'bold', col = '#1e293b'))
  grid::upViewport()
  grid::pushViewport(grid::viewport(layout.pos.row = 2))
  subtitle <- paste0('State: ', toupper(zone %||% '\u2013'), '   \u00b7   Region: ', toupper(region %||% '\u2013'),
                     '   \u00b7   District: ', toupper(district %||% '\u2013'))
  grid::grid.text(subtitle, x = 0.02, just = 'left', gp = grid::gpar(fontsize = 9.5, col = '#475569'))
  grid::upViewport(2)

  # Campaign badge and logo, side by side (badge left of logo) -- both
  # given their own fixed-width column on the right and vertically
  # centered at the SAME height, rather than stacked top/bottom as a
  # previous version had them (which worked but wasn't what was wanted
  # here). Logo gets a narrower column (0.16) since it's just the mark;
  # badge gets a wider one (0.22) immediately to its left for the
  # campaign name text.
  logo <- .get_gpei_logo()
  logo_col_w  <- 0.16
  badge_col_w <- 0.22

  if (!is.null(logo)) {
    grid::pushViewport(grid::viewport(x = 1, width = logo_col_w, just = 'right'))
    # grid.raster() with only height given auto-preserves the image's
    # own aspect ratio for width, so no manual aspect computation
    # needed here.
    grid::grid.raster(logo, x = 0.98, y = 0.5, just = c('right', 'center'),
                      height = grid::unit(0.5, 'npc'))
    grid::upViewport()
  }

  # Campaign name badge -- immediately left of the logo's column, where
  # the reference image had its "Targeted — Rounds X" ribbon. Width
  # sized to the text itself (capped) rather than a fixed box, so a
  # long campaign name doesn't clip.
  if (nzchar(campaign_name %||% '')) {
    grid::pushViewport(grid::viewport(x = 1 - logo_col_w, width = badge_col_w, just = 'right'))
    # grid.roundrect() is NOT a real base-grid drawing function (only
    # roundrectGrob() exists, as a grob constructor) -- grid.draw() is
    # what actually renders it.
    grid::grid.draw(grid::roundrectGrob(x = 0.9, y = 0.5, width = 0.9, height = 0.5, just = c('right', 'center'),
                                        r = grid::unit(0.15, 'snpc'),
                                        gp = grid::gpar(fill = .GPEI_BLUE, col = NA)))
    grid::grid.text(campaign_name, x = 0.85, y = 0.5, just = c('right', 'center'),
                    gp = grid::gpar(fontsize = 10, fontface = 'bold', col = 'white'))
    grid::upViewport()
  }
  grid::upViewport()
}

# Renders ONE stat card: a light grey rounded rectangle, a big number,
# and a small label underneath -- neutral grey/white per explicit
# instruction, not GPEI-colored (the badge above is the only branded
# color element on the page).
.draw_stat_card <- function(x, width, value, label, subtext = NULL) {
  grid::grid.draw(grid::roundrectGrob(x = x, y = 0.5, width = width * 0.92, height = 0.88, just = 'left',
                                      r = grid::unit(0.08, 'snpc'), gp = grid::gpar(fill = '#f1f5f9', col = '#e2e8f0')))
  # Three-line layout when subtext is given (big value, label, tiny
  # grey subtext below) vs the original two-line layout otherwise --
  # subtext is only ever the WorldPop estimate shown alongside a field
  # target value, so most cards (IDP count, Teams) stay two-line.
  if (!is.null(subtext)) {
    grid::grid.text(value, x = x + width * 0.46, y = 0.70, gp = grid::gpar(fontsize = 14, fontface = 'bold', col = '#1e293b'))
    grid::grid.text(label, x = x + width * 0.46, y = 0.42, gp = grid::gpar(fontsize = 7.5, col = '#64748b'))
    grid::grid.text(subtext, x = x + width * 0.46, y = 0.16, gp = grid::gpar(fontsize = 6.5, col = '#94a3b8'))
  } else {
    grid::grid.text(value, x = x + width * 0.46, y = 0.62, gp = grid::gpar(fontsize = 15, fontface = 'bold', col = '#1e293b'))
    grid::grid.text(label, x = x + width * 0.46, y = 0.28, gp = grid::gpar(fontsize = 7.5, col = '#64748b'))
  }
}

# cards: list of list(value=, label=). legend_items: list of
# list(swatch_col=, shape=('line'|'point'), label=).
.draw_cards_legend_strip <- function(cards, legend_items) {
  grid::pushViewport(grid::viewport(layout.pos.row = 3))
  legend_w <- 0.30

  # Boxed legend, left portion of the strip -- outside the map, per
  # explicit instruction (the old version drew this as a tmap-native
  # legend overlaid on the map itself).
  grid::pushViewport(grid::viewport(x = 0, width = legend_w, just = 'left'))
  grid::grid.rect(x = 0.03, width = 0.94, just = 'left', gp = grid::gpar(fill = 'white', col = '#cbd5e1'))
  n_items <- max(length(legend_items), 1)
  item_h  <- 1 / n_items
  for (i in seq_along(legend_items)) {
    it <- legend_items[[i]]
    y_center <- 1 - (i - 0.5) * item_h
    if (identical(it$shape, 'line')) {
      grid::grid.lines(x = c(0.08, 0.16), y = c(y_center, y_center),
                       gp = grid::gpar(col = it$swatch_col, lwd = it$lwd %||% 2, lty = it$lty %||% 'solid'))
    } else {
      grid::grid.points(x = 0.12, y = y_center, pch = it$pch %||% 19, size = grid::unit(0.5, 'char'), gp = grid::gpar(col = it$swatch_col))
    }
    grid::grid.text(it$label, x = 0.19, y = y_center, just = 'left', gp = grid::gpar(fontsize = 8, col = '#334155'))
  }
  grid::upViewport()

  # Cards, remaining width, evenly spaced.
  grid::pushViewport(grid::viewport(x = legend_w, width = 1 - legend_w, just = 'left'))
  n_cards <- max(length(cards), 1)
  card_w  <- 1 / n_cards
  for (i in seq_along(cards)) {
    .draw_stat_card((i - 1) * card_w, card_w, cards[[i]]$value, cards[[i]]$label, cards[[i]]$subtext)
  }
  grid::upViewport()
  grid::upViewport()
}

.draw_footer_band <- function(basemap) {
  grid::pushViewport(grid::viewport(layout.pos.row = 4))
  # basemap can be several (table pages credit every basemap the PDF uses).
  basemap_label <- paste(vapply(basemap %||% 'none', function(b) switch(b,
                          osm       = 'OpenStreetMap',
                          satellite = 'Esri Satellite',
                          topo      = 'OpenTopoMap',
                          'no basemap'), character(1)), collapse = ', ')
  current_month_year <- format(Sys.Date(), '%b %Y')
  sources <- paste0(
    'Sources: ', basemap_label, '; WorldPop 2025 (100m U5 grid); ',
    'CCCM IDP dataset, Nov 2025; MHFL, ', current_month_year, ' \u2014 WHO Somalia'
  )
  produced <- paste0('Produced: ', format(Sys.Date(), '%d %b %Y'))
  disclaimer <- 'Disclaimer: For campaign planning purposes only - boundaries and figures are estimates, not official statistics.'
  grid::grid.text(paste0(produced, '   \u00b7   ', disclaimer), x = 0.01, y = 0.72, just = 'left',
                  gp = grid::gpar(fontsize = 6.5, col = '#94a3b8'))
  grid::grid.text(sources, x = 0.01, y = 0.28, just = 'left', gp = grid::gpar(fontsize = 6.5, col = '#94a3b8'))
  grid::upViewport()
}

# Combines the three chrome bands above with a map (a tmap object) into
# ONE full page: header, map (via print(tm_obj, vp=...), tmap's own grid
# integration), cards+legend strip, footer. Called once per map page
# from build_printable_maps_pdf() -- replaces the old bare print(tm_obj)
# call, which gave tmap the entire page and relied on tm_title()/an
# overlaid legend for everything this now draws as separate chrome.
.print_chrome_page <- function(tm_obj, title, campaign_name, zone, region, district, cards, legend_items, basemap, page_width_in, page_height_in) {
  grid::grid.newpage()

  # 0.25in margin on all sides -- content area (everything below: header,
  # map, cards/legend strip, footer) is the full page minus this margin.
  # Margin is inset as an NPC fraction of the actual page size (rather
  # than a fixed fraction) so it holds at exactly 0.25in regardless of
  # what page dimensions are chosen -- a wider page needs a SMALLER npc
  # fraction to hold the same physical 0.25in, and vice versa.
  margin_in <- 0.25
  content_width_in  <- page_width_in - 2 * margin_in
  content_height_in <- page_height_in - 2 * margin_in
  margin_x_npc <- margin_in / page_width_in
  margin_y_npc <- margin_in / page_height_in
  grid::pushViewport(grid::viewport(
    x = margin_x_npc, y = margin_y_npc,
    width = 1 - 2 * margin_x_npc, height = 1 - 2 * margin_y_npc,
    just = c('left', 'bottom')
  ))

  grid::pushViewport(grid::viewport(layout = grid::grid.layout(
    nrow = 4, heights = grid::unit(.PRINT_ROW_HEIGHTS, 'npc')
  )))
  cat('[export_debug]   .print_chrome_page: page layout pushed -- drawing header\n')
  .draw_header_band(title, campaign_name, zone, region, district)
  cat('[export_debug]   .print_chrome_page: header drawn OK -- printing map\n')
  grid::pushViewport(grid::viewport(layout.pos.row = 2))
  # Third approach to embedding the map, after two that each produced a
  # real, confirmed bug on actual output: print(tm_obj, vp=...) created
  # an extra blank PDF page (tmap apparently still advances the page
  # internally even when handed a vp); tmap_grob()+grid.draw() then
  # produced the OPPOSITE problem -- the grob doesn't respect the given
  # viewport's bounds at all, so on detail pages (whose aspect ratio
  # differs from the overview page) it overflowed upward into the
  # header row, drawing the map ON TOP of the header/badge/logo, and on
  # every page it also left the map badly letterboxed with large blank
  # margins rather than filling row 2. Both were confirmed from actual
  # export output, not hypothetical.
  #
  # This third approach renders the tmap object to its OWN temporary
  # PNG device, sized in inches to exactly match row 2's physical
  # dimensions on the real page, then draws that PNG back in as a
  # grid.raster() -- the same technique already used for the GPEI logo.
  # A raster image has no ambiguity about viewport bounds: grid.raster()
  # always scales to exactly fill whatever viewport/width/height it's
  # given, so this is structurally immune to both bugs above, at the
  # cost of the map being a raster rather than vector in the final PDF
  # (a reasonable trade for a page whose reference layout itself was a
  # rasterized map book, not vector GIS output).
  #
  # Sized to content_width_in/content_height_in (the page MINUS the
  # 0.25in margin), not the raw page_width_in/page_height_in -- the
  # margin viewport pushed above already shrank row 2's actual on-page
  # footprint, so rendering the temp PNG at the full page size would
  # make the raster's own aspect ratio wrong for the space it actually
  # has to fill, distorting the map when grid.raster() stretches it in.
  map_height_in <- content_height_in * .PRINT_ROW_HEIGHTS['map']
  map_tmp <- tempfile(fileext = '.png')
  cat(sprintf('[export_debug]   .print_chrome_page: opening temp map PNG device (%.2f x %.2f in), basemap=%s\n',
              content_width_in, map_height_in, basemap %||% 'NULL'))
  grDevices::png(map_tmp, width = content_width_in, height = map_height_in, units = 'in', res = 150)
  # print(tm_obj) wrapped in its own tryCatch, and dev.off() called
  # UNCONDITIONALLY right after (not inside the tryCatch) -- guarantees
  # this temp device closes even if print() errors, so a map-render
  # failure (reported happening specifically with basemap = "none")
  # can never leave a dangling graphics device that corrupts the OUTER
  # pdf()/png() device's state for every page after it. Confirmed real
  # failure mode worth guarding against, not hypothetical.
  map_print_ok <- tryCatch({
    print(tm_obj)
    TRUE
  }, error = function(e) {
    cat('[export_debug]   .print_chrome_page: print(tm_obj) into temp PNG device ERRORED:', conditionMessage(e), '\n')
    FALSE
  })
  grDevices::dev.off()
  map_raster <- if (map_print_ok) tryCatch(png::readPNG(map_tmp), error = function(e) {
    cat('[export_debug]   .print_chrome_page: readPNG(map_tmp) ERRORED:', conditionMessage(e), '\n')
    NULL
  }) else NULL
  if (!is.null(map_raster)) {
    grid::grid.raster(map_raster, width = grid::unit(1, 'npc'), height = grid::unit(1, 'npc'))
  } else {
    # Map render failed -- show a visible placeholder instead of silently
    # leaving row 2 blank with no indication anything went wrong.
    grid::grid.rect(gp = grid::gpar(fill = '#fef2f2', col = '#fecaca'))
    grid::grid.text('Map could not be rendered for this page.', gp = grid::gpar(col = '#b91c1c', fontsize = 10))
  }
  unlink(map_tmp)
  cat('[export_debug]   .print_chrome_page: map printed OK -- drawing cards/legend strip\n')
  grid::upViewport()
  .draw_cards_legend_strip(cards, legend_items)
  cat('[export_debug]   .print_chrome_page: cards/legend strip drawn OK -- drawing footer\n')
  .draw_footer_band(basemap)
  cat('[export_debug]   .print_chrome_page: footer drawn OK -- page complete\n')
  grid::upViewport()  # pop the 4-row layout viewport
  grid::upViewport()  # pop the 0.25in margin viewport
}

.district_overview_map <- function(ha_sf, district_sf, fac_sf, idp_sf, district_name, campaign_name, basemap_provider = NULL, basemap = 'none', pop_sf = NULL, subdiv_sf = NULL, urban_sf = NULL, zoom = .PRINT_BASEMAP_ZOOM) {
  # Satellite imagery has no consistent light/dark base the way OSM's pale
  # cartography does, so plain black borders and unadorned text can
  # disappear against it -- neon blue + white-halo text/markers now also
  # apply to topo (OpenTopoMap's contour lines and shaded relief create
  # the same low-contrast problem as satellite imagery, just a
  # different-looking one); OSM and no-basemap keep the original scheme,
  # which already has enough contrast.
  needs_high_contrast  <- basemap %in% c('satellite', 'topo')
  border_col    <- if (needs_high_contrast) '#00e5ff' else 'black'
  district_col  <- if (needs_high_contrast) '#00e5ff' else 'black'
  text_col      <- if (needs_high_contrast) '#00e5ff' else 'black'

  # Explicit bbox anchor, from actual debug evidence: this overview map
  # (multiple health-area polygons chained with facility + IDP point
  # layers) crashes specifically with basemap="none" ("only 0's may be
  # mixed with negative subscripts", deep in tmap's own bbox-computation
  # code) -- but the same data succeeds once ANY basemap is set (which
  # forces a specific plot extent), and the single-polygon detail map
  # succeeds even with basemap="none". That pattern points at tmap's
  # automatic bbox UNION across several chained tm_shape() layers (health
  # areas, facilities, IDP points) breaking in some edge case when
  # nothing forces a fixed extent upfront. Giving tm_shape() an explicit
  # bbox here does the same job a basemap was incidentally doing --
  # NOT independently verified as the actual root cause (tmap's internal
  # bbox code isn't something I can step through), but it's a targeted
  # attempt based on the specific failure pattern, not a guess in the
  # dark. If this doesn't resolve it, .print_chrome_page()'s existing
  # tryCatch will still catch the failure and log the (hopefully
  # different, more specific) error message rather than crash the export.
  m <- tmap::tm_shape(ha_sf, bbox = sf::st_bbox(ha_sf))
  if (!is.null(basemap_provider)) m <- m + tmap::tm_basemap(basemap_provider, zoom = zoom)  # user-adjustable now (was a hardcoded constant) -- NOT independently verified against the installed tmap version's exact tm_basemap() zoom behavior, worth confirming visually on a real export

  # Population overlay, same 0.5 opacity as the in-app map. fill_color
  # is already a per-polygon hex color from make_population_overlay_sf()
  # (quantile-bucketed, computed once, not a value tmap needs to map
  # through its own color scale) -- values = "identity" turned out NOT
  # to be a real tmap4 keyword (confirmed by the actual error this threw
  # on a real export). Fixed by supplying the sorted unique hex colors
  # as the values vector instead: tm_scale_categorical(values = ...)
  # maps a literal color vector onto the sorted unique levels of the
  # categorical variable, in order -- since the "categories" here
  # already ARE hex colors, mapping each one back to itself is a true
  # identity mapping, using a documented parameter rather than an
  # invented keyword. Still worth confirming visually on a real export.
  if (!is.null(pop_sf) && nrow(pop_sf) > 0) {
    pop_colors <- sort(unique(pop_sf$fill_color))
    m <- m + tmap::tm_shape(pop_sf) +
      tmap::tm_fill(fill = 'fill_color', fill.scale = tmap::tm_scale_categorical(values = pop_colors),
                   fill.legend = tmap::tm_legend_hide(), fill_alpha = 0.5) +
      tmap::tm_shape(ha_sf)
  }

  # Layer order, bottom to top: subdivision lines, health area borders,
  # district border, urban extent, subdivision names, points, health area
  # name callouts. Health area borders go on top of the subdivision lines
  # they often coincide with, and the callouts go last so nothing is ever
  # drawn over the labels a reader looks for first.
  #
  # Subdivisions and campaign/urban extent are context-only reference
  # layers, same purple/teal-dashed color convention every other map in
  # the app already uses for them (mod_facility_map.R,
  # mod_orientation_tab.R, paint-app.js's own subdivisionLayer/
  # settlementExtentsLayer).

  if (!is.null(subdiv_sf) && nrow(subdiv_sf) > 0)
    m <- m + .subdivision_lines(subdiv_sf)

  # No fill -- boundaries and area-name labels only, per request. Area
  # identity used to be conveyed by categorical fill color plus a legend;
  # with the fill gone, the name label on each polygon is now what
  # identifies it instead.
  m <- m + tmap::tm_shape(ha_sf) +
    tmap::tm_borders(col = border_col, lwd = if (needs_high_contrast) 2.5 else 2)

  if (!is.null(district_sf) && nrow(district_sf) > 0)
    m <- m + tmap::tm_shape(district_sf) + tmap::tm_borders(col = district_col, lwd = 2)

  if (!is.null(urban_sf) && nrow(urban_sf) > 0)
    m <- m + tmap::tm_shape(urban_sf) + tmap::tm_borders(col = '#0d9488', lwd = 1, lty = 'dashed')

  if (!is.null(subdiv_sf) && nrow(subdiv_sf) > 0) {
    sub_lbl <- .subdivision_labels(subdiv_sf)
    if (!is.null(sub_lbl)) m <- m + sub_lbl
  }

  if (!is.null(fac_sf) && nrow(fac_sf) > 0)
    # No facility_name label here (dot only) -- the health area's own
    # dfa_name callout below already identifies it, and health areas are
    # routinely named after their main facility, so a second,
    # near-identical text label right next to the dot was pure visual
    # duplication on a real export (confirmed from actual output).
    m <- m + tmap::tm_shape(fac_sf) +
      tmap::tm_symbols(fill = '#0d9488', size = .PRINT_PT_SIZE$overview[['fac']], shape = 21,
                       col = 'white', col_alpha = 1, lwd = if (needs_high_contrast) 2 else 1)

  if (!is.null(idp_sf) && nrow(idp_sf) > 0)
    m <- m + tmap::tm_shape(idp_sf) +
      tmap::tm_symbols(fill = '#FF1744', size = .PRINT_PT_SIZE$overview[['idp']], shape = 24,
                       col = 'white', col_alpha = 1, lwd = if (needs_high_contrast) 2 else 1)

  m <- m + tmap::tm_shape(ha_sf) + .callout_text('dfa_name', size = 0.65)

  # tm_title() removed -- the title now lives in the separate header
  # band drawn by .print_chrome_page() (bigger, styled consistently with
  # the rest of the new page chrome, and frees this much map canvas for
  # the map itself, per "maximize the size of the map"). tm_scalebar()/
  # tm_compass() bg.color is tmap4's element-background option -- same
  # verification caveat as tm_text()'s shadow above.
  m +
    tmap::tm_scalebar(bg.color = if (needs_high_contrast) 'white' else NA) +
    tmap::tm_compass(position = c('right', 'top'), bg.color = if (needs_high_contrast) 'white' else NA)
}

.health_area_detail_map <- function(ha_one, team_one, fac_one, idp_one, area_name, district_name, basemap_provider = NULL, basemap = 'none', pop_one = NULL, subdiv_one = NULL, urban_one = NULL, zoom = .PRINT_BASEMAP_ZOOM) {
  # Same satellite + topo gate as .district_overview_map() above.
  needs_high_contrast <- basemap %in% c('satellite', 'topo')
  ha_border_col   <- if (needs_high_contrast) '#00e5ff' else 'black'
  team_border_col <- if (needs_high_contrast) '#00e5ff' else '#334155'
  team_text_col   <- if (needs_high_contrast) '#00e5ff' else '#334155'

  m <- tmap::tm_shape(ha_one)
  if (!is.null(basemap_provider)) m <- m + tmap::tm_basemap(basemap_provider, zoom = zoom)  # user-adjustable now (was a hardcoded constant) -- NOT independently verified against the installed tmap version's exact tm_basemap() zoom behavior, worth confirming visually on a real export

  # Same population overlay as the overview map, clipped to this health
  # area's own extent by the caller before it gets here. Same
  # sorted-unique-colors-as-values fix as the overview map -- see that
  # function's comment for why.
  if (!is.null(pop_one) && nrow(pop_one) > 0) {
    pop_colors <- sort(unique(pop_one$fill_color))
    m <- m + tmap::tm_shape(pop_one) +
      tmap::tm_fill(fill = 'fill_color', fill.scale = tmap::tm_scale_categorical(values = pop_colors),
                   fill.legend = tmap::tm_legend_hide(), fill_alpha = 0.5) +
      tmap::tm_shape(ha_one)
  }

  # Same layer order as .district_overview_map() above (subdivision lines
  # under the health area border, labels/callouts on top of everything).
  # Subdivisions/urban extent are clipped to this health area's own extent
  # by the caller before they get here.
  if (!is.null(subdiv_one) && nrow(subdiv_one) > 0)
    m <- m + .subdivision_lines(subdiv_one)

  # No fill here either -- the health area's own boundary is already this
  # page's title/context, so it gets a border only, no label needed on
  # the shape itself.
  m <- m + tmap::tm_shape(ha_one) +
    tmap::tm_borders(col = ha_border_col, lwd = if (needs_high_contrast) 3.5 else 3)

  if (!is.null(urban_one) && nrow(urban_one) > 0)
    m <- m + tmap::tm_shape(urban_one) + tmap::tm_borders(col = '#0d9488', lwd = 1, lty = 'dashed')

  if (!is.null(team_one) && nrow(team_one) > 0)
    m <- m + tmap::tm_shape(team_one) +
      tmap::tm_borders(col = team_border_col, lwd = if (needs_high_contrast) 2.5 else 2) +
      tmap::tm_text('dfa_name', size = 0.45, col = team_text_col, fontface = 'bold',
                    shadow = TRUE, shadow.col = 'white')

  if (!is.null(subdiv_one) && nrow(subdiv_one) > 0) {
    sub_lbl <- .subdivision_labels(subdiv_one, label_min_frac = 0.05)
    if (!is.null(sub_lbl)) m <- m + sub_lbl
  }

  if (!is.null(idp_one) && nrow(idp_one) > 0)
    m <- m + tmap::tm_shape(idp_one) +
      tmap::tm_symbols(fill = '#FF1744', size = .PRINT_PT_SIZE$detail[['idp']], shape = 24,
                       col = 'white', col_alpha = 1, lwd = if (needs_high_contrast) 2 else 1)

  if (!is.null(fac_one) && nrow(fac_one) > 0)
    m <- m + tmap::tm_shape(fac_one) +
      tmap::tm_symbols(fill = '#0d9488', size = .PRINT_PT_SIZE$detail[['fac']], shape = 21,
                       col = 'white', col_alpha = 1, lwd = if (needs_high_contrast) 2 else 1) +
      .callout_text('facility_name', size = 0.7, ymod = 1.3)

  # tm_title() removed -- same reasoning as .district_overview_map()
  # above, title now lives in the separate header band.
  m +
    tmap::tm_scalebar(bg.color = if (needs_high_contrast) 'white' else NA) +
    tmap::tm_compass(position = c('right', 'top'), bg.color = if (needs_high_contrast) 'white' else NA)
}
