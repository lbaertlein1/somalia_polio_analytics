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
  satellite = 'Esri.WorldImagery',
  topo      = 'OpenTopoMap'
)

# Higher than tm_basemap()'s own auto-detected zoom, for sharper export
# imagery -- worth adjusting after seeing a real export at typical
# health-area/district extents, since too high a zoom for a large extent
# just means more tiles fetched without a visible sharpness gain.
.PRINT_BASEMAP_ZOOM <- 16

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
#' @param basemap        "none" (default), "osm", "satellite", or "topo" —
#'                        tile layer under the vector layers on every map page.
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
build_printable_maps_pdf <- function(file, version, district_name, campaign_id,
                                     campaign_name = '', paper = 'a4_landscape',
                                     basemap = 'none', u5_rast = NULL, show_pop = FALSE) {
  
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
  
  # Urban-areas layer for the population summary tables' urban/rural
  # split -- fetch_urban_areas_for_district() itself already handles the
  # admin-configurable URL and falls back to no data (NULL) rather than
  # erroring, so this only needs its own tryCatch as a second layer of
  # defense against something unexpected in the fetch/cache path.
  urban_sf <- if (!is.null(district_sf) && nrow(district_sf) > 0)
    tryCatch(fetch_urban_areas_for_district(district_sf), error = function(e) NULL) else NULL
  
  size <- .PRINT_PAGE_SIZES_IN[[paper]] %||% .PRINT_PAGE_SIZES_IN$a4_landscape
  basemap_provider <- .resolve_basemap(basemap)
  
  grDevices::pdf(file, width = unname(size['width']), height = unname(size['height']))
  on.exit(grDevices::dev.off(), add = TRUE)
  
  print(.district_overview_map(ha_sf, district_sf, fac_sf, idp_sf, district_name, campaign_name, basemap_provider, basemap, pop_sf))
  
  summary_df <- .build_health_area_summary_table(ha_sf, team_targets, u5_rast, campaign_id, urban_sf)
  .print_table_page(summary_df, paste0(district_name, ' \u2014 Health Area Summary'))
  
  area_names <- setdiff(unique(as.character(ha_sf$dfa_name)), extra_dfa_names)
  for (nm in area_names) {
    ha_one   <- ha_sf[ha_sf$dfa_name == nm, , drop = FALSE]
    team_one <- if (!is.null(team_sf) && 'health_area' %in% names(team_sf))
      team_sf[team_sf$health_area == nm, , drop = FALSE] else NULL
    fac_one  <- .points_within(fac_sf, ha_one)
    idp_one  <- .points_within(idp_sf, ha_one)
    # Clipped to this health area's own extent, same tryCatch-guarded
    # pattern as everything else pop_sf touches -- a clip failure (e.g.
    # a still-invalid geometry st_make_valid didn't fully resolve) means
    # this one page just prints without the overlay, not that the whole
    # export fails.
    pop_one <- if (!is.null(pop_sf) && nrow(pop_sf) > 0)
      tryCatch(sf::st_intersection(pop_sf, sf::st_geometry(ha_one) |> sf::st_union()), error = function(e) NULL)
    else NULL
    print(.health_area_detail_map(ha_one, team_one, fac_one, idp_one, nm, district_name, basemap_provider, basemap, pop_one))
    
    if (!is.null(team_one) && nrow(team_one) > 0) {
      field_target <- .unwrap_num(team_targets[[nm]]$target_pop)
      team_df <- .build_team_summary_table(team_one, field_target, u5_rast, urban_sf)
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
.build_health_area_summary_table <- function(ha_sf, team_targets, u5_rast, campaign_id, urban_sf = NULL) {
  area_names <- setdiff(unique(as.character(ha_sf$dfa_name)), extra_dfa_names)
  rows <- lapply(area_names, function(nm) {
    poly    <- ha_sf[ha_sf$dfa_name == nm, , drop = FALSE]
    split   <- .polygon_u5_population_urban_rural(poly, u5_rast, urban_sf)
    wp_val  <- sum(split$total, na.rm = TRUE)
    urban_val <- sum(split$urban, na.rm = TRUE)
    rural_val <- sum(split$rural, na.rm = TRUE)
    
    tgt             <- team_targets[[nm]]
    field_pop       <- .unwrap_num(tgt$target_pop)
    req_teams       <- .unwrap_num(tgt$requested_teams)
    # Recommended Teams is deliberately a single, non-stratified figure
    # -- a stratified urban/rural version of this was tried and
    # reverted (see mod_facility_tab.R's outreach_counts_card for the
    # full reasoning). Urban Pop/Rural Pop below are still reported as
    # informational population detail -- that's not a recommendation
    # that could disagree with anything, just a population breakdown.
    recommended_teams <- tryCatch(compute_n_teams(wp_val, campaign_id = campaign_id), error = function(e) NA_integer_)
    
    data.frame(
      `Health Area`             = nm,
      `Target Pop (WorldPop)`   = .fmt_pop(wp_val),
      `Target Pop (Field)`       = .fmt_field(field_pop),
      `Urban Pop (WorldPop)`     = .fmt_pop(urban_val),
      `Rural Pop (WorldPop)`     = .fmt_pop(rural_val),
      `Recommended Teams`        = if (is.na(recommended_teams)) '\u2013' else as.character(as.integer(recommended_teams)),
      `Field Requested Teams`    = if (is.na(req_teams) || req_teams <= 0) '\u2013' else as.character(as.integer(req_teams)),
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
#' target set. Also splits each team's own population into urban/rural
#' components, same urban_sf/reporting-only caveat as the health-area
#' table above.
.build_team_summary_table <- function(team_one, health_area_field_target, u5_rast, urban_sf = NULL) {
  if (is.null(team_one) || nrow(team_one) == 0) return(NULL)
  team_names <- setdiff(unique(as.character(team_one$dfa_name)), extra_dfa_names)
  if (length(team_names) == 0) return(NULL)
  n_teams <- length(team_names)
  per_team_field <- if (!is.na(health_area_field_target) && n_teams > 0)
    health_area_field_target / n_teams else NA_real_
  
  rows <- lapply(team_names, function(tn) {
    poly    <- team_one[team_one$dfa_name == tn, , drop = FALSE]
    split   <- .polygon_u5_population_urban_rural(poly, u5_rast, urban_sf)
    wp_val  <- sum(split$total, na.rm = TRUE)
    urban_val <- sum(split$urban, na.rm = TRUE)
    rural_val <- sum(split$rural, na.rm = TRUE)
    data.frame(
      Team                     = tn,
      `Target Pop (WorldPop)` = .fmt_pop(wp_val),
      `Target Pop (Field)`     = .fmt_field(per_team_field),
      `Urban Pop (WorldPop)`   = .fmt_pop(urban_val),
      `Rural Pop (WorldPop)`   = .fmt_pop(rural_val),
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

.district_overview_map <- function(ha_sf, district_sf, fac_sf, idp_sf, district_name, campaign_name, basemap_provider = NULL, basemap = 'none', pop_sf = NULL) {
  # Satellite imagery has no consistent light/dark base the way OSM's pale
  # cartography does, so plain black borders and unadorned text can
  # disappear against it -- neon blue + white-halo text/markers only
  # kick in for satellite specifically; OSM and no-basemap keep the
  # original scheme, which already has enough contrast.
  is_satellite  <- identical(basemap, 'satellite')
  border_col    <- if (is_satellite) '#00e5ff' else 'black'
  district_col  <- if (is_satellite) '#00e5ff' else 'black'
  text_col      <- if (is_satellite) '#00e5ff' else 'black'
  
  m <- tmap::tm_shape(ha_sf)
  if (!is.null(basemap_provider)) m <- m + tmap::tm_basemap(basemap_provider, zoom = .PRINT_BASEMAP_ZOOM)  # higher-than-default zoom for sharper export imagery -- NOT independently verified against the installed tmap version's exact tm_basemap() zoom behavior, worth confirming visually on a real export
  
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
  
  # No fill -- boundaries and area-name labels only, per request. Area
  # identity used to be conveyed by categorical fill color plus a legend;
  # with the fill gone, the name label on each polygon is now what
  # identifies it instead.
  #
  # shadow = TRUE is tmap4's text-halo option -- NOT independently
  # verified against the installed tmap version's exact tm_text()
  # argument name/behavior, worth confirming visually on a real satellite
  # export; if it's not actually rendering a halo, that's the first thing
  # to check.
  m <- m +
    tmap::tm_borders(col = border_col, lwd = if (is_satellite) 1.2 else 0.8) +
    tmap::tm_text('dfa_name', size = 0.45, col = text_col, fontface = 'bold',
                  shadow = TRUE, shadow.col = 'white')
  
  if (!is.null(district_sf) && nrow(district_sf) > 0)
    m <- m + tmap::tm_shape(district_sf) + tmap::tm_borders(col = district_col, lwd = 2)
  
  if (!is.null(fac_sf) && nrow(fac_sf) > 0)
    m <- m + tmap::tm_shape(fac_sf) +
    tmap::tm_symbols(fill = '#0d9488', size = 0.3, shape = 21,
                     col = 'white', col_alpha = 1, lwd = if (is_satellite) 2 else 1) +
    tmap::tm_text('facility_name', size = 0.4, ymod = 0.7, shadow = TRUE, shadow.col = 'white')
  
  if (!is.null(idp_sf) && nrow(idp_sf) > 0)
    m <- m + tmap::tm_shape(idp_sf) +
    tmap::tm_symbols(fill = '#d95f0e', size = 0.25, shape = 24,
                     col = 'white', col_alpha = 1, lwd = if (is_satellite) 2 else 1)
  
  title <- paste0(district_name, if (nzchar(campaign_name)) paste0(' — ', campaign_name) else '')
  # tm_scalebar()/tm_compass() bg.color is tmap4's element-background
  # option -- same verification caveat as tm_text()'s shadow above.
  m + tmap::tm_title(title) +
    tmap::tm_scalebar(bg.color = if (is_satellite) 'white' else NA) +
    tmap::tm_compass(position = c('right', 'top'), bg.color = if (is_satellite) 'white' else NA)
}

.health_area_detail_map <- function(ha_one, team_one, fac_one, idp_one, area_name, district_name, basemap_provider = NULL, basemap = 'none', pop_one = NULL) {
  is_satellite <- identical(basemap, 'satellite')
  ha_border_col   <- if (is_satellite) '#00e5ff' else 'black'
  team_border_col <- if (is_satellite) '#00e5ff' else '#334155'
  team_text_col   <- if (is_satellite) '#00e5ff' else '#334155'
  
  m <- tmap::tm_shape(ha_one)
  if (!is.null(basemap_provider)) m <- m + tmap::tm_basemap(basemap_provider, zoom = .PRINT_BASEMAP_ZOOM)  # higher-than-default zoom for sharper export imagery -- NOT independently verified against the installed tmap version's exact tm_basemap() zoom behavior, worth confirming visually on a real export
  
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
  
  # No fill here either -- the health area's own boundary is already this
  # page's title/context, so it gets a border only, no label needed on
  # the shape itself.
  m <- m + tmap::tm_borders(col = ha_border_col, lwd = if (is_satellite) 2.5 else 2)
  
  if (!is.null(team_one) && nrow(team_one) > 0)
    m <- m + tmap::tm_shape(team_one) +
    tmap::tm_borders(col = team_border_col, lwd = 0.6) +
    tmap::tm_text('dfa_name', size = 0.4, col = team_text_col, fontface = 'bold',
                  shadow = TRUE, shadow.col = 'white')
  
  if (!is.null(fac_one) && nrow(fac_one) > 0)
    m <- m + tmap::tm_shape(fac_one) +
    tmap::tm_symbols(fill = '#0d9488', size = 0.4, shape = 21,
                     col = 'white', col_alpha = 1, lwd = if (is_satellite) 2 else 1) +
    tmap::tm_text('facility_name', size = 0.5, ymod = 0.8, shadow = TRUE, shadow.col = 'white')
  
  if (!is.null(idp_one) && nrow(idp_one) > 0)
    m <- m + tmap::tm_shape(idp_one) +
    tmap::tm_symbols(fill = '#d95f0e', size = 0.35, shape = 24,
                     col = 'white', col_alpha = 1, lwd = if (is_satellite) 2 else 1)
  
  m + tmap::tm_title(paste0(district_name, ' — ', area_name)) +
    tmap::tm_scalebar(bg.color = if (is_satellite) 'white' else NA) +
    tmap::tm_compass(position = c('right', 'top'), bg.color = if (is_satellite) 'white' else NA)
}