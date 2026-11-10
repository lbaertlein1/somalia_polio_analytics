# =============================================================================
# download_helpers_v2.R
#
# Replaces build_district_download() for the v2 data model. Separate file
# (additive) rather than editing download_helpers.R in place, since the v1
# function is built entirely around planning_data (microplan), which no
# longer exists.
#
# Team-area rework: team areas are now their own table
# (team_area_versions), one independently-current version PER health area
# — NOT a field on the health-area version's own snapshot anymore. A
# district's team-area export has to be assembled by combining every
# currently-shared team_area_versions row across all its health areas,
# not read off version$snap the way health-area/landmark/IDP/facility data
# still can be.
#
# Format support: GeoJSON (default), ESRI Shapefile, and KML, selected via
# a driver/extension pair rather than three near-duplicate functions.
# Shapefiles are inherently multi-file (.shp/.shx/.dbf/.prj) — no special
# handling needed here, since the existing zip step already just collects
# every file in the temp directory regardless of how many one layer wrote.
#
# Output files (any that have no data for a given district are simply
# omitted from the zip, same behavior as v1):
#   health_areas.<ext>       — health area polygons (smoothed presentation
#                               geometry if available, else raw)
#   team_areas.<ext>         — team area polygons, same basis, combined
#                               across every health area's current version
#   idp_settlements.<ext>    — IDP settlement points
#   landmarks.<ext>          — landmark points
#   facilities.csv           — all facility records
#   health_area_population_summary.csv — one row per health area: WorldPop
#                               U5, Scaled WorldPop U5 (districts with an
#                               In-Scope Field U5 only), Field Target U5,
#                               IDP count, recommended/requested teams
#   team_population_summary.csv — one row per team: WorldPop U5, Scaled
#                               WorldPop U5, Field Target U5 (the team's own,
#                               else its share of the health area's)
#   scope_population_summary.csv — one row per district: WorldPop U5 (In
#                               Scope), In-Scope Field U5, scale factor
# =============================================================================

#' write.csv() writes in the system's native encoding by default, with no
#' UTF-8 byte-order mark -- Excel then falls back to guessing the
#' encoding from the system locale, and misreads any non-ASCII character
#' as garbled text. The only place that currently bites is the Unicode
#' en-dash .fmt_field() (printable_export.R) uses for NA/blank field-
#' target values, but facility names can plausibly contain non-ASCII
#' text too, so every CSV in this file goes through this instead of
#' plain write.csv() -- cheap insurance, not just a spot fix for the one
#' symptom that happened to get noticed.
.write_csv_utf8_bom <- function(x, path, row.names = FALSE, ...) {
  con <- file(path, open = 'wb')
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw('\xef\xbb\xbf'), con)
  write.csv(x, con, row.names = row.names, ...)
}

#' One row recording where a district's population figures come from:
#' WorldPop U5 over the In Scope area, the In-Scope Field U5 entered at
#' Campaign Scope (blank if none), and the factor WorldPop is multiplied by
#' to give the other summaries' Scaled WorldPop U5 (1 when there's no
#' In-Scope Field U5). The factor shares the field figure over the
#' populated areas only (scope_pop_scale()), so it's larger than
#' field / In Scope WorldPop whenever part of the scope is Unpopulated.
.scope_population_row <- function(district_name, snap, campaign_id = NULL) {
  wp    <- .unwrap_num(snap$scope_worldpop_pop)
  field <- .unwrap_num(snap$scope_target_pop)
  scale <- scope_pop_scale(snap, campaign_id)
  data.frame(
    district_name                  = district_name,
    `WorldPop U5 (In Scope)`       = if (is.na(wp)) NA_real_ else round(wp),
    `In-Scope Field U5`            = if (is.na(field)) NA_real_ else round(field),
    `WorldPop Scale Factor`        = if (is.na(scale)) 1 else round(scale, 3),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

#' Campaign + district codes for every export file: camp_id, camp_nm,
#' dist_id (the district's dist_uid in the campaign's boundary set) and
#' reg_cd (region code), matched on district_name.
.export_codes_ctx <- function(campaign_id) {
  d <- tryCatch(sf::st_drop_geometry(districts_shp_for_campaign(campaign_id)), error = function(e) NULL)
  nm <- tryCatch(.db_query(pool, "SELECT campaign_name FROM campaigns WHERE campaign_id = ?c",
                           list(c = as.integer(campaign_id)))$campaign_name[1], error = function(e) NA_character_)
  list(campaign_id = as.integer(campaign_id), campaign_name = nm %||% NA_character_,
       codes = if (is.null(d)) NULL else data.frame(district_name = as.character(d$district_name),
                                                    dist_uid = as.character(d$DIST_UID), reg_code = as.character(d$REG_CODE),
                                                    stringsAsFactors = FALSE))
}
.add_export_codes <- function(df, ctx) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  # <= 7 characters: sf shortens longer shapefile field names.
  df$camp_id <- ctx$campaign_id; df$camp_nm <- ctx$campaign_name
  m <- if (!is.null(ctx$codes) && 'district_name' %in% names(df)) match(as.character(df$district_name), ctx$codes$district_name) else rep(NA_integer_, nrow(df))
  df$dist_id <- if (!is.null(ctx$codes)) ctx$codes$dist_uid[m] else NA_character_
  df$reg_cd  <- if (!is.null(ctx$codes)) ctx$codes$reg_code[m] else NA_character_
  if (inherits(df, 'sf')) sf::st_sf(sf::st_drop_geometry(df), geometry = sf::st_geometry(df)) else df
}

#' Health Area ID + Facility ID columns, right after `Health Area`, for
#' the population summary CSVs (from the version's health_area_ids).
.ha_id_cols <- function(df, ids) {
  if (is.null(df) || nrow(df) == 0 || !'Health Area' %in% names(df)) return(df)
  ids <- ids %||% list(); nm <- as.character(df$`Health Area`)
  get <- function(k) vapply(nm, function(n) as.character(ids[[n]][[k]] %||% NA_character_), character(1), USE.NAMES = FALSE)
  at <- match('Health Area', names(df))
  out <- df[, seq_len(at), drop = FALSE]
  out[['Health Area ID']] <- get('health_area_id'); out[['Facility ID']] <- get('facility_id')
  cbind(out, df[, -seq_len(at), drop = FALSE])
}

#' For each selected coordination site, the health area it produced
#' (health_area_id + current name); blank for facilities not selected.
.fac_ha_ids <- function(df, ids) {
  ids <- ids %||% list()
  fac <- vapply(ids, function(e) as.character(e$facility_id %||% NA_character_), character(1))
  k <- match(as.character(df$facility_id), fac)
  df$health_area_id   <- vapply(k, function(i) if (is.na(i)) NA_character_ else as.character(ids[[i]]$health_area_id %||% NA_character_), character(1))
  df$health_area_name <- ifelse(is.na(k), NA_character_, names(ids)[k])
  df
}

#' Same, plus Team ID right after `Team`, for the team summary CSV.
.team_id_cols <- function(df, ha_ids, team_ids) {
  df <- .ha_id_cols(df, ha_ids)
  if (is.null(df) || nrow(df) == 0 || !'Team' %in% names(df)) return(df)
  team_ids <- team_ids %||% list()
  tid <- vapply(as.character(df$Team), function(n) as.character(team_ids[[n]] %||% NA_character_), character(1), USE.NAMES = FALSE)
  at <- match('Team', names(df))
  out <- df[, seq_len(at), drop = FALSE]; out[['Team ID']] <- tid
  cbind(out, df[, -seq_len(at), drop = FALSE])
}

#' Add each health area's ha_id (health_area_id) and fac_id (the
#' coordination site's facility_id) from the version's health_area_ids.
#' Short names so they survive the shapefile's 10-character field limit.
.with_ha_ids <- function(ha_sf, ids) {
  if (is.null(ha_sf) || nrow(ha_sf) == 0 || !'dfa_name' %in% names(ha_sf)) return(ha_sf)
  ids <- ids %||% list()
  ha_sf$ha_id  <- vapply(as.character(ha_sf$dfa_name), function(n) as.character(ids[[n]]$health_area_id %||% NA_character_), character(1), USE.NAMES = FALSE)
  ha_sf$fac_id <- vapply(as.character(ha_sf$dfa_name), function(n) as.character(ids[[n]]$facility_id %||% NA_character_), character(1), USE.NAMES = FALSE)
  ha_sf
}

#' Population fields on the health_areas / team_areas GIS layers, from the
#' same tables as the population summary CSVs (formatted text there, numbers
#' here). Health areas: by ha_id. Teams: by ta_id; a "team area not mapped"
#' feature (a whole health area) gets its health area's figures, so the team
#' layer still sums to the health-area totals. No match (Unpopulated,
#' Inaccessible, unmapped districts) is left blank.
.POP_FIELDS_HA   <- c(wp_u5 = 'WorldPop U5', wp_u5_scl = 'Scaled WorldPop U5', field_u5 = 'Field Target U5',
                      idp_sites = 'IDP Settlements', teams_rec = 'Recommended Teams', teams_req = 'Field Requested Teams')
.POP_FIELDS_TEAM <- c(wp_u5 = 'WorldPop U5', wp_u5_scl = 'Scaled WorldPop U5', field_u5 = 'Field Target U5')
.add_pop_fields <- function(layer, ha_tbl, team_tbl = NULL) {
  if (is.null(layer) || nrow(layer) == 0) return(layer)
  num <- function(v) suppressWarnings(as.numeric(gsub(',', '', as.character(v))))
  is_team <- 'ta_id' %in% names(layer)
  flds <- if (is_team) .POP_FIELDS_TEAM else .POP_FIELDS_HA
  ha_k <- if (!is.null(ha_tbl) && nrow(ha_tbl) > 0) match(as.character(layer$ha_id), as.character(ha_tbl[['Health Area ID']]), incomparables = NA) else rep(NA_integer_, nrow(layer))
  tm_k <- if (is_team && !is.null(team_tbl) && nrow(team_tbl) > 0) match(as.character(layer$ta_id), as.character(team_tbl[['Team ID']]), incomparables = NA) else rep(NA_integer_, nrow(layer))
  for (f in names(flds)) {
    col <- flds[[f]]
    v <- if (!is.null(ha_tbl) && col %in% names(ha_tbl)) num(ha_tbl[[col]])[ha_k] else rep(NA_real_, nrow(layer))
    if (is_team) {
      from_team <- if (!is.null(team_tbl) && col %in% names(team_tbl)) num(team_tbl[[col]])[tm_k] else rep(NA_real_, nrow(layer))
      # mapped teams: the team's own figure; "not mapped" rows: the health area's
      v <- ifelse(layer$mapped %in% 'Yes', from_team, v)
    }
    layer[[f]] <- v
  }
  sf::st_sf(sf::st_drop_geometry(layer), geometry = sf::st_geometry(layer))
}

#' Shapefile field names are max 10 characters; sf otherwise abbreviates
#' EVERY field to 7 (dfa_nam, helth_r, dstrct_) once any is longer. Long
#' names get readable short ones instead.
.SHP_FIELD_NAMES <- c(district_name = 'dist_name', region_name = 'reg_name', health_area = 'ha_name',
                      in_campaign = 'in_camp', health_area_id = 'ha_id', health_area_name = 'ha_nm',
                      idp_population = 'idp_pop', u5_population = 'u5_pop', crossing_name = 'crossing',
                      last_confirmed = 'last_conf', subdivision_name = 'subdiv')
.shp_field_names <- function(x, spec) {
  if (!identical(spec$ext, 'shp')) return(x)
  nm <- names(x); hit <- nm %in% names(.SHP_FIELD_NAMES) & !(.SHP_FIELD_NAMES[nm] %in% nm)
  nm[hit] <- .SHP_FIELD_NAMES[nm[hit]]; names(x) <- nm
  x
}

#' Excluded areas -- in the *_with_excluded_areas GIS files only.
.EXCLUDED_AREA_TYPES <- c('Unpopulated', 'Inaccessible', 'Out of scope')

#' area_type on a health_areas / team_areas layer (kind = 'health area' or
#' 'team area'): the kind, "<kind> not mapped" for a placeholder, or the
#' excluded-area type. `mapped` is blank on excluded areas.
.add_area_type <- function(layer, kind) {
  if (is.null(layer) || nrow(layer) == 0) return(layer)
  nm <- as.character(layer$dfa_name); excl <- nm %in% .EXCLUDED_AREA_TYPES
  layer$area_type <- ifelse(excl, nm, ifelse(layer$mapped %in% 'No', paste(kind, 'not mapped'), kind))
  layer$mapped[excl] <- NA_character_
  layer
}

#' The district's out-of-scope area (Campaign Scope tab "Out of Scope"),
#' minus anything already covered by `cover_sf`, as one "Out of scope"
#' feature (EPSG:4326) -- or NULL when the whole district is in scope.
.out_of_scope_sf <- function(snap, cover_sf) {
  sc <- snap$scope_saved_dfa_sf
  if (is.null(sc) || !inherits(sc, 'sf') || nrow(sc) == 0 || !'dfa_name' %in% names(sc)) return(NULL)
  oos <- sc[sc$dfa_name == 'Out of Scope', , drop = FALSE]
  if (nrow(oos) == 0) return(NULL)
  g <- sf::st_union(safe_make_valid(sf::st_transform(oos, 3857)))
  if (!is.null(cover_sf) && nrow(cover_sf) > 0)
    g <- sf::st_difference(g, sf::st_union(safe_make_valid(sf::st_transform(cover_sf, 3857))))
  p <- suppressWarnings(sf::st_cast(sf::st_collection_extract(safe_make_valid(g), 'POLYGON'), 'POLYGON'))
  p <- p[as.numeric(sf::st_area(p)) >= 100]          # drop edge slivers
  if (length(p) == 0) return(NULL)
  sf::st_sf(dfa_name = 'Out of scope', geometry = sf::st_transform(sf::st_cast(sf::st_union(p), 'MULTIPOLYGON'), 4326))
}

#' Excluded areas removed (the *_without_excluded_areas files).
.without_excluded <- function(layer) {
  if (is.null(layer) || nrow(layer) == 0 || !'area_type' %in% names(layer)) return(layer)
  layer[!layer$area_type %in% .EXCLUDED_AREA_TYPES, , drop = FALSE]
}

#' Campaign districts with excluded areas removed: each district dissolved
#' from its health_areas features that are not excluded.
.districts_without_excluded <- function(ha_layer) {
  x <- .without_excluded(ha_layer)
  if (is.null(x) || nrow(x) == 0 || !'district_name' %in% names(x)) return(NULL)
  keep <- intersect(c('zone_name', 'region_name', 'district_name'), names(x))
  x <- sf::st_transform(x, 3857)
  out <- x |> dplyr::group_by(dplyr::across(dplyr::all_of(keep))) |>
    dplyr::summarise(geometry = sf::st_union(safe_make_valid(geometry)), .groups = 'drop') |> sf::st_as_sf()
  out$area_type <- 'district'
  sf::st_transform(out, 4326)
}

#' README.txt for an export zip: what every file in it is, and the fields.
.write_export_readme <- function(dir, scope_label, campaign_name, spec) {
  files <- sort(list.files(dir))
  layer_of <- function(f) sub('[.][^.]+$', '', f)
  ext <- spec$ext
  desc <- c(
    health_areas_with_excluded_areas    = 'Health areas, plus the excluded areas (Unpopulated, Inaccessible, Out of scope) -- covers the whole district with no gaps.',
    health_areas_without_excluded_areas = 'Health areas only -- the areas vaccination teams cover. Excluded areas removed.',
    team_areas_with_excluded_areas      = 'Team areas, plus the excluded areas -- no gaps. A health area not yet divided into teams is one feature, "<health area> - team area not mapped".',
    team_areas_without_excluded_areas   = 'Team areas only (with "team area not mapped" health areas), excluded areas removed.',
    districts_with_excluded_areas       = 'Whole district outlines.',
    districts_without_excluded_areas    = 'District outlines with the excluded areas removed (the area vaccination teams cover).',
    subdivisions                        = 'City sections / subdivisions inside the district(s), for reference.',
    campaign_extent                     = 'Campaign (urban) extent configured for the campaign, for reference.',
    idp_settlements                     = 'IDP settlements (points) from the site registry, with population.',
    microplan_sites                     = 'All site-registry sites (points): IDP settlements, nomadic sites, crossing points, etc.',
    landmarks                           = 'Landmarks added in the app (points).',
    health_area_population_summary      = 'One row per health area: WorldPop U5, Scaled WorldPop U5, Field Target U5, IDP settlements, recommended and field requested teams.',
    team_population_summary             = 'One row per mapped team area: WorldPop U5, Scaled WorldPop U5, Field Target U5.',
    scope_population_summary            = 'One row per district: in-scope WorldPop U5, In-Scope Field U5 and the scale factor between them.',
    facilities                          = 'Facility list (MHFL + facilities added in the app) with coordinates; selected coordination sites carry the ID and name of the health area they produced.')
  lines <- c(
    sprintf('Somalia Health Area Mapping Tool -- export for %s%s', scope_label, if (nzchar(campaign_name %||% '')) paste0(', ', campaign_name) else ''),
    sprintf('Produced %s. Map layers are %s (EPSG:4326, WGS84 lat/lon).', format(Sys.time(), '%d %b %Y %H:%M'), toupper(ext)),
    if (identical(ext, 'shp')) 'Each shapefile layer is several files with the same name (.shp geometry, .shx index, .dbf attributes, .prj projection, .cpg UTF-8 encoding, .qix spatial index) -- keep them together.' else NULL,
    '', 'FILES', '-----')
  for (l in unique(layer_of(files))) {
    if (l == 'README') next
    fx <- files[layer_of(files) == l]
    shown <- if (identical(ext, 'shp') && any(grepl('[.]shp$', fx))) paste0(l, '.shp (+ ', paste(setdiff(tools::file_ext(fx), 'shp'), collapse = ', '), ')') else paste(fx, collapse = ', ')
    lines <- c(lines, shown, paste0('    ', desc[[l]] %||% ''), '')
  }
  lines <- c(lines,
    'EXCLUDED AREAS', '--------------',
    'Unpopulated  - no resident population; not vaccinated.',
    'Inaccessible - cannot be reached (e.g. insecurity); not vaccinated in this campaign.',
    'Out of scope - parts of the district outside this campaign\'s scope (Campaign Scope tab).',
    '"not mapped" features (in both versions): "<district> - health area not mapped" covers a district with no published health areas yet;',
    '"<health area> - team area not mapped" covers a health area not yet divided into team areas.',
    '',
    'FIELDS (map layers; shapefile names in brackets where shorter)', '------',
    'dfa_name                  Name of the health area / team area (or the excluded-area type / "not mapped" label)',
    'area_type                 health area | team area | health area not mapped | team area not mapped | Unpopulated | Inaccessible | Out of scope | district',
    'mapped                    Yes = drawn in the app; No = "not mapped" placeholder; blank = excluded area',
    'health_area [ha_name]     Health area the team area belongs to (team layers)',
    'ha_id                     Health area ID (stable across renames)',
    'fac_id                    Facility ID of the health area\'s coordination site',
    'ta_id                     Team area ID (stable across renames)',
    'wp_u5                     WorldPop under-5 population (100 m grid)',
    'wp_u5_scl                 WorldPop U5 scaled to the district\'s In-Scope Field U5, where entered',
    'field_u5                  Field Target U5 entered for the health area / team',
    'idp_sites                 Number of IDP settlements in the health area',
    'teams_rec, teams_req      Recommended teams / Field Requested Teams',
    'zone_name, region_name [reg_name], district_name [dist_name]',
    'camp_id, camp_nm          Campaign ID / name',
    'dist_id, reg_cd           District ID (e.g. BAN-WBR) / region code',
    '', 'Population fields are blank for excluded areas and for districts with no published health areas.',
    'Disclaimer: for campaign planning purposes only -- boundaries and figures are estimates, not official statistics.')
  writeLines(lines, file.path(dir, 'README.txt'), useBytes = FALSE)
}

#' GIS-layer placeholders for a campaign district with no published health
#' areas: one district-wide feature in each layer, "<district> - health
#' area not mapped" / "<district> - team area not mapped" (mapped = "No").
.unmapped_district_sf <- function(district_geom, district_name, what) {
  df <- data.frame(dfa_name = paste0(district_name, ' - ', what, ' not mapped'), stringsAsFactors = FALSE)
  if (what == 'team area') df$health_area <- NA_character_
  df$ha_id <- NA_character_; df$fac_id <- NA_character_
  if (what == 'team area') df$ta_id <- NA_character_
  df$mapped <- 'No'
  sf::st_sf(df, geometry = sf::st_geometry(district_geom))
}

#' The team_areas map layer with no holes: every health area that has no
#' published team layout yet is added as one feature covering the whole
#' health area, named "<health area> - team area not mapped" (mapped =
#' "No"); district-level Unpopulated / Inaccessible areas are added as-is.
#' Only for the GIS layers (shp/kml/geojson) -- the team population CSV
#' stays mapped teams only.
.team_layer_filled <- function(team_sf, ha_sf, ids) {
  if (is.null(ha_sf) || nrow(ha_sf) == 0 || !'dfa_name' %in% names(ha_sf)) return(team_sf)
  ids <- ids %||% list(); res <- c('Unpopulated', 'Inaccessible')
  mapped <- if (is.null(team_sf) || nrow(team_sf) == 0) character(0) else unique(as.character(team_sf$health_area))
  ha <- sf::st_transform(ha_sf, 4326)
  ha <- ha[!as.character(ha$dfa_name) %in% mapped, , drop = FALSE]
  fill <- NULL
  if (nrow(ha) > 0) {
    nm <- as.character(ha$dfa_name)
    fill <- sf::st_sf(
      dfa_name    = ifelse(nm %in% res, nm, paste0(nm, ' - team area not mapped')),
      health_area = nm,
      ha_id       = vapply(nm, function(n) as.character(ids[[n]]$health_area_id %||% NA_character_), character(1), USE.NAMES = FALSE),
      fac_id      = vapply(nm, function(n) as.character(ids[[n]]$facility_id %||% NA_character_), character(1), USE.NAMES = FALSE),
      ta_id       = NA_character_,
      mapped      = 'No',
      geometry    = sf::st_geometry(ha))
  }
  if (is.null(team_sf) || nrow(team_sf) == 0) return(fill)
  team <- sf::st_transform(team_sf, 4326); team$mapped <- 'Yes'
  team <- sf::st_sf(sf::st_drop_geometry(team), geometry = sf::st_geometry(team))
  if (is.null(fill)) team else dplyr::bind_rows(team, fill)
}

#' Map a user-facing format choice to its sf driver name and file extension.
#' Kept in one place so the export UI and the zip-builders always agree on
#' what "shp"/"kml"/"geojson" mean.
export_format_spec <- function(format = c('geojson', 'shp', 'kml')) {
  format <- match.arg(format)
  switch(format,
    geojson = list(driver = 'GeoJSON',          ext = 'geojson', opts = character(0)),
    # UTF-8 attributes + a .cpg saying so (GDAL's default is Latin-1, no
    # .cpg), and a .qix spatial index. ArcGIS's own .sbn/.sbx index is a
    # proprietary format GDAL can't write; ArcGIS builds it itself.
    shp     = list(driver = 'ESRI Shapefile',    ext = 'shp', opts = c('ENCODING=UTF-8', 'SPATIAL_INDEX=YES')),
    kml     = list(driver = 'KML',               ext = 'kml', opts = character(0))
  )
}

#' Build one district's v2 download package (one shared mapping_versions
#' row for health areas, plus every current team_area_versions row across
#' its health areas).
#'
#' @param file           output zip path (as passed by a Shiny downloadHandler)
#' @param district_name  character
#' @param zone, region   character, for attribute columns
#' @param version        a single parsed HEALTH-AREA version list, as
#'                        returned by db_get_version_by_id() /
#'                        db_get_shared_version() (has a `$snap` element
#'                        with saved_dfa_sf, smoothed_dfa_sf, dfa_names,
#'                        idp_settlements, landmarks, app_sf/odk_sf —
#'                        NOT team-area fields, those live in
#'                        team_area_versions now).
#' @param campaign_id    integer — needed to look up each health area's
#'                        current team-area version independently.
#' @param format         'geojson' (default), 'shp', or 'kml'.
build_district_download_v2 <- function(file, district_name, zone = '', region = '',
                                       version, campaign_id, format = 'geojson') {

  spec <- export_format_spec(format)
  codes_ctx <- .export_codes_ctx(campaign_id)
  .x <- function(df) .add_export_codes(df, codes_ctx)

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  snap <- version$snap %||% list()
  # IDP settlements from the site registry (old snapshot as fallback), and
  # every registry site in the district for a microplan_sites file.
  snap$idp_settlements <- tryCatch(version_idp_points(snap, district_name, campaign_id), error = function(e) snap$idp_settlements)
  sites_dl <- tryCatch(district_sites_df(district_name, campaign_id), error = function(e) NULL)

  .write_sf <- function(sf_obj, fname) {
    if (is.null(sf_obj) || nrow(sf_obj) == 0) return(invisible(NULL))
    out <- sf_obj |>
      dplyr::mutate(zone_name = .env$zone, region_name = .env$region, district_name = .env$district_name) |>
      sf::st_transform(4326) |> .x() |> .shp_field_names(spec)
    sf::st_write(out, file.path(tmp, fname), driver = spec$driver, layer_options = spec$opts, delete_dsn = TRUE, quiet = TRUE)
  }

  # Prefer smoothed/presentation geometry for exports — raw grid-stepped
  # polygons look unprofessional outside the app; fall back to raw if no
  # smoothed geometry was generated yet (e.g. version never submitted).
  ha_sf <- snap$smoothed_dfa_sf %||% snap$saved_dfa_sf
  # Computed-at-submit WorldPop per health area (mapping_versions.dfa_
  # worldpop_pop) -- read here so .build_health_area_summary_table()
  # can use it instead of re-extracting from the raster.
  dfa_worldpop_pop <- snap$dfa_worldpop_pop %||% list()

  # Team areas: combine every health area's CURRENT team-area version —
  # there is no single "the district's team areas" anymore, only
  # per-health-area ones, each independently current or not.
  team_field_overrides_by_ha <- list()
  team_worldpop_by_ha <- list()
  team_ids_by_ha <- list()
  team_sf <- tryCatch({
    ha_names <- setdiff(unlist(snap$dfa_names %||% list()), c('Inaccessible', 'Unpopulated'))
    parts <- lapply(ha_names, function(han) {
      tv <- db_get_current_team_area_version(pool, campaign_id, district_name, han)
      if (is.null(tv)) return(NULL)
      geom <- tv$snap$smoothed_team_sf %||% tv$snap$saved_team_sf
      if (is.null(geom) || nrow(geom) == 0) return(NULL)
      geom$health_area <- han
      geom$ha_id  <- tv$health_area_id %||% NA_character_
      geom$fac_id <- tv$facility_id %||% NA_character_; geom$ta_id <- vapply(as.character(geom$dfa_name %||% rep(NA_character_, nrow(geom))), function(n) as.character(tv$snap$team_ids[[n]] %||% NA_character_), character(1), USE.NAMES = FALSE)
      team_field_overrides_by_ha[[han]] <<- tv$snap$team_field_pop_overrides %||% list()
      # Computed-at-submit WorldPop per team (team_area_versions.team_
      # worldpop_pop) -- same reasoning as dfa_worldpop_pop above.
      team_worldpop_by_ha[[han]] <<- tv$snap$team_worldpop_pop %||% list()
      team_ids_by_ha[[han]] <<- tv$snap$team_ids %||% list()
      geom
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0) NULL else do.call(rbind, parts)
  }, error = function(e) NULL)

  # health_areas / team_areas layers are written after the population
  # tables below, which they take their population fields from.
  ha_pop_tbl <- NULL; team_pop_tbl <- NULL

  # Subdivisions and campaign/urban extent, as their own data layers --
  # matching what the printable PDF export now also draws on its maps
  # (see printable_export.R), so someone opening this boundary export in
  # GIS software gets the same context layers, not just health/team
  # areas. Looked up from districts_shp by name, NOT snap$district_
  # boundary_sf -- that field is a pre-existing, apparently-never-
  # populated one (confirmed by checking every place it's written to;
  # nothing in the app ever sets it), so relying on it here would
  # silently skip these layers for every export. districts_shp is the
  # same reliable source mod_facility_tab.R's own district_base() and
  # printable_export.R's own zone/region lookup already trust.
  district_sf_for_fetch <- tryCatch(
    districts_shp_for_campaign(campaign_id) |> dplyr::filter(district_name == !!district_name) |>
      dplyr::group_by(district_name) |> dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
      sf::st_as_sf() |> safe_make_valid() |> sf::st_transform(4326),
    error = function(e) NULL
  )
  subdiv_sf_export <- if (!is.null(district_sf_for_fetch))
    tryCatch(fetch_subdivisions_for_district(district_sf_for_fetch), error = function(e) NULL) else NULL
  urban_sf_export <- if (!is.null(district_sf_for_fetch))
    tryCatch(fetch_campaign_extent_for_district(district_sf_for_fetch, campaign_id), error = function(e) NULL) else NULL
  .write_sf(subdiv_sf_export, paste0('subdivisions.', spec$ext))
  .write_sf(urban_sf_export,  paste0('campaign_extent.', spec$ext))

  # Population/team-count summary CSVs -- reuses the exact same table
  # builders the printable PDF export already uses (printable_export.R),
  # rather than a second implementation of the same WorldPop-extraction +
  # field-target logic that could drift out of sync with the PDF's own
  # numbers. u5_rast is the app's global WorldPop raster, loaded once at
  # startup -- not passed as a parameter here since this function follows
  # the same implicit-global convention printable_export.R's own callers
  # already rely on.
  tryCatch({
    if (!is.null(ha_sf) && nrow(ha_sf) > 0) {
      team_targets <- snap$team_targets %||% list()
      # Reuses urban_sf_export (fetched once, above) rather than a
      # second fetch_campaign_extent_for_district() call for the same
      # district/campaign.
      urban_sf <- urban_sf_export
      # Same construction as printable_export.R's own idp_sf and
      # mod_intro_tab_v2.R's popup -- missing this argument entirely
      # (as this call previously did) is the exact bug that left IDP
      # counts at 0 in this CSV regardless of what was actually stored.
      idp_sf_export <- NULL
      if (!is.null(snap$idp_settlements) && nrow(snap$idp_settlements) > 0 &&
          all(c('lon', 'lat') %in% names(snap$idp_settlements))) {
        idp_sf_export <- sf::st_as_sf(snap$idp_settlements, coords = c('lon', 'lat'), crs = 4326, remove = FALSE)
      }
      pop_scale <- scope_pop_scale(snap, campaign_id)
      ha_summary <- .build_health_area_summary_table(ha_sf, team_targets, u5_rast, campaign_id, urban_sf, idp_sf_export, dfa_worldpop_pop, pop_scale)
      if (!is.null(ha_summary) && nrow(ha_summary) > 0) {
        ha_pop_tbl <- .ha_id_cols(cbind(district_name = district_name, ha_summary), snap$health_area_ids)
        .write_csv_utf8_bom(.x(ha_pop_tbl), file.path(tmp, 'health_area_population_summary.csv'), row.names = FALSE)
      }
      .write_csv_utf8_bom(.x(.scope_population_row(district_name, snap, campaign_id)),
               file.path(tmp, 'scope_population_summary.csv'), row.names = FALSE)

      if (!is.null(team_sf) && nrow(team_sf) > 0) {
        team_rows <- lapply(unique(team_sf$health_area), function(han) {
          team_one <- team_sf[team_sf$health_area == han, , drop = FALSE]
          tgt <- team_targets[[han]]
          field_target <- .unwrap_num(tgt$target_pop)
          tbl <- .build_team_summary_table(team_one, field_target, u5_rast, urban_sf, team_field_overrides_by_ha[[han]], team_worldpop_by_ha[[han]], pop_scale,
                                           ha_worldpop = dfa_worldpop_pop[[han]], campaign_id = campaign_id)
          if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
          .team_id_cols(cbind(district_name = district_name, `Health Area` = han, tbl), snap$health_area_ids, team_ids_by_ha[[han]])
        })
        team_rows <- Filter(Negate(is.null), team_rows)
        if (length(team_rows) > 0) {
          team_pop_tbl <- do.call(rbind, team_rows)
          .write_csv_utf8_bom(.x(team_pop_tbl), file.path(tmp, 'team_population_summary.csv'), row.names = FALSE)
        }
      }
    }
  }, error = function(e) {
    warning(paste('Population summary export failed, continuing without it:', e$message))
  })

  # Each area layer twice: with the excluded areas (Unpopulated,
  # Inaccessible, Out of scope -- no gaps) and without them.
  ha_layer <- if (!is.null(ha_sf) && nrow(ha_sf) > 0) sf::st_transform(dplyr::mutate(.with_ha_ids(ha_sf, snap$health_area_ids), mapped = 'Yes'), 4326) else NULL
  oos <- tryCatch(.out_of_scope_sf(snap, ha_layer), error = function(e) { cat('[download] out-of-scope area failed:', e$message, '\n'); NULL })
  ha_layer   <- .add_area_type(.add_pop_fields(dplyr::bind_rows(ha_layer, oos), ha_pop_tbl), 'health area')
  team_layer <- .add_area_type(.add_pop_fields(dplyr::bind_rows(.team_layer_filled(team_sf, ha_sf, snap$health_area_ids), oos), ha_pop_tbl, team_pop_tbl), 'team area')
  .write_sf(ha_layer,                      paste0('health_areas_with_excluded_areas.', spec$ext))
  .write_sf(.without_excluded(ha_layer),   paste0('health_areas_without_excluded_areas.', spec$ext))
  .write_sf(team_layer,                    paste0('team_areas_with_excluded_areas.', spec$ext))
  .write_sf(.without_excluded(team_layer), paste0('team_areas_without_excluded_areas.', spec$ext))
  if (!is.null(district_sf_for_fetch) && nrow(district_sf_for_fetch) > 0)
    .write_sf(sf::st_sf(area_type = 'district', geometry = sf::st_geometry(district_sf_for_fetch)), paste0('districts_with_excluded_areas.', spec$ext))
  if (!is.null(ha_layer) && nrow(ha_layer) > 0)
    .write_sf(dplyr::select(.districts_without_excluded(dplyr::mutate(ha_layer, district_name = .env$district_name)), area_type),
              paste0('districts_without_excluded_areas.', spec$ext))

  # IDP settlements / landmarks are stored as plain data frames with lon/lat,
  # not sf — convert if present.
  .write_points_df <- function(df, fname) {
    if (is.null(df) || nrow(df) == 0 || !all(c('lon', 'lat') %in% names(df))) return(invisible(NULL))
    pts <- sf::st_as_sf(df, coords = c('lon', 'lat'), crs = 4326, remove = FALSE) |>
      dplyr::mutate(zone_name = .env$zone, region_name = .env$region, district_name = .env$district_name) |> .x() |> .shp_field_names(spec)
    sf::st_write(pts, file.path(tmp, fname), driver = spec$driver, layer_options = spec$opts, delete_dsn = TRUE, quiet = TRUE)
  }
  .write_points_df(snap$idp_settlements, paste0('idp_settlements.', spec$ext))
  .write_points_df(sites_dl,             paste0('microplan_sites.', spec$ext))
  .write_points_df(snap$landmarks,       paste0('landmarks.', spec$ext))

  # Facilities — combine locked ODK snapshot + user-added sites, same as
  # server.R does when restoring a session. Always CSV, regardless of
  # boundary format — a facility list isn't geometry-format-dependent.
  fac_parts <- Filter(function(x) inherits(x, 'sf') && nrow(x) > 0,
                      list(snap$odk_sf, snap$app_sf))
  if (length(fac_parts) > 0) {
    fac_sf <- do.call(rbind, fac_parts)
    fac_out <- sf::st_drop_geometry(fac_sf) |>
      dplyr::mutate(
        zone_name = .env$zone, region_name = .env$region, district_name = .env$district_name,
        lon = sf::st_coordinates(fac_sf)[, 1], lat = sf::st_coordinates(fac_sf)[, 2]
      ) |>
      .fac_ha_ids(snap$health_area_ids) |>
      dplyr::select(zone_name, region_name, district_name,
                    dplyr::any_of(c('facility_id', 'facility_name', 'facility_type',
                                    'hf_ownership', 'polio_sia_coordination_site',
                                    'health_area_id', 'health_area_name',
                                    'operational', 'lat', 'lon')))
    .write_csv_utf8_bom(.x(fac_out), file.path(tmp, 'facilities.csv'), row.names = FALSE)
  }

  out_files <- list.files(tmp, full.names = TRUE)
  if (length(out_files) == 0) {
    .write_csv_utf8_bom(data.frame(message = 'No data available.'), file, row.names = FALSE)
    return(invisible(NULL))
  }
  tryCatch(.write_export_readme(tmp, paste(district_name, 'district'), codes_ctx$campaign_name, spec),
           error = function(e) cat('[download] README failed:', e$message, '\n'))
  out_files <- list.files(tmp, full.names = TRUE)
  tryCatch(
    zip::zip(zipfile = file, files = out_files, mode = 'cherry-pick'),
    error = function(e) utils::zip(zipfile = file, files = out_files, flags = '-j')
  )
  invisible(NULL)
}

#' Build ONE stitched-together package for a whole campaign: a single
#' health_areas layer and a single team_areas layer spanning every
#' district (not a zip-of-per-district-zips, which is what this used to
#' produce), plus a districts layer covering every district in
#' districts_shp with an in_campaign flag marking which ones are
#' actually assigned to this campaign. IDP/landmarks/facilities are
#' stitched the same way, for the same reason build_district_download_v2()
#' includes them at all -- omitting them here would make the campaign
#' export silently lose data that the per-district export still has.
#'
#' @param file           output zip path
#' @param campaign_id    integer
#' @param format         'geojson' (default), 'shp', or 'kml'
#' @param progress_callback optional function(i, n, district_name) called
#'                       once per district as it's processed -- lets a
#'                       caller (e.g. Shiny's withProgress/incProgress)
#'                       show real per-district progress on what can be a
#'                       long loop, rather than a single opaque "working".
#'                       Never required; a plain function() {} default is
#'                       used when the caller doesn't need progress.
build_campaign_download_v2 <- function(file, campaign_id, format = 'geojson', progress_callback = function(i, n, district_name) {}) {
  spec <- export_format_spec(format)
  codes_ctx <- .export_codes_ctx(campaign_id)
  .x <- function(df) .add_export_codes(df, codes_ctx)

  progress <- tryCatch(db_get_campaign_progress(pool, campaign_id), error = function(e) NULL)
  if (is.null(progress) || nrow(progress) == 0) {
    .write_csv_utf8_bom(data.frame(message = 'No published districts for this campaign.'), file, row.names = FALSE)
    return(invisible(NULL))
  }

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  ha_parts       <- list()
  team_parts     <- list()
  idp_parts      <- list()
  site_parts     <- list()
  landmark_parts <- list()
  fac_parts      <- list()
  subdiv_parts   <- list()
  urban_parts    <- list()
  ha_summary_parts   <- list()
  team_summary_parts <- list()
  scope_pop_parts    <- list()

  for (i in seq_len(nrow(progress))) {
    dname   <- progress$district_name[i]
    progress_callback(i, nrow(progress), dname)
    ver_id  <- progress$version_id[i]
    version <- tryCatch(db_get_version_by_id(pool, ver_id), error = function(e) NULL)
    if (is.null(version)) next

    dinfo <- districts_shp_for_campaign(campaign_id) |> dplyr::filter(district_name == dname)
    zone_val   <- if (nrow(dinfo) > 0) as.character(dinfo$zone_name[1])   else ''
    region_val <- if (nrow(dinfo) > 0) as.character(dinfo$region_name[1]) else ''

    snap <- version$snap %||% list()
    snap$idp_settlements <- tryCatch(version_idp_points(snap, dname, campaign_id), error = function(e) snap$idp_settlements)
    sd <- tryCatch(district_sites_df(dname, campaign_id), error = function(e) NULL)
    if (!is.null(sd) && nrow(sd) > 0)
      site_parts[[dname]] <- sf::st_as_sf(sd, coords = c('lon', 'lat'), crs = 4326, remove = FALSE) |>
        dplyr::mutate(district_name = dname)

    # Subdivisions and campaign/urban extent for THIS district -- same
    # per-district fetch as the single-district export path above, same
    # reasoning for using districts_shp rather than snap$district_
    # boundary_sf (see that path's own comment for why).
    dsf_for_fetch <- if (nrow(dinfo) > 0) tryCatch(
      dinfo |> dplyr::group_by(district_name) |> dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
        sf::st_as_sf() |> safe_make_valid() |> sf::st_transform(4326),
      error = function(e) NULL
    ) else NULL
    subdiv_d <- if (!is.null(dsf_for_fetch)) tryCatch(fetch_subdivisions_for_district(dsf_for_fetch), error = function(e) NULL) else NULL
    urban_d  <- if (!is.null(dsf_for_fetch)) tryCatch(fetch_campaign_extent_for_district(dsf_for_fetch, campaign_id), error = function(e) NULL) else NULL
    if (!is.null(subdiv_d) && nrow(subdiv_d) > 0)
      subdiv_parts[[dname]] <- subdiv_d |> dplyr::mutate(zone_name = zone_val, region_name = region_val, district_name = dname)
    if (!is.null(urban_d) && nrow(urban_d) > 0)
      urban_parts[[dname]] <- urban_d |> dplyr::mutate(zone_name = zone_val, region_name = region_val, district_name = dname)

    # Health areas -- same smoothed-preferred-over-raw logic as the
    # per-district export, each district's piece tagged and reprojected
    # to a common CRS (4326) before collecting, since sf requires a
    # single matching CRS across everything eventually combined.
    ha_sf <- snap$smoothed_dfa_sf %||% snap$saved_dfa_sf
    dfa_worldpop_pop <- snap$dfa_worldpop_pop %||% list()
    if (!is.null(ha_sf) && nrow(ha_sf) > 0) {
      ha_parts[[dname]] <- .with_ha_ids(ha_sf, snap$health_area_ids) |>
        dplyr::mutate(mapped = 'Yes', zone_name = zone_val, region_name = region_val, district_name = dname) |>
        sf::st_transform(4326)
    }
    oos_d <- tryCatch(.out_of_scope_sf(snap, ha_parts[[dname]]), error = function(e) NULL)
    if (!is.null(oos_d)) {
      oos_d <- dplyr::mutate(oos_d, zone_name = zone_val, region_name = region_val, district_name = dname)
      ha_parts[[dname]] <- dplyr::bind_rows(ha_parts[[dname]], oos_d)
    }

    # Team areas -- same per-health-area combination as the per-district
    # export (there is no single "this district's team areas" row,
    # only independently-current ones per health area).
    team_field_overrides_by_ha <- list()
    team_worldpop_by_ha <- list()
    team_ids_by_ha <- list()
    team_sf <- tryCatch({
      ha_names <- setdiff(unlist(snap$dfa_names %||% list()), c('Inaccessible', 'Unpopulated'))
      tparts <- lapply(ha_names, function(han) {
        tv <- db_get_current_team_area_version(pool, campaign_id, dname, han)
        if (is.null(tv)) return(NULL)
        geom <- tv$snap$smoothed_team_sf %||% tv$snap$saved_team_sf
        if (is.null(geom) || nrow(geom) == 0) return(NULL)
        geom$health_area <- han
        geom$ha_id  <- tv$health_area_id %||% NA_character_
        geom$fac_id <- tv$facility_id %||% NA_character_; geom$ta_id <- vapply(as.character(geom$dfa_name %||% rep(NA_character_, nrow(geom))), function(n) as.character(tv$snap$team_ids[[n]] %||% NA_character_), character(1), USE.NAMES = FALSE)
        team_field_overrides_by_ha[[han]] <<- tv$snap$team_field_pop_overrides %||% list()
        team_worldpop_by_ha[[han]] <<- tv$snap$team_worldpop_pop %||% list()
        team_ids_by_ha[[han]] <<- tv$snap$team_ids %||% list()
        geom
      })
      tparts <- Filter(Negate(is.null), tparts)
      if (length(tparts) == 0) NULL else dplyr::bind_rows(tparts)
    }, error = function(e) NULL)
    team_layer <- .team_layer_filled(team_sf, ha_sf, snap$health_area_ids)
    if (!is.null(team_layer) && nrow(team_layer) > 0) {
      team_parts[[dname]] <- team_layer |>
        dplyr::mutate(zone_name = zone_val, region_name = region_val, district_name = dname) |>
        sf::st_transform(4326)
    }
    if (!is.null(oos_d)) team_parts[[dname]] <- dplyr::bind_rows(team_parts[[dname]], oos_d)

    if (!is.null(snap$idp_settlements) && nrow(snap$idp_settlements) > 0 &&
        all(c('lon', 'lat') %in% names(snap$idp_settlements))) {
      idp_parts[[dname]] <- sf::st_as_sf(snap$idp_settlements, coords = c('lon', 'lat'), crs = 4326, remove = FALSE) |>
        dplyr::mutate(zone_name = zone_val, region_name = region_val, district_name = dname)
    }
    if (!is.null(snap$landmarks) && nrow(snap$landmarks) > 0 &&
        all(c('lon', 'lat') %in% names(snap$landmarks))) {
      landmark_parts[[dname]] <- sf::st_as_sf(snap$landmarks, coords = c('lon', 'lat'), crs = 4326, remove = FALSE) |>
        dplyr::mutate(zone_name = zone_val, region_name = region_val, district_name = dname)
    }

    fparts <- Filter(function(x) inherits(x, 'sf') && nrow(x) > 0, list(snap$odk_sf, snap$app_sf))
    if (length(fparts) > 0) {
      fsf <- dplyr::bind_rows(fparts)
      fac_parts[[dname]] <- sf::st_drop_geometry(fsf) |>
        dplyr::mutate(
          zone_name = zone_val, region_name = region_val, district_name = dname,
          lon = sf::st_coordinates(fsf)[, 1], lat = sf::st_coordinates(fsf)[, 2]
        ) |> .fac_ha_ids(snap$health_area_ids)
    }

    # Same population/team-count summary as the single-district export,
    # accumulated per district in this same loop rather than a separate
    # pass -- keeps ha_sf/team_sf/team_targets in scope from this
    # iteration instead of re-fetching each district's version a second
    # time after the loop.
    tryCatch({
      if (!is.null(ha_sf) && nrow(ha_sf) > 0) {
        team_targets <- snap$team_targets %||% list()
        # Reuses urban_d (fetched once per district, above) rather than
        # a second fetch_campaign_extent_for_district() call.
        urban_sf <- urban_d
        # Same construction as the single-district export above.
        idp_sf_export <- NULL
        if (!is.null(snap$idp_settlements) && nrow(snap$idp_settlements) > 0 &&
            all(c('lon', 'lat') %in% names(snap$idp_settlements))) {
          idp_sf_export <- sf::st_as_sf(snap$idp_settlements, coords = c('lon', 'lat'), crs = 4326, remove = FALSE)
        }
        pop_scale <- scope_pop_scale(snap, campaign_id)
        ha_summary <- .build_health_area_summary_table(ha_sf, team_targets, u5_rast, campaign_id, urban_sf, idp_sf_export, dfa_worldpop_pop, pop_scale)
        if (!is.null(ha_summary) && nrow(ha_summary) > 0) {
          ha_summary_parts[[dname]] <- .ha_id_cols(cbind(district_name = dname, ha_summary), snap$health_area_ids)
        }
        scope_pop_parts[[dname]] <- .scope_population_row(dname, snap, campaign_id)
        if (!is.null(team_sf) && nrow(team_sf) > 0) {
          d_team_rows <- lapply(unique(team_sf$health_area), function(han) {
            team_one <- team_sf[team_sf$health_area == han, , drop = FALSE]
            tgt <- team_targets[[han]]
            field_target <- .unwrap_num(tgt$target_pop)
            tbl <- .build_team_summary_table(team_one, field_target, u5_rast, urban_sf, team_field_overrides_by_ha[[han]], team_worldpop_by_ha[[han]], pop_scale,
                                             ha_worldpop = dfa_worldpop_pop[[han]], campaign_id = campaign_id)
            if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
            .team_id_cols(cbind(district_name = dname, `Health Area` = han, tbl), snap$health_area_ids, team_ids_by_ha[[han]])
          })
          d_team_rows <- Filter(Negate(is.null), d_team_rows)
          if (length(d_team_rows) > 0) team_summary_parts[[dname]] <- do.call(rbind, d_team_rows)
        }
      }
    }, error = function(e) {
      warning(paste('Population summary failed for', dname, '-- continuing without it:', e$message))
    })
  }

  # dplyr::bind_rows() rather than do.call(rbind, ...) -- different
  # districts' pieces can plausibly have slightly different column sets
  # (e.g. one district never had any Inaccessible cells drawn), and
  # bind_rows() fills missing columns with NA instead of erroring.
  .write_combined_sf <- function(parts, fname, pop = identity) {
    if (length(parts) == 0) return(invisible(NULL))
    combined <- if (inherits(parts, 'sf')) parts else dplyr::bind_rows(parts)
    combined <- .x(pop(combined)) |> .shp_field_names(spec)
    if (is.null(combined) || nrow(combined) == 0) return(invisible(NULL))
    sf::st_write(combined, file.path(tmp, fname), driver = spec$driver, layer_options = spec$opts, delete_dsn = TRUE, quiet = TRUE)
  }
  # Campaign districts with no published health areas: one district-wide
  # placeholder in the health-area and team-area layers, so neither layer
  # has a hole where a district is still unmapped.
  tryCatch({
    cd <- db_get_campaign_districts(pool, campaign_id)
    for (dname in setdiff(if (is.null(cd)) character(0) else cd$district_name, names(ha_parts))) {
      dinfo <- districts_shp_for_campaign(campaign_id) |> dplyr::filter(district_name == dname)
      if (nrow(dinfo) == 0) next
      dg <- dinfo |> dplyr::group_by(district_name) |> dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
        sf::st_as_sf() |> safe_make_valid() |> sf::st_transform(4326)
      tag <- function(x) dplyr::mutate(x, zone_name = as.character(dinfo$zone_name[1]), region_name = as.character(dinfo$region_name[1]), district_name = dname)
      ha_parts[[dname]]   <- tag(.unmapped_district_sf(dg, dname, 'health area'))
      team_parts[[dname]] <- tag(.unmapped_district_sf(dg, dname, 'team area'))
    }
  }, error = function(e) cat('[download] unmapped-district placeholders failed:', e$message, '\n'))

  ha_pop_all   <- if (length(ha_summary_parts))   dplyr::bind_rows(ha_summary_parts)   else NULL
  team_pop_all <- if (length(team_summary_parts)) dplyr::bind_rows(team_summary_parts) else NULL
  # Each area layer twice: with the excluded areas (Unpopulated,
  # Inaccessible, Out of scope -- no gaps) and without them.
  ha_all   <- if (length(ha_parts))   .add_area_type(.add_pop_fields(dplyr::bind_rows(ha_parts), ha_pop_all), 'health area') else NULL
  team_all <- if (length(team_parts)) .add_area_type(.add_pop_fields(dplyr::bind_rows(team_parts), ha_pop_all, team_pop_all), 'team area') else NULL
  .write_combined_sf(ha_all,                      paste0('health_areas_with_excluded_areas.', spec$ext))
  .write_combined_sf(.without_excluded(ha_all),   paste0('health_areas_without_excluded_areas.', spec$ext))
  .write_combined_sf(team_all,                    paste0('team_areas_with_excluded_areas.', spec$ext))
  .write_combined_sf(.without_excluded(team_all), paste0('team_areas_without_excluded_areas.', spec$ext))
  .write_combined_sf(idp_parts,      paste0('idp_settlements.', spec$ext))
  .write_combined_sf(site_parts,     paste0('microplan_sites.', spec$ext))
  .write_combined_sf(landmark_parts, paste0('landmarks.', spec$ext))
  .write_combined_sf(subdiv_parts,   paste0('subdivisions.', spec$ext))
  .write_combined_sf(urban_parts,    paste0('campaign_extent.', spec$ext))

  if (length(ha_summary_parts) > 0) {
    combined <- dplyr::bind_rows(ha_summary_parts)
    if (nrow(combined) > 0) .write_csv_utf8_bom(.x(combined), file.path(tmp, 'health_area_population_summary.csv'), row.names = FALSE)
  }
  if (length(team_summary_parts) > 0) {
    combined <- dplyr::bind_rows(team_summary_parts)
    if (nrow(combined) > 0) .write_csv_utf8_bom(.x(combined), file.path(tmp, 'team_population_summary.csv'), row.names = FALSE)
  }
  if (length(scope_pop_parts) > 0)
    .write_csv_utf8_bom(.x(dplyr::bind_rows(scope_pop_parts)), file.path(tmp, 'scope_population_summary.csv'), row.names = FALSE)

  if (length(fac_parts) > 0) {
    fac_out <- dplyr::bind_rows(fac_parts) |>
      dplyr::select(zone_name, region_name, district_name,
                    dplyr::any_of(c('facility_id', 'facility_name', 'facility_type',
                                    'hf_ownership', 'polio_sia_coordination_site',
                                    'health_area_id', 'health_area_name',
                                    'operational', 'lat', 'lon')))
    .write_csv_utf8_bom(.x(fac_out), file.path(tmp, 'facilities.csv'), row.names = FALSE)
  }

  # District outlines: the campaign's districts (db_get_campaign_districts()
  # -- assigned, published or not), whole and with the excluded areas removed.
  campaign_districts <- tryCatch(db_get_campaign_districts(pool, campaign_id), error = function(e) NULL)
  campaign_district_names <- if (!is.null(campaign_districts) && nrow(campaign_districts) > 0)
    campaign_districts$district_name else character(0)
  tryCatch({
    keep <- c('zone_name', 'region_name', 'district_name')
    districts_out <- districts_shp_for_campaign(campaign_id) |>
      dplyr::filter(district_name %in% campaign_district_names) |>
      dplyr::group_by(dplyr::across(dplyr::any_of(keep))) |>
      dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
      sf::st_as_sf() |> safe_make_valid() |> sf::st_transform(4326) |>
      dplyr::mutate(area_type = 'district')
    .write_combined_sf(districts_out, paste0('districts_with_excluded_areas.', spec$ext))
    .write_combined_sf(.districts_without_excluded(ha_all), paste0('districts_without_excluded_areas.', spec$ext))
  }, error = function(e) cat('[download] failed to write districts layers:', e$message, '\n'))

  out_files <- list.files(tmp, full.names = TRUE)
  if (length(out_files) == 0) {
    .write_csv_utf8_bom(data.frame(message = 'No data available.'), file, row.names = FALSE)
    return(invisible(NULL))
  }
  tryCatch(.write_export_readme(tmp, 'all districts', codes_ctx$campaign_name, spec),
           error = function(e) cat('[download] README failed:', e$message, '\n'))
  out_files <- list.files(tmp, full.names = TRUE)
  tryCatch(
    zip::zip(zipfile = file, files = out_files, mode = 'cherry-pick'),
    error = function(e) utils::zip(zipfile = file, files = out_files, flags = '-j')
  )
  invisible(NULL)
}
