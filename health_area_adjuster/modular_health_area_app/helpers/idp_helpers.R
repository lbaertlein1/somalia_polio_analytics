# =============================================================================
# idp_helpers.R
#
# Fetches IDP settlement points for the selected district, following the
# same pattern as subdivision_helpers.R's fetch_subdivisions_for_district():
# an ArcGIS FeatureServer query filtered to the district's bounding box,
# session-cached per district.
#
# The key difference from subdivisions: the endpoint is admin-configurable
# (db_get_data_source_url(pool, 'idp_settlements_url')) rather than
# hardcoded, per your call that health-facility endpoints stay hardcoded
# but subdivisions/IDP are admin-editable. And since there is no real IDP
# data source wired up yet, field names for the settlement name are
# auto-detected from whatever the endpoint returns rather than assumed —
# swap in the real field name once a real source exists (a one-line change
# in .name_field_candidates below, or just let auto-detection keep working
# if the real source happens to use one of the listed candidates).
#
# Returns an sf POINT object (CRS 4326) with columns:
#   idp_name, geometry
# or NULL if no admin-configured URL is set, the request fails, or no
# features intersect the district.
# =============================================================================

# Session-level cache keyed by district_name — same lifetime/scope as
# .subdivision_cache in subdivision_helpers.R.
.idp_cache <- new.env(parent = emptyenv())

# "Settlement" and "IDP_Indivi" confirmed directly against the actual
# IDP Site Master List ArcGIS layer's real field metadata -- listed
# first so they match before the generic fallbacks below do. The
# generic candidates stay in the list for robustness against a
# DIFFERENT IDP source ever being configured with its own field names.
.name_field_candidates <- c(
  "Settlement", "idp_name", "settlement_name", "site_name", "name", "NAME", "NAME_US", "SITE_NAME"
)

.pop_field_candidates <- c(
  "IDP_Indivi", "population", "pop", "idp_population", "num_individuals", "individuals", "hh_count", "households"
)

.detect_field <- function(nm, candidates) {
  hit <- candidates[candidates %in% nm]
  if (length(hit) > 0) return(hit[1])
  # case-insensitive fallback
  low_match <- nm[match(tolower(candidates), tolower(nm), nomatch = 0)]
  low_match <- low_match[low_match != ""]
  if (length(low_match) > 0) return(low_match[1])
  NA_character_
}

# -----------------------------------------------------------------------------
# fetch_idp_settlements_for_district()
# -----------------------------------------------------------------------------
fetch_idp_settlements_for_district <- function(district_sf) {

  req(!is.null(district_sf), nrow(district_sf) > 0)

  district_name <- as.character(district_sf$district_name[[1]] %||% district_sf$dfa_name[[1]] %||% "unknown")
  cache_key <- paste0("dist_", gsub("[^A-Za-z0-9]", "_", district_name))
  if (!is.null(.idp_cache[[cache_key]])) {
    cat("[idp] cache hit for", district_name, "\n")
    return(.idp_cache[[cache_key]])
  }

  url <- tryCatch(db_get_data_source_url(pool, "idp_settlements_url"), error = function(e) NULL)
  if (is.null(url) || !nzchar(trimws(url %||% ""))) {
    cat("[idp] no source URL configured — skipping fetch for", district_name, "\n")
    return(NULL)
  }

  cat("[idp] fetching for", district_name, "\n")

  bbox <- district_sf |> sf::st_transform(4326) |> sf::st_bbox()
  geometry_param <- sprintf(
    '{"xmin":%f,"ymin":%f,"xmax":%f,"ymax":%f,"spatialReference":{"wkid":4326}}',
    bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]
  )

  query_params <- list(
    where            = "1=1",
    geometry         = geometry_param,
    geometryType     = "esriGeometryEnvelope",
    inSR             = "4326",
    spatialRel       = "esriSpatialRelIntersects",
    outFields        = "*",
    returnGeometry   = "true",
    outSR            = "4326",
    f                = "geojson"
  )

  resp <- tryCatch(
    httr::GET(url, query = query_params, httr::timeout(30)),
    error = function(e) { warning("[idp] request failed: ", conditionMessage(e)); NULL }
  )

  if (is.null(resp) || httr::http_error(resp)) {
    warning("[idp] HTTP error for district: ", district_name)
    .idp_cache[[cache_key]] <- NULL
    return(NULL)
  }

  raw_sf <- tryCatch({
    body <- httr::content(resp, as = "text", encoding = "UTF-8")
    sf::st_read(body, quiet = TRUE)
  }, error = function(e) { warning("[idp] parse error: ", conditionMessage(e)); NULL })

  if (is.null(raw_sf) || nrow(raw_sf) == 0) {
    cat("[idp] no features returned for", district_name, "\n")
    .idp_cache[[cache_key]] <- NULL
    return(NULL)
  }

  raw_sf <- sf::st_transform(raw_sf, 4326)

  # Keep point geometry only (settlements are points, not polygons/lines)
  raw_sf <- raw_sf[sf::st_geometry_type(raw_sf) %in% c("POINT", "MULTIPOINT"), , drop = FALSE]
  if (nrow(raw_sf) == 0) {
    warning("[idp] source returned no point geometry for: ", district_name)
    .idp_cache[[cache_key]] <- NULL
    return(NULL)
  }

  district_geom <- district_sf |> sf::st_transform(4326) |> sf::st_union() |> sf::st_make_valid()
  inside <- lengths(sf::st_within(raw_sf, district_geom)) > 0
  clipped <- raw_sf[inside, , drop = FALSE]

  if (nrow(clipped) == 0) {
    cat("[idp] no features inside district boundary for", district_name, "\n")
    .idp_cache[[cache_key]] <- NULL
    return(NULL)
  }

  nm <- names(sf::st_drop_geometry(clipped))
  name_field <- .detect_field(nm, .name_field_candidates)
  pop_field  <- .detect_field(nm, .pop_field_candidates)

  out <- clipped |>
    dplyr::mutate(
      idp_name = if (!is.na(name_field)) as.character(.data[[name_field]]) else paste("IDP Settlement", dplyr::row_number()),
      idp_population = if (!is.na(pop_field)) suppressWarnings(as.numeric(.data[[pop_field]])) else NA_real_
    ) |>
    dplyr::select(idp_name, idp_population, geometry) |>
    dplyr::filter(!is.na(idp_name), nzchar(idp_name))

  if (nrow(out) == 0) {
    .idp_cache[[cache_key]] <- NULL
    return(NULL)
  }

  cat("[idp]", nrow(out), "settlements clipped for", district_name,
      if (is.na(name_field)) " (name field not detected — using generic labels)" else "", "\n")
  .idp_cache[[cache_key]] <- out
  out
}

# Converts an idp_sf (idp_name/idp_population/geometry, per the function
# above) into the list(list(lat=, lon=, name=), ...) shape paint-app.js's
# paint_load_scene expects for idpPoints -- same conversion mod_facility_
# map.R and mod_campaign_scope_tab.R's own landmark_pts already do for
# their respective point layers, pulled out here once since all three
# paint-canvas tabs (Campaign Scope, Health Areas, Team Areas) need the
# identical conversion for THIS specific data.
idp_sf_to_points_list <- function(idp_sf) {
  if (is.null(idp_sf) || nrow(idp_sf) == 0) return(list())
  # Per-row st_coordinates(), NOT a single bulk call across the whole sf
  # object -- fetch_idp_settlements_for_district() deliberately keeps
  # BOTH POINT and MULTIPOINT geometries (idp_helpers.R's own geometry-
  # type filter), and st_coordinates() on a MULTIPOINT returns one row
  # per sub-point, not one row per feature. A single bulk call would
  # silently misalign coords[i,] against idp_sf$idp_name[i] the moment
  # any one row is a multipoint with >1 vertex -- producing wrong (or
  # out-of-range/NA) coordinates for every row after it, which is
  # exactly the kind of bad lat/lon that crashes Leaflet's CircleMarker
  # on the client (`Cannot read properties of null (reading 'lat')`).
  # Per-row extraction can never misalign across rows; a genuine
  # multipoint feature just contributes its first vertex.
  pts <- lapply(seq_len(nrow(idp_sf)), function(i) {
    coords <- tryCatch(sf::st_coordinates(sf::st_geometry(idp_sf)[i]), error = function(e) NULL)
    if (is.null(coords) || nrow(coords) == 0) return(NULL)
    lon <- coords[1, 1]; lat <- coords[1, 2]
    if (is.na(lon) || is.na(lat)) return(NULL)
    list(lat = lat, lon = lon, name = idp_sf$idp_name[i])
  })
  Filter(Negate(is.null), pts)
}
