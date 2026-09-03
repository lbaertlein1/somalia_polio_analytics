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

.name_field_candidates <- c(
  "idp_name", "settlement_name", "site_name", "name", "NAME", "NAME_US", "SITE_NAME"
)

.pop_field_candidates <- c(
  "population", "pop", "idp_population", "num_individuals", "individuals", "hh_count", "households"
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
