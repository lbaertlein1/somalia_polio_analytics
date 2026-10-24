# =============================================================================
# subdivision_helpers.R
#
# Fetches urban subdivision polygons from the ArcGIS FeatureServer for the
# selected district. Results are clipped to the district boundary, cached
# per district within the session, and used as a context layer on every
# map, the printable PDF and the boundary download (plus soft barriers
# for health-area generation, via subdivisions_to_boundary_lines()).
#
# Source:
#   https://services.arcgis.com/5T5nSi527N4F7luB/ArcGIS/rest/services/
#   som_city_sections_2026v1/FeatureServer/0
#
# Fields used:
#   NAME_US  — subdivision name (English)
#   NAME_L2  — district name in service (for reference only; not used for matching)
# =============================================================================

ARCGIS_SUBDIVISIONS_URL <- paste0(
  "https://services.arcgis.com/5T5nSi527N4F7luB/ArcGIS/rest/services/",
  "som_city_sections_2026v1/FeatureServer/0/query"
)


# Session-level cache keyed by district_name
.subdivision_cache <- new.env(parent = emptyenv())

# Cache is keyed only by district (plus the dist_/urban_ prefix) -- NOT
# by URL. So changing either data-source URL in the admin page has no
# effect on a district that was already fetched and cached under the
# OLD url, until this is called to clear it out. Called from
# mod_admin_tab_v2.R's save_sources_btn handler whenever either URL is
# actually saved -- without this, "live" URL changes would only ever
# apply to districts nobody had looked at yet, which isn't genuinely
# live in any way an admin testing the change would notice.
clear_subdivision_cache <- function() {
  rm(list = ls(envir = .subdivision_cache, all.names = TRUE), envir = .subdivision_cache)
}


# -----------------------------------------------------------------------------
# fetch_subdivisions_for_district()
#
# Queries the ArcGIS endpoint with a bounding-box spatial filter derived from
# district_sf, then clips every returned polygon to the district boundary.
# Returns an sf object (CRS 4326) with columns:
#   subdivision_name, geometry
# or NULL if no subdivisions intersect the district.
# -----------------------------------------------------------------------------
fetch_subdivisions_for_district <- function(district_sf, url = ARCGIS_SUBDIVISIONS_URL, cache_prefix = "dist_") {
  
  req(!is.null(district_sf), nrow(district_sf) > 0)
  
  district_name <- as.character(district_sf$district_name[[1]])
  
  # Return cached result if available
  cache_key <- paste0(cache_prefix, gsub("[^A-Za-z0-9]", "_", district_name))
  if (!is.null(.subdivision_cache[[cache_key]])) {
    cat("[subdivisions] cache hit for", district_name, "\n")
    return(.subdivision_cache[[cache_key]])
  }
  
  cat("[subdivisions] fetching for", district_name, "| cache_prefix:", cache_prefix, "| url:", url, "\n")
  
  # Build bounding box in WGS84 for the spatial filter
  bbox <- district_sf |>
    sf::st_transform(4326) |>
    sf::st_bbox()
  
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
    # "*" -- NOT the previous "FID,NAME_US". This function is shared by
    # three different layers now (city sections, campaign extent,
    # settlement extents), each with its OWN field schema -- "NAME_US"
    # is specific to city sections (subdivisions_url); the capitals-
    # bounding-box layer (campaign_extent_url) uses "NAME_L2" instead,
    # and settlement extents uses plain "NAME". Requesting a field name
    # a layer doesn't actually have is exactly what produced ArcGIS's
    # "'outFields' parameter is invalid" error, confirmed directly
    # against the capitals-bounding-box layer's real field list. "*"
    # sidesteps needing to know each layer's exact schema up front --
    # the name field itself is then auto-detected below from whatever
    # actually came back, the same pattern idp_helpers.R already uses.
    outFields        = "*",
    returnGeometry   = "true",
    outSR            = "4326",
    f                = "geojson"
  )
  
  resp <- tryCatch(
    httr::GET(url,
              query   = query_params,
              httr::timeout(30)),
    error = function(e) {
      warning("[subdivisions] request failed: ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(resp) || httr::http_error(resp)) {
    warning("[subdivisions] HTTP error for district: ", district_name)
    .subdivision_cache[[cache_key]] <- NULL
    return(NULL)
  }
  
  raw_sf <- tryCatch({
    body <- httr::content(resp, as = "text", encoding = "UTF-8")
    sf::st_read(body, quiet = TRUE)
  }, error = function(e) {
    warning("[subdivisions] parse error: ", conditionMessage(e))
    NULL
  })
  
  if (is.null(raw_sf) || nrow(raw_sf) == 0) {
    cat("[subdivisions] no features returned for", district_name, "\n")
    .subdivision_cache[[cache_key]] <- NULL
    return(NULL)
  }
  
  # Ensure CRS is 4326
  raw_sf <- sf::st_transform(raw_sf, 4326)
  
  # Clip each subdivision to the district boundary
  district_geom <- district_sf |>
    sf::st_transform(4326) |>
    sf::st_union() |>
    sf::st_make_valid()
  
  clipped <- tryCatch({
    result <- sf::st_intersection(
      sf::st_make_valid(raw_sf),
      district_geom
    )
    # Keep only polygon geometry types after intersection
    result <- result[
      sf::st_geometry_type(result) %in% c("POLYGON", "MULTIPOLYGON"), ,
      drop = FALSE
    ]
    result
  }, error = function(e) {
    warning("[subdivisions] clip error: ", conditionMessage(e))
    NULL
  })
  
  if (is.null(clipped) || nrow(clipped) == 0) {
    cat("[subdivisions] no features remaining after clip for", district_name, "\n")
    .subdivision_cache[[cache_key]] <- NULL
    return(NULL)
  }
  
  # Standardise output — keep subdivision_name and geometry only.
  # Auto-detected from whatever name-like field actually exists on
  # THIS layer's response, rather than assuming "NAME_US" specifically
  # -- same reasoning as the outFields="*" change above, and the same
  # auto-detection pattern idp_helpers.R already uses for its own
  # per-source field-name variation. "subdivision_name" (the very
  # column being built) is checked first, defensively, in case a
  # future source is ever pre-shaped to already use that name.
  .name_field_candidates <- c("subdivision_name", "NAME_US", "NAME_L2", "NAME", "Name", "name",
                              "NAME_LS", "DISP_L2", "DISP_LS")
  name_field <- .name_field_candidates[.name_field_candidates %in% names(clipped)][1]
  name_field_found <- !is.na(name_field)

  out <- clipped |>
    dplyr::mutate(subdivision_name = if (name_field_found) as.character(.data[[name_field]]) else "")

  if (name_field_found) {
    # A real name field exists -- an individual row missing its own
    # name value is a genuine data-quality issue worth dropping, same
    # as before this change.
    out <- out |> dplyr::filter(!is.na(subdivision_name), nzchar(subdivision_name))
  }
  # No name field found at all on this layer (e.g. a source with no
  # name-like column whatsoever) -- every row keeps subdivision_name =
  # "" rather than being dropped entirely. The geometry itself is still
  # perfectly valid and usable (this matters specifically for campaign
  # extent, where the polygon is what's needed, not a label) -- an
  # absent name should never silently discard otherwise-good shapes.

  out <- out |> dplyr::select(subdivision_name) |> dplyr::arrange(subdivision_name)
  
  if (nrow(out) == 0) {
    .subdivision_cache[[cache_key]] <- NULL
    return(NULL)
  }
  
  cat("[subdivisions]", nrow(out), "subdivisions clipped for", district_name, "\n")
  .subdivision_cache[[cache_key]] <- out
  out
}


# -----------------------------------------------------------------------------
# fetch_campaign_extent_for_district()
#
# Thin wrapper around fetch_subdivisions_for_district() for the "campaign
# extent" layer specifically -- the outer boundary a 'partial' district's
# Campaign Scope canvas is built over and can never paint past (see
# mod_campaign_scope_tab.R). A conceptually separate use of a subdivision-
# shaped ArcGIS layer from the intro tab's own subdivision picker, which
# still uses fetch_subdivisions_for_district() directly against
# ARCGIS_SUBDIVISIONS_URL unchanged.
#
# Reads its URL from the admin-configurable 'campaign_extent_url' data-
# source setting, which can be set globally or overridden per campaign
# (db_get_data_source_url's own campaign_id parameter -- see mod_admin_
# tab_v2.R's Manage Districts modal for the per-campaign override UI).
# Returns NULL if no URL is configured at all, at either level -- no
# computed fallback (the WorldPop density approximation this used to have
# was removed; an admin either provides a real extent or the Scope canvas
# is just the whole blank district instead).
#
# A separate cache_prefix (including campaign_id, since the URL can
# differ per campaign) keeps this from colliding with the picker's own
# "dist_"-prefixed cache entries for the same district, in the same
# shared .subdivision_cache environment.
#
# Returns the same shape as fetch_subdivisions_for_district() --
# subdivision_name/geometry columns -- since callers here only ever use
# the geometry, never that name column; keeping the same column name
# avoids a second, parallel output shape for no actual behavioral reason.
# -----------------------------------------------------------------------------
fetch_campaign_extent_for_district <- function(district_sf, campaign_id = NULL) {
  url <- tryCatch(db_get_data_source_url(pool, 'campaign_extent_url', campaign_id = campaign_id), error = function(e) NULL)
  if (is.null(url) || !nzchar(url)) return(NULL)
  # cache_prefix includes campaign_id -- this URL can differ per campaign
  # (db_get_data_source_url's own per-campaign override), so two different
  # campaigns querying the same district must never share a cached result.
  fetch_subdivisions_for_district(district_sf, url = url,
                                  cache_prefix = paste0('extent_', campaign_id %||% 'global', '_'))
}

# -----------------------------------------------------------------------------
# fetch_settlement_extents_for_district()
#
# WHO Settlement Extents (Capitals) -- shown as a context-only reference
# overlay on every map (Landmarks, Facilities, Campaign Scope, Health Areas,
# Team Areas), the same treatment fetch_subdivisions_for_district() already
# gets. Never involved in BFS propagation, never used for scope's own
# extent (that's fetch_campaign_extent_for_district() above, a distinct
# admin-configured source), and returns NULL rather than falling back to
# anything if no admin URL is configured -- there is nothing sensible to
# approximate a "regional capital built-up area" from.
# -----------------------------------------------------------------------------
fetch_settlement_extents_for_district <- function(district_sf) {
  url <- tryCatch(db_get_data_source_url(pool, 'settlement_extents_url'), error = function(e) NULL)
  if (is.null(url) || !nzchar(url)) return(NULL)
  fetch_subdivisions_for_district(district_sf, url = url, cache_prefix = 'settlement_extents_')
}


# -----------------------------------------------------------------------------
# subdivisions_to_boundary_lines()
#
# Extracts the interior subdivision boundary lines from a subdivisions sf
# object — i.e. the shared edges between subdivisions, clipped to the district.
# These are passed to the health area generation module as soft barriers.
#
# Returns an sf linestring object (CRS 4326), or NULL if fewer than 2
# subdivisions (no interior boundaries to extract).
# -----------------------------------------------------------------------------
subdivisions_to_boundary_lines <- function(subdivisions_sf) {
  if (is.null(subdivisions_sf) || nrow(subdivisions_sf) < 2) return(NULL)
  
  tryCatch({
    subdiv_3857 <- subdivisions_sf |>
      sf::st_transform(3857) |>
      sf::st_make_valid()
    
    # Extract boundary of each subdivision individually, then union.
    # This gives all edges (shared interior + outer).
    all_bounds <- sf::st_union(sf::st_boundary(subdiv_3857))
    
    # Buffer the outer envelope slightly (10 m) to absorb floating-point
    # misalignment, then subtract to isolate interior shared edges only.
    outer_buf <- sf::st_buffer(
      sf::st_boundary(sf::st_union(subdiv_3857)),
      dist = 10
    )
    
    interior <- tryCatch(
      sf::st_difference(all_bounds, outer_buf),
      error = function(e) all_bounds
    )
    interior <- sf::st_make_valid(interior)
    
    # Cast to individual linestrings and drop empties
    lines_sfc <- tryCatch(
      sf::st_cast(sf::st_sfc(interior, crs = 3857), "LINESTRING"),
      error = function(e) sf::st_sfc(interior, crs = 3857)
    )
    lines_sfc <- lines_sfc[!sf::st_is_empty(lines_sfc)]
    if (length(lines_sfc) == 0) return(NULL)
    
    result <- sf::st_as_sf(
      data.frame(boundary_type = rep("subdivision", length(lines_sfc))),
      geometry = lines_sfc
    ) |>
      sf::st_transform(4326)
    
    cat("[subdivisions] boundary lines extracted,",
        round(sum(as.numeric(sf::st_length(
          sf::st_transform(result, 3857)
        ))) / 1000, 1), "km total\n")
    
    result
  }, error = function(e) {
    warning("[subdivisions] boundary line extraction failed: ", conditionMessage(e))
    NULL
  })
}


