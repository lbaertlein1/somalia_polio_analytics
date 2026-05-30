# =============================================================================
# subdivision_helpers.R
#
# Fetches urban subdivision polygons from the ArcGIS FeatureServer for the
# selected district. Results are clipped to the district boundary, cached
# per district within the session, and used to offer subdivision-level
# planning areas on the intro tab.
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

# Minimum area (m²) for the remainder polygon to be offered as a planning
# option. Remainders smaller than this are treated as boundary slivers.
REMAINDER_MIN_AREA_M2 <- 1e6   # 1 km²

# Session-level cache keyed by district_name
.subdivision_cache <- new.env(parent = emptyenv())


# -----------------------------------------------------------------------------
# fetch_subdivisions_for_district()
#
# Queries the ArcGIS endpoint with a bounding-box spatial filter derived from
# district_sf, then clips every returned polygon to the district boundary.
# Returns an sf object (CRS 4326) with columns:
#   subdivision_name, geometry
# or NULL if no subdivisions intersect the district.
# -----------------------------------------------------------------------------
fetch_subdivisions_for_district <- function(district_sf) {
  
  req(!is.null(district_sf), nrow(district_sf) > 0)
  
  district_name <- as.character(district_sf$district_name[[1]])
  
  # Return cached result if available
  cache_key <- paste0("dist_", gsub("[^A-Za-z0-9]", "_", district_name))
  if (!is.null(.subdivision_cache[[cache_key]])) {
    cat("[subdivisions] cache hit for", district_name, "\n")
    return(.subdivision_cache[[cache_key]])
  }
  
  cat("[subdivisions] fetching for", district_name, "\n")
  
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
    outFields        = "FID,NAME_US",
    returnGeometry   = "true",
    outSR            = "4326",
    f                = "geojson"
  )
  
  resp <- tryCatch(
    httr::GET(ARCGIS_SUBDIVISIONS_URL,
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
  
  # Standardise output — keep subdivision_name and geometry only
  out <- clipped |>
    dplyr::transmute(
      subdivision_name = as.character(NAME_US)
    ) |>
    dplyr::filter(!is.na(subdivision_name), nzchar(subdivision_name)) |>
    dplyr::arrange(subdivision_name)
  
  if (nrow(out) == 0) {
    .subdivision_cache[[cache_key]] <- NULL
    return(NULL)
  }
  
  cat("[subdivisions]", nrow(out), "subdivisions clipped for", district_name, "\n")
  .subdivision_cache[[cache_key]] <- out
  out
}


# -----------------------------------------------------------------------------
# compute_remainder()
#
# Returns an sf row representing the district area not covered by the
# subdivision union, or NULL if that area is smaller than REMAINDER_MIN_AREA_M2
# (i.e. the subdivisions effectively cover the full district, or any gap is
# just a boundary sliver).
# -----------------------------------------------------------------------------
compute_remainder <- function(district_sf, subdivisions_sf) {
  
  if (is.null(subdivisions_sf) || nrow(subdivisions_sf) == 0) return(NULL)
  
  tryCatch({
    district_geom <- district_sf |>
      sf::st_transform(3857) |>
      sf::st_union() |>
      sf::st_make_valid()
    
    subdiv_union <- subdivisions_sf |>
      sf::st_transform(3857) |>
      sf::st_union() |>
      sf::st_make_valid()
    
    remainder_geom <- sf::st_difference(district_geom, subdiv_union)
    remainder_geom <- sf::st_make_valid(remainder_geom)
    
    # Extract polygons only (st_difference can return GEOMETRYCOLLECTION)
    remainder_geom <- tryCatch(
      sf::st_collection_extract(remainder_geom, "POLYGON"),
      error = function(e) remainder_geom
    )
    
    area_m2 <- as.numeric(sf::st_area(remainder_geom))
    
    if (length(area_m2) == 0 || sum(area_m2, na.rm = TRUE) < REMAINDER_MIN_AREA_M2) {
      cat("[subdivisions] remainder too small (<", REMAINDER_MIN_AREA_M2 / 1e6, "km²), not offering\n")
      return(NULL)
    }
    
    remainder_sf <- sf::st_as_sf(
      data.frame(subdivision_name = "District area outside subdivisions"),
      geometry = sf::st_sfc(sf::st_union(remainder_geom), crs = 3857)
    ) |>
      sf::st_transform(4326)
    
    cat("[subdivisions] remainder area:", round(sum(area_m2) / 1e6, 1), "km²\n")
    remainder_sf
    
  }, error = function(e) {
    warning("[subdivisions] remainder computation failed: ", conditionMessage(e))
    NULL
  })
}


# -----------------------------------------------------------------------------
# build_planning_area_sf()
#
# Returns the effective planning polygon as an sf object given the user's
# selection. district_sf is always the fallback for "Full district".
# -----------------------------------------------------------------------------
build_planning_area_sf <- function(
    selection,          # character: subdivision_name, "Full district", or
    #            "District area outside subdivisions"
    district_sf,
    subdivisions_sf,
    remainder_sf
) {
  if (is.null(selection) || !nzchar(selection) ||
      selection == "Full district") {
    return(district_sf |> sf::st_transform(4326))
  }
  
  if (selection == "District area outside subdivisions") {
    if (!is.null(remainder_sf)) return(remainder_sf)
    warning("[subdivisions] remainder requested but not available; falling back to full district")
    return(district_sf |> sf::st_transform(4326))
  }
  
  # Named subdivision
  row <- subdivisions_sf[subdivisions_sf$subdivision_name == selection, ,
                         drop = FALSE]
  if (nrow(row) == 1) return(row)
  
  warning("[subdivisions] subdivision '", selection, "' not found; falling back to full district")
  district_sf |> sf::st_transform(4326)
}


# -----------------------------------------------------------------------------
# make_planning_label()
#
# Builds the combined label used as the DB key and in filenames.
# "Kismayo" + "Farjano"     -> "Kismayo — Farjano"
# "Kismayo" + "Full district" -> "Kismayo"
# -----------------------------------------------------------------------------
make_planning_label <- function(district_name, subdivision_name) {
  if (is.null(subdivision_name) ||
      !nzchar(subdivision_name) ||
      subdivision_name == "Full district") {
    return(district_name)
  }
  paste0(district_name, " \u2014 ", subdivision_name)
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


# -----------------------------------------------------------------------------
# compute_urban_hull()
#
# Returns the union of all subdivision polygons (dissolved into one geometry),
# clipped to the district boundary. This is the "Urban" planning unit.
# Uses direct union — no convex hull — so islands are preserved as-is.
# Returns an sf polygon or NULL on error.
# -----------------------------------------------------------------------------
compute_urban_hull <- function(subdivisions_sf, district_sf) {
  if (is.null(subdivisions_sf) || nrow(subdivisions_sf) == 0) return(NULL)
  tryCatch({
    subdiv_3857 <- subdivisions_sf |>
      sf::st_transform(3857) |>
      sf::st_make_valid()
    
    district_3857 <- district_sf |>
      sf::st_transform(3857) |>
      sf::st_union() |>
      sf::st_make_valid()
    
    # Direct union of subdivisions (no convex hull — islands preserved)
    urban_union <- sf::st_union(subdiv_3857) |>
      sf::st_make_valid()
    
    # Clip to district boundary
    urban <- sf::st_intersection(urban_union, district_3857) |>
      sf::st_make_valid()
    
    urban <- tryCatch(
      sf::st_collection_extract(urban, "POLYGON"),
      error = function(e) urban
    )
    
    cat("[planning_unit] urban area computed,",
        round(as.numeric(sf::st_area(urban)) / 1e6, 1), "km²\n")
    
    sf::st_as_sf(
      data.frame(planning_unit = "Urban"),
      geometry = sf::st_sfc(sf::st_union(urban), crs = 3857)
    ) |> sf::st_transform(4326)
  }, error = function(e) {
    warning("[planning_unit] urban area failed: ", conditionMessage(e))
    NULL
  })
}


# -----------------------------------------------------------------------------
# compute_rural_remainder()
#
# Returns the district polygon minus the urban hull — the "Rural" planning unit.
# Returns an sf polygon or NULL if the remainder is negligible (<1 km²).
# -----------------------------------------------------------------------------
compute_rural_remainder <- function(district_sf, urban_hull_sf) {
  if (is.null(urban_hull_sf)) return(NULL)
  tryCatch({
    district_3857 <- district_sf |>
      sf::st_transform(3857) |>
      sf::st_union() |>
      sf::st_make_valid()
    
    urban_3857 <- urban_hull_sf |>
      sf::st_transform(3857) |>
      sf::st_union() |>
      sf::st_make_valid()
    
    rural <- sf::st_difference(district_3857, urban_3857) |>
      sf::st_make_valid()
    
    rural <- tryCatch(
      sf::st_collection_extract(rural, "POLYGON"),
      error = function(e) rural
    )
    
    area_m2 <- sum(as.numeric(sf::st_area(rural)), na.rm = TRUE)
    if (area_m2 < 1e6) {
      cat("[planning_unit] rural remainder too small (<1 km²), not offering\n")
      return(NULL)
    }
    
    # st_union above already ensures a single geometry; wrap in sf with one row
    sf::st_as_sf(
      data.frame(planning_unit = "Rural"),
      geometry = sf::st_sfc(sf::st_union(rural), crs = 3857)
    ) |> sf::st_transform(4326)
  }, error = function(e) {
    warning("[planning_unit] rural remainder failed: ", conditionMessage(e))
    NULL
  })
}