`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

safe_make_valid <- function(x) {
  tryCatch(sf::st_make_valid(x), error = function(e) x)
}

clamp_num <- function(x, lo, hi) {
  max(lo, min(hi, x))
}

round_to_step <- function(x, step) {
  round(x / step) * step
}

# =============================================================================
# Sphere-safe polygons.
#
# st_make_valid() works on a flat plane, but sf/tmap treat lon/lat polygons
# as lying on a sphere (s2), where every edge is a great-circle arc. A long
# edge bows slightly away from the straight line, so a polygon that is valid
# on a flat map can have two edges cross on the sphere ("Loop 0 is not
# valid: Edge 4 crosses edge 61") -- and then any s2 operation on it (area,
# intersection, drawing with tmap) fails.
#
# s2_repair_polygons() finds such rows and fixes them by adding a vertex
# every 50 m (in the local UTM zone, so arcs and straight lines agree),
# then re-validating. Rows already valid on the sphere are returned
# untouched. Applied to every geometry saved to the database
# (.to_json_for_db) and again in the printable export as a backstop.
# Non-lon/lat data is returned as is. Attribute "s2_unfixed" = rows that
# are still invalid afterwards (normally 0).
# =============================================================================
s2_repair_polygons <- function(x, log_label = NULL) {
  if (is.null(x) || !(inherits(x, 'sf') || inherits(x, 'sfc')) || length(sf::st_geometry(x)) == 0) return(x)
  if (isFALSE(sf::st_is_longlat(x))) return(x)
  geo <- sf::st_geometry(x)
  if (is.na(sf::st_crs(geo))) sf::st_crs(geo) <- 4326
  invalid <- function(g) {
    v <- tryCatch(s2::s2_is_valid(g), error = function(e) rep(NA, length(g)))
    is.na(v) | !v
  }
  bad <- invalid(geo)
  if (!any(bad)) { attr(x, 's2_unfixed') <- 0L; return(x) }
  fixed <- lapply(which(bad), function(i) {
    g <- geo[i]
    ctr <- tryCatch(suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_set_crs(sf::st_zm(g), NA)))), error = function(e) NULL)
    if (is.null(ctr)) return(g)
    epsg <- (if (ctr[1, 'Y'] < 0) 32700 else 32600) + floor((ctr[1, 'X'] + 180) / 6) + 1
    out <- tryCatch(sf::st_segmentize(sf::st_transform(g, epsg), 50) |> sf::st_make_valid() |>
                      sf::st_collection_extract('POLYGON') |> sf::st_union() |> sf::st_transform(sf::st_crs(geo)),
                    error = function(e) NULL)
    if (is.null(out) || length(out) != 1) g else out
  })
  for (k in seq_along(fixed)) geo[which(bad)[k]] <- fixed[[k]]
  still <- sum(invalid(geo[bad]))
  if (!is.null(log_label))
    cat(sprintf('[s2_repair] %s: repaired %d of %d polygon(s) invalid on the sphere\n', log_label, sum(bad) - still, sum(bad)))
  if (inherits(x, 'sf')) sf::st_geometry(x) <- geo else x <- geo
  attr(x, 's2_unfixed') <- still
  x
}
