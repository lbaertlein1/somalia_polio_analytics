# =============================================================================
# exports.R -- a saved list's missed places as files, for the Send tab:
#   - KML per health area, to import into a Google My Maps map whose share
#     link is then sent to the supervisors (My Maps has no API, so the
#     operator imports it by hand -- the Send tab walks them through it)
#   - shapefiles for the district (operator-facing: includes children)
#   - a printed PDF map per health area, large format
# `places` are vb_get_places() rows: sf points (the navigation point) with
# an `outline` polygon column. Supervisor-facing files (KML, PDF) never show
# child numbers.
# =============================================================================

.xml <- function(x) htmltools::htmlEscape(as.character(x))

#' A health area's main parts only: drawn boundaries can carry many tiny
#' slivers from smoothing (a hundred or more rings), which would clutter
#' the maps. Parts under 5% of the largest are dropped.
.main_parts <- function(ha_poly) {
  if (is.null(ha_poly) || nrow(ha_poly) == 0) return(NULL)
  parts <- sf::st_cast(sf::st_cast(sf::st_union(sf::st_geometry(ha_poly)), 'MULTIPOLYGON'), 'POLYGON')
  a <- as.numeric(sf::st_area(parts))
  sf::st_sf(geometry = parts[a >= 0.05 * max(a)])
}

.kml_ring <- function(m) paste(sprintf('%.6f,%.6f,0', m[, 1], m[, 2]), collapse = ' ')
# a polygon's outer ring
.kml_outer <- function(poly) { m <- sf::st_coordinates(poly); .kml_ring(m[m[, 'L1'] == 1, 1:2, drop = FALSE]) }

#' One health area's missed places as KML: each place's area (one placemark
#' per polygon part -- My Maps doesn't import multi-geometries) and its
#' numbered navigation point with a directions link; the health area
#' boundary, if drawn, as a line. Returns the KML text.
places_kml <- function(places, title, ha_poly = NULL) {
  p <- places[order(places$place_no), ]
  team <- ifelse(is.na(p$assigned_team), '', paste0('<br>Team: ', .xml(team_label(p$assigned_team))))
  areas <- unlist(lapply(seq_len(nrow(p)), function(i) {
    parts <- sf::st_cast(sf::st_sfc(p$outline[[i]], crs = 4326), 'POLYGON')
    vapply(seq_along(parts), function(j) sprintf(
      '<Placemark><name>#%d %s</name><styleUrl>#area</styleUrl><Polygon><outerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></outerBoundaryIs></Polygon></Placemark>',
      p$place_no[i], .xml(p$label[i]), .kml_outer(parts[j])), '')
  }))
  points <- sprintf(
    '<Placemark><name>#%d</name><description><![CDATA[%s%s<br><a href="%s">Directions</a>]]></description><styleUrl>#point</styleUrl><Point><coordinates>%.6f,%.6f,0</coordinates></Point></Placemark>',
    p$place_no, .xml(p$label), team, gmaps_nav(p$lat, p$lon), p$lon, p$lat)
  ha_poly <- .main_parts(ha_poly)
  border <- if (!is.null(ha_poly) && nrow(ha_poly) > 0) {
    rings <- sf::st_geometry(ha_poly)
    vapply(seq_along(rings), function(j) sprintf(
      '<Placemark><name>Health area boundary</name><styleUrl>#border</styleUrl><LineString><coordinates>%s</coordinates></LineString></Placemark>',
      .kml_outer(rings[j])), '')
  }
  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n<kml xmlns="http://www.opengis.net/kml/2.2"><Document>',
    '<name>', .xml(title), '</name>',
    '<Style id="area"><LineStyle><color>ff1d1d7f</color><width>2</width></LineStyle><PolyStyle><color>662626dc</color></PolyStyle></Style>',
    '<Style id="point"><IconStyle><color>ff2626dc</color></IconStyle></Style>',
    '<Style id="border"><LineStyle><color>ff2a170f</color><width>3</width></LineStyle></Style>',
    paste(c(border, areas, points), collapse = '\n'),
    '</Document></kml>\n')
}

#' The district's missed places as two shapefiles (areas and navigation
#' points, same attributes), zipped into `file`.
places_shapefile_zip <- function(places, file) {
  d <- tempfile('shp'); dir.create(d); on.exit(unlink(d, recursive = TRUE))
  att <- data.frame(district = places$district_name, ha = places$health_area_name, place_no = places$place_no,
                    where_ = places$label, team = team_label(places$assigned_team), children = round(places$u5_pop),
                    status = places$status, lon = places$lon, lat = places$lat)
  sf::st_write(sf::st_sf(att, geometry = sf::st_cast(places$outline, 'MULTIPOLYGON')), file.path(d, 'missed_areas.shp'),
               quiet = TRUE, layer_options = 'ENCODING=UTF-8')
  sf::st_write(sf::st_sf(att, geometry = sf::st_geometry(places)), file.path(d, 'missed_points.shp'),
               quiet = TRUE, layer_options = 'ENCODING=UTF-8')
  zip::zip(file, list.files(d, full.names = TRUE), mode = 'cherry-pick')
}

#' Background tiles for a printed map. 'satellite' is Esri imagery with
#' Esri's transparent road and place-name layers laid over it (like
#' Google's satellite view with labels); 'osm' is the OpenStreetMap street
#' map. NULL if the tiles can't be fetched.
.map_tiles <- function(area, zoom, basemap) {
  cache <- file.path(tempdir(), 'tiles')
  get <- function(provider) tryCatch(maptiles::get_tiles(area, crop = TRUE, zoom = zoom, cachedir = cache, provider = provider),
                                     error = function(e) { message('[pdf] tiles: ', conditionMessage(e)); NULL })
  if (basemap == 'osm') return(get('OpenStreetMap'))
  img <- get('Esri.WorldImagery')
  if (is.null(img)) return(NULL)
  for (svc in c('World_Transportation', 'World_Boundaries_and_Places')) {
    ov <- get(maptiles::create_provider(name = paste0('esri_', svc), citation = 'Esri',
      url = sprintf('https://server.arcgisonline.com/ArcGIS/rest/services/Reference/%s/MapServer/tile/{z}/{y}/{x}', svc)))
    if (is.null(ov) || terra::nlyr(ov) < 4) next   # (no transparency: can't be laid over)
    ov <- terra::resample(ov, img, method = 'near')
    a <- terra::ifel(is.na(ov[[4]]), 0, ov[[4]] / 255)
    img <- img[[1:3]] * (1 - a) + terra::ifel(is.na(ov[[1:3]]), 0, ov[[1:3]]) * a
  }
  img
}

PAGE_MM <- list(A4 = c(297, 210), A3 = c(420, 297), A2 = c(594, 420), A1 = c(841, 594), A0 = c(1189, 841))   # landscape

#' One health area's printed map (PDF, landscape `size`): the missed areas
#' and numbered points over a street map or satellite background, the
#' health area boundary if drawn, the landmark each place's description names (from
#' `landmarks`: sf with `name` -- local place names, IDP sites, health
#' facilities), health facilities (`facilities`: sf with facility_name) as
#' red crosses, optionally the teams' GPS tracks (`tracks`: track_lines(); `track_points`:
#' clean_pings() points),
#' and the list of places beside it. The numbered places are drawn last, on top.
places_pdf <- function(places, file, title, subtitle, ha_poly = NULL, size = 'A1', basemap = c('osm', 'satellite'), landmarks = NULL,
                       tracks = NULL, track_points = NULL, facilities = NULL) {
  basemap <- match.arg(basemap)
  page <- PAGE_MM[[size]]
  p <- places[order(places$place_no), ]
  outl <- sf::st_transform(sf::st_sf(geometry = sf::st_cast(p$outline, 'MULTIPOLYGON')), 3857)
  pts <- sf::st_transform(sf::st_sf(no = p$place_no, geometry = sf::st_geometry(p)), 3857)
  ha <- .main_parts(ha_poly)
  if (!is.null(ha)) ha <- sf::st_transform(ha, 3857)

  # The map's extent (health area, or the places) with a margin, and tiles
  # detailed enough for the page at ~100 dpi (zoom capped at 16, so a
  # health area takes tens of tiles, not hundreds). Tiles are kept between
  # exports in the session's temp folder.
  bb <- sf::st_bbox(if (!is.null(ha)) ha else outl)
  pad <- 0.05 * max(bb[['xmax']] - bb[['xmin']], bb[['ymax']] - bb[['ymin']], 500)
  bb <- bb + c(-pad, -pad, pad, pad)
  px <- page[1] * 0.7 / 25.4 * 100
  zoom <- min(16, max(10, round(log2(156543.03 * px / (bb[['xmax']] - bb[['xmin']])))))
  tiles <- .map_tiles(sf::st_as_sfc(bb), zoom, basemap)

  s <- page[1] / 841   # text scales with the page (sizes below are for A1)
  map <- ggplot2::ggplot()
  # The background as one picture (not a table of every pixel, which takes
  # hundreds of MB for a large page)
  if (!is.null(tiles)) {
    v <- terra::values(tiles[[1:3]], mat = TRUE)
    v[is.na(v)] <- 255; v <- pmin(pmax(v, 0), 255)
    pic <- grDevices::as.raster(matrix(grDevices::rgb(v[, 1], v[, 2], v[, 3], maxColorValue = 255),
                                       nrow = terra::nrow(tiles), byrow = TRUE))
    rm(v)
    e <- as.vector(terra::ext(tiles))
    map <- map + ggplot2::annotation_raster(pic, xmin = e[['xmin']], xmax = e[['xmax']], ymin = e[['ymin']], ymax = e[['ymax']])
  }
  if (!is.null(tracks) && nrow(tracks) > 0)
    map <- map + ggplot2::geom_sf(data = sf::st_transform(tracks, 3857), colour = '#3b82f6', linewidth = 0.35 * s, alpha = 0.8)
  # the GPS points themselves, as on the app's maps
  if (!is.null(track_points) && nrow(track_points) > 0) {
    tp <- if (nrow(track_points) > 40000) track_points[round(seq(1, nrow(track_points), length.out = 40000)), ] else track_points
    tp <- sf::st_transform(sf::st_as_sf(tp, coords = c('lon', 'lat'), crs = 4326), 3857)
    map <- map + ggplot2::geom_sf(data = tp, shape = 16, size = 1.1 * s, colour = TRACK_POINT, alpha = 0.9)
  }
  if (!is.null(ha)) map <- map + ggplot2::geom_sf(data = ha, fill = NA, colour = if (basemap == 'satellite') 'white' else '#0f172a', linewidth = 1.6 * s)
  map <- map + ggplot2::geom_sf(data = outl, fill = '#dc2626', alpha = 0.3, colour = '#ff1a1a', linewidth = 1.3 * s)
  in_map <- function(x) {
    if (is.null(x) || nrow(x) == 0) return(NULL)
    x <- sf::st_transform(x, 3857); xy <- sf::st_coordinates(x)
    x <- x[xy[, 1] > bb[['xmin']] & xy[, 1] < bb[['xmax']] & xy[, 2] > bb[['ymin']] & xy[, 2] < bb[['ymax']], ]
    if (nrow(x) == 0) NULL else x
  }
  nudge <- 0.012 * (bb[['ymax']] - bb[['ymin']])
  # Health facilities: a red cross and the name
  if (!is.null(fac <- in_map(facilities)))
    map <- map +
      ggplot2::geom_sf(data = fac, shape = 3, size = 8 * s, colour = 'white', stroke = 3.2 * s) +
      ggplot2::geom_sf(data = fac, shape = 3, size = 8 * s, colour = '#dc2626', stroke = 1.8 * s) +
      ggplot2::geom_sf_label(data = fac, ggplot2::aes(label = facility_name), size = 5.5 * s, colour = '#b91c1c', fontface = 'bold',
                             fill = 'white', alpha = 0.85, label.size = 0, nudge_y = nudge)
  # The landmark each place's description names ("160 m SW of Madarasa",
  # "at Hodan IDP site"): the landmark of that name nearest the place. Places
  # described another way ("in Eldas HC") show none; places naming the same
  # landmark show it once.
  named <- sub('^(at |[0-9,]+ m (N|NE|E|SE|S|SW|W|NW) of )', '', p$label)
  named[named == p$label] <- NA   # (no landmark in the description)
  pick <- if (is.null(landmarks) || nrow(landmarks) == 0) integer(0) else unlist(lapply(seq_len(nrow(p)), function(i) {
    cand <- which(landmarks$name == named[i])
    if (length(cand) == 0) return(integer(0))
    cand[sf::st_nearest_feature(sf::st_geometry(p)[i], landmarks[cand, ])]
  }))
  if (length(pick) && !is.null(lm <- in_map(landmarks[base::unique(pick), ])))
    map <- map +
      ggplot2::geom_sf(data = lm, shape = 23, size = 7 * s, fill = '#facc15', colour = '#0f172a', stroke = 0.8 * s) +
      ggplot2::geom_sf_label(data = lm, ggplot2::aes(label = name), size = 5.5 * s, fill = 'white', alpha = 0.85,
                             label.size = 0, nudge_y = nudge)
  # The numbered places on top of everything
  map <- map +
    ggplot2::geom_sf(data = pts, shape = 21, size = 16 * s, fill = '#dc2626', colour = 'white', stroke = 1.2 * s) +
    ggplot2::geom_sf_text(data = pts, ggplot2::aes(label = no), colour = 'white', fontface = 'bold', size = 7 * s) +
    ggplot2::coord_sf(crs = 3857, xlim = bb[c('xmin', 'xmax')], ylim = bb[c('ymin', 'ymax')], expand = FALSE, datum = NA) +
    ggspatial::annotation_scale(location = 'bl', text_cex = 1.4 * s, height = ggplot2::unit(0.4 * s, 'cm')) +
    ggspatial::annotation_north_arrow(location = 'tr', height = ggplot2::unit(2 * s, 'cm'), width = ggplot2::unit(2 * s, 'cm')) +
    ggplot2::theme_void()

  # The list of places beside the map: number, where, team
  rows <- sprintf('%d.  %s%s', p$place_no, p$label, ifelse(is.na(p$assigned_team), '', paste0('  (', team_label(p$assigned_team), ')')))
  per_col <- 30
  cols <- split(rows, ceiling(seq_along(rows) / per_col))
  lst <- ggplot2::ggplot() + ggplot2::xlim(0, length(cols)) + ggplot2::ylim(-per_col, 1) + ggplot2::theme_void() +
    ggplot2::annotate('text', x = 0, y = 1, label = 'Missed places', hjust = 0, vjust = 1, fontface = 'bold', size = 11 * s)
  for (k in seq_along(cols))
    lst <- lst + ggplot2::annotate('text', x = k - 1, y = -seq_along(cols[[k]]), label = strwrap(cols[[k]], 70, simplify = FALSE) |>
                                     vapply(paste, '', collapse = '\n   '), hjust = 0, vjust = 1, size = 7 * s)

  plot <- patchwork::wrap_plots(map, lst, widths = c(0.7, 0.3 * length(cols))) +
    # the subtitle's second line is the key
    patchwork::plot_annotation(title = title, subtitle = paste0(subtitle, '\nRed circles: missed places, numbered as in the list  ·  ',
                               "Yellow diamonds: the landmark each place's description refers to  ·  Red crosses: health facilities"),
                               theme = ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 20 * s, colour = '#475569', lineheight = 1.3),
      plot.title = ggplot2::element_text(size = 34 * s, face = 'bold'),
      plot.margin = ggplot2::margin(15 * s, 15 * s, 15 * s, 15 * s, 'mm')))
  # (a .png file name gives a quick low-resolution preview instead)
  if (grepl('[.]png$', file)) ggplot2::ggsave(file, plot, width = page[1], height = page[2], units = 'mm', dpi = 40)
  else ggplot2::ggsave(file, plot, width = page[1], height = page[2], units = 'mm', device = grDevices::cairo_pdf)
  invisible(file)
}

#' A file-name-safe version of a name ("Eldas Health Center" -> "Eldas_Health_Center")
.file_part <- function(x) gsub('_+', '_', gsub('[^A-Za-z0-9]+', '_', x))
