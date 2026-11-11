# Export one self-contained Leaflet HTML per district from <WORK>/route_out/<district>.rds: current vs street-following proposal, overlaid.
# Env: WORK, DISTRICTS (comma list), OUTDIR.
suppressMessages(library(sf)); sf::sf_use_s2(FALSE); options(warn = -1)
S <- Sys.getenv('WORK'); if (!nzchar(S)) stop('set WORK to a working folder'); OUTDIR <- Sys.getenv('OUTDIR'); dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
gj <- function(x) geojsonsf::sf_geojson(x, digits = 6)
esc <- function(s) gsub('<', '&lt;', gsub('&', '&amp;', s))
for (D in trimws(strsplit(Sys.getenv('DISTRICTS'), ',')[[1]])) {
  r <- readRDS(file.path(S, 'route_out', paste0(gsub('[^A-Za-z0-9]+', '_', D), '.rds')))
  cur <- sf::st_sf(name = r$cur$dfa_name, geometry = sf::st_geometry(r$cur)); prop <- sf::st_sf(name = r$prop$dfa_name, geometry = sf::st_geometry(r$prop))
  # land changing hands: piece of proposal area j that was in current area i
  ch <- do.call(rbind, lapply(seq_len(nrow(prop)), function(j) { d <- sf::st_difference(sf::st_geometry(prop)[j], sf::st_union(sf::st_geometry(cur)[cur$name == prop$name[j]]))
    d <- tryCatch(sf::st_collection_extract(d, 'POLYGON'), error = function(e) d); if (!length(d) || all(sf::st_is_empty(d))) return(NULL)
    sf::st_sf(to = prop$name[j], geometry = sf::st_union(d)) }))
  if (!is.null(ch)) { ch$m2 <- round(as.numeric(sf::st_area(sf::st_transform(ch, 3857)))); ch <- ch[ch$m2 > 50, ] }
  bb <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(sf::st_transform(sf::st_union(cur), 3857), 300))) |> sf::st_transform(4326)
  roads <- if (!is.null(r$roads)) { x <- suppressWarnings(sf::st_intersection(r$roads, bb)); x <- x[sf::st_dimension(x) == 1, ]; sf::st_sf(cls = x$cls, nm = ifelse(is.na(x$name), '', x$name), geometry = sf::st_geometry(x)) } else NULL
  sites <- if (!is.null(r$sites) && nrow(r$sites)) sf::st_sf(name = r$sites$facility_name, geometry = sf::st_geometry(sf::st_transform(r$sites, 4326))) else NULL
  wp <- r$wp; wp$diff <- wp$worldpop_proposal - wp$worldpop_now; wp$pct <- ifelse(wp$worldpop_now > 0, round(100 * wp$diff / wp$worldpop_now, 1), NA)
  rows <- paste(sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td class="%s">%+d (%s%%)</td></tr>', esc(wp$health_area), format(wp$worldpop_now, big.mark = ','), format(wp$worldpop_proposal, big.mark = ','),
    ifelse(abs(wp$pct %||% 0) >= 10, 'big', ''), as.integer(wp$diff), ifelse(is.na(wp$pct), '-', sprintf('%+.1f', wp$pct))), collapse = '')
  rw <- r$row
  stats <- sprintf('Boundary along a street (within 3 m): <b>%.0f%% &rarr; %.0f%%</b> &middot; land changing health area: <b>%.1f%%</b> of the district &middot; boundary stretches re-routed: %d of %d &middot; coordination sites ending outside their own area: %d',
    rw$on_road_now, rw$on_road_prop, rw$area_moved_pct, rw$stretches_routed, rw$stretches_routed + rw$stretches_kept, rw$sites_moved_out)
  html <- paste0('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>', esc(D), ' boundary review</title>
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"><script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<style>html,body{margin:0;height:100%;font-family:Arial,sans-serif}#map{position:absolute;top:0;bottom:0;left:0;right:340px}#side{position:absolute;top:0;right:0;width:340px;bottom:0;overflow:auto;padding:10px;box-sizing:border-box;font-size:12px;background:#fafafa;border-left:1px solid #ccc}
h2{font-size:15px;margin:4px 0 8px}table{border-collapse:collapse;width:100%}td,th{border-bottom:1px solid #ddd;padding:3px 4px;text-align:left}td.big{color:#b00;font-weight:bold}.key span{display:inline-block;width:22px;height:0;margin-right:6px;vertical-align:middle}
.lbl{background:none;border:none;box-shadow:none;font-size:11px;font-weight:bold;color:#222;text-shadow:0 0 3px #fff,0 0 3px #fff}
@media (max-width:700px){#map{right:0;bottom:45%}#side{top:55%;width:100%;border-left:none;border-top:1px solid #ccc}}</style></head><body><div id="map"></div><div id="side">
<h2>', esc(D), ' &ndash; current vs street-following proposal</h2><p>DRAFT for review only &ndash; nothing has been changed in the app.</p><p>', stats, '</p>
<div class="key"><p><span style="border-top:3px solid #d62728"></span>current health areas (published)<br><span style="border-top:3px dashed #1f4fd6"></span>street-following proposal<br>
<span style="border-top:8px solid #ff9900;opacity:.6"></span>land that would change health area<br><span style="border-top:1px solid #777"></span>OSM streets / tracks / paths</p></div>
<p>Use the layer control (top right of the map) to switch layers on and off; click an area for its name.</p>
<table><tr><th>Health area</th><th>WorldPop U5 now</th><th>proposal</th><th>change</th></tr>', rows, '</table></div>
<script>
var cur=', gj(cur), ';var prop=', gj(prop), ';var ch=', if (!is.null(ch) && nrow(ch)) gj(ch) else '{"type":"FeatureCollection","features":[]}', ';var roads=', if (!is.null(roads) && nrow(roads)) gj(roads) else '{"type":"FeatureCollection","features":[]}',
  ';var sites=', if (!is.null(sites)) gj(sites) else '{"type":"FeatureCollection","features":[]}', ';
var map=L.map("map");var osm=L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",{maxZoom:19,attribution:"&copy; OpenStreetMap contributors"}).addTo(map);
var img=L.tileLayer("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",{maxZoom:19,attribution:"Esri World Imagery"});
var lr=L.geoJSON(roads,{style:{color:"#777",weight:1,opacity:.8},onEachFeature:function(f,l){if(f.properties.nm)l.bindTooltip(f.properties.nm+" ("+f.properties.cls+")")}});
var lc=L.geoJSON(cur,{style:{color:"#d62728",weight:3,fillColor:"#d62728",fillOpacity:.06},onEachFeature:function(f,l){l.bindPopup("<b>"+f.properties.name+"</b><br>current")}}).addTo(map);
var lp=L.geoJSON(prop,{style:{color:"#1f4fd6",weight:3,dashArray:"6 5",fillOpacity:0},onEachFeature:function(f,l){l.bindPopup("<b>"+f.properties.name+"</b><br>proposal")}}).addTo(map);
var lx=L.geoJSON(ch,{style:{color:"#ff9900",weight:0,fillColor:"#ff9900",fillOpacity:.45},onEachFeature:function(f,l){l.bindPopup("Would move to <b>"+f.properties.to+"</b><br>"+f.properties.m2.toLocaleString()+" m&sup2;")}}).addTo(map);
var ls=L.geoJSON(sites,{pointToLayer:function(f,ll){return L.circleMarker(ll,{radius:5,color:"#000",weight:1,fillColor:"#ffd700",fillOpacity:1})},onEachFeature:function(f,l){l.bindTooltip(f.properties.name)}}).addTo(map);
var ll=L.layerGroup();cur.features.forEach(function(f){var g=L.geoJSON(f);ll.addLayer(L.marker(g.getBounds().getCenter(),{icon:L.divIcon({className:"lbl",html:f.properties.name,iconSize:[140,14]})}))});ll.addTo(map);
lr.addTo(map);L.control.layers({"Streets (OSM)":osm,"Satellite (Esri)":img},{"Current boundaries (red)":lc,"Proposal (blue dashed)":lp,"Land changing area (orange)":lx,"OSM streets":lr,"Coordination sites":ls,"Labels":ll},{collapsed:false}).addTo(map);
L.control.scale().addTo(map);map.fitBounds(lc.getBounds());
</script></body></html>')
  f <- file.path(OUTDIR, paste0(gsub('[^A-Za-z0-9]+', '_', D), '_boundary_review.html')); writeLines(html, f, useBytes = TRUE)
  cat(sprintf('%s -> %s (%.0f KB)\n', D, f, file.size(f) / 1024)) }
