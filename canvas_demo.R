library(shiny)
library(sf)
library(jsonlite)
library(leaflet)

# ── Load & prep ──────────────────────────────────────────────────────────────
catchments <- readRDS("data/catchments.Rds")
catchments <- st_make_valid(catchments)
n <- nrow(catchments)

pal <- c("#e74c3c","#27ae60","#3498db","#9b59b6","#e67e22",
         "#1abc9c","#f39c12","#2980b9","#d35400","#16a085",
         "#8e44ad","#2c3e50","#c0392b","#2ecc71","#2471a3")
catchments$color <- pal[((seq_len(n) - 1L) %% length(pal)) + 1L]

# ── Densify edges ────────────────────────────────────────────────────────────
densify_ring <- function(coords, max_seg) {
  nr  <- nrow(coords)
  out <- NULL
  for (i in seq_len(nr - 1L)) {
    x0 <- coords[i,   1]; y0 <- coords[i,   2]
    x1 <- coords[i+1, 1]; y1 <- coords[i+1, 2]
    seg_len <- sqrt((x1-x0)^2 + (y1-y0)^2)
    n_div   <- max(1L, ceiling(seg_len / max_seg))
    ts      <- seq(0, 1, length.out = n_div + 1L)[-(n_div + 1L)]
    out     <- rbind(out, cbind(x0 + ts*(x1-x0), y0 + ts*(y1-y0)))
  }
  rbind(out, out[1L, , drop = FALSE])
}

all_edge_lens <- unlist(lapply(seq_len(n), function(i) {
  co <- st_coordinates(catchments[i,])[, c("X","Y")]
  sqrt(diff(co[,1])^2 + diff(co[,2])^2)
}))
max_seg <- median(all_edge_lens[all_edge_lens > 0]) / 4
cat(sprintf("max_seg = %.7f deg\n", max_seg))

densified_rings <- lapply(seq_len(n), function(i) {
  co <- st_coordinates(catchments[i,])[, c("X","Y")]
  densify_ring(co, max_seg)
})

# ── Shared node pool ──────────────────────────────────────────────────────────
node_x <- numeric(0); node_y <- numeric(0)

find_or_add_node <- function(x, y) {
  if (length(node_x) > 0L) {
    idx <- which(abs(node_x - x) < 1e-9 & abs(node_y - y) < 1e-9)
    if (length(idx) > 0L) return(idx[1L])
  }
  node_x <<- c(node_x, x); node_y <<- c(node_y, y)
  length(node_x)
}

ring_indices <- lapply(densified_rings, function(r)
  vapply(seq_len(nrow(r)), function(k) find_or_add_node(r[k,1], r[k,2]), integer(1)))

node_count <- tabulate(unlist(ring_indices), nbins = length(node_x))
node_fixed <- node_count == 1L
cat("Nodes:", length(node_x), "  Fixed:", sum(node_fixed), "\n")

# ── Seams ────────────────────────────────────────────────────────────────────
find_seams <- function() {
  seams <- list()
  for (i in seq_len(n - 1L)) {
    ri <- ring_indices[[i]]
    for (j in (i + 1L):n) {
      rj      <- ring_indices[[j]]
      in_sh   <- ri %in% rj
      shared_pos <- which(in_sh)
      if (length(shared_pos) < 2L) next
      runs <- split(shared_pos, cumsum(c(1L, diff(shared_pos) != 1L)))
      for (run in runs) {
        if (length(run) < 2L) next
        seams[[length(seams) + 1L]] <- list(
          i        = i - 1L,
          j        = j - 1L,
          nodeIdxs = ri[run] - 1L)
      }
    }
  }
  seams
}
seams <- find_seams()
cat("Seams:", length(seams), "\n")

# ── Map centre ───────────────────────────────────────────────────────────────
bb      <- st_bbox(catchments)
map_lng <- mean(c(bb["xmin"], bb["xmax"]))
map_lat <- mean(c(bb["ymin"], bb["ymax"]))

# ── Serialise ─────────────────────────────────────────────────────────────────
nodes_json <- toJSON(lapply(seq_along(node_x), function(i)
  list(x = node_x[i], y = node_y[i], fixed = node_fixed[i])), auto_unbox = TRUE)
rings_json <- toJSON(lapply(ring_indices, function(ri) ri - 1L), auto_unbox = TRUE)
meta_json  <- toJSON(lapply(seq_len(n), function(i)
  list(name = catchments$name[i], color = catchments$color[i])), auto_unbox = TRUE)
seams_json <- toJSON(seams, auto_unbox = TRUE)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    # Leaflet CSS
    tags$link(rel="stylesheet",
              href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"),
    tags$style(HTML("
      * { box-sizing:border-box; }
      html, body { margin:0; padding:0; height:100%; overflow:hidden;
                   font-family:monospace; background:#1a1a2e; }
      #map-wrap { position:relative; width:100vw; height:100vh; }
      #leaflet-map { position:absolute; inset:0; }
      /* Canvas sits on top of the leaflet map pane */
      #overlay-canvas {
        position:absolute; inset:0;
        pointer-events:none;   /* leaflet handles pan/zoom by default */
        z-index:450;
      }
      /* Re-enable pointer events only when in edit mode */
      #overlay-canvas.edit-active { pointer-events:all; }
      #ctrl {
        position:absolute; top:14px; left:50%; transform:translateX(-50%);
        z-index:1000;
        display:flex; align-items:center; gap:12px;
        background:rgba(17,17,17,0.92); border:1px solid #333;
        border-radius:20px; padding:7px 18px;
        color:#aaa; font-size:13px; white-space:nowrap;
        backdrop-filter:blur(4px);
      }
      #ctrl input[type=range]{ width:110px; accent-color:#f1c40f; }
      #sv { color:#f1c40f; min-width:40px; }
      #edit-btn {
        padding:3px 12px; border-radius:12px; border:1px solid #555;
        background:#222; color:#ccc; cursor:pointer; font-size:12px;
        font-family:monospace; transition:all .15s;
      }
      #edit-btn.active { background:#f1c40f; color:#111; border-color:#f1c40f; }
      #tip {
        position:absolute; pointer-events:none; display:none; z-index:1100;
        background:rgba(0,0,0,0.88); color:#fff; font-size:12px;
        padding:4px 10px; border-radius:8px; border:1px solid #555;
      }
      #inf {
        position:absolute; bottom:12px; left:50%; transform:translateX(-50%);
        z-index:1000; color:#777; font-size:12px;
        background:rgba(17,17,17,0.85); padding:5px 14px;
        border-radius:16px; border:1px solid #222;
      }
    "))
  ),
  div(id="map-wrap",
      div(id="leaflet-map"),
      tags$canvas(id="overlay-canvas"),
      div(id="ctrl",
          tags$button(id="edit-btn", "Edit borders"),
          tags$span("\u03c3"),
          tags$input(id="sslider", type="range",
                     min="0.0001", max="0.015", value="0.003", step="0.0001"),
          tags$span(id="sv", "0.003")
      ),
      div(id="tip"),
      div(id="inf", id="inf",
          "toggle \u2018Edit borders\u2019 to drag \u2014 pan/zoom otherwise")
  ),
  
  # Leaflet JS
  tags$script(src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"),
  
  tags$script(HTML(paste0("(function(){\n",
                          "const NODES_INIT=", nodes_json, ";\n",
                          "const RINGS=",      rings_json, ";\n",
                          "const META=",       meta_json,  ";\n",
                          "const SEAMS=",      seams_json, ";\n",
                          "const MAP_LNG=",    map_lng,    ";\n",
                          "const MAP_LAT=",    map_lat,    ";\n",
                          "
// ── Leaflet map ───────────────────────────────────────────────────────────
const lmap = L.map('leaflet-map', {
  center: [MAP_LAT, MAP_LNG],
  zoom: 13,
  zoomControl: true
});
L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
  attribution: '&copy; OpenStreetMap contributors',
  maxZoom: 19
}).addTo(lmap);

// ── Canvas overlay ────────────────────────────────────────────────────────
const cv  = document.getElementById('overlay-canvas');
const ctx = cv.getContext('2d');
const tip = document.getElementById('tip');
const wrap = document.getElementById('map-wrap');

function resizeCanvas(){
  cv.width  = wrap.clientWidth;
  cv.height = wrap.clientHeight;
  draw();
}
window.addEventListener('resize', resizeCanvas);
resizeCanvas();

// ── Geo <-> canvas projection (via Leaflet) ───────────────────────────────
// We use leaflet's latLngToContainerPoint so the overlay stays in sync
// with pan and zoom automatically.
function toC(lng, lat){
  const pt = lmap.latLngToContainerPoint(L.latLng(lat, lng));
  return {x: pt.x, y: pt.y};
}
function toG(cx, cy){
  const ll = lmap.containerPointToLatLng(L.point(cx, cy));
  return {x: ll.lng, y: ll.lat};
}

// Redraw on every map move/zoom
lmap.on('move zoom viewreset zoomend moveend', draw);

// ── Sigma ─────────────────────────────────────────────────────────────────
let sigma = 0.003;
document.getElementById('sslider').addEventListener('input', function(){
  sigma = +this.value;
  document.getElementById('sv').textContent = sigma.toFixed(4);
  draw();
});

// ── Edit mode toggle ──────────────────────────────────────────────────────
let editMode = false;
const editBtn = document.getElementById('edit-btn');
editBtn.addEventListener('click', ()=>{
  editMode = !editMode;
  editBtn.classList.toggle('active', editMode);
  cv.classList.toggle('edit-active', editMode);
  if(!editMode){ hoverIdx=-1; draw(); }
});

// ── Live node pool ────────────────────────────────────────────────────────
let nodes = NODES_INIT.map(nd=>({x:nd.x, y:nd.y, fixed:nd.fixed}));

// ── Helpers ───────────────────────────────────────────────────────────────
function gauss(d){ return Math.exp(-(d*d)/(2*sigma*sigma)); }

function d2seg(px,py,ax,ay,bx,by){
  const dx=bx-ax,dy=by-ay,l2=dx*dx+dy*dy;
  if(l2===0) return Math.hypot(px-ax,py-ay);
  const t=Math.max(0,Math.min(1,((px-ax)*dx+(py-ay)*dy)/l2));
  return Math.hypot(px-(ax+t*dx),py-(ay+t*dy));
}

function nearestOnSeam(si, gqx, gqy){
  const idxs=SEAMS[si].nodeIdxs;
  let best={dist:Infinity,x:gqx,y:gqy};
  for(let k=0;k<idxs.length-1;k++){
    const a=nodes[idxs[k]], b=nodes[idxs[k+1]];
    const dx=b.x-a.x,dy=b.y-a.y,l2=dx*dx+dy*dy;
    const t=l2===0?0:Math.max(0,Math.min(1,((gqx-a.x)*dx+(gqy-a.y)*dy)/l2));
    const cx=a.x+t*dx,cy=a.y+t*dy;
    const d=Math.hypot(gqx-cx,gqy-cy);
    if(d<best.dist) best={dist:d,x:cx,y:cy};
  }
  return best;
}

function hitSeam(cx2,cy2){
  let best={idx:-1,dist:Infinity};
  SEAMS.forEach((s,si)=>{
    for(let k=0;k<s.nodeIdxs.length-1;k++){
      const a=toC(nodes[s.nodeIdxs[k]].x,   nodes[s.nodeIdxs[k]].y);
      const b=toC(nodes[s.nodeIdxs[k+1]].x, nodes[s.nodeIdxs[k+1]].y);
      const d=d2seg(cx2,cy2,a.x,a.y,b.x,b.y);
      if(d<11&&d<best.dist) best={idx:si,dist:d};
    }
  });
  return best.idx;
}

// ── State ─────────────────────────────────────────────────────────────────
let hoverIdx=-1, dragging=false, dragSt=null, mouseCan=null;

// ── Draw ──────────────────────────────────────────────────────────────────
function hexToRgba(hex, alpha){
  const r=parseInt(hex.slice(1,3),16);
  const g=parseInt(hex.slice(3,5),16);
  const b=parseInt(hex.slice(5,7),16);
  return 'rgba('+r+','+g+','+b+','+alpha+')';
}

function draw(){
  const CW=cv.width, CH=cv.height;
  ctx.clearRect(0,0,CW,CH);

  // polygons
  RINGS.forEach((ring,pi)=>{
    if(ring.length<3) return;
    const p0=toC(nodes[ring[0]].x,nodes[ring[0]].y);
    ctx.beginPath(); ctx.moveTo(p0.x,p0.y);
    for(let k=1;k<ring.length;k++){
      const p=toC(nodes[ring[k]].x,nodes[ring[k]].y);
      ctx.lineTo(p.x,p.y);
    }
    ctx.closePath();
    ctx.fillStyle   = hexToRgba(META[pi].color, 0.35);
    ctx.fill();
    ctx.strokeStyle = hexToRgba(META[pi].color, 0.7);
    ctx.lineWidth   = 1.5;
    ctx.stroke();
  });

  if(!editMode) return;

  const asi = dragging ? dragSt.idx : hoverIdx;

  // influence ribbon
  if(asi>=0 && mouseCan){
    const s=SEAMS[asi];
    const mg=toG(mouseCan.x,mouseCan.y);
    const anchor=dragging?dragSt.anchorGeo:nearestOnSeam(asi,mg.x,mg.y);
    ctx.save(); ctx.globalCompositeOperation='screen';
    for(let k=0;k<s.nodeIdxs.length-1;k++){
      const na=nodes[s.nodeIdxs[k]], nb=nodes[s.nodeIdxs[k+1]];
      const a=toC(na.x,na.y), b=toC(nb.x,nb.y);
      const wA=gauss(Math.hypot(na.x-anchor.x,na.y-anchor.y));
      const wB=gauss(Math.hypot(nb.x-anchor.x,nb.y-anchor.y));
      if(wA<0.005&&wB<0.005) continue;
      const g=ctx.createLinearGradient(a.x,a.y,b.x,b.y);
      g.addColorStop(0,'rgba(241,196,15,'+(wA*0.3).toFixed(3)+')');
      g.addColorStop(1,'rgba(241,196,15,'+(wB*0.3).toFixed(3)+')');
      ctx.beginPath(); ctx.moveTo(a.x,a.y); ctx.lineTo(b.x,b.y);
      ctx.strokeStyle=g; ctx.lineWidth=Math.max(wA,wB)*28;
      ctx.lineCap='round'; ctx.stroke();
    }
    ctx.restore();
  }

  // seam lines (edit mode only — otherwise polygon strokes are enough)
  SEAMS.forEach((s,si)=>{
    const active=si===asi;
    const p0=toC(nodes[s.nodeIdxs[0]].x,nodes[s.nodeIdxs[0]].y);
    ctx.beginPath(); ctx.moveTo(p0.x,p0.y);
    for(let k=1;k<s.nodeIdxs.length;k++){
      const p=toC(nodes[s.nodeIdxs[k]].x,nodes[s.nodeIdxs[k]].y);
      ctx.lineTo(p.x,p.y);
    }
    ctx.strokeStyle = active&&dragging ? '#f1c40f'
                    : active           ? 'rgba(255,220,50,0.95)'
                    :                    'rgba(255,255,255,0.55)';
    ctx.lineWidth   = active ? 3 : 1.5;
    ctx.lineJoin='round'; ctx.lineCap='round';
    ctx.shadowColor = active&&dragging?'#f1c40f':'transparent';
    ctx.shadowBlur  = active&&dragging?8:0;
    ctx.stroke(); ctx.shadowBlur=0;
  });

  // interior nodes on active seam
  if(asi>=0){
    const s=SEAMS[asi];
    const mg=mouseCan?toG(mouseCan.x,mouseCan.y):null;
    const anchor=dragging?dragSt.anchorGeo:(mg?nearestOnSeam(asi,mg.x,mg.y):null);
    s.nodeIdxs.forEach(ni=>{
      if(nodes[ni].fixed) return;
      const p=toC(nodes[ni].x,nodes[ni].y);
      const w=anchor?gauss(Math.hypot(nodes[ni].x-anchor.x,nodes[ni].y-anchor.y)):0;
      const r=2.5+w*5;
      ctx.beginPath(); ctx.arc(p.x,p.y,r,0,Math.PI*2);
      ctx.fillStyle=w>0.5?'#f1c40f':w>0.15?'#ffe066':'rgba(255,255,255,0.8)';
      ctx.shadowColor=w>0.1?'#f1c40f':'transparent'; ctx.shadowBlur=w*10;
      ctx.fill(); ctx.shadowBlur=0;
      ctx.strokeStyle='rgba(10,10,20,0.5)'; ctx.lineWidth=1; ctx.stroke();
    });
  }

  // labels in edit mode
  ctx.font='bold 11px monospace'; ctx.textAlign='center'; ctx.textBaseline='middle';
  META.forEach((m,pi)=>{
    const ring=RINGS[pi];
    const cgx=ring.reduce((s,ni)=>s+nodes[ni].x,0)/ring.length;
    const cgy=ring.reduce((s,ni)=>s+nodes[ni].y,0)/ring.length;
    const cp=toC(cgx,cgy);
    const label=m.name
      .replace(' Health Centre','').replace(' Dispensary','')
      .replace(' MCH Centre','').replace(' Hospital','').replace(' PHU','');
    ctx.shadowColor='rgba(0,0,0,0.8)'; ctx.shadowBlur=3;
    ctx.fillStyle='rgba(255,255,255,0.9)';
    ctx.fillText(label,cp.x,cp.y);
    ctx.shadowBlur=0;
  });
}

// ── Mouse events (only fire in edit mode) ─────────────────────────────────
function canXY(e){
  const r=cv.getBoundingClientRect();
  return {x:e.clientX-r.left, y:e.clientY-r.top};
}

cv.addEventListener('mousemove',e=>{
  if(!editMode) return;
  const c=canXY(e); mouseCan=c;
  if(dragging){
    const s=SEAMS[dragSt.idx];
    const g=toG(c.x,c.y);
    const dx=g.x-dragSt.startGeo.x, dy=g.y-dragSt.startGeo.y;
    s.nodeIdxs.forEach((ni,k)=>{
      if(nodes[ni].fixed) return;
      const w=gauss(Math.hypot(
        dragSt.origPos[k].x-dragSt.anchorGeo.x,
        dragSt.origPos[k].y-dragSt.anchorGeo.y));
      nodes[ni].x=dragSt.origPos[k].x+w*dx;
      nodes[ni].y=dragSt.origPos[k].y+w*dy;
    });
    cv.style.cursor='grabbing';
    if(window.Shiny) Shiny.setInputValue('node_edits',
      nodes.map(nd=>({x:nd.x,y:nd.y})));
    draw(); return;
  }
  const si=hitSeam(c.x,c.y);
  if(si!==hoverIdx){ hoverIdx=si; draw(); }
  else if(si>=0) draw();
  cv.style.cursor=si>=0?'grab':'default';
  if(si>=0){
    tip.style.display='block';
    tip.style.left=(e.clientX+14)+'px'; tip.style.top=(e.clientY-8)+'px';
    tip.textContent=META[SEAMS[si].i].name+' / '+META[SEAMS[si].j].name;
  } else { tip.style.display='none'; }
});

cv.addEventListener('mousedown',e=>{
  if(!editMode) return;
  const c=canXY(e);
  const si=hitSeam(c.x,c.y);
  if(si<0) return;
  e.stopPropagation();   // prevent leaflet pan
  const s=SEAMS[si];
  const g=toG(c.x,c.y);
  dragSt={
    idx:si,
    anchorGeo:nearestOnSeam(si,g.x,g.y),
    startGeo:{x:g.x,y:g.y},
    origPos:s.nodeIdxs.map(ni=>({x:nodes[ni].x,y:nodes[ni].y}))
  };
  dragging=true; cv.style.cursor='grabbing'; draw();
});

document.addEventListener('mouseup',()=>{
  if(!dragging) return;
  dragging=false; dragSt=null;
  cv.style.cursor=editMode?'default':'default'; draw();
});

cv.addEventListener('mouseleave',()=>{
  mouseCan=null; hoverIdx=-1; tip.style.display='none';
  if(!dragging) draw();
});

draw();
})()")))
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  observeEvent(input$node_edits, {
    # input$node_edits: list of {x=lng, y=lat} for every node
    # Rebuild sf polygons:
    # coords_mat <- do.call(rbind, lapply(input$node_edits, unlist))
    # new_polys  <- lapply(ring_indices, function(ri) {
    #   st_polygon(list(coords_mat[ri, , drop=FALSE]))
    # })
    # new_sfc <- st_sfc(new_polys, crs=st_crs(catchments))
  })
}

shinyApp(ui, server)