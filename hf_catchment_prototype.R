library(shiny)
library(sf)
library(dplyr)
library(jsonlite)

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

catchments <- readRDS(if (file.exists("data/catchments_edited.Rds"))
  "data/catchments_edited.Rds" else "data/catchments.Rds")

type_colors <- c(
  "Hospital"      = "#c0392b",
  "Health Centre" = "#2980b9",
  "MCH Centre"    = "#8e44ad",
  "PHU"           = "#27ae60",
  "Dispensary"    = "#e67e22"
)
get_col <- function(type) unname(type_colors[type]) %||% "#888888"

# Build adjacency in R using exact coordinate match
build_adjacency <- function(cats) {
  n <- nrow(cats); pairs <- list()
  for (i in seq_len(n-1)) {
    for (j in (i+1):n) {
      rA <- st_coordinates(cats[i,])[,c("X","Y")]
      rB <- st_coordinates(cats[j,])[,c("X","Y")]
      idxA <- integer(0); idxB <- integer(0)
      for (a in seq_len(nrow(rA)))
        for (b in seq_len(nrow(rB)))
          if (rA[a,1]==rB[b,1] && rA[a,2]==rB[b,2]) {
            idxA <- c(idxA, a-1L); idxB <- c(idxB, b-1L)
          }
      if (!length(idxA)) next
      pairs[[length(pairs)+1]] <- list(
        fidA=cats$facility_id[i], fidB=cats$facility_id[j],
        idxA=idxA, idxB=idxB)
    }
  }
  pairs
}

to_geojson <- function(sf_obj) {
  sf_obj$color <- sapply(sf_obj$type, get_col)
  tmp <- tempfile(fileext=".geojson")
  st_write(sf_obj, tmp, delete_dsn=TRUE, quiet=TRUE)
  paste(readLines(tmp, warn=FALSE), collapse="\n")
}

adj   <- build_adjacency(catchments)
GJ    <- to_geojson(catchments)
PAIRS <- toJSON(adj, auto_unbox=TRUE)

# Bounding box of data for canvas projection
bb      <- st_bbox(catchments)
GEO_X1  <- as.numeric(bb["xmin"])
GEO_Y1  <- as.numeric(bb["ymin"])
GEO_X2  <- as.numeric(bb["xmax"])
GEO_Y2  <- as.numeric(bb["ymax"])

ui <- fluidPage(
  tags$head(tags$style(HTML("
    html,body{margin:0;padding:0;height:100%;background:#f0f2f5;
              font-family:'Helvetica Neue',Arial,sans-serif;}
    #wrap{display:flex;height:100vh;}
    #canvas-col{flex:1;display:flex;flex-direction:column;
                align-items:center;justify-content:center;padding:16px;}
    #catchCanvas{background:white;border-radius:6px;
                 box-shadow:0 2px 12px rgba(0,0,0,0.15);
                 cursor:default;}
    #panel{width:280px;flex-shrink:0;background:#fff;
           border-left:1px solid #ddd;padding:16px;
           overflow-y:auto;font-size:13px;}
    h3{margin:0 0 4px;font-size:15px;color:#1a2b3c;}
    .sub{color:#999;font-size:11px;}
    hr{border:none;border-top:1px solid #eee;margin:12px 0;}
    .instr{background:#f0f4ff;border:1px solid #c5d0f0;border-radius:5px;
           padding:10px;font-size:12px;line-height:1.7;margin:10px 0;}
    .btn{display:block;width:100%;padding:9px;border:none;border-radius:4px;
         font-size:13px;font-weight:600;cursor:pointer;color:white;margin-bottom:6px;}
    .btn-save{background:#1a2b3c;}.btn-undo{background:#2980b9;}
    .btn-reset{background:#e74c3c;}
    #msg{font-size:12px;color:#555;min-height:20px;margin:8px 0;
         padding:6px;background:#f8f8f8;border-radius:4px;}
    .sigma-row{display:flex;align-items:center;gap:8px;font-size:12px;margin:8px 0;}
    .sigma-row input{flex:1;accent-color:#e67e22;}
    .sigma-val{color:#e67e22;font-weight:700;min-width:28px;}
    .fcard{border-left:4px solid #ccc;padding:5px 8px;margin-bottom:5px;font-size:12px;}
    .fname{font-weight:600;color:#1a2b3c;}.ftype{color:#888;font-size:11px;}
    .tile-credit{font-size:9px;color:#aaa;margin-top:4px;text-align:right;width:100%;}
  "))),
  
  # Leaflet just for background tiles — drawn onto canvas via drawImage
  tags$link(rel="stylesheet",
            href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"),
  tags$script(src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"),
  
  div(id="wrap",
      div(id="canvas-col",
          tags$canvas(id="catchCanvas"),
          div(class="tile-credit","© OpenStreetMap contributors")
      ),
      div(id="panel",
          tags$h3("Catchment Editor"),
          tags$p(class="sub","Mogadishu District"),
          hr(),
          div(class="instr",
              tags$b("Drag any border:"), br(),
              "Move near a border — it glows orange.", br(),
              "Click and drag sideways to reshape.", br(),
              "Nearby vertices follow with smooth falloff."
          ),
          div(class="sigma-row",
              tags$span("Spread:"),
              tags$input(id="sigma-slider",type="range",
                         min="20",max="300",value="90",step="5"),
              tags$span(class="sigma-val",id="sigma-val","90")
          ),
          tags$button("💾  Save",          class="btn btn-save",  id="save-btn"),
          tags$button("↩  Undo",           class="btn btn-undo",  id="undo-btn"),
          tags$button("↺  Reset Original", class="btn btn-reset", id="reset-btn"),
          div(id="msg","Hover near a border to begin."),
          hr(),
          tags$b("Facilities"),br(),br(),
          tagList(lapply(seq_len(nrow(catchments)), function(i) {
            col <- get_col(catchments$type[i])
            div(class="fcard",style=paste0("border-left-color:",col),
                div(class="fname",catchments$name[i]),
                div(class="ftype", catchments$type[i])
            )
          }))
      )
  ),
  
  tags$script(HTML(paste0('
var ORIG_GJ  = ', GJ, ';
var ADJ      = ', PAIRS, ';
var GEO      = {x1:', GEO_X1, ',y1:', GEO_Y1, ',x2:', GEO_X2, ',y2:', GEO_Y2, '};
var sigma    = 90;
var PAD      = 40;

var canvas, ctx, W, H;
var current, history = [];
var dragging=false, activePair=null;
var dragAnchorX, anchorPY;
var origCoordsA, origCoordsB;
var hoverPair=null, mouseX=0, mouseY=0;

function clone(o){ return JSON.parse(JSON.stringify(o)); }
function byFid(gj){
  var m={};
  gj.features.forEach(function(f){ m[f.properties.facility_id]=f; });
  return m;
}

// ── geo <-> canvas projection ─────────────────────────────────────────
// Simple linear map: geo bbox -> canvas with padding
function geoToCanvas(lng,lat){
  var gw=GEO.x2-GEO.x1, gh=GEO.y2-GEO.y1;
  var cw=W-2*PAD,        ch=H-2*PAD;
  return {
    x: PAD + (lng-GEO.x1)/gw * cw,
    y: PAD + (1-(lat-GEO.y1)/gh) * ch   // flip Y
  };
}
function canvasToGeo(cx,cy){
  var gw=GEO.x2-GEO.x1, gh=GEO.y2-GEO.y1;
  var cw=W-2*PAD,        ch=H-2*PAD;
  return {
    lng: GEO.x1 + (cx-PAD)/cw * gw,
    lat: GEO.y1 + (1-(cy-PAD)/ch) * gh
  };
}

// ── startup ───────────────────────────────────────────────────────────
document.addEventListener("DOMContentLoaded", function(){
  canvas = document.getElementById("catchCanvas");
  ctx    = canvas.getContext("2d");
  current = clone(ORIG_GJ);

  function resize(){
    var col = document.getElementById("canvas-col");
    var maxW = col.clientWidth  - 32;
    var maxH = col.clientHeight - 32;
    // maintain aspect ratio of geo bbox
    var gw=GEO.x2-GEO.x1, gh=GEO.y2-GEO.y1;
    var aspect = gw/gh;
    var w=maxW, h=w/aspect;
    if(h>maxH){ h=maxH; w=h*aspect; }
    W=canvas.width =Math.floor(w);
    H=canvas.height=Math.floor(h);
    draw();
  }
  resize();
  window.addEventListener("resize", resize);

  canvas.addEventListener("mousemove", onMouseMove);
  canvas.addEventListener("mousedown", onMouseDown);
  document.addEventListener("mouseup",  onMouseUp);
  canvas.addEventListener("mouseleave", function(){
    if(!dragging){ hoverPair=null; draw(); }
  });

  document.getElementById("sigma-slider").addEventListener("input",function(){
    sigma=+this.value;
    document.getElementById("sigma-val").innerText=sigma;
    draw();
  });
});

// ── geometry helpers ──────────────────────────────────────────────────
function getRingPx(feat){
  return feat.geometry.coordinates[0].map(function(c){
    return geoToCanvas(c[0],c[1]);
  });
}

function getBorderPts(pair){
  var fi=byFid(current);
  var ring=fi[pair.fidA].geometry.coordinates[0];
  var pts=pair.idxA.map(function(k){ return geoToCanvas(ring[k][0],ring[k][1]); });
  pts.sort(function(a,b){ return a.y-b.y; });
  return pts;
}

function distSeg(px,py,ax,ay,bx,by){
  var dx=bx-ax,dy=by-ay,len2=dx*dx+dy*dy;
  if(len2<1e-10) return Math.hypot(px-ax,py-ay);
  var t=Math.max(0,Math.min(1,((px-ax)*dx+(py-ay)*dy)/len2));
  return Math.hypot(px-ax-t*dx,py-ay-t*dy);
}

function nearestPair(mx,my,thresh){
  var best=null,bd=thresh;
  ADJ.forEach(function(pair){
    var pts=getBorderPts(pair);
    for(var k=0;k<pts.length-1;k++){
      var d=distSeg(mx,my,pts[k].x,pts[k].y,pts[k+1].x,pts[k+1].y);
      if(d<bd){bd=d;best=pair;}
    }
  });
  return best;
}

// ── draw ──────────────────────────────────────────────────────────────
function draw(){
  ctx.clearRect(0,0,W,H);

  var fi=byFid(current);

  // filled polygons
  current.features.forEach(function(f){
    var pts=getRingPx(f);
    var col=f.properties.color||"#888";
    ctx.beginPath();
    ctx.moveTo(pts[0].x,pts[0].y);
    for(var k=1;k<pts.length;k++) ctx.lineTo(pts[k].x,pts[k].y);
    ctx.closePath();
    ctx.fillStyle=col+"33";   // 20% opacity
    ctx.fill();
  });

  // border lines
  ADJ.forEach(function(pair){
    var pts=getBorderPts(pair);
    if(pts.length<2) return;
    var isHot=(hoverPair&&hoverPair===pair)||activePair===pair;
    var anchorY=dragging&&activePair===pair ? anchorPY
               : isHot ? mouseY : null;

    // glow ribbon behind line when hot
    if(isHot && anchorY!==null){
      for(var k=0;k<pts.length-1;k++){
        var wA=Math.exp(-((pts[k].y  -anchorY)*(pts[k].y  -anchorY))/(2*sigma*sigma));
        var wB=Math.exp(-((pts[k+1].y-anchorY)*(pts[k+1].y-anchorY))/(2*sigma*sigma));
        var wMax=Math.max(wA,wB);
        if(wMax<0.02) continue;
        ctx.beginPath();
        ctx.moveTo(pts[k].x,pts[k].y);
        ctx.lineTo(pts[k+1].x,pts[k+1].y);
        ctx.strokeStyle="rgba(241,196,15,"+(wMax*0.25).toFixed(2)+")";
        ctx.lineWidth=16*wMax;
        ctx.lineCap="round";
        ctx.stroke();
      }
    }

    // main border line
    ctx.beginPath();
    ctx.moveTo(pts[0].x,pts[0].y);
    for(var k=1;k<pts.length;k++) ctx.lineTo(pts[k].x,pts[k].y);
    ctx.strokeStyle = isHot?"#e67e22":"rgba(60,60,60,0.7)";
    ctx.lineWidth   = isHot?3:1.5;
    ctx.lineJoin    = "round"; ctx.lineCap="round";
    ctx.shadowColor = isHot?"#e67e22":"transparent";
    ctx.shadowBlur  = isHot?8:0;
    ctx.stroke();
    ctx.shadowBlur  = 0;

    // vertex dots sized by weight
    if(isHot && anchorY!==null){
      pts.forEach(function(p){
        var w=Math.exp(-((p.y-anchorY)*(p.y-anchorY))/(2*sigma*sigma));
        if(w<0.02) return;
        ctx.beginPath();
        ctx.arc(p.x,p.y,3+w*7,0,Math.PI*2);
        ctx.fillStyle="rgba(241,196,15,"+(w*0.9).toFixed(2)+")";
        ctx.shadowColor="#f1c40f"; ctx.shadowBlur=w*14;
        ctx.fill(); ctx.shadowBlur=0;
      });
    }
  });

  // polygon outlines (on top)
  current.features.forEach(function(f){
    var pts=getRingPx(f);
    var col=f.properties.color||"#888";
    ctx.beginPath();
    ctx.moveTo(pts[0].x,pts[0].y);
    for(var k=1;k<pts.length;k++) ctx.lineTo(pts[k].x,pts[k].y);
    ctx.closePath();
    ctx.strokeStyle=col;
    ctx.lineWidth=1;
    ctx.stroke();
  });

  // facility dots
  current.features.forEach(function(f){
    var col=f.properties.color||"#888";
    var p=geoToCanvas(f.properties.facility_lng||f.properties.lng,
                      f.properties.facility_lat||f.properties.lat);
    if(!p||isNaN(p.x)) return;
    ctx.beginPath();
    ctx.arc(p.x,p.y,5,0,Math.PI*2);
    ctx.fillStyle=col;
    ctx.strokeStyle="white"; ctx.lineWidth=1.5;
    ctx.fill(); ctx.stroke();
  });
}

// ── mouse ─────────────────────────────────────────────────────────────
function getXY(e){
  var r=canvas.getBoundingClientRect();
  return {
    x:(e.clientX-r.left)*(W/r.width),
    y:(e.clientY-r.top) *(H/r.height)
  };
}

function onMouseMove(e){
  var pos=getXY(e);
  mouseX=pos.x; mouseY=pos.y;

  if(dragging){
    var totalDX=pos.x-dragAnchorX;
    var fi=byFid(current);
    var ringA=fi[activePair.fidA].geometry.coordinates[0];
    var ringB=fi[activePair.fidB].geometry.coordinates[0];

    activePair.idxA.forEach(function(k){
      var op=geoToCanvas(origCoordsA[k][0],origCoordsA[k][1]);
      var dy=op.y-anchorPY;
      var w=Math.exp(-(dy*dy)/(2*sigma*sigma));
      var np=canvasToGeo(op.x+w*totalDX, op.y);
      ringA[k][0]=np.lng; ringA[k][1]=np.lat;
      if(k===0){ringA[ringA.length-1][0]=ringA[0][0];
                ringA[ringA.length-1][1]=ringA[0][1];}
    });
    activePair.idxB.forEach(function(k){
      var op=geoToCanvas(origCoordsB[k][0],origCoordsB[k][1]);
      var dy=op.y-anchorPY;
      var w=Math.exp(-(dy*dy)/(2*sigma*sigma));
      var np=canvasToGeo(op.x+w*totalDX, op.y);
      ringB[k][0]=np.lng; ringB[k][1]=np.lat;
      if(k===0){ringB[ringB.length-1][0]=ringB[0][0];
                ringB[ringB.length-1][1]=ringB[0][1];}
    });
    draw();
    return;
  }

  var near=nearestPair(pos.x,pos.y,14);
  if(near!==hoverPair){ hoverPair=near; draw(); }
  canvas.style.cursor=near?"grab":"default";
}

function onMouseDown(e){
  var pos=getXY(e);
  var near=nearestPair(pos.x,pos.y,14);
  if(!near) return;

  dragging      = true;
  activePair    = near;
  hoverPair     = near;
  dragAnchorX   = pos.x;
  anchorPY      = pos.y;
  canvas.style.cursor="grabbing";

  var fi=byFid(current);
  origCoordsA=clone(fi[near.fidA].geometry.coordinates[0]);
  origCoordsB=clone(fi[near.fidB].geometry.coordinates[0]);

  history.push(clone(current));
  if(history.length>30) history.shift();
  draw();
}

function onMouseUp(){
  if(!dragging) return;
  dragging=false; activePair=null;
  canvas.style.cursor=hoverPair?"grab":"default";
  draw();
  document.getElementById("msg").innerText=
    "Border updated. Click Save to keep changes.";
}

// ── buttons ───────────────────────────────────────────────────────────
document.addEventListener("click",function(e){
  if(e.target.id==="undo-btn"){
    if(!history.length){
      document.getElementById("msg").innerText="Nothing to undo.";return;
    }
    current=history.pop(); draw();
    document.getElementById("msg").innerText="Undone.";
  }
  if(e.target.id==="save-btn"){
    Shiny.setInputValue("save_geojson",
      JSON.stringify(current),{priority:"event"});
    document.getElementById("msg").innerText="Saving...";
  }
  if(e.target.id==="reset-btn"){
    history=[]; current=clone(ORIG_GJ); draw();
    document.getElementById("msg").innerText="Reset.";
    Shiny.setInputValue("do_reset",Date.now(),{priority:"event"});
  }
});
')))
)

server <- function(input, output, session) {
  session$onFlushed(function(){
    shiny::insertUI("body","afterBegin",tags$script(HTML("
      Shiny.addCustomMessageHandler('saveResult',function(msg){
        document.getElementById('msg').innerText=msg;
      });
    ")),immediate=TRUE)
  },once=TRUE)
  
  observeEvent(input$save_geojson,{
    tryCatch({
      tmp <- tempfile(fileext=".geojson")
      writeLines(input$save_geojson,tmp)
      updated <- st_read(tmp,quiet=TRUE)
      saveRDS(updated,"data/catchments_edited.Rds")
      session$sendCustomMessage("saveResult","💾 Saved.")
    },error=function(e){
      session$sendCustomMessage("saveResult",paste("Error:",e$message))
    })
  })
  observeEvent(input$do_reset,{
    if(file.exists("data/catchments_edited.Rds"))
      file.remove("data/catchments_edited.Rds")
  })
}

shinyApp(ui, server)