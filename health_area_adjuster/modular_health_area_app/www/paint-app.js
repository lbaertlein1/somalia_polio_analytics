(function() {
  window.paintApps = window.paintApps || {};

  // =============================================================================
// Traces real painted-cell data into the SAME raw shape the prototype's own
// traceAreaBoundaries() produces (rawIdRingsByArea + vertexPos), so the
// prototype's already-tested simplifyVertexBoundary() (extracted verbatim
// below, unchanged) can run on it without any new logic duplicating what it
// already does correctly: edge classification, outer-boundary detection,
// max-spacing reduction, junction preservation.
//
// Vertex identity uses "${row}_${col}", exactly matching the prototype's own
// scheme -- real cells sit on a genuine regular grid (st_make_grid, never
// individually clipped; only filtered by a centroid-inside test), so this is
// not an approximation, it's the same construction with a different (and
// possibly irregular-shaped) valid-cell set.
// =============================================================================

function traceRealGridBoundaries(cells, cellCorners, assignments) {
  const vertexPos = {};
  const cellIds = Object.keys(cells);

  function cornerIds(row, col) {
    return {
      sw: row + '_' + col,
      se: row + '_' + (col + 1),
      ne: (row + 1) + '_' + (col + 1),
      nw: (row + 1) + '_' + col
    };
  }

  // Classifies each of a cell's 4 raw corners as SW/SE/NE/NW by comparing
  // to the cell's own centroid, rather than assuming any fixed starting
  // point or winding direction in the source ring. st_make_grid()'s
  // convention isn't verified here against live R output, so this makes
  // correctness independent of that assumption entirely.
  function classifyCorners(corners) {
    const cLat = corners.reduce((s, p) => s + p[0], 0) / 4;
    const cLng = corners.reduce((s, p) => s + p[1], 0) / 4;
    const out = {};
    corners.forEach(p => {
      const key = (p[0] < cLat ? 's' : 'n') + (p[1] < cLng ? 'w' : 'e');
      out[key] = p;
    });
    return out;
  }

  cellIds.forEach(cid => {
    const { row, col } = cells[cid];
    const ids = cornerIds(row, col);
    const classified = classifyCorners(cellCorners[cid]);
    ['sw', 'se', 'ne', 'nw'].forEach(key => {
      if (!vertexPos[ids[key]] && classified[key]) {
        vertexPos[ids[key]] = { lat: classified[key][0], lng: classified[key][1] };
      }
    });
  });

  const cellAtRC = {};
  cellIds.forEach(cid => { cellAtRC[cells[cid].row + '_' + cells[cid].col] = cid; });

  const areaEdges = {};
  function addEdge(area, v1, v2) {
    if (!areaEdges[area]) areaEdges[area] = [];
    areaEdges[area].push([v1, v2]);
  }

  cellIds.forEach(cid => {
    const { row, col } = cells[cid];
    const area = assignments[cid];
    if (!area) return;
    const ids = cornerIds(row, col);
    const sides = [
      { v1: ids.sw, v2: ids.se, nbr: (row - 1) + '_' + col },
      { v1: ids.se, v2: ids.ne, nbr: row + '_' + (col + 1) },
      { v1: ids.ne, v2: ids.nw, nbr: (row + 1) + '_' + col },
      { v1: ids.nw, v2: ids.sw, nbr: row + '_' + (col - 1) }
    ];
    sides.forEach(s => {
      const nbrCid = cellAtRC[s.nbr];
      const nbrArea = nbrCid ? assignments[nbrCid] : null;
      if (nbrArea !== area) addEdge(area, s.v1, s.v2);
    });
  });

  // Chain each area's edges into closed ring(s) -- same reconstruction
  // approach as the outer-ring chainer below, applied per area.
  const rawIdRingsByArea = {};
  Object.keys(areaEdges).forEach(area => {
    const edges = areaEdges[area];
    const outMap = {};
    edges.forEach(([v1, v2]) => { outMap[v1] = v2; });
    const rings = [];
    const used = new Set();
    edges.forEach(([startV]) => {
      if (used.has(startV)) return;
      const ring = [startV];
      used.add(startV);
      let current = outMap[startV];
      while (current !== startV) {
        if (current === undefined || used.has(current)) break;
        ring.push(current);
        used.add(current);
        current = outMap[current];
      }
      ring.push(startV);
      rings.push(ring);
    });
    rawIdRingsByArea[area] = rings;
  });

  return { rawIdRingsByArea, vertexPos };
}

// ============================================================================
// Extracted verbatim from the prototype's convertToVertexBoundary() (the
// portion after tracing, before rendering) -- unit-tested there across 160+
// (N, seed) combinations, multipart/hole geometry, and large-N performance.
// Only change: N and vertexPos are explicit parameters instead of reading
// a DOM element / closing over a module-level variable, and it returns
// { areaRings, outerVertexIds } instead of assigning to globals and
// triggering a redraw.
// ============================================================================
function simplifyVertexBoundary(rawIdRingsByArea, vertexPos, N) {
  // Flat list of all rings, each tagged with its owning area — used
  // throughout instead of assuming one ring per area.
  const allRingsFlat = []; // { areaId, ring: [vertexId,...] }
  Object.keys(rawIdRingsByArea).forEach(a => {
    rawIdRingsByArea[a].forEach(ring => allRingsFlat.push({ areaId: a, ring }));
  });

  // 3. Classify every edge (consecutive vertex pair) by how many DISTINCT
  //    areas reference it: 1 = outer boundary (district edge for health
  //    areas, health-area edge for team areas) — must stay fixed. 2 =
  //    interior shared edge between two areas — editable. This also lets
  //    us classify vertices: a vertex is LOCKED if any edge touching it
  //    is an outer edge (moving it would distort the outer boundary,
  //    even though it also happens to be an endpoint of an interior edge).
  const edgeAreas = {}; // "vA|vB" (sorted) -> Set of areaIds
  allRingsFlat.forEach(({ areaId, ring }) => {
    for (let i = 0; i < ring.length - 1; i++) {
      const key = [ring[i], ring[i + 1]].sort().join('|');
      (edgeAreas[key] = edgeAreas[key] || new Set()).add(areaId);
    }
  });

  outerVertexIds = new Set();
  Object.keys(edgeAreas).forEach(key => {
    if (edgeAreas[key].size <= 1) {
      key.split('|').forEach(vid => outerVertexIds.add(vid));
    }
  });

  // Vertices where 3+ areas meet — always kept regardless of N, so
  // simplifying one area's edge can never orphan its neighbors' matching
  // edge at a true junction point.
  const vertexAreaCount = {};
  Object.keys(rawIdRingsByArea).forEach(a => {
    const seen = new Set();
    rawIdRingsByArea[a].forEach(ring => ring.forEach(vid => seen.add(vid)));
    seen.forEach(vid => { vertexAreaCount[vid] = (vertexAreaCount[vid] || 0) + 1; });
  });
  function isJunction(vid) {
    return (vertexAreaCount[vid] || 0) >= 3;
  }

  // 4. Keep decision is GLOBAL and per-vertex-id (not per-ring-position) —
  //    this is what guarantees a shared edge simplifies to the SAME
  //    subset of vertices for both areas that border it. Outer-boundary
  //    vertices are ALWAYS kept (never simplified away) since the outer
  //    boundary must stay exactly as drawn.
  let globalIdx = 0;
  const globalOrder = {};
  allRingsFlat.forEach(({ ring }) => {
    ring.forEach(vid => { if (globalOrder[vid] === undefined) globalOrder[vid] = globalIdx++; });
  });

  // Max-spacing force-keeps, computed as a GLOBAL, direction-independent
  // property rather than a live per-ring "distance since last kept"
  // accumulator. That per-ring approach was a real bug: two areas
  // sharing a boundary walk that same physical edge in OPPOSITE
  // directions (mirror images), so a live accumulator can force-keep
  // different vertices on each side of what must be an identical shared
  // boundary — silently breaking the "shared vertex" guarantee every
  // other part of this file depends on (merge, snap, and now absorption
  // all assume a shared edge is the exact same vertex sequence from both
  // sides). Fixed by computing it once as a graph-distance property:
  // multi-source BFS from every already-forced vertex (outer/junction/
  // every-Nth) over the RAW undirected adjacency, iteratively adding any
  // vertex whose distance reaches the cap, until stable. Graph distance
  // to a set of fixed points doesn't care which direction anyone walks
  // it in, so both mirrored copies of a shared edge get identical
  // results.
  const rawAdjacency = {};
  allRingsFlat.forEach(({ ring }) => {
    for (let i = 0; i < ring.length - 1; i++) {
      const a = ring[i], b = ring[i + 1];
      (rawAdjacency[a] = rawAdjacency[a] || new Set()).add(b);
      (rawAdjacency[b] = rawAdjacency[b] || new Set()).add(a);
    }
  });
  function baseKeepVertex(vid) {
    return outerVertexIds.has(vid) || isJunction(vid) || (globalOrder[vid] % N === 0);
  }
  const spacingForced = new Set();
  Object.keys(rawAdjacency).forEach(vid => { if (baseKeepVertex(vid)) spacingForced.add(vid); });
  let spacingChanged = true;
  while (spacingChanged) {
    spacingChanged = false;
    const dist = {};
    let queue = [];
    spacingForced.forEach(v => { dist[v] = 0; queue.push(v); });
    let qi = 0;
    while (qi < queue.length) {
      const cur = queue[qi++];
      const d = dist[cur];
      if (d >= N) continue;
      rawAdjacency[cur].forEach(n => {
        if (dist[n] === undefined) { dist[n] = d + 1; queue.push(n); }
      });
    }
    Object.keys(dist).forEach(v => {
      if (dist[v] >= N && !spacingForced.has(v)) { spacingForced.add(v); spacingChanged = true; }
    });
  }
  function keepVertex(vid) {
    return baseKeepVertex(vid) || spacingForced.has(vid);
  }

  // 5. Build simplified rings by walking each raw ring (every part of
  //    every area) and keeping only "keep" vertices, in order.
  areaRings = {}; // areaId -> array of parts, each part = [vertexId,...] closed
  function dedupeConsecutive(r) {
    // Collapses any immediately-repeated vertex (e.g. a self-touching
    // pinch point in a highly irregular boundary passing through the
    // same corner twice) into one. Always safe — a zero-length segment
    // contributes nothing to the shape either way.
    const out = [];
    r.forEach(v => { if (out.length === 0 || out[out.length - 1] !== v) out.push(v); });
    return out;
  }
  function ensureClosed(r) {
    if (r.length > 0 && r[r.length - 1] !== r[0]) r.push(r[0]);
    return r;
  }
  Object.keys(rawIdRingsByArea).forEach(a => {
    areaRings[a] = rawIdRingsByArea[a].map(ring => {
      const kept = [];
      for (let i = 0; i < ring.length - 1; i++) {
        if (keepVertex(ring[i])) kept.push(ring[i]);
      }
      if (kept.length < 3) {
        // fallback: too aggressive a simplification for this tiny ring —
        // keep every original vertex instead of collapsing it away.
        return ensureClosed(dedupeConsecutive(ring.slice(0, -1)));
      }
      return ensureClosed(dedupeConsecutive(kept));
    });
  });

  return { areaRings, outerVertexIds };
}

// Chains outer-boundary edges (edgeOwners size === 1) into ordered ring(s),
// usable for point-in-polygon testing.
function buildOuterRingsFromModel(areaRings, outerVertexIds) {
  const outMap = {};
  Object.keys(areaRings).forEach(area => {
    areaRings[area].forEach(ring => {
      for (let i = 0; i < ring.length - 1; i++) {
        const v1 = ring[i], v2 = ring[i + 1];
        if (outerVertexIds.has(v1) && outerVertexIds.has(v2)) {
          outMap[v1] = v2;
        }
      }
    });
  });

  const rings = [];
  const used = new Set();
  Object.keys(outMap).forEach(startV => {
    if (used.has(startV)) return;
    const ring = [startV];
    used.add(startV);
    let current = outMap[startV];
    while (current !== startV) {
      if (current === undefined || used.has(current)) break;
      ring.push(current);
      used.add(current);
      current = outMap[current];
    }
    ring.push(startV);
    rings.push(ring);
  });
  return rings;
}

function pointInOuterRings(pt, outerRings, vertexPos) {
  return outerRings.some(ring => {
    const poly = ring.map(vid => vertexPos[vid]);
    let inside = false;
    for (let i = 0, j = poly.length - 2; i < poly.length - 1; j = i++) {
      const xi = poly[i].lng, yi = poly[i].lat;
      const xj = poly[j].lng, yj = poly[j].lat;
      const intersect = ((yi > pt.lat) !== (yj > pt.lat)) &&
        (pt.lng < (xj - xi) * (pt.lat - yi) / (yj - yi) + xi);
      if (intersect) inside = !inside;
    }
    return inside;
  });
}


// =============================================================================
// Reconstructs correct GeoJSON (Polygon-with-holes, or MultiPolygon for
// genuinely disconnected pieces) from the flat per-area ring lists produced
// by traceRealGridBoundaries + simplifyVertexBoundary.
//
// Winding convention, verified by construction (not assumed): each area's
// edges are added by walking each of its cells' own sides in a fixed
// SW->SE->NE->NW->SW order. For a merged EXTERIOR boundary this traces
// counter-clockwise; for a HOLE (the boundary facing a different area's
// territory enclosed within), the same construction traces clockwise --
// confirmed directly against a hand-built island case below. This happens
// to match RFC 7946's own convention (exterior CCW, holes CW), though nothing
// here depends on that -- classification uses the SIGN, not an assumption
// about what any consumer expects.
// =============================================================================

function signedRingArea(ring, vertexPos) {
  let sum = 0;
  for (let i = 0; i < ring.length - 1; i++) {
    const p1 = vertexPos[ring[i]], p2 = vertexPos[ring[i + 1]];
    sum += p1.lng * p2.lat - p2.lng * p1.lat;
  }
  return sum / 2;
}

function ringToLngLat(ring, vertexPos) {
  return ring.map(vid => [vertexPos[vid].lng, vertexPos[vid].lat]);
}

// Point-in-polygon (ray casting) against a ring given as vertex ids.
function pointInRing(pt, ring, vertexPos) {
  const poly = ring.map(vid => vertexPos[vid]);
  let inside = false;
  for (let i = 0, j = poly.length - 2; i < poly.length - 1; j = i++) {
    const xi = poly[i].lng, yi = poly[i].lat;
    const xj = poly[j].lng, yj = poly[j].lat;
    const intersect = ((yi > pt.lat) !== (yj > pt.lat)) &&
      (pt.lng < (xj - xi) * (pt.lat - yi) / (yj - yi) + xi);
    if (intersect) inside = !inside;
  }
  return inside;
}

// Builds a GeoJSON FeatureCollection from areaRings. One Feature per area;
// geometry is Polygon if the area has exactly one exterior ring, MultiPolygon
// if it has more than one (genuinely disconnected pieces) -- each exterior
// paired with whichever holes fall inside it.
// Builds a point -> areaName membership tester from the current vertex
// boundary -- used when leaving vertex mode to rasterize the refined
// boundary back into cell assignments ("Back to Painting" continues from
// the refined shape, not the pre-refinement one). Correctly excludes holes
// and handles genuinely multipart areas, reusing the same exterior/hole
// classification as buildAreaGeojson (same winding convention, verified
// there against real traced data).
function buildAreaMembershipTester(areaRings, vertexPos) {
  const areaData = {};
  Object.keys(areaRings).forEach(area => {
    const exteriors = [], holes = [];
    areaRings[area].forEach(ring => {
      const signed = signedRingArea(ring, vertexPos);
      if (signed > 0) exteriors.push(ring); else holes.push(ring);
    });
    const holesByExteriorIdx = exteriors.map(() => []);
    holes.forEach(hole => {
      const testPt = vertexPos[hole[0]];
      for (let i = 0; i < exteriors.length; i++) {
        if (pointInRing(testPt, exteriors[i], vertexPos)) { holesByExteriorIdx[i].push(hole); break; }
      }
    });
    areaData[area] = { exteriors, holesByExteriorIdx };
  });

  return function(pt) {
    for (const area of Object.keys(areaData)) {
      const { exteriors, holesByExteriorIdx } = areaData[area];
      for (let i = 0; i < exteriors.length; i++) {
        if (!pointInRing(pt, exteriors[i], vertexPos)) continue;
        const inHole = holesByExteriorIdx[i].some(h => pointInRing(pt, h, vertexPos));
        if (!inHole) return area;
      }
    }
    return null; // no area claims this point -- caller should keep the cell's existing assignment
  };
}

function buildAreaGeojson(areaRings, vertexPos, areaNamePropertyKey) {
  areaNamePropertyKey = areaNamePropertyKey || 'dfa_name';
  const features = [];

  Object.keys(areaRings).forEach(area => {
    const rings = areaRings[area];
    const exteriors = [], holes = [];
    rings.forEach(ring => {
      const signed = signedRingArea(ring, vertexPos);
      if (signed > 0) exteriors.push(ring);
      else holes.push(ring);
    });

    if (exteriors.length === 0) return; // defensive -- shouldn't happen for a real area

    // Assign each hole to the exterior that contains it. A hole's own
    // vertices sit ON that exterior's boundary only in a degenerate case;
    // use the hole's own centroid-ish first vertex offset slightly inward
    // is unnecessary here -- any vertex of the hole ring is strictly
    // interior to its containing exterior (holes never touch the exterior
    // boundary in this topology), so a plain point-in-ring test on the
    // hole's first vertex against each candidate exterior is sufficient.
    const holesByExteriorIdx = exteriors.map(() => []);
    holes.forEach(hole => {
      const testPt = vertexPos[hole[0]];
      let assigned = false;
      for (let i = 0; i < exteriors.length; i++) {
        if (pointInRing(testPt, exteriors[i], vertexPos)) {
          holesByExteriorIdx[i].push(hole);
          assigned = true;
          break;
        }
      }
      if (!assigned) {
        // Shouldn't happen in a well-formed topology; keep the hole out
        // rather than silently drop the whole feature.
        console.warn('[buildAreaGeojson] hole ring for area', area, 'not contained by any exterior -- dropped');
      }
    });

    const polygons = exteriors.map((ext, i) => {
      const coords = [ringToLngLat(ext, vertexPos)];
      holesByExteriorIdx[i].forEach(h => coords.push(ringToLngLat(h, vertexPos)));
      return coords;
    });

    const geometry = polygons.length === 1
      ? { type: 'Polygon', coordinates: polygons[0] }
      : { type: 'MultiPolygon', coordinates: polygons };

    features.push({
      type: 'Feature',
      properties: { [areaNamePropertyKey]: area },
      geometry
    });
  });

  return { type: 'FeatureCollection', features };
}


// =============================================================================
// Snaps the traced outer boundary (currently the coarse grid-cell staircase
// approximation -- an artifact of make_paint_grid()'s centroid-inside cell
// filtering, not the district's actual shape) onto the TRUE district/health-
// area polygon boundary. Every outer vertex gets repositioned to its nearest
// point on the true boundary, preserving vertex IDs/topology/connectivity --
// only WHERE each one sits changes, not the graph structure built by
// traceRealGridBoundaries/simplifyVertexBoundary.
//
// This corrects the whole outer path, not just major (visible) vertices --
// snapping only the visible junction/corner markers while leaving the
// invisible in-between spacing vertices on the old staircase would produce
// a zigzagging line between "correct" points, which defeats the purpose.
// =============================================================================

// Extracts every ring (as arrays of {lat,lng}) from a GeoJSON district
// geometry, handling Polygon, MultiPolygon, and a FeatureCollection/Feature
// wrapper defensively -- administrative boundaries are usually a single
// Polygon exterior ring with no holes, but this doesn't assume that.
function extractBoundaryRings(districtGeojson) {
  const rings = [];
  function fromGeometry(geom) {
    if (!geom) return;
    if (geom.type === 'Polygon') {
      geom.coordinates.forEach(ring => rings.push(ring.map(([lng, lat]) => ({ lat, lng }))));
    } else if (geom.type === 'MultiPolygon') {
      geom.coordinates.forEach(poly => poly.forEach(ring => rings.push(ring.map(([lng, lat]) => ({ lat, lng })))));
    }
  }
  if (districtGeojson.type === 'FeatureCollection') {
    districtGeojson.features.forEach(f => fromGeometry(f.geometry));
  } else if (districtGeojson.type === 'Feature') {
    fromGeometry(districtGeojson.geometry);
  } else {
    fromGeometry(districtGeojson);
  }
  return rings;
}

// Closest point on a single line segment a-b to point p.
function closestPointOnSegmentRaw(p, a, b) {
  const dx = b.lng - a.lng, dy = b.lat - a.lat;
  const lenSq = dx * dx + dy * dy;
  if (lenSq === 0) return { lat: a.lat, lng: a.lng };
  let t = ((p.lng - a.lng) * dx + (p.lat - a.lat) * dy) / lenSq;
  t = Math.max(0, Math.min(1, t));
  return { lat: a.lat + t * dy, lng: a.lng + t * dx };
}

function distSq(a, b) {
  const dlat = a.lat - b.lat, dlng = a.lng - b.lng;
  return dlat * dlat + dlng * dlng;
}

// Finds the closest point to `p` on ANY segment of ANY ring in `rings`.
function closestPointOnRings(p, rings) {
  let best = null, bestDist = Infinity;
  rings.forEach(ring => {
    for (let i = 0; i < ring.length - 1; i++) {
      const candidate = closestPointOnSegmentRaw(p, ring[i], ring[i + 1]);
      const d = distSq(p, candidate);
      if (d < bestDist) { bestDist = d; best = candidate; }
    }
  });
  return best;
}

// Snaps every vertex in outerVertexIds onto the true boundary rings,
// mutating vertexPos in place (same objects, same references -- callers
// that already hold a reference to vertexPos see the correction immediately).
function snapOuterBoundaryToTruth(vertexPos, outerVertexIds, districtGeojson) {
  const rings = extractBoundaryRings(districtGeojson);
  if (rings.length === 0) return 0; // nothing to snap onto -- leave positions as traced
  let snapped = 0;
  outerVertexIds.forEach(vid => {
    const p = vertexPos[vid];
    if (!p) return;
    const projected = closestPointOnRings(p, rings);
    if (projected) {
      vertexPos[vid] = projected;
      snapped++;
    }
  });
  return snapped;
}


// =============================================================================
// Vertex-mode additions to paint-app.js's createPaintApp() object.
// Step 2 of the workflow: paint rough areas (existing engine, unchanged),
// then refine boundaries here. Same map instance, same object -- this is
// a dedicated boundary-refinement view, not an overlay on the paint canvas
// (every other layer is hidden while active).
// =============================================================================

const VERTEX_ENGINE_SRC = "function distDeg(a, b) {\n  return Math.hypot(a.lat - b.lat, a.lng - b.lng);\n}\n\nfunction closestPointOnSegment(p, a, b) {\n  const dx = b.lng - a.lng, dy = b.lat - a.lat;\n  const lenSq = dx * dx + dy * dy;\n  if (lenSq === 0) return { lat: a.lat, lng: a.lng };\n  let t = ((p.lng - a.lng) * dx + (p.lat - a.lat) * dy) / lenSq;\n  t = Math.max(0, Math.min(1, t));\n  return { lat: a.lat + t * dy, lng: a.lng + t * dx };\n}\n\nfunction _orientation(p, q, r) {\n  const val = (q.lng - p.lng) * (r.lat - p.lat) - (q.lat - p.lat) * (r.lng - p.lng);\n  if (Math.abs(val) < 1e-12) return 0;\n  return val > 0 ? 1 : 2;\n}\nfunction _onSegment(p, q, r) {\n  return q.lng <= Math.max(p.lng, r.lng) + 1e-12 && q.lng >= Math.min(p.lng, r.lng) - 1e-12 &&\n         q.lat <= Math.max(p.lat, r.lat) + 1e-12 && q.lat >= Math.min(p.lat, r.lat) - 1e-12;\n}\n// True if segments p1-p2 and p3-p4 cross or touch. NOTE: returns true for\n// a shared endpoint too \u2014 callers must exclude edges adjacent to the\n// vertex being tested themselves, or every ordinary drag would falsely\n// \"overlap\" with its own neighboring edges.\nfunction segmentsIntersect(p1, p2, p3, p4) {\n  const o1 = _orientation(p1, p2, p3), o2 = _orientation(p1, p2, p4);\n  const o3 = _orientation(p3, p4, p1), o4 = _orientation(p3, p4, p2);\n  if (o1 !== o2 && o3 !== o4) return true;\n  if (o1 === 0 && _onSegment(p1, p3, p2)) return true;\n  if (o2 === 0 && _onSegment(p1, p4, p2)) return true;\n  if (o3 === 0 && _onSegment(p3, p1, p4)) return true;\n  if (o4 === 0 && _onSegment(p3, p2, p4)) return true;\n  return false;\n}\n\n\nfunction getNeighborsOf(vid) {\n  const neighbors = new Set();\n  Object.values(areaRings).forEach(parts => parts.forEach(ring => {\n    for (let i = 0; i < ring.length - 1; i++) {\n      if (ring[i] !== vid) continue;\n      const nextIdx = (i + 1) % (ring.length - 1);\n      const prevIdx = (i - 1 + ring.length - 1) % (ring.length - 1);\n      neighbors.add(ring[nextIdx]);\n      neighbors.add(ring[prevIdx]);\n    }\n  }));\n  neighbors.delete(vid);\n  return [...neighbors];\n}\n\n// Live overlap PREVENTION (not post-hoc removal) lives as\n// wouldOverlapMulti() further down, alongside the drag interaction that\n// uses it \u2014 a single-vertex version isn't needed since falloff-driven\n// drags always move a SET of vertices at once, even when that set has\n// only one member (a vertex far from everything else).\n\n\n\nfunction countAreasAtVertex(vid) {\n  return Object.keys(areaRings).filter(a => areaRings[a].some(ring => ring.includes(vid))).length;\n}\n\n// Starts a constrained junction drag directly from its marker \u2014 the\n// same activeDrag shape (weights={[vid]:1}, junctionSegment set) that\n// grabbing a junction via a nearby line already produces, so the shared\n// mousemove/mouseup handlers need no changes to support this entry point.\n\nfunction mergeVertexInto(vid, target) {\n  Object.keys(areaRings).forEach(a => {\n    areaRings[a] = areaRings[a].map(ring => {\n      const replaced = ring.map(v => v === vid ? target : v);\n      const deduped = [];\n      replaced.forEach(v => { if (deduped.length === 0 || deduped[deduped.length - 1] !== v) deduped.push(v); });\n      if (deduped.length > 0 && deduped[deduped.length - 1] !== deduped[0]) deduped.push(deduped[0]);\n      return deduped;\n    });\n  });\n  delete vertexPos[vid];\n}\n\n// Inserts a brand-new vertex at `pos` into every ring that has the exact\n// consecutive pair (v1,v2) \u2014 the shared-edge case \u2014 and returns its id.\nfunction splitEdgeAt(v1, v2, pos) {\n  const newId = 'v' + (Object.keys(vertexPos).length + Math.floor(Math.random() * 1000000));\n  vertexPos[newId] = { lat: pos.lat, lng: pos.lng };\n  Object.keys(areaRings).forEach(a => {\n    areaRings[a].forEach(ring => {\n      for (let i = 0; i < ring.length - 1; i++) {\n        if ((ring[i] === v1 && ring[i + 1] === v2) || (ring[i] === v2 && ring[i + 1] === v1)) {\n          ring.splice(i + 1, 0, newId);\n          return;\n        }\n      }\n    });\n  });\n  return newId;\n}\n\n// =============================================================================\n// Vertex-to-vertex snap: dragging a vertex close enough to ANOTHER vertex\n// (same area or a different one \u2014 both are allowed now) makes them the\n// SAME shared vertex from then on. Runs on mouseup, not live during the\n// drag, so passing near another vertex mid-move doesn't trigger an\n// accidental merge.\n//\n// Only snaps onto OTHER draggable (non-outer-locked) vertices \u2014 snapping\n// onto the fixed outer boundary would let an interior edit alter the\n// outer boundary itself.\n// =============================================================================\n\nfunction tryMergeVertex(vid) {\n  if (!vertexPos[vid]) return false;\n  if (outerVertexIds.has(vid)) return false;\n\n  let target = null, bestDist = Infinity;\n  Object.keys(vertexPos).forEach(otherVid => {\n    if (otherVid === vid) return;\n    if (outerVertexIds.has(otherVid)) return;\n    const d = distDeg(vertexPos[vid], vertexPos[otherVid]);\n    if (d < bestDist) { bestDist = d; target = otherVid; }\n  });\n  if (!target || bestDist > SNAP_THRESHOLD_DEG) return false;\n\n  mergeVertexInto(vid, target);\n  const newAreaCount = Object.keys(areaRings).filter(a => areaRings[a].some(ring => ring.includes(target))).length;\n  log('Snapped vertex ' + vid + ' onto ' + target + ' \u2014 merged point now shared by ' + newAreaCount + ' area(s).');\n  return true;\n}\n\n// =============================================================================\n// Vertex-to-line snap: if a dragged vertex didn't land close enough to\n// another VERTEX, check whether it landed close to a LINE instead (an\n// interior edge it doesn't already touch). If so, that line splits at the\n// closest point \u2014 a new vertex is inserted there, shared by both areas\n// that border that edge \u2014 and the dragged vertex merges into that new\n// split point.\n// =============================================================================\n\nfunction trySnapToLine(vid) {\n  if (!vertexPos[vid]) return false;\n  const neighbors = new Set(getNeighborsOf(vid));\n\n  // Derived fresh from areaRings/outerVertexIds rather than depending on\n  // a cached `boundaryLines` from the renderer's own state -- same fix,\n  // same reasoning, as wouldOverlapMulti() above: that external variable\n  // was never part of this engine's own sandbox, so referencing it bare\n  // threw ReferenceError the moment this function actually ran (only on\n  // the split-then-drag path, which is why it went unnoticed until then).\n  const boundaryLines = [];\n  const seenPairs = new Set();\n  Object.keys(areaRings).forEach(a => {\n    areaRings[a].forEach(ring => {\n      for (let i = 0; i < ring.length - 1; i++) {\n        const v1 = ring[i], v2 = ring[i + 1];\n        const key = [v1, v2].sort().join('|');\n        if (seenPairs.has(key)) continue;\n        seenPairs.add(key);\n        boundaryLines.push({ v1, v2, isOuter: outerVertexIds.has(v1) && outerVertexIds.has(v2) });\n      }\n    });\n  });\n\n  let best = null, bestDist = Infinity, bestPos = null;\n  boundaryLines.forEach(line => {\n    if (line.isOuter) return;\n    if (line.v1 === vid || line.v2 === vid) return; // vid's own adjacent edge\n    if (neighbors.has(line.v1) && neighbors.has(line.v2)) return; // degenerate tiny-triangle case\n    const p1 = vertexPos[line.v1], p2 = vertexPos[line.v2];\n    const cp = closestPointOnSegment(vertexPos[vid], p1, p2);\n    const d = distDeg(vertexPos[vid], cp);\n    if (d < bestDist) { bestDist = d; best = line; bestPos = cp; }\n  });\n\n  if (!best || bestDist > SNAP_THRESHOLD_DEG) return false;\n\n  const newVid = splitEdgeAt(best.v1, best.v2, bestPos);\n  mergeVertexInto(vid, newVid);\n  log('Snapped vertex ' + vid + ' onto the line between ' + best.v1 + ' and ' + best.v2 + ' \u2014 line split, new shared point created.');\n  return true;\n}\n\n// =============================================================================\n// Line-driven drag interaction. There are no visible vertex markers at\n// all \u2014 the boundary lines themselves are what the user sees and clicks.\n// mousedown on an interior line either grabs its nearer endpoint (click\n// landed close to one) or splits the line at the click point and grabs\n// the new vertex \u2014 either way, a drag then follows the mouse via the\n// map's own mousemove/mouseup, live-enforcing the outer-boundary\n// containment and overlap-prevention checks, and finalizing with\n// vertex-to-vertex then vertex-to-line snapping on release.\n// =============================================================================\n\nlet activeDrag = null; // { vid, startPos, weights, originalPositions } | null\n\n// Proportional-editing falloff: dragging a vertex pulls its boundary\n// neighbors along too, by a decreasing amount the further away they are.\n// \"Distance\" is measured in STEPS along the boundary (ring adjacency),\n// not raw spatial distance, so the effect follows the shape rather than\n// pulling in an unrelated vertex that just happens to be spatially close.\n// Locked (outer) vertices never move, but influence still passes THROUGH\n// them to reach vertices beyond \u2014 only their own weight is excluded.\nfunction computeFalloffWeights(vid) {\n  const dist = { [vid]: 0 };\n  const queue = [vid];\n  let qi = 0;\n  while (qi < queue.length) {\n    const cur = queue[qi++];\n    if (dist[cur] >= FALLOFF_MAX_STEPS) continue;\n    getNeighborsOf(cur).forEach(n => {\n      if (dist[n] === undefined) { dist[n] = dist[cur] + 1; queue.push(n); }\n    });\n  }\n  const weights = {};\n  Object.keys(dist).forEach(v => {\n    if (outerVertexIds.has(v)) return; // locked \u2014 excluded from the moving set entirely\n    const d = dist[v];\n    if (FALLOFF_SIGMA_STEPS <= 0) {\n      // \"0 steps\" means only the exact grabbed vertex moves \u2014 the\n      // Gaussian formula divides by sigma and is undefined at sigma=0,\n      // so this is handled as an explicit special case rather than\n      // relying on the formula degenerating gracefully.\n      if (d === 0) weights[v] = 1;\n      return;\n    }\n    const w = Math.exp(-(d * d) / (2 * FALLOFF_SIGMA_STEPS * FALLOFF_SIGMA_STEPS));\n    if (w >= FALLOFF_MIN_WEIGHT) weights[v] = w;\n  });\n  return weights;\n}\n\n// Multi-vertex version of the overlap-prevention check: tests every\n// moving vertex's edges (to ALL its neighbors, moving or static) against\n// every interior line that doesn't share an endpoint with that specific\n// edge. Does not check moving vertices against each other for mutual\n// self-crossing within the deformed region itself \u2014 a smooth Gaussian\n// falloff rarely folds over its own local neighborhood, and adding that\n// check is a larger scope than this pass covers; noting it as a known\n// gap rather than silently skipping it.\nfunction wouldOverlapMulti(movingPositions) {\n  // boundaryLines is derived fresh here from areaRings/outerVertexIds\n  // rather than depending on the renderer's own cached line list (which in\n  // the original prototype was populated by drawVertexBoundaries() and\n  // could go stale between render frames) -- same edges, computed\n  // directly from the current topology every call.\n  const boundaryLines = [];\n  const seenPairs = new Set();\n  Object.keys(areaRings).forEach(a => {\n    areaRings[a].forEach(ring => {\n      for (let i = 0; i < ring.length - 1; i++) {\n        const v1 = ring[i], v2 = ring[i + 1];\n        const key = [v1, v2].sort().join('|');\n        if (seenPairs.has(key)) continue;\n        seenPairs.add(key);\n        boundaryLines.push({ v1, v2, isOuter: outerVertexIds.has(v1) && outerVertexIds.has(v2) });\n      }\n    });\n  });\n\n  const resolvedPos = (v) => movingPositions[v] || vertexPos[v];\n  for (const vid of Object.keys(movingPositions)) {\n    const neighbors = getNeighborsOf(vid);\n    for (const n of neighbors) {\n      const a = resolvedPos(vid), b = resolvedPos(n);\n      for (const line of boundaryLines) {\n        if (line.isOuter) continue;\n        if (line.v1 === vid || line.v2 === vid || line.v1 === n || line.v2 === n) continue;\n        const c = resolvedPos(line.v1), d = resolvedPos(line.v2);\n        if (segmentsIntersect(a, b, c, d)) return true;\n      }\n    }\n  }\n  return false;\n}\n\n\nfunction getJunctionSlideSegment(vid) {\n  if (!outerVertexIds.has(vid)) return null;\n  const neighbors = getNeighborsOf(vid);\n  const outerNeighbors = neighbors.filter(n => outerVertexIds.has(n));\n  const interiorNeighbors = neighbors.filter(n => !outerVertexIds.has(n));\n  if (outerNeighbors.length !== 2 || interiorNeighbors.length === 0) return null;\n  const [a, b] = outerNeighbors;\n  const COLLINEAR_TOLERANCE_RAD = 5 * Math.PI / 180; // must be within 5 degrees of a straight 180-degree line through vid\n  const angle = angleBetween(vertexPos[a], vertexPos[vid], vertexPos[b]);\n  if (Math.abs(angle - Math.PI) > COLLINEAR_TOLERANCE_RAD) return null; // a real corner, not a straight run \u2014 stays fully locked\n\n  // Extend past simple spacing points in each direction, stopping only\n  // at a real barrier: a true corner, or another junction (a different\n  // area's own boundary touching the edge \u2014 the only place ownership\n  // along the border can actually change). A spacing point added purely\n  // to satisfy the max-vertex-spacing rule on a straight run isn't a\n  // meaningful barrier at all \u2014 it's collinear with everything around\n  // it by construction, so sliding past it doesn't change the\n  // boundary's shape. Without this, the slide range was bounded by\n  // whatever the NEAREST such point happened to be (observed directly:\n  // every junction's range came out as exactly 2 raw grid steps,\n  // completely independent of grid size or how long the actual straight\n  // run was), rather than the true extent of the straight run.\n  return { a: extendAlongStraightRun(a, vid), b: extendAlongStraightRun(b, vid) };\n}\n\nfunction extendAlongStraightRun(startVid, cameFrom) {\n  const COLLINEAR_TOLERANCE_RAD = 5 * Math.PI / 180;\n  let prev = cameFrom, current = startVid;\n  const maxSteps = Object.keys(vertexPos).length + 1; // defensive cap \u2014 a well-formed boundary always terminates well before this\n  for (let step = 0; step < maxSteps; step++) {\n    const neighbors = getNeighborsOf(current);\n    const outerNeighbors = neighbors.filter(n => outerVertexIds.has(n));\n    const interiorNeighbors = neighbors.filter(n => !outerVertexIds.has(n));\n    if (interiorNeighbors.length > 0) return current; // another junction \u2014 a real barrier, stop here\n    if (outerNeighbors.length !== 2) return current; // unusual topology \u2014 stop defensively rather than guess\n    const [x, y] = outerNeighbors;\n    const angle = angleBetween(vertexPos[x], vertexPos[current], vertexPos[y]);\n    if (Math.abs(angle - Math.PI) > COLLINEAR_TOLERANCE_RAD) return current; // a true corner \u2014 stop here\n    const next = outerNeighbors.find(n => n !== prev);\n    if (next === undefined) return current; // defensive \u2014 shouldn't happen, avoids an infinite loop\n    prev = current;\n    current = next;\n  }\n  return current; // hit the defensive step cap \u2014 stop rather than loop forever\n}\n\nfunction angleBetween(p1, p0, p2) {\n  const v1 = { lat: p1.lat - p0.lat, lng: p1.lng - p0.lng };\n  const v2 = { lat: p2.lat - p0.lat, lng: p2.lng - p0.lng };\n  const dot = v1.lat * v2.lat + v1.lng * v2.lng;\n  const mag1 = Math.hypot(v1.lat, v1.lng), mag2 = Math.hypot(v2.lat, v2.lng);\n  if (mag1 === 0 || mag2 === 0) return Math.PI; // degenerate \u2014 treat as wide/safe rather than falsely triggering\n  const cos = Math.max(-1, Math.min(1, dot / (mag1 * mag2)));\n  return Math.acos(cos);\n}\n\n// Classifies a border vertex for MARKER/rendering purposes \u2014 richer than\n// getJunctionSlideSegment, which only answers \"is this safely SLIDABLE\"\n// (used for the actual drag constraint). This also identifies a true\n// corner that happens to ALSO touch an interior area boundary: that's\n// genuinely an intersection too \u2014 worth showing as major, so it's\n// visible and the person can see it's meaningful \u2014 but not draggable,\n// since a corner has no single straight line to slide along without\n// distorting the boundary's own shape. Returns null for anything that\n// isn't a border intersection at all (not on the border, or a purely\n// locked spacing point with no interior branch).\nfunction classifyBorderIntersection(vid) {\n  if (!outerVertexIds.has(vid)) return null;\n  const neighbors = getNeighborsOf(vid);\n  const outerNeighbors = neighbors.filter(n => outerVertexIds.has(n));\n  const interiorNeighbors = neighbors.filter(n => !outerVertexIds.has(n));\n  if (outerNeighbors.length !== 2 || interiorNeighbors.length === 0) return null;\n  const seg = getJunctionSlideSegment(vid);\n  if (seg) return { slidable: true, seg };\n  return { slidable: false }; // a true corner that also touches an interior boundary\n}\n\n// Closes a border-touching sliver that a \"loop back\" peninsula check\n// can't see at all: an interior boundary meeting the outer edge at a\n// shallow angle, tapering an area's territory to almost nothing right at\n// the junction \u2014 not a spike that goes out and doubles back, just two\n// boundaries converging. Detected directly as an angle at the junction,\n// between the interior branch and whichever outer neighbor it's closer\n// to. Closed by merging the junction into THAT outer neighbor \u2014 since\n// the junction's position was already constrained to the segment\n// between its two outer neighbors, merging it onto one of them can't\n// move the outer boundary's own path; it just eliminates the thin wedge\n// on that side entirely (the other area, which already bordered the\n// outer edge up to this junction, absorbs it automatically via the\n// existing consecutive-duplicate dedup in mergeVertexInto).\n\nfunction closeThinBorderWedges(minAngleRad) {\n  let totalClosed = 0;\n  for (let pass = 0; pass < 20; pass++) {\n    let closedThisPass = false;\n    for (const J of Object.keys(vertexPos)) {\n      const seg = getJunctionSlideSegment(J);\n      if (!seg) continue;\n      const interiorNeighbors = getNeighborsOf(J).filter(n => !outerVertexIds.has(n));\n      const posJ = vertexPos[J], posA = vertexPos[seg.a], posB = vertexPos[seg.b];\n      let mergeTarget = null;\n      for (const N of interiorNeighbors) {\n        const posN = vertexPos[N];\n        const angleToA = angleBetween(posN, posJ, posA);\n        const angleToB = angleBetween(posN, posJ, posB);\n        const minAngle = Math.min(angleToA, angleToB);\n        if (minAngle < minAngleRad) { mergeTarget = angleToA < angleToB ? seg.a : seg.b; break; }\n      }\n      if (mergeTarget === null) continue;\n      mergeVertexInto(J, mergeTarget);\n      totalClosed++;\n      closedThisPass = true;\n      break; // topology changed \u2014 restart the scan\n    }\n    if (!closedThisPass) break;\n  }\n  return totalClosed;\n}\n\n\nfunction computeSegmentIntersectionPoint(p1, p2, p3, p4) {\n  const x1 = p1.lng, y1 = p1.lat, x2 = p2.lng, y2 = p2.lat;\n  const x3 = p3.lng, y3 = p3.lat, x4 = p4.lng, y4 = p4.lat;\n  const denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);\n  if (Math.abs(denom) < 1e-15) return null;\n  const t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom;\n  return { lat: y1 + t * (y2 - y1), lng: x1 + t * (x2 - x1) };\n}\n\n// Closes slivers/gaps: any two distinct, non-locked vertices sitting\n// closer together than the repair threshold get merged into one \u2014 the\n// same merge machinery used for live snap-to-vertex, just swept\n// proactively across the whole vertex set instead of triggered by one\n// drag. Iterates since each merge changes the vertex set; capped so a\n// pathological case can't loop forever.\nfunction repairNearCoincidentVertices(thresholdDeg) {\n  let totalMerged = 0;\n  for (let pass = 0; pass < 10; pass++) {\n    const vids = Object.keys(vertexPos).filter(v => !outerVertexIds.has(v));\n    let mergedThisPass = false;\n    outer:\n    for (let i = 0; i < vids.length; i++) {\n      const a = vids[i];\n      if (!vertexPos[a]) continue;\n      for (let j = i + 1; j < vids.length; j++) {\n        const b = vids[j];\n        if (!vertexPos[b]) continue;\n        if (distDeg(vertexPos[a], vertexPos[b]) <= thresholdDeg) {\n          mergeVertexInto(a, b);\n          totalMerged++;\n          mergedThisPass = true;\n          break outer; // vertex set changed \u2014 restart the scan\n        }\n      }\n    }\n    if (!mergedThisPass) break;\n  }\n  return totalMerged;\n}\n\n// Closes overlaps: finds pairs of interior edges (from unrelated parts of\n// the boundary \u2014 edges that don't already share an endpoint) that cross,\n// and fuses them at their crossing point. This converts an invalid\n// self-intersection into a valid shared vertex at that exact point \u2014\n// topologically legal, though it can leave a visible pinch rather than a\n// clean separation. That's the honest scope of a from-scratch repair\n// without a real polygon-clipping library: it eliminates the invalid\n// geometry, but \"automatically closing an overlap\" here means \"resolve\n// it into a pinch point,\" not \"cleanly redraw both shapes around each\n// other\" \u2014 a general boolean-geometry operation is a different, larger\n// feature.\n\nfunction repairOverlaps() {\n  // Snapshot before attempting anything \u2014 if the attempt doesn't fully\n  // converge, roll back completely rather than leave a partially-fused,\n  // possibly MORE fragmented mess than the original overlap. A failed\n  // repair should be a no-op from the user's perspective, not \"fixed 25\n  // crossings, created 1000 more.\"\n  const areaRingsSnapshot = JSON.parse(JSON.stringify(areaRings));\n  const vertexPosSnapshot = JSON.parse(JSON.stringify(vertexPos));\n  const initialCrossings = countCrossings();\n\n  let totalResolved = 0;\n  const MAX_PASSES = 25;\n  let pass = 0;\n  for (; pass < MAX_PASSES; pass++) {\n    const uniqueEdges = [];\n    const seen = new Set();\n    Object.keys(areaRings).forEach(a => {\n      areaRings[a].forEach(ring => {\n        for (let i = 0; i < ring.length - 1; i++) {\n          const v1 = ring[i], v2 = ring[i + 1];\n          const key = [v1, v2].sort().join('|');\n          if (seen.has(key)) return;\n          seen.add(key);\n          uniqueEdges.push({ v1, v2 });\n        }\n      });\n    });\n\n    let resolvedThisPass = false;\n    outer:\n    for (let i = 0; i < uniqueEdges.length; i++) {\n      const e1 = uniqueEdges[i];\n      for (let j = i + 1; j < uniqueEdges.length; j++) {\n        const e2 = uniqueEdges[j];\n        if (e1.v1 === e2.v1 || e1.v1 === e2.v2 || e1.v2 === e2.v1 || e1.v2 === e2.v2) continue; // shares an endpoint \u2014 legitimate adjacency\n        const p1 = vertexPos[e1.v1], p2 = vertexPos[e1.v2];\n        const p3 = vertexPos[e2.v1], p4 = vertexPos[e2.v2];\n        if (!segmentsIntersect(p1, p2, p3, p4)) continue;\n        const ip = computeSegmentIntersectionPoint(p1, p2, p3, p4);\n        if (!ip) continue; // parallel/degenerate \u2014 nothing sensible to fuse at\n        const n1 = splitEdgeAt(e1.v1, e1.v2, ip);\n        const n2 = splitEdgeAt(e2.v1, e2.v2, ip);\n        mergeVertexInto(n2, n1);\n        totalResolved++;\n        resolvedThisPass = true;\n        break outer; // topology changed \u2014 restart the scan\n      }\n    }\n    if (!resolvedThisPass) break;\n  }\n\n  const remaining = countCrossings();\n\n  // Roll back entirely if it didn't fully converge (remaining > 0) OR if,\n  // even having \"converged\" (no more crossing pairs the heuristic itself\n  // would act on), the attempt somehow left things worse than where it\n  // started. A fuse-at-intersection heuristic can only ever make a\n  // genuine two-crossing-point overlap between separately-shaped areas\n  // WORSE (as observed empirically \u2014 25 fusions on two overlapping\n  // triangles produced 1131 residual crossings among the fragments) \u2014 it\n  // has no way to correctly reorganize that topology, so attempting it\n  // and leaving the partial result in place would be actively harmful,\n  // not just incomplete.\n  if (remaining > 0 || remaining > initialCrossings) {\n    areaRings = areaRingsSnapshot;\n    vertexPos = vertexPosSnapshot;\n    return { resolved: 0, remaining: initialCrossings, hitPassCap: pass >= MAX_PASSES, rolledBack: true };\n  }\n\n  return { resolved: totalResolved, remaining: 0, hitPassCap: pass >= MAX_PASSES, rolledBack: false };\n}\n\n// Counts pairs of interior edges (not sharing an endpoint) that cross,\n// across the CURRENT areaRings/vertexPos state.\n\nfunction countCrossings() {\n  const edges = [];\n  const seen = new Set();\n  Object.keys(areaRings).forEach(a => areaRings[a].forEach(ring => {\n    for (let i = 0; i < ring.length - 1; i++) {\n      const key = [ring[i], ring[i + 1]].sort().join('|');\n      if (seen.has(key)) continue;\n      seen.add(key);\n      edges.push({ v1: ring[i], v2: ring[i + 1] });\n    }\n  }));\n  let count = 0;\n  for (let i = 0; i < edges.length; i++) {\n    for (let j = i + 1; j < edges.length; j++) {\n      const e1 = edges[i], e2 = edges[j];\n      if (e1.v1 === e2.v1 || e1.v1 === e2.v2 || e1.v2 === e2.v1 || e1.v2 === e2.v2) continue;\n      if (segmentsIntersect(vertexPos[e1.v1], vertexPos[e1.v2], vertexPos[e2.v1], vertexPos[e2.v2])) count++;\n    }\n  }\n  return count;\n}\n\n// =============================================================================\n// Small-area / island absorption. A small ring part can exist as a\n// SEPARATE ring in this data structure two ways: fully encircled by one\n// interior neighbor (a genuine hole in that neighbor's boundary, not a\n// notch), or bordering the FIXED outer boundary on one side and exactly\n// one interior neighbor on the other (a sliver squeezed against the\n// district edge \u2014 the outer edge itself never moves, but the sliver's\n// territory can still change hands). Either way it's absorbed into that\n// sole neighbor. A shape that's still connected to the rest of its own\n// territory (a thin peninsula, not touching the outer edge) is\n// topologically different: it shows up as a notch embedded WITHIN the\n// neighbor's larger ring, not as a separate ring at all \u2014 so it can't be\n// absorbed the same way until it's first \"pinched off\" into a proper\n// island (see pinchOffThinNecks below).\n// =============================================================================\n\nfunction ringArea(ring) {\n  let sum = 0;\n  for (let i = 0; i < ring.length - 1; i++) {\n    const p1 = vertexPos[ring[i]], p2 = vertexPos[ring[i + 1]];\n    sum += p1.lng * p2.lat - p2.lng * p1.lat;\n  }\n  return Math.abs(sum) / 2;\n}\n\n// For a ring's edges, which SINGLE other area borders all of them? Returns\n// null if the ring touches the outer boundary, touches more than one\n// other area, or touches its own owning area's other ring parts (none of\n// those have a single well-defined \"absorb into\" target).\n\nfunction soleBorderingArea(ownerAreaId, ring) {\n  return soleBorderingAreaForEdges(ownerAreaId, ring, ring.length - 1);\n}\n\n// Same check, but for an OPEN path (vertices[0..count]) rather than a\n// closed ring \u2014 used for a candidate spike/notch BEFORE it's been cut\n// out, where checking a closed loop would require fabricating a\n// base-to-base \"closing\" edge that doesn't exist yet in the real\n// boundary (that's exactly what the pinch-off is supposed to create),\n// and would wrongly fail to find a neighbor for it since nothing borders\n// an edge nobody has drawn.\nfunction soleBorderingAreaForEdges(ownerAreaId, vertices, edgeCount) {\n  let bordering = null;\n  let sawInteriorEdge = false;\n  for (let i = 0; i < edgeCount; i++) {\n    const v1 = vertices[i], v2 = vertices[i + 1];\n    if (outerVertexIds.has(v1) && outerVertexIds.has(v2)) continue; // an edge along the FIXED outer boundary itself doesn't move and doesn't count toward \"which neighbor\" \u2014 the sliver's territory can still be reassigned even though this particular edge stays exactly where it is\n    sawInteriorEdge = true;\n    let edgeArea = null;\n    for (const a of Object.keys(areaRings)) {\n      if (a === ownerAreaId) continue;\n      const found = areaRings[a].some(r => {\n        for (let k = 0; k < r.length - 1; k++) {\n          if ((r[k] === v1 && r[k + 1] === v2) || (r[k] === v2 && r[k + 1] === v1)) return true;\n        }\n        return false;\n      });\n      if (found) { edgeArea = a; break; }\n    }\n    if (edgeArea === null) return null; // an interior edge with no single clear neighbor \u2014 bail rather than guess\n    if (bordering === null) bordering = edgeArea;\n    else if (bordering !== edgeArea) return null; // borders more than one neighbor\n  }\n  if (!sawInteriorEdge) return null; // nothing but outer boundary \u2014 no neighbor to absorb into at all\n  return bordering;\n}\n\n// Absorbs every ring part below the area threshold into its sole\n// bordering neighbor: the small ring is removed from its owner, and the\n// neighbor's matching hole (if this was a proper island) is removed too,\n// filling in the space. Iterates since each absorption can change what's\n// available to check next.\n\nfunction absorbSmallAreas(areaThresholdDeg2) {\n  let totalAbsorbed = 0;\n  for (let pass = 0; pass < 20; pass++) {\n    let absorbedThisPass = false;\n    outer:\n    for (const ownerAreaId of Object.keys(areaRings)) {\n      const parts = areaRings[ownerAreaId];\n      // Never absorb an area's LAST remaining ring part \u2014 that would\n      // delete the area entirely, which is a bigger decision than a\n      // sliver-cleanup pass should make silently.\n      if (parts.length <= 1) continue;\n      for (let idx = 0; idx < parts.length; idx++) {\n        const ring = parts[idx];\n        const area = ringArea(ring);\n        if (area > areaThresholdDeg2) continue;\n        const neighbor = soleBorderingArea(ownerAreaId, ring);\n        if (neighbor === null) continue; // touches the outer boundary or multiple areas \u2014 leave it, can't safely auto-resolve\n        // Remove the small ring from its owner.\n        areaRings[ownerAreaId] = parts.filter((_, i) => i !== idx);\n        // If the neighbor has a matching hole (same vertex set), remove\n        // it too \u2014 that's what \"fills in\" the absorbed territory for a\n        // proper island (fully surrounded, so the neighbor's territory\n        // already implicitly covers that space once the cutout is gone).\n        // A sliver that instead touches the FIXED OUTER boundary has no\n        // such hole \u2014 the neighbor never had a cutout there to begin\n        // with, since the sliver's territory was never \"inside\" the\n        // neighbor's own shape. In that case there's nothing to remove;\n        // the ring itself must be added to the neighbor directly, or the\n        // absorbed territory would become unclaimed by anyone.\n        const ringVertexSet = new Set(ring.slice(0, -1));\n        const neighborParts = areaRings[neighbor];\n        const matchIdx = neighborParts.findIndex(r => {\n          const rSet = new Set(r.slice(0, -1));\n          return rSet.size === ringVertexSet.size && [...rSet].every(v => ringVertexSet.has(v));\n        });\n        if (matchIdx !== -1) {\n          areaRings[neighbor] = neighborParts.filter((_, i) => i !== matchIdx);\n        } else {\n          areaRings[neighbor] = neighborParts.concat([ring]);\n        }\n        totalAbsorbed++;\n        absorbedThisPass = true;\n        break outer; // ring-part indices shifted \u2014 restart the scan\n      }\n    }\n    if (!absorbedThisPass) break;\n  }\n  return totalAbsorbed;\n}\n\n// =============================================================================\n// Thin-peninsula pinch-off. Detects a thin \"out and back\" excursion\n// within a ring (two non-adjacent vertices sitting close together, with a\n// long path between them) and \u2014 only when that excursion borders exactly\n// ONE other area throughout, on BOTH sides of the neck \u2014 splits it into\n// its own separate ring, doing the matching split on the neighbor's\n// mirrored notch at the same time. That turns the peninsula into a\n// proper freestanding island (and a proper hole in the neighbor), which\n// absorbSmallAreas() can then pick up and absorb normally. If the\n// neighbor's mirror can't be found cleanly, nothing is changed \u2014 a\n// half-pinched peninsula would be worse than leaving it alone.\n// =============================================================================\n\n\nfunction findRingSubsequence(ring, va, vb) {\n  // Finds va and vb as vertex ids within `ring` (ignoring the closing\n  // duplicate) and returns {ia, ib} their indices, or null if either\n  // isn't present.\n  const n = ring.length - 1;\n  let ia = -1, ib = -1;\n  for (let k = 0; k < n; k++) {\n    if (ring[k] === va) ia = k;\n    if (ring[k] === vb) ib = k;\n  }\n  if (ia === -1 || ib === -1) return null;\n  return { ia, ib };\n}\n\n\nfunction pinchOffThinNecks(neckThresholdDeg, minExcursionRatio) {\n  let totalPinched = 0;\n  for (let pass = 0; pass < 20; pass++) {\n    let pinchedThisPass = false;\n\n    outer:\n    for (const ownerAreaId of Object.keys(areaRings)) {\n      const parts = areaRings[ownerAreaId];\n      for (let ringIdx = 0; ringIdx < parts.length; ringIdx++) {\n        const ring = parts[ringIdx];\n        const n = ring.length - 1;\n        if (n < 6) continue; // too small to contain both a spike and a meaningful remaining body\n\n        // Collect every candidate that passes the shape checks (neck\n        // width, elongation ratio) for THIS ring, rather than acting on\n        // the first one found. A long thin peninsula has many valid-\n        // looking near-pairs along its own length \u2014 nibbling at the\n        // first one found peels it off in slices from the tip inward,\n        // and each slice's own remaining \"main body\" can then look like\n        // ANOTHER thin shape worth pinching, cascading until it\n        // consumes the whole thing (observed directly: a compact 3x3\n        // base got fully eaten this way). Taking the LARGEST valid span\n        // instead captures the whole peninsula in one cut.\n        const candidates = [];\n        for (let i = 0; i < n; i++) {\n          for (let j = i + 3; j < n; j++) {\n            if (i === 0 && j === n - 1) continue; // adjacent via the closing wrap, not a real neck\n            const d = distDeg(vertexPos[ring[i]], vertexPos[ring[j]]);\n            if (d > neckThresholdDeg) continue;\n            let pathLen = 0;\n            for (let k = i; k < j; k++) pathLen += distDeg(vertexPos[ring[k]], vertexPos[ring[k + 1]]);\n            if (pathLen < d * minExcursionRatio) continue;\n            candidates.push({ i, j, pathLen });\n          }\n        }\n        if (candidates.length === 0) continue;\n        candidates.sort((a, b) => (b.j - b.i) - (a.j - a.i)); // largest span first\n\n        for (const cand of candidates) {\n          const { i, j } = cand;\n\n          // Candidate spike: the OPEN path ring[i..j] \u2014 checked as a\n          // path, not a closed ring, since fabricating a base-to-base\n          // closing edge here (one that doesn't exist in the real\n          // boundary yet) would make soleBorderingArea wrongly fail to\n          // find a neighbor for it.\n          const spikePath = ring.slice(i, j + 1);\n          const neighbor = soleBorderingAreaForEdges(ownerAreaId, spikePath, spikePath.length - 1);\n          if (neighbor === null) continue;\n\n          // Find the SAME two vertices in the neighbor's own ring(s) \u2014\n          // its mirrored notch must exist there for this to be a\n          // genuine peninsula-into-that-neighbor (not, say, a spike\n          // that only touches the neighbor along one side).\n          let neighborRingIdx = -1, neighborSub = null;\n          for (let nk = 0; nk < areaRings[neighbor].length; nk++) {\n            const sub = findRingSubsequence(areaRings[neighbor][nk], ring[i], ring[j]);\n            if (sub) { neighborRingIdx = nk; neighborSub = sub; break; }\n          }\n          if (neighborRingIdx === -1) continue; // no mirrored notch found \u2014 don't guess, leave it alone\n\n          // Split the owner's ring: main body loses the spike (i..j\n          // collapses to a direct i-j edge); the spike becomes its own\n          // closed ring. ring.slice(j) already ends with the ring's\n          // closing duplicate of ring[0], so this concatenation is\n          // already properly closed on its own.\n          const mainRing = ring.slice(0, i + 1).concat(ring.slice(j));\n          const newSpikeRing = ring.slice(i, j + 1).concat([ring[i]]);\n\n          // Safety check: require the piece being removed to be small\n          // in ABSOLUTE terms (reusing the same \"small enough to\n          // absorb\" bar as absorbSmallAreas, for a single coherent\n          // definition of \"small\" across both steps) \u2014 not relative to\n          // the CURRENT remaining main area. A relative check sounds\n          // reasonable but doesn't actually prevent runaway erosion:\n          // each individual nibble can stay under a 25%-of-remainder\n          // cap while the remainder itself keeps shrinking pass over\n          // pass, cumulatively consuming the whole shape (observed\n          // directly \u2014 a legitimate tooth's single correct large\n          // candidate got rejected by the relative check, so smaller\n          // sub-candidates were tried instead, each individually\n          // \"safe\" by that measure, together eating the entire base).\n          // An absolute cap has no such moving target.\n          const spikeAreaCheck = ringArea(newSpikeRing);\n          if (spikeAreaCheck > MIN_AREA_THRESHOLD_DEG2) continue;\n\n          // Split the neighbor's mirrored ring the same way. nRing is\n          // already closed by construction (same reasoning as above),\n          // so no extra re-closing logic is needed here either.\n          const nRing = areaRings[neighbor][neighborRingIdx];\n          const nA = neighborSub.ia, nB = neighborSub.ib;\n          const lo = Math.min(nA, nB), hi = Math.max(nA, nB);\n          const nMainRing = nRing.slice(0, lo + 1).concat(nRing.slice(hi));\n          const nNotchRing = nRing.slice(lo, hi + 1).concat([nRing[lo]]);\n\n          // Sanity: both results must still be valid closed rings with\n          // at least 3 unique vertices. If not, don't apply anything.\n          const validRing = (r) => r.length >= 4 && r[0] === r[r.length - 1];\n          if (!validRing(mainRing) || !validRing(newSpikeRing) || !validRing(nMainRing) || !validRing(nNotchRing)) continue;\n\n          areaRings[ownerAreaId] = parts.map((r, k) => k === ringIdx ? mainRing : r).concat([newSpikeRing]);\n          areaRings[neighbor] = areaRings[neighbor].map((r, k) => k === neighborRingIdx ? nMainRing : r).concat([nNotchRing]);\n\n          totalPinched++;\n          pinchedThisPass = true;\n          break outer;\n        }\n      }\n    }\n    if (!pinchedThisPass) break;\n  }\n  return totalPinched;\n}\n\n\nfunction saveVertexEdits() {\n  // Runs the full repair pipeline in cycles until one full cycle makes\n  // no changes at all, rather than a fixed number of manual re-passes.\n  // Each individual step (pinch-off, absorb, gap-merge, overlap-repair)\n  // already loops internally until IT is stable, but a fix from one\n  // step can hand fresh work to another (overlap-repair can leave a\n  // freshly-split point close enough to need gap-merging; absorbing a\n  // ring can leave a neighbor newly small enough to absorb in turn) \u2014\n  // capping this at a fixed \"twice\" was only ever a guess based on the\n  // one cascade depth actually tested, not a guarantee.\n  //\n  // Deliberately NOT included here: automatic border-wedge closing.\n  // Closing a thin angle at a junction necessarily transfers that space\n  // to the neighboring area \u2014 and if that neighbor is itself somewhat\n  // elongated, the \"fix\" doesn't shrink anything, it just relocates\n  // which area holds the thin presence, sometimes making that area's\n  // own border reach LONGER in the process (confirmed directly: an area\n  // that reached to one junction ended up reaching to the NEXT one\n  // after \"closing\" its neighbor's thin angle). That's not a threshold\n  // to tune \u2014 deciding which area SHOULD get disputed border territory\n  // needs whole-shape context an automatic local-angle heuristic doesn't\n  // have. Junction vertices are rendered as visible, directly draggable\n  // points instead (see drawVertexBoundaries) \u2014 a person adjusts them\n  // deliberately rather than an automatic pass guessing.\n  let totalPinched = 0, totalAbsorbed = 0, totalGaps = 0;\n  let lastOverlapResult = { resolved: 0, remaining: 0, rolledBack: false };\n\n  for (let cycle = 0; cycle < 5; cycle++) {\n    const pinched = pinchOffThinNecks(PENINSULA_NECK_THRESHOLD_DEG, PENINSULA_MIN_RATIO);\n    const absorbed = absorbSmallAreas(MIN_AREA_THRESHOLD_DEG2);\n    const merged = repairNearCoincidentVertices(SNAP_THRESHOLD_DEG);\n    const overlapResult = repairOverlaps();\n\n    totalPinched += pinched;\n    totalAbsorbed += absorbed;\n    totalGaps += merged;\n    lastOverlapResult = overlapResult;\n\n    if (pinched === 0 && absorbed === 0 && merged === 0 && overlapResult.resolved === 0) break; // stable\n  }\n\n  drawVertexBoundaries();\n\n  const overlapResult = lastOverlapResult;\n  const parts = [];\n  if (totalPinched > 0) parts.push(totalPinched + ' thin peninsula(s) pinched off');\n  if (totalAbsorbed > 0) parts.push(totalAbsorbed + ' small area(s)/island(s) absorbed into a neighbor');\n  if (totalGaps > 0) parts.push(totalGaps + ' gap/sliver point(s) closed');\n  if (overlapResult.resolved > 0) parts.push(overlapResult.resolved + ' overlap(s) resolved');\n  if (parts.length === 0 && !overlapResult.rolledBack) {\n    log('Saved. No holes, slivers, overlaps, or thin peninsulas found.');\n  } else if (parts.length > 0) {\n    log('Saved. ' + parts.join(', ') + '.');\n  } else {\n    log('Saved. No gap/sliver repairs were needed.');\n  }\n  if (overlapResult.rolledBack) {\n    log('Warning: ' + overlapResult.remaining + ' overlap(s) detected but NOT automatically repaired \\u2014 ' +\n        'the attempt didn\\u2019t fully resolve them (or made it worse), so it was rolled back rather than ' +\n        'leave the boundary in a partially-fixed, more fragmented state. This fuse-at-intersection ' +\n        'heuristic works for a local self-fold but can\\u2019t reliably untangle two separately-shaped ' +\n        'areas that genuinely overlap at two points \\u2014 that needs real polygon boolean operations, ' +\n        'which this prototype doesn\\u2019t have. Manual adjustment may be needed.');\n  }\n}\n\n\n";

// Fixed snap distance -- 100m, not user-configurable. Converted using a
// constant meters-per-degree-latitude approximation, consistent with the
// rest of this engine's naive degree-Euclidean distance convention (it
// never accounts for longitude compression by latitude either).
const METERS_PER_DEGREE_LAT = 111320;
const FIXED_SNAP_METERS = 100;

function buildVertexEngine(vertexPos, areaRings, outerVertexIds, constants, logFn, drawFn) {
  const sandbox = {
    vertexPos, areaRings, outerVertexIds,
    log: logFn, drawVertexBoundaries: drawFn,
    ...constants
  };
  const fnBody = Object.keys(sandbox).map(k => `var ${k} = sandbox.${k};`).join('\n') + '\n' +
    VERTEX_ENGINE_SRC +
    '\nreturn { distDeg, closestPointOnSegment, getNeighborsOf, countAreasAtVertex, mergeVertexInto, ' +
    'splitEdgeAt, tryMergeVertex, trySnapToLine, computeFalloffWeights, wouldOverlapMulti, ' +
    'getJunctionSlideSegment, extendAlongStraightRun, angleBetween, classifyBorderIntersection, ' +
    'repairNearCoincidentVertices, repairOverlaps, soleBorderingArea, soleBorderingAreaForEdges, ' +
    'absorbSmallAreas, pinchOffThinNecks, saveVertexEdits, ' +
    'get_state: () => ({ vertexPos, areaRings, outerVertexIds }) };';
  return new Function('sandbox', fnBody)(sandbox);
}

// stiffness: 1 (loosest, widest falloff pull) .. 10 (stiffest, most rigid,
// nearly no falloff spread) -- inverse mapping to FALLOFF_SIGMA_STEPS.
function stiffnessToSigma(stiffness) {
  // Range extended from 1-10 to 1-20 -- 10 was an arbitrary starting
  // point, not a real limit (FALLOFF_SIGMA_STEPS <= 0 is already an
  // explicit, safe special case in computeFalloffWeights: "only the
  // grabbed vertex moves"). Piecewise so stiffness=1..10 behaves exactly
  // as before (sigma 5.0 down to 0.5), then continues at a shallower
  // slope from 10..20 so sigma reaches exactly 0 (fully rigid) at the
  // new max, rather than saturating early and wasting part of the range.
  const s = Math.max(1, Math.min(20, stiffness || 6));
  if (s <= 10) return (11 - s) / 2;       // 1 -> 5.0, 10 -> 0.5
  return 0.5 - (s - 10) * 0.05;           // 10 -> 0.5, 20 -> 0 (rigid)
}

function computeVertexConstants(vertexPos, sampleRow, sampleCol, stiffness) {
  const sw = vertexPos[sampleRow + '_' + sampleCol];
  const se = vertexPos[sampleRow + '_' + (sampleCol + 1)];
  const cellDeg = Math.hypot(sw.lat - se.lat, sw.lng - se.lng);
  return {
    SNAP_THRESHOLD_DEG: FIXED_SNAP_METERS / METERS_PER_DEGREE_LAT,   // fixed 100m, not scaled by cell size
    MIN_AREA_THRESHOLD_DEG2: (cellDeg * 3) * (cellDeg * 3),
    PENINSULA_NECK_THRESHOLD_DEG: cellDeg * 1.5,
    PENINSULA_MIN_RATIO: 3,
    MIN_BORDER_WEDGE_ANGLE_RAD: 20 * Math.PI / 180,
    FALLOFF_MAX_STEPS: 8,
    FALLOFF_MIN_WEIGHT: 0.02,
    FALLOFF_SIGMA_STEPS: stiffnessToSigma(stiffness)
  };
}


function getApp(msg) {
    const key = msg.mapId;
    if (!window.paintApps[key]) {
      window.paintApps[key] = createPaintApp(msg);
    }
    return window.paintApps[key];
  }

  function showLoading(msg) {
    const el = document.getElementById(msg.loadingOverlayId);
    if (el) el.style.display = 'flex';
  }

  function hideLoading(msg) {
    const el = document.getElementById(msg.loadingOverlayId);
    if (el) el.style.display = 'none';
  }

  function createPaintApp(msg) {
    return {
      mapId: msg.mapId,
      loadingOverlayId: msg.loadingOverlayId,
      readyInputId: msg.readyInputId,
      assignmentsInputId: msg.assignmentsInputId,
      // Optional — if provided, undo-stack depth is pushed to this Shiny
      // input every time it changes, so the R side can enable/disable an
      // Undo button. Safe to omit; undo still works locally without it.
      undoCountInputId: msg.undoCountInputId || null,
      map: null,
      districtLayer: null,
      popLayer: null,
      frictionLayer: null,
      subdivisionLayer: null,
      gridLayer: null,
      savedLayer: null,
      seedLayer: null,
      facilityLayer: null,
      landmarkLayer: null,
      brushPreview: null,
      isPainting: false,
      assignments: {},
      initialAssignments: {},
      dfaColors: {},
      activeDfa: null,
      cellLayers: {},
      centroids: {},
      neighbors: {},
      edgeCells: {},
      brushSize: 300,
      boundaryOnly: false,
      baseLayers: {},
      baseControl: null,
      currentBaseLayer: null,
      isRightPanning: false,
      rightPanStart: null,

      // ── Undo ──────────────────────────────────────────────────────────────
      // One unified stack covers every cell-ownership change: brush strokes
      // (one stroke = one entry, regardless of how many cells it touched —
      // bracketed by mousedown/mouseup so a whole drag undoes in one step),
      // and reset-to-auto-generated. Each entry is a full snapshot of
      // `assignments` from immediately before the change. Undo is purely
      // client-side/local — it does not round-trip to R until the next
      // submit, same as painting itself.
      undoStack: [],
      maxUndoSteps: 50,
      _strokeBaseline: null,

  // ── State ──────────────────────────────────────────────────────────────
  mode: 'paint',
  vertexPos: {}, areaRings: {}, outerVertexIds: new Set(), outerRings: [],
  vertexLayerGroup: null, boundaryLines: [], activeDrag: null, vertexEngine: null,
  vertexSmoothness: 2,   // "keep every Nth vertex" -- higher = more simplified
  vertexStiffness: 6,    // maps to FALLOFF_SIGMA_STEPS -- higher = more rigid
  _rawIdRingsByArea: null, _rawVertexPos: null, _hiddenForVertexMode: [],

  // ── Entering / leaving vertex mode ────────────────────────────────────
  // Extracts row/col + corner geometry directly from the grid layer already
  // loaded for painting. Traces once, caches the RAW (unsimplified) result
  // so the smoothness slider can re-simplify live without re-tracing from
  // cells (whose layer is hidden once in vertex mode).
  enterVertexMode: function(opts) {
    if (!this.gridLayer) return;
    opts = opts || {};
    const cells = {}, cellCorners = {}, assignments = {};
    this.gridLayer.eachLayer((layer) => {
      const props = layer.feature.properties;
      const cid = String(props.cell_id);
      if (props.row === undefined || props.col === undefined) return;
      cells[cid] = { row: props.row, col: props.col };
      // GeoJSON rings are [lng,lat]; the engine's convention is [lat,lng].
      const ring = layer.feature.geometry.coordinates[0];
      cellCorners[cid] = ring.slice(0, 4).map(([lng, lat]) => [lat, lng]);
      assignments[cid] = this.assignments[cid];
    });
    this._vertexCellSample = cells[Object.keys(cells)[0]];

    const { rawIdRingsByArea, vertexPos } = traceRealGridBoundaries(cells, cellCorners, assignments);
    this._rawIdRingsByArea = rawIdRingsByArea;
    this._rawVertexPos = vertexPos;

    this.vertexSmoothness = opts.smoothness || this.vertexSmoothness || 2;
    this.vertexStiffness = opts.stiffness || this.vertexStiffness || 6;
    this._applyVertexParams();

    this.mode = 'vertex';
    this._hidePaintLayersForVertexMode();
    this.ensureVertexLayerGroup();
    this.drawVertexBoundaries();
  },

  // ── Vertex-mode undo stack -- mirrors the paint-mode pushUndo/undo/
  // commitStroke pattern exactly, but snapshots vertexPos/areaRings/
  // outerVertexIds instead of assignments, since a vertex edit can change
  // topology (splits/merges) as well as position.
  pushVertexUndo: function(snapshot) {
    if (!this.vertexUndoStack) this.vertexUndoStack = [];
    this.vertexUndoStack.push(snapshot);
    if (this.vertexUndoStack.length > this.maxUndoSteps) this.vertexUndoStack.shift();
    this.emitVertexUndoCount();
  },

  // Captures the current vertex state as a plain, independently-mutable
  // snapshot (deep copy) -- called at the START of a drag (baseline) and
  // also reusable wherever a restorable checkpoint is needed.
  _snapshotVertexState: function() {
    return {
      vertexPos: JSON.parse(JSON.stringify(this.vertexPos)),
      areaRings: JSON.parse(JSON.stringify(this.areaRings)),
      outerVertexIds: [...this.outerVertexIds]
    };
  },

  _restoreVertexSnapshot: function(snapshot) {
    this.vertexPos = snapshot.vertexPos;
    this.areaRings = snapshot.areaRings;
    this.outerVertexIds = new Set(snapshot.outerVertexIds);
    this.outerRings = buildOuterRingsFromModel(this.areaRings, this.outerVertexIds);
    // Engine wraps vertexPos/areaRings/outerVertexIds by reference, so
    // swapping those objects out from under it leaves it stale --
    // rebuild it around the restored objects (current stiffness setting
    // carries over unchanged, same as any other engine rebuild).
    this._rebuildEngineOnly();
    this.drawVertexBoundaries();
  },

  vertexUndo: function() {
    if (!this.vertexUndoStack || this.vertexUndoStack.length === 0) return;
    const snapshot = this.vertexUndoStack.pop();
    this._restoreVertexSnapshot(snapshot);
    this.emitVertexUndoCount();
  },

  emitVertexUndoCount: function() {
    if (typeof Shiny === 'undefined') return;
    const id = this.vertexUndoCountInputId || (this.assignmentsInputId + '_vertex_undo_count');
    Shiny.setInputValue(id, (this.vertexUndoStack || []).length, { priority: 'event' });
  },

  // "Reset" for the refine step -- discards every manual edit made this
  // session and re-derives the boundary fresh from the cached raw trace
  // at the current smoothness/stiffness (the same re-derivation a
  // smoothness change triggers), clearing the undo stack since none of
  // its checkpoints are meaningful once the whole edit history is wiped.
  resetVertexEdits: function() {
    if (!this._rawIdRingsByArea) return;
    this._applyVertexParams();
  },

  // Dedicated boundary-refinement view -- hide every other layer rather
  // than overlay this on top of the paint canvas. Tracks exactly which
  // ones were actually visible (by key, not by object reference) so
  // exitVertexMode() restores only those, and re-callable safely if a
  // scene reload recreates these layers while vertex mode is still active
  // (see the defensive call at the end of loadScene()) -- checking
  // map.hasLayer() fresh each time means it correctly hides whatever the
  // CURRENT this[key] instance is, not a stale one.
  _hidePaintLayersForVertexMode: function() {
    this._hiddenForVertexMode = this._hiddenForVertexMode || [];
    ['gridLayer', 'brushPreview', 'savedLayer', 'districtLayer', 'popLayer',
     'frictionLayer', 'subdivisionLayer', 'seedLayer', 'facilityLayer', 'landmarkLayer']
      .forEach(key => {
        const layer = this[key];
        if (layer && this.map.hasLayer(layer)) {
          this.map.removeLayer(layer);
          if (!this._hiddenForVertexMode.includes(key)) this._hiddenForVertexMode.push(key);
        }
      });
  },

  // Re-simplifies from the cached raw trace at the current smoothness,
  // snaps the outer boundary onto the true district/health-area polygon
  // (correcting the grid-staircase approximation -- see boundary_snap.js),
  // and rebuilds the engine with the current constants (including
  // stiffness). Called on entry and whenever smoothness/stiffness change.
  _applyVertexParams: function() {
    const { areaRings, outerVertexIds } = simplifyVertexBoundary(
      this._rawIdRingsByArea, this._rawVertexPos, this.vertexSmoothness
    );
    // Fresh copy -- manual edits (drags/merges) must never mutate the
    // cached raw trace, since smoothness changes re-derive from it.
    const vertexPos = {};
    Object.keys(this._rawVertexPos).forEach(k => { vertexPos[k] = { ...this._rawVertexPos[k] }; });

    if (this._lastDistrictGeojson) {
      snapOuterBoundaryToTruth(vertexPos, outerVertexIds, this._lastDistrictGeojson);
    }

    this.vertexPos = vertexPos;
    this.areaRings = areaRings;
    this.outerVertexIds = outerVertexIds;
    this.outerRings = buildOuterRingsFromModel(areaRings, outerVertexIds);

    // Re-derivation invalidates every existing undo checkpoint -- vertex
    // IDs from before may not even exist in the freshly-simplified set.
    // Centralized here since all three callers (initial entry, a
    // smoothness change, and an explicit reset) need this equally.
    this.vertexUndoStack = [];
    this.emitVertexUndoCount();

    this._rebuildEngineOnly();
    if (this.mode === 'vertex') this.drawVertexBoundaries();
  },

  // Rebuilds ONLY the engine's constants (in particular FALLOFF_SIGMA_STEPS,
  // derived from stiffness) against whatever vertexPos/areaRings CURRENTLY
  // are -- manual edits included, untouched. Cell-size-derived constants
  // still come from the raw trace/sample cell, since those describe the
  // grid itself, not the current edited shape, and are unaffected by
  // edits either way.
  _rebuildEngineOnly: function() {
    const constants = computeVertexConstants(
      this._rawVertexPos, this._vertexCellSample.row, this._vertexCellSample.col, this.vertexStiffness
    );
    this.vertexEngine = buildVertexEngine(
      this.vertexPos, this.areaRings, this.outerVertexIds, constants,
      (msg) => { /* wire to the same log surface paint-app already has, if any */ },
      () => { this.syncFromEngine(); this.drawVertexBoundaries(); }
    );
  },

  // Smoothness legitimately has to re-derive the boundary from the cached
  // raw trace -- changing N changes which vertices even exist, so there's
  // no meaningful way to "keep" an edit to a vertex that may no longer be
  // part of the simplified set. This is a deliberate, documented tradeoff.
  setVertexSmoothness: function(n) {
    if (!this._rawIdRingsByArea) return;
    this.vertexSmoothness = n;
    this._applyVertexParams();
  },

  // Stiffness only affects how far a DRAG's pull spreads -- it has no
  // bearing on the boundary's current shape at all, so it must never
  // touch vertexPos/areaRings or discard edits. Only the engine's
  // constants get rebuilt, against whatever is currently on the canvas.
  setVertexStiffness: function(s) {
    if (!this._rawIdRingsByArea) return;
    this.vertexStiffness = s;
    this._rebuildEngineOnly();
  },

  // Returns to paint mode. "Back to Painting" continues FROM the current
  // vertex-refined boundary, not from whatever was painted before
  // refinement started -- so every cell's assignment is rewritten to
  // whichever refined area polygon actually contains its centroid, using
  // the same exterior/hole-aware membership test buildAreaGeojson uses
  // for output. A cell no area claims (shouldn't normally happen with a
  // valid boundary, but a manual edit could in principle produce a gap)
  // keeps its prior assignment rather than being cleared.
  exitVertexMode: function() {
    if (this.vertexEngine) {
      const tester = buildAreaMembershipTester(this.areaRings, this.vertexPos);
      Object.keys(this.centroids).forEach(cid => {
        const c = this.centroids[cid];
        const area = tester({ lat: c.lat, lng: c.lng });
        if (area) this.assignments[cid] = area;
      });
    }

    this.mode = 'paint';
    if (this.vertexLayerGroup) this.map.removeLayer(this.vertexLayerGroup);
    (this._hiddenForVertexMode || []).forEach(key => {
      const layer = this[key];
      if (layer) layer.addTo(this.map);
    });
    this._hiddenForVertexMode = [];
    this.vertexEngine = null;
    this.refreshAllStyles();
  },

  ensureVertexLayerGroup: function() {
    if (!this.vertexLayerGroup) this.vertexLayerGroup = L.layerGroup();
    this.vertexLayerGroup.addTo(this.map);
  },

  // Re-syncs from the engine's own closure state -- necessary because
  // repairOverlaps() can wholesale-reassign areaRings/vertexPos on rollback
  // (restoring a pre-attempt snapshot) rather than mutating them in place.
  syncFromEngine: function() {
    if (!this.vertexEngine) return;
    const state = this.vertexEngine.get_state();
    this.vertexPos = state.vertexPos;
    this.areaRings = state.areaRings;
    this.outerVertexIds = state.outerVertexIds;
    this.outerRings = buildOuterRingsFromModel(this.areaRings, this.outerVertexIds);
  },

  // ── Rendering (ported from the prototype, `this.`-ified) ──────────────
  drawVertexBoundaries: function() {
    this.vertexLayerGroup.clearLayers();
    this.boundaryLines = [];
    // Tracking structures for the incremental update path used during an
    // active drag (see updateVertexPositionsIncremental) -- record which
    // rendered Leaflet object corresponds to which vertex ID(s), so a
    // drag frame can update ONLY the handful of objects whose geometry
    // actually depends on a moving vertex via setLatLng/setLatLngs,
    // instead of tearing down and recreating every polygon/line/marker
    // in the whole boundary on every single mousemove event. At realistic
    // scale (thousands of grid cells, several health areas) a full
    // rebuild can mean over a thousand real SVG elements destroyed and
    // recreated per frame -- confirmed directly to cost hundreds of ms
    // to over a second per frame at that scale, which reads as the
    // refinement view simply failing to render/respond during a drag.
    this._polygonLayers = [];  // { layer, ring }
    this._lineLayers = [];     // { layer, hitTarget, v1, v2 }
    this._markerLayers = {};   // vid -> layer

    const vp = this.vertexPos, ar = this.areaRings, ov = this.outerVertexIds, eng = this.vertexEngine;

    // Always draw the TRUE district boundary directly from the district
    // GeoJSON, as its own dedicated outline -- independent of whatever
    // the per-area ring tracing/simplification produces. This guarantees
    // the outer boundary is complete and continuous even if some area's
    // traced outer segments have a gap (e.g. a long straight run with no
    // interior junction nearby), since this line has no dependency on
    // vertices, areas, or simplification at all -- it's the raw polygon
    // outline, always fully present.
    if (this._lastDistrictGeojson) {
      extractBoundaryRings(this._lastDistrictGeojson).forEach(ring => {
        L.polyline(ring.map(p => [p.lat, p.lng]), {
          color: '#0f172a', weight: 3, interactive: false
        }).addTo(this.vertexLayerGroup);
      });
    }

    Object.keys(ar).forEach(a => {
      ar[a].forEach(ring => {
        const latlngs = ring.map(vid => [vp[vid].lat, vp[vid].lng]);
        const polyLayer = L.polygon(latlngs, {
          // No fill -- transparent, showing just the boundary lines/markers
          // over the base map tiles. Vertex mode is a dedicated line-editing
          // view, not a colored-area overview (that's what painting is for).
          color: '#1e293b', weight: 0, fillColor: this.dfaColors[a] || '#757575',
          fillOpacity: 0, interactive: false
        }).addTo(this.vertexLayerGroup);
        this._polygonLayers.push({ layer: polyLayer, ring });
      });
    });

    const drawnPairs = new Set();
    Object.keys(ar).forEach(a => {
      ar[a].forEach(ring => {
        for (let i = 0; i < ring.length - 1; i++) {
          const v1 = ring[i], v2 = ring[i + 1];
          const pairKey = [v1, v2].sort().join('|');
          if (drawnPairs.has(pairKey)) return;
          drawnPairs.add(pairKey);
          const isOuter = ov.has(v1) && ov.has(v2);
          const latlngs = [[vp[v1].lat, vp[v1].lng], [vp[v2].lat, vp[v2].lng]];
          // Interior lines are the primary click/drag target for ordinary
          // (minor) vertices, but a thin visible line has a correspondingly
          // narrow hit-test corridor in Leaflet -- hard to land a click on
          // precisely. Slightly thicker visually (2px -> 3px), plus a much
          // wider INVISIBLE line drawn on top with the same coordinates and
          // mousedown handler, so the actual clickable corridor is far more
          // forgiving without the boundary looking heavy.
          const line = L.polyline(latlngs, { color: '#0f172a', weight: isOuter ? 3 : 3, interactive: !isOuter })
            .addTo(this.vertexLayerGroup);
          let hitTarget = null;
          if (!isOuter) {
            line.on('mousedown', (e) => this.onLineMouseDown(v1, v2, e));
            hitTarget = L.polyline(latlngs, { opacity: 0, weight: 18, interactive: true })
              .addTo(this.vertexLayerGroup);
            hitTarget.on('mousedown', (e) => this.onLineMouseDown(v1, v2, e));
          }
          this.boundaryLines.push({ layer: line, v1, v2, isOuter });
          this._lineLayers.push({ layer: line, hitTarget, v1, v2 });
        }
      });
    });

    const seenVertices = new Set();
    Object.values(ar).forEach(parts => parts.forEach(ring => ring.forEach(vid => {
      if (seenVertices.has(vid)) return;
      seenVertices.add(vid);
      const borderInfo = eng.classifyBorderIntersection(vid);
      if (borderInfo) {
        if (borderInfo.slidable) {
          this.addVertexMarker(vid, 'major', (e) => this.onJunctionMarkerMouseDown(vid, borderInfo.seg, e));
        } else {
          this.addVertexMarker(vid, 'major', (e) => this.onFixedCornerMouseDown(vid, e));
        }
        return;
      }
      if (ov.has(vid)) return;
      const areaCount = eng.countAreasAtVertex(vid);
      if (areaCount >= 3) {
        this.addVertexMarker(vid, 'major', (e) => this.onOrdinaryVertexMarkerMouseDown(vid, e));
      }
    })));
  },

  // Lightweight position-only update for an active drag frame -- touches
  // only the polygons/lines/markers whose geometry actually includes one
  // of the moved vertices, via setLatLngs/setLatLng, with no DOM teardown,
  // no recreation, and no re-running the (comparatively expensive) major-
  // vertex classification. Safe because topology (which vertices are
  // major/minor/junction, which areas border what) never changes during
  // a drag -- only positions move; merges/splits that CAN change topology
  // only happen at mouseup, which still does a full drawVertexBoundaries().
  updateVertexPositionsIncremental: function(movedVertexIds) {
    const vp = this.vertexPos;
    const movedSet = new Set(movedVertexIds);

    this._polygonLayers.forEach(({ layer, ring }) => {
      if (ring.some(v => movedSet.has(v))) {
        layer.setLatLngs(ring.map(vid => [vp[vid].lat, vp[vid].lng]));
      }
    });
    this._lineLayers.forEach(({ layer, hitTarget, v1, v2 }) => {
      if (movedSet.has(v1) || movedSet.has(v2)) {
        const latlngs = [[vp[v1].lat, vp[v1].lng], [vp[v2].lat, vp[v2].lng]];
        layer.setLatLngs(latlngs);
        if (hitTarget) hitTarget.setLatLngs(latlngs);
      }
    });
    movedVertexIds.forEach(vid => {
      const marker = this._markerLayers[vid];
      if (marker) marker.setLatLng([vp[vid].lat, vp[vid].lng]);
    });
  },

  addVertexMarker: function(vid, cls, onMouseDown) {
    const pos = this.vertexPos[vid];
    const isMajor = cls === 'major';
    const size = isMajor ? 14 : 8, dotSize = isMajor ? 12 : 6, border = isMajor ? 3 : 2;
    const marker = L.marker([pos.lat, pos.lng], {
      draggable: false,
      icon: L.divIcon({
        className: '', iconSize: [size, size], iconAnchor: [size/2, size/2],
        html: '<div style="width:' + dotSize + 'px;height:' + dotSize + 'px;border-radius:50%;background:#fff;' +
              'border:' + border + 'px solid #0f172a;box-shadow:0 0 0 ' + (isMajor?2:1) + 'px #fff;"></div>'
      })
    }).addTo(this.vertexLayerGroup);
    marker.on('mousedown', onMouseDown);
    this._markerLayers[vid] = marker;
  },

  onJunctionMarkerMouseDown: function(vid, seg, e) {
    if (this.activeDrag) return;
    this._captureVertexBaselineOnce();
    L.DomEvent.stopPropagation(e);
    if (e.originalEvent) L.DomEvent.preventDefault(e.originalEvent);
    if (this.map.dragging) this.map.dragging.disable();
    this.activeDrag = {
      vid, startPos: { lat: this.vertexPos[vid].lat, lng: this.vertexPos[vid].lng },
      weights: { [vid]: 1 },
      originalPositions: { [vid]: { lat: this.vertexPos[vid].lat, lng: this.vertexPos[vid].lng } },
      junctionSegment: seg
    };
    this.drawVertexBoundaries();
  },

  onFixedCornerMouseDown: function(vid, e) {
    L.DomEvent.stopPropagation(e);
    if (e.originalEvent) L.DomEvent.preventDefault(e.originalEvent);
    // TODO: surface via whatever notification channel paint-app/Shiny uses.
  },

  onOrdinaryVertexMarkerMouseDown: function(vid, e) {
    if (this.activeDrag) return;
    this._captureVertexBaselineOnce();
    L.DomEvent.stopPropagation(e);
    if (e.originalEvent) L.DomEvent.preventDefault(e.originalEvent);
    if (this.map.dragging) this.map.dragging.disable();
    const weights = this.vertexEngine.computeFalloffWeights(vid);
    const originalPositions = {};
    Object.keys(weights).forEach(v => { originalPositions[v] = { lat: this.vertexPos[v].lat, lng: this.vertexPos[v].lng }; });
    this.activeDrag = { vid, startPos: { lat: this.vertexPos[vid].lat, lng: this.vertexPos[vid].lng }, weights, originalPositions, junctionSegment: null };
    this.drawVertexBoundaries();
  },

  onLineMouseDown: function(v1, v2, e) {
    if (this.activeDrag) return;
    this._captureVertexBaselineOnce();
    const eng = this.vertexEngine, vp = this.vertexPos;
    const clickPos = { lat: e.latlng.lat, lng: e.latlng.lng };
    const d1 = eng.distDeg(clickPos, vp[v1]), d2 = eng.distDeg(clickPos, vp[v2]);
    const GRAB_RADIUS = eng.distDeg(vp[v1], vp[v2]) * 0.35; // slightly more forgiving than before -- easier to land on the endpoint rather than mid-line
    let grabVid = null;
    if (d1 < GRAB_RADIUS && d1 <= d2) grabVid = v1;
    else if (d2 < GRAB_RADIUS) grabVid = v2;

    L.DomEvent.stopPropagation(e);
    if (e.originalEvent) L.DomEvent.preventDefault(e.originalEvent);
    if (this.map.dragging) this.map.dragging.disable();

    if (grabVid) {
      const junctionSeg = eng.getJunctionSlideSegment(grabVid);
      if (junctionSeg) { this.onJunctionMarkerMouseDown(grabVid, junctionSeg, e); return; }
      this.onOrdinaryVertexMarkerMouseDown(grabVid, e);
      return;
    }
    const newVid = eng.splitEdgeAt(v1, v2, clickPos);
    this.syncFromEngine();
    this.onOrdinaryVertexMarkerMouseDown(newVid, e);
  },

  // Captures the pre-gesture state exactly once per drag gesture,
  // regardless of which of the three mousedown entry points fires first --
  // critical for onLineMouseDown's split-then-grab path, where splitEdgeAt()
  // mutates state (adds a vertex) BEFORE onOrdinaryVertexMarkerMouseDown
  // runs; without this guard, capturing the baseline inside that later
  // call would miss the split itself, and undo would silently leave the
  // split vertex behind after undoing only the drag.
  _captureVertexBaselineOnce: function() {
    if (this._vertexBaselineCaptured) return;
    this._vertexEditBaselineStr = JSON.stringify(this._snapshotVertexState());
    this._vertexBaselineCaptured = true;
  },


  onVertexMouseMove: function(e) {
    if (!this.activeDrag) return;
    const proposed = { lat: e.latlng.lat, lng: e.latlng.lng };
    const eng = this.vertexEngine;
    let candidatePositions;
    if (this.activeDrag.junctionSegment) {
      const seg = this.activeDrag.junctionSegment;
      const projected = eng.closestPointOnSegment(proposed, this.vertexPos[seg.a], this.vertexPos[seg.b]);
      candidatePositions = { [this.activeDrag.vid]: projected };
    } else {
      const rawDeltaLat = proposed.lat - this.activeDrag.startPos.lat;
      const rawDeltaLng = proposed.lng - this.activeDrag.startPos.lng;
      candidatePositions = {};
      Object.keys(this.activeDrag.weights).forEach(v => {
        const w = this.activeDrag.weights[v];
        const orig = this.activeDrag.originalPositions[v];
        candidatePositions[v] = { lat: orig.lat + rawDeltaLat * w, lng: orig.lng + rawDeltaLng * w };
      });
    }

    if (!this.activeDrag.junctionSegment) {
      if (!Object.values(candidatePositions).every(p => pointInOuterRings(p, this.outerRings, this.vertexPos))) return;
    }
    if (eng.wouldOverlapMulti(candidatePositions)) return;

    Object.keys(candidatePositions).forEach(v => { this.vertexPos[v] = candidatePositions[v]; });
    this.updateVertexPositionsIncremental(Object.keys(candidatePositions));
  },

  onVertexMouseUp: function() {
    if (!this.activeDrag) return;
    const vid = this.activeDrag.vid;
    const wasJunction = !!this.activeDrag.junctionSegment;
    this.activeDrag = null;
    if (this.map.dragging) this.map.dragging.enable();
    if (!wasJunction) {
      const eng = this.vertexEngine;
      const merged = eng.tryMergeVertex(vid);
      if (!merged) eng.trySnapToLine(vid);
      this.syncFromEngine();
    }
    this.drawVertexBoundaries();

    // Push an undo checkpoint if this gesture actually changed anything --
    // mirrors paint mode's commitStroke() exactly: baseline captured once
    // at gesture start (see _captureVertexBaselineOnce), pushed only if
    // the final state (after the drag AND any merge/snap side effect)
    // actually differs from it.
    if (this._vertexBaselineCaptured) {
      const baselineStr = this._vertexEditBaselineStr;
      this._vertexEditBaselineStr = null;
      this._vertexBaselineCaptured = false;
      if (baselineStr !== JSON.stringify(this._snapshotVertexState())) {
        this.pushVertexUndo(JSON.parse(baselineStr));
      }
    }
  },

  saveVertexBoundaryEdits: function() {
    if (!this.vertexEngine) return;
    this.vertexEngine.saveVertexEdits();
    this.syncFromEngine();
    this.drawVertexBoundaries();
  },

  emitVertexGeojson: function() {
    const geojson = buildAreaGeojson(this.areaRings, this.vertexPos, this.vertexAreaNameKey || 'dfa_name');
    Shiny.setInputValue(
      this.vertexGeojsonInputId || (this.assignmentsInputId + '_vertex_geojson'),
      { geojson: JSON.stringify(geojson), nonce: Date.now() },
      { priority: 'event' }
    );
  },

      currentBrushSize: function() {

        return this.brushSize || 300;
      },

      ensureMap: function() {
        if (this.map) return;

        this.map = L.map(this.mapId, {
          zoomSnap: 0.25,
          preferCanvas: true,
          dragging: false
        });

        const osm = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
          maxZoom: 20,
          attribution: '&copy; OpenStreetMap contributors'
        });

        const esriImagery = L.tileLayer(
          'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
          {
            maxZoom: 20,
            attribution: 'Tiles &copy; Esri'
          }
        );

        const cartoLight = L.tileLayer(
          'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',
          {
            maxZoom: 20,
            subdomains: 'abcd',
            attribution: '&copy; OpenStreetMap contributors &copy; CARTO'
          }
        );

        const topo = L.tileLayer('https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png', {
          maxZoom: 17,
          attribution: 'Map data: &copy; OpenStreetMap contributors, SRTM | Map style: &copy; OpenTopoMap'
        });

        this.baseLayers = {
          'OpenStreetMap': osm,
          'ESRI Satellite': esriImagery,
          'CARTO Light': cartoLight,
          'Topo': topo
        };

        this.currentBaseLayer = osm;
        this.currentBaseLayer.addTo(this.map);

        this.baseControl = L.control.layers(this.baseLayers, null, {
          collapsed: true,
          position: 'topright'
        }).addTo(this.map);

        this.brushPreview = L.circle([0, 0], {
          radius: this.currentBrushSize(),
          color: '#222222',
          weight: 1,
          opacity: 0.7,
          fillOpacity: 0.05,
          interactive: false
        });

        this.map.on('baselayerchange', (e) => {
          this.currentBaseLayer = e.layer;
        });

        const container = this.map.getContainer();

        L.DomEvent.on(container, 'contextmenu', function(e) {
          L.DomEvent.preventDefault(e);
        });

        this.map.on('mousemove', (e) => {
          if (this.mode === 'vertex') { this.onVertexMouseMove(e); return; }
          if (this.brushPreview) {
            this.brushPreview.setLatLng(e.latlng);
            this.brushPreview.setRadius(this.currentBrushSize());
            if (!this.map.hasLayer(this.brushPreview)) {
              this.brushPreview.addTo(this.map);
            }
          }

          if (this.isPainting) {
            this.paintAtLatLng(e.latlng);
          }
        });

        L.DomEvent.on(container, 'mousedown', (e) => {
          if (this.mode === 'vertex') {
            if (e.button === 2) {
              this.isRightPanning = true;
              this.rightPanStart = { x: e.clientX, y: e.clientY };
              L.DomEvent.preventDefault(e);
            }
            return;
          }
          if (e.button === 0) {
            if (!this.gridLayer) return;
            // Snapshot assignments before this stroke begins, so the whole
            // drag (however many cells it touches) undoes as one step.
            this._strokeBaseline = JSON.stringify(this.assignments);
            this.isPainting = true;
            this.paintAtLatLng(this.map.mouseEventToLatLng(e));
            L.DomEvent.preventDefault(e);
            return;
          }

          if (e.button === 2) {
            this.isRightPanning = true;
            this.rightPanStart = { x: e.clientX, y: e.clientY };
            L.DomEvent.preventDefault(e);
          }
        });

        L.DomEvent.on(document, 'mousemove', (e) => {
          if (!this.isRightPanning || !this.rightPanStart) return;

          const dx = e.clientX - this.rightPanStart.x;
          const dy = e.clientY - this.rightPanStart.y;

          this.map.panBy([-dx, -dy], { animate: false });
          this.rightPanStart = { x: e.clientX, y: e.clientY };
          L.DomEvent.preventDefault(e);
        });

        L.DomEvent.on(document, 'mouseup', (e) => {
          if (this.mode === 'vertex') {
            if (e.button === 0) this.onVertexMouseUp();
            if (e.button === 2) { this.isRightPanning = false; this.rightPanStart = null; }
            return;
          }
          if (e.button === 0) {
            this.isPainting = false;
            this.commitStroke();
          }
          if (e.button === 2) {
            this.isRightPanning = false;
            this.rightPanStart = null;
          }
        });

        setTimeout(() => this.map.invalidateSize(), 300);
        setTimeout(() => this.map.invalidateSize(), 900);
      },

      isBoundaryCell: function(id) {
        id = String(id);
        const myDfa = this.assignments[id];
        const nbrs = this.neighbors[id] || [];
        const touchesEdge = !!this.edgeCells[id];

        if (touchesEdge) return true;

        for (let i = 0; i < nbrs.length; i++) {
          const nbrId = String(nbrs[i]);
          if (this.assignments[nbrId] !== myDfa) return true;
        }

        return false;
      },

      fillForDfa: function(dfa) {
        return this.dfaColors[dfa] || '#757575';
      },

      borderColorForDfa: function(dfa) {
        if (dfa === 'Inaccessible') return '#D7301F';
        if (dfa === 'Unpopulated') return '#FFFFFF';
        if (dfa === this.activeDfa) return '#FFD400';
        return '#000000';
      },

      styleForFeature: function(feature) {
        const id = String(feature.properties.cell_id);
        const dfa = this.assignments[id];
        const fillColor = this.fillForDfa(dfa);
        const isBoundary = this.isBoundaryCell(id);
        const isSelectedBoundary = isBoundary && dfa === this.activeDfa;

        let fillOpacity;
        if (this.boundaryOnly) {
          fillOpacity = isSelectedBoundary ? 0.9 : 0.0;
        } else {
          fillOpacity = isSelectedBoundary ? 0.85 : 0.3;
        }

        return {
          stroke: isSelectedBoundary,
          color: this.borderColorForDfa(dfa),
          weight: isSelectedBoundary ? 0.8 : 0,
          opacity: isSelectedBoundary ? 1.0 : 0.0,
          fillColor: fillColor,
          fillOpacity: fillOpacity
        };
      },

      popStyleForFeature: function(feature) {
        return {
          stroke: false,
          fillColor: feature.properties.fill_color || '#000000',
          fillOpacity: 0.5
        };
      },

      frictionStyleForFeature: function(feature) {
        return {
          stroke: false,
          fillColor: feature.properties.fill_color || '#000000',
          fillOpacity: 0.5
        };
      },

      clearFrictionLayer: function() {
        if (!this.map) return;
        if (this.frictionLayer) {
          this.map.removeLayer(this.frictionLayer);
          this.frictionLayer = null;
        }
      },

      buildFrictionLayer: function(frictionGeojson) {
        this.clearFrictionLayer();

        if (!this.map || !frictionGeojson) return;

        const frGeo =
          (typeof frictionGeojson === 'string')
            ? JSON.parse(frictionGeojson)
            : frictionGeojson;

        this.frictionLayer = L.geoJSON(frGeo, {
          style: (feature) => this.frictionStyleForFeature(feature),
          interactive: false
        });
      },

      refreshCellsAndNeighbors: function(cellIds) {
        const toUpdate = {};
        for (let i = 0; i < cellIds.length; i++) {
          const id = String(cellIds[i]);
          toUpdate[id] = true;
          const nbrs = this.neighbors[id] || [];
          for (let j = 0; j < nbrs.length; j++) {
            toUpdate[String(nbrs[j])] = true;
          }
        }

        Object.keys(toUpdate).forEach((id) => {
          if (this.cellLayers[id]) {
            this.cellLayers[id].setStyle(this.styleForFeature(this.cellLayers[id].feature));
          }
        });
      },

      paintCells: function(cellIds) {
        const dfa = this.activeDfa;
        if (!dfa || !cellIds || cellIds.length === 0) return;
        for (let i = 0; i < cellIds.length; i++) {
          const id = String(cellIds[i]);
          this.assignments[id] = dfa;
        }
        this.refreshCellsAndNeighbors(cellIds);
      },

      paintAtLatLng: function(latlng) {
        if (!this.gridLayer) return;
        const brushSize = this.currentBrushSize();
        const touched = [];

        for (const id in this.centroids) {
          const c = this.centroids[id];
          const d = this.map.distance([latlng.lat, latlng.lng], [c.lat, c.lng]);
          if (d <= brushSize) touched.push(id);
        }

        this.paintCells(touched);
      },

      refreshAllStyles: function() {
        if (!this.gridLayer) return;
        this.gridLayer.eachLayer((layer) => {
          layer.setStyle(this.styleForFeature(layer.feature));
        });
      },

      // ── Undo stack management ────────────────────────────────────────────

      pushUndo: function(snapshot) {
        if (!this.undoStack) this.undoStack = [];
        this.undoStack.push(snapshot);
        if (this.undoStack.length > this.maxUndoSteps) this.undoStack.shift();
        this.emitUndoCount();
      },

      // Called on mouseup — closes out whatever stroke was in progress
      // (if any) and pushes one undo entry if it actually changed anything.
      commitStroke: function() {
        if (this._strokeBaseline == null) return;
        const baselineStr = this._strokeBaseline;
        this._strokeBaseline = null;
        if (baselineStr === JSON.stringify(this.assignments)) return; // no-op stroke
        this.pushUndo(JSON.parse(baselineStr));
      },

      undo: function() {
        if (!this.undoStack || this.undoStack.length === 0) return;
        this.assignments = this.undoStack.pop();
        this.refreshAllStyles();
        this.emitUndoCount();
      },

      emitUndoCount: function() {
        if (!this.undoCountInputId || typeof Shiny === 'undefined') return;
        Shiny.setInputValue(
          this.undoCountInputId,
          (this.undoStack || []).length,
          { priority: 'event' }
        );
      },

      clearSeedLayer: function() {
        if (!this.map) return;
        if (this.seedLayer) {
          this.map.removeLayer(this.seedLayer);
          this.seedLayer = null;
        }
      },

      drawSeedPoints: function(seedPoints) {
        this.clearSeedLayer();
        if (!this.map || !seedPoints || !Array.isArray(seedPoints) || seedPoints.length === 0) return;

        this.seedLayer = L.layerGroup();

        seedPoints.forEach((pt) => {
          if (pt.lon == null || pt.lat == null) return;
          // SIA coordination sites — most prominent
          const marker = L.circleMarker([pt.lat, pt.lon], {
            radius: 8,
            color: '#ffffff',
            weight: 2,
            opacity: 1,
            fillColor: '#0d9488',
            fillOpacity: 1,
            interactive: false
          });

          marker.bindTooltip(String(pt.dfa_name || ''), {
            permanent: true,
            direction: 'right',
            offset: [10, 0],
            className: 'dfa-tooltip'
          });

          marker.addTo(this.seedLayer);
        });

        this.seedLayer.addTo(this.map);
        this.bringPointLayersToFront();
      },

      clearFacilityLayer: function() {
        if (!this.map) return;
        if (this.facilityLayer) {
          this.map.removeLayer(this.facilityLayer);
          this.facilityLayer = null;
        }
      },

      clearLandmarkLayer: function() {
        if (!this.map) return;
        if (this.landmarkLayer) {
          this.map.removeLayer(this.landmarkLayer);
          this.landmarkLayer = null;
        }
      },

      drawFacilityPoints: function(facilityPoints) {
        this.clearFacilityLayer();
        if (!this.map || !facilityPoints || !Array.isArray(facilityPoints) || facilityPoints.length === 0) return;

        this.facilityLayer = L.layerGroup();

        facilityPoints.forEach((pt) => {
          if (pt.lon == null || pt.lat == null) return;
          // Non-SIA facilities — equal prominence to landmarks
          const marker = L.circleMarker([pt.lat, pt.lon], {
            radius: 5,
            color: '#ffffff',
            weight: 1.5,
            opacity: 1,
            fillColor: '#d95f0e',
            fillOpacity: 0.9,
            interactive: false
          });

          if (pt.name) {
            marker.bindTooltip(String(pt.name), {
              permanent: true,
              direction: 'right',
              offset: [6, 0],
              className: 'hf-label'
            });
          }

          marker.addTo(this.facilityLayer);
        });

        this.facilityLayer.addTo(this.map);
      },

      drawLandmarkPoints: function(landmarkPoints) {
        this.clearLandmarkLayer();
        if (!this.map || !landmarkPoints || !Array.isArray(landmarkPoints) || landmarkPoints.length === 0) return;

        this.landmarkLayer = L.layerGroup();

        landmarkPoints.forEach((pt) => {
          if (pt.lon == null || pt.lat == null) return;
          // Landmarks — equal prominence to non-SIA facilities
          const marker = L.circleMarker([pt.lat, pt.lon], {
            radius: 5,
            color: '#ffffff',
            weight: 1.5,
            opacity: 1,
            fillColor: '#7c3aed',
            fillOpacity: 0.9,
            interactive: false
          });

          if (pt.name) {
            marker.bindTooltip(String(pt.name), {
              permanent: true,
              direction: 'right',
              offset: [6, 0],
              className: 'landmark-label'
            });
          }

          marker.addTo(this.landmarkLayer);
        });

        this.landmarkLayer.addTo(this.map);
      },

      // Bring all point layers to front in correct order:
      // landmarks → facilities → seeds (seeds always on top)
      bringSubdivisionToFront: function() {
        if (this.subdivisionLayer && this.map.hasLayer(this.subdivisionLayer))
          this.subdivisionLayer.bringToFront();
      },

      bringPointLayersToFront: function() {
        if (this.landmarkLayer) {
          this.landmarkLayer.eachLayer(function(l) { if (l.bringToFront) l.bringToFront(); });
        }
        if (this.facilityLayer) {
          this.facilityLayer.eachLayer(function(l) { if (l.bringToFront) l.bringToFront(); });
        }
        if (this.seedLayer) {
          this.seedLayer.eachLayer(function(l) { if (l.bringToFront) l.bringToFront(); });
        }
      },

      clearScene: function() {
        if (!this.map) return;

        if (this.districtLayer) {
          this.map.removeLayer(this.districtLayer);
          this.districtLayer = null;
        }
        if (this.seedLayer) {
          this.map.removeLayer(this.seedLayer);
          this.seedLayer = null;
        }
        if (this.facilityLayer) {
          this.map.removeLayer(this.facilityLayer);
          this.facilityLayer = null;
        }
        if (this.landmarkLayer) {
          this.map.removeLayer(this.landmarkLayer);
          this.landmarkLayer = null;
        }
        if (this.popLayer) {
          this.map.removeLayer(this.popLayer);
          this.popLayer = null;
        }
        if (this.frictionLayer) {
          this.map.removeLayer(this.frictionLayer);
          this.frictionLayer = null;
        }
        if (this.subdivisionLayer) {
          this.map.removeLayer(this.subdivisionLayer);
          this.subdivisionLayer = null;
        }
        if (this.gridLayer) {
          this.map.removeLayer(this.gridLayer);
          this.gridLayer = null;
        }
        if (this.savedLayer) {
          this.map.removeLayer(this.savedLayer);
          this.savedLayer = null;
        }

        this.cellLayers = {};
        this.centroids = {};
        this.neighbors = {};
        this.edgeCells = {};
      },

      setPopulationVisibility: function(showIt) {
        if (!this.map || !this.popLayer) return;
        if (showIt) {
          if (!this.map.hasLayer(this.popLayer)) this.popLayer.addTo(this.map);
        } else {
          if (this.map.hasLayer(this.popLayer)) this.map.removeLayer(this.popLayer);
        }
        this.bringPointLayersToFront();
      },

      setFrictionVisibility: function(showIt) {
        if (!this.map || !this.frictionLayer) return;

        if (showIt) {
          if (!this.map.hasLayer(this.frictionLayer)) {
            this.frictionLayer.addTo(this.map);
          }
          if (this.frictionLayer.bringToBack) this.frictionLayer.bringToBack();
        } else {
          if (this.map.hasLayer(this.frictionLayer)) {
            this.map.removeLayer(this.frictionLayer);
          }
        }

        if (this.popLayer && this.map.hasLayer(this.popLayer) && this.popLayer.bringToFront) {
          this.popLayer.bringToFront();
        }
        if (this.gridLayer && this.gridLayer.bringToFront) {
          this.gridLayer.bringToFront();
        }
        if (this.savedLayer && this.savedLayer.bringToFront) {
          this.savedLayer.bringToFront();
        }
        this.bringPointLayersToFront();
      },

      setBrushSize: function(v) {
        this.brushSize = v;
        if (this.brushPreview) this.brushPreview.setRadius(v);
      },

      setBoundaryOnly: function(v) {
        this.boundaryOnly = !!v;
        this.refreshAllStyles();
      },

      setColorsAndActive: function(colorsObj, activeDfa) {
        this.dfaColors = colorsObj || {};
        this.activeDfa = activeDfa || null;
        this.refreshAllStyles();

        if (this.savedLayer) {
          this.savedLayer.eachLayer((layer) => {
            const nm = layer.feature.properties && layer.feature.properties.dfa_name;
            layer.setStyle({
              color: this.borderColorForDfa(nm),
              weight: 2.5,
              opacity: 1,
              dashArray: null,
              fill: false
            });
          });
        }
      },

      loadScene: function(msg) {
        this.ensureMap();
        this.clearScene();

        this.initialAssignments = JSON.parse(JSON.stringify(msg.initialAssignments));
        this.assignments = JSON.parse(JSON.stringify(msg.initialAssignments));
        this.dfaColors = msg.dfaColors || {};
        this.activeDfa = msg.activeDfa || null;
        this.neighbors = msg.neighbors || {};
        this.edgeCells = msg.edgeCells || {};
        this.brushSize = msg.brushSize || this.brushSize;
        this.boundaryOnly = !!msg.boundaryOnly;

        // A new scene starts with a clean undo history — undoing past a
        // freshly loaded/restored scene wouldn't make sense.
        this.undoStack = [];
        this._strokeBaseline = null;
        if (msg.undoCountInputId) this.undoCountInputId = msg.undoCountInputId;
        this.emitUndoCount();

        if (this.brushPreview) this.brushPreview.setRadius(this.brushSize);

        const districtGeo = (typeof msg.districtGeojson === 'string') ? JSON.parse(msg.districtGeojson) : msg.districtGeojson;
        // Stashed for vertex mode's boundary snap (see boundary_snap.js) --
        // the outer edge traced from painted cells is only a grid-staircase
        // approximation of this true polygon.
        this._lastDistrictGeojson = districtGeo;
        const gridGeo = (typeof msg.gridGeojson === 'string') ? JSON.parse(msg.gridGeojson) : msg.gridGeojson;

        this.districtLayer = L.geoJSON(districtGeo, {
          style: {
            color: '#000000',
            weight: 2,
            fill: false,
            opacity: 1
          }
        }).addTo(this.map);

        if (msg.popGeojson) {
          const popGeo = (typeof msg.popGeojson === 'string') ? JSON.parse(msg.popGeojson) : msg.popGeojson;
          this.popLayer = L.geoJSON(popGeo, {
            style: (feature) => this.popStyleForFeature(feature),
            interactive: false
          });
          if (msg.showPop) this.popLayer.addTo(this.map);
        }

        if (msg.frictionGeojson) {
          this.buildFrictionLayer(msg.frictionGeojson);
          if (msg.showFriction) {
            this.setFrictionVisibility(true);
          }
        }

        if (msg.subdivisionGeojson) {
          console.log('[paint] subdivisionGeojson received, length:', 
            typeof msg.subdivisionGeojson === 'string' ? msg.subdivisionGeojson.length : 'object');
          const subdivGeo = (typeof msg.subdivisionGeojson === 'string')
            ? JSON.parse(msg.subdivisionGeojson)
            : msg.subdivisionGeojson;
          console.log('[paint] subdivGeo features:', subdivGeo && subdivGeo.features ? subdivGeo.features.length : 'none');
          this.subdivisionLayer = L.geoJSON(subdivGeo, {
            style: {
              color: '#7c3aed',
              weight: 2,
              dashArray: '6 4',
              fill: false,
              opacity: 0.85,
              interactive: false
            }
          }).addTo(this.map);
        }

        this.gridLayer = L.geoJSON(gridGeo, {
          style: (feature) => this.styleForFeature(feature),
          onEachFeature: (feature, layer) => {
            const id = String(feature.properties.cell_id);
            this.cellLayers[id] = layer;
            this.centroids[id] = {
              lng: feature.properties.centroid_lon,
              lat: feature.properties.centroid_lat
            };
            layer.on('click', (e) => this.paintAtLatLng(e.latlng));
          }
        }).addTo(this.map);

        if (msg.savedGeojson) {
          const savedGeo = (typeof msg.savedGeojson === 'string') ? JSON.parse(msg.savedGeojson) : msg.savedGeojson;
          this.savedLayer = L.geoJSON(savedGeo, {
            style: (feature) => {
              const nm = feature.properties && feature.properties.dfa_name;
              return {
                color: this.borderColorForDfa(nm),
                weight: 2.5,
                opacity: 1,
                dashArray: null,
                fill: false
              };
            }
          }).addTo(this.map);
        }

        if (this.popLayer && msg.showPop && this.popLayer.bringToFront) {
          this.popLayer.bringToFront();
        }
        if (this.gridLayer && this.gridLayer.bringToFront) {
          this.gridLayer.bringToFront();
        }
        if (this.savedLayer && this.savedLayer.bringToFront) {
          this.savedLayer.bringToFront();
        }
        // Subdivision boundaries sit above grid/saved but below point markers
        this.bringSubdivisionToFront();

        this.drawSeedPoints(msg.seedPoints || []);
        this.drawFacilityPoints(msg.facilityPoints || []);
        this.drawLandmarkPoints(msg.landmarkPoints || []);
        this.bringPointLayersToFront();
        this.map.fitBounds(this.districtLayer.getBounds(), { padding: [10, 10] });

        setTimeout(() => {
          this.map.invalidateSize();
          if (this.frictionLayer && msg.showFriction) {
            this.setFrictionVisibility(true);
          }
          if (this.popLayer && msg.showPop && this.popLayer.bringToFront) {
            this.popLayer.bringToFront();
          }
          if (this.gridLayer && this.gridLayer.bringToFront) {
            this.gridLayer.bringToFront();
          }
          if (this.savedLayer && this.savedLayer.bringToFront) {
            this.savedLayer.bringToFront();
          }
          this.bringSubdivisionToFront();
          this.bringPointLayersToFront();
        }, 150);

        Shiny.setInputValue(this.readyInputId, Date.now(), { priority: 'event' });

        // A scene reload can be triggered by things entirely unrelated to
        // vertex mode (tab switches, restores, various other observers on
        // the R side calling send_current_scene()) -- loadScene() itself
        // has no notion of vertex mode and will happily re-show every
        // paint-mode layer it just (re)built. If a reload happens to land
        // while vertex mode is still active, immediately re-hide them so
        // the refinement view stays undisturbed rather than the grid
        // reappearing underneath it.
        if (this.mode === 'vertex') {
          this._hidePaintLayersForVertexMode();
          this.ensureVertexLayerGroup();
        }
      },

      resetAssignments: function() {
        // Reset is itself an undoable action — snapshot the current state
        // before reverting to auto-generated, unless nothing would change.
        const beforeStr = JSON.stringify(this.assignments);
        if (beforeStr !== JSON.stringify(this.initialAssignments)) {
          this.pushUndo(JSON.parse(beforeStr));
        }
        this.assignments = JSON.parse(JSON.stringify(this.initialAssignments));
        this.refreshAllStyles();
        if (this.savedLayer) {
          this.map.removeLayer(this.savedLayer);
          this.savedLayer = null;
        }
      },

      emitAssignments: function() {
        Shiny.setInputValue(
          this.assignmentsInputId,
          { assignments: this.assignments, nonce: Date.now() },
          { priority: 'event' }
        );
      },

      showSaved: function(geojsonText) {
        if (!this.map) return;

        if (this.savedLayer) {
          this.map.removeLayer(this.savedLayer);
          this.savedLayer = null;
        }

        if (!geojsonText) {
          this.bringPointLayersToFront();
          return;
        }

        const gj = (typeof geojsonText === 'string') ? JSON.parse(geojsonText) : geojsonText;
        this.savedLayer = L.geoJSON(gj, {
          style: (feature) => {
            const nm = feature.properties && feature.properties.dfa_name;
            return {
              color: this.borderColorForDfa(nm),
              weight: 2.5,
              opacity: 1,
              dashArray: null,
              fill: false
            };
          }
        }).addTo(this.map);

        this.bringPointLayersToFront();
      }
    };
  }

  document.addEventListener('DOMContentLoaded', function() {
    if (!window.Shiny) return;

    Shiny.addCustomMessageHandler('show_loading', function(msg) {
      showLoading(msg);
    });

    Shiny.addCustomMessageHandler('hide_loading', function(msg) {
      hideLoading(msg);
    });

    Shiny.addCustomMessageHandler('paint_load_scene', function(msg) {
      getApp(msg).loadScene(msg);
    });

    Shiny.addCustomMessageHandler('paint_reset', function(msg) {
      getApp(msg).resetAssignments();
    });

    Shiny.addCustomMessageHandler('paint_undo', function(msg) {
      getApp(msg).undo();
    });

    Shiny.addCustomMessageHandler('paint_enter_vertex_mode', function(msg) {
      getApp(msg).enterVertexMode({ smoothness: msg.smoothness, stiffness: msg.stiffness });
    });

    Shiny.addCustomMessageHandler('paint_exit_vertex_mode', function(msg) {
      getApp(msg).exitVertexMode();
    });

    Shiny.addCustomMessageHandler('paint_set_vertex_smoothness', function(msg) {
      getApp(msg).setVertexSmoothness(msg.value);
    });

    Shiny.addCustomMessageHandler('paint_set_vertex_stiffness', function(msg) {
      getApp(msg).setVertexStiffness(msg.value);
    });

    Shiny.addCustomMessageHandler('paint_save_vertex_edits', function(msg) {
      getApp(msg).saveVertexBoundaryEdits();
    });

    Shiny.addCustomMessageHandler('paint_request_vertex_geojson', function(msg) {
      getApp(msg).emitVertexGeojson();
    });

    Shiny.addCustomMessageHandler('paint_vertex_undo', function(msg) {
      getApp(msg).vertexUndo();
    });

    Shiny.addCustomMessageHandler('paint_reset_vertex_edits', function(msg) {
      getApp(msg).resetVertexEdits();
    });

    Shiny.addCustomMessageHandler('paint_request_assignments', function(msg) {
      getApp(msg).emitAssignments();
    });

    Shiny.addCustomMessageHandler('paint_show_saved', function(msg) {
      getApp(msg).showSaved(msg.geojson);
    });

    Shiny.addCustomMessageHandler('paint_toggle_population', function(msg) {
      var app = getApp(msg);
      // Build layer lazily if geojson provided and layer not yet built
      if (msg.geojson && !app.popLayer) {
        var popGeo = (typeof msg.geojson === 'string') ? JSON.parse(msg.geojson) : msg.geojson;
        app.popLayer = L.geoJSON(popGeo, {
          style: function(f) {
            return { fillColor: f.properties.fill_color, fillOpacity: 0.5, weight: 0, stroke: false };
          }
        });
      }
      app.setPopulationVisibility(!!msg.show);
    });

    Shiny.addCustomMessageHandler('paint_toggle_friction', function(msg) {
      var app = getApp(msg);
      // Build layer lazily if geojson provided and layer not yet built
      if (msg.geojson && !app.frictionLayer) {
        app.buildFrictionLayer(msg.geojson);
      }
      app.setFrictionVisibility(!!msg.show);
    });

    Shiny.addCustomMessageHandler('paint_set_brush', function(msg) {
      getApp(msg).setBrushSize(msg.value);
    });

    Shiny.addCustomMessageHandler('paint_set_boundary_only', function(msg) {
      getApp(msg).setBoundaryOnly(msg.value);
    });

    Shiny.addCustomMessageHandler('paint_set_colors', function(msg) {
      getApp(msg).setColorsAndActive(msg.colors, msg.activeDfa);
    });
  });
})();
