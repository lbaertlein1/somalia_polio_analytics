(function() {
  window.paintApps = window.paintApps || {};

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
      map: null,
      districtLayer: null,
      popLayer: null,
      gridLayer: null,
      savedLayer: null,
      seedLayer: null,
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
          if (e.button === 0) {
            if (!this.gridLayer) return;
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
          if (e.button === 0) this.isPainting = false;
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
          fillOpacity: 0.35
        };
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

      clearSeedLayer: function() {
        if (!this.map) return;
        if (this.seedLayer) {
          this.map.removeLayer(this.seedLayer);
          this.seedLayer = null;
        }
      },

      bringSeedPointsToFront: function() {
        if (!this.seedLayer) return;
        this.seedLayer.eachLayer(function(layer) {
          if (layer && layer.bringToFront) layer.bringToFront();
        });
      },

      drawSeedPoints: function(seedPoints) {
        this.clearSeedLayer();
        if (!this.map || !seedPoints || !Array.isArray(seedPoints) || seedPoints.length === 0) return;

        this.seedLayer = L.layerGroup();

        seedPoints.forEach((pt) => {
          if (pt.lon == null || pt.lat == null) return;
          const marker = L.circleMarker([pt.lat, pt.lon], {
            radius: 4,
            color: '#000000',
            weight: 1,
            opacity: 1,
            fillColor: '#000000',
            fillOpacity: 1,
            interactive: false
          });

          marker.bindTooltip(String(pt.dfa_name || ''), {
            permanent: true,
            direction: 'right',
            offset: [8, 0],
            className: 'dfa-tooltip'
          });

          marker.addTo(this.seedLayer);
        });

        this.seedLayer.addTo(this.map);
        this.bringSeedPointsToFront();
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
        if (this.popLayer) {
          this.map.removeLayer(this.popLayer);
          this.popLayer = null;
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
        this.bringSeedPointsToFront();
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

        if (this.brushPreview) this.brushPreview.setRadius(this.brushSize);

        const districtGeo = (typeof msg.districtGeojson === 'string') ? JSON.parse(msg.districtGeojson) : msg.districtGeojson;
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

        this.drawSeedPoints(msg.seedPoints || []);
        this.bringSeedPointsToFront();
        this.map.fitBounds(this.districtLayer.getBounds(), { padding: [10, 10] });

        setTimeout(() => {
          this.map.invalidateSize();
          this.bringSeedPointsToFront();
        }, 150);

        Shiny.setInputValue(this.readyInputId, Date.now(), { priority: 'event' });
      },

      resetAssignments: function() {
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
          this.bringSeedPointsToFront();
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

        this.bringSeedPointsToFront();
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

    Shiny.addCustomMessageHandler('paint_request_assignments', function(msg) {
      getApp(msg).emitAssignments();
    });

    Shiny.addCustomMessageHandler('paint_show_saved', function(msg) {
      getApp(msg).showSaved(msg.geojson);
    });

    Shiny.addCustomMessageHandler('paint_toggle_population', function(msg) {
      getApp(msg).setPopulationVisibility(!!msg.show);
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
