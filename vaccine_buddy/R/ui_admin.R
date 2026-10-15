# =============================================================================
# ui_admin.R -- admin app, in the health area adjuster's layout and look
# (www/styles.css, header bar with tab buttons, grey left sidebar), behind
# the adjuster-style login screen (mod_auth.R) with this app's own accounts.
#
# A tool for quickly getting missed places to supervisors during a
# campaign, for users with little training -- not an analysis dashboard.
# Everything is per campaign + district (selected top right), in three
# steps, each leading to the next:
#   1 Find missed places   dates -> find missed squares -> paint squares on/off
#                          -> split into places -> check map and list -> save
#   2 Check supervisors    who gets each health area's list -> continue
#   3 Send                 WhatsApp click-to-send per supervisor, live follow-up
# Messages are prepared automatically for the newest saved list.
# Coverage figures (share visited, population reached) are deliberately
# never shown; only missed places and the approximate children in each.
# Admin page (admins only): users, each campaign's teams (which health area
# each belongs to, or none, or ignored) and three analysis settings.
#
# Districts with no health areas drawn in the adjuster use health areas
# taken from the team names (infer_health_areas()); their maps show the
# district outline but no health area boundaries.
# =============================================================================

# styles.css hides DataTables paging/search chrome (as in the adjuster), so
# tables show every row in a scroll box instead of pages.
DT_OPTS <- list(paging = FALSE, scrollY = '40vh', scrollX = TRUE, dom = 't')

# "ELDAS HC T3 (60 m away)" -- the vaccination team whose GPS track came
# closest to a missed place (or its mapped team area, where there is one)
.team_closest <- function(pl) {
  ifelse(pl$team_source %in% 'nearest carrier' & !is.na(pl$team_dist_m),
         sprintf('%s (%s m away)', team_label(pl$assigned_team), format(round(pl$team_dist_m), big.mark = ',')),
         team_label(pl$assigned_team))
}

# A button in a table row or map popup that sends `input` = {i, nonce}
# without also selecting the row
.row_btn <- function(input, i, label) {
  sprintf('<button class="btn btn-default btn-xs" onclick="event.stopPropagation(); Shiny.setInputValue(\'%s\', {i: %d, nonce: Math.random()}, {priority: \'event\'})">%s</button>',
          input, i, label)
}

# The analysis settings admins can change (Admin page): setting = label.
# Everything else in DEFAULT_PARAMS is fixed.
PARAM_UI <- c(
  min_place_u5       = 'Only list missed places with at least this many children',
  max_place_children = 'Split missed areas with more than this many children',
  no_map_reach_m     = 'Districts with no health areas drawn: look for missed places this far from team tracks (m)'
)

# A team's role, as shown on the Admin page
.role_label <- function(role, health_area) {
  ifelse(role == 'ignore', 'Ignored – not a vaccination team',
         ifelse(role == 'no_area', 'Not part of a health area',
                ifelse(is.na(health_area), 'Not matched – where it worked most', health_area)))
}


admin_ui <- function() {
  # One tab = sidebar (first column, grey in styles.css) + main panel
  tab_layout <- function(sidebar, ...) fluidRow(column(3, div(style = 'padding: 12px 14px;', sidebar)), column(9, div(style = 'padding: 12px 16px;', ...)))
  step <- function(icon_name, title, ...) div(class = 'step', div(class = 'step-h', span(class = 'step-n', icon(icon_name)), title), ...)
  # A section folded away until clicked
  more <- function(title, ...) tags$details(class = 'more', tags$summary(title), div(style = 'margin-top: 6px;', ...))

  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
      tags$style(HTML("
        #header-right .form-group { margin: 0; }
        #header-right .hdr-sel { display: flex; flex-direction: column; }
        #header-right .hdr-sel span { font-size: 10px; color: #94a3b8; line-height: 1.2; }
        #header-right select { height: 26px; font-size: 12px; padding: 1px 6px; background: #273549; color: #f1f5f9; border: 1px solid #334155; }
        .step { margin-bottom: 16px; }
        .step-h { font-size: 13px; font-weight: 600; color: #0f172a; margin-bottom: 6px; display: flex; align-items: center; gap: 8px; }
        .step-n { display: inline-flex; width: 22px; height: 22px; border-radius: 50%; background: #0d9488; color: #fff;
                  font-size: 11px; align-items: center; justify-content: center; flex: none; }
        .hint { font-size: 12px; color: #475569; margin: 0 0 6px; }
        .summary { font-size: 14px; color: #0f172a; margin: 0 0 10px; }
        .summary .muted { color: #64748b; font-size: 12px; display: block; margin-top: 2px; }
        .track-bar { display: flex; flex-wrap: wrap; align-items: center; gap: 14px; margin-bottom: 6px; font-size: 12px; }
        .track-bar .shiny-input-container, .track-bar .checkbox { margin: 0; }
        .track-bar .radio-inline { padding: 3px 10px; border: 1px solid #cbd5e1; border-radius: 14px; margin: 0 4px 0 0; background: #fff; }
        .track-bar .radio-inline input { display: none; }
        .track-bar .radio-inline:has(input:checked) { background: #0d9488; border-color: #0d9488; color: #fff; }
        .brush-size { display: flex; align-items: center; gap: 4px; color: #64748b; font-size: 11px; }
        .brush-size .irs { margin-top: -14px; }
        .msg-preview { white-space: pre-wrap; font-family: inherit; font-size: 12px; background: #f8fafc; border: 1px solid #e2e8f0; }
        .data-box { padding: 8px; background: #fff; border: 1px solid #e2e8f0; border-radius: 4px; margin-bottom: 8px; }
        .callout { padding: 8px; background: #fffbeb; border: 1px solid #fcd34d; border-radius: 4px; margin-bottom: 8px; font-size: 12px; }
        .unsaved { font-size: 12px; color: #b45309; margin-bottom: 6px; }
        .mm-cell { display: flex; align-items: center; gap: 4px; }
        .mm-cell .mm-link { width: 190px; height: 24px; padding: 2px 6px; font-size: 11px; }
        .msg-panel { background: #fff; border: 1px solid #e2e8f0; border-radius: 4px; padding: 10px; position: sticky; top: 10px; }
        .export-panel { max-width: 640px; }
        details.more { margin: 8px 0; font-size: 12px; }
        details.more summary { cursor: pointer; color: #0f766e; font-weight: 600; }
        .set-default { font-size: 10px; color: #b45309; margin-top: -10px; margin-bottom: 6px; }
        .adding.leaflet-container, .adding .leaflet-interactive { cursor: crosshair !important; }
        .add-hint { font-size: 12px; background: #ecfeff; border: 1px solid #67e8f9; border-radius: 4px; padding: 6px 10px; margin-bottom: 6px; }
        #admin_tabs { margin-bottom: 12px; }
      ")),
      tags$script(HTML("
        $(document).on('click', '.hdr-tab', function() {
          $('#main_tabs a[data-value=\"' + $(this).data('tab') + '\"]').tab('show');
        });
        $(document).on('shown.bs.tab', '#main_tabs a[data-toggle=\"tab\"]', function(e) {
          var val = $(e.target).data('value');
          $('.hdr-tab').removeClass('hdr-tab-active');
          $('.hdr-tab[data-tab=\"' + val + '\"]').addClass('hdr-tab-active');
          setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 150);
        });
        // A file made by the server (a health area's My Maps file), saved by the browser
        Shiny.addCustomMessageHandler('save_text', function(m) {
          var a = document.createElement('a');
          a.href = URL.createObjectURL(new Blob([m.text], {type: m.type}));
          a.download = m.name; document.body.appendChild(a); a.click();
          setTimeout(function() { URL.revokeObjectURL(a.href); a.remove(); }, 1000);
        });
        // Missed places back on top of the Find map's other shapes
        Shiny.addCustomMessageHandler('places_front', function(m) {
          var w = HTMLWidgets.find('#an_map'); if (!w || !w.getMap()) return;
          var g = w.getMap().layerManager.getLayerGroup('places', false);
          if (g) g.eachLayer(function(l) { if (l.bringToFront) l.bringToFront(); });
        });
        // Painting missed places on the Find map, on the analysis's own 100 m
        // grid (mechanics as the health area adjuster's paint-app.js): left
        // button paints the grid cell under the brush, right button drags
        // the map; on release the stroke's cells go to input$paint_cells.
        // The grid comes from the server (m.grid: its UTM extent and a local
        // affine lon/lat <-> UTM fit). State lives on each map object, since
        // the map is redrawn when the district changes.
        Shiny.addCustomMessageHandler('paint_mode', function(m) {
          var w = HTMLWidgets.find('#an_map'); if (!w || !w.getMap()) return;
          var map = w.getMap();
          var P = map._vbPaint;
          if (!P) {
            P = map._vbPaint = {on: false, down: false, cells: {}, stroke: null, pending: [], last: null, pan: null,
                                brush: L.circle([0, 0], {radius: 50, color: '#222', weight: 1, opacity: 0.7, fillOpacity: 0.05, interactive: false})};
            var toLL = function(x, y) { var g = P.g, dx = x - g.x0, dy = y - g.y0; return [g.lat0 + g.c * dx + g.d * dy, g.lon0 + g.a * dx + g.b * dy]; };
            var toXY = function(ll) { var g = P.g, dl = ll.lng - g.lon0, dp = ll.lat - g.lat0;
                                      return [g.x0 + g.ia * dl + g.ib * dp, g.y0 + g.ic * dl + g.id * dp]; };
            // the cell under the brush centre, and every cell whose centre
            // is under the brush (P.size: its diameter in metres)
            var paintXY = function(x, y) {
              var g = P.g, rad = (P.size || g.cs) / 2, n = Math.ceil(rad / g.cs);
              var c0 = Math.floor((x - g.xmin) / g.cs), r0 = Math.floor((g.ymax - y) / g.cs);
              paintCell(c0, r0);
              for (var dr = -n; dr <= n; dr++) for (var dc = -n; dc <= n; dc++) {
                var cx = g.xmin + (c0 + dc + 0.5) * g.cs, cy = g.ymax - (r0 + dr + 0.5) * g.cs;
                if (Math.hypot(cx - x, cy - y) <= rad) paintCell(c0 + dc, r0 + dr);
              }
            };
            var paintCell = function(col, row) {
              var g = P.g;
              if (col < 0 || row < 0 || col >= g.ncol || row >= g.nrow) return;
              var k = row * g.ncol + col + 1;   // terra cell number
              if (P.cells[k]) return; P.cells[k] = true;
              var x0 = g.xmin + col * g.cs, y1 = g.ymax - row * g.cs;
              L.polygon([toLL(x0, y1), toLL(x0 + g.cs, y1), toLL(x0 + g.cs, y1 - g.cs), toLL(x0, y1 - g.cs)],
                        {stroke: false, fillColor: P.erase ? '#64748b' : '#dc2626', fillOpacity: 0.55, interactive: false}).addTo(P.stroke);
            };
            // every cell along the way, however fast the mouse moves
            var paintTo = function(ll) {
              var p = toXY(ll), q = P.last || p, n = Math.max(1, Math.ceil(Math.hypot(p[0] - q[0], p[1] - q[1]) / (P.g.cs / 4)));
              for (var i = 1; i <= n; i++) paintXY(q[0] + (p[0] - q[0]) * i / n, q[1] + (p[1] - q[1]) * i / n);
              P.last = p;
            };
            var box = map.getContainer();
            L.DomEvent.on(box, 'contextmenu', function(e) { if (P.on) L.DomEvent.preventDefault(e); });
            L.DomEvent.on(box, 'mousedown', function(e) {
              if (!P.on || !P.g) return;
              if (e.button === 0) {
                P.down = true; P.cells = {}; P.last = null; P.stroke = L.layerGroup().addTo(map);
                paintTo(map.mouseEventToLatLng(e)); L.DomEvent.preventDefault(e);
              } else if (e.button === 2) { P.pan = {x: e.clientX, y: e.clientY}; L.DomEvent.preventDefault(e); }
            });
            map.on('mousemove', function(e) {
              if (!P.on) return;
              P.brush.setLatLng(e.latlng); if (!map.hasLayer(P.brush)) P.brush.addTo(map);
              if (P.down) paintTo(e.latlng);
            });
            L.DomEvent.on(document, 'mousemove', function(e) {
              if (!P.pan) return;
              map.panBy([P.pan.x - e.clientX, P.pan.y - e.clientY], {animate: false}); P.pan = {x: e.clientX, y: e.clientY};
            });
            L.DomEvent.on(document, 'mouseup', function(e) {
              if (e.button === 2) { P.pan = null; return; }
              if (e.button !== 0 || !P.down) return;
              P.down = false;
              var ks = Object.keys(P.cells).map(Number);
              if (!ks.length) { map.removeLayer(P.stroke); return; }
              P.pending.push(P.stroke);   // shown until the server has added the place
              Shiny.setInputValue('paint_cells', {cells: ks, nonce: Math.random()}, {priority: 'event'});
            });
          }
          if (m.clear) { var s = P.pending.shift(); if (s) map.removeLayer(s); return; }
          if ('erase' in m) P.erase = m.erase;   // remove squares instead of adding them
          if (m.size) { P.size = m.size; P.brush.setRadius(m.size / 2); }
          if (m.on === undefined) return;
          P.on = m.on; if (m.grid) P.g = m.grid;
          if (m.on) { map.dragging.disable(); map.doubleClickZoom.disable(); }
          else {
            map.dragging.enable(); map.doubleClickZoom.enable(); P.down = false; P.pan = null;
            map.removeLayer(P.brush); P.pending.forEach(function(s) { map.removeLayer(s); }); P.pending = [];
          }
        });
      "))
    ),

    authUI('auth'),

    shinyjs::hidden(div(
      id = 'main_app',
      div(id = 'app-header',
          div(id = 'header-title',
              tags$span('Vaccine Buddy Follow-up', style = 'font-size: 14px; line-height: 1;'),
              tags$span('Missed places for health area supervisors', style = 'color: #0d9488; font-size: 14px; line-height: 1;')),
          div(id = 'header-tabs',
              tags$button(class = 'hdr-tab hdr-tab-active', `data-tab` = 'find', '1  Find missed places'),
              tags$button(class = 'hdr-tab', `data-tab` = 'supervisors', '2  Check supervisors'),
              tags$button(class = 'hdr-tab', `data-tab` = 'send', '3  Send to supervisors')),
          div(id = 'header-right',
              div(class = 'hdr-sel', span('Campaign'), selectInput('campaign', NULL, choices = NULL, width = '200px')),
              div(class = 'hdr-sel', span('District'), selectInput('district', NULL, choices = NULL, width = '160px')),
              shinyjs::hidden(tags$button(id = 'hdr_admin', class = 'hdr-tab', `data-tab` = 'admin',
                                          style = 'align-self: center; border-radius: 4px; padding: 5px 12px;', icon('gear'), ' Admin')),
              uiOutput('who', inline = TRUE))),

      tabsetPanel(
        id = 'main_tabs', type = 'tabs',

        tabPanel('Find missed places', value = 'find', tab_layout(
          sidebar = tagList(
            step('calendar', 'Days to include',
                 dateRangeInput('dates', NULL, start = Sys.Date() - 3, end = Sys.Date(), width = '100%'),
                 div(class = 'data-box', uiOutput('sync_status'),
                     actionButton('sync_now', 'Check for new data', class = 'btn btn-default', width = '100%',
                                  icon = icon('rotate'), style = 'margin-top: 6px;'))),
            step('magnifying-glass', 'Find missed areas',
                 p(class = 'hint', 'Squares (100 m) where children live that no vaccination team passed.'),
                 actionButton('run', 'Find missed areas', class = 'btn btn-primary', width = '100%', icon = icon('magnifying-glass'))),
            step('paintbrush', 'Check and adjust',
                 p(class = 'hint', 'With the brush above the map, add squares the teams missed, or remove ones no one should ',
                   'go back to (a market, a compound with no children). Do this before splitting.')),
            step('scissors', 'Split into missed places',
                 p(class = 'hint', 'Groups the squares into places a team can walk, each with a description and its team.'),
                 shinyjs::disabled(actionButton('split', 'Split into missed places', class = 'btn btn-primary', width = '100%',
                                                icon = icon('scissors')))),
            step('check', 'Check, then save',
                 p(class = 'hint', 'Look over the places on the map and in the list, then save them so they can be sent.'),
                 shinyjs::disabled(actionButton('save_run', 'Save and check supervisors', class = 'btn btn-success',
                                                width = '100%', icon = icon('arrow-right'))))
          ),
          uiOutput('an_summary'),
          div(class = 'track-bar',
              div(style = 'display: flex; align-items: center; gap: 6px;', tags$b('Health area:'),
                  selectInput('ha_filter', NULL, choices = c('All health areas' = 'all'), width = '240px')),
              checkboxInput('trk_show', 'Show team GPS tracks', value = FALSE),
              checkboxInput('u5_show', 'Show where children live (WorldPop under 5)', value = FALSE),
              shinyjs::hidden(div(id = 'brush_box', style = 'display: flex; align-items: center; gap: 6px;', tags$b('Brush:'),
                  radioButtons('brush', NULL, inline = TRUE, selected = 'off',
                               choices = c('Off – move the map' = 'off', 'Add squares' = 'add', 'Remove squares' = 'remove')),
                  div(class = 'brush-size', span('Size'),
                      sliderInput('brush_m', NULL, min = 100, max = 1000, step = 100, value = 100, ticks = FALSE,
                                  post = ' m', width = '130px'))))),
          uiOutput('brush_hint'),
          leafletOutput('an_map', height = '60vh'),
          div(style = 'height: 12px;'),
          div(class = 'rightbar-title', 'Missed places (click one to see it on the map)'),
          uiOutput('list_hint'),
          DTOutput('an_places')
        )),

        tabPanel('Check supervisors', value = 'supervisors', tab_layout(
          sidebar = tagList(
            step('user-check', 'One supervisor per health area',
                 p(class = 'hint', 'Type each supervisor\'s name and WhatsApp number in the table. Changes save by themselves.'),
                 uiOutput('sup_copy_ui'),
                 actionButton('sup_continue', 'Continue to Send', class = 'btn btn-success', width = '100%', icon = icon('arrow-right'))),
            more('Fill in with Excel instead',
                 p(class = 'hint', 'Download this district\'s health areas, add each supervisor\'s name and phone, then upload it.'),
                 downloadButton('sup_template', 'Download template', class = 'btn btn-default btn-sm', style = 'width: 100%;'),
                 div(style = 'height: 6px;'),
                 fileInput('sup_upload', NULL, accept = c('.xlsx', '.xls', '.csv'), width = '100%', buttonLabel = 'Upload...'))
          ),
          uiOutput('sup_checks'),
          rHandsontableOutput('sup_table')
        )),

        tabPanel('Send to supervisors', value = 'send', tab_layout(
          sidebar = tagList(
            step('list', 'Missed places list',
                 uiOutput('send_run_info'),
                 more('Send an earlier list', selectInput('send_run', NULL, choices = NULL, width = '100%'))),
            step('paper-plane', 'How to send',
                 radioButtons('send_how', NULL, selected = 'app',
                              choices = c('WhatsApp with the app link' = 'app', 'WhatsApp with a Google My Maps link' = 'mymaps',
                                          'Export files (shapefile, printed maps)' = 'export'))),
            conditionalPanel("input.send_how != 'export'",
              step('envelope', 'What the message includes',
                   radioButtons('msg_list', NULL, selected = 'all',
                                choices = c('All missed places, with directions' = 'all',
                                            'The top 5 missed places, with directions' = 'top5', 'No list of places' = 'none')),
                   checkboxInput('msg_app', 'The link to the supervisor app (to send each team its places)', value = TRUE),
                   checkboxInput('msg_pdf', 'A map to download (PDF)', value = FALSE),
                   conditionalPanel("input.msg_pdf", uiOutput('pdf_status'))))
          ),
          # WhatsApp (either link): one table, one row per health area, and the
          # selected row's message on the right
          conditionalPanel("input.send_how != 'export'", fluidRow(
            column(8,
                   uiOutput('send_progress'),
                   DTOutput('send_table')),
            column(4, div(class = 'msg-panel', uiOutput('send_preview'))))),
          conditionalPanel("input.send_how == 'export'", div(class = 'export-panel',
            div(class = 'rightbar-title', 'Shapefile'),
            p(class = 'hint', 'The whole district: missed area polygons and their navigation points, with health area, ',
              'place number, description, team and children.'),
            downloadButton('exp_shp', 'Download shapefile (zip)', class = 'btn btn-default'),
            tags$hr(),
            div(class = 'rightbar-title', 'Printed maps'),
            p(class = 'hint', 'One map per health area (PDF, landscape), with its missed places numbered and listed. ',
              'The first time takes about half a minute per health area.'),
            div(style = 'display: flex; gap: 10px; align-items: flex-end;',
                div(div(class = 'mini-label', 'Page size'), selectInput('exp_size', NULL, c('A3', 'A2', 'A1', 'A0'), selected = 'A1', width = '110px')),
                div(div(class = 'mini-label', 'Background'), selectInput('exp_base', NULL, c('Street map' = 'osm', 'Satellite' = 'satellite'), width = '150px')),
                div(style = 'margin-bottom: 15px;', downloadButton('exp_pdf', 'Download printed maps (zip)', class = 'btn btn-default')))))
        )),

        # Admins only (the header button is shown, and every action checked
        # on the server, only for role = admin)
        tabPanel('Admin', value = 'admin', div(style = 'padding: 12px 16px;',
          tabsetPanel(
            id = 'admin_tabs', type = 'pills',

            tabPanel('Teams', value = 'teams', fluidRow(
              column(8,
                     uiOutput('teams_hint'),
                     DTOutput('teams_table')),
              column(4, uiOutput('tm_form')))),

            tabPanel('Users', value = 'users', fluidRow(
              column(8, DTOutput('users_table')),
              column(4,
                     actionButton('u_add', 'Add user', class = 'btn btn-primary', width = '100%', icon = icon('user-plus')),
                     p(class = 'hint', style = 'margin-top: 6px;', 'Or click someone in the table to edit or remove them.'),
                     tags$hr(style = 'margin: 10px 0;'),
                     uiOutput('u_form_title'),
                     div(class = 'mini-label', 'Username (used to sign in)'),
                     textInput('u_username', NULL, width = '100%'),
                     div(class = 'mini-label', 'Name'),
                     textInput('u_display', NULL, width = '100%'),
                     uiOutput('u_pw_label'),
                     passwordInput('u_password', NULL, width = '100%'),
                     div(class = 'mini-label', 'Role'),
                     radioButtons('u_role', NULL, choices = c('Regular' = 'user', 'Admin – can add users and change settings' = 'admin'), selected = 'user'),
                     actionButton('u_save', 'Save', class = 'btn btn-success', width = '100%', icon = icon('floppy-disk')),
                     div(style = 'height: 8px;'),
                     shinyjs::hidden(actionButton('u_delete', 'Remove this user', class = 'btn btn-danger', width = '100%', icon = icon('trash')))))),

            tabPanel('Settings', value = 'settings', div(style = 'max-width: 520px;',
              p(class = 'hint', 'Used every time anyone finds missed places, in every district. Saved lists keep the settings they were made with.'),
              uiOutput('settings_form'),
              div(style = 'display: flex; gap: 8px; margin-top: 8px;',
                  actionButton('set_save', 'Save settings', class = 'btn btn-primary', icon = icon('floppy-disk')),
                  actionButton('set_reset', 'Back to the defaults', class = 'btn btn-default', icon = icon('rotate-left')))))
          )
        ))
      )
    ))
  )
}

admin_server <- function(input, output, session, auth) {
  user <- reactive(auth$username %||% 'unknown')
  output$who <- renderUI(div(class = 'nav-user-av', title = auth$display_name %||% user(), toupper(substr(user(), 1, 2))))

  if (is.null(adj_pool) || is.null(vb_pool) || is.null(u5_rast)) {
    showModal(modalDialog(title = 'Setup incomplete', easyClose = TRUE,
      tags$ul(if (is.null(adj_pool)) tags$li('Health area planning database not reachable (ADJ_DB_* in .env).'),
              if (is.null(vb_pool)) tags$li('Vaccine Buddy database not reachable (VB_DB_* in .env; run scripts/create_db.R).'),
              if (is.null(u5_rast)) tags$li(sprintf('WorldPop raster not found at %s.', WORLDPOP_FILE)))))
  }

  # Analysis settings (changed on the Admin page)
  params_changed <- reactiveVal(0)
  params <- reactive({ params_changed(); vb_get_params() })

  # --- Campaign and district --------------------------------------------------------
  # Where this user left off: the campaign, district and days they last had
  # chosen (saved as they change, below; their search there comes back too)
  # -- else the newest campaign from its start date
  campaigns <- tryCatch(adj_get_campaigns(), error = function(e) data.frame(campaign_id = integer(0), campaign_name = character(0), start_date = as.Date(character(0))))
  last_view <- vb_get_last_view(isolate(user()))
  resume_cid <- if (isTRUE(last_view$campaign_id %in% campaigns$campaign_id)) last_view$campaign_id else NULL
  updateSelectInput(session, 'campaign', choices = setNames(campaigns$campaign_id, campaigns$campaign_name), selected = resume_cid)
  last_district <- last_view$district_name
  # the days, used once: when that campaign first opens
  view_dates <- if (!is.null(resume_cid) && !is.na(last_view$date_from) && !is.na(last_view$date_to)) c(last_view$date_from, last_view$date_to)

  campaign_id <- reactive({ req(input$campaign); as.integer(input$campaign) })
  campaign_name <- reactive(campaigns$campaign_name[campaigns$campaign_id == campaign_id()])

  areas <- reactive({
    cid <- campaign_id()
    withProgress(message = 'Loading health areas...', adj_load_campaign_cached(cid))
  })

  # The campaign's districts: with published health areas, or (from their
  # boundary) without
  observeEvent(areas(), {
    a <- areas()
    d <- base::sort(base::unique(c(a$health_areas$district_name, a$districts$district_name)))
    keep <- isolate(input$district)
    sel <- if (isTRUE(keep %in% d)) keep else if (isTRUE(last_district %in% d)) last_district else NULL
    updateSelectInput(session, 'district', choices = if (length(d)) d else c('No districts' = ''), selected = sel)
    sd <- campaigns$start_date[campaigns$campaign_id == campaign_id()]
    if (!is.null(view_dates) && identical(campaign_id(), as.integer(resume_cid))) {
      updateDateRangeInput(session, 'dates', start = view_dates[1], end = view_dates[2])
      view_dates <<- NULL
    } else if (length(sd) && !is.na(sd)) updateDateRangeInput(session, 'dates', start = as.Date(sd), end = max(as.Date(sd), Sys.Date()))
  })
  district <- reactive({ req(input$district); input$district })

  # Saved a moment after any of them changes
  view_now <- debounce(reactive(list(campaign_id(), district(), input$dates)), 1000)
  observeEvent(view_now(), {
    v <- view_now(); req(length(v[[3]]) == 2, !anyNA(v[[3]]))
    vb_set_last_view(user(), v[[1]], v[[2]], v[[3]][1], v[[3]][2])
  })

  # This district's slice of every layer
  areas_d <- reactive({
    a <- areas(); d <- district()
    lapply(a, function(x) if (is.null(x)) NULL else x[x$district_name == d, ])
  })
  # No health areas drawn for this district in this campaign: they come
  # from the team names in the GPS data instead
  no_map <- reactive({ h <- areas_d()$health_areas; is.null(h) || nrow(h) == 0 })

  # --- Vaccine Buddy data: status + on-demand update ------------------------------
  # The app never pulls from ODK itself. Data is updated on the VM hourly,
  # and "Check for new data" only files a request that the VM's
  # every-minute job picks up (scripts/sync_odk.R --if-requested). While a
  # request is open the button is disabled and the status refreshes every
  # 15 s; repeated clicks (or other users) don't queue more requests.
  sync_changed <- reactiveVal(0)
  sync_state <- reactive({
    sync_changed()
    st <- tryCatch(vb_sync_status(), error = function(e) list(error = conditionMessage(e)))
    invalidateLater(if (is.null(st$error) && nrow(st$pending)) 15000 else 60000)
    st
  })

  observe({
    st <- sync_state()
    shinyjs::toggleState('sync_now', is.null(st$error) && nrow(st$pending) == 0)
  })

  output$sync_status <- renderUI({
    st <- sync_state()
    small <- function(...) div(style = 'font-size: 11px; color: #475569;', ...)
    if (!is.null(st$error)) return(div(style = 'font-size: 11px; color: #dc2626;', 'Could not read the data status: ', st$error))
    hm <- function(t) format(lubridate::with_tz(t, ODK_TZ), '%H:%M')
    ago <- function(t) { m <- round(as.numeric(difftime(Sys.time(), t, units = 'mins')))
                         if (m < 60) sprintf('%d min ago', m) else if (m < 48 * 60) sprintf('%.0f hours ago', m / 60) else sprintf('%.0f days ago', m / 1440) }
    pend <- st$pending
    tagList(
      if (st$n_rows == 0) small(tags$b('No team GPS data stored yet.'))
      else small('Team GPS data up to ', tags$b(format(lubridate::with_tz(st$latest_ts, ODK_TZ), '%d %b %H:%M'))),
      if (nrow(pend) && is.na(pend$picked_at[1]))
        div(style = 'font-size: 11px; color: #0f766e;', icon('clock'), sprintf(' Update requested at %s – starting within a minute...', hm(pend$requested_at[1])))
      else if (nrow(pend))
        div(style = 'font-size: 11px; color: #0f766e;', icon('rotate', class = 'fa-spin'), sprintf(' Updating since %s...', hm(pend$picked_at[1])))
      else if (nrow(st$last_ok)) small(sprintf('Last updated %s', ago(st$last_ok$finished_at[1]))),
      if (!nrow(pend) && nrow(st$last_err)) div(style = 'font-size: 11px; color: #dc2626;', 'Last update failed: ', st$last_err$message[1]))
  })

  observeEvent(input$sync_now, {
    shinyjs::disable('sync_now')
    r <- tryCatch(vb_request_sync(user()), error = function(e) 'error')
    stale <- tryCatch(vb_last_request_stale(), error = function(e) FALSE)
    showNotification(switch(r,
      requested = 'Update requested. New data usually arrives within a few minutes – you can keep working.',
      already   = 'An update is already on its way.',
      'Could not request an update.'), type = if (r == 'error') 'error' else 'message', duration = 6)
    if (stale) showNotification('The last request was never picked up – the update job on the server may not be running.',
                                type = 'warning', duration = 10)
    sync_changed(sync_changed() + 1)
  })

  # A finished update brings new data: tell the user once
  last_done <- reactiveVal(NULL)
  observe({
    st <- sync_state(); req(is.null(st$error), nrow(st$last_ok))
    t <- st$last_ok$finished_at[1]
    if (!is.null(last_done()) && t > last_done())
      showNotification('Team GPS data updated. Click "Find missed places" to use it.', type = 'message', duration = 8)
    last_done(t)
  })

  # --- 1. Find missed places ------------------------------------------------------------
  # In three steps on one map: find the missed squares (shown as one red
  # layer); paint squares on or off by hand; split them into missed places
  # (split_places()) to check and save. One way only: once split, the
  # squares can only change by finding missed areas again.
  #
  # analysis(): params, clean (GPS), areas, no_map, dates, campaign_id,
  # district, ctx (the district's grid and what split_places() needs),
  # found_missed / coverage0 (as found), missed (cell numbers now),
  # painted (cells painted in by hand), result (places -- NULL until split
  # -- and coverage, as vb_save_run() takes them).
  analysis <- reactiveVal(NULL)
  saved_run <- reactiveVal(NULL)        # run_id once these places are saved
  excluded <- reactiveVal(integer(0))   # step 5: places (indexes into an$result$places) left out
  split_id <- reactiveVal(0)            # bumped by each split (or resume): a new list
  found <- reactiveVal(0)               # bumped by each new search (not by edits)
  split_done <- reactive(!is.null(analysis()$result$places))

  # Missed places stay the top map layer, so they can always be hovered and
  # clicked: after anything else is drawn (the map's own updates go first)
  places_to_front <- function() session$onFlushed(function() session$sendCustomMessage('places_front', list()), once = TRUE)

  # --- Saved automatically, picked up again ---------------------------------
  # Each search is saved once (work_sessions.base), and what's changed since
  # -- squares painted on or off, the places split, the list saved, the tab
  # -- a moment after each change (edits). Opening a campaign + district this
  # user worked on brings it all back.
  pack_base <- function(an) {
    an$result <- NULL; an$missed <- NULL; an$painted <- NULL
    # terra objects don't survive saving as they are; the GPS is already in an$clean
    an$ctx$grid <- terra::wrap(an$ctx$grid); an$ctx$pings <- NULL
    an
  }
  unpack_base <- function(an) {
    an$ctx$grid <- terra::unwrap(an$ctx$grid); an$ctx$pings <- an$clean$pings
    an
  }
  work_edits <- reactive({
    an <- analysis(); req(an)
    list(missed = an$missed, painted = an$painted, result = an$result, saved_run = saved_run(), tab = input$main_tabs)
  })
  work_edits_later <- debounce(work_edits, 1500)
  observeEvent(work_edits_later(), {
    an <- isolate(analysis()); req(an)
    tryCatch(vb_save_work(user(), an$campaign_id, an$district, edits = work_edits_later()),
             error = function(e) message('[autosave] ', conditionMessage(e)))
  })

  first_open <- TRUE
  observeEvent(list(campaign_id(), district()), {
    analysis(NULL); saved_run(NULL)
    w <- tryCatch(vb_load_work(user(), campaign_id(), district()), error = function(e) { message('[resume] ', conditionMessage(e)); NULL })
    if (!is.null(w$base$ctx)) {   # (work saved by an older version of the app has no ctx: not resumed)
      an <- unpack_base(w$base); e <- w$edits
      an$missed <- if (!is.null(e)) e$missed else an$found_missed
      an$painted <- e$painted %||% numeric(0)
      an$result <- e$result %||% list(places = NULL, coverage = an$coverage0)
      analysis(an); saved_run(e$saved_run)
      found(found() + 1)
      # its days -- except at sign-in, where the days the user last chose stay
      if (!first_open) updateDateRangeInput(session, 'dates', start = as.Date(an$date_from), end = as.Date(an$date_to))
      if (first_open && !is.null(e$tab)) updateTabsetPanel(session, 'main_tabs', selected = e$tab)
      showNotification(sprintf('Picked up where you left off (saved %s).', format(lubridate::with_tz(w$updated_at, ODK_TZ), '%d %b %H:%M')),
                       type = 'message', duration = 5)
    }
    first_open <<- FALSE
  })
  observe({
    an <- analysis()
    # One way through: find -> adjust (brush) -> split -> check and save.
    # Once split, the squares can only change by finding missed areas again.
    shinyjs::toggle('brush_box', condition = !is.null(an) && !split_done())
    shinyjs::toggleState('split', !is.null(an) && !split_done() && length(an$missed) > 0)
    shinyjs::toggleState('save_run', split_done())
  })

  # Step 1: the missed squares
  observeEvent(input$run, {
    req(input$dates, u5_rast)
    a <- areas_d(); p <- params(); nm <- no_map()
    if (nm && (is.null(a$districts) || nrow(a$districts) == 0))
      return(showNotification('No boundary found for this district, so it can\'t be searched.', type = 'warning'))
    bb <- sf::st_bbox(if (nm) a$districts else a$health_areas)
    withProgress(message = 'Finding missed areas', value = 0, {
      tryCatch({
        setProgress(0.1, detail = 'Loading team GPS data')
        rows <- vb_load_entries(input$dates[1], input$dates[2], bb)
        if (nrow(rows) == 0) stop('No team GPS data for this district on these days. Check the dates, or click "Check for new data".')
        # Ignored teams, teams in no health area, health areas set by an admin
        p <- apply_team_roles(p, team_roles(rows$team_name, tryCatch(vb_get_team_roles(campaign_id()), error = function(e) NULL)))
        cl <- clean_pings(rows, p)
        if (nm) {
          setProgress(0.2, detail = 'Working out health areas from the team names')
          a$health_areas <- infer_health_areas(a$districts, cl$pings, p)
          if (is.null(a$health_areas))
            stop('No vaccination team GPS data inside this district on these days. Check the dates, or click "Check for new data".')
        }
        setProgress(0.3, detail = 'Comparing team tracks with where children live')
        names_df <- tryCatch(vb_load_place_names(bb), error = function(e) NULL)
        res <- run_analysis(a, cl$pings, p, u5_rast, place_names = names_df)
        d <- district()
        an <- list(params = p, clean = cl, areas = a, no_map = nm, date_from = input$dates[1], date_to = input$dates[2],
                   campaign_id = campaign_id(), district = d, ctx = res$ctx[[d]],
                   found_missed = res$missed[[d]], coverage0 = res$coverage[[d]],
                   missed = res$missed[[d]], painted = numeric(0), result = list(places = NULL, coverage = res$coverage[[d]]))
        analysis(an); saved_run(NULL)
        found(found() + 1)
        # Saved straight away, replacing this user's earlier work here
        setProgress(0.95, detail = 'Saving')
        tryCatch(vb_save_work(user(), an$campaign_id, d, base = pack_base(an),
                              edits = list(missed = an$missed, painted = an$painted, result = an$result, saved_run = NULL, tab = 'find')),
                 error = function(e) message('[autosave] ', conditionMessage(e)))
      }, error = function(e) showNotification(conditionMessage(e), type = 'error', duration = NULL))
    })
  })

  # Health area filter follows the analysed health areas
  observeEvent(list(found(), input$district), {
    updateSelectInput(session, 'ha_filter', selected = 'all',
                      choices = c('All health areas' = 'all', if (!is.null(analysis())) ha_names()))
  })

  output$an_summary <- renderUI({
    an <- analysis()
    if (is.null(an)) return(p(class = 'summary', style = 'color: #64748b;',
                              'Choose the days on the left, then click ', tags$b('Find missed areas'), '.',
                              if (isTRUE(no_map())) span(class = 'muted', sprintf(
                                'No health areas are drawn for %s in this campaign, so they will be taken from the team names in the GPS data.',
                                input$district))))
    pl <- an$result$places
    ha <- an$result$coverage[an$result$coverage$team_area == '', ]
    no_data <- sort(ha$health_area[!ha$has_data])
    kids <- sum(an$ctx$cells$u5_pop[an$ctx$cells$cell %in% an$missed])
    p(class = 'summary',
      if (length(an$missed) == 0) tagList(tags$b('No missed areas'), sprintf(' in %s.', an$district))
      else if (is.null(pl)) tagList(tags$b(sprintf('%s missed %s', format(length(an$missed), big.mark = ','), if (length(an$missed) == 1) 'square' else 'squares')),
                                    sprintf(' (100 m) in %s, with about %s children under 5.', an$district, .fmt_n(kids)),
                                    span(class = 'muted', 'Check them on the map and paint squares on or off with the brush, then split them into missed places.'))
      else tagList(tags$b(sprintf('%d missed %s', nrow(pl), if (nrow(pl) == 1) 'place' else 'places')),
                   sprintf(' in %s, with about %s children under 5.', an$district, .fmt_n(sum(pl$u5_pop))),
                   span(class = 'muted', 'Check them, then save. To change the squares, find missed areas again.')),
      if (an$no_map) span(class = 'muted', sprintf(
        'No health areas are drawn for %s, so they were taken from the team names (e.g. ELDAS_HC_T2 is ELDAS HC). Missed areas were looked for within %s m of team tracks.',
        an$district, format(an$params$no_map_reach_m, big.mark = ','))),
      if (length(no_data)) span(class = 'muted', sprintf('No team GPS data for %d health %s, so %s skipped: %s.',
                                length(no_data), if (length(no_data) == 1) 'area' else 'areas',
                                if (length(no_data) == 1) 'it was' else 'they were', paste(no_data, collapse = ', '))))
  })

  # Map: base layers per district; missed squares, places, tracks and
  # highlights via proxy
  output$an_map <- renderLeaflet({
    a <- areas_d()
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addMapPane('u5', zIndex = 250) |>   # WorldPop: above the basemap, below tracks and places
      # Same keyless basemaps as the adjuster (CARTO now needs an API key)
      addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap') |>
      addProviderTiles(providers$Esri.WorldImagery, group = 'ESRI Satellite') |>
      addProviderTiles(providers$OpenTopoMap, group = 'Topo') |>
      addLayersControl(baseGroups = c('OpenStreetMap', 'ESRI Satellite', 'Topo'), options = layersControlOptions(collapsed = TRUE))
    # Drawn health areas, or else just the district outline
    outline <- if (!no_map()) a$health_areas else a$districts
    if (!is.null(outline) && nrow(outline) > 0) {
      bb <- sf::st_bbox(outline)
      m <- m |> fitBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
      m <- if (!no_map()) addPolygons(m, data = outline, color = '#0f172a', weight = 2, fill = FALSE, label = ~health_area)
           else addPolygons(m, data = outline, color = '#64748b', weight = 2, dashArray = '6 4', fill = FALSE, label = ~district_name)
    }
    # Only this district's SIA coordination sites / facilities a health area
    # is named after (all facilities still serve as landmarks in labels)
    f <- a$facilities
    if (!is.null(f) && nrow(f) > 0)
      f <- f[f$polio_sia_coordination_site %in% 'Yes' | trimws(f$facility_name) %in% trimws(a$health_areas$health_area), ]
    m <- add_facilities(m, f, label_class = NULL)
    m |> addLegend('bottomright', colors = c('#dc2626', TRACK_POINT), labels = c('Missed', 'Team GPS track'), opacity = 1)
  })

  # Before splitting: the missed squares, as one layer
  observe({
    an <- analysis()
    proxy <- leafletProxy('an_map') |> clearGroup('missed')
    req(an, !split_done())
    lay <- missed_layer(an$ctx, an$missed)
    if (is.null(lay)) return()
    proxy |> addPolygons(data = lay, group = 'missed', color = '#7f1d1d', weight = 1, fillColor = '#dc2626', fillOpacity = 0.4,
                         options = pathOptions(interactive = FALSE))
  })

  # After splitting: the numbered places (hover for the details, click to
  # find it in the list)
  observe({
    an <- analysis()
    proxy <- leafletProxy('an_map') |> clearGroup('places') |> clearGroup('hl')
    pl <- if (is.null(an)) NULL else an$result$places
    if (is.null(pl) || nrow(pl) == 0) return()
    ids <- paste0('p', seq_len(nrow(pl)))
    badge <- as.character(pl$place_no)
    hover <- lapply(sprintf('<b>#%d</b> &middot; %s<br>%s<br>Team that passed closest: %s<br>~%s children under 5',
                            pl$place_no, htmltools::htmlEscape(pl$health_area), htmltools::htmlEscape(pl$label),
                            htmltools::htmlEscape(.team_closest(pl)), .fmt_n(pl$u5_pop)), htmltools::HTML)
    # While painting, places are click-through: nothing in the brush's way
    inter <- !adding()
    if (!inter) hover <- NULL
    proxy |>
      addPolygons(data = sf::st_sf(id = ids, geometry = pl$outline), layerId = ids, group = 'places',
                  color = '#7f1d1d', weight = 1.5, fillColor = '#dc2626', fillOpacity = 0.35, label = hover,
                  options = pathOptions(interactive = inter)) |>
      addCircleMarkers(data = pl, lng = ~lon, lat = ~lat, layerId = ids, group = 'places', radius = 12,
                       color = '#fff', weight = 2, fillColor = '#dc2626', fillOpacity = 1, label = hover,
                       options = pathOptions(interactive = inter)) |>
      addLabelOnlyMarkers(data = pl, lng = ~lon, lat = ~lat, group = 'places', label = badge,
                          labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE,
                                                      style = list(color = '#fff', `font-weight` = '700', `font-size` = '10px', `pointer-events` = 'none')))
  })

  # Team GPS tracks over all the analysed days. With a health area chosen,
  # only the teams that worked there (their whole track, inside and outside it).
  observe({
    an <- analysis()
    proxy <- leafletProxy('an_map') |> clearGroup('tracks')
    req(an, isTRUE(input$trk_show))
    pg <- an$clean$pings
    if (nzchar(ha_sel())) {
      cov <- an$result$coverage
      teams <- trimws(strsplit(cov$teams[cov$team_area == '' & cov$health_area == ha_sel()][1] %||% '', ',')[[1]])
      pg <- pg[pg$team_name %in% teams, ]
    }
    if (nrow(pg) == 0) return()
    add_tracks(proxy, pg, an$params)
    places_to_front()
  })

  # WorldPop under-5 estimates for the district (each ~100 m square), on
  # demand. Very large districts are coarsened so the image stays small.
  u5_image <- reactive({
    a <- areas_d(); req(u5_rast)
    # The district boundary (else its drawn health areas)
    outline <- if (!is.null(a$districts) && nrow(a$districts) > 0) a$districts else a$health_areas
    req(outline, nrow(outline) > 0)
    v <- terra::vect(outline)
    x <- terra::crop(u5_rast, terra::ext(v) + 0.01, snap = 'out')
    f <- ceiling(sqrt(terra::ncell(x) / 1e6))
    if (f > 1) x <- terra::aggregate(x, fact = f, fun = 'mean', na.rm = TRUE)
    x <- terra::mask(x, v, touches = TRUE)   # nothing outside the district
    x[x < 1] <- NA   # under 1 child (not counted as populated): transparent
    x
  })
  observe({
    proxy <- leafletProxy('an_map') |> clearGroup('u5') |> removeControl('u5_legend')
    req(isTRUE(input$u5_show))
    x <- u5_image()
    # Whole-number bands from 1, the last one ending at the district's
    # highest square
    top <- ceiling(terra::global(x, 'max', na.rm = TRUE)[1, 1])
    req(is.finite(top))
    bins <- c(1, 2, 5, 10, 20, 50, 100, 200, 500)
    bins <- c(bins[bins < top], top)
    if (length(bins) < 2) bins <- c(1, 2)
    pal <- colorBin('YlOrRd', domain = c(1, max(bins)), bins = bins, na.color = 'transparent')
    proxy |>
      addRasterImage(x, colors = pal, opacity = 0.6, group = 'u5', method = 'ngb', options = gridOptions(pane = 'u5')) |>
      addLegend('bottomleft', pal = pal, values = bins, title = 'Children under 5<br>per 100 m square',
                labFormat = labelFormat(digits = 0, between = ' – '), layerId = 'u5_legend', opacity = 0.6)
  })

  # --- Step 2: paint missed squares on or off -------------------------------
  # The brush (above the map) paints the analysis's own 100 m grid cells;
  # each stroke updates the missed squares as soon as the mouse is let go.
  # (only before splitting: the workflow runs one way -- find, adjust, split, save)
  adding <- reactive(!is.null(analysis()) && !split_done() && isTRUE(input$brush %in% c('add', 'remove')))
  observeEvent(list(found(), input$main_tabs, input$district), updateRadioButtons(session, 'brush', selected = 'off'))

  # The district's analysis grid for the browser: its extent, and a local
  # affine fit between its UTM metres and lon/lat (exact enough within a district)
  paint_grid <- function(ctx) {
    r <- ctx$grid; cs <- terra::res(r)[1]
    # plain numbers: Shiny sends a named one (as terra's extent gives them)
    # to the browser as an object, not a number
    e <- unname(as.vector(terra::ext(r)))   # xmin, xmax, ymin, ymax
    x0 <- (e[1] + e[2]) / 2; y0 <- (e[3] + e[4]) / 2
    ll <- unname(sf::sf_project(paste0('EPSG:', ctx$epsg), 'EPSG:4326', rbind(c(x0, y0), c(x0 + 1000, y0), c(x0, y0 + 1000))))
    a <- (ll[2, 1] - ll[1, 1]) / 1000; c <- (ll[2, 2] - ll[1, 2]) / 1000   # per metre east
    b <- (ll[3, 1] - ll[1, 1]) / 1000; d <- (ll[3, 2] - ll[1, 2]) / 1000   # per metre north
    det <- a * d - b * c
    lapply(list(xmin = e[1], ymax = e[4], ncol = terra::ncol(r), nrow = terra::nrow(r), cs = cs,
                x0 = x0, y0 = y0, lon0 = ll[1, 1], lat0 = ll[1, 2], a = a, b = b, c = c, d = d,
                ia = d / det, ib = -b / det, ic = -c / det, id = a / det), function(v) unname(as.numeric(v)))
  }
  observe({
    on <- adding()
    shinyjs::toggleClass('an_map', 'adding', condition = on)
    ctx <- if (on) isolate(analysis())$ctx else NULL
    session$sendCustomMessage('paint_mode', list(on = on, erase = identical(input$brush, 'remove'), size = input$brush_m %||% 100,
                                                 grid = if (!is.null(ctx)) paint_grid(ctx)))
  })
  output$brush_hint <- renderUI(if (adding()) div(class = 'add-hint', icon('paintbrush'),
    if (identical(input$brush, 'remove')) ' Hold the left mouse button and paint over squares to remove them.'
    else ' Hold the left mouse button and paint over squares that were missed.',
    ' Drag with the right mouse button to move the map.'))

  observeEvent(input$paint_cells, {
    on.exit(session$sendCustomMessage('paint_mode', list(clear = TRUE)))
    an <- analysis(); req(an, adding())
    painted <- intersect(as.numeric(unlist(input$paint_cells$cells)), an$ctx$cells$cell)   # inside the health areas
    if (length(painted) == 0) return(showNotification('Paint inside a health area.', type = 'warning', duration = 3))
    if (identical(input$brush, 'remove')) {
      an$missed <- setdiff(an$missed, painted); an$painted <- setdiff(an$painted, painted)
    } else {
      new <- setdiff(painted, an$missed)
      an$missed <- c(an$missed, new); an$painted <- base::union(an$painted, new)
    }
    analysis(an); saved_run(NULL)
  })

  # --- Step 3: split into missed places --------------------------------------
  observeEvent(input$split, {
    an <- analysis(); req(an, !split_done())
    updateRadioButtons(session, 'brush', selected = 'off')
    sp <- withProgress(message = 'Splitting into missed places', value = 0.5,
                       tryCatch(split_places(an$ctx, an$missed, an$painted, an$params),
                                error = function(e) { showNotification(paste('Could not split them:', conditionMessage(e)), type = 'error'); NULL }))
    req(sp)
    if (is.null(sp$places)) return(showNotification('There are no missed squares to split.', type = 'warning'))
    sp$places <- sp$places[order(sp$places$health_area, sp$places$place_no), ]
    an$result <- sp
    analysis(an); saved_run(NULL)
  })

  # Chosen health area, or '' for all
  ha_sel <- reactive({ h <- input$ha_filter %||% 'all'; if (identical(h, 'all')) '' else h })

  # Health area filter: zooms to the health area on the map (and outlines
  # it, where it's drawn), and limits the list to its missed places
  # (tracks above follow it too).
  observe({
    a <- areas_d(); h <- ha_sel(); an <- isolate(analysis())   # not on every edit
    proxy <- leafletProxy('an_map') |> clearGroup('ha_hl')
    whole <- if (!no_map()) a$health_areas else a$districts
    if (!nzchar(h)) {   # back to all health areas: show the whole district again
      if (is.null(whole) || nrow(whole) == 0) return()
      bb <- sf::st_bbox(whole)
      return(proxy |> flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']]))
    }
    polys <- if (!is.null(an)) an$areas$health_areas else a$health_areas
    poly <- if (is.null(polys)) NULL else polys[polys$health_area == h, ]
    if (is.null(poly) || nrow(poly) == 0) return()
    bb <- sf::st_bbox(poly)
    # Health areas taken from team names aren't real boundaries: zoom only
    if (!no_map())
      proxy <- proxy |> addPolygons(data = poly, group = 'ha_hl', color = '#0d9488', weight = 4, fillColor = '#0d9488', fillOpacity = 0.06,
                                    options = pathOptions(interactive = FALSE))
    places_to_front()
    proxy |> flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
  })

  # Which places (indexes into an$result$places) the list shows
  shown_idx <- reactive({
    an <- analysis(); req(an, an$result$places)
    h <- ha_sel()
    i <- seq_len(nrow(an$result$places))
    if (nzchar(h)) i[an$result$places$health_area == h] else i
  })

  # List <-> map: selecting a row outlines that place on the map; clicking
  # a place on the map selects its row (switching the health area filter
  # first if the place is in another health area).
  observeEvent(input$an_places_rows_selected, {
    an <- analysis(); r <- input$an_places_rows_selected
    proxy <- leafletProxy('an_map') |> clearGroup('hl')
    req(an, an$result$places, length(r) == 1)
    o <- an$result$places$outline[shown_idx()[r]]
    bb <- sf::st_bbox(sf::st_buffer(o, 0.003))
    proxy |>
      addPolygons(data = sf::st_sf(geometry = o), group = 'hl', color = '#facc15', weight = 5, fill = FALSE, opacity = 1,
                  options = pathOptions(interactive = FALSE)) |>
      flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
  }, ignoreNULL = FALSE)

  pending_select <- reactiveVal(NULL)
  select_place <- function(i) {
    session$onFlushed(function() DT::selectRows(DT::dataTableProxy('an_places'), match(i, isolate(shown_idx()))), once = TRUE)
  }
  map_click <- function(ev) {
    if (isolate(adding()) || is.null(ev$id) || !startsWith(ev$id, 'p')) return()   # painting, not choosing
    i <- as.integer(sub('p', '', ev$id))
    if (i %in% isolate(shown_idx())) return(DT::selectRows(DT::dataTableProxy('an_places'), match(i, isolate(shown_idx()))))
    pending_select(i)
    updateSelectInput(session, 'ha_filter', selected = isolate(analysis())$result$places$health_area[i])
  }
  observeEvent(shown_idx(), {
    i <- pending_select()
    if (!is.null(i) && i %in% shown_idx()) { select_place(i); pending_select(NULL) }
  })
  observeEvent(input$an_map_shape_click, map_click(input$an_map_shape_click))
  observeEvent(input$an_map_marker_click, map_click(input$an_map_marker_click))

  output$list_hint <- renderUI({
    an <- analysis()
    if (!is.null(an) && !split_done() && length(an$missed))
      p(class = 'hint', 'The list of missed places appears here once you split the missed squares (step 4).')
  })
  output$an_places <- renderDT({
    an <- analysis(); req(an, an$result$places)
    pl <- sf::st_drop_geometry(an$result$places)[shown_idx(), ]
    tbl <- data.frame(`#` = pl$place_no, `Health area` = pl$health_area, Where = pl$label,
                      `Team that passed closest` = .team_closest(pl),
                      Children = ifelse(pl$u5_pop < 10, round(pl$u5_pop), signif(pl$u5_pop, 2)),   # shown as ~1,100
                      check.names = FALSE)
    datatable(tbl, rownames = FALSE, selection = 'single', options = DT_OPTS) |>
      formatCurrency('Children', currency = '~', digits = 0)
  })

  observeEvent(input$save_run, {
    an <- analysis()
    req(an, split_done())
    if (is.null(saved_run())) {
      run_id <- tryCatch(
        vb_save_run(an$campaign_id, campaign_name(), an$district, an$date_from, an$date_to, an$params, an$clean$report,
                    an$clean$pings, an$result, user()),
        error = function(e) { showNotification(paste('Could not save:', conditionMessage(e)), type = 'error', duration = NULL); NULL })
      req(run_id)
      saved_run(run_id)
      runs_changed(runs_changed() + 1)
      showNotification('Saved. Check the supervisors, then continue to Send.', type = 'message')
    }
    updateTabsetPanel(session, 'main_tabs', selected = 'supervisors')
  })

  # --- 2. Check supervisors ------------------------------------------------------------
  # Saved lists of this district, newest first
  runs_changed <- reactiveVal(0)
  runs <- reactive({
    runs_changed()
    tryCatch(vb_get_runs(campaign_id(), district()), error = function(e) NULL)
  })

  # The district's health areas: drawn ones, or else the ones taken from
  # team names by the current (or the last saved) search
  ha_names <- reactive({
    if (!no_map()) return(base::sort(base::unique(areas_d()$health_areas$health_area)))
    an <- analysis()
    h <- if (!is.null(an)) an$result$coverage$health_area else {
      r <- runs()
      if (is.null(r) || nrow(r) == 0) NULL else tryCatch(vb_get_coverage(r$run_id[1])$health_area_name, error = function(e) NULL)
    }
    base::sort(base::unique(h))
  })

  # Just a name and phone for each health area: a table with one row per
  # health area, loaded from the database and saved a moment after each
  # edit (the rows that changed). Health areas left without them simply get
  # no message.
  sup_load <- reactiveVal(0)   # bumped after copying or uploading: reload the table
  sup_stored <- reactive({
    sup_load()
    h <- ha_names()
    s <- tryCatch(vb_get_supervisors(campaign_id(), district()), error = function(e) NULL)
    i <- match(h, s$health_area_name)
    data.frame(health_area_name = h, supervisor_name = ifelse(is.na(i), '', s$supervisor_name[i]),
               phone = ifelse(is.na(i), '', s$phone[i]))
  })

  output$sup_table <- renderRHandsontable({
    rhandsontable(sup_stored(), rowHeaders = NULL, stretchH = 'all', height = 500, contextMenu = FALSE,
                  colHeaders = c('Health area', 'Supervisor', 'Phone')) |>
      hot_col('Health area', readOnly = TRUE)
  })

  # The table as it is now (this district's rows only -- just after a
  # district change it can briefly still hold the previous one)
  sup_now <- reactive({
    req(input$sup_table)
    d <- hot_to_r(input$sup_table)
    d[] <- lapply(d, function(x) trimws(dplyr::coalesce(as.character(x), '')))
    d[d$health_area_name %in% ha_names(), ]
  })

  # Save the rows that differ from the last save (or load)
  sup_last <- NULL
  observeEvent(sup_stored(), sup_last <<- sup_stored())
  save_supervisors <- function(d) {
    old <- sup_last[match(d$health_area_name, sup_last$health_area_name), ]
    changed <- d[d$supervisor_name != old$supervisor_name | d$phone != old$phone, ]
    if (nrow(changed) == 0) return(TRUE)
    tryCatch({
      vb_set_supervisors(campaign_id(), district(), changed, user())
      sup_last[match(changed$health_area_name, sup_last$health_area_name), ] <<- changed
      TRUE
    }, error = function(e) { showNotification(paste('Could not save:', conditionMessage(e)), type = 'error'); FALSE })
  }
  sup_paused <- debounce(sup_now, 1500)
  observeEvent(sup_paused(), save_supervisors(sup_paused()))

  output$sup_checks <- renderUI({
    line <- function(color, icon_name, text) div(style = sprintf('font-size: 12px; color: %s; margin-bottom: 10px;', color), icon(icon_name), ' ', text)
    if (length(ha_names()) == 0 && isTRUE(no_map()))
      return(line('#b45309', 'circle-info', sprintf(
        'No health areas are drawn for %s, so they come from the team names in the GPS data. Find missed places first (step 1) to list them here.',
        district())))
    d <- sup_now()
    missing <- d$health_area_name[!nzchar(d$supervisor_name) | nchar(normalize_phone(d$phone)) < 11]
    if (length(missing)) line('#b45309', 'circle-info', paste('No name and phone yet, so nothing will be sent for:', paste(missing, collapse = ', ')))
    else line('#15803d', 'check', 'Every health area has a supervisor.')
  })

  observeEvent(input$sup_continue, {
    if (!is.null(input$sup_table) && !save_supervisors(sup_now())) return()
    r <- runs()
    if (is.null(r) || nrow(r) == 0)
      return(showNotification('No missed places saved for this district yet – find and save them first (step 1).', type = 'warning'))
    updateSelectInput(session, 'send_run', selected = r$run_id[1])
    n <- prepare_messages(r$run_id[1])
    if (!is.null(n)) showNotification(sprintf('%d %s ready to send.', n, if (n == 1) 'message' else 'messages'), type = 'message')
    updateTabsetPanel(session, 'main_tabs', selected = 'send')
  })

  # While nothing is filled in: copy from the latest earlier campaign that
  # has supervisors for this district
  sup_source <- reactive({
    s <- tryCatch(vb_supervisor_campaigns(district()), error = function(e) NULL)
    s <- s[s$campaign_id != campaign_id() & s$campaign_id %in% campaigns$campaign_id, ]
    if (is.null(s) || nrow(s) == 0) return(NULL)
    s <- s[order(match(s$campaign_id, campaigns$campaign_id)), ][1, ]
    s$campaign_name <- campaigns$campaign_name[match(s$campaign_id, campaigns$campaign_id)]
    s
  })
  output$sup_copy_ui <- renderUI({
    src <- sup_source()
    if (is.null(src) || any(nzchar(sup_stored()$supervisor_name))) return(NULL)
    div(class = 'callout', sprintf('No supervisors saved for %s in this campaign yet.', district()),
        actionButton('sup_copy', sprintf('Copy %d from %s', src$n, src$campaign_name),
                     class = 'btn btn-default btn-sm', width = '100%', icon = icon('copy'), style = 'margin-top: 6px;'))
  })
  observeEvent(input$sup_copy, {
    n <- tryCatch(vb_copy_supervisors(sup_source()$campaign_id, campaign_id(), district(), user()), error = function(e) NA)
    sup_load(sup_load() + 1)
    showNotification(sprintf('Copied %s supervisors. Check the health area names still match.', n))
  })

  # Or with Excel: download the table, fill it in, upload it
  output$sup_template <- downloadHandler(
    filename = function() sprintf('supervisors_%s_%s.xlsx', gsub('[^A-Za-z0-9]+', '_', district()), Sys.Date()),
    content = function(file) {
      d <- sup_stored()
      writexl::write_xlsx(list(Supervisors = data.frame(health_area = d$health_area_name, supervisor_name = d$supervisor_name, phone = d$phone)), file)
    })
  observeEvent(input$sup_upload, {
    f <- input$sup_upload
    new <- tryCatch(
      if (grepl('[.]csv$', f$name, ignore.case = TRUE)) utils::read.csv(f$datapath, stringsAsFactors = FALSE, colClasses = 'character')
      else as.data.frame(readxl::read_excel(f$datapath, col_types = 'text')),
      error = function(e) NULL)
    if (!is.null(new)) names(new) <- tolower(trimws(names(new)))
    if (is.null(new) || !all(c('health_area', 'supervisor_name', 'phone') %in% names(new)))
      return(showNotification('Please use the downloaded template (columns health_area, supervisor_name, phone).', type = 'error'))
    new <- data.frame(health_area_name = trimws(new$health_area), supervisor_name = trimws(dplyr::coalesce(new$supervisor_name, '')),
                      phone = trimws(dplyr::coalesce(new$phone, '')))
    new <- new[new$health_area_name %in% ha_names() & !duplicated(new$health_area_name, fromLast = TRUE), ]
    tryCatch(vb_set_supervisors(campaign_id(), district(), new, user()),
             error = function(e) showNotification(paste('Could not save:', conditionMessage(e)), type = 'error'))
    sup_load(sup_load() + 1)
    showNotification(sprintf('%d health areas loaded from the file.', nrow(new)), type = 'message')
  })

  # --- 3. Send ------------------------------------------------------------------------------
  observe({
    r <- runs()
    ch <- if (is.null(r) || nrow(r) == 0) c('Nothing saved yet for this district' = '')
          else setNames(r$run_id, sprintf('%s to %s – %d missed places (saved %s by %s)',
                                          format(as.Date(r$data_from), '%d %b'), format(as.Date(r$data_to), '%d %b'), r$n_places,
                                          format(lubridate::with_tz(r$created_at, ODK_TZ), '%d %b %H:%M'), r$created_by))
    updateSelectInput(session, 'send_run', choices = ch)   # newest selected
  })

  send_refresh <- reactiveVal(0)
  run_id <- reactive({ req(input$send_run); as.integer(input$send_run) })

  output$send_run_info <- renderUI({
    r <- runs()
    if (is.null(r) || nrow(r) == 0)
      return(p(class = 'hint', 'Nothing saved yet for this district. Find and save missed places first (step 1).'))
    x <- r[r$run_id %in% as.integer(input$send_run), ]
    if (nrow(x) == 0) x <- r[1, ]
    div(class = 'hint',
        tags$b(sprintf('%s to %s', format(as.Date(x$data_from), '%d %b'), format(as.Date(x$data_to), '%d %b'))),
        sprintf(' – %d missed %s', x$n_places, if (x$n_places == 1) 'place' else 'places'), tags$br(),
        sprintf('Saved %s by %s', format(lubridate::with_tz(x$created_at, ODK_TZ), '%d %b %H:%M'), x$created_by),
        if (x$run_id != r$run_id[1]) div(style = 'color: #b45309; margin-top: 4px;', 'This is an earlier list, not the newest.'))
  })

  # One message per supervisor of a health area with team GPS data (one
  # without must not get a "no missed places" message). Supervisors already
  # sent theirs keep it as sent. Returns the number of messages, or NULL.
  prepare_messages <- function(rid) {
    if (!nzchar(APP_PUBLIC_URL)) { showNotification('Set APP_PUBLIC_URL in .env so links point at this app.', type = 'error'); return(NULL) }
    tryCatch({
      run <- vb_get_run(rid)
      sups <- vb_get_supervisors(campaign_id(), district())
      places <- vb_get_places(rid)
      cov <- vb_get_coverage(rid)
      existing <- vb_get_dispatches(rid)
      # Sending with a Google My Maps link: each health area's map, once pasted in
      links <- if (identical(isolate(input$send_how), 'mymaps')) vb_get_mymaps(rid) else NULL
      # What the operator chose to include
      opt <- isolate(msg_opts())
      pdfs <- if (opt$pdf) vb_pdf_areas(rid)$health_area_name else character(0)
      map_link <- function(h) { u <- links$url[match(h, links$health_area_name)]; if (length(u) && !is.na(u)) u else NULL }
      with_data <- cov$health_area_name[cov$team_area_name == '' & cov$has_data %in% TRUE]
      # complete rows only (the table saves itself as people type)
      sups <- sups[sups$health_area_name %in% with_data & nzchar(trimws(sups$supervisor_name)) & nchar(normalize_phone(sups$phone)) >= 11, ]
      sups <- sups[!duplicated(sups$health_area_name), ]   # one supervisor per health area
      msgs <- lapply(seq_len(nrow(sups)), function(i) {
        s <- sups[i, ]
        tok <- existing$token[existing$supervisor_id == s$supervisor_id][1]
        if (is.na(tok)) tok <- new_token()
        pa <- if (is.null(places)) NULL else places[places$health_area_name == s$health_area_name, ]
        data.frame(supervisor_id = s$supervisor_id, supervisor_name = s$supervisor_name, phone = s$phone, token = tok,
                   message = supervisor_message(s$supervisor_name, district(), s$health_area_name, run$data_to, pa,
                                                link = if (opt$app) supervisor_link(tok), map_link = map_link(s$health_area_name),
                                                pdf_link = if (s$health_area_name %in% pdfs) paste0(supervisor_link(tok), '&pdf=1'),
                                                with_list = opt$list != 'none', max_list = if (opt$list == 'top5') 5 else Inf))
      })
      if (length(msgs)) vb_upsert_dispatches(rid, dplyr::bind_rows(msgs), user())
      send_refresh(send_refresh() + 1)
      nrow(sups)
    }, error = function(e) { showNotification(paste('Could not prepare the messages:', conditionMessage(e)), type = 'error'); NULL })
  }

  # Messages are kept ready whenever the Send tab is open (supervisors may
  # have changed since)
  # What goes in the messages (the sidebar's "What the message includes")
  msg_opts <- reactive(list(list = input$msg_list %||% 'all', app = isTRUE(input$msg_app), pdf = isTRUE(input$msg_pdf)))

  observeEvent(list(input$main_tabs, input$send_run, input$send_how, msg_opts()), {
    req(input$main_tabs == 'send', isTruthy(input$send_run))
    if (msg_opts()$pdf) make_pdfs(as.integer(input$send_run))
    prepare_messages(as.integer(input$send_run))
  })

  # A list that already has My Maps links opens on sending with them
  observeEvent(input$send_run, {
    req(isTruthy(input$send_run))
    updateRadioButtons(session, 'send_how', selected = if (nrow(vb_get_mymaps(as.integer(input$send_run)))) 'mymaps' else 'app')
  })

  # One row per health area of the list: its supervisor's message (if any)
  # and, sending with My Maps, its map link
  mm_changed <- reactiveVal(0)
  send_rows <- reactive({
    send_refresh(); mm_changed()
    rid <- run_id()
    cov <- vb_get_coverage(rid)
    cov <- cov[cov$team_area_name == '', ]
    list(ha = cov[order(cov$health_area_name), c('health_area_name', 'has_data', 'n_places')],
         d = vb_get_dispatches(rid), links = vb_get_mymaps(rid), how = input$send_how, sups = tryCatch(vb_get_supervisors(campaign_id(), district()), error = function(e) NULL),
         opt = msg_opts(), pdfs = vb_pdf_areas(rid))
  })
  # The row's dispatch (one supervisor per health area)
  row_dispatch <- function(x) match(x$ha$health_area_name, x$d$health_area_name)

  # --- Sending with a Google My Maps link ---------------------------------------
  # My Maps has no API, so for each health area the operator imports the
  # app's KML file into a new My Maps map and pastes its share link back,
  # all from the health area's row: 1 file, 2 My Maps, 3 title, 4 link.
  run_places <- reactive(vb_get_places(run_id()))
  run_row <- reactive({ r <- runs(); r[r$run_id %in% run_id(), ] })
  # Health areas of the list with missed places, and their map titles
  mm_areas <- reactive({ pl <- run_places(); if (is.null(pl)) character(0) else base::sort(base::unique(pl$health_area_name)) })
  mm_title <- function(h) {
    r <- run_row()
    sprintf('Missed places – %s, %s (%s to %s)', h, district(), format(as.Date(r$data_from), '%d %b'), format(as.Date(r$data_to), '%d %b %Y'))
  }
  mm_kml <- function(h) {
    pl <- run_places(); drawn <- areas_d()$health_areas
    places_kml(pl[pl$health_area_name == h, ], mm_title(h), ha_poly = if (!is.null(drawn)) drawn[drawn$health_area == h, ])
  }
  # A row's file: made on request and handed to the browser to save
  observeEvent(input$mm_file, {
    h <- isolate(send_rows())$ha$health_area_name[as.integer(input$mm_file$i)]; req(h %in% mm_areas())
    session$sendCustomMessage('save_text', list(name = paste0(.file_part(mm_title(h)), '.kml'), text = mm_kml(h),
                                                type = 'application/vnd.google-earth.kml+xml'))
  })
  observeEvent(input$mm_link, {
    h <- isolate(send_rows())$ha$health_area_name[as.integer(input$mm_link$i)]; req(h %in% mm_areas())
    url <- trimws(input$mm_link$url %||% '')
    if (nzchar(url) && !grepl('^https?://', url)) url <- paste0('https://', url)
    if (nzchar(url) && !grepl('google\\.[a-z.]+/maps/d/|mymaps\\.google|maps\\.app\\.goo\\.gl|goo\\.gl/maps', url))
      return(showNotification('That doesn\'t look like a Google My Maps share link (it should contain google.com/maps/d/).', type = 'warning'))
    ok <- tryCatch({ vb_set_mymaps(run_id(), h, url, user()); TRUE },
                   error = function(e) { showNotification(paste('Could not save the link:', conditionMessage(e)), type = 'error'); FALSE })
    if (ok) { prepare_messages(run_id()); mm_changed(mm_changed() + 1) }
  })

  # A health area's printed map of the list (exports.R)
  area_pdf <- function(h, file, size, basemap) {
    pl <- run_places(); drawn <- areas_d()$health_areas; r <- run_row()
    places_pdf(pl[pl$health_area_name == h, ], file,
               title = sprintf('Missed places – %s, %s', h, district()),
               subtitle = sprintf('From vaccination team GPS, %s to %s', format(as.Date(r$data_from), '%d %b'), format(as.Date(r$data_to), '%d %b %Y')),
               ha_poly = if (!is.null(drawn)) drawn[drawn$health_area == h, ], size = size, basemap = basemap)
  }

  # --- A map to download (PDF) in each message ---------------------------------------
  # Made as soon as it's included (A3 street map: read on a phone, or
  # printed), once per list -- only health areas without one yet -- then
  # stored and downloaded by supervisors from their ?t=<token>&pdf=1 link.
  make_pdfs <- function(rid) {
    todo <- setdiff(mm_areas(), vb_pdf_areas(rid)$health_area_name)
    if (length(todo) == 0) return(invisible())
    d <- tempfile('pdf'); dir.create(d); on.exit(unlink(d, recursive = TRUE))
    withProgress(message = 'Making the PDF maps', value = 0, tryCatch({
      for (k in seq_along(todo)) {
        setProgress((k - 0.5) / length(todo), detail = todo[k])
        f <- file.path(d, sprintf('%d.pdf', k))
        area_pdf(todo[k], f, 'A3', 'osm')
        vb_save_pdf(rid, todo[k], f, 'A3', 'osm', user())
      }
    }, error = function(e) showNotification(paste('Could not make the maps:', conditionMessage(e)), type = 'error', duration = NULL)))
  }
  output$pdf_status <- renderUI({
    x <- send_rows(); has <- mm_areas()
    p(class = 'hint', sprintf('%d of %d health area maps ready.', sum(has %in% x$pdfs$health_area_name), length(has)))
  })

  # --- Export files: shapefile, printed maps -----------------------------------------
  output$exp_shp <- downloadHandler(
    filename = function() sprintf('missed_places_%s_%s.zip', .file_part(district()), format(as.Date(run_row()$data_to), '%Y%m%d')),
    content = function(file) {
      pl <- run_places(); req(pl)
      places_shapefile_zip(pl, file)
    })
  output$exp_pdf <- downloadHandler(
    filename = function() sprintf('missed_places_maps_%s_%s_%s.zip', .file_part(district()), input$exp_size,
                                  format(as.Date(run_row()$data_to), '%Y%m%d')),
    content = function(file) {
      req(run_places())
      has <- mm_areas()
      d <- tempfile('pdf'); dir.create(d); on.exit(unlink(d, recursive = TRUE))
      withProgress(message = 'Making the printed maps', value = 0, {
        for (k in seq_along(has)) {
          setProgress(k / length(has), detail = has[k])
          area_pdf(has[k], file.path(d, paste0(.file_part(has[k]), '.pdf')), input$exp_size, input$exp_base)
        }
      })
      zip::zip(file, list.files(d, full.names = TRUE), mode = 'cherry-pick')
    })

  output$send_progress <- renderUI({
    x <- send_rows(); j <- row_dispatch(x)
    n <- sum(!is.na(j))
    if (n == 0) return(p(class = 'summary', style = 'color: #64748b;', 'No messages to send yet.'))
    p(class = 'summary', tags$b(sprintf('%d of %d sent', sum(!is.na(x$d$send_clicked_at[j[!is.na(j)]])), n)),
      if (identical(x$how, 'mymaps')) span(class = 'muted', sprintf(
        '%d of %d My Maps links added. In My Maps: Create a new map, Import the file, paste the title, then Share with Anyone with the link.',
        sum(mm_areas() %in% x$links$health_area_name), length(mm_areas()))))
  })

  send_tbl <- function(x) {
    j <- row_dispatch(x); d <- x$d[j, ]; h <- x$ha$health_area_name; i <- seq_along(h)
    has_msg <- !is.na(j)
    mm <- identical(x$how, 'mymaps')
    url <- x$links$url[match(h, x$links$health_area_name)]
    with_places <- h %in% isolate(mm_areas())
    why <- ifelse(!x$ha$has_data %in% TRUE, '<span style="color: #94a3b8;">No team GPS data</span>',
           ifelse(!h %in% x$sups$health_area_name[nzchar(x$sups$supervisor_name) & nchar(normalize_phone(x$sups$phone)) >= 11],
                  '<a href="#" style="color: #b45309;" onclick="$(\'.hdr-tab[data-tab=supervisors]\').click(); return false;">No supervisor or phone – add</a>',
                  '<span style="color: #94a3b8;">Not ready yet</span>'))
    # A message with no link and no list says nothing useful
    empty <- !mm && !x$opt$app && !x$opt$pdf && x$opt$list == 'none'
    send <- ifelse(!has_msg, why,
            ifelse(empty, '<span style="color: #b45309;">Choose what the message includes</span>',
            ifelse(mm & with_places & is.na(url), '<span style="color: #b45309;">Add the map link first</span>',
            ifelse(x$opt$pdf & with_places & !h %in% x$pdfs$health_area_name, '<span style="color: #b45309;">Make the PDF maps first</span>',
                   sprintf('<a class="btn btn-success btn-sm" target="_blank" href="%s" onclick="Shiny.setInputValue(\'sent_click\', {id: %d, nonce: Math.random()}, {priority: \'event\'})">%s</a>',
                           wa_link(d$phone, d$message), d$dispatch_id, ifelse(is.na(d$send_clicked_at), 'Send', 'Send again'))))))
    out <- data.frame(`Health area` = h, Supervisor = ifelse(has_msg, d$supervisor_name, ''), Phone = ifelse(has_msg, d$phone, ''),
                      `Missed places` = x$ha$n_places, check.names = FALSE)
    if (mm) {
      js_str <- function(s) as.character(jsonlite::toJSON(s, auto_unbox = TRUE))
      act <- function(input, k) sprintf("event.stopPropagation(); Shiny.setInputValue('%s', {i: %d, nonce: Math.random()}, {priority: 'event'})", input, k)
      out$`1. File` <- ifelse(with_places, sprintf('<button class="btn btn-default btn-xs" onclick="%s"><i class="fa fa-download"></i></button>', act('mm_file', i)), '')
      # (My Maps has no address that creates a map directly: its home page's "Create a new map" button does)
      out$`2. My Maps` <- ifelse(with_places, '<a class="btn btn-default btn-xs" href="https://www.google.com/maps/d/" target="_blank" onclick="event.stopPropagation();">Open</a>', '')
      out$`3. Title` <- ifelse(with_places, sprintf('<button class="btn btn-default btn-xs" title="%s" onclick="event.stopPropagation(); navigator.clipboard.writeText(%s); this.innerText = \'Copied\';">Copy</button>',
                                                    htmltools::htmlEscape(vapply(h, mm_title, ''), attribute = TRUE), htmltools::htmlEscape(vapply(vapply(h, mm_title, ''), js_str, ''), attribute = TRUE)), '')
      out$`4. Share link` <- ifelse(with_places, sprintf(
        '<div class="mm-cell">%s<input type="url" class="form-control mm-link" value="%s" placeholder="Paste the share link" onclick="event.stopPropagation();" onchange="Shiny.setInputValue(\'mm_link\', {i: %d, url: this.value, nonce: Math.random()}, {priority: \'event\'})"></div>',
        ifelse(is.na(url), '', '<i class="fa fa-circle-check" style="color: #16a34a;"></i>'), htmltools::htmlEscape(ifelse(is.na(url), '', url), attribute = TRUE), i), '')
    }
    out$Send <- send
    out$Sent <- ifelse(has_msg & !is.na(d$send_clicked_at), format(lubridate::with_tz(d$send_clicked_at, ODK_TZ), '%d %b %H:%M'), '')
    out
  }

  # A new table per list and method; later changes (a link pasted, a message
  # sent) only swap its data, so the selected row and scroll position stay
  output$send_table <- renderDT({
    run_id(); input$send_how
    tbl <- send_tbl(isolate(send_rows()))
    html <- c('1. File', '2. My Maps', '3. Title', '4. Share link', 'Send')
    datatable(tbl, rownames = FALSE, escape = setdiff(names(tbl), html), selection = 'single',
              options = modifyList(DT_OPTS, list(scrollY = 'calc(100vh - 210px)',
                                                 columnDefs = list(list(orderable = FALSE, targets = which(names(tbl) %in% html) - 1)))))
  })
  observeEvent(send_rows(), {
    DT::replaceData(DT::dataTableProxy('send_table'), send_tbl(send_rows()), rownames = FALSE,
                    resetPaging = FALSE, clearSelection = 'none')
  }, ignoreInit = TRUE)

  observeEvent(input$sent_click, {
    vb_mark_sent(input$sent_click$id)
    send_refresh(send_refresh() + 1)
  })

  # On the right: the selected row's message, else the first one with a message
  output$send_preview <- renderUI({
    x <- send_rows(); j <- row_dispatch(x)
    r <- input$send_table_rows_selected %||% which(!is.na(j))[1]
    if (length(r) == 0 || is.na(r)) return(p(class = 'hint', 'No messages yet.'))
    if (is.na(j[r])) return(tagList(div(class = 'rightbar-title', x$ha$health_area_name[r]), p(class = 'hint', 'No message for this health area.')))
    d <- x$d[j[r], ]
    tagList(div(class = 'rightbar-title', sprintf('Message to %s', d$supervisor_name)),
            tags$pre(class = 'msg-preview', d$message))
  })
  # --- Admin: users and analysis settings (admins only) ----------------------------------
  is_admin <- reactive(identical(auth$role, 'admin'))
  observe(if (is_admin()) shinyjs::show('hdr_admin') else shinyjs::hide('hdr_admin'))

  output$settings_form <- renderUI({
    req(is_admin())
    p <- params()
    tagList(lapply(names(PARAM_UI), function(k) div(
      div(class = 'mini-label', PARAM_UI[[k]]),
      numericInput(paste0('set_', k), NULL, p[[k]], min = 0, width = '100%'),
      if (!identical(as.numeric(p[[k]]), as.numeric(DEFAULT_PARAMS[[k]])))
        div(class = 'set-default', sprintf('Changed – the default is %s', DEFAULT_PARAMS[[k]])))))
  })

  observeEvent(input$set_save, {
    req(is_admin())
    new <- lapply(setNames(nm = names(PARAM_UI)), function(k) {
      v <- input[[paste0('set_', k)]]
      if (is.numeric(v) && length(v) == 1 && !is.na(v) && v >= 0) v else NA
    })
    problem <- if (anyNA(unlist(new))) 'Please enter a number (0 or more) in every box.'
      else if (new$max_place_children < 20) 'Missed areas can only be split into places of 20 children or more.'
      else NULL
    if (!is.null(problem)) return(showNotification(problem, type = 'error'))
    ok <- tryCatch({ vb_save_params(new, user()); TRUE },
                   error = function(e) { showNotification(paste('Could not save:', conditionMessage(e)), type = 'error'); FALSE })
    if (ok) { params_changed(params_changed() + 1); showNotification('Settings saved. They apply the next time missed places are found.', type = 'message') }
  })

  observeEvent(input$set_reset, {
    req(is_admin())
    ok <- tryCatch({ vb_save_params(DEFAULT_PARAMS, user()); TRUE },
                   error = function(e) { showNotification(paste('Could not save:', conditionMessage(e)), type = 'error'); FALSE })
    if (ok) { params_changed(params_changed() + 1); showNotification('Settings are back to the defaults.', type = 'message') }
  })

  # Teams of the campaign (selected top right), from the Vaccine Buddy GPS:
  # the district each worked in most and its role (team_roles()), which an
  # admin can change. Loaded only when an admin opens this page.
  campaign_days <- reactive({
    sd <- campaigns$start_date[campaigns$campaign_id == campaign_id()]
    if (length(sd) == 0 || is.na(sd)) return(c(Sys.Date() - 60, Sys.Date()))
    later <- campaigns$start_date[!is.na(campaigns$start_date) & campaigns$start_date > sd]
    # a week before it starts, until the next campaign starts
    c(as.Date(sd) - 7, if (length(later)) min(later) - 1 else max(as.Date(sd) + 60, Sys.Date()))
  })
  team_squares <- reactive({
    req(is_admin(), identical(input$main_tabs, 'admin'), identical(input$admin_tabs, 'teams'))
    days <- campaign_days()
    withProgress(message = 'Loading teams...', vb_team_squares(days[1], days[2]))
  })
  teams_changed <- reactiveVal(0)
  team_list <- reactive({
    sq <- team_squares()
    shapes <- adj_district_shapes(campaign_id())
    if (nrow(sq) == 0 || is.null(shapes)) return(NULL)
    hit <- sf::st_intersects(sf::st_as_sf(sq, coords = c('lon', 'lat'), crs = 4326), shapes)
    sq$district <- shapes$district_name[vapply(hit, function(i) if (length(i)) i[1] else NA_integer_, integer(1))]
    sq <- sq[!is.na(sq$district), ]
    if (nrow(sq) == 0) return(NULL)
    by <- as.data.frame(dplyr::summarise(dplyr::group_by(sq, team_name, district), n = sum(n), .groups = 'drop'))
    tot <- tapply(by$n, by$team_name, sum)
    by <- by[order(by$team_name, -by$n), ]
    by <- by[!duplicated(by$team_name), ]
    # From its name: ignored / no health area / the drawn health area of its
    # district its name matches (else the name itself, where none are drawn)
    drawn <- areas()$health_areas
    def <- team_roles(by$team_name)
    def <- def[match(by$team_name, def$team_name), ]
    auto_ha <- vapply(seq_len(nrow(by)), function(i) {
      if (def$role[i] != 'area') return(NA_character_)
      h <- drawn$health_area[drawn$district_name == by$district[i]]
      if (length(h) == 0) return(team_ha_name(by$team_name[i]))
      b <- .name_match(by$team_name[i], h)
      if (length(b)) h[b[1]] else NA_character_
    }, character(1))
    teams_changed()
    ov <- tryCatch(vb_get_team_roles(campaign_id()), error = function(e) NULL)
    j <- match(by$team_name, ov$team_name)
    out <- data.frame(team_name = by$team_name, district = by$district, n_points = as.integer(tot[by$team_name]),
                      auto_role = def$role, auto_ha = auto_ha, set_by_admin = !is.na(j))
    out$role <- ifelse(is.na(j), out$auto_role, ov$role[j])
    out$health_area <- ifelse(is.na(j), out$auto_ha, ov$health_area_name[j])
    out[order(out$district, out$team_name), ]
  })

  output$teams_hint <- renderUI({
    days <- campaign_days()
    p(class = 'hint', sprintf('Teams with GPS data in %s (%s to %s – change the campaign at the top right). ', campaign_name(),
                              format(days[1], '%d %b'), format(days[2], '%d %b %Y')),
      'A team belongs to the health area its name gives unless you change it here. Changes apply the next time missed places are found.')
  })

  output$teams_table <- renderDT({
    t <- team_list()
    validate(need(!is.null(t), 'No Vaccine Buddy GPS data for this campaign\'s days yet.'))
    tbl <- data.frame(Team = t$team_name, District = t$district, `Belongs to` = .role_label(t$role, t$health_area),
                      `Set by` = ifelse(t$set_by_admin, 'Admin', 'Its name'), `GPS points` = t$n_points, check.names = FALSE)
    datatable(tbl, rownames = FALSE, selection = 'single', options = modifyList(DT_OPTS, list(scrollY = 'calc(100vh - 260px)'))) |>
      formatStyle('Set by', fontWeight = styleEqual('Admin', 'bold')) |>
      formatStyle('Belongs to', color = styleEqual('Not matched – where it worked most', '#b45309'))
  })

  tm_sel <- reactive({
    t <- team_list(); i <- input$teams_table_rows_selected
    if (is.null(t) || length(i) != 1) NULL else t[i, ]
  })

  output$tm_form <- renderUI({
    t <- tm_sel()
    if (is.null(t)) return(p(class = 'hint', 'Click a team to move it to another health area, or to mark it as not part of a ',
                             'health area (e.g. Surge teams) or not a vaccination team (e.g. FETP monitors).'))
    # Its district's health areas: drawn, else those the team names give
    drawn <- areas()$health_areas
    h <- drawn$health_area[drawn$district_name == t$district]
    if (length(h) == 0) { all <- team_list(); h <- all$health_area[all$district == t$district & all$role == 'area'] }
    h <- base::sort(base::unique(stats::na.omit(c(h, if (t$role == 'area') t$health_area))))
    current <- if (!t$set_by_admin) 'auto' else if (t$role == 'area') paste0('ha:', t$health_area) else t$role
    tagList(
      div(class = 'rightbar-title', t$team_name),
      p(class = 'hint', sprintf('Worked mostly in %s · %s GPS points', t$district, format(t$n_points, big.mark = ','))),
      div(class = 'mini-label', 'Belongs to'),
      selectInput('tm_choice', NULL, width = '100%', selected = current, choices = list(
        'Automatic' = stats::setNames('auto', sprintf('From its name: %s', .role_label(t$auto_role, t$auto_ha))),
        'Health area' = stats::setNames(paste0('ha:', h), h),
        'Other' = c('Not part of a health area (e.g. Surge teams)' = 'no_area',
                    'Ignore – not a vaccination team (e.g. FETP monitors)' = 'ignore'))),
      actionButton('tm_save', 'Save', class = 'btn btn-success', width = '100%', icon = icon('floppy-disk')))
  })

  observeEvent(input$tm_save, {
    req(is_admin()); t <- tm_sel(); v <- input$tm_choice; req(t, v)
    ok <- tryCatch({
      if (v == 'auto') vb_set_team_role(campaign_id(), t$team_name, NULL, user = user())
      else if (startsWith(v, 'ha:')) vb_set_team_role(campaign_id(), t$team_name, 'area', sub('^ha:', '', v), user = user())
      else vb_set_team_role(campaign_id(), t$team_name, v, user = user())
      TRUE
    }, error = function(e) { showNotification(paste('Could not save:', conditionMessage(e)), type = 'error'); FALSE })
    if (ok) {
      teams_changed(teams_changed() + 1)
      showNotification(sprintf('%s saved. It applies the next time missed places are found.', t$team_name), type = 'message')
    }
  })

  users_changed <- reactiveVal(0)
  users <- reactive({ users_changed(); req(is_admin()); vb_list_users() })

  output$users_table <- renderDT({
    u <- users()
    tbl <- data.frame(Username = u$username, Name = u$display_name,
                      Role = ifelse(u$role == 'admin', 'Admin', 'Regular'),
                      Added = format(lubridate::with_tz(u$created_at, ODK_TZ), '%d %b %Y'))
    datatable(tbl, rownames = FALSE, selection = 'single', options = modifyList(DT_OPTS, list(scrollY = '30vh')))
  })

  # The form is either adding a new user or editing the one clicked in the
  # table (username locked, so editing can't rename or create an account).
  u_mode <- reactiveVal('add')
  output$u_form_title <- renderUI(div(class = 'rightbar-title',
    if (u_mode() == 'add') 'Add a new user' else sprintf('Edit %s', input$u_username)))
  output$u_pw_label <- renderUI(div(class = 'mini-label',
    if (u_mode() == 'add') 'Password (at least 8 characters)' else 'New password (leave blank to keep the current one)'))
  observe({
    shinyjs::toggleState('u_username', u_mode() == 'add')
    shinyjs::toggle('u_delete', condition = u_mode() == 'edit')
  })

  # Click a user -> load them into the form for editing
  observeEvent(input$users_table_rows_selected, {
    u <- users(); i <- input$users_table_rows_selected
    req(length(i) == 1)
    u_mode('edit')
    updateTextInput(session, 'u_username', value = u$username[i])
    updateTextInput(session, 'u_display', value = u$display_name[i])
    updateTextInput(session, 'u_password', value = '')
    updateRadioButtons(session, 'u_role', selected = u$role[i])
  })

  clear_user_form <- function() {
    u_mode('add')
    for (id in c('u_username', 'u_display', 'u_password')) updateTextInput(session, id, value = '')
    updateRadioButtons(session, 'u_role', selected = 'user')
    DT::selectRows(DT::dataTableProxy('users_table'), NULL)
  }
  observeEvent(input$u_add, clear_user_form())

  observeEvent(input$u_save, {
    req(is_admin())
    uname <- tolower(trimws(input$u_username %||% '')); disp <- trimws(input$u_display %||% '')
    pw <- input$u_password %||% ''; role <- input$u_role
    u <- users(); existing <- uname %in% u$username
    problem <- if (!grepl('^[a-z0-9._-]{2,}$', uname)) 'Username: at least 2 characters, letters/numbers/. _ - only, no spaces.'
      else if (u_mode() == 'add' && existing) sprintf('There is already a user called %s – click them in the table to edit.', uname)
      else if (!nzchar(disp)) 'Please enter a name.'
      else if (!existing && nchar(pw) < 8) 'New users need a password of at least 8 characters.'
      else if (nzchar(pw) && nchar(pw) < 8) 'Passwords need at least 8 characters.'
      else if (existing && role != 'admin' && u$role[u$username == uname] == 'admin' && sum(u$role == 'admin') == 1) 'This is the only admin – make someone else admin first.'
      else NULL
    if (!is.null(problem)) return(showNotification(problem, type = 'error'))
    ok <- tryCatch({ vb_save_user(uname, disp, role, if (nzchar(pw)) pw else NULL); TRUE },
                   error = function(e) { showNotification(paste('Could not save:', conditionMessage(e)), type = 'error'); FALSE })
    if (ok) {
      showNotification(sprintf('%s %s.', uname, if (existing) 'updated' else 'added'), type = 'message')
      users_changed(users_changed() + 1); clear_user_form()
    }
  })

  observeEvent(input$u_delete, {
    req(is_admin())
    u <- users(); i <- input$users_table_rows_selected
    if (length(i) != 1) return(showNotification('Click the user to remove in the table first.', type = 'warning'))
    target <- u$username[i]
    if (identical(target, user())) return(showNotification('You can\'t remove yourself.', type = 'warning'))
    if (u$role[i] == 'admin' && sum(u$role == 'admin') == 1) return(showNotification('You can\'t remove the only admin.', type = 'warning'))
    showModal(modalDialog(title = 'Remove user?', sprintf('Remove %s (%s)? They will no longer be able to sign in.', target, u$display_name[i]),
                          footer = tagList(modalButton('Cancel'), actionButton('u_delete_ok', 'Remove', class = 'btn btn-danger'))))
  })
  observeEvent(input$u_delete_ok, {
    req(is_admin())
    u <- users(); i <- input$users_table_rows_selected
    removeModal()
    req(length(i) == 1, !identical(u$username[i], user()))
    vb_delete_user(u$username[i])
    showNotification(sprintf('%s removed.', u$username[i]), type = 'message')
    users_changed(users_changed() + 1); clear_user_form()
  })
}
