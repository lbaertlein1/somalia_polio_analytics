# =============================================================================
# ui_admin.R -- admin app, in the health area adjuster's layout and look
# (www/styles.css, header bar with tab buttons, grey left sidebar), behind
# the adjuster-style login screen (mod_auth.R) with this app's own accounts.
#
# A tool for quickly getting missed places to supervisors during a
# campaign, for users with little training -- not an analysis dashboard.
# Everything is per campaign + district (selected top right), in three
# numbered steps:
#   1 Find missed places   dates -> find -> check map and list -> save
#   2 Supervisors          who gets each health area's list
#   3 Send                 WhatsApp click-to-send per supervisor, follow-up
# Coverage figures (share visited, population reached) are deliberately
# never shown; only missed places and the approximate children in each.
# Analysis settings are fixed in global.R (DEFAULT_PARAMS).
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

TRACK_POINT <- '#2563eb'
TRACK_LINE  <- '#93c5fd'

admin_ui <- function() {
  # One tab = sidebar (first column, grey in styles.css) + main panel
  tab_layout <- function(sidebar, ...) fluidRow(column(3, sidebar), column(9, div(style = 'padding: 12px 16px;', ...)))
  step <- function(n, title, ...) div(class = 'step', div(class = 'step-h', span(class = 'step-n', n), title), ...)

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
                  font-size: 12px; align-items: center; justify-content: center; flex: none; }
        .hint { font-size: 12px; color: #475569; margin: 0 0 6px; }
        .summary { font-size: 14px; color: #0f172a; margin: 0 0 10px; }
        .summary .muted { color: #64748b; font-size: 12px; display: block; margin-top: 2px; }
        .track-bar { display: flex; flex-wrap: wrap; align-items: center; gap: 10px; margin-bottom: 6px; font-size: 12px; }
        .track-bar .shiny-input-container { margin: 0; }
        .track-bar .radio-inline { padding: 3px 10px; border: 1px solid #cbd5e1; border-radius: 14px; margin: 0 4px 0 0; background: #fff; }
        .track-bar .radio-inline input { display: none; }
        .track-bar .radio-inline:has(input:checked) { background: #0d9488; border-color: #0d9488; color: #fff; }
        .msg-preview { white-space: pre-wrap; font-family: inherit; font-size: 12px; background: #f8fafc; border: 1px solid #e2e8f0; }
        .data-box { padding: 8px; background: #fff; border: 1px solid #e2e8f0; border-radius: 4px; margin-bottom: 8px; }
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
              tags$button(class = 'hdr-tab', `data-tab` = 'supervisors', '2  Supervisors'),
              tags$button(class = 'hdr-tab', `data-tab` = 'send', '3  Send to supervisors')),
          div(id = 'header-right',
              div(class = 'hdr-sel', span('Campaign'), selectInput('campaign', NULL, choices = NULL, width = '200px')),
              div(class = 'hdr-sel', span('District'), selectInput('district', NULL, choices = NULL, width = '160px')),
              uiOutput('who', inline = TRUE))),

      tabsetPanel(
        id = 'main_tabs', type = 'tabs',

        tabPanel('Find missed places', value = 'find', tab_layout(
          sidebar = tagList(
            step(1, 'Campaign and district', uiOutput('where_hint')),
            step(2, 'Days to include',
                 dateRangeInput('dates', NULL, start = Sys.Date() - 3, end = Sys.Date(), width = '100%'),
                 div(class = 'data-box', uiOutput('sync_status'),
                     actionButton('sync_now', 'Check for new data', class = 'btn btn-default', width = '100%',
                                  icon = icon('rotate'), style = 'margin-top: 6px;'))),
            step(3, 'Find missed places',
                 actionButton('run', 'Find missed places', class = 'btn btn-primary', width = '100%', icon = icon('magnifying-glass')),
                 div(style = 'margin-top: 10px;',
                   div(class = 'mini-label', 'Only list missed places with at least this many children'),
                   numericInput('p_min_place_u5', NULL, DEFAULT_PARAMS$min_place_u5, min = 0, step = 5, width = '100%'),
                   div(class = 'mini-label', 'Split missed areas with more than this many children'),
                   numericInput('p_max_place_children', NULL, DEFAULT_PARAMS$max_place_children, min = 20, step = 50, width = '100%'))),
            step(4, 'Check, then save',
                 p(class = 'hint', 'Look over the map and list. When they look right, save them so they can be sent.'),
                 shinyjs::disabled(actionButton('save_run', 'Save and continue', class = 'btn btn-success', width = '100%', icon = icon('arrow-right'))))
          ),
          uiOutput('an_summary'),
          div(class = 'track-bar',
              div(style = 'display: flex; align-items: center; gap: 6px;', tags$b('Health area:'),
                  selectInput('ha_filter', NULL, choices = c('All health areas' = 'all'), width = '240px')),
              tags$b('Team GPS tracks:'),
              radioButtons('trk_day', NULL, choices = c('Off' = 'off'), selected = 'off', inline = TRUE)),
          leafletOutput('an_map', height = '60vh'),
          div(style = 'height: 12px;'),
          div(class = 'rightbar-title', 'Missed places (click one to see it on the map)'),
          DTOutput('an_places')
        )),

        tabPanel('Supervisors', value = 'supervisors', tab_layout(
          sidebar = tagList(
            step(1, 'Download the template',
                 p(class = 'hint', 'An Excel sheet listing this district\'s health areas. Add each supervisor\'s name and phone number.'),
                 downloadButton('sup_template', 'Download template', class = 'btn btn-default', style = 'width: 100%;')),
            step(2, 'Upload it when filled in',
                 fileInput('sup_upload', NULL, accept = c('.xlsx', '.xls', '.csv'), width = '100%', buttonLabel = 'Upload...')),
            step(3, 'Check and save',
                 p(class = 'hint', 'Fix anything in the table if needed (right-click to add or remove a row), then save.'),
                 actionButton('sup_save', 'Save supervisors', class = 'btn btn-primary', width = '100%', icon = icon('floppy-disk'))),
            tags$hr(),
            div(class = 'mini-label', 'Or copy this district\'s supervisors from an earlier campaign'),
            selectInput('sup_copy_from', NULL, choices = NULL, width = '100%'),
            actionButton('sup_copy', 'Copy', class = 'btn btn-default', width = '100%')
          ),
          uiOutput('sup_checks'),
          rHandsontableOutput('sup_table')
        )),

        tabPanel('Send to supervisors', value = 'send', tab_layout(
          sidebar = tagList(
            step(1, 'Choose the saved list', selectInput('send_run', NULL, choices = NULL, width = '100%')),
            step(2, 'Prepare the messages',
                 actionButton('send_prepare', 'Prepare messages', class = 'btn btn-primary', width = '100%', icon = icon('envelope'))),
            step(3, 'Send each one',
                 p(class = 'hint', tags$b('Send'), ' opens WhatsApp with the message ready – press send there. ',
                   'Each supervisor gets a private link to their own map and list. Opened and Done update as they use it.')),
            uiOutput('send_preview')
          ),
          DTOutput('send_table')
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

  # --- Campaign and district --------------------------------------------------------
  campaigns <- tryCatch(adj_get_campaigns(), error = function(e) data.frame(campaign_id = integer(0), campaign_name = character(0), start_date = as.Date(character(0))))
  updateSelectInput(session, 'campaign', choices = setNames(campaigns$campaign_id, campaigns$campaign_name))
  updateSelectInput(session, 'sup_copy_from', choices = setNames(campaigns$campaign_id, campaigns$campaign_name))

  campaign_id <- reactive({ req(input$campaign); as.integer(input$campaign) })
  campaign_name <- reactive(campaigns$campaign_name[campaigns$campaign_id == campaign_id()])

  areas <- reactive({
    cid <- campaign_id()
    withProgress(message = 'Loading health areas...', adj_load_campaign_cached(cid))
  })

  # Districts with published health areas in this campaign
  observeEvent(areas(), {
    a <- areas()
    d <- base::sort(base::unique(a$health_areas$district_name))
    updateSelectInput(session, 'district', choices = if (length(d)) d else c('No published health areas' = ''))
    sd <- campaigns$start_date[campaigns$campaign_id == campaign_id()]
    if (length(sd) && !is.na(sd)) updateDateRangeInput(session, 'dates', start = as.Date(sd), end = max(as.Date(sd), Sys.Date()))
  })
  district <- reactive({ req(input$district); input$district })

  # This district's slice of every layer
  areas_d <- reactive({
    a <- areas(); d <- district()
    lapply(a, function(x) if (is.null(x)) NULL else x[x$district_name == d, ])
  })
  # base:: and reactives read first: packages such as terra mask sort()/unique()
  # with S4 generics that turn Shiny's silent req() into a real error
  ha_names <- reactive({ a <- areas_d(); base::sort(base::unique(a$health_areas$health_area)) })

  output$where_hint <- renderUI(p(class = 'hint',
    if (isTruthy(input$district)) tagList('Working on ', tags$b(input$district), ' in ', tags$b(campaign_name()), '. Change this at the top right.')
    else 'Choose them at the top right.'))

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
  analysis <- reactiveVal(NULL)
  observeEvent(list(campaign_id(), district()), { analysis(NULL); shinyjs::disable('save_run') })

  observeEvent(input$run, {
    req(input$dates, u5_rast)
    a <- areas_d()
    if (is.null(a$health_areas) || nrow(a$health_areas) == 0)
      return(showNotification('This district has no published health areas for this campaign.', type = 'warning'))
    p <- DEFAULT_PARAMS
    num <- function(x, default) if (is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0) x else default
    p$min_place_u5   <- num(input$p_min_place_u5, p$min_place_u5)
    p$max_place_children <- max(20, num(input$p_max_place_children, p$max_place_children))
    withProgress(message = 'Finding missed places', value = 0, {
      tryCatch({
        setProgress(0.1, detail = 'Loading team GPS data')
        rows <- vb_load_entries(input$dates[1], input$dates[2], sf::st_bbox(a$health_areas))
        if (nrow(rows) == 0) stop('No team GPS data for this district on these days. Check the dates, or click "Check for new data".')
        cl <- clean_pings(rows, p)
        setProgress(0.3, detail = 'Comparing team tracks with where children live')
        names_df <- tryCatch(vb_load_place_names(sf::st_bbox(a$health_areas)), error = function(e) NULL)
        res <- run_analysis(a, cl$pings, p, u5_rast, place_names = names_df)
        if (!is.null(res$places)) res$places <- res$places[order(res$places$health_area, res$places$place_no), ]
        analysis(list(params = p, clean = cl, result = res, date_from = input$dates[1], date_to = input$dates[2],
                      campaign_id = campaign_id(), district = district(), saved_run_id = NULL))
        shinyjs::enable('save_run')
      }, error = function(e) showNotification(conditionMessage(e), type = 'error', duration = NULL))
    })
  })

  # Track controls follow the analysed days and health areas
  observeEvent(analysis(), {
    an <- analysis()
    days <- if (is.null(an)) character(0) else base::sort(base::unique(as.Date(an$clean$pings$ts, tz = ODK_TZ)))
    updateRadioButtons(session, 'trk_day', inline = TRUE, selected = if (length(days)) 'all' else 'off',
                       choices = c('Off' = 'off', if (length(days)) c('All days' = 'all',
                                   setNames(as.character(days), format(days, '%a %d %b')))))
    updateSelectInput(session, 'ha_filter', choices = c('All health areas' = 'all', if (!is.null(an)) ha_names()), selected = 'all')
  }, ignoreNULL = FALSE)

  output$an_summary <- renderUI({
    an <- analysis()
    if (is.null(an)) return(p(class = 'summary', style = 'color: #64748b;',
                              'Choose the days on the left, then click ', tags$b('Find missed places'), '.'))
    pl <- an$result$places
    ha <- an$result$coverage[an$result$coverage$team_area == '', ]
    no_data <- sort(ha$health_area[!ha$has_data])
    n <- if (is.null(pl)) 0 else nrow(pl)
    p(class = 'summary',
      if (n == 0) tagList(tags$b('No missed places found'), sprintf(' in %s.', an$district))
      else tagList(tags$b(sprintf('%d missed %s', n, if (n == 1) 'place' else 'places')),
                   sprintf(' in %s, with about %s children under 5.', an$district, .fmt_n(sum(pl$u5_pop)))),
      if (length(no_data)) span(class = 'muted', sprintf('No team GPS data for %d health %s, so %s skipped: %s.',
                                length(no_data), if (length(no_data) == 1) 'area' else 'areas',
                                if (length(no_data) == 1) 'it was' else 'they were', paste(no_data, collapse = ', '))))
  })

  # Map: base layers per district; places, tracks and highlight via proxy
  output$an_map <- renderLeaflet({
    a <- areas_d()
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      # Same keyless basemaps as the adjuster (CARTO now needs an API key)
      addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap') |>
      addProviderTiles(providers$Esri.WorldImagery, group = 'ESRI Satellite') |>
      addProviderTiles(providers$OpenTopoMap, group = 'Topo') |>
      addLayersControl(baseGroups = c('OpenStreetMap', 'ESRI Satellite', 'Topo'), options = layersControlOptions(collapsed = TRUE))
    if (!is.null(a$health_areas) && nrow(a$health_areas) > 0) {
      bb <- sf::st_bbox(a$health_areas)
      m <- m |>
        addPolygons(data = a$health_areas, color = '#0f172a', weight = 2, fill = FALSE, label = ~health_area) |>
        fitBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
    }
    # Only this district's SIA coordination sites / facilities a health area
    # is named after (all facilities still serve as landmarks in labels)
    f <- a$facilities
    if (!is.null(f) && nrow(f) > 0)
      f <- f[f$polio_sia_coordination_site %in% 'Yes' | trimws(f$facility_name) %in% trimws(a$health_areas$health_area), ]
    m <- add_facilities(m, f, label_class = NULL)
    m |> addLegend('bottomright', colors = c('#dc2626', TRACK_POINT), labels = c('Missed place', 'Team GPS track'), opacity = 1)
  })

  observe({
    an <- analysis()
    proxy <- leafletProxy('an_map') |> clearGroup('places') |> clearGroup('hl')
    pl <- if (is.null(an)) NULL else an$result$places
    if (is.null(pl) || nrow(pl) == 0) return()
    ids <- paste0('p', seq_len(nrow(pl)))
    # Dot shows the place number (as in the list); hover for the details
    badge <- as.character(pl$place_no)
    details <- lapply(sprintf('<b>#%d</b> &middot; %s<br>%s<br>Team that passed closest: %s<br>~%s children under 5',
                              pl$place_no, htmltools::htmlEscape(pl$health_area), htmltools::htmlEscape(pl$label),
                              htmltools::htmlEscape(.team_closest(pl)), .fmt_n(pl$u5_pop)), htmltools::HTML)
    proxy |>
      addPolygons(data = sf::st_sf(id = ids, geometry = pl$outline), layerId = ids, group = 'places',
                  color = '#7f1d1d', weight = 1.5, fillColor = '#dc2626', fillOpacity = 0.35, label = details) |>
      addCircleMarkers(data = pl, lng = ~lon, lat = ~lat, layerId = ids, group = 'places', radius = 12,
                       color = '#fff', weight = 2, fillColor = '#dc2626', fillOpacity = 1, label = details) |>
      addLabelOnlyMarkers(data = pl, lng = ~lon, lat = ~lat, group = 'places', label = badge,
                          labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE,
                                                      style = list(color = '#fff', `font-weight` = '700', `font-size` = '10px', `pointer-events` = 'none')))
  })

  # Team GPS tracks: small points with light route lines, per day or all
  # days. With a health area chosen, only the teams that worked there
  # (their whole track, inside and outside it).
  observe({
    an <- analysis()
    proxy <- leafletProxy('an_map') |> clearGroup('tracks')
    req(an, input$trk_day, input$trk_day != 'off')
    pg <- an$clean$pings
    pg$day <- as.character(as.Date(pg$ts, tz = ODK_TZ))
    if (input$trk_day != 'all') pg <- pg[pg$day == input$trk_day, ]
    if (nzchar(ha_sel())) {
      cov <- an$result$coverage
      teams <- trimws(strsplit(cov$teams[cov$team_area == '' & cov$health_area == ha_sel()][1] %||% '', ',')[[1]])
      pg <- pg[pg$team_name %in% teams, ]
    }
    if (nrow(pg) == 0) return()
    pg <- pg[order(pg$device_id, pg$ts), ]
    # A new line wherever the device changes or there's a long gap/jump
    brk <- c(TRUE, pg$device_id[-1] != pg$device_id[-nrow(pg)]) | is.na(pg$gap_min) |
      pg$gap_min > an$params$max_gap_min | pg$step_m > an$params$max_link_m
    pg$seg <- cumsum(brk)
    segs <- pg[pg$seg %in% pg$seg[duplicated(pg$seg)], c('seg', 'lon', 'lat')]
    if (nrow(segs) > 0)
      proxy |> addPolylines(data = sfheaders::sf_linestring(segs, x = 'lon', y = 'lat', linestring_id = 'seg'),
                            group = 'tracks', color = TRACK_LINE, weight = 1.5, opacity = 0.7)
    pts <- if (nrow(pg) > 40000) pg[round(seq(1, nrow(pg), length.out = 40000)), ] else pg
    proxy |> addCircleMarkers(data = pts, lng = ~lon, lat = ~lat, group = 'tracks', radius = 1.8, stroke = FALSE,
                              fillColor = TRACK_POINT, fillOpacity = 0.7,
                              popup = ~sprintf('%s<br>%s', team_label(team_name), format(ts, '%a %d %b %H:%M')))
  })

  # Chosen health area, or '' for all
  ha_sel <- reactive({ h <- input$ha_filter %||% 'all'; if (identical(h, 'all')) '' else h })

  # Health area filter: outlines and zooms to the health area on the map,
  # and limits the list to its missed places (tracks above follow it too).
  observe({
    a <- areas_d(); h <- ha_sel()
    proxy <- leafletProxy('an_map') |> clearGroup('ha_hl')
    if (is.null(a$health_areas) || nrow(a$health_areas) == 0) return()
    if (!nzchar(h)) {   # back to all health areas: show the whole district again
      bb <- sf::st_bbox(a$health_areas)
      return(proxy |> flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']]))
    }
    poly <- a$health_areas[a$health_areas$health_area == h, ]
    if (nrow(poly) == 0) return()
    bb <- sf::st_bbox(poly)
    proxy |>
      addPolygons(data = poly, group = 'ha_hl', color = '#0d9488', weight = 4, fillColor = '#0d9488', fillOpacity = 0.06) |>
      flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
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
    req(an, length(r) == 1)
    o <- an$result$places$outline[shown_idx()[r]]
    bb <- sf::st_bbox(sf::st_buffer(o, 0.003))
    proxy |>
      addPolygons(data = sf::st_sf(geometry = o), group = 'hl', color = '#facc15', weight = 5, fill = FALSE, opacity = 1) |>
      flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
  }, ignoreNULL = FALSE)

  pending_select <- reactiveVal(NULL)
  select_place <- function(i) {
    session$onFlushed(function() DT::selectRows(DT::dataTableProxy('an_places'), match(i, isolate(shown_idx()))), once = TRUE)
  }
  map_click <- function(ev) {
    if (is.null(ev$id) || !startsWith(ev$id, 'p')) return()
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
    req(an)
    if (!is.null(an$saved_run_id)) { updateTabsetPanel(session, 'main_tabs', selected = 'send'); return() }
    run_id <- tryCatch(
      vb_save_run(an$campaign_id, campaign_name(), an$district, an$date_from, an$date_to, an$params, an$clean$report,
                  an$clean$pings, an$result, user()),
      error = function(e) { showNotification(paste('Could not save:', conditionMessage(e)), type = 'error', duration = NULL); NULL })
    req(run_id)
    an$saved_run_id <- run_id
    analysis(an)
    runs_changed(runs_changed() + 1)
    updateTabsetPanel(session, 'main_tabs', selected = 'send')
    showNotification('Saved. Now prepare and send the messages.', type = 'message')
  })

  # --- 2. Supervisors ----------------------------------------------------------------------
  sup_data <- reactiveVal(NULL)
  sup_reload <- function() sup_data(tryCatch(vb_get_supervisors(campaign_id(), district()), error = function(e) NULL))
  observeEvent(list(campaign_id(), district()), sup_reload())

  # Template: one row per health area of this district (plus any saved
  # supervisors), name and phone to fill in.
  output$sup_template <- downloadHandler(
    filename = function() sprintf('supervisors_%s_%s.xlsx', gsub('[^A-Za-z0-9]+', '_', district()), Sys.Date()),
    content = function(file) {
      cur <- sup_data()
      saved <- if (!is.null(cur) && nrow(cur)) data.frame(health_area = cur$health_area_name, supervisor_name = cur$supervisor_name,
                                                          phone = cur$phone) else NULL
      todo <- setdiff(ha_names(), saved$health_area)
      blank <- data.frame(health_area = todo, supervisor_name = rep('', length(todo)), phone = rep('', length(todo)))
      tmpl <- rbind(saved, blank)
      writexl::write_xlsx(list(Supervisors = tmpl[order(tmpl$health_area), ]), file)
    })

  observeEvent(input$sup_upload, {
    f <- input$sup_upload
    new <- tryCatch(
      if (grepl('[.]csv$', f$name, ignore.case = TRUE)) utils::read.csv(f$datapath, stringsAsFactors = FALSE, colClasses = 'character')
      else as.data.frame(readxl::read_excel(f$datapath, col_types = 'text')),
      error = function(e) NULL)
    if (!is.null(new)) names(new) <- tolower(trimws(names(new)))
    need <- c('health_area', 'supervisor_name', 'phone')
    if (is.null(new) || !all(need %in% names(new)))
      return(showNotification('Please use the downloaded template (columns health_area, supervisor_name, phone).', type = 'error'))
    new <- new[!is.na(new$phone) & nzchar(trimws(new$phone)), ]
    # Keep the ids of supervisors already saved, so their sending history stays linked
    cur <- sup_data()
    key <- function(h, ph) paste(h, normalize_phone(ph))
    id <- if (is.null(cur) || nrow(cur) == 0) rep(NA_integer_, nrow(new))
          else cur$supervisor_id[match(key(new$health_area, new$phone), key(cur$health_area_name, cur$phone))]
    sup_data(data.frame(supervisor_id = id, district_name = rep(district(), nrow(new)), health_area_name = trimws(new$health_area),
                        supervisor_name = trimws(new$supervisor_name), phone = trimws(new$phone), stringsAsFactors = FALSE))
    showNotification(sprintf('%d supervisors loaded from the file. Check the table, then click Save supervisors.', nrow(new)))
  })

  output$sup_table <- renderRHandsontable({
    d <- sup_data()
    if (is.null(d) || nrow(d) == 0) d <- data.frame(supervisor_id = NA_integer_, health_area_name = '', supervisor_name = '', phone = '')
    d <- d[, c('supervisor_id', 'health_area_name', 'supervisor_name', 'phone')]
    rhandsontable(d, rowHeaders = NULL, stretchH = 'all', height = 500,
                  colHeaders = c('ID', 'Health area', 'Supervisor', 'Phone')) |>
      hot_col('ID', readOnly = TRUE, width = 40) |>
      hot_col('Health area', type = 'dropdown', source = ha_names(), strict = TRUE) |>
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })

  sup_edited <- reactive({
    req(input$sup_table)
    d <- hot_to_r(input$sup_table)
    d[nzchar(trimws(paste0(d$supervisor_name, d$phone))), ]
  })

  output$sup_checks <- renderUI({
    d <- sup_edited(); h <- ha_names()
    unknown <- d[!d$health_area_name %in% h, ]
    bad_phone <- d[nchar(normalize_phone(d$phone)) < 11, ]
    missing <- setdiff(h, d$health_area_name)
    line <- function(color, ...) div(style = sprintf('font-size: 12px; color: %s; margin-bottom: 4px;', color), ...)
    div(style = 'margin-bottom: 10px;',
        if (nrow(unknown)) line('#dc2626', icon('triangle-exclamation'), sprintf(' %d row(s) need a health area from the list.', nrow(unknown))),
        if (nrow(bad_phone)) line('#dc2626', icon('triangle-exclamation'), sprintf(' Check these phone numbers: %s', paste(bad_phone$phone, collapse = ', '))),
        if (length(missing)) line('#b45309', icon('circle-info'), sprintf(' No supervisor yet for: %s', paste(missing, collapse = ', ')))
        else if (length(h)) line('#15803d', icon('check'), ' Every health area has a supervisor.'))
  })

  observeEvent(input$sup_save, {
    d <- sup_edited()
    ok <- tryCatch({ vb_save_supervisors(campaign_id(), district(), d, user()); TRUE },
                   error = function(e) { showNotification(paste('Could not save:', conditionMessage(e)), type = 'error', duration = NULL); FALSE })
    if (ok) { sup_reload(); showNotification(sprintf('Saved %d supervisors for %s.', nrow(d), district()), type = 'message') }
  })

  observeEvent(input$sup_copy, {
    req(input$sup_copy_from)
    if (as.integer(input$sup_copy_from) == campaign_id()) return(showNotification('Pick a different campaign to copy from.'))
    n <- tryCatch(vb_copy_supervisors(input$sup_copy_from, campaign_id(), district(), user()), error = function(e) NA)
    sup_reload()
    showNotification(sprintf('Copied %s supervisors. Check the health area names still match.', n))
  })

  # --- 3. Send ------------------------------------------------------------------------------
  runs_changed <- reactiveVal(0)
  runs <- reactive({
    runs_changed()
    tryCatch(vb_get_runs(campaign_id(), district()), error = function(e) NULL)
  })
  observe({
    r <- runs()
    ch <- if (is.null(r) || nrow(r) == 0) c('Nothing saved yet for this district' = '')
          else setNames(r$run_id, sprintf('%s to %s – %d missed places (saved %s by %s)',
                                          format(as.Date(r$data_from), '%d %b'), format(as.Date(r$data_to), '%d %b'), r$n_places,
                                          format(lubridate::with_tz(r$created_at, ODK_TZ), '%d %b %H:%M'), r$created_by))
    updateSelectInput(session, 'send_run', choices = ch)
  })

  send_refresh <- reactiveVal(0)
  run_id <- reactive({ req(input$send_run); as.integer(input$send_run) })

  observeEvent(input$send_prepare, {
    rid <- run_id()
    run <- vb_get_run(rid)
    sups <- vb_get_supervisors(campaign_id(), district())
    if (nrow(sups) == 0) return(showNotification('Add supervisors for this district first (step 2).', type = 'warning'))
    if (!nzchar(APP_PUBLIC_URL)) return(showNotification('Set APP_PUBLIC_URL in .env so links point at this app.', type = 'error'))
    places <- vb_get_places(rid)
    cov <- vb_get_coverage(rid)
    existing <- vb_get_dispatches(rid)
    # Only health areas with team GPS data: one without must not get a
    # "no missed places" message.
    with_data <- cov$health_area_name[cov$team_area_name == '' & cov$has_data %in% TRUE]
    in_run <- sups$health_area_name %in% with_data
    sups <- sups[in_run, ]
    msgs <- lapply(seq_len(nrow(sups)), function(i) {
      s <- sups[i, ]
      tok <- existing$token[existing$supervisor_id == s$supervisor_id][1]
      if (is.na(tok)) tok <- new_token()
      pa <- if (is.null(places)) NULL else places[places$health_area_name == s$health_area_name, ]
      data.frame(supervisor_id = s$supervisor_id, supervisor_name = s$supervisor_name, phone = s$phone, token = tok,
                 message = supervisor_message(s$supervisor_name, district(), s$health_area_name, run$data_to, pa, supervisor_link(tok)))
    })
    if (length(msgs)) vb_upsert_dispatches(rid, dplyr::bind_rows(msgs), user())
    send_refresh(send_refresh() + 1)
    showNotification(sprintf('%d messages ready.%s', nrow(sups),
                             if (any(!in_run)) sprintf(' %d supervisor(s) skipped: no team GPS data in their health area.', sum(!in_run)) else ''),
                     type = 'message')
  })

  send_rows <- reactive({
    send_refresh()
    d <- vb_get_dispatches(run_id())
    if (nrow(d) == 0) NULL else d
  })

  # Refresh Sent / Opened / Done every minute while this tab is open
  observe({
    req(input$main_tabs == 'send')
    invalidateLater(60000)
    send_refresh(isolate(send_refresh()) + 1)
  })

  output$send_table <- renderDT({
    d <- send_rows()
    validate(need(!is.null(d), 'Choose a saved list and click "Prepare messages".'))
    fmt_t <- function(x) ifelse(is.na(x), '', format(lubridate::with_tz(x, ODK_TZ), '%d %b %H:%M'))
    tbl <- data.frame(
      Send = sprintf('<a class="btn btn-success" target="_blank" href="%s" onclick="Shiny.setInputValue(\'sent_click\', {id: %d, nonce: Math.random()}, {priority: \'event\'})">Send</a>',
                     wa_link(d$phone, d$message), d$dispatch_id),
      `Health area` = d$health_area_name, Supervisor = d$supervisor_name, Phone = d$phone,
      `Missed places` = d$n,
      Sent = fmt_t(d$send_clicked_at),
      Opened = ifelse(d$n_opens > 0, fmt_t(d$last_opened_at), ''),
      Done = ifelse(d$n > 0, sprintf('%d of %d', d$closed, d$n), '–'),
      check.names = FALSE)
    datatable(tbl, rownames = FALSE, escape = FALSE, selection = 'single',
              # full window height (below the header bar)
              options = modifyList(DT_OPTS, list(scrollY = 'calc(100vh - 150px)', columnDefs = list(list(orderable = FALSE, targets = 0)))))
  })

  observeEvent(input$sent_click, {
    vb_mark_sent(input$sent_click$id)
    send_refresh(send_refresh() + 1)
  })

  output$send_preview <- renderUI({
    d <- send_rows(); i <- input$send_table_rows_selected
    if (is.null(d) || is.null(i)) return(p(class = 'hint', 'Click a row to see its message and open the supervisor\'s page.'))
    tagList(div(class = 'mini-label', sprintf('Message to %s', d$supervisor_name[i])),
            tags$pre(class = 'msg-preview', d$message[i]),
            # &preview=1: your look at their page isn't counted as them opening it
            tags$a(href = paste0(supervisor_link(d$token[i]), '&preview=1'), target = '_blank', icon('up-right-from-square'), ' Open their page'))
  })
}
