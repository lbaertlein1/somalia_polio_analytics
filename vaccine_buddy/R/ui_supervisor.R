# =============================================================================
# ui_supervisor.R -- the page a supervisor opens from WhatsApp (?t=<token>)
#
# Phone-first, in the health area adjuster's look (slate header, teal
# accent, Bootstrap 3): map on top (their health area, numbered missed
# places, facilities, IDP sites, "you are here"), then the places grouped
# by team, most children first. Each place has Navigate (Google Maps),
# Done, Can't reach and No children; status is saved so the admin side can follow up.
#
# Teams: a place's team is its adjuster team area where one is mapped,
# otherwise the vaccination team that passed closest
# (assign_teams() in analysis.R). "Send to team" forwards a team its places
# on WhatsApp with a link to this page showing only them (&team=). The
# supervisor can also tick any places and send those (&p=<numbers>) to
# hand out work differently. Team leaders opening those links can mark
# places done too.
# =============================================================================

STATUS_COLORS <- c(open = '#dc2626', done = '#16a34a', unreachable = '#64748b', no_children = '#a8a29e')
# unname(): leaflet ignores a named colour vector
status_col <- function(status) unname(STATUS_COLORS[status])

supervisor_ui <- function() {
  fluidPage(
    title = 'Missed places',
    tags$head(
      tags$meta(name = 'viewport', content = 'width=device-width, initial-scale=1'),
      tags$style(HTML("
        body { background: #f0f4f8; padding: 0 0 90px; font-family: -apple-system, 'Segoe UI', Roboto, sans-serif; }
        .container-fluid { padding: 0 !important; }
        .sup-head { background: #1e293b; color: #f1f5f9; padding: 12px 16px 10px; }
        .sup-head h1 { font-size: 17px; font-weight: 600; margin: 0; }
        .sup-head .sub { color: #5eead4; font-size: 12px; margin-top: 2px; }
        .sup-stats { display: flex; gap: 18px; margin-top: 8px; font-size: 13px; color: #cbd5e1; }
        .sup-stats b { font-size: 17px; color: #fff; }
        .newer { background: #fef3c7; padding: 8px 16px; font-size: 13px; }
        .howto { padding: 7px 16px; font-size: 12px; color: #334155; background: #f1f5f9; border-bottom: 1px solid #e2e8f0; }
        .sup-body { padding: 10px 12px; }
        .mini-label { font-size: 11px; color: #64748b; margin-bottom: 3px; font-weight: 500; }
        .group-h { display: flex; justify-content: space-between; align-items: center; margin: 16px 2px 6px; gap: 8px; }
        .group-h h2 { font-size: 15px; font-weight: 600; color: #0f172a; margin: 0; }
        .group-h .why { font-size: 11px; color: #64748b; font-weight: 400; display: block; }
        .place { background: #fff; border-radius: 8px; padding: 10px 12px; margin-bottom: 8px;
                 border: 1px solid #e2e8f0; border-left: 6px solid #dc2626; }
        .place.done { border-left-color: #16a34a; opacity: .65; }
        .place.unreachable, .place.no_children { border-left-color: #64748b; opacity: .65; }
        .place.picked { box-shadow: 0 0 0 2px #0d9488; }
        .place .top { display: flex; gap: 10px; align-items: flex-start; }
        .place .pick { width: 22px; height: 22px; margin-top: 1px; flex: none; accent-color: #0d9488; }
        .place .t { font-weight: 600; font-size: 14px; color: #0f172a; cursor: pointer; }
        .place .m { color: #475569; font-size: 12px; margin: 2px 0 8px; }
        .place .btns { display: flex; gap: 6px; flex-wrap: wrap; }
        .b { display: inline-block; border-radius: 6px; padding: 8px 12px; font-size: 13px; font-weight: 600;
             border: 1px solid transparent; text-decoration: none !important; cursor: pointer; line-height: 1.2; }
        .b-nav  { background: #0d9488; color: #fff !important; }
        .b-done { background: #fff; color: #15803d; border-color: #86efac; }
        .b-skip { background: #fff; color: #475569; border-color: #cbd5e1; }
        .b-wa   { background: #16a34a; color: #fff !important; }
        .b-sm   { padding: 6px 10px; font-size: 12px; }
        .pickbar { position: fixed; left: 0; right: 0; bottom: 0; z-index: 1000; background: #fff;
                   border-top: 1px solid #e2e8f0; padding: 10px 12px; display: flex; gap: 8px;
                   box-shadow: 0 -2px 8px rgba(0,0,0,.08); }
        .pickbar .b-wa { flex: 1; text-align: center; }
        .team-label { color: #1d4ed8 !important; font-weight: 700; font-size: 13px !important;
                      text-shadow: 0 0 3px #fff, 0 0 3px #fff, 0 0 3px #fff; }
        .lm-label { font-size: 10px !important; font-weight: 600; text-shadow: 0 0 2px #fff, 0 0 2px #fff, 0 0 2px #fff; }
        .num-label { color: #fff !important; font-weight: 700; font-size: 11px !important; pointer-events: none; }
        .form-group { margin-bottom: 0; }
      ")),
      tags$script(HTML("
        function placeAction(id, status) {
          Shiny.setInputValue('place_action', {id: id, status: status, nonce: Math.random()}, {priority: 'event'});
        }
        function placeFocus(id) {
          Shiny.setInputValue('place_focus', {id: id, nonce: Math.random()}, {priority: 'event'});
          window.scrollTo({top: 0, behavior: 'smooth'});
        }
        function placePick(el, id) {
          el.closest('.place').classList.toggle('picked', el.checked);
          Shiny.setInputValue('place_pick', {id: id, on: el.checked, nonce: Math.random()}, {priority: 'event'});
        }
      "))
    ),
    uiOutput('sup_head'),
    leafletOutput('sup_map', height = '50vh'),
    div(class = 'sup-body',
        div(style = 'display:flex; gap:12px; align-items:center;',
            uiOutput('sup_team_ui', style = 'flex:1'),
            checkboxInput('sup_tracks', 'Team GPS tracks', value = FALSE),
            checkboxInput('sup_show_closed', 'Show done', value = FALSE)),
        uiOutput('sup_list')),
    uiOutput('sup_pickbar')
  )
}

supervisor_server <- function(input, output, session, token) {
  preview <- identical(parseQueryString(isolate(session$clientData$url_search))$preview, '1')
  info <- tryCatch(vb_open_dispatch(token, count_open = !preview), error = function(e) { message('[sup] ', conditionMessage(e)); NULL })
  if (is.null(info)) {
    output$sup_head <- renderUI(div(class = 'sup-head', tags$h1('Link not valid'),
                                    div(class = 'sub', 'Ask your district focal point for a new link.')))
    return(invisible())
  }

  layers <- tryCatch(adj_area_layers(info$campaign_id, info$district_name, info$health_area_name),
                     error = function(e) list())
  places <- reactiveVal(vb_get_places(info$run_id, info$district_name, info$health_area_name))
  all0 <- isolate(places())
  # The settings the list was made with
  run_params <- utils::modifyList(DEFAULT_PARAMS, tryCatch(as.list(jsonlite::fromJSON(info$params_json)), error = function(e) list()))
  # The health area's extent -- or, where no health areas are drawn (they
  # came from the team names), around its missed places
  has_ha <- !is.null(layers$health_area) && nrow(layers$health_area) > 0
  area_bb <- if (has_ha) sf::st_bbox(layers$health_area)
             else if (!is.null(all0) && nrow(all0) > 0) sf::st_bbox(sf::st_union(all0$outline)) + c(-0.005, -0.005, 0.005, 0.005)
             else NULL

  # Links forwarded to a team narrow the page: &team=<assigned team> or
  # &p=<place numbers> (places the supervisor ticked for that team).
  q <- parseQueryString(isolate(session$clientData$url_search))
  teams <- base::sort(base::unique(stats::na.omit(all0$assigned_team)))
  team_q <- if (!is.null(q$team) && q$team %in% teams) q$team else NULL
  pick_q <- if (!is.null(q$p)) suppressWarnings(as.integer(strsplit(q$p, ',')[[1]])) else NULL
  team_view <- !is.null(team_q) || length(pick_q) > 0

  in_view <- function(p) {
    if (is.null(p)) return(p)
    if (length(pick_q)) p <- p[p$place_no %in% pick_q, ]
    if (!is.null(team_q)) p <- p[p$assigned_team %in% team_q, ]
    if (!team_view && !(input$sup_team %||% 'all') %in% c('all', '')) p <- p[p$assigned_team %in% input$sup_team, ]
    p
  }
  shown <- reactive({
    p <- in_view(places())
    if (!is.null(p) && !isTRUE(input$sup_show_closed)) p <- p[p$status == 'open', ]
    p
  })
  picked <- reactiveVal(integer(0))   # place_ids ticked for sending to a team
  list_redraw <- reactiveVal(0)       # bumped to clear ticks on screen

  output$sup_team_ui <- renderUI({
    if (team_view || length(teams) < 2) return(NULL)
    selectInput('sup_team', NULL, choices = c('All teams' = 'all', setNames(teams, team_label(teams))), selected = 'all', width = '100%')
  })

  output$sup_head <- renderUI({
    p <- in_view(places())
    open <- if (is.null(p)) 0 else sum(p$status == 'open')
    kids <- if (is.null(p)) 0 else sum(p$u5_pop[p$status == 'open'])
    title <- if (!is.null(team_q)) sprintf('%s – %s', team_label(team_q), info$health_area_name)
             else if (length(pick_q)) sprintf('Your places – %s', info$health_area_name)
             else sprintf('%s – %s', info$health_area_name, info$district_name)
    tagList(
      if (!is.null(info$newer_token))
        div(class = 'newer', 'A newer list is available: ', tags$a(href = paste0('?t=', info$newer_token), 'open it')),
      div(class = 'sup-head',
          tags$h1(title),
          div(class = 'sub', sprintf('Missed places · vaccination team GPS up to %s', format(as.Date(info$data_to), '%d %b %Y'))),
          div(class = 'sup-stats',
              div(tags$b(open), if (open == 1) ' place left' else ' places left'),
              div(tags$b(.fmt_n(kids)), ' children under 5 (est.)'),
              if (!is.null(p)) div(tags$b(sum(p$status != 'open')), ' done'))),
      div(class = 'howto',
          'Red areas have houses the vaccination teams did not pass. Tap ', tags$b('Navigate'), ' for directions and ',
          tags$b('Done'), ' once the children there are vaccinated.',
          if (!team_view) tagList(' Use ', tags$b('Send to team'), ' to give each team its places on WhatsApp, or tick places to send them to any team.'))
    )
  })

  # --- Map: static layers once, places via proxy so the view isn't reset ----
  output$sup_map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addMapPane('tracks', zIndex = 405) |>
      addMapPane('places', zIndex = 420) |>
      addProviderTiles(providers$OpenStreetMap, group = 'Map') |>
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') |>
      addLayersControl(baseGroups = c('Map', 'Satellite'), options = layersControlOptions(collapsed = TRUE)) |>
      addControlGPS(options = gpsOptions(position = 'topleft', activate = TRUE, autoCenter = FALSE))
    if (!is.null(layers$team_areas) && nrow(layers$team_areas) > 0)
      m <- m |> addPolygons(data = layers$team_areas, color = '#2563eb', weight = 2, dashArray = '6 4', fill = FALSE) |>
        addLabelOnlyMarkers(data = suppressWarnings(sf::st_point_on_surface(layers$team_areas)), label = ~team_area,
                            labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE, className = 'team-label'))
    if (has_ha)
      m <- m |> addPolygons(data = layers$health_area, color = '#0f172a', weight = 3, fill = FALSE)
    m <- add_facilities(m, layers$facilities)
    # Zoom: forwarded team links to their places, otherwise the health area
    focus <- in_view(all0)
    bb <- if (team_view && !is.null(focus) && nrow(focus) > 0) sf::st_bbox(sf::st_buffer(sf::st_union(focus$outline), 0.001))
          else area_bb
    if (!is.null(bb)) m <- m |> fitBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
    dot <- function(col) sprintf('<span style="display:inline-block;width:11px;height:11px;border-radius:50%%;background:%s;border:1.5px solid #fff;box-shadow:0 0 0 1px #94a3b8;vertical-align:-1px;"></span>', col)
    legend <- paste0(
      '<div style="background:#fff;padding:6px 9px;border-radius:6px;font-size:11px;line-height:1.7;box-shadow:0 1px 4px rgba(0,0,0,.25);">',
      dot(STATUS_COLORS[['open']]), ' Missed place<br>',
      dot(STATUS_COLORS[['done']]), ' Done<br>',
      '<span style="display:inline-block;width:14px;border-top:2px solid ', TRACK_LINE, ';vertical-align:3px;"></span> Team GPS track<br>',
      '<img src="', FACILITY_ICON$iconUrl, '" width="11" height="11" style="vertical-align:-1px;"> Health facility</div>')
    m |> addControl(htmltools::HTML(legend), position = 'bottomright')
  })

  observe({
    p <- shown()
    proxy <- leafletProxy('sup_map') |> clearGroup('places')
    if (is.null(p) || nrow(p) == 0) return()
    outl <- sf::st_sf(place_id = p$place_id, status = p$status, geometry = p$outline)
    proxy |> addPolygons(data = outl, group = 'places', color = '#334155', weight = 1.5,
                         fillColor = status_col(outl$status), fillOpacity = 0.35, options = pathOptions(pane = 'places'))
    # Numbered badge: a coloured circle (tap -> popup with Navigate) with
    # the place number drawn on top; the number passes taps through.
    proxy |>
      addCircleMarkers(data = p, group = 'places', lng = ~lon, lat = ~lat, radius = 11, color = '#fff', weight = 2,
                       fillColor = status_col(p$status), fillOpacity = 1, options = pathOptions(pane = 'places'),
                       popup = ~sprintf('<b>#%d</b> %s<br>~%s children under 5<br>%s<br><a href="%s" target="_blank"><b>Navigate ▶</b></a>',
                                        place_no, htmltools::htmlEscape(label), .fmt_n(u5_pop), team_label(assigned_team), gmaps_nav(lat, lon))) |>
      addLabelOnlyMarkers(data = p, group = 'places', lng = ~lon, lat = ~lat, label = ~as.character(place_no),
                          labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE, className = 'num-label'))
  })

  # --- Team GPS tracks (off by default: saves mobile data) ---------------------
  # Only this health area's own teams -- or just the team / ticked places'
  # teams in a link forwarded to a team. Loaded once, when first switched on.
  ha_teams <- base::union(trimws(strsplit(info$teams %||% '', ',')[[1]]), teams)
  ha_teams <- ha_teams[nzchar(ha_teams)]
  track_pings <- reactive({
    req(isTRUE(input$sup_tracks), !is.null(area_bb), length(ha_teams) > 0)
    rows <- vb_load_entries(info$data_from, info$data_to, area_bb)
    rows <- rows[rows$team_name %in% ha_teams, ]
    if (nrow(rows) == 0) return(NULL)
    clean_pings(rows, run_params)$pings
  })
  observe({
    proxy <- leafletProxy('sup_map') |> clearGroup('tracks')
    req(isTRUE(input$sup_tracks))
    pg <- tryCatch(track_pings(), error = function(e) NULL)
    if (is.null(pg)) return()
    show <- if (!is.null(team_q)) team_q
            else if (length(pick_q)) base::unique(all0$assigned_team[all0$place_no %in% pick_q])
            else if (!(input$sup_team %||% 'all') %in% c('all', '')) input$sup_team
            else ha_teams
    add_tracks(proxy, pg[pg$team_name %in% show, ], run_params, pane = 'tracks', max_points = 8000)
  })

  observeEvent(input$place_focus, {
    p <- places()
    r <- p[p$place_id == input$place_focus$id, ]
    if (nrow(r) == 1) leafletProxy('sup_map') |> setView(r$lon, r$lat, zoom = 17)
  })

  observeEvent(input$place_action, {
    a <- input$place_action
    req(a$status %in% names(STATUS_COLORS))
    ok <- tryCatch({ vb_set_place_status(a$id, a$status, via = paste0(if (team_view) 'team:' else 'supervisor:', info$dispatch_id)); TRUE },
                   error = function(e) FALSE)
    if (!ok) return(showNotification('Could not save – check your connection and try again.', type = 'error'))
    p <- places()
    p$status[p$place_id == a$id] <- a$status
    places(p)
    if (a$status != 'open') picked(setdiff(picked(), a$id))
  })

  observeEvent(input$place_pick, {
    id <- as.integer(input$place_pick$id)
    picked(if (isTRUE(input$place_pick$on)) union(picked(), id) else setdiff(picked(), id))
  })

  # --- Send ticked places to a team ---------------------------------------------
  output$sup_pickbar <- renderUI({
    ids <- picked()
    if (team_view || length(ids) == 0) return(NULL)
    p <- as.data.frame(sf::st_drop_geometry(places()))
    p <- p[p$place_id %in% ids, ]
    link <- paste0(supervisor_link(token), '&p=', paste(sort(p$place_no), collapse = ','))
    div(class = 'pickbar',
        tags$a(class = 'b b-wa', target = '_blank',
               href = wa_link(NULL, team_message(NULL, info$health_area_name, p, link)),
               onclick = "setTimeout(function(){ Shiny.setInputValue('pick_clear', Math.random()); }, 500)",
               icon('whatsapp'), sprintf(' Send %d ticked %s to a team', nrow(p), if (nrow(p) == 1) 'place' else 'places')),
        tags$button(class = 'b b-skip', onclick = "Shiny.setInputValue('pick_clear', Math.random())", 'Clear'))
  })
  observeEvent(input$pick_clear, { picked(integer(0)); list_redraw(list_redraw() + 1) })

  # --- List, grouped by team -------------------------------------------------------
  place_card <- function(r, is_picked) {
    open <- r$status == 'open'
    idp_note <- !is.na(r$near_idp) && !grepl(r$near_idp, r$label, fixed = TRUE)
    div(class = paste('place', r$status, if (is_picked) 'picked'),
        div(class = 'top',
            if (open && !team_view)
              tags$input(type = 'checkbox', class = 'pick', checked = if (is_picked) NA,
                         onchange = sprintf('placePick(this, %d)', r$place_id)),
            div(style = 'flex:1',
                div(class = 't', onclick = sprintf('placeFocus(%d)', r$place_id), sprintf('#%d  %s', r$place_no, r$label)),
                div(class = 'm', sprintf('~%s children under 5', .fmt_n(r$u5_pop)),
                    if (!open) tags$b(sprintf(' · %s', c(done = 'Done', unreachable = "Can't reach", no_children = 'No children')[[r$status]])),
                    if (idp_note) tags$span(style = 'color:#7c3aed; font-weight:600;', sprintf(' · at %s IDP site', r$near_idp)),
                    if (identical(r$team_source, 'nearest carrier') && !is.na(r$team_dist_m))
                      sprintf(' · %s passed %s m away', team_label(r$assigned_team), format(r$team_dist_m, big.mark = ','))))),
        div(class = 'btns',
            tags$a(class = 'b b-nav', href = gmaps_nav(r$lat, r$lon), target = '_blank', icon('location-arrow'), ' Navigate'),
            if (open) tagList(
              tags$button(class = 'b b-done', onclick = sprintf("placeAction(%d,'done')", r$place_id), icon('check'), ' Done'),
              tags$button(class = 'b b-skip', onclick = sprintf("placeAction(%d,'unreachable')", r$place_id), "Can't reach"),
              tags$button(class = 'b b-skip', onclick = sprintf("placeAction(%d,'no_children')", r$place_id), 'No children'))
            else tags$button(class = 'b b-skip', onclick = sprintf("placeAction(%d,'open')", r$place_id), 'Undo')))
  }

  output$sup_list <- renderUI({
    list_redraw()
    p <- shown()
    if (is.null(p) || nrow(p) == 0)
      return(div(style = 'padding: 20px; text-align: center; color: #64748b;', 'No open missed places. Well done!'))
    p <- as.data.frame(sf::st_drop_geometry(p))
    p <- p[order(p$place_no), ]
    sel <- isolate(picked())
    cards <- function(d) lapply(seq_len(nrow(d)), function(i) place_card(d[i, ], d$place_id[i] %in% sel))

    if (team_view || length(teams) == 0) return(tagList(
      div(class = 'group-h', tags$h2(sprintf('%d %s, most children first', nrow(p), if (nrow(p) == 1) 'place' else 'places'))),
      cards(p)))

    # Teams with the most children to reach first
    all_open <- as.data.frame(sf::st_drop_geometry(places()))
    all_open <- all_open[all_open$status == 'open', ]
    p$team <- dplyr::coalesce(p$assigned_team, '')
    order_teams <- names(sort(tapply(p$u5_pop, p$team, sum), decreasing = TRUE))
    lapply(order_teams, function(team) {
      tp <- p[p$team == team, ]
      team_open <- all_open[dplyr::coalesce(all_open$assigned_team, '') == team, ]
      tagList(
        div(class = 'group-h',
            tags$h2(sprintf('%s · %d %s', if (nzchar(team)) team_label(team) else 'No team',
                            nrow(tp), if (nrow(tp) == 1) 'place' else 'places'),
                    if (identical(tp$team_source[1], 'nearest carrier'))
                      tags$span(class = 'why', 'the team that passed closest')),
            if (nrow(team_open) > 0 && nzchar(team))
              tags$a(class = 'b b-wa b-sm', target = '_blank',
                     href = wa_link(NULL, team_message(team, info$health_area_name, team_open,
                                                       paste0(supervisor_link(token), '&team=', utils::URLencode(team, reserved = TRUE)))),
                     icon('whatsapp'), ' Send to team')),
        cards(tp))
    })
  })
}
