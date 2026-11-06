# =============================================================================
# mod_sites_tab.R -- Sites tab
#
# View, add, edit, move and remove microplan sites (IDP and nomadic
# settlements; fixed, cross-border and transit vaccination sites) in the
# ODK site registry (project 36 "sites" list; helpers/sites_registry.R).
#
#   - Uses the session's district (no picker). In a health-area session a
#     health-area dropdown is shown; in a team-area session the health area
#     is already chosen, so only the team-area dropdown is shown. Health /
#     team areas come from the session campaign's CURRENT (published)
#     versions.
#   - Map: pick a type under "Add a site", then click the map. Drag a
#     marker to move a site. Click a marker or table row to edit / remove.
#   - Placing or moving a site outside the selected health/team area warns
#     (naming the area it falls in) and then allows it; outside the
#     district is blocked.
#   - Every change is written to ODK immediately. If ODK is unreachable the
#     last saved copy is shown read-only.
# =============================================================================

# Site markers: the same shape + colour per type as the printed maps
# (.EXPORT_SITE_STYLE, helpers/printable_export.R), drawn as small SVG
# images on plain Leaflet markers (draggable).
#
# Not leaflet's awesome markers: this app loads its own Leaflet (ui.R, for
# paint-app.js) next to the R leaflet package's copy, and the awesome-
# markers plugin attaches to one copy while the map uses the other, so no
# awesome marker was ever drawn ("Cannot read properties of undefined
# (reading 'icon')" in the browser).
.SITE_TYPE_HEX <- setNames(.EXPORT_SITE_STYLE$fill, .EXPORT_SITE_STYLE$type)
.site_marker_svg <- function(type, fill = .SITE_TYPE_HEX[[type]]) {
  shape <- .EXPORT_SITE_STYLE$shape[.EXPORT_SITE_STYLE$type == type]
  body <- switch(as.character(shape),
    `24` = '<polygon points="11,2 20,19 2,19"/>',            # triangle (IDP)
    `23` = '<polygon points="11,1 21,11 11,21 1,11"/>',       # diamond (nomadic)
    `22` = '<rect x="3" y="3" width="16" height="16"/>',      # square (fixed)
    `25` = '<polygon points="2,3 20,3 11,20"/>',              # down triangle (cross-border)
    '<circle cx="11" cy="11" r="8"/>')                        # circle (transit)
  svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="22" height="22"><g fill="%s" stroke="white" stroke-width="2">%s</g></svg>',
                 fill, body)
  paste0("data:image/svg+xml;charset=UTF-8,", utils::URLencode(svg, reserved = TRUE))
}
.SITE_MARKER_URL <- vapply(.EXPORT_SITE_STYLE$type, .site_marker_svg, "")
.SITE_MARKER_URL_REMOVED <- vapply(.EXPORT_SITE_STYLE$type, function(t) .site_marker_svg(t, "#9ca3af"), "")

sitesTabUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    style = "padding: 12px 18px;",
    column(
      3,
      uiOutput(ns("context")),
      uiOutput(ns("ha_ui")),
      selectInput(ns("team"), "Team area", choices = c("All team areas" = ""), width = "100%"),
      uiOutput(ns("level_note")),
      tags$hr(style = "margin: 10px 0;"),
      div(class = "mini-label", "Site types shown"),
      checkboxGroupInput(ns("types_shown"), NULL, choices = setNames(names(SITE_TYPES), SITE_TYPES),
                         selected = names(SITE_TYPES)),
      checkboxInput(ns("show_removed"), "Also show removed sites (grey) -- click one to restore it", FALSE),
      tags$hr(style = "margin: 10px 0;"),
      div(class = "mini-label", "Add a site"),
      selectInput(ns("add_type"), NULL, width = "100%",
                  choices = c("Choose a type, then click the map" = "", setNames(names(SITE_TYPES), SITE_TYPES))),
      tags$p(style = "font-size: 11px; color: #64748b; margin-top: -6px;",
             "Drag a marker to move a site. Click a marker or a table row to edit or remove it."),
      tags$hr(style = "margin: 10px 0;"),
      uiOutput(ns("status")),
      div(style = "display: flex; gap: 6px; flex-wrap: wrap;",
          actionButton(ns("refresh"), "Reload from ODK", icon = icon("rotate"), class = "btn btn-default btn-sm"),
          downloadButton(ns("download"), "Excel", class = "btn btn-default btn-sm"))
    ),
    column(
      9,
      uiOutput(ns("offline_banner")),
      leaflet::leafletOutput(ns("map"), height = "560px"),
      div(style = "margin-top: 12px;", DT::DTOutput(ns("table")))
    )
  )
}

sitesTabServer <- function(id, campaign_id_r, district_r, health_area_r = reactive(NULL), username_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- registry data ------------------------------------------------------
    reg <- reactiveVal(NULL)
    load_registry <- function(force = FALSE) {
      reg(withProgress(message = "Loading sites from ODK...", value = 0.5, sites_registry_get(force = force)))
    }
    observeEvent(input$refresh, load_registry(force = TRUE), ignoreInit = TRUE)
    offline <- reactive(isTRUE(reg()$offline))

    # ---- district (the session's) --------------------------------------------------
    district_name <- reactive({ d <- district_r(); req(!is.null(d), nzchar(d)); d })
    district_sf <- reactive({
      d <- districts_shp_for_campaign(campaign_id_r())
      d <- d[d$district_name == district_name(), , drop = FALSE]
      req(nrow(d) > 0)
      sf::st_transform(safe_make_valid(d), 4326)
    })
    district_code <- reactive(district_sf()$DIST_UID[1])

    # Load the registry the first time a district is chosen.
    observeEvent(district_sf(), { if (is.null(reg())) load_registry() }, ignoreInit = FALSE)

    # ---- health areas / team areas (session campaign's current versions) ----
    ha_sf <- reactive({
      dn <- district_name(); cid <- campaign_id_r()
      if (is.null(cid)) return(NULL)
      v <- tryCatch(db_get_shared_version(pool, cid, dn), error = function(e) NULL)
      g <- if (is.null(v)) NULL else (v$snap$smoothed_dfa_sf %||% v$snap$saved_dfa_sf)
      if (is.null(g) || !nrow(g)) return(NULL)
      g <- g[!g$dfa_name %in% extra_dfa_names, , drop = FALSE]
      sf::st_transform(safe_make_valid(g), 4326)
    })
    team_sf <- reactive({
      ha <- ha_sf(); cid <- campaign_id_r()
      if (is.null(ha)) return(NULL)
      parts <- lapply(unique(ha$dfa_name), function(han) {
        tv <- tryCatch(db_get_current_team_area_version(pool, cid, district_name(), han), error = function(e) NULL)
        g <- if (is.null(tv)) NULL else (tv$snap$smoothed_team_sf %||% tv$snap$saved_team_sf)
        if (is.null(g) || !nrow(g)) return(NULL)
        g <- g[!g$dfa_name %in% extra_dfa_names, , drop = FALSE]
        if (!nrow(g)) return(NULL)
        g <- sf::st_transform(safe_make_valid(g[, "dfa_name"]), 4326)
        g$health_area <- han
        g
      })
      parts <- Filter(Negate(is.null), parts)
      if (!length(parts)) NULL else do.call(rbind, parts)
    })

    # Health area: fixed by the session (team-area session) or picked here.
    fixed_ha <- reactive({ h <- health_area_r(); if (is.null(h) || !nzchar(h)) NULL else h })
    sel_ha <- reactive(fixed_ha() %||% (if (nzchar(input$ha %||% "")) input$ha else NULL))
    # Team: value is "<health area>||<team area>" (team names repeat across
    # health areas); "" = all team areas.
    sel_team <- reactive({
      v <- input$team %||% ""
      if (!nzchar(v)) return(NULL)
      parts <- strsplit(v, "||", fixed = TRUE)[[1]]
      if (length(parts) != 2) return(NULL)
      list(ha = parts[1], team = parts[2])
    })
    level <- reactive(if (!is.null(sel_team())) "team" else if (!is.null(sel_ha())) "ha" else "district")

    output$context <- renderUI({
      tagList(
        div(class = "mini-label", "District"),
        tags$p(style = "font-size: 15px; font-weight: 600; margin: 0 0 8px;", district_name()),
        if (!is.null(fixed_ha())) tagList(
          div(class = "mini-label", "Health area"),
          tags$p(style = "font-size: 14px; font-weight: 600; margin: 0 0 8px;", fixed_ha())))
    })
    output$ha_ui <- renderUI({
      if (!is.null(fixed_ha())) return(NULL)
      ha <- ha_sf()
      ch <- if (is.null(ha)) character(0) else sort(unique(ha$dfa_name))
      sel <- isolate(input$ha)
      selectInput(ns("ha"), "Health area", width = "100%",
                  choices = c("Whole district" = "", ch),
                  selected = if (isTRUE(sel %in% ch)) sel else "")
    })
    observe({
      tm <- team_sf(); han <- sel_ha()
      if (!is.null(tm) && !is.null(han)) tm <- tm[tm$health_area == han, , drop = FALSE]
      ch <- if (is.null(tm) || !nrow(tm)) character(0) else {
        o <- order(tm$health_area, tm$dfa_name)
        setNames(paste(tm$health_area, tm$dfa_name, sep = "||")[o],
                 (if (is.null(han)) paste0(tm$dfa_name, " (", tm$health_area, ")") else tm$dfa_name)[o])
      }
      cur <- isolate(input$team)
      updateSelectInput(session, "team", choices = c("All team areas" = "", ch),
                        selected = if (isTRUE(cur %in% ch)) cur else "")
    })
    output$level_note <- renderUI({
      msg <- if (is.null(ha_sf())) {
        "No published health areas for this district in the session's campaign -- showing the whole district."
      } else if (!is.null(fixed_ha()) && !fixed_ha() %in% ha_sf()$dfa_name) {
        sprintf("Health area %s is not in the published health areas -- showing the whole district.", fixed_ha())
      } else if (is.null(team_sf())) {
        "No published team areas yet for this district."
      }
      if (!is.null(msg)) tags$p(style = "font-size: 11px; color: #b45309;", msg)
    })

    # Polygon of the selected level (NULL = whole district). A team-area
    # polygon has a health_area column; a health-area polygon does not.
    view_poly <- reactive({
      if (level() == "team" && !is.null(team_sf())) {
        t <- sel_team()
        g <- team_sf()[team_sf()$dfa_name == t$team & team_sf()$health_area == t$ha, , drop = FALSE]
        if (nrow(g)) return(g)
      }
      if (level() %in% c("ha", "team") && !is.null(ha_sf())) {
        g <- ha_sf()[ha_sf()$dfa_name == (sel_team()$ha %||% sel_ha()), , drop = FALSE]
        if (nrow(g)) return(g)
      }
      NULL
    })
    view_is_team <- reactive({ vp <- view_poly(); !is.null(vp) && "health_area" %in% names(vp) })
    view_label <- reactive({
      vp <- view_poly()
      if (is.null(vp)) district_name()
      else if (view_is_team()) paste("team area", sel_team()$team)
      else paste("health area", vp$dfa_name[1])
    })

    # Which health area / team area a point falls in.
    area_of <- function(lat, lon) {
      p <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
      s2 <- sf::sf_use_s2(); suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(s2)))
      inside <- function(g) if (is.null(g)) integer(0) else suppressMessages(sf::st_intersects(p, g)[[1]])
      hi <- inside(ha_sf()); ti <- inside(team_sf())
      list(in_district = length(inside(district_sf())) > 0,
           ha = if (length(hi)) ha_sf()$dfa_name[hi[1]] else NA_character_,
           team = if (length(ti)) team_sf()$dfa_name[ti[1]] else NA_character_)
    }

    # ---- sites in the district / in view ---------------------------------------
    # Every site in the district, removed ones included (they can be
    # shown and restored); district_sites() = active only.
    all_district_sites <- reactive({
      r <- reg(); req(!is.null(r$sites))
      s <- r$sites[r$sites$district_code == district_code(), , drop = FALSE]
      s <- sites_with_coords(s)
      s <- s[!is.na(s$lat) & !is.na(s$lon), , drop = FALSE]
      s$health_area <- NA_character_; s$team_area <- NA_character_
      if (nrow(s)) {
        pts <- sf::st_as_sf(s[, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
        s2 <- sf::sf_use_s2(); suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(s2)))
        if (!is.null(ha_sf())) {
          i <- vapply(suppressMessages(sf::st_intersects(pts, ha_sf())), function(x) if (length(x)) x[1] else NA_integer_, 1L)
          s$health_area <- ha_sf()$dfa_name[i]
        }
        if (!is.null(team_sf())) {
          i <- vapply(suppressMessages(sf::st_intersects(pts, team_sf())), function(x) if (length(x)) x[1] else NA_integer_, 1L)
          s$team_area <- team_sf()$dfa_name[i]
        }
      }
      s
    })
    district_sites <- reactive({ s <- all_district_sites(); s[s$status != "removed", , drop = FALSE] })
    view_sites <- reactive({
      s <- if (isTRUE(input$show_removed)) all_district_sites() else district_sites()
      s <- s[s$site_type %in% (input$types_shown %||% character(0)), , drop = FALSE]
      vp <- view_poly()
      if (!is.null(vp)) {
        s <- if (view_is_team()) s[s$team_area %in% sel_team()$team & s$health_area %in% sel_team()$ha, , drop = FALSE]
             else s[s$health_area %in% vp$dfa_name, , drop = FALSE]
      }
      s
    })

    # ---- status / banner ---------------------------------------------------------
    output$offline_banner <- renderUI({
      if (!offline()) return(NULL)
      div(style = "background:#fef3c7;border:1px solid #f59e0b;border-radius:6px;padding:8px 12px;margin-bottom:8px;font-size:13px;",
          icon("triangle-exclamation"), " ODK Central cannot be reached -- showing the copy saved ",
          format(reg()$fetched_at %||% NA, "%d %b %Y %H:%M"), ". Editing is switched off until it is back (Reload from ODK).")
    })
    output$status <- renderUI({
      s <- view_sites()
      n_removed <- sum(s$status == "removed")
      s <- s[s$status != "removed", , drop = FALSE]
      counts <- table(factor(s$site_type, levels = names(SITE_TYPES)))
      tagList(
        tags$p(style = "font-size: 12px; margin: 0 0 4px;", tags$b(nrow(s)), " sites in ", view_label(),
               if (n_removed) tags$span(style = "color: #94a3b8;", sprintf(" (+%d removed)", n_removed))),
        tags$ul(style = "font-size: 11px; color: #475569; padding-left: 16px; margin: 0 0 8px;",
                lapply(names(SITE_TYPES), function(t) if (counts[[t]] > 0)
                  tags$li(tags$img(src = .SITE_MARKER_URL[[t]], width = 12, height = 12, style = "vertical-align: -1px;"), " ",
                          sprintf("%s: %d", SITE_TYPES[[t]], counts[[t]])))),
        tags$p(style = "font-size: 11px; color: #94a3b8;",
               "Loaded ", format(reg()$fetched_at %||% Sys.time(), "%H:%M"))
      )
    })

    # ---- map -------------------------------------------------------------------------
    output$map <- leaflet::renderLeaflet({
      d <- district_sf()
      bb <- sf::st_bbox(d)
      leaflet::leaflet(options = leaflet::leafletOptions(doubleClickZoom = FALSE)) |>
        leaflet::addProviderTiles("OpenStreetMap", group = "Streets") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(baseGroups = c("Streets", "Satellite"), position = "topright") |>
        leaflet::addPolygons(data = d, fill = FALSE, color = "#0f172a", weight = 2.5, group = "district") |>
        leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) |>
        # Report marker drags back to Shiny as input$map_marker_dragend.
        htmlwidgets::onRender("
          function(el, x) {
            var map = this;
            // Tell Shiny the map now exists, so the outlines and markers
            // (sent through leafletProxy) are drawn onto it. Without this,
            // anything sent while the tab was hidden is lost.
            Shiny.setInputValue(el.id + '_ready', Date.now() + Math.random());
            map.on('layeradd', function(e) {
              var l = e.layer;
              if (l.options && l.options.draggable && !l._siteDragBound) {
                l._siteDragBound = true;
                l.on('dragend', function(ev) {
                  var p = ev.target.getLatLng();
                  Shiny.setInputValue(el.id + '_marker_dragend',
                    {id: ev.target.options.layerId, lat: p.lat, lng: p.lng, nonce: Math.random()});
                });
              }
            });
          }")
    })

    # Health / team area outlines; selected area highlighted and zoomed to.
    observe({
      req(input$map_ready)
      ha <- ha_sf(); tm <- team_sf(); vp <- view_poly()
      p <- leaflet::leafletProxy("map") |> leaflet::clearGroup("areas")
      if (!is.null(ha)) p <- p |> leaflet::addPolygons(data = ha, fill = FALSE, color = "#475569", weight = 1.5,
                                                         label = ~dfa_name, group = "areas")
      han <- sel_team()$ha %||% sel_ha()
      if (!is.null(tm) && !is.null(han) && any(tm$health_area == han))
        p <- p |> leaflet::addPolygons(data = tm[tm$health_area == han, ], fill = FALSE, color = "#94a3b8",
                                       weight = 1, dashArray = "4", label = ~dfa_name, group = "areas")
      if (!is.null(vp) && nrow(vp)) {
        p <- p |> leaflet::addPolygons(data = vp, fillColor = "#0d9488", fillOpacity = 0.08, color = "#0d9488",
                                       weight = 3, group = "areas")
        bb <- sf::st_bbox(vp)
        p |> leaflet::flyToBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      } else if (!is.null(district_sf())) {
        bb <- sf::st_bbox(district_sf())
        p |> leaflet::flyToBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      }
    })

    # Site markers. redraw() forces a redraw (e.g. to snap a dragged marker
    # back when a move is cancelled or blocked).
    redraw <- reactiveVal(0)
    observe({
      req(input$map_ready)
      redraw()
      s <- view_sites()
      p <- leaflet::leafletProxy("map") |> leaflet::clearGroup("sites")
      if (!nrow(s)) return()
      removed <- s$status == "removed"
      add <- function(p, k, draggable) {
        if (!any(k)) return(p)
        x <- s[k, , drop = FALSE]
        # unname(): htmlwidgets sends a NAMED vector to the browser as an
        # object rather than an array, and then no marker is drawn.
        urls <- unname((if (draggable) .SITE_MARKER_URL else .SITE_MARKER_URL_REMOVED)[x$site_type])
        icons <- leaflet::icons(iconUrl = urls, iconWidth = 22, iconHeight = 22, iconAnchorX = 11, iconAnchorY = 11)
        leaflet::addMarkers(p,
          lng = x$lon, lat = x$lat, layerId = x$`__id`, icon = icons, group = "sites",
          label = unname(sprintf("%s (%s)%s", x$site_name, SITE_TYPES[x$site_type], if (draggable) "" else " -- removed")),
          options = leaflet::markerOptions(draggable = draggable && !offline()))
      }
      p <- add(p, removed, FALSE)
      add(p, !removed, TRUE)
    })

    # ---- Excel download of the sites in view -------------------------------------
    download_df <- reactive({
        s <- view_sites()
        sub_lbl <- setNames(SITE_SUBTYPES$label, SITE_SUBTYPES$name)[s$site_subtype]
        data.frame(
          Site = s$site_name, Type = unname(SITE_TYPES[s$site_type]),
          Kind = ifelse(is.na(sub_lbl), s$site_subtype, sub_lbl), `Kind (other)` = s$subtype_other,
          District = s$district_name, `Health area` = s$health_area, `Team area` = s$team_area,
          Latitude = round(s$lat, 6), Longitude = round(s$lon, 6),
          `U5 population` = suppressWarnings(as.integer(s$u5_population)),
          `Last confirmed` = s$last_confirmed_date, `Crossing point` = s$crossing_name,
          `MHFL facility ID` = s$linked_facility_id, Notes = s$notes, Source = s$source,
          `Last edited by` = s$last_edited_by, `Last edited at` = s$last_edited_at, Status = s$status, `Site ID` = s$`__id`,
          check.names = FALSE, stringsAsFactors = FALSE)
    })
    output$download <- downloadHandler(
      filename = function() sprintf("sites_%s_%s.xlsx", gsub("[^A-Za-z0-9]+", "_", view_label()), format(Sys.Date(), "%Y%m%d")),
      content = function(file) writexl::write_xlsx(download_df(), file))

    # ---- table -----------------------------------------------------------------------
    output$table <- DT::renderDT({
      s <- view_sites()
      sub_lbl <- setNames(SITE_SUBTYPES$label, SITE_SUBTYPES$name)[s$site_subtype]
      sub_lbl <- ifelse(is.na(sub_lbl), s$site_subtype, sub_lbl)
      d <- data.frame(
        Site = s$site_name, Type = unname(SITE_TYPES[s$site_type]),
        Kind = unname(sub_lbl),
        `U5 pop.` = suppressWarnings(as.integer(s$u5_population)),
        `Health area` = s$health_area, `Team area` = s$team_area,
        `Last edited by` = s$last_edited_by,
        check.names = FALSE, stringsAsFactors = FALSE)
      if (isTRUE(input$show_removed)) d$Status <- ifelse(s$status == "removed", "Removed", "Active")
      DT::datatable(
        d,
        selection = "single", rownames = FALSE,
        options = list(pageLength = 10, dom = "ftip", order = list(list(1, "asc"), list(0, "asc"))))
    })

    # ---- edit / add modal ---------------------------------------------------------
    editing <- reactiveVal(NULL)   # list(id, version, lat, lon, site) ; id NULL = new
    pending <- reactiveVal(NULL)   # payload awaiting out-of-area confirmation

    subtype_choices <- function(type) {
      st <- SITE_SUBTYPES[SITE_SUBTYPES$site_type == type, , drop = FALSE]
      setNames(st$name, st$label)
    }

    # site: the site's current values (also used to re-show a new site's
    # form with what was typed); is_new: adding rather than editing.
    show_site_modal <- function(site = NULL, lat, lon, type, is_new = is.null(site)) {
      v <- function(k) { x <- site[[k]]; if (is.null(x) || length(x) == 0 || is.na(x[1])) "" else as.character(x[1]) }
      fac <- site_mhfl_facilities(district_code())
      showModal(modalDialog(
        title = if (is_new) paste("Add", tolower(SITE_TYPES[[type]])) else paste("Edit site:", site$site_name),
        size = "m", easyClose = FALSE,
        selectInput(ns("f_type"), "Site type", choices = setNames(names(SITE_TYPES), SITE_TYPES), selected = type),
        conditionalPanel(sprintf("['fixed','cross_border','transit'].indexOf(input['%s']) >= 0", ns("f_type")),
          selectInput(ns("f_subtype"), "What kind of site is it?", choices = subtype_choices(type),
                      selected = v("site_subtype"))),
        conditionalPanel(sprintf("(input['%s'] || '').indexOf('_other') >= 0", ns("f_subtype")),
          textInput(ns("f_subtype_other"), "Describe the kind of site", value = v("subtype_other"))),
        conditionalPanel(sprintf("input['%s'] == 'fixed' && input['%s'] == 'fixed_health_facility'", ns("f_type"), ns("f_subtype")),
          selectizeInput(ns("f_facility"), "Health facility (MHFL)",
                         choices = c("Not in the list" = "", fac), selected = v("linked_facility_id"),
                         options = list(placeholder = "Type to search, or leave blank if not listed"))),
        textInput(ns("f_name"), "Site name", value = v("site_name"), width = "100%"),
        fluidRow(column(6, numericInput(ns("f_lat"), "Latitude", value = round(lat, 6), step = 0.0001)),
                 column(6, numericInput(ns("f_lon"), "Longitude", value = round(lon, 6), step = 0.0001))),
        conditionalPanel(sprintf("['idp','nomadic'].indexOf(input['%s']) >= 0", ns("f_type")),
          fluidRow(
            column(6, numericInput(ns("f_u5"), "Estimated population aged under 5",
                                   value = suppressWarnings(as.numeric(v("u5_population"))), min = 0)),
            column(6, dateInput(ns("f_confirmed"), "Date presence last confirmed", value = Sys.Date(),
                                max = Sys.Date())))),
        conditionalPanel(sprintf("input['%s'] == 'cross_border'", ns("f_type")),
          textInput(ns("f_crossing"), "Crossing point name", value = v("crossing_name"), width = "100%")),
        textAreaInput(ns("f_notes"), "Notes", value = v("notes"), width = "100%", rows = 2),
        if (!is_new) tags$p(style = "font-size: 11px; color: #94a3b8;",
                            sprintf("Last edited by %s, %s", v("last_edited_by"), substr(v("last_edited_at"), 1, 16))),
        footer = tagList(
          if (!is_new) actionButton(ns("f_remove"), "Remove site...", class = "btn btn-danger", style = "float: left;"),
          modalButton("Cancel"),
          actionButton(ns("f_save"), if (is_new) "Add site" else "Save", class = "btn btn-primary"))
      ))
    }

    observeEvent(input$f_type, {
      updateSelectInput(session, "f_subtype", choices = subtype_choices(input$f_type),
                        selected = isolate(editing()$site$site_subtype) %||% character(0))
    }, ignoreInit = TRUE)
    # Picking an MHFL facility on a new site fills its name and GPS.
    observeEvent(input$f_facility, {
      ed <- editing(); req(!is.null(ed), is.null(ed$id), nzchar(input$f_facility %||% ""))
      pt <- site_mhfl_point(input$f_facility); req(!is.null(pt))
      updateTextInput(session, "f_name", value = pt$name)
      if (!is.na(pt$lat) && !is.na(pt$lon)) {
        updateNumericInput(session, "f_lat", value = round(pt$lat, 6))
        updateNumericInput(session, "f_lon", value = round(pt$lon, 6))
      }
    }, ignoreInit = TRUE)

    open_existing <- function(id) {
      s <- all_district_sites(); site <- s[s$`__id` == id, , drop = FALSE]
      req(nrow(site) == 1)
      editing(list(id = id, version = site$`__version`, site = as.list(site)))
      if (identical(site$status, "removed")) {
        reason <- REMOVAL_REASONS[site$removal_reason]
        showModal(modalDialog(
          title = paste("Removed site:", site$site_name),
          tags$p(sprintf("%s, removed by %s on %s.", SITE_TYPES[[site$site_type]] %||% site$site_type,
                         site$last_edited_by, substr(site$last_edited_at, 1, 10))),
          tags$p("Reason: ", if (is.na(reason)) site$removal_reason else reason),
          footer = tagList(modalButton("Close"),
                           actionButton(ns("f_restore"), "Restore site", class = "btn btn-primary"))))
        return(invisible())
      }
      show_site_modal(as.list(site), site$lat, site$lon, site$site_type)
    }
    guard_editable <- function() {
      if (offline()) { showNotification("ODK Central is unreachable -- editing is off.", type = "warning"); return(FALSE) }
      TRUE
    }

    observeEvent(input$map_click, {
      req(nzchar(input$add_type %||% ""), guard_editable())
      cl <- input$map_click
      editing(list(id = NULL, version = NULL, site = list()))
      show_site_modal(NULL, cl$lat, cl$lng, input$add_type)
    })
    observeEvent(input$map_marker_click, { req(guard_editable()); open_existing(input$map_marker_click$id) })
    observeEvent(input$table_rows_selected, {
      s <- view_sites(); i <- input$table_rows_selected
      req(length(i) == 1, guard_editable())
      leaflet::leafletProxy("map") |> leaflet::flyTo(s$lon[i], s$lat[i], zoom = 15)
      open_existing(s$`__id`[i])
    })

    # Build the property payload from the modal inputs.
    modal_payload <- function() {
      type <- input$f_type
      pop  <- type %in% c("idp", "nomadic")
      has_sub <- type %in% c("fixed", "cross_border", "transit")
      sub  <- if (has_sub) input$f_subtype %||% "" else ""
      hf   <- identical(sub, "fixed_health_facility")
      fac  <- if (hf) input$f_facility %||% "" else ""
      d <- sf::st_drop_geometry(district_sf())
      list(
        region_code = d$REG_CODE[1], district_code = d$DIST_UID[1], district_name = d$district_name[1],
        site_type = type, site_subtype = sub,
        subtype_other = if (grepl("_other$", sub)) trimws(input$f_subtype_other %||% "") else "",
        mhfl_match = if (hf) (if (nzchar(fac)) "listed" else "not_listed") else "",
        linked_facility_id = fac,
        site_name = trimws(input$f_name %||% ""),
        geometry = sprintf("%.6f %.6f 0 0", input$f_lat, input$f_lon),
        u5_population = if (pop && !is.na(input$f_u5 %||% NA)) as.character(round(input$f_u5)) else "",
        last_confirmed_date = if (pop) format(input$f_confirmed %||% Sys.Date(), "%Y-%m-%d") else "",
        crossing_name = if (type == "cross_border") trimws(input$f_crossing %||% "") else "",
        notes = trimws(input$f_notes %||% ""),
        .lat = input$f_lat, .lon = input$f_lon)
    }

    write_site <- function(ed, payload) {
      data <- payload[!startsWith(names(payload), ".")]
      user <- username_r() %||% "app user"
      res <- tryCatch({
        if (is.null(ed$id)) { sites_registry_create(data, user); "ok" }
        else sites_registry_update(ed$id, ed$version, data, user)
      }, error = function(e) { showNotification(paste("Could not save to ODK:", conditionMessage(e)), type = "error", duration = 8); "error" })
      if (identical(res, "conflict")) {
        showNotification("Someone else changed this site in the meantime -- reloaded the latest version. Please re-apply your change.",
                         type = "warning", duration = 8)
        load_registry(force = TRUE)
      } else if (identical(res, "ok")) {
        removeModal(); editing(NULL); pending(NULL)
        showNotification(if (is.null(ed$id)) "Site added." else "Site saved.", type = "message", duration = 3)
        reg(sites_registry_get())
      }
    }

    # Other active sites within NEAR_M metres of a point (possible duplicate).
    NEAR_M <- 100
    nearby_sites <- function(lat, lon, exclude_id = NULL) {
      s <- district_sites()
      if (!is.null(exclude_id)) s <- s[s$`__id` != exclude_id, , drop = FALSE]
      if (!nrow(s)) return(s)
      rad <- pi / 180
      d <- 2 * 6371000 * asin(sqrt(sin((s$lat - lat) * rad / 2)^2 +
                                   cos(lat * rad) * cos(s$lat * rad) * sin((s$lon - lon) * rad / 2)^2))
      s$dist_m <- d
      s <- s[d <= NEAR_M, , drop = FALSE]
      s[order(s$dist_m), , drop = FALSE]
    }

    # Checks before writing: outside the district = blocked; outside the
    # selected health/team area or close to another site = warn, then allow.
    check_and_write <- function(ed, payload) {
      a <- area_of(payload$.lat, payload$.lon)
      if (!a$in_district) {
        showNotification(sprintf("That point is outside %s. Sites must be inside the district.", district_name()),
                         type = "error", duration = 6)
        return(invisible())
      }
      warn <- list()
      vp <- view_poly()
      outside <- !is.null(vp) && (
        if (view_is_team()) !identical(a$team, sel_team()$team) || !identical(a$ha, sel_team()$ha)
        else !identical(a$ha, vp$dfa_name[1]))
      if (outside) {
        where <- if (view_is_team()) {
          if (is.na(a$team)) "outside every team area" else sprintf("in team area %s (health area %s)", a$team, a$ha)
        } else if (is.na(a$ha)) "outside every health area" else sprintf("in health area %s", a$ha)
        warn <- c(warn, list(tags$p(sprintf("This point is %s, not in %s.", where, view_label()))))
      }
      near <- nearby_sites(payload$.lat, payload$.lon, ed$id)
      if (nrow(near)) {
        warn <- c(warn, list(tags$p(sprintf("It is close to %s -- is it the same place?",
                                            if (nrow(near) == 1) "another site" else paste(nrow(near), "other sites"))),
                             tags$ul(lapply(seq_len(min(nrow(near), 5)), function(i)
                               tags$li(sprintf("%s (%s), %d m away", near$site_name[i],
                                               SITE_TYPES[[near$site_type[i]]] %||% near$site_type[i],
                                               round(near$dist_m[i])))))))
      }
      if (length(warn)) {
        pending(list(ed = ed, payload = payload))
        showModal(modalDialog(
          title = "Please check before saving", tagList(warn, tags$p("Save it anyway?")),
          footer = tagList(actionButton(ns("cancel_outside"), "Go back"),
                           actionButton(ns("confirm_outside"), "Save anyway", class = "btn btn-warning"))))
        return(invisible())
      }
      write_site(ed, payload)
    }
    # "Go back": a dragged marker snaps back; an add/edit returns to its form.
    observeEvent(input$cancel_outside, {
      pd <- pending(); pending(NULL); removeModal(); redraw(redraw() + 1)
      ed <- pd$ed
      if (!is.null(ed) && !is.null(pd$payload$site_type)) {
        site <- utils::modifyList(ed$site %||% list(), pd$payload[!startsWith(names(pd$payload), ".")])
        show_site_modal(site, pd$payload$.lat, pd$payload$.lon, pd$payload$site_type, is_new = is.null(ed$id))
      }
    })
    observeEvent(input$confirm_outside, { pd <- pending(); req(!is.null(pd$payload)); write_site(pd$ed, pd$payload) })

    observeEvent(input$f_save, {
      ed <- editing(); req(!is.null(ed))
      p <- modal_payload()
      problems <- c(
        if (!nzchar(p$site_name)) "a site name",
        if (p$site_type %in% c("fixed", "cross_border", "transit") && !nzchar(p$site_subtype)) "what kind of site it is",
        if (grepl("_other$", p$site_subtype) && !nzchar(p$subtype_other)) "a description of the kind of site",
        if (is.na(p$.lat %||% NA) || is.na(p$.lon %||% NA)) "latitude and longitude")
      if (length(problems)) {
        showNotification(paste("Please enter", paste(problems, collapse = ", "), "."), type = "warning", duration = 5)
        return()
      }
      check_and_write(ed, p)
    })

    # Remove (soft delete with a reason).
    observeEvent(input$f_remove, {
      ed <- editing(); req(!is.null(ed$id))
      showModal(modalDialog(
        title = paste("Remove", ed$site$site_name),
        tags$p("The site stays in ODK marked as removed, so syncs will not add it back."),
        selectInput(ns("f_reason"), "Why is it being removed?", choices = setNames(names(REMOVAL_REASONS), REMOVAL_REASONS)),
        footer = tagList(modalButton("Cancel"), actionButton(ns("f_confirm_remove"), "Remove", class = "btn btn-danger"))))
    })
    observeEvent(input$f_confirm_remove, {
      ed <- editing(); req(!is.null(ed$id))
      res <- tryCatch(sites_registry_remove(ed$id, ed$version, input$f_reason, username_r() %||% "app user"),
                      error = function(e) { showNotification(paste("Could not remove:", conditionMessage(e)), type = "error"); "error" })
      if (identical(res, "conflict")) {
        showNotification("Someone else changed this site in the meantime -- reloaded.", type = "warning", duration = 6)
        load_registry(force = TRUE)
      } else if (identical(res, "ok")) {
        removeModal(); editing(NULL)
        showNotification("Site removed.", type = "message", duration = 3)
        reg(sites_registry_get())
      }
    })

    observeEvent(input$f_restore, {
      ed <- editing(); req(!is.null(ed$id), guard_editable())
      res <- tryCatch(sites_registry_update(ed$id, ed$version, list(status = "active", removal_reason = ""),
                                            username_r() %||% "app user"),
                      error = function(e) { showNotification(paste("Could not restore:", conditionMessage(e)), type = "error"); "error" })
      if (identical(res, "conflict")) {
        showNotification("Someone else changed this site in the meantime -- reloaded.", type = "warning", duration = 6)
        load_registry(force = TRUE)
      } else if (identical(res, "ok")) {
        removeModal(); editing(NULL)
        showNotification("Site restored.", type = "message", duration = 3)
        reg(sites_registry_get())
      }
    })

    # Drag a marker to move a site (confirm first).
    observeEvent(input$map_marker_dragend, {
      dr <- input$map_marker_dragend
      s <- district_sites(); site <- s[s$`__id` == dr$id, , drop = FALSE]
      req(nrow(site) == 1)
      if (!guard_editable()) { redraw(redraw() + 1); return() }
      ed <- list(id = dr$id, version = site$`__version`, site = as.list(site))
      editing(ed)
      pending(list(ed = ed, lat = dr$lat, lng = dr$lng))
      showModal(modalDialog(
        title = paste("Move", site$site_name),
        sprintf("Move this site to %.5f, %.5f?", dr$lat, dr$lng),
        footer = tagList(actionButton(ns("cancel_move"), "Cancel"),
                         actionButton(ns("confirm_move"), "Move", class = "btn btn-primary"))))
    })
    observeEvent(input$cancel_move, { removeModal(); pending(NULL); redraw(redraw() + 1) })
    observeEvent(input$confirm_move, {
      pd <- pending(); req(!is.null(pd$lat))
      site <- pd$ed$site
      payload <- list(geometry = sprintf("%.6f %.6f 0 0", pd$lat, pd$lng), .lat = pd$lat, .lon = pd$lng)
      check_and_write(pd$ed, payload)
      # Snap the marker back if the move was not written (blocked); a
      # successful write redraws from the updated registry anyway.
      redraw(redraw() + 1)
    })
  })
}
