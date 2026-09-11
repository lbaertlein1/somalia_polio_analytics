# =============================================================================
# mod_intro_tab_v2.R
#
# Complete redesign. The intro page is now the district-status table and its
# two drill-down pickers — the ONLY entry point into a district's work.
# There is no more silent auto-resume, no automatic branch/carry-forward/
# blank modal, and no linear region-then-district cascading selector. Region
# survives only as an informational column on the table.
#
# Two drill-downs per district row, each a modal:
#   - "Health Areas" — a single-step picker: current (pinned top, default
#     action) or a dropdown of the user's own drafts + "Start blank".
#     Landing tab after activation is always tab_orientation — a blank
#     draft has no landmarks/facilities yet, and a resumed draft's
#     already-completed stages just show as already done, same as the old
#     app's restore-a-snapshot behavior always worked.
#   - "Team Areas" — two steps: first pick a health area (from the
#     district's CURRENT health-area version's dfa_names — team areas are
#     unavailable entirely for a district with no current health-area
#     version yet), then the SAME current-or-draft picker one level down,
#     scoped to that health area. Landing tab is tab_team_area_mapping
#     directly — team areas don't depend on orientation/facilities being
#     revisited, only on the health-area version being available, which
#     this module activates silently in the background (the user never
#     sees a health-area picker for this path — it's always "whatever is
#     currently current district-wide", per the rule that team areas can
#     only ever be drawn against a district's current health-area map).
#
# This module doesn't call session-manager activation functions directly —
# those instances live in server.R, instantiated once per session alongside
# the health-area/team-area tabs, not owned by this module. Instead it
# exposes two request reactives (activate_health_area_request,
# activate_team_area_request) that server.R observes and acts on — actually
# calling activate_version_id()/activate_team_area_version_id() and
# switching tabs. Keeps this module decoupled from session-manager instances
# it has no other reason to know about.
# =============================================================================

introTabUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 12,
      div(
        style = 'padding: 20px 28px;',

        div(
          style = 'display:flex;align-items:center;gap:16px;margin-bottom:16px;',
          div(
            style = 'flex:1;',
            tags$h2(style = 'font-size: 20px; font-weight: 600; color: #0f172a; margin: 0 0 2px;',
                    'District Health Area Planning Tool'),
            tags$p(style = 'font-size: 13px; color: #64748b; margin: 0;',
                   'Select a campaign to see progress across its districts.')
          ),
          # The User Guide link used to live here -- it's now in the
          # persistent app-header bar (see ui.R) instead, so it's visible
          # from every tab rather than just this one.
          div(
            style = 'width:260px;',
            div(class = 'mini-label', 'Campaign'),
            selectInput(ns('campaign'), NULL,
                       choices  = setNames('', 'Select campaign...'),
                       selected = '', width = '100%')
          )
        ),

        div(
          id = ns('intro_loading_overlay'),
          style = 'padding:24px;text-align:center;color:#64748b;font-size:13px;',
          icon('spinner', class = 'fa-spin'), ' Loading district data...'
        ),
        # Per-output spinners for the cards/table/map -- the overlay
        # above only covers the FIRST load (hidden as soon as
        # campaign_district_status() resolves); it says nothing about
        # how long the cards/table/map's own renderUI()/renderLeaflet()
        # calls then take to actually draw, which can be a further
        # visible delay (especially the leaflet map) with no indicator
        # at all previously. Each wrap below listens for Shiny's own
        # shiny:recalculating/shiny:recalculated events (fired on
        # document, event.name = the output's namespaced id) rather
        # than a new package dependency (shinycssloaders isn't used
        # anywhere else in this app) -- both events are part of
        # Shiny.js's public, documented API.
        tags$style(HTML(sprintf('
          .intro-loading-wrap { position: relative; min-height: 40px; }
          .intro-loading-wrap.intro-is-loading > *:not(.intro-spinner-overlay) { opacity: 0.35; transition: opacity 0.15s; }
          .intro-spinner-overlay {
            display: none; position: absolute; inset: 0; z-index: 5;
            align-items: center; justify-content: center;
            font-size: 13px; color: #64748b; background: rgba(255,255,255,0.5);
          }
          .intro-loading-wrap.intro-is-loading .intro-spinner-overlay { display: flex; }
        '))),
        tags$script(HTML(sprintf("
          (function() {
            var ids = ['%s', '%s', '%s'];
            function wrapId(id) { return '#' + $.escapeSelector(id) + '-wrap'; }
            $(document).on('shiny:recalculating', function(ev) {
              if (ids.indexOf(ev.name) > -1) $(wrapId(ev.name)).addClass('intro-is-loading');
            });
            $(document).on('shiny:recalculated', function(ev) {
              if (ids.indexOf(ev.name) > -1) $(wrapId(ev.name)).removeClass('intro-is-loading');
            });
          })();
        ", ns('campaign_map_cards'), ns('district_table_ui'), ns('campaign_map')))),
        shinyjs::hidden(
          div(
            id = ns('intro_loaded_content'),
            div(id = paste0(ns('campaign_map_cards'), '-wrap'), class = 'intro-loading-wrap',
              div(class = 'intro-spinner-overlay', icon('spinner', class = 'fa-spin'), HTML('&nbsp;Loading...')),
              uiOutput(ns('campaign_map_cards'))
            ),
            fluidRow(
              column(width = 6,
                div(id = paste0(ns('district_table_ui'), '-wrap'), class = 'intro-loading-wrap',
                  div(class = 'intro-spinner-overlay', icon('spinner', class = 'fa-spin'), HTML('&nbsp;Loading...')),
                  uiOutput(ns('district_table_ui'))
                )
              ),
              column(width = 6,
                div(class = 'mini-label', style = 'margin-bottom: 4px;', 'Campaign map'),
                div(id = paste0(ns('campaign_map'), '-wrap'), class = 'intro-loading-wrap',
                  div(class = 'intro-spinner-overlay', icon('spinner', class = 'fa-spin'), HTML('&nbsp;Loading...')),
                  leaflet::leafletOutput(ns('campaign_map'), height = '520px')
                )
              )
            )
          )
        ),

        tags$hr(style = 'border-color: #e2e8f0; margin: 28px 0 20px;'),

        div(
          style = 'max-width: 1100px;',

        .intro_section('Objective',
                       tags$p(
                         style = 'font-size: 13px; color: #475569; line-height: 1.7; margin: 0;',
                         'This tool supports district teams in preparing for community outreach activities by ',
                         'designating coordination sites, drawing health area boundaries, and defining team ',
                         'territories within each health area for a given campaign.'
                       )
        ),

        .intro_section('Key concepts',
                       tagList(
                         .concept_block(
                           'Campaign',
                           paste0(
                             'The outreach round this planning work is for. A campaign only shows the ',
                             'districts an admin has assigned to it — each may have a published, ',
                             'campaign-specific set of health areas and team areas that other users can ',
                             'view and build on.'
                           )
                         ),
                         .concept_block(
                           'Health area',
                           paste0(
                             'A geographic sub-unit of a district, each overseen by one coordination site. ',
                             'Once coordination sites are selected, boundaries are drawn automatically by ',
                             'expanding outward from each site across the district, weighted by population ',
                             'and how easily areas can be reached. Boundaries can then be adjusted manually.'
                           )
                         ),
                         .concept_block(
                           'Team area',
                           paste0(
                             'A sub-division of a health area assigned to a single outreach team. Drawn the ',
                             'same way as health areas, but nested within a health area\u2019s boundary, and can ',
                             'only ever be drawn against that district\u2019s CURRENT health area map.'
                           )
                         ),
                         .concept_block(
                           'Coordination site',
                           paste0(
                             'A health facility designated as the base of operations for one outreach ',
                             'team. Coordination sites anchor the health area boundaries \u2014 each site is ',
                             'the centre of its surrounding area.'
                           )
                         )
                       )
        ),

        .intro_section('How it works',
                       div(
                         style = 'display: flex; flex-direction: column; gap: 10px;',
                         .intro_step('1', 'Pick a campaign, find your district',
                                     'The table above shows every district assigned to the selected campaign, and how far along each one is.'),
                         .intro_step('2', 'Open Health Areas or Team Areas',
                                     paste0('Each district row has two entry points. Health Areas covers landmarks, ',
                                            'facilities, and health-area boundaries. Team Areas covers outreach-team ',
                                            'territory within one health area at a time, and only opens once a ',
                                            'district has a current health-area map.')),
                         .intro_step('3', 'Choose current, or one of your own drafts',
                                     paste0('The current version is always the default choice. Your own past drafts ',
                                            '(if any) and "Start blank" are one click away in the dropdown below it.')),
                         .intro_step('4', 'Submit, and set as current when ready',
                                     paste0('Submitting a health-area or team-area map offers the option to make it ',
                                            'the current one right there — no separate publish step.'))
                       )
        )
        )
      )
    )
  )
}


# ── Helpers (unchanged from v1) ────────────────────────────────────────────────

.intro_section <- function(title, body) {
  div(
    style = 'margin-bottom: 28px;',
    tags$h4(
      style = paste0(
        'font-size: 13px; font-weight: 700; color: #475569; margin: 0 0 12px;',
        'text-transform: uppercase; letter-spacing: .06em;'
      ),
      title
    ),
    body,
    tags$hr(style = 'border-color: #e2e8f0; margin-top: 20px; margin-bottom: 0;')
  )
}

.concept_block <- function(term, defn) {
  div(
    style = paste0(
      'padding: 10px 14px; margin-bottom: 8px;',
      'border-left: 3px solid #0d9488; background: #f8fafc; border-radius: 0 6px 6px 0;'
    ),
    div(style = 'font-size: 13px; font-weight: 600; color: #0f172a; margin-bottom: 3px;', term),
    div(style = 'font-size: 13px; color: #475569; line-height: 1.6;', defn)
  )
}

.intro_step <- function(num, title, desc) {
  div(
    style = 'display: flex; align-items: flex-start; gap: 14px;',
    div(
      style = paste0(
        'width: 26px; height: 26px; border-radius: 50%; background: #e2e8f0; color: #94a3b8;',
        'display: flex; align-items: center; justify-content: center;',
        'font-size: 11px; font-weight: 700; flex-shrink: 0; margin-top: 1px;'
      ),
      num
    ),
    div(
      div(style = 'font-size: 13px; font-weight: 600; color: #0f172a; margin-bottom: 2px;', title),
      div(style = 'font-size: 12px; color: #64748b; line-height: 1.5;', desc)
    )
  )
}

# Single source of truth for the campaign map's 4-category district
# shading -- used both by the legend (UI) and by the server-side
# classification/coloring logic below, so the two can never drift out
# of sync with each other.
.CAMPAIGN_MAP_COLORS <- c(
  not_associated       = '#e2e8f0',  # neutral gray -- outside this campaign entirely
  ha_not_mapped        = '#fca5a5',  # light red -- assigned, nothing published yet
  ha_mapped_ta_pending = '#fcd34d',  # amber -- health areas published, team areas incomplete
  all_mapped           = '#86efac'   # light green -- every health area has current team areas
)
.CAMPAIGN_MAP_LABELS <- c(
  not_associated       = 'Not associated with this campaign',
  ha_not_mapped        = 'Associated \u2014 health areas not yet mapped',
  ha_mapped_ta_pending = 'Health areas mapped \u2014 team areas pending',
  all_mapped           = 'All team areas mapped'
)

# .campaign_map_legend() removed -- replaced by output$campaign_map_cards
# (live district counts per category), which now sits above the map
# instead of a static color-key legend beside it.


# =============================================================================
# Server
# =============================================================================

# username_r — reactive, logged-in user (needed for "your own drafts")
introTabServer <- function(id, districts_shp, username_r, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Shown the moment a drill-down request actually starts firing --
    # replaces whichever picker modal was open. Deliberately no footer/
    # cancel button (the work is short and there's nothing sensible to
    # cancel mid-flight). Dismissed once the client reports the tab
    # switch has actually happened (server.R's own input$main_tabs
    # observer), or explicitly on any error path below that aborts
    # before a request ever fires (e.g. a failed db_create_*_version
    # call) -- otherwise those specific error paths would leave this
    # stuck open forever, since no tab switch ever happens to dismiss it.
    #
    # Needs session$onFlushed(..., once = TRUE) at every call site below,
    # not just calling showModal() then doing the work inline -- Shiny
    # batches an observer's outgoing messages into ONE flush sent to the
    # client only once the whole observer finishes, so showing this modal
    # and then immediately running the heavy work in the SAME observer
    # would mean the client never actually sees it until that work is
    # already done. onFlushed defers the real work to the NEXT flush,
    # after THIS one (showing the modal) has actually reached the browser.
    .show_loading_modal <- function(msg = 'Loading...') {
      showModal(modalDialog(
        title = NULL, size = 's', easyClose = FALSE, footer = NULL,
        tags$div(
          style = 'text-align:center;padding:6px 0;',
          tags$style(HTML(
            '@keyframes intro_loading_spin { to { transform: rotate(360deg); } }'
          )),
          tags$div(style = paste0(
            'display:inline-block;width:26px;height:26px;margin-bottom:10px;',
            'border:3px solid #e2e8f0;border-top-color:#0d9488;border-radius:50%;',
            'animation:intro_loading_spin 0.8s linear infinite;'
          )),
          tags$p(style = 'font-size:13px;color:#334155;font-weight:600;margin:0;', msg)
        )
      ))
    }

    # ── Campaign choices ──────────────────────────────────────────────────────
    observe({
      campaigns <- tryCatch(db_get_campaigns(pool, active_only = TRUE), error = function(e) NULL)
      if (is.null(campaigns) || nrow(campaigns) == 0) {
        updateSelectInput(session, 'campaign',
                          choices = setNames('', 'No active campaigns'), selected = '')
        return()
      }
      choices <- setNames(as.character(campaigns$campaign_id), campaigns$campaign_name)
      # Defaults to the top campaign in the list (choices[1] -- campaigns
      # come back ORDER BY created_at DESC from db_get_campaigns(), so
      # this is the most recently created active campaign) rather than
      # leaving the picker blank. "Select campaign..." stays available
      # as an option for anyone who wants to explicitly clear it, just
      # never the default.
      updateSelectInput(session, 'campaign',
                        choices = c(setNames('', 'Select campaign...'), choices),
                        selected = unname(choices[1]))
    })

    campaign_id <- reactive({
      v <- input$campaign %||% ''
      if (!nzchar(v)) return(NULL)
      as.integer(v)
    })

    region_lookup <- reactive({
      districts_shp |> sf::st_drop_geometry() |>
        dplyr::distinct(district_name, region_name) |>
        dplyr::mutate(district_name = as.character(district_name), region_name = as.character(region_name))
    })

    # ── Table data: one row per district assigned to the selected campaign ────
    # Depends on active_tab() so navigating back to this tab always
    # re-queries current progress -- e.g. after submitting and marking a
    # health area current elsewhere, coming back here should show it
    # without needing to touch the campaign selector to force a refresh.
    table_data <- reactive({
      req(identical(active_tab(), 'tab_intro'))
      cid <- campaign_id()
      req(!is.null(cid))
      assigned <- tryCatch(db_get_campaign_districts(pool, cid), error = function(e) NULL)
      if (is.null(assigned) || nrow(assigned) == 0) return(NULL)

      progress <- tryCatch(db_get_campaign_progress(pool, cid), error = function(e) NULL)
      lookup   <- region_lookup()

      rows <- lapply(assigned$district_name, function(dname) {
        region <- lookup$region_name[match(dname, lookup$district_name)] %||% ''
        prog_row <- if (!is.null(progress)) progress[progress$district_name == dname, , drop = FALSE] else NULL
        has_current_ha <- !is.null(prog_row) && nrow(prog_row) > 0

        n_health_areas    <- NA_integer_
        ha_published_by   <- NA_character_
        ha_published_at   <- as.POSIXct(NA)
        n_teams_mapped    <- 0L
        locked            <- FALSE

        if (has_current_ha) {
          dfa_names_parsed <- tryCatch(.from_json_vec_db(prog_row$dfa_names[1]), error = function(e) NULL)
          real_ha_names    <- setdiff(dfa_names_parsed, c('Inaccessible', 'Unpopulated'))
          n_health_areas   <- length(real_ha_names)
          ha_published_by  <- prog_row$submitted_by[1] %||% prog_row$owner_username[1]
          ha_published_at  <- prog_row$shared_at[1]
          n_teams_mapped   <- as.integer(prog_row$team_areas_mapped_count[1] %||% 0L)
          locked           <- isTRUE(tryCatch(db_district_has_locked_team_areas(pool, cid, dname), error = function(e) FALSE))
        }

        data.frame(
          district_name    = dname,
          region_name       = region,
          has_current_ha     = has_current_ha,
          n_health_areas       = n_health_areas,
          ha_published_by        = ha_published_by,
          ha_published_at          = ha_published_at,
          n_teams_mapped              = n_teams_mapped,
          locked                        = locked,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    })

    # ── Campaign overview map: every district in Somalia, shaded by status
    # relative to the selected campaign, with health-area/team-area
    # boundaries drawn on top wherever they've been published. Reuses
    # table_data() (already computed above) for classification rather
    # than re-deriving the same has_current_ha/n_health_areas/
    # n_teams_mapped logic a second time -- one source of truth for
    # "what stage is this district at".
    campaign_district_status <- reactive({
      cid <- campaign_id()
      all_districts <- districts_shp |>
        dplyr::group_by(district_name) |>
        dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
        sf::st_as_sf() |> safe_make_valid() |> sf::st_transform(4326)
      cat("[campaign_map_debug] n districts after union:", nrow(all_districts), "\n")
      cat("[campaign_map_debug] geometry type distribution:",
         paste(capture.output(print(table(sf::st_geometry_type(all_districts)))), collapse = ' | '), "\n")

      if (is.null(cid)) {
        all_districts$status_key <- 'not_associated'
        all_districts$status_label <- .CAMPAIGN_MAP_LABELS[['not_associated']]
        all_districts$fill_color <- .CAMPAIGN_MAP_COLORS[['not_associated']]
        return(all_districts)
      }

      assigned <- tryCatch(db_get_campaign_districts(pool, cid), error = function(e) NULL)
      td <- table_data()  # NULL if nothing assigned yet -- handled below

      status_for <- function(dname) {
        if (is.null(assigned) || !(dname %in% assigned$district_name)) return('not_associated')
        row <- if (!is.null(td)) td[td$district_name == dname, , drop = FALSE] else NULL
        if (is.null(row) || nrow(row) == 0 || !isTRUE(row$has_current_ha[1])) return('ha_not_mapped')
        n_ha <- suppressWarnings(as.integer(row$n_health_areas[1]))
        n_ta <- suppressWarnings(as.integer(row$n_teams_mapped[1]))
        if (is.na(n_ha) || n_ha <= 0) return('ha_mapped_ta_pending')
        if (!is.na(n_ta) && n_ta >= n_ha) 'all_mapped' else 'ha_mapped_ta_pending'
      }

      all_districts$status_key   <- vapply(as.character(all_districts$district_name), status_for, character(1))
      all_districts$status_label <- unname(.CAMPAIGN_MAP_LABELS[all_districts$status_key])
      all_districts$fill_color   <- unname(.CAMPAIGN_MAP_COLORS[all_districts$status_key])
      cat("[campaign_map_debug] status_key table:", paste(capture.output(print(table(all_districts$status_key))), collapse = ' | '), "\n")
      cat("[campaign_map_debug] distinct fill_color values:", paste(unique(all_districts$fill_color), collapse = ', '), "\n")
      all_districts
    })

    # Loading overlay -- shown until campaign_district_status() (the
    # potentially-slow districts_shp union/classification) resolves for
    # the first time, then hidden for the rest of the session. This
    # reactive always returns a non-NULL sf object once it completes
    # (even with no campaign selected, per the not_associated branch
    # above), so a single successful evaluation is exactly "the table
    # and map are ready to show".
    observeEvent(campaign_district_status(), {
      shinyjs::hide('intro_loading_overlay')
      shinyjs::show('intro_loaded_content')
    }, once = TRUE)

    # ── Summary cards -- replaces the old static legend with LIVE district
    # counts per status category, same 4 categories/colors as the map
    # itself (.CAMPAIGN_MAP_COLORS/.CAMPAIGN_MAP_LABELS, so these can
    # never drift out of sync with what the map is actually showing).
    output$campaign_map_cards <- renderUI({
      status_sf <- campaign_district_status()
      counts <- table(factor(status_sf$status_key, levels = names(.CAMPAIGN_MAP_COLORS)))
      div(
        style = 'display:flex;gap:10px;margin-bottom:12px;flex-wrap:wrap;',
        lapply(names(.CAMPAIGN_MAP_COLORS), function(key) {
          div(
            style = sprintf(
              'flex:1;min-width:150px;background:%s;border-radius:8px;padding:10px 12px;',
              .CAMPAIGN_MAP_COLORS[[key]]
            ),
            div(style = 'font-size:22px;font-weight:700;color:#1e293b;', unname(counts[key])),
            div(style = 'font-size:11px;color:#334155;', .CAMPAIGN_MAP_LABELS[[key]])
          )
        })
      )
    })

    # Health-area/team-area boundary geometry for every district with a
    # current, published health-area version in this campaign --
    # separate from campaign_district_status() above since it needs an
    # actual DB fetch (and, for team areas, one more fetch per health
    # area) per mapped district, not just the summary counts table_data()
    # already has. Each district wrapped in its own tryCatch so one
    # failure doesn't blank the whole map.
    # ── On-demand, single-district fetch for the boundary-review popup
    # (map click or "View" button) -- NOT a bulk, every-district fetch.
    # The old campaign_boundaries() reactive fetched every mapped
    # district's health-area AND team-area boundaries (one DB round
    # trip per health area, per district) unconditionally, just to draw
    # them on the overview map -- exactly what made that map slow. The
    # overview map itself no longer needs boundary geometry at all (see
    # output$campaign_map below); this replaces that bulk fetch with a
    # single-district version, called only when the user actually asks
    # to see one district's detail.
    .fetch_district_boundaries_for_review <- function(cid, dname) {
      ver <- tryCatch(db_get_shared_version(pool, cid, dname), error = function(e) NULL)
      if (is.null(ver)) return(list(ha_sf = NULL, ta_sf = NULL))
      ha_geom <- ver$snap$smoothed_dfa_sf %||% ver$snap$saved_dfa_sf
      ha_sf <- if (!is.null(ha_geom) && nrow(ha_geom) > 0) sf::st_transform(safe_make_valid(ha_geom), 4326) else NULL

      ta_parts <- list()
      ha_names <- setdiff(unlist(ver$snap$dfa_names %||% list()), c('Inaccessible', 'Unpopulated'))
      for (han in ha_names) {
        tv <- tryCatch(db_get_current_team_area_version(pool, cid, dname, han), error = function(e) NULL)
        if (is.null(tv)) next
        ta_geom <- tv$snap$smoothed_team_sf %||% tv$snap$saved_team_sf
        if (!is.null(ta_geom) && nrow(ta_geom) > 0) ta_parts[[han]] <- sf::st_transform(safe_make_valid(ta_geom), 4326)
      }
      list(
        ha_sf = ha_sf,
        ta_sf = if (length(ta_parts) > 0) dplyr::bind_rows(ta_parts) else NULL
      )
    }

    output$campaign_map <- leaflet::renderLeaflet({
      status_sf <- campaign_district_status()

      # Districts only, color-coded by status -- no health-area/team-
      # area boundaries drawn here anymore (that bulk fetch was what
      # made this map slow; see .fetch_district_boundaries_for_review()
      # above for the single-district, on-demand replacement used by
      # the click/"View" popup instead). layerId enables the click
      # handler below to know which district was clicked.
      leaflet::leaflet(status_sf) |>
        leaflet::addProviderTiles('OpenStreetMap') |>
        leaflet::addPolygons(
          layerId = ~district_name,
          fillColor = ~fill_color, fillOpacity = 0.55, weight = 2.5, color = '#000000',
          label = ~paste0(district_name, ': ', status_label)
        )
    })

    # ── District boundary-review popup -- view-only current health-area
    # and team-area boundaries for ONE district, opened either by
    # clicking its polygon on the overview map or the "View" button on
    # its table row (both call this same function).
    .show_district_boundary_popup <- function(dname) {
      cid <- campaign_id(); req(!is.null(cid), nzchar(dname %||% ''))
      showModal(modalDialog(
        title = sprintf('%s \u2014 Current Boundaries', dname), size = 'l', easyClose = TRUE,
        footer = modalButton('Close'),
        tags$p(style = 'font-size:11px;color:#94a3b8;margin:0 0 8px;',
               'View only -- current published health-area and team-area boundaries for this district.'),
        leaflet::leafletOutput(session$ns('district_review_popup_map'), height = '420px')
      ))
      bounds <- tryCatch(.fetch_district_boundaries_for_review(cid, dname), error = function(e) list(ha_sf = NULL, ta_sf = NULL))
      output$district_review_popup_map <- leaflet::renderLeaflet({
        if (is.null(bounds$ha_sf) || nrow(bounds$ha_sf) == 0) {
          return(leaflet::leaflet() |> leaflet::addProviderTiles('OpenStreetMap') |>
                   leaflet::setView(lng = 45, lat = 5, zoom = 5))
        }
        area_col <- if ('dfa_name' %in% names(bounds$ha_sf)) 'dfa_name' else names(bounds$ha_sf)[1]
        areas_u  <- unique(as.character(bounds$ha_sf[[area_col]]))
        pal <- leaflet::colorFactor(grDevices::rainbow(max(length(areas_u), 1), s = 0.6, v = 0.9), domain = areas_u)
        m <- leaflet::leaflet(bounds$ha_sf) |>
          leaflet::addProviderTiles('OpenStreetMap') |>
          leaflet::addPolygons(fillColor = ~pal(get(area_col)), fillOpacity = 0.5, weight = 1.5,
                              color = '#4b5563', label = ~get(area_col))
        if (!is.null(bounds$ta_sf) && nrow(bounds$ta_sf) > 0) {
          team_col <- if ('dfa_name' %in% names(bounds$ta_sf)) 'dfa_name' else names(bounds$ta_sf)[1]
          m <- m |> leaflet::addPolygons(
            data = bounds$ta_sf, fill = FALSE, weight = 0.8, color = '#111827', opacity = 0.85,
            label = ~get(team_col)
          )
        }
        m
      })
    }

    observeEvent(input$campaign_map_shape_click, {
      click <- input$campaign_map_shape_click
      req(!is.null(click$id))
      .show_district_boundary_popup(click$id)
    })

    output$district_table_ui <- renderUI({
      df <- table_data()
      if (is.null(df)) {
        return(div(style = 'padding:16px;color:#64748b;font-size:13px;',
                   if (is.null(campaign_id())) 'Select a campaign to see its districts.'
                   else 'No districts are assigned to this campaign yet — an admin can assign them from the Admin panel.'))
      }
      df <- df[order(df$region_name, df$district_name), ]

      ha_status <- ifelse(
        df$has_current_ha,
        sprintf('%d health areas \u00b7 %s, %s', df$n_health_areas,
               df$ha_published_by, format(df$ha_published_at, '%d %b %Y')),
        'Not started'
      )
      team_status <- ifelse(
        df$has_current_ha,
        sprintf('%d of %d health areas%s', df$n_teams_mapped, df$n_health_areas,
               ifelse(df$locked, ' \u00b7 \U0001F512 locked', '')),
        '\u2014'
      )
      action_ha <- vapply(df$district_name, function(dn) {
        sprintf('<button class="btn btn-default btn-xs" onclick="Shiny.setInputValue(\'%sha_row_click\', \'%s\', {priority:\'event\'})">Health Areas</button>',
               ns(''), dn)
      }, character(1))
      action_team <- vapply(seq_len(nrow(df)), function(i) {
        if (!df$has_current_ha[i]) return('<span style="color:#94a3b8;font-size:11px;">Needs health areas first</span>')
        sprintf('<button class="btn btn-default btn-xs" onclick="Shiny.setInputValue(\'%steam_row_click\', \'%s\', {priority:\'event\'})">Team Areas</button>',
               ns(''), df$district_name[i])
      }, character(1))
      # "View" opens the same read-only boundary popup as clicking the
      # district's polygon on the overview map (see
      # .show_district_boundary_popup()) -- disabled text instead of a
      # button when there's nothing published yet to show, same
      # has_current_ha gate action_team already uses above.
      action_view <- vapply(seq_len(nrow(df)), function(i) {
        if (!df$has_current_ha[i]) return('<span style="color:#94a3b8;font-size:11px;">\u2013</span>')
        sprintf('<button class="btn btn-default btn-xs" onclick="Shiny.setInputValue(\'%sview_row_click\', \'%s\', {priority:\'event\'})">View</button>',
               ns(''), df$district_name[i])
      }, character(1))

      # Grouped into one native <details>/<summary> block per region --
      # expands/collapses with zero JS (every modern browser handles this
      # natively), which is why this replaced the single flat DT table
      # rather than trying to add DT's RowGroup extension: RowGroup
      # doesn't support collapse/expand out of the box either, and
      # bolting that on would mean custom JS reaching into DT's
      # re-rendered DOM on every sort/search, with real risk of losing
      # collapse state on each redraw. The tradeoff worth knowing: this
      # drops the DT search box the old table had -- there's no
      # cross-region search now, only expand/collapse. Defaults to OPEN
      # for every region (matching the old flat table's "everything
      # visible" behavior) -- collapsing is something the user opts into
      # per region, not a forced default.
      # Fixed, IDENTICAL column widths across every region's table --
      # each <table> below is otherwise independent (default table-
      # layout: auto sizes every table's columns from its OWN content
      # alone), which is exactly what made this look clunky: the same
      # column landing at a different width in every region depending
      # on what happened to be in it. A shared <colgroup> + table-
      # layout:fixed forces every table to use these same widths
      # regardless of content -- wrapping long status text onto a
      # second line instead of truncating it, rather than growing the
      # column (which is what caused the misalignment in the first
      # place).
      COL_WIDTHS <- c('20%', '40%', '40%')
      col_group <- tags$colgroup(
        tags$col(style = sprintf('width:%s;', COL_WIDTHS[1])),
        tags$col(style = sprintf('width:%s;', COL_WIDTHS[2])),
        tags$col(style = sprintf('width:%s;', COL_WIDTHS[3]))
      )
      cell_style <- 'padding:5px 8px;font-size:12px;word-break:break-word;'

      regions <- unique(df$region_name)
      region_blocks <- lapply(regions, function(rn) {
        idx <- which(df$region_name == rn)
        rows <- lapply(idx, function(i) {
          tags$tr(
            tags$td(style = cell_style, HTML(paste0(df$district_name[i], ' ', action_view[i]))),
            tags$td(style = cell_style, HTML(paste0(ha_status[i], '<br>', action_ha[i]))),
            tags$td(style = cell_style, HTML(paste0(team_status[i], '<br>', action_team[i])))
          )
        })
        tags$details(
          open = NA,   # NA renders the bare `open` attribute -- expanded by default
          style = 'margin-bottom:4px;border:1px solid #e2e8f0;border-radius:6px;overflow:hidden;',
          tags$summary(
            style = 'cursor:pointer;padding:6px 10px;background:#f8fafc;font-size:12px;font-weight:700;color:#334155;list-style:none;',
            sprintf('%s (%d district%s)', rn, length(idx), if (length(idx) == 1) '' else 's')
          ),
          tags$table(
            style = 'width:100%;table-layout:fixed;border-collapse:collapse;',
            col_group,
            tags$thead(
              tags$tr(
                tags$th(style = 'text-align:left;padding:5px 8px;font-size:11px;color:#64748b;border-bottom:1px solid #e2e8f0;', 'District'),
                tags$th(style = 'text-align:left;padding:5px 8px;font-size:11px;color:#64748b;border-bottom:1px solid #e2e8f0;', 'Health Areas'),
                tags$th(style = 'text-align:left;padding:5px 8px;font-size:11px;color:#64748b;border-bottom:1px solid #e2e8f0;', 'Team Areas')
              )
            ),
            tags$tbody(rows)
          )
        )
      })
      div(style = 'overflow-y:auto;max-height:calc(100vh - 420px);', region_blocks)
    })

    # ── Request events consumed by server.R — this module never activates a
    # session-manager instance directly, since those instances live outside it.
    ha_request   <- reactiveVal(NULL)
    team_request <- reactiveVal(NULL)

    # ── Health Areas drill-down ─────────────────────────────────────────────
    picker_district <- reactiveVal(NULL)

    observeEvent(input$view_row_click, {
      .show_district_boundary_popup(input$view_row_click)
    }, ignoreInit = TRUE)

    observeEvent(input$ha_row_click, {
      dname <- input$ha_row_click
      picker_district(dname)
      .show_health_area_picker(dname)
    }, ignoreInit = TRUE)

    .show_health_area_picker <- function(district_name) {
      cid <- campaign_id(); uname <- username_r() %||% ''
      current <- tryCatch(db_get_shared_version(pool, cid, district_name), error = function(e) NULL)
      own_drafts <- tryCatch(db_get_owner_versions(pool, uname, district_name), error = function(e) NULL)
      if (!is.null(own_drafts) && nrow(own_drafts) > 0) {
        own_drafts <- own_drafts[own_drafts$campaign_id == cid & is.na(own_drafts$archived_at), , drop = FALSE]
        if (!is.null(current)) own_drafts <- own_drafts[own_drafts$version_id != current$version_id, , drop = FALSE]
      }

      dropdown_choices <- c(setNames('__blank__', 'Start blank'))
      if (!is.null(own_drafts) && nrow(own_drafts) > 0) {
        draft_choices <- setNames(
          as.character(own_drafts$version_id),
          paste0('Your draft v', own_drafts$version_number,
                ' (', format(own_drafts$last_updated_at, '%d %b %Y'), ')')
        )
        dropdown_choices <- c(draft_choices, dropdown_choices)
      }

      showModal(modalDialog(
        title = paste0('Health Areas \u2014 ', district_name),
        size = 'm', easyClose = TRUE, footer = modalButton('Cancel'),
        if (!is.null(current)) {
          div(
            style = 'border:1px solid #99f6e4;border-radius:8px;padding:12px 14px;margin-bottom:14px;background:#f0fdfa;',
            div(style = 'font-size:12px;font-weight:600;color:#0f172a;margin-bottom:4px;', 'Current version'),
            div(style = 'font-size:11px;color:#64748b;margin-bottom:8px;',
               sprintf('Published by %s on %s', current$submitted_by %||% current$owner_username,
                       tryCatch(format(current$shared_at, '%d %b %Y'), error = function(e) ''))),
            actionButton(ns('ha_pick_current'), 'Continue with current',
                        class = 'btn btn-primary', style = 'font-weight:600;')
          )
        } else {
          div(style = 'font-size:12px;color:#64748b;margin-bottom:14px;',
             'No current health-area map for this district yet.')
        },
        div(style = 'font-size:11px;color:#64748b;margin-bottom:6px;', 'Or continue a previous version'),
        div(
          style = 'display:flex;gap:6px;',
          selectInput(ns('ha_pick_dropdown'), NULL, choices = dropdown_choices, width = '100%'),
          actionButton(ns('ha_pick_dropdown_go'), 'Go', class = 'btn btn-default')
        )
      ))
    }

    observeEvent(input$ha_pick_current, {
      dname <- picker_district(); req(!is.null(dname))
      current <- tryCatch(db_get_shared_version(pool, campaign_id(), dname), error = function(e) NULL)
      req(!is.null(current))
      .show_loading_modal(paste0('Loading ', dname, '...'))
      session$onFlushed(function() {
      # has_boundaries drives server.R's decision to skip straight to
      # Health Areas and lock out Landmarks/Facilities re-editing for
      # this session -- "Continue with current" always means an
      # already-published, already-locked-in version, so this is
      # effectively always TRUE here, but computed properly (not just
      # assumed) in case a shared version somehow has no saved boundaries
      # yet (e.g. published at facilities-stage only, if that's ever
      # possible).
      ha_request(list(
        district_name   = dname,
        version_id      = current$version_id,
        has_boundaries  = !is.null(current$snap$saved_dfa_sf) && nrow(current$snap$saved_dfa_sf) > 0,
        # Drives campaign_scope_locked_to_current in server.R -- "Continue
        # with current" is always an already-published, already-locked-in
        # version, so this is effectively always TRUE here too, same
        # reasoning as has_boundaries just above.
        has_facilities  = isTRUE(current$has_facilities),
        ts              = Sys.time()
      ))
      }, once = TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input$ha_pick_dropdown_go, {
      dname <- picker_district(); req(!is.null(dname))
      choice <- input$ha_pick_dropdown %||% ''
      req(nzchar(choice))
      # Captured as plain variables BEFORE onFlushed -- reactive
      # expressions (campaign_id(), username_r()) can't be called from
      # inside an onFlushed callback at all ("Operation not allowed
      # without an active reactive context", confirmed directly), since
      # that callback runs outside any reactive context.
      cid <- campaign_id(); uname <- username_r() %||% ''
      .show_loading_modal(paste0('Loading ', dname, '...'))
      session$onFlushed(function() {
      if (identical(choice, '__blank__')) {
        new_id <- tryCatch(
          db_create_blank_version(pool, uname, cid, dname),
          error = function(e) { showNotification(paste('Could not start a new draft:', e$message), type = 'error', duration = 6); NULL }
        )
        if (is.null(new_id)) { removeModal(); return() }
        # A genuinely fresh blank draft -- always has_boundaries = FALSE,
        # never skips Landmarks/Facilities. has_facilities = FALSE for
        # the same reason -- Campaign Scope's own lock (see
        # campaign_scope_locked_to_current in server.R) never applies to
        # a brand-new draft either.
        ha_request(list(district_name = dname, version_id = new_id, has_boundaries = FALSE, has_facilities = FALSE, ts = Sys.time()))
      } else {
        # own_drafts (which populated this dropdown) is metadata-only --
        # a real fetch is needed here to know whether this SPECIFIC draft
        # already has saved boundaries or is still blank/in-progress.
        picked_id <- as.integer(choice)
        picked    <- tryCatch(db_get_version_by_id(pool, picked_id), error = function(e) NULL)
        if (is.null(picked)) { removeModal(); return() }
        ha_request(list(
          district_name  = dname,
          version_id     = picked_id,
          has_boundaries = !is.null(picked) && !is.null(picked$snap$saved_dfa_sf) && nrow(picked$snap$saved_dfa_sf) > 0,
          has_facilities = !is.null(picked) && isTRUE(picked$has_facilities),
          ts             = Sys.time()
        ))
      }
      }, once = TRUE)
    }, ignoreInit = TRUE)

    # ── Team Areas drill-down (two steps: health area, then version) ────────
    team_picker_district <- reactiveVal(NULL)
    team_picker_ha_name  <- reactiveVal(NULL)

    observeEvent(input$team_row_click, {
      dname <- input$team_row_click
      team_picker_district(dname)
      team_picker_ha_name(NULL)
      .show_team_health_area_step(dname)
    }, ignoreInit = TRUE)

    .show_team_health_area_step <- function(district_name) {
      cid <- campaign_id()
      current_ha <- tryCatch(db_get_shared_version(pool, cid, district_name), error = function(e) NULL)
      if (is.null(current_ha) || is.null(current_ha$snap$dfa_names)) {
        showModal(modalDialog(
          title = paste0('Team Areas \u2014 ', district_name), size = 's', easyClose = TRUE,
          footer = modalButton('Close'),
          tags$p(style = 'font-size:13px;color:#64748b;',
                'This district needs a current health-area map before team areas can be mapped.')
        ))
        return(invisible(NULL))
      }
      ha_names <- setdiff(unlist(current_ha$snap$dfa_names), c('Inaccessible', 'Unpopulated'))
      summary_df <- tryCatch(db_get_district_team_area_summary(pool, cid, district_name), error = function(e) NULL)

      rows <- lapply(sort(ha_names), function(han) {
        has_current <- !is.null(summary_df) && any(summary_df$health_area_name == han & summary_df$has_current)
        div(
          style = 'display:flex;justify-content:space-between;align-items:center;padding:8px 4px;border-bottom:1px solid #f1f5f9;',
          div(style = 'font-size:13px;color:#0f172a;', han,
             if (has_current) tags$span(style = 'color:#166534;font-size:11px;margin-left:6px;', '\u2713 current mapped')),
          tags$button(
            class = 'btn btn-default btn-xs', type = 'button', 'Open',
            onclick = sprintf("Shiny.setInputValue('%steam_pick_ha', '%s', {priority:'event'})", ns(''), han)
          )
        )
      })

      showModal(modalDialog(
        title = paste0('Team Areas \u2014 ', district_name, ': choose a health area'),
        size = 'm', easyClose = TRUE, footer = modalButton('Cancel'),
        div(style = 'max-height:400px;overflow-y:auto;', rows)
      ))
    }

    observeEvent(input$team_pick_ha, {
      han <- input$team_pick_ha
      team_picker_ha_name(han)
      .show_team_version_step(team_picker_district(), han)
    }, ignoreInit = TRUE)

    .show_team_version_step <- function(district_name, health_area_name) {
      cid <- campaign_id(); uname <- username_r() %||% ''
      current <- tryCatch(db_get_current_team_area_version(pool, cid, district_name, health_area_name),
                          error = function(e) NULL)
      own_drafts <- tryCatch(db_get_owner_team_versions(pool, uname, cid, district_name, health_area_name),
                             error = function(e) NULL)
      if (!is.null(own_drafts) && nrow(own_drafts) > 0) {
        own_drafts <- own_drafts[is.na(own_drafts$archived_at), , drop = FALSE]
        if (!is.null(current)) own_drafts <- own_drafts[own_drafts$team_version_id != current$team_version_id, , drop = FALSE]
      }

      dropdown_choices <- c(setNames('__blank__', 'Start blank'))
      if (!is.null(own_drafts) && nrow(own_drafts) > 0) {
        draft_choices <- setNames(
          as.character(own_drafts$team_version_id),
          paste0('Your draft v', own_drafts$version_number,
                ' (', format(own_drafts$last_updated_at, '%d %b %Y'), ')')
        )
        dropdown_choices <- c(draft_choices, dropdown_choices)
      }

      showModal(modalDialog(
        title = paste0('Team Areas \u2014 ', district_name, ' / ', health_area_name),
        size = 'm', easyClose = TRUE,
        footer = tagList(
          actionButton(ns('team_pick_back'), 'Back', class = 'btn btn-default'),
          modalButton('Cancel')
        ),
        if (!is.null(current)) {
          div(
            style = 'border:1px solid #99f6e4;border-radius:8px;padding:12px 14px;margin-bottom:14px;background:#f0fdfa;',
            div(style = 'font-size:12px;font-weight:600;color:#0f172a;margin-bottom:4px;', 'Current version'),
            div(style = 'font-size:11px;color:#64748b;margin-bottom:8px;',
               sprintf('Published by %s on %s', current$submitted_by %||% current$owner_username,
                       tryCatch(format(current$shared_at, '%d %b %Y'), error = function(e) ''))),
            actionButton(ns('team_pick_current'), 'Continue with current',
                        class = 'btn btn-primary', style = 'font-weight:600;')
          )
        } else {
          div(style = 'font-size:12px;color:#64748b;margin-bottom:14px;',
             'No current team map for this health area yet.')
        },
        div(style = 'font-size:11px;color:#64748b;margin-bottom:6px;', 'Or continue a previous version'),
        div(
          style = 'display:flex;gap:6px;',
          selectInput(ns('team_pick_dropdown'), NULL, choices = dropdown_choices, width = '100%'),
          actionButton(ns('team_pick_dropdown_go'), 'Go', class = 'btn btn-default')
        )
      ))
    }

    observeEvent(input$team_pick_back, {
      .show_team_health_area_step(team_picker_district())
    }, ignoreInit = TRUE)

    observeEvent(input$team_pick_current, {
      dname <- team_picker_district(); han <- team_picker_ha_name()
      req(!is.null(dname), !is.null(han))
      cid <- campaign_id()
      current_ha <- tryCatch(db_get_shared_version(pool, cid, dname), error = function(e) NULL)
      current_team <- tryCatch(db_get_current_team_area_version(pool, cid, dname, han), error = function(e) NULL)
      req(!is.null(current_ha), !is.null(current_team))
      .show_loading_modal(paste0('Loading ', han, '...'))
      session$onFlushed(function() {
        team_request(list(district_name = dname, health_area_name = han,
                          health_area_version_id = current_ha$version_id,
                          team_version_id = current_team$team_version_id, ts = Sys.time()))
      }, once = TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input$team_pick_dropdown_go, {
      dname <- team_picker_district(); han <- team_picker_ha_name()
      req(!is.null(dname), !is.null(han))
      choice <- input$team_pick_dropdown %||% ''
      req(nzchar(choice))
      cid <- campaign_id()
      current_ha <- tryCatch(db_get_shared_version(pool, cid, dname), error = function(e) NULL)
      req(!is.null(current_ha))
      # Captured as a plain variable BEFORE onFlushed -- same reason as
      # ha_pick_dropdown_go's identical fix: username_r() can't be
      # called from inside an onFlushed callback at all.
      uname <- username_r() %||% ''
      .show_loading_modal(paste0('Loading ', han, '...'))
      session$onFlushed(function() {
      if (identical(choice, '__blank__')) {
        new_id <- tryCatch(
          db_create_team_area_draft(pool, uname, cid, dname, han, current_ha$version_id),
          error = function(e) { showNotification(paste('Could not start a new team-area draft:', e$message), type = 'error', duration = 6); NULL }
        )
        if (is.null(new_id)) { removeModal(); return() }
        team_request(list(district_name = dname, health_area_name = han,
                          health_area_version_id = current_ha$version_id,
                          team_version_id = new_id, ts = Sys.time()))
      } else {
        team_request(list(district_name = dname, health_area_name = han,
                          health_area_version_id = current_ha$version_id,
                          team_version_id = as.integer(choice), ts = Sys.time()))
      }
      }, once = TRUE)
    }, ignoreInit = TRUE)

    # ── Public interface ──────────────────────────────────────────────────────
    list(
      campaign_id                  = campaign_id,
      activate_health_area_request = reactive(ha_request()),
      activate_team_area_request   = reactive(team_request())
    )
  })
}
