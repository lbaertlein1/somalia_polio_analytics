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
        style = 'padding: 20px 28px; max-width: 1100px;',

        div(
          style = 'display:flex;align-items:center;gap:16px;margin-bottom:16px;',
          div(
            style = 'flex:1;',
            tags$h2(style = 'font-size: 20px; font-weight: 600; color: #0f172a; margin: 0 0 2px;',
                    'District Health Area Planning Tool'),
            tags$p(style = 'font-size: 13px; color: #64748b; margin: 0;',
                   'Select a campaign to see progress across its districts.')
          ),
          div(
            style = 'width:260px;',
            div(class = 'mini-label', 'Campaign'),
            selectInput(ns('campaign'), NULL,
                       choices  = setNames('', 'Select campaign...'),
                       selected = '', width = '100%')
          )
        ),

        uiOutput(ns('district_table_ui')),

        tags$hr(style = 'border-color: #e2e8f0; margin: 28px 0 20px;'),

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


# =============================================================================
# Server
# =============================================================================

# username_r — reactive, logged-in user (needed for "your own drafts")
introTabServer <- function(id, districts_shp, username_r, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Campaign choices ──────────────────────────────────────────────────────
    observe({
      campaigns <- tryCatch(db_get_campaigns(pool, active_only = TRUE), error = function(e) NULL)
      if (is.null(campaigns) || nrow(campaigns) == 0) {
        updateSelectInput(session, 'campaign',
                          choices = setNames('', 'No active campaigns'), selected = '')
        return()
      }
      choices <- setNames(as.character(campaigns$campaign_id), campaigns$campaign_name)
      updateSelectInput(session, 'campaign',
                        choices = c(setNames('', 'Select campaign...'), choices), selected = '')
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

    output$district_table_ui <- renderUI({
      df <- table_data()
      if (is.null(df)) {
        return(div(style = 'padding:16px;color:#64748b;font-size:13px;',
                   if (is.null(campaign_id())) 'Select a campaign to see its districts.'
                   else 'No districts are assigned to this campaign yet — an admin can assign them from the Admin panel.'))
      }
      DT::DTOutput(ns('district_table'))
    })

    output$district_table <- DT::renderDT({
      df <- table_data()
      req(!is.null(df))
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
      actions <- paste(action_ha, action_team, sep = ' ')

      display <- data.frame(
        District      = df$district_name,
        Region         = df$region_name,
        `Health Areas`  = ha_status,
        `Team Areas`     = team_status,
        Actions           = actions,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(display, escape = FALSE, rownames = FALSE, selection = 'none',
                    options = list(dom = 'ft', pageLength = 200, scrollX = TRUE,
                                  scrollY = 'calc(100vh - 420px)', scrollCollapse = TRUE))
    })

    # ── Request events consumed by server.R — this module never activates a
    # session-manager instance directly, since those instances live outside it.
    ha_request   <- reactiveVal(NULL)
    team_request <- reactiveVal(NULL)

    # ── Health Areas drill-down ─────────────────────────────────────────────
    picker_district <- reactiveVal(NULL)

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
      removeModal()
      ha_request(list(district_name = dname, version_id = current$version_id, ts = Sys.time()))
    }, ignoreInit = TRUE)

    observeEvent(input$ha_pick_dropdown_go, {
      dname <- picker_district(); req(!is.null(dname))
      choice <- input$ha_pick_dropdown %||% ''
      req(nzchar(choice))
      removeModal()
      if (identical(choice, '__blank__')) {
        new_id <- tryCatch(
          db_create_blank_version(pool, username_r() %||% '', campaign_id(), dname),
          error = function(e) { showNotification(paste('Could not start a new draft:', e$message), type = 'error', duration = 6); NULL }
        )
        req(!is.null(new_id))
        ha_request(list(district_name = dname, version_id = new_id, ts = Sys.time()))
      } else {
        ha_request(list(district_name = dname, version_id = as.integer(choice), ts = Sys.time()))
      }
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
      removeModal()
      team_request(list(district_name = dname, health_area_name = han,
                        health_area_version_id = current_ha$version_id,
                        team_version_id = current_team$team_version_id, ts = Sys.time()))
    }, ignoreInit = TRUE)

    observeEvent(input$team_pick_dropdown_go, {
      dname <- team_picker_district(); han <- team_picker_ha_name()
      req(!is.null(dname), !is.null(han))
      choice <- input$team_pick_dropdown %||% ''
      req(nzchar(choice))
      cid <- campaign_id()
      current_ha <- tryCatch(db_get_shared_version(pool, cid, dname), error = function(e) NULL)
      req(!is.null(current_ha))
      removeModal()
      if (identical(choice, '__blank__')) {
        new_id <- tryCatch(
          db_create_team_area_draft(pool, username_r() %||% '', cid, dname, han, current_ha$version_id),
          error = function(e) { showNotification(paste('Could not start a new team-area draft:', e$message), type = 'error', duration = 6); NULL }
        )
        req(!is.null(new_id))
        team_request(list(district_name = dname, health_area_name = han,
                          health_area_version_id = current_ha$version_id,
                          team_version_id = new_id, ts = Sys.time()))
      } else {
        team_request(list(district_name = dname, health_area_name = han,
                          health_area_version_id = current_ha$version_id,
                          team_version_id = as.integer(choice), ts = Sys.time()))
      }
    }, ignoreInit = TRUE)

    # ── Public interface ──────────────────────────────────────────────────────
    list(
      campaign_id                  = campaign_id,
      activate_health_area_request = reactive(ha_request()),
      activate_team_area_request   = reactive(team_request())
    )
  })
}
