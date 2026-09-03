# =============================================================================
# mod_intro_tab.R  (v2)
#
# Changes from v1:
#   - Practice/Actual toggle removed entirely.
#   - Urban/rural planning-area split removed — a district is one planning
#     unit now, not two independently-versioned ones. Subdivision polygons
#     are still fetched (fetch_subdivisions_for_district) because they're
#     still used downstream as the soft-barrier reference layer during
#     health-area generation — they just no longer carve the district into
#     separate planning areas here.
#   - Campaign selector added (admin-created, shared list from campaigns
#     table). Both district AND campaign must be chosen before the app is
#     "ready" — this pair is what session_mgr keys on.
# =============================================================================

introTabUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    # ── Left sidebar — campaign + district selector ──────────────────────────
    column(
      width = 2,

      div(class = 'rightbar-title', style = 'margin-top: 4px;', 'Select Campaign & District'),

      div(class = 'mini-label', 'Campaign'),
      selectInput(ns('campaign'), NULL,
                  choices  = setNames('', 'Select campaign...'),
                  selected = '', width = '100%'),

      tags$hr(style = 'margin: 10px 0;'),

      div(class = 'mini-label', 'Region'),
      selectInput(ns('region'), NULL,
                  choices  = setNames('', 'Select region...'),
                  selected = '', width = '100%'),

      div(class = 'mini-label', 'District'),
      selectInput(ns('district'), NULL,
                  choices  = setNames('', 'Select district...'),
                  selected = '', width = '100%'),

      tags$hr(style = 'margin: 12px 0;'),

      tags$button(
        id       = ns('continue'),
        class    = 'btn btn-primary btn-block intro-continue-btn',
        type     = 'button',
        disabled = NA,
        style    = 'font-weight: 600; font-size: 13px; height: 36px; width: 100%;',
        onclick  = paste0(
          "if(!this.disabled){",
          "$('#main_tabs a[data-value=\"tab_orientation\"]').tab('show');",
          "}"
        ),
        'Continue \u2192'
      )
    ),

    # ── Main panel ────────────────────────────────────────────────────────────
    column(
      width = 10,
      div(
        style = 'padding: 28px 40px; max-width: 860px;',

        tags$h2(
          style = 'font-size: 22px; font-weight: 600; color: #0f172a; margin-bottom: 4px;',
          'District Health Area Planning Tool'
        ),
        tags$p(
          style = 'font-size: 14px; color: #64748b; margin-bottom: 28px;',
          'Prepare districts for community outreach activities by designating coordination ',
          'sites, drawing health area boundaries, and defining outreach team territories.'
        ),

        tags$hr(style = 'border-color: #e2e8f0; margin-bottom: 24px;'),

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
                             'The outreach round this planning work is for. Each district may have a ',
                             'published, campaign-specific set of health areas and team areas that other ',
                             'users can view and build on.'
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
                             'same way as health areas, but nested within a health area\u2019s boundary.'
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
                         .intro_step('1', 'Select a campaign and district',
                                     'Choose the campaign and district from the panel on the left, then click Continue.'),
                         .intro_step('2', 'Add landmarks',
                                     paste0('Familiarise the group with the district. Drop optional landmark pins to mark ',
                                            'recognisable reference points such as schools, hospitals, or road junctions.')),
                         .intro_step('3', 'Review health facilities and select coordination sites',
                                     paste0('Review health facility locations for your district. Correct any GPS positions ',
                                            'by dragging pins on the map. Mark which facilities will serve as coordination ',
                                            'sites \u2014 these become the anchors for the health area boundaries.')),
                         .intro_step('4', 'Define health areas',
                                     paste0('Boundaries are drawn automatically from the selected coordination sites. ',
                                            'Adjust them using the drawing tool to reflect local knowledge.')),
                         .intro_step('5', 'Define team areas',
                                     paste0('Within each health area, boundaries for individual outreach teams are drawn ',
                                            'the same way, and can be adjusted by hand.'))
                       )
        ),

        .intro_section('Overview video',
                       tagList(
                         tags$iframe(
                           src             = '',
                           id              = ns('video_frame'),
                           width           = '560',
                           height          = '315',
                           frameborder     = '0',
                           allowfullscreen = NA,
                           style           = 'border: 1px solid #e2e8f0; border-radius: 6px;
                    background: #f1f5f9; display: block;'
                         ),
                         tags$p(
                           style = 'font-size: 12px; color: #94a3b8; margin-top: 6px;',
                           'Video walkthrough coming soon.'
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

.intro_step <- function(num, title, desc, done = FALSE) {
  dot_bg    <- if (done) '#0d9488' else '#e2e8f0'
  dot_color <- if (done) '#ffffff' else '#94a3b8'
  dot_inner <- if (done) {
    tags$svg(
      xmlns = 'http://www.w3.org/2000/svg', viewBox = '0 0 12 12',
      width = '12', height = '12', fill = 'none',
      tags$path(d = 'M2 6l3 3 5-5', stroke = '#fff',
                `stroke-width` = '2', `stroke-linecap` = 'round',
                `stroke-linejoin` = 'round')
    )
  } else { num }

  div(
    style = 'display: flex; align-items: flex-start; gap: 14px;',
    div(
      style = paste0(
        'width: 26px; height: 26px; border-radius: 50%;',
        'background: ', dot_bg, '; color: ', dot_color, ';',
        'display: flex; align-items: center; justify-content: center;',
        'font-size: 11px; font-weight: 700; flex-shrink: 0; margin-top: 1px;'
      ),
      dot_inner
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

introTabServer <- function(id, districts_shp) {
  moduleServer(id, function(input, output, session) {

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

    # ── Region / district selectors — any user, any district ─────────────────

    observe({
      regions <- sort(unique(as.character(stats::na.omit(districts_shp$region_name))))
      updateSelectInput(session, 'region',
                        choices  = c(setNames('', 'Select region...'), regions), selected = '')
    })

    observeEvent(input$region, {
      if (!nzchar(input$region %||% '')) {
        updateSelectInput(session, 'district',
                          choices = setNames('', 'Select district...'), selected = '')
        return()
      }
      dists <- districts_shp |>
        dplyr::filter(region_name == input$region) |>
        dplyr::pull(district_name) |>
        as.character() |> unique() |> sort()
      updateSelectInput(session, 'district',
                        choices = c(setNames('', 'Select district...'), dists), selected = '')
    }, ignoreInit = FALSE)

    # ── Zone ─────────────────────────────────────────────────────────────────

    zone_derived <- reactive({
      req(nzchar(input$district %||% ''))
      d <- districts_shp |> dplyr::filter(district_name == input$district)
      if (nrow(d) == 0) return('')
      as.character(d$zone_name[1]) %||% ''
    })

    # ── District sf — the whole district, no urban/rural split ───────────────

    district_sf_full <- reactive({
      req(nzchar(input$district %||% ''))
      dsf <- districts_shp |>
        dplyr::filter(district_name == input$district) |>
        dplyr::summarise(
          district_name = dplyr::first(district_name),
          geometry      = sf::st_union(geometry),
          .groups       = 'drop'
        ) |>
        sf::st_as_sf() |>
        safe_make_valid()
      sf::st_transform(dsf, 4326)
    })

    # ── Subdivisions — still fetched, used only as a soft-barrier reference
    #    layer during generation, not to split the planning area ─────────────

    subdivisions_rv     <- reactiveVal(NULL)
    subdivisions_fetched <- reactiveVal(FALSE)

    observeEvent(input$district, {
      subdivisions_rv(NULL)
      subdivisions_fetched(FALSE)
      req(nzchar(input$district %||% ''))
      dsf  <- tryCatch(district_sf_full(), error = function(e) NULL)
      if (is.null(dsf)) { subdivisions_fetched(TRUE); return() }
      subs <- tryCatch(fetch_subdivisions_for_district(dsf), error = function(e) NULL)
      subdivisions_rv(subs)
      subdivisions_fetched(TRUE)
    }, ignoreInit = TRUE)

    planning_area_sf <- reactive({
      req(nzchar(input$district %||% ''))
      district_sf_full()
    })

    planning_label <- reactive({
      req(nzchar(input$district %||% ''))
      input$district
    })

    planning_ready <- reactive({
      nzchar(input$district %||% '') && !is.null(campaign_id()) && isTRUE(subdivisions_fetched())
    })

    observe({
      shinyjs::toggleState('continue', condition = isTRUE(planning_ready()))
    })

    # ── Public interface ──────────────────────────────────────────────────────

    list(
      zone             = zone_derived,
      region           = reactive(input$region),
      district         = reactive(input$district),
      campaign_id      = campaign_id,
      district_ready   = planning_ready,
      planning_ready   = planning_ready,
      planning_label   = planning_label,
      planning_area_sf = planning_area_sf,
      subdivisions_r   = subdivisions_rv
    )
  })
}
