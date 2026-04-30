# =============================================================================
# mod_intro_tab.R
# =============================================================================

introTabUI <- function(id, zone_choices) {
  ns <- NS(id)
  
  fluidRow(
    
    # ── Left sidebar — district selector ─────────────────────────────────────
    column(
      width = 2,
      
      div(class = 'rightbar-title', style = 'margin-top: 4px;', 'Select District'),
      
      div(class = 'mini-label', 'Zone'),
      selectInput(ns('zone'), NULL,
                  choices  = c(setNames('', 'Select zone...'), zone_choices),
                  selected = '', width = '100%'),
      
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
          'Prepare districts for polio SIA campaigns by designating SIA coordination ',
          'sites, drawing health area boundaries, and recording planning data for ',
          'each vaccination team.'
        ),
        
        tags$hr(style = 'border-color: #e2e8f0; margin-bottom: 24px;'),
        
        # ── Objective ─────────────────────────────────────────────────────────
        .intro_section('Objective',
                       tags$p(
                         style = 'font-size: 13px; color: #475569; line-height: 1.7; margin: 0;',
                         'This tool supports district teams in preparing for polio SIA campaigns by ',
                         'designating SIA coordination sites and drawing health area boundaries. ',
                         'The output serves as the starting point for health area microplanning.'
                       )
        ),
        
        # ── Key concepts ──────────────────────────────────────────────────────
        .intro_section('Key concepts',
                       tagList(
                         .concept_block(
                           'Health area',
                           paste0(
                             'A geographic sub-unit of a district, each overseen by one SIA coordination site ',
                             'and ideally covering around 2,000 under-5 children served by 5\u20136 vaccination ',
                             'teams over one campaign round. ',
                             'In this tool, once coordination sites are selected, boundaries are drawn ',
                             'automatically by expanding outward from each site across the district, ',
                             'weighted by population and how easily areas can be reached. ',
                             'Boundaries can then be adjusted manually to reflect local knowledge.'
                           )
                         ),
                         .concept_block(
                           'SIA coordination site',
                           paste0(
                             'A health facility designated as the base of operations for one vaccination ',
                             'team. Teams assemble here, collect supplies, and report back each day. ',
                             'Coordination sites anchor the health area boundaries \u2014 each site is the ',
                             'centre of its surrounding area. A typical district has one coordination ',
                             'site per 2,000 under-5 children.'
                           )
                         )
                       )
        ),
        
        # ── How it works ──────────────────────────────────────────────────────
        .intro_section('How it works',
                       div(
                         style = 'display: flex; flex-direction: column; gap: 10px;',
                         .intro_step('1', 'Select your district',
                                     'Choose your zone, region, and district from the panel on the left, then click Continue.'),
                         .intro_step('2', 'Add landmarks',
                                     paste0('Familiarise the group with the district. Drop optional landmark pins to mark ',
                                            'recognisable reference points such as schools, hospitals, or road junctions. ',
                                            'Landmarks are for orientation during this session only and are not used elsewhere.')),
                         .intro_step('3', 'Review health facilities and select coordination sites',
                                     paste0('Review health facility locations for your district. Correct any GPS positions ',
                                            'by dragging pins on the map. Mark which facilities will serve as SIA coordination ',
                                            'sites \u2014 these become the anchors for the health area boundaries.')),
                         .intro_step('4', 'Define health areas',
                                     paste0('Boundaries are drawn automatically from the selected coordination sites. ',
                                            'Adjust them using the drawing tool to reflect the group\'s local knowledge ',
                                            'of the district.')),
                         .intro_step('5', 'Complete planning data',
                                     paste0('For each health area, enter the vaccination team count, estimated under-5 ',
                                            'population, and supervisor contact details. Areas turn teal when marked complete.'))
                       )
        ),
        
        # ── Video ─────────────────────────────────────────────────────────────
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


# ── Helpers ───────────────────────────────────────────────────────────────────

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
                `stroke-width` = '1.5', `stroke-linecap` = 'round')
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

introTabServer <- function(id, districts_shp, allowed_districts_r = reactive('ALL')) {
  moduleServer(id, function(input, output, session) {
    
    allowed_shp <- reactive({
      allowed <- allowed_districts_r()
      if (identical(allowed, 'ALL') || length(allowed) == 0) return(districts_shp)
      districts_shp |> dplyr::filter(district_name %in% allowed)
    })
    
    observeEvent(allowed_shp(), {
      zones <- sort(unique(as.character(stats::na.omit(allowed_shp()$zone_name))))
      updateSelectInput(session, 'zone',
                        choices = c(setNames('', 'Select zone...'), zones), selected = '')
      updateSelectInput(session, 'region',
                        choices = setNames('', 'Select region...'), selected = '')
      updateSelectInput(session, 'district',
                        choices = setNames('', 'Select district...'), selected = '')
    }, ignoreInit = FALSE)
    
    observeEvent(input$zone, {
      if (!nzchar(input$zone %||% '')) {
        updateSelectInput(session, 'region',
                          choices = setNames('', 'Select region...'), selected = '')
        updateSelectInput(session, 'district',
                          choices = setNames('', 'Select district...'), selected = '')
        return()
      }
      regions <- allowed_shp() |>
        dplyr::filter(zone_name == input$zone) |>
        dplyr::pull(region_name) |>
        as.character() |> unique() |> sort()
      updateSelectInput(session, 'region',
                        choices = c(setNames('', 'Select region...'), regions), selected = '')
      updateSelectInput(session, 'district',
                        choices = setNames('', 'Select district...'), selected = '')
    }, ignoreInit = FALSE)
    
    observeEvent(list(input$zone, input$region), {
      if (!nzchar(input$zone %||% '') || !nzchar(input$region %||% '')) {
        updateSelectInput(session, 'district',
                          choices = setNames('', 'Select district...'), selected = '')
        return()
      }
      dists <- allowed_shp() |>
        dplyr::filter(zone_name == input$zone, region_name == input$region) |>
        dplyr::pull(district_name) |>
        as.character() |> unique() |> sort()
      updateSelectInput(session, 'district',
                        choices = c(setNames('', 'Select district...'), dists), selected = '')
    }, ignoreInit = FALSE)
    
    list(
      zone           = reactive(input$zone),
      region         = reactive(input$region),
      district       = reactive(input$district),
      district_ready = reactive(nzchar(input$district %||% ''))
    )
  })
}
