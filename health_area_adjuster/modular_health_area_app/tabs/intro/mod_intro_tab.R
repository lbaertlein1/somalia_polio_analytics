# =============================================================================
# mod_intro_tab.R
# =============================================================================

introTabUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    
    # ── Left sidebar — district selector ─────────────────────────────────────
    column(
      width = 2,
      
      div(class = 'rightbar-title', style = 'margin-top: 4px;', 'Select District'),
      
      div(class = 'mini-label', 'Region'),
      selectInput(ns('region'), NULL,
                  choices  = setNames('', 'Select region...'),
                  selected = '', width = '100%'),
      
      div(class = 'mini-label', 'District'),
      selectInput(ns('district'), NULL,
                  choices  = setNames('', 'Select district...'),
                  selected = '', width = '100%'),
      
      uiOutput(ns('planning_unit_ui')),
      
      tags$hr(style = 'margin: 12px 0;'),
      
      # ── Practice / Actual toggle ──────────────────────────────────────────
      div(class = 'mini-label', 'Session type'),
      radioButtons(
        ns('mode_click'), label = NULL,
        choices  = c('Actual' = 'actual', 'Practice' = 'practice'),
        selected = 'actual',
        inline   = TRUE
      ),
      
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
        
        .intro_section('Objective',
                       tags$p(
                         style = 'font-size: 13px; color: #475569; line-height: 1.7; margin: 0;',
                         'This tool supports district teams in preparing for polio SIA campaigns by ',
                         'designating SIA coordination sites and drawing health area boundaries. ',
                         'The output serves as the starting point for health area microplanning.'
                       )
        ),
        
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
        
        .intro_section('How it works',
                       div(
                         style = 'display: flex; flex-direction: column; gap: 10px;',
                         .intro_step('1', 'Select your district',
                                     'Choose your region and district from the panel on the left, then click Continue.'),
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

introTabServer <- function(id, districts_shp, allowed_districts_r = reactive('ALL')) {
  moduleServer(id, function(input, output, session) {
    
    allowed_shp <- reactive({
      allowed <- allowed_districts_r()
      if (identical(allowed, 'ALL') || length(allowed) == 0) return(districts_shp)
      districts_shp |> dplyr::filter(district_name %in% allowed)
    })
    
    # ── Practice / Actual mode ────────────────────────────────────────────────
    
    # TRUE = practice, FALSE = actual
    is_practice_rv <- reactiveVal(FALSE)
    
    # Pending mode change (waiting for user confirmation if session active)
    pending_mode_rv <- reactiveVal(NULL)
    
    observeEvent(input$mode_click, {
      new_mode <- input$mode_click == 'practice'
      if (new_mode == is_practice_rv()) return()   # no change
      
      # If a district is already selected, warn before switching
      if (nzchar(input$district %||% '')) {
        pending_mode_rv(new_mode)
        showModal(modalDialog(
          title     = 'Switch session type?',
          easyClose = FALSE,
          footer    = NULL,
          size      = 's',
          div(
            style = 'font-size: 13px; color: #475569; margin-bottom: 16px;',
            if (new_mode)
              'Switching to Practice mode will clear your current session. Practice data is saved separately and will not affect actual submissions.'
            else
              'Switching to Actual mode will clear your current session. Any unsaved changes will be lost.'
          ),
          div(
            style = 'display:flex;gap:10px;justify-content:flex-end;',
            actionButton(session$ns('mode_switch_cancel'), 'Cancel',
                         class = 'btn btn-default'),
            actionButton(session$ns('mode_switch_confirm'), 'Switch',
                         class = 'btn btn-warning', style = 'font-weight:600;')
          )
        ))
      } else {
        # No active district — switch silently
        .apply_mode(new_mode)
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$mode_switch_confirm, {
      new_mode <- pending_mode_rv()
      req(!is.null(new_mode))
      pending_mode_rv(NULL)
      removeModal()
      .apply_mode(new_mode)
    }, ignoreInit = TRUE)
    
    observeEvent(input$mode_switch_cancel, {
      pending_mode_rv(NULL)
      removeModal()
      # Revert radio to current mode
      updateRadioButtons(session, 'mode_click',
                         selected = if (is_practice_rv()) 'practice' else 'actual')
    }, ignoreInit = TRUE)
    
    .apply_mode <- function(new_mode) {
      is_practice_rv(new_mode)
    }
    
    # ── Region / district selectors ───────────────────────────────────────────
    
    observeEvent(allowed_shp(), {
      regions <- sort(unique(as.character(stats::na.omit(allowed_shp()$region_name))))
      updateSelectInput(session, 'region',
                        choices  = c(setNames('', 'Select region...'), regions), selected = '')
      updateSelectInput(session, 'district',
                        choices  = setNames('', 'Select district...'), selected = '')
    }, ignoreInit = FALSE)
    
    observeEvent(input$region, {
      if (!nzchar(input$region %||% '')) {
        updateSelectInput(session, 'district',
                          choices = setNames('', 'Select district...'), selected = '')
        return()
      }
      dists <- allowed_shp() |>
        dplyr::filter(region_name == input$region) |>
        dplyr::pull(district_name) |>
        as.character() |> unique() |> sort()
      updateSelectInput(session, 'district',
                        choices = c(setNames('', 'Select district...'), dists), selected = '')
    }, ignoreInit = FALSE)
    
    # ── Zone ─────────────────────────────────────────────────────────────────
    
    zone_derived <- reactive({
      req(nzchar(input$district %||% ''))
      d <- allowed_shp() |> dplyr::filter(district_name == input$district)
      if (nrow(d) == 0) return('')
      as.character(d$zone_name[1]) %||% ''
    })
    
    # ── District sf ──────────────────────────────────────────────────────────
    
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
    
    # ── Subdivisions ─────────────────────────────────────────────────────────
    
    subdivisions_rv     <- reactiveVal(NULL)
    subdivisions_fetched <- reactiveVal(FALSE)
    
    observeEvent(input$district, {
      subdivisions_rv(NULL)
      urban_hull_rv(NULL)
      rural_remain_rv(NULL)
      subdivisions_fetched(FALSE)
      req(nzchar(input$district %||% ''))
      dsf  <- tryCatch(district_sf_full(), error = function(e) NULL)
      if (is.null(dsf)) { subdivisions_fetched(TRUE); return() }
      subs <- tryCatch(fetch_subdivisions_for_district(dsf), error = function(e) NULL)
      subdivisions_rv(subs)
      if (is.null(subs) || nrow(subs) == 0) subdivisions_fetched(TRUE)
    }, ignoreInit = TRUE)
    
    urban_hull_rv   <- reactiveVal(NULL)
    rural_remain_rv <- reactiveVal(NULL)
    
    observeEvent(subdivisions_rv(), {
      subs <- subdivisions_rv()
      dsf  <- tryCatch(district_sf_full(), error = function(e) NULL)
      if (is.null(subs) || nrow(subs) == 0 || is.null(dsf)) {
        urban_hull_rv(NULL); rural_remain_rv(NULL)
        subdivisions_fetched(TRUE)
        return()
      }
      hull  <- compute_urban_hull(subs, dsf)
      rural <- compute_rural_remainder(dsf, hull)
      urban_hull_rv(hull)
      rural_remain_rv(rural)
      subdivisions_fetched(TRUE)
    }, ignoreInit = TRUE)
    
    # ── Planning unit UI ─────────────────────────────────────────────────────
    
    output$planning_unit_ui <- renderUI({
      req(nzchar(input$district %||% ''))
      hull <- urban_hull_rv()
      if (is.null(hull)) return(NULL)
      tagList(
        tags$hr(style = 'margin: 8px 0;'),
        div(class = 'mini-label', 'Planning area'),
        selectInput(
          session$ns('planning_unit'), NULL,
          choices  = c('Urban area', if (!is.null(rural_remain_rv())) 'Rural area'),
          selected = 'Urban area',
          width    = '100%'
        )
      )
    })
    
    planning_area_sf <- reactive({
      req(nzchar(input$district %||% ''))
      unit <- input$planning_unit %||% 'Urban area'
      if (unit == 'Urban area' && !is.null(urban_hull_rv()))   return(urban_hull_rv())
      if (unit == 'Rural area' && !is.null(rural_remain_rv())) return(rural_remain_rv())
      district_sf_full()
    })
    
    planning_label <- reactive({
      req(nzchar(input$district %||% ''))
      unit <- input$planning_unit %||% 'Urban area'
      if (!is.null(urban_hull_rv()))
        return(paste0(input$district, ' — ', unit))
      input$district
    })
    
    planning_ready <- reactive({
      nzchar(input$district %||% '') && isTRUE(subdivisions_fetched())
    })
    
    # ── Public interface ──────────────────────────────────────────────────────
    
    list(
      zone             = zone_derived,
      region           = reactive(input$region),
      district         = reactive(input$district),
      district_ready   = planning_ready,
      planning_ready   = planning_ready,
      planning_label   = planning_label,
      planning_area_sf = planning_area_sf,
      subdivisions_r   = subdivisions_rv,
      is_practice      = is_practice_rv          # <-- new: passed to session_mgr
    )
  })
}