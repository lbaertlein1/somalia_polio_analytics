# =============================================================================
# mod_intro_tab.R
# Left sidebar: district selector + Continue button
# Main panel:   welcome, step guide, video placeholder
#
# Continue button enable/disable and tab navigation are handled entirely
# in client-side JS (ui.R) — no server round-trips, instant response.
# =============================================================================

introTabUI <- function(id, zone_choices) {
  ns <- NS(id)
  
  fluidRow(
    
    # ── Left sidebar — district selector ─────────────────────────────────────
    column(
      width = 2,
      
      div(
        class = 'rightbar-title',
        style = 'margin-top: 4px;',
        'Select District'
      ),
      
      div(class = 'mini-label', 'Zone'),
      selectInput(
        ns('zone'), NULL,
        choices  = c(setNames('', 'Select zone...'), zone_choices),
        selected = '',
        width    = '100%'
      ),
      
      div(class = 'mini-label', 'Region'),
      selectInput(
        ns('region'), NULL,
        choices  = setNames('', 'Select region...'),
        selected = '',
        width    = '100%'
      ),
      
      div(class = 'mini-label', 'District'),
      selectInput(
        ns('district'), NULL,
        choices  = setNames('', 'Select district...'),
        selected = '',
        width    = '100%'
      ),
      
      tags$hr(style = 'margin: 12px 0;'),
      
      # Continue button — disabled by default, enabled via JS when district selected
      # onclick switches tab directly in JS — no server round-trip
      tags$button(
        id      = ns('continue'),
        class   = 'btn btn-primary btn-block intro-continue-btn',
        type    = 'button',
        disabled = NA,
        style   = 'font-weight: 600; font-size: 13px; height: 36px; width: 100%;',
        onclick = paste0(
          "if(!this.disabled){",
          "$('#main_tabs a[data-value=\"tab_orientation\"]').tab('show');",
          "}"
        ),
        'Continue \u2192'
      )
    ),
    
    # ── Main panel — guide + video ────────────────────────────────────────────
    column(
      width = 10,
      
      div(
        style = 'padding: 28px 40px; max-width: 860px;',
        
        tags$h2(
          style = 'font-size: 22px; font-weight: 600; color: #0f172a; margin-bottom: 6px;',
          'Welcome to the District Planning Tool'
        ),
        tags$p(
          style = 'font-size: 14px; color: #64748b; margin-bottom: 28px;',
          'This tool guides district teams through the process of mapping health ',
          'facilities and defining health areas for polio vaccination campaigns. ',
          'Follow the steps below, working from left to right across the tabs.'
        ),
        
        tags$hr(style = 'border-color: #e2e8f0; margin-bottom: 24px;'),
        
        tags$h4(
          style = 'font-size: 13px; font-weight: 700; color: #475569;
                   text-transform: uppercase; letter-spacing: .06em; margin-bottom: 16px;',
          'How it works'
        ),
        
        div(
          style = 'display: flex; flex-direction: column; gap: 12px; margin-bottom: 32px;',
          .intro_step('1', 'Select your district',
                      'Choose your zone, region, and district from the panel on the left.'),
          .intro_step('2', 'Map health facilities',
                      'Review MHFL facility locations on the map. Drag pins to correct GPS positions. Mark SIA coordination sites.'),
          .intro_step('3', 'Define health areas',
                      'Paint health area boundaries on the map. Each coordination site anchors one health area.'),
          .intro_step('4', 'Complete planning data',
                      'Enter population estimates, team counts, supervisor names, and coordinator contacts for each health area.')
        ),
        
        tags$hr(style = 'border-color: #e2e8f0; margin-bottom: 24px;'),
        
        div(
          style = 'margin-bottom: 24px;',
          tags$h4(
            style = 'font-size: 13px; font-weight: 700; color: #475569;
                     text-transform: uppercase; letter-spacing: .06em; margin-bottom: 12px;',
            'Overview video'
          ),
          # Set src = 'https://www.youtube.com/embed/YOUR_VIDEO_ID' when ready
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
}


# ── Step card helper ──────────────────────────────────────────────────────────
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
# Server — no enable/disable logic here, handled by JS in ui.R
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
