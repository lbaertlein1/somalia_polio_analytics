# R/mod_report.R — Field Teams Report tab

report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "card mb",
        div(class = "card-header",
            span(class = "card-title", "Field Teams Report"),
            tags$button(
              class   = "btn-teal",
              onclick = paste0("Shiny.setInputValue('", ns("dl_click"),
                               "', Math.random(), {priority:'event'})"),
              "\u2b07 Download"
            )
        ),
        div(class = "card-body",
            
            # 3-stat summary
            uiOutput(ns("stat_summary")),
            
            # Key Findings & Actions — AI generated
            div(style = "font-size:14px;font-weight:600;margin-bottom:12px",
                "Key Findings & Actions"),
            uiOutput(ns("actions_list"))
        )
    )
  )
}

report_server <- function(id, data, year_filter, insights = NULL) {
  moduleServer(id, function(input, output, session) {
    yr <- reactive(year_filter())
    
    # ── 3-stat summary ─────────────────────────────────────────────────────────
    output$stat_summary <- renderUI({
      kpi <- data$kpi[[yr()]]
      stat_box <- function(value, label, color = "var(--text)") {
        div(style = "padding:14px;background:#fafbfc;border:1px solid var(--border);border-radius:8px",
            div(style = paste0("font-size:22px;font-weight:700;",
                               "font-family:'JetBrains Mono',monospace;color:", color),
                format(value, big.mark = ",")),
            div(style = "font-size:11px;color:var(--muted)", label)
        )
      }
      div(style = "display:grid;grid-template-columns:repeat(3,1fr);gap:12px;margin-bottom:20px",
          stat_box(kpi$camps,      "Camps enumerated"),
          stat_box(kpi$zd_total,   "Zero-dose children",    "var(--red)"),
          stat_box(kpi$vaccinated, "Children vaccinated",   "var(--green)")
      )
    })
    
    # ── Key Findings & Actions (AI) ────────────────────────────────────────────
    if (!is.null(insights)) {
      render_insight(output, "actions_list", insights, "report_actions",
                     class = "insight")
    } else {
      output$actions_list <- renderUI({
        div(class = "insight insight-warn",
            HTML("Key findings will appear here once insights are generated."))
      })
    }
    
    # ── Download ───────────────────────────────────────────────────────────────
    observeEvent(input$dl_click, {
      kpi    <- data$kpi[[yr()]]
      yr_lbl <- switch(yr(),
                       all    = "All time (Jun 2024 \u2013 present)",
                       `2024` = "January \u2013 December 2024",
                       `2025` = "January \u2013 December 2025",
                       `2026` = "January \u2013 April 2026"
      )
      txt <- paste0(
        "NOMADIC MAPPING \u2014 POLIO ANALYTICS\n",
        "Field Teams Report\n",
        "Generated: ", format(Sys.Date(), "%d %b %Y"), "\n",
        "Period: ", yr_lbl, "\n\n",
        "HEADLINE NUMBERS\n",
        "  Camps enumerated:    ", format(kpi$camps,      big.mark = ","), "\n",
        "  Zero-dose children:  ", format(kpi$zd_total,   big.mark = ","), "\n",
        "  Children vaccinated: ", format(kpi$vaccinated, big.mark = ","), "\n",
        "  Outreach sessions:   ", format(kpi$sessions,   big.mark = ","), "\n\n",
        "See dashboard for full district breakdown and maps.\n"
      )
      session$sendCustomMessage("download_file", list(
        content  = txt,
        filename = paste0("Nomadic_Report_", yr(), "_",
                          format(Sys.Date(), "%Y%m%d"), ".txt")
      ))
    })
  })
}