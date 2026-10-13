# R/mod_kpi.R
# Five KPI cards — camps, population, zero-dose, vaccinated, sessions

kpi_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("kpi_bar"))
}

kpi_server <- function(id, data, year_filter) {
  moduleServer(id, function(input, output, session) {
    output$kpi_bar <- renderUI({
      yr  <- year_filter()
      kpi <- data$kpi[[yr]]

      fmt <- function(n) {
        n <- as.numeric(n)
        if (is.na(n)) return("—")
        if (n >= 1e6) paste0(round(n / 1e6, 1), "M")
        else if (n >= 1e3) formatC(n, format = "d", big.mark = ",")
        else as.character(n)
      }

      make_card <- function(icon_html, value, label, value_color = NULL) {
        style_val <- if (!is.null(value_color))
          paste0("color:", value_color, ";") else ""
        div(class = "kpi-card",
          div(class = "kpi-icon", HTML(icon_html)),
          div(
            div(class = "kpi-value", style = style_val, fmt(value)),
            div(class = "kpi-label", label)
          )
        )
      }

      icon_camps <- '<svg viewBox="0 0 48 48" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path d="M24 6L4 36h40L24 6z" fill="#0d9488" opacity=".15" stroke="#0d9488" stroke-width="2"/>
        <path d="M24 14L10 36h28L24 14z" fill="#0d9488" opacity=".3" stroke="#0d9488" stroke-width="1.5"/>
        <rect x="20" y="36" width="8" height="6" rx="1" fill="#0d9488"/>
      </svg>'

      icon_pop <- '<svg viewBox="0 0 48 48" fill="none" xmlns="http://www.w3.org/2000/svg">
        <circle cx="16" cy="16" r="7" fill="#2563eb" opacity=".7"/>
        <circle cx="32" cy="16" r="7" fill="#2563eb" opacity=".5"/>
        <path d="M4 40c0-7 5-12 12-12s12 5 12 12" fill="#2563eb" opacity=".7"/>
        <path d="M20 40c0-7 5-12 12-12s12 5 12 12" fill="#2563eb" opacity=".4"/>
      </svg>'

      icon_zd <- '<svg viewBox="0 0 48 48" fill="none" xmlns="http://www.w3.org/2000/svg">
        <rect x="20" y="6" width="8" height="24" rx="4" fill="#dc2626" opacity=".7"/>
        <circle cx="24" cy="38" r="4" fill="#dc2626" opacity=".7"/>
        <line x1="8" y1="8" x2="40" y2="40" stroke="#dc2626" stroke-width="3" stroke-linecap="round"/>
      </svg>'

      icon_vacc <- '<svg viewBox="0 0 48 48" fill="none" xmlns="http://www.w3.org/2000/svg">
        <rect x="22" y="4" width="4" height="8" rx="2" fill="#059669"/>
        <rect x="18" y="12" width="12" height="20" rx="3" fill="#059669" opacity=".7"/>
        <rect x="20" y="14" width="8" height="10" rx="2" fill="#e6f7f6"/>
        <path d="M22 32l-8 10h20L22 32z" fill="#059669" opacity=".5"/>
      </svg>'

      icon_sess <- '<svg viewBox="0 0 48 48" fill="none" xmlns="http://www.w3.org/2000/svg">
        <circle cx="24" cy="18" r="10" fill="#d97706" opacity=".3" stroke="#d97706" stroke-width="2"/>
        <path d="M24 28v8M18 34h12" stroke="#d97706" stroke-width="2.5" stroke-linecap="round"/>
        <circle cx="24" cy="18" r="4" fill="#d97706"/>
      </svg>'

      div(class = "kpi-bar",
        make_card(icon_camps, kpi$camps,      "Camps enumerated"),
        make_card(icon_pop,   kpi$pop,        "Total population"),
        make_card(icon_zd,    kpi$zd_total,   "Zero-dose children identified", "#dc2626"),
        make_card(icon_vacc,  kpi$vaccinated, "Children vaccinated (outreach)", "#059669"),
        make_card(icon_sess,  kpi$sessions,   "Outreach sessions")
      )
    })
  })
}
