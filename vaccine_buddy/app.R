source('global.R')

# A request carrying ?t=<token> is a supervisor (or a team leader they
# forwarded to) opening their link: no login, just their own page.
# Anything else is the admin app behind the adjuster's login screen.
#   ?t=<token>&pdf=1 is the same supervisor's map to download (PDF).
ui <- function(req) {
  q <- parseQueryString(req$QUERY_STRING %||% '')
  if (!is.null(q$t) && nzchar(q$t)) { if (identical(q$pdf, '1')) pdf_ui() else supervisor_ui() } else admin_ui()
}

server <- function(input, output, session) {
  q <- parseQueryString(isolate(session$clientData$url_search))
  token <- q$t
  if (!is.null(token) && nzchar(token) && identical(q$pdf, '1')) return(pdf_server(input, output, session, token))
  if (!is.null(token) && nzchar(token)) return(supervisor_server(input, output, session, token))

  auth <- authServer('auth')
  # Admin features (and any database reads behind them) start only after
  # sign-in, not merely hidden behind the login overlay. (Not once = TRUE:
  # that observer would fire at startup with logged_in = FALSE and be gone
  # before anyone signs in.)
  started <- FALSE
  observeEvent(auth$logged_in, {
    req(isTRUE(auth$logged_in), !started)
    started <<- TRUE
    shinyjs::hide('login_screen')
    shinyjs::show('main_app')
    admin_server(input, output, session, auth)
  })
}

shinyApp(ui, server)
