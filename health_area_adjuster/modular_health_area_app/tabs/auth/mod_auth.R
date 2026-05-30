# =============================================================================
# mod_auth.R  —  full-screen login overlay + user session
#
# Returns a reactiveValues with:
#   $logged_in         logical
#   $username          character
#   $display_name      character
#   $role              'admin' | 'user'
#   $allowed_districts character vector of district_name, OR 'ALL' for admin
#
# Phase 6 migration: replace validate_credentials() body with a DB query.
#   The returned contract (list or NULL) must stay the same.
# =============================================================================

authUI <- function(id) {
  ns <- NS(id)
  
  div(
    id    = 'login_screen',
    style = paste0(
      'position:fixed;inset:0;background:#0d1424;',
      'display:flex;align-items:center;justify-content:center;z-index:9999;'
    ),
    
    div(
      style = 'background:#fff;border-radius:12px;padding:36px 32px;width:340px;',
      
      # Logo + title
      div(
        style = 'text-align:center;margin-bottom:28px;',
        div(
          style = 'margin-bottom:12px;',
          tags$img(
            src    = 'https://upload.wikimedia.org/wikipedia/commons/a/a0/Flag_of_Somalia.svg',
            width  = '72px',
            height = 'auto',
            style  = 'border-radius:6px; box-shadow:0 1px 4px rgba(0,0,0,0.2);'
          )
        ),
        tags$div(
          tags$h4(
            style = 'margin:0 0 2px;color:#0f172a;font-size:18px;font-weight:600;',
            'Somalia District Health Area Planning'
          )
        ),
        tags$p(
          style = 'margin:4px 0 0;color:#64748b;font-size:12px;line-height:1.5;',
          'Supporting immunization campaigns and public health outreach'
        ),
        tags$p(
          style = 'margin:8px 0 0;color:#94a3b8;font-size:13px;',
          'Sign in to continue'
        )
      ),
      
      uiOutput(ns('error_msg')),
      
      div(class = 'mini-label', style = 'margin-top:4px;', 'Username'),
      textInput(ns('username'), NULL, placeholder = 'Enter username', width = '100%'),
      
      div(class = 'mini-label', 'Password'),
      passwordInput(ns('password'), NULL, placeholder = 'Enter password', width = '100%'),
      
      actionButton(
        ns('login_btn'), 'Sign in',
        class = 'btn btn-primary',
        width = '100%',
        style = 'margin-top:10px;font-weight:600;font-size:14px;height:40px;'
      )
    ),
    
    # Submit on Enter key
    tags$script(HTML(sprintf(
      "$(document).on('keydown', function(e) {
         if (e.which === 13 && $('#login_screen').is(':visible')) {
           $('#%s').click();
         }
       });",
      ns('login_btn')
    )))
  )
}


authServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    user_session <- reactiveValues(
      logged_in         = FALSE,
      username          = NULL,
      display_name      = NULL,
      role              = NULL,
      allowed_districts = NULL
    )
    
    output$error_msg <- renderUI(NULL)
    
    # Phase 6: replace this function body with a DBI/pool query.
    validate_credentials <- function(uname, pword) {
      db_validate_credentials(pool, uname, pword)
    }
    
    observeEvent(input$login_btn, {
      uname <- trimws(input$username %||% '')
      pword <- input$password        %||% ''
      
      if (!nzchar(uname) || !nzchar(pword)) {
        output$error_msg <- renderUI(
          .auth_error('Please enter your username and password.')
        )
        return()
      }
      
      match_row <- validate_credentials(uname, pword)
      
      if (is.null(match_row)) {
        output$error_msg <- renderUI(
          .auth_error('Incorrect username or password.')
        )
        return()
      }
      
      output$error_msg <- renderUI(NULL)
      
      role <- match_row$role
      
      allowed <- db_get_allowed_districts(pool, uname, role)
      
      user_session$logged_in         <- TRUE
      user_session$username          <- uname
      user_session$display_name      <- match_row$display_name
      user_session$role              <- role
      user_session$allowed_districts <- allowed
      
    }, ignoreInit = TRUE)
    
    user_session
  })
}

.auth_error <- function(msg) {
  div(
    style = paste0(
      'color:#dc2626;font-size:12px;margin-bottom:10px;',
      'background:#fef2f2;border:1px solid #fecaca;',
      'border-radius:6px;padding:8px 10px;'
    ),
    msg
  )
}