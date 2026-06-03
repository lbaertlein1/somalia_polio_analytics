# =============================================================================
# mod_session_manager.R
#
# Versioned saving:
#   - Each district+mode has an independent version chain.
#   - DB writes happen ONLY via submit_stage(), called by each tab's Submit button.
#   - "Start new" archives the current version before clearing state.
#   - Practice and Actual sessions are stored and retrieved independently.
#
# On district/mode change:
#   - Checks district_submissions for a current submission matching district+mode.
#   - If found → show modal: Resume / Start new
#   - If not found → start silently
# =============================================================================


sessionToolbarUI <- function(id) {
  ns <- NS(id)
  
  shinyjs::hidden(
    div(
      id    = 'session_bar',
      style = paste0(
        'display:flex;align-items:center;gap:8px;',
        'padding:4px 10px;background:#f8fafc;',
        'border-bottom:1px solid #e2e8f0;font-size:12px;'
      ),
      uiOutput(ns('session_label'), inline = TRUE)
    )
  )
}


# district_r       — planning label e.g. "Kismayo — Urban"
# is_practice_r    — reactive logical from intro module
sessionManagerServer <- function(id, username_r, district_r, district_ready_r,
                                 is_practice_r = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Resolve default inside moduleServer where reactive context is available
    .is_practice <- if (is.null(is_practice_r)) reactiveVal(FALSE) else is_practice_r
    
    rv <- reactiveValues(
      active          = FALSE,
      pending_saved   = NULL,   # submission shown in resume modal
      restore_snap    = NULL,   # set only on "Resume" click
      restore_counter = 0L      # incremented only on "Resume"
    )
    
    # ── On district or mode change: check for existing submission ─────────────
    
    # Track which district+mode the session was last activated for so that
    # re-fires of district_ready_r() don't reset an already-active session.
    last_activated_key <- reactiveVal(NULL)
    
    observeEvent(list(district_r(), district_ready_r(), .is_practice()), {
      req(isTRUE(district_ready_r()))
      req(nzchar(district_r() %||% ''))
      
      session_key <- paste0(district_r(), '|', .is_practice())
      
      # Already active for this exact district+mode — don't re-check DB
      if (isTRUE(rv$active) && identical(last_activated_key(), session_key)) return()
      
      submission <- tryCatch(
        db_get_district_submission(pool, district_r(), .is_practice()),
        error = function(e) { cat('[session] DB check error:', e$message, '\n'); NULL }
      )
      
      if (!is.null(submission)) {
        rv$pending_saved <- submission
        .show_resume_modal(submission, .is_practice())
      } else {
        rv$active        <- TRUE
        rv$restore_snap  <- NULL
        rv$pending_saved <- NULL
        last_activated_key(session_key)
      }
    }, ignoreInit = TRUE)
    
    # Keep last_activated_key in sync when session becomes active via Resume/Start new
    observeEvent(rv$active, {
      if (isTRUE(rv$active)) {
        last_activated_key(paste0(district_r() %||% '', '|', .is_practice()))
      }
    }, ignoreInit = TRUE)
    
    # ── Resume modal ──────────────────────────────────────────────────────────
    
    .check <- function(flag) if (isTRUE(flag)) '\u2713' else '\u2013'
    .col   <- function(flag) if (isTRUE(flag)) '#166534' else '#94a3b8'
    
    .show_resume_modal <- function(sub, practice) {
      saved_time <- tryCatch(
        format(sub$last_submitted_at, '%d %b %Y, %H:%M'),
        error = function(e) 'unknown time'
      )
      started_time <- tryCatch(
        format(sub$first_submitted_at, '%d %b %Y, %H:%M'),
        error = function(e) 'unknown'
      )
      
      stage_items <- list(
        list(label = 'Landmarks',    flag = sub$has_landmarks),
        list(label = 'Facilities',   flag = sub$has_facilities),
        list(label = 'Health Areas', flag = sub$has_areas),
        list(label = 'Microplan',    flag = sub$has_microplan)
      )
      
      stage_tags <- lapply(stage_items, function(s) {
        tags$span(
          style = paste0('color:', .col(s$flag), '; margin-right: 14px; font-size: 12px;'),
          paste(.check(s$flag), s$label)
        )
      })
      
      mode_badge <- if (practice) {
        tags$span(
          style = paste0(
            'background:#fef9c3;color:#854d0e;border:1px solid #fde68a;',
            'border-radius:20px;padding:1px 8px;font-size:10px;font-weight:700;',
            'margin-left:6px;vertical-align:middle;'
          ),
          'PRACTICE'
        )
      } else {
        tags$span(
          style = paste0(
            'background:#f0fdf4;color:#166534;border:1px solid #bbf7d0;',
            'border-radius:20px;padding:1px 8px;font-size:10px;font-weight:700;',
            'margin-left:6px;vertical-align:middle;'
          ),
          'ACTUAL'
        )
      }
      
      showModal(modalDialog(
        title     = tagList('Resume previous submission?', mode_badge),
        easyClose = FALSE,
        footer    = NULL,
        size      = 'm',
        
        div(
          style = paste0(
            'border:1px solid #e2e8f0;border-radius:8px;',
            'padding:14px 16px;margin-bottom:16px;background:#f8fafc;'
          ),
          div(
            style = 'font-size:13px;font-weight:600;color:#0f172a;margin-bottom:8px;',
            sub$district_name
          ),
          div(style = 'margin-bottom: 6px;', do.call(tagList, stage_tags)),
          div(
            style = 'font-size:11px;color:#94a3b8;',
            paste0(
              'Started: ', started_time, '  \u00b7  ',
              'Last submitted: ', saved_time,
              if (nzchar(sub$submitted_by %||% ''))
                paste0('  \u00b7  by ', sub$submitted_by)
              else '',
              '  \u00b7  v', sub$version
            )
          )
        ),
        
        div(
          style = 'display:flex;gap:10px;justify-content:flex-end;',
          actionButton(session$ns('new_session_btn'), 'Start new',
                       class = 'btn btn-default'),
          actionButton(session$ns('resume_btn'), 'Resume',
                       class = 'btn btn-primary', style = 'font-weight:600;')
        )
      ))
    }
    
    # ── Resume ────────────────────────────────────────────────────────────────
    
    observeEvent(input$resume_btn, {
      sub <- rv$pending_saved
      req(!is.null(sub))
      rv$active          <- TRUE
      rv$restore_snap    <- sub$snap
      rv$restore_counter <- rv$restore_counter + 1L
      rv$pending_saved   <- NULL
      removeModal()
      showNotification('Previous submission restored.', type = 'message', duration = 2)
    }, ignoreInit = TRUE)
    
    # ── Start new: archive current, clear state ───────────────────────────────
    
    observeEvent(input$new_session_btn, {
      dname    <- district_r() %||% ''
      practice <- .is_practice()
      
      if (nzchar(dname)) {
        tryCatch(
          db_archive_current_submission(pool, dname, practice),
          error = function(e) cat('[session] archive error:', e$message, '\n')
        )
      }
      
      rv$active        <- TRUE
      rv$restore_snap  <- NULL
      rv$pending_saved <- NULL
      removeModal()
    }, ignoreInit = TRUE)
    
    # ── Session bar ───────────────────────────────────────────────────────────
    
    observe({
      req(isTRUE(rv$active))
      shinyjs::show('session_bar')
    })
    
    output$session_label <- renderUI({
      req(rv$active)
      practice <- .is_practice()
      badge <- if (practice)
        tags$span(
          style = paste0(
            'background:#fef9c3;color:#854d0e;border:1px solid #fde68a;',
            'border-radius:20px;padding:1px 7px;font-size:10px;font-weight:700;',
            'margin-left:6px;'
          ),
          'PRACTICE'
        )
      else NULL
      
      tagList(
        tags$span(
          style = 'color:#64748b;font-size:12px;',
          sprintf('%s  \u00b7  %s',
                  district_r() %||% '',
                  username_r()  %||% '')
        ),
        badge
      )
    })
    
    # ── submit_stage: the ONLY function that writes to DB ─────────────────────
    
    submit_stage <- function(stage, data) {
      if (!nzchar(district_r() %||% '')) {
        showNotification('No district selected.',
                         type = 'warning', duration = 4)
        return(invisible(NULL))
      }
      
      tryCatch({
        db_submit_stage(pool, district_r(), username_r() %||% '', stage, data,
                        is_practice = .is_practice())
        stage_label <- switch(stage,
                              landmarks  = 'Landmarks',
                              facilities = 'Facilities',
                              areas      = 'Health Areas',
                              microplan  = 'Microplan',
                              stage
        )
        showNotification(
          paste0(stage_label, ' submitted successfully.'),
          type = 'message', duration = 3
        )
      }, error = function(e) {
        showNotification(
          paste0('Submit failed: ', e$message),
          type = 'error', duration = 6
        )
      })
      
      invisible(NULL)
    }
    
    # ── save_snapshot: no-op kept for compat ─────────────────────────────────
    
    save_snapshot <- function(snapshot_data, trigger = 'local') invisible(NULL)
    
    # ── Public interface ──────────────────────────────────────────────────────
    
    list(
      active = reactive(rv$active),
      
      restore_snapshot = reactive({
        rv$restore_counter
        if (rv$restore_counter < 1L) return(NULL)
        rv$restore_snap
      }),
      
      submit_stage   = submit_stage,
      save_snapshot  = save_snapshot,
      set_collect_fn = function(fn) invisible(NULL)
    )
  })
}