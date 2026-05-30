# =============================================================================
# mod_session_manager.R
#
# Saving is now purely local (in-memory, within the browser session).
# DB writes happen ONLY via submit_stage(), called by each tab's Submit button.
#
# On district select:
#   - Checks district_submissions for a prior submission for this district.
#   - If found → show modal: Resume / Start new
#   - If not found → start silently
#
# Resume restores from the last submitted state (district_submissions).
# =============================================================================


sessionToolbarUI <- function(id) {
  # Toolbar is now minimal — no save button since saving is local only.
  # Kept as a visible district/user indicator once a district is selected.
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


# district_r is now the planning label (e.g. "Kismayo — Urban", "Dolow")
# passed from intro$planning_label() in server.R
sessionManagerServer <- function(id, username_r, district_r, district_ready_r) {
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(
      active          = FALSE,
      pending_saved   = NULL,   # submission data shown in resume modal
      restore_snap    = NULL,   # set only on "Resume" click
      restore_counter = 0L      # incremented only on "Resume" — never on submit
    )
    
    # ── On planning label change: check for existing submission ─────────────────
    # Fires when district changes OR when user switches between Full/Urban/Rural.
    # district_ready_r() gates on subdivision fetch completing, so planning_label
    # is stable by the time this fires.
    
    observeEvent(list(district_r(), district_ready_r()), {
      req(isTRUE(district_ready_r()))
      req(nzchar(district_r() %||% ''))
      
      submission <- tryCatch(
        db_get_district_submission(pool, district_r()),
        error = function(e) { cat('[session] DB check error:', e$message, '\n'); NULL }
      )
      
      if (!is.null(submission)) {
        rv$pending_saved <- submission
        .show_resume_modal(submission)
      } else {
        rv$active         <- TRUE
        rv$restore_snap   <- NULL
        rv$pending_saved  <- NULL
      }
    }, ignoreInit = TRUE)
    
    # ── Resume modal ──────────────────────────────────────────────────────────
    
    .check <- function(flag) if (isTRUE(flag)) '\u2713' else '\u2013'
    .col   <- function(flag) if (isTRUE(flag)) '#166534' else '#94a3b8'
    
    .show_resume_modal <- function(sub) {
      saved_time <- tryCatch(
        format(sub$last_submitted_at, '%d %b %Y, %H:%M'),
        error = function(e) 'unknown time'
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
      
      showModal(modalDialog(
        title     = 'Resume previous submission?',
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
            paste0('Last submitted: ', saved_time,
                   if (nzchar(sub$submitted_by %||% ''))
                     paste0(' by ', sub$submitted_by)
                   else '')
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
    
    observeEvent(input$resume_btn, {
      sub <- rv$pending_saved
      req(!is.null(sub))
      rv$active          <- TRUE
      rv$restore_snap    <- sub$snap
      rv$restore_counter <- rv$restore_counter + 1L   # triggers restore in tabs
      rv$pending_saved   <- NULL
      removeModal()
      showNotification('Previous submission restored.', type = 'message', duration = 2)
    }, ignoreInit = TRUE)
    
    observeEvent(input$new_session_btn, {
      rv$active         <- TRUE
      rv$restore_snap   <- NULL
      rv$pending_saved  <- NULL
      removeModal()
    }, ignoreInit = TRUE)
    
    # ── Session bar ───────────────────────────────────────────────────────────
    
    observe({
      req(isTRUE(rv$active))
      shinyjs::show('session_bar')
    })
    
    output$session_label <- renderUI({
      req(rv$active)
      tags$span(
        style = 'color:#64748b;font-size:12px;',
        sprintf('%s  \u00b7  %s',
                district_r() %||% '',
                username_r()  %||% '')
      )
    })
    
    # ── submit_stage: the ONLY function that writes to DB ─────────────────────
    # Called by each tab's Submit button handler.
    # stage = "landmarks" | "facilities" | "areas" | "microplan"
    # data  = list with the stage's fields
    
    submit_stage <- function(stage, data) {
      req(isTRUE(rv$active))
      req(nzchar(district_r() %||% ''))
      
      tryCatch({
        db_submit_stage(pool, district_r(), username_r() %||% '', stage, data)
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
    
    # ── save_snapshot: local only, no DB write ────────────────────────────────
    # Kept so existing save_snapshot_fn calls in tab modules don't error.
    # Does nothing — inter-tab state flows through reactive parameters.
    
    save_snapshot <- function(snapshot_data, trigger = 'local') {
      invisible(NULL)
    }
    
    # ── Public interface ──────────────────────────────────────────────────────
    
    list(
      active = reactive(rv$active),
      
      # Fires ONLY when user clicks "Resume" — tab restore observers use this
      restore_snapshot = reactive({
        rv$restore_counter
        if (rv$restore_counter < 1L) return(NULL)
        rv$restore_snap
      }),
      
      submit_stage   = submit_stage,
      save_snapshot  = save_snapshot,                    # no-op, kept for compat
      set_collect_fn = function(fn) invisible(NULL)      # no-op, kept for compat
    )
  })
}