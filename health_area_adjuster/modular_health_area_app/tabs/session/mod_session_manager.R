# =============================================================================
# mod_session_manager.R
# Session persistence: save and restore a single snapshot per user+district.
#
# On district select:
#   - If a saved snapshot exists → show modal: Continue / Start new
#   - If none → start silently
#
# Snapshot is written to DB on every save_snapshot() call (overwrites previous).
# Tab modules receive restore_snapshot() which fires ONLY when user clicks
# "Continue" — never on auto-save. This prevents saves from triggering restores.
# =============================================================================


# ── UI ────────────────────────────────────────────────────────────────────────

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
      
      actionButton(
        ns('save_btn'), 'Save',
        icon  = icon('floppy-disk'),
        class = 'btn btn-default btn-sm',
        style = 'padding:2px 10px;'
      ),
      
      tags$span(style = 'color:#d1d5db;', '|'),
      
      uiOutput(ns('session_label'), inline = TRUE)
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

sessionManagerServer <- function(id, username_r, district_r, district_ready_r) {
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(
      session_id       = NULL,
      started_at       = NULL,
      active           = FALSE,
      collect_state_fn = NULL,
      pending_saved    = NULL,   # saved session shown in modal
      restore_snap     = NULL,   # snapshot to restore (set only on "Continue")
      restore_counter  = 0L,    # incremented only on "Continue" — never on save
      latest_snap      = NULL    # most recently saved snapshot
    )
    
    # ── Storage helpers ───────────────────────────────────────────────────────
    
    .read_latest <- function(uname, dist) {
      tryCatch({
        meta <- db_list_sessions(pool, uname, dist)
        if (is.null(meta) || nrow(meta) == 0) return(NULL)
        sid   <- meta$session_id[1]
        snaps <- db_get_snapshots(pool, sid)
        if (length(snaps) == 0) return(NULL)
        list(
          session_id    = sid,
          started_at    = meta$started_at[1],
          saved_at      = meta$saved_at[1],
          username      = meta$username[1],
          district_name = meta$district_name[1],
          snap          = snaps[[length(snaps)]]
        )
      }, error = function(e) {
        cat('[session] read error:', e$message, '\n')
        NULL
      })
    }
    
    .write_snap <- function(snap_data) {
      tryCatch({
        db_create_session(pool, rv$session_id, username_r(), district_r())
        db_save_snapshot(pool, rv$session_id, snap_data$trigger %||% 'manual', snap_data)
        db_update_session_saved_at(pool, rv$session_id)
        db_prune_old_sessions(pool, username_r(), district_r())
      }, error = function(e) {
        cat('[session] write error:', e$message, '\n')
      })
    }
    
    .new_id <- function() {
      paste0(format(Sys.time(), '%Y%m%d%H%M%S'), '_', sample(1000L:9999L, 1L))
    }
    
    .start_new <- function() {
      rv$session_id     <- .new_id()
      rv$started_at     <- Sys.time()
      rv$active         <- TRUE
      rv$restore_snap   <- NULL
      rv$latest_snap    <- NULL
      rv$pending_saved  <- NULL
      # restore_counter NOT incremented — new session never triggers restore
    }
    
    # ── On district selection ─────────────────────────────────────────────────
    
    observeEvent(district_ready_r(), {
      req(isTRUE(district_ready_r()))
      req(nzchar(username_r() %||% ''), nzchar(district_r() %||% ''))
      
      saved <- .read_latest(username_r(), district_r())
      
      if (!is.null(saved)) {
        rv$pending_saved <- saved
        .show_load_modal(saved)
      } else {
        .start_new()
      }
    }, ignoreInit = TRUE)
    
    # ── Load modal ────────────────────────────────────────────────────────────
    
    .show_load_modal <- function(saved) {
      snap     <- saved$snap
      has_fac  <- !is.null(snap$odk_sf)      && nrow(snap$odk_sf)      > 0
      has_area <- !is.null(snap$saved_dfa_sf) && nrow(snap$saved_dfa_sf) > 0
      
      fac_txt  <- if (has_fac)  '\u2713 Facilities'  else '\u2013 Facilities'
      area_txt <- if (has_area) '\u2713 Health Areas' else '\u2013 Health Areas'
      fac_col  <- if (has_fac)  '#166534' else '#94a3b8'
      area_col <- if (has_area) '#166534' else '#94a3b8'
      
      saved_time <- tryCatch(
        format(saved$saved_at, '%d %b %Y, %H:%M'),
        error = function(e) 'unknown time'
      )
      
      showModal(modalDialog(
        title     = 'Resume previous session?',
        easyClose = FALSE,
        footer    = NULL,
        size      = 'm',
        
        div(
          style = paste0(
            'border:1px solid #e2e8f0;border-radius:8px;',
            'padding:14px 16px;margin-bottom:16px;background:#f8fafc;'
          ),
          div(
            style = 'font-size:13px;font-weight:600;color:#0f172a;margin-bottom:6px;',
            saved$district_name
          ),
          div(
            style = 'font-size:12px;margin-bottom:4px;',
            tags$span(style = paste0('color:', fac_col,  ';margin-right:12px;'), fac_txt),
            tags$span(style = paste0('color:', area_col, ';'), area_txt)
          ),
          div(
            style = 'font-size:11px;color:#94a3b8;',
            paste0('Last saved: ', saved_time)
          )
        ),
        
        div(
          style = 'display:flex;gap:10px;justify-content:flex-end;',
          actionButton(session$ns('new_session_btn'), 'Start new',
                       class = 'btn btn-default'),
          actionButton(session$ns('continue_btn'), 'Continue from last save',
                       class = 'btn btn-primary', style = 'font-weight:600;')
        )
      ))
    }
    
    observeEvent(input$continue_btn, {
      saved <- rv$pending_saved
      req(!is.null(saved))
      
      rv$session_id      <- saved$session_id
      rv$started_at      <- saved$started_at
      rv$active          <- TRUE
      rv$latest_snap     <- saved$snap
      rv$restore_snap    <- saved$snap
      rv$restore_counter <- rv$restore_counter + 1L  # triggers restore in tabs
      
      removeModal()
      showNotification('Session resumed.', type = 'message', duration = 2)
    }, ignoreInit = TRUE)
    
    observeEvent(input$new_session_btn, {
      .start_new()
      removeModal()
    }, ignoreInit = TRUE)
    
    # ── Toolbar ───────────────────────────────────────────────────────────────
    
    observe({
      req(isTRUE(rv$active))
      shinyjs::show('session_bar')
    })
    
    output$session_label <- renderUI({
      req(rv$active, rv$started_at)
      tags$span(
        style = 'color:#64748b;font-size:12px;',
        sprintf('%s  \u00b7  %s  \u00b7  %s',
                district_r() %||% '',
                username_r()  %||% '',
                format(rv$started_at, '%d %b %Y, %H:%M'))
      )
    })
    
    # ── save_snapshot ─────────────────────────────────────────────────────────
    
    save_snapshot <- function(snapshot_data, trigger = 'manual') {
      req(isTRUE(rv$active))
      req(nzchar(username_r() %||% ''), nzchar(district_r() %||% ''))
      
      snap <- c(list(snapshot_at = Sys.time(), trigger = trigger), snapshot_data)
      rv$latest_snap <- snap
      .write_snap(snap)
      invisible(NULL)
    }
    
    # ── Manual save button ────────────────────────────────────────────────────
    
    observeEvent(input$save_btn, {
      fn <- rv$collect_state_fn
      if (!is.null(fn)) {
        snap <- tryCatch(fn(), error = function(e) NULL)
        if (!is.null(snap)) {
          save_snapshot(snap, trigger = 'manual')
          showNotification('Session saved.', type = 'message', duration = 2)
        }
      } else {
        showNotification('Nothing to save yet.', type = 'warning', duration = 2)
      }
    }, ignoreInit = TRUE)
    
    # ── Public interface ──────────────────────────────────────────────────────
    
    list(
      active     = reactive(rv$active),
      started_at = reactive(rv$started_at),
      session_id = reactive(rv$session_id),
      
      # Fires ONLY when user clicks "Continue" — tab restore observers use this
      restore_snapshot = reactive({
        rv$restore_counter  # explicit dependency — only changes on "Continue"
        if (rv$restore_counter < 1L) return(NULL)
        rv$restore_snap
      }),
      
      # Latest saved snapshot — for reading current state (admin, microplan etc.)
      current_snapshot = reactive(rv$latest_snap),
      
      save_snapshot  = save_snapshot,
      set_collect_fn = function(fn) rv$collect_state_fn <- fn
    )
  })
}
