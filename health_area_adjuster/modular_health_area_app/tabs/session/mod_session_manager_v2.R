# =============================================================================
# mod_session_manager_v2.R
#
# Replaces mod_session_manager.R's Resume/Start-new modal with the
# ownership/publish/branch model:
#
#   - DB writes happen ONLY via submit_stage(), which wraps
#     db_submit_stage_v2(). If the version being edited is currently shared
#     (published), that call transparently forks a new version — this module
#     just needs to track whatever version_id comes back.
#   - On district+campaign selection:
#       1. User has their own active (non-archived) draft for this
#          district+campaign -> resume it silently, no modal.
#       2. No draft of their own, but a shared version exists for this
#          district+campaign -> auto-suggest modal, default action
#          "Branch from published version", with "Carry forward from a
#          different campaign" and "Start blank" as alternatives.
#       3. No draft, no shared version in this campaign, but a shared
#          version exists in some OTHER campaign for this district ->
#          modal offering "Carry forward from a different campaign" /
#          "Start blank" (no same-campaign branch option, since none
#          exists).
#       4. Nothing at all -> start blank silently, no modal.
#   - No practice/actual mode.
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
      uiOutput(ns('session_label'), inline = TRUE),
      uiOutput(ns('publish_control'), inline = TRUE)
    )
  )
}


# username_r        — reactive, logged-in user
# district_r        — reactive, selected district_name
# campaign_id_r      — reactive, selected campaign_id
# district_ready_r    — reactive logical, TRUE once district+campaign are both chosen
sessionManagerServer <- function(id, username_r, district_r, campaign_id_r, district_ready_r) {
  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues(
      active            = FALSE,
      version_id        = NULL,
      is_shared         = FALSE,
      pending_options    = NULL,  # list of candidate versions shown in the modal
      restore_snap        = NULL,
      restore_counter       = 0L
    )

    last_activated_key <- reactiveVal(NULL)

    # ── On district or campaign change: decide what to offer ──────────────────

    observeEvent(list(district_r(), campaign_id_r(), district_ready_r()), {
      req(isTRUE(district_ready_r()))
      req(nzchar(district_r() %||% ''))
      req(!is.null(campaign_id_r()))

      session_key <- paste0(district_r(), '|', campaign_id_r())
      if (isTRUE(rv$active) && identical(last_activated_key(), session_key)) return()

      dname <- district_r()
      cid   <- campaign_id_r()
      uname <- username_r() %||% ''

      own_draft <- tryCatch(
        db_get_active_draft(pool, uname, cid, dname),
        error = function(e) { cat('[session] active_draft check error:', e$message, '\n'); NULL }
      )

      if (!is.null(own_draft)) {
        .activate(own_draft$version_id, isTRUE(own_draft$is_shared), snap = NULL, session_key)
        return()
      }

      shared_here <- tryCatch(
        db_get_shared_version(pool, cid, dname),
        error = function(e) { cat('[session] shared_version check error:', e$message, '\n'); NULL }
      )

      other_shared <- tryCatch(
        db_get_shareable_versions(pool, dname, campaign_id = NULL),
        error = function(e) NULL
      )
      # exclude the current campaign from the "carry forward" pool — that
      # case is covered by shared_here / the direct branch option
      if (!is.null(other_shared) && nrow(other_shared) > 0) {
        other_shared <- other_shared[other_shared$campaign_id != cid, , drop = FALSE]
      }
      has_other_shared <- !is.null(other_shared) && nrow(other_shared) > 0

      if (!is.null(shared_here) || has_other_shared) {
        rv$pending_options <- list(
          shared_here      = shared_here,
          other_shared     = other_shared,
          district_name    = dname,
          campaign_id      = cid
        )
        .show_source_modal(shared_here, other_shared, dname)
      } else {
        # Nothing to branch from — start blank silently
        new_id <- tryCatch(
          db_create_blank_version(pool, uname, cid, dname),
          error = function(e) { cat('[session] create_blank_version error:', e$message, '\n'); NULL }
        )
        req(!is.null(new_id))
        .activate(new_id, FALSE, snap = NULL, session_key)
      }
    }, ignoreInit = TRUE)

    observeEvent(rv$active, {
      if (isTRUE(rv$active)) {
        last_activated_key(paste0(district_r() %||% '', '|', campaign_id_r() %||% ''))
      }
    }, ignoreInit = TRUE)

    .activate <- function(version_id, is_shared, snap, session_key) {
      rv$active        <- TRUE
      rv$version_id     <- version_id
      rv$is_shared        <- isTRUE(is_shared)
      rv$pending_options     <- NULL
      last_activated_key(session_key)
      if (!is.null(snap)) {
        rv$restore_snap    <- snap
        rv$restore_counter    <- rv$restore_counter + 1L
      }
    }

    # ── Source-selection modal (auto-suggested branch / carry-forward / blank) ─

    .show_source_modal <- function(shared_here, other_shared, dname) {
      has_here  <- !is.null(shared_here)
      has_other <- !is.null(other_shared) && nrow(other_shared) > 0

      here_block <- if (has_here) {
        div(
          style = 'border:1px solid #99f6e4;border-radius:8px;padding:12px 14px;margin-bottom:10px;background:#f0fdfa;',
          div(style = 'font-size:12px;font-weight:600;color:#0f172a;margin-bottom:4px;',
              'Published version available for this campaign'),
          div(style = 'font-size:11px;color:#64748b;margin-bottom:8px;',
              sprintf('Last published %s',
                      tryCatch(format(shared_here$shared_at, '%d %b %Y'), error = function(e) ''))),
          actionButton(session$ns('branch_here_btn'), 'Branch from published version',
                       class = 'btn btn-primary', style = 'font-weight:600;')
        )
      } else NULL

      other_block <- if (has_other) {
        choices <- setNames(
          other_shared$version_id,
          paste0(other_shared$district_name, ' — v', other_shared$version_number,
                 ' (campaign ', other_shared$campaign_id, ')')
        )
        div(
          style = 'border:1px solid #e2e8f0;border-radius:8px;padding:12px 14px;margin-bottom:10px;',
          div(style = 'font-size:12px;font-weight:600;color:#0f172a;margin-bottom:8px;',
              'Carry forward final boundaries from a different campaign'),
          selectInput(session$ns('carry_forward_source'), NULL, choices = choices, width = '100%'),
          actionButton(session$ns('carry_forward_btn'), 'Carry forward',
                       class = 'btn btn-default')
        )
      } else NULL

      showModal(modalDialog(
        title     = paste0('Start work on ', dname),
        easyClose = FALSE,
        footer    = NULL,
        size      = 'm',
        here_block,
        other_block,
        div(
          style = 'display:flex;justify-content:flex-end;margin-top:6px;',
          actionButton(session$ns('start_blank_btn'), 'Start blank', class = 'btn btn-default')
        )
      ))
    }

    observeEvent(input$branch_here_btn, {
      opts <- rv$pending_options
      req(!is.null(opts), !is.null(opts$shared_here))
      new_id <- tryCatch(
        db_branch_version(pool, opts$shared_here$version_id, username_r() %||% '',
                          opts$campaign_id, opts$district_name),
        error = function(e) { cat('[session] branch error:', e$message, '\n'); NULL }
      )
      req(!is.null(new_id))
      full <- db_get_version_by_id(pool, new_id)
      removeModal()
      .activate(new_id, FALSE, snap = full$snap, paste0(opts$district_name, '|', opts$campaign_id))
      showNotification('Branched from the published version.', type = 'message', duration = 2)
    }, ignoreInit = TRUE)

    observeEvent(input$carry_forward_btn, {
      opts <- rv$pending_options
      req(!is.null(opts), !is.null(input$carry_forward_source))
      new_id <- tryCatch(
        db_branch_version(pool, as.integer(input$carry_forward_source), username_r() %||% '',
                          opts$campaign_id, opts$district_name),
        error = function(e) { cat('[session] carry_forward error:', e$message, '\n'); NULL }
      )
      req(!is.null(new_id))
      full <- db_get_version_by_id(pool, new_id)
      removeModal()
      .activate(new_id, FALSE, snap = full$snap, paste0(opts$district_name, '|', opts$campaign_id))
      showNotification('Carried forward boundaries from the prior campaign.', type = 'message', duration = 2)
    }, ignoreInit = TRUE)

    observeEvent(input$start_blank_btn, {
      opts <- rv$pending_options
      req(!is.null(opts))
      new_id <- tryCatch(
        db_create_blank_version(pool, username_r() %||% '', opts$campaign_id, opts$district_name),
        error = function(e) { cat('[session] create_blank_version error:', e$message, '\n'); NULL }
      )
      req(!is.null(new_id))
      removeModal()
      .activate(new_id, FALSE, snap = NULL, paste0(opts$district_name, '|', opts$campaign_id))
    }, ignoreInit = TRUE)

    # ── Session bar ────────────────────────────────────────────────────────────

    observe({
      req(isTRUE(rv$active))
      shinyjs::show('session_bar')
    })

    output$session_label <- renderUI({
      req(rv$active)
      status_badge <- if (isTRUE(rv$is_shared)) {
        tags$span(
          style = paste0('background:#f0fdf4;color:#166534;border:1px solid #bbf7d0;',
                         'border-radius:20px;padding:1px 8px;font-size:10px;font-weight:700;margin-left:6px;'),
          'PUBLISHED'
        )
      } else {
        tags$span(
          style = paste0('background:#fef9c3;color:#854d0e;border:1px solid #fde68a;',
                         'border-radius:20px;padding:1px 8px;font-size:10px;font-weight:700;margin-left:6px;'),
          'DRAFT'
        )
      }
      tagList(
        tags$span(style = 'color:#64748b;font-size:12px;',
                  sprintf('%s  \u00b7  %s', district_r() %||% '', username_r() %||% '')),
        status_badge
      )
    })

    output$publish_control <- renderUI({
      req(rv$active)
      if (isTRUE(rv$is_shared)) return(NULL)
      actionButton(session$ns('publish_btn'), 'Publish',
                  class = 'btn btn-primary btn-sm', style = 'margin-left:10px;font-weight:600;')
    })

    observeEvent(input$publish_btn, {
      req(!is.null(rv$version_id))
      showModal(modalDialog(
        title = 'Publish this version?',
        paste0('This will become the shared version for ', district_r(),
              '. Anyone can branch from it. Continuing to edit afterward creates a new version automatically.'),
        easyClose = FALSE,
        footer = tagList(
          actionButton(session$ns('publish_cancel'), 'Cancel'),
          actionButton(session$ns('publish_confirm'), 'Publish', class = 'btn btn-primary')
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$publish_cancel, removeModal(), ignoreInit = TRUE)

    observeEvent(input$publish_confirm, {
      removeModal()
      tryCatch({
        db_publish_version(pool, rv$version_id)
        rv$is_shared <- TRUE
        showNotification('Published.', type = 'message', duration = 3)
      }, error = function(e) {
        showNotification(paste0('Publish failed: ', e$message), type = 'error', duration = 6)
      })
    }, ignoreInit = TRUE)

    # ── submit_stage: the ONLY function that writes to DB ─────────────────────
    # If the active version is currently shared, db_submit_stage_v2 forks a
    # new version and returns its id — update local state to follow it.

    submit_stage <- function(stage, data) {
      req(!is.null(rv$version_id))
      tryCatch({
        new_id <- db_submit_stage_v2(pool, rv$version_id, username_r() %||% '', stage, data)
        if (!identical(new_id, rv$version_id)) {
          rv$version_id <- new_id
          rv$is_shared  <- FALSE   # a fork is always unpublished
          showNotification('Your edit created a new version (the previous one stays published).',
                           type = 'message', duration = 4)
        }
        stage_label <- switch(stage,
                              landmarks     = 'Landmarks',
                              facilities    = 'Facilities',
                              idp           = 'IDP settlements',
                              areas         = 'Health areas',
                              team_areas    = 'Team areas',
                              stage
        )
        showNotification(paste0(stage_label, ' submitted successfully.'),
                         type = 'message', duration = 3)
      }, error = function(e) {
        showNotification(paste0('Submit failed: ', e$message), type = 'error', duration = 6)
      })
      invisible(NULL)
    }

    # ── Explicit "Refresh from source" — UI must warn before calling this ─────

    refresh_snapshot <- function(odk_sf = NULL, app_sf = NULL, district_boundary_sf = NULL) {
      req(!is.null(rv$version_id))
      tryCatch({
        db_refresh_snapshot(pool, rv$version_id, odk_sf = odk_sf, app_sf = app_sf,
                            district_boundary_sf = district_boundary_sf)
        showNotification('Facility/boundary data refreshed.', type = 'message', duration = 3)
      }, error = function(e) {
        showNotification(paste0('Refresh failed: ', e$message), type = 'error', duration = 6)
      })
      invisible(NULL)
    }

    # ── Public interface ────────────────────────────────────────────────────

    list(
      active      = reactive(rv$active),
      version_id  = reactive(rv$version_id),
      is_shared   = reactive(rv$is_shared),

      restore_snapshot = reactive({
        rv$restore_counter
        if (rv$restore_counter < 1L) return(NULL)
        rv$restore_snap
      }),

      submit_stage      = submit_stage,
      refresh_snapshot  = refresh_snapshot
    )
  })
}
