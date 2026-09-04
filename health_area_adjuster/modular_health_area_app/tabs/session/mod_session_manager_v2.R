# =============================================================================
# mod_session_manager_v2.R
#
# Replaces the old auto-detect/modal session manager entirely. Under the
# intro-table-driven flow, there is no more silent auto-resume and no more
# automatically-shown branch/carry-forward/blank modal — the intro table's
# own drill-down pickers (mod_intro_tab_v2.R) are the ONLY way a version
# gets chosen, and whatever the user picks there (current, one of their own
# drafts, or "start blank" — which the picker itself creates via
# db_create_blank_version()/db_create_team_area_draft() before handing off)
# arrives here as an already-resolved version_id. This module's job shrinks
# to: given that id, activate it as the active session, and manage
# submit/fork-follow/make-current from there. No more internal modal, no
# more auto-detection branching, no more standalone "Publish" button —
# "make current" is called from the submit-flow prompt in the tab itself,
# not from a persistent toolbar control here.
#
# Health areas and team areas are independently versioned now (see
# mod_db_v2.R's SECTION 13 notes), so this file has two separate
# constructors rather than one generic one — teamAreaSessionServer adds
# staleness-awareness (is the pinned health-area version still current?)
# that has no health-area-side equivalent.
# =============================================================================


# =============================================================================
# Health areas
# =============================================================================

sessionToolbarUI <- function(id) {
  ns <- NS(id)
  shinyjs::hidden(
    div(
      id    = ns('session_bar'),
      style = paste0(
        'display:flex;align-items:center;gap:8px;',
        'padding:4px 10px;background:#f8fafc;',
        'border-bottom:1px solid #e2e8f0;font-size:12px;'
      ),
      uiOutput(ns('session_label'), inline = TRUE)
    )
  )
}

.session_badge <- function(text, bg, fg, border) {
  tags$span(
    style = paste0('background:', bg, ';color:', fg, ';border:1px solid ', border, ';',
                   'border-radius:20px;padding:1px 8px;font-size:10px;font-weight:700;margin-left:6px;'),
    text
  )
}

# username_r     — reactive, logged-in user
# district_r     — reactive, selected district_name
# campaign_id_r  — reactive, selected campaign_id
#
# Returns a list including activate_version_id(version_id) — called by the
# intro tab's picker (or by mod_health_area_tab.R after a fork) with an
# EXPLICIT id to make it the active session. Nothing happens on its own.
healthAreaSessionServer <- function(id, username_r, district_r, campaign_id_r) {
  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues(
      active           = FALSE,
      version_id       = NULL,
      is_shared        = FALSE,
      restore_snap     = NULL,
      restore_counter  = 0L
    )

    activate_version_id <- function(version_id) {
      full <- tryCatch(db_get_version_by_id(pool, version_id), error = function(e) NULL)
      if (is.null(full)) {
        showNotification('Could not load that health-area version.', type = 'error', duration = 5)
        return(invisible(FALSE))
      }
      rv$active           <- TRUE
      rv$version_id        <- version_id
      rv$is_shared          <- isTRUE(full$is_shared)
      rv$restore_snap          <- full$snap
      rv$restore_counter          <- rv$restore_counter + 1L
      shinyjs::show(session$ns('session_bar'))
      invisible(TRUE)
    }

    deactivate <- function() {
      rv$active <- FALSE; rv$version_id <- NULL; rv$is_shared <- FALSE
      shinyjs::hide(session$ns('session_bar'))
    }

    output$session_label <- renderUI({
      req(rv$active)
      badge <- if (isTRUE(rv$is_shared))
        .session_badge('CURRENT', '#f0fdf4', '#166534', '#bbf7d0')
      else
        .session_badge('DRAFT', '#fef9c3', '#854d0e', '#fde68a')
      tagList(
        tags$span(style = 'color:#64748b;font-size:12px;',
                  sprintf('%s  \u00b7  %s', district_r() %||% '', username_r() %||% '')),
        badge
      )
    })

    # The ONLY function that writes health-area progress to the DB. If the
    # active version is currently shared, db_submit_stage_v2 forks a new
    # version and returns its id — follow it. on_forked(new_id), if
    # supplied, lets the caller (mod_health_area_tab.R) react to the fork
    # (e.g. updating whatever UI shows "you're now on version N").
    submit_stage <- function(stage, data, on_forked = NULL) {
      req(!is.null(rv$version_id))
      tryCatch({
        new_id <- db_submit_stage_v2(pool, rv$version_id, username_r() %||% '', stage, data)
        if (!identical(new_id, rv$version_id)) {
          rv$version_id <- new_id
          rv$is_shared  <- FALSE   # a fork is always unpublished
          showNotification('Your edit created a new version (the previous one stays current).',
                           type = 'message', duration = 4)
          if (!is.null(on_forked)) on_forked(new_id)
        }
        stage_label <- switch(stage,
                              landmarks  = 'Landmarks',
                              facilities = 'Facilities',
                              idp        = 'IDP settlements',
                              areas      = 'Health areas',
                              stage
        )
        showNotification(paste0(stage_label, ' submitted successfully.'),
                         type = 'message', duration = 3)
      }, error = function(e) {
        showNotification(paste0('Submit failed: ', e$message), type = 'error', duration = 6)
      })
      invisible(NULL)
    }

    # "Make current" — called from the submit-flow prompt in
    # mod_health_area_tab.R, not a persistent toolbar button. actor_role
    # determines whether the district-wide team-area lock applies (see
    # db_publish_version() / db_district_has_locked_team_areas() in
    # mod_db_v2.R) — the caller is responsible for passing the actual
    # logged-in user's role, never hardcoding 'admin'.
    make_current <- function(actor_role = 'user') {
      req(!is.null(rv$version_id))
      tryCatch({
        db_publish_version(pool, rv$version_id, actor_role = actor_role)
        rv$is_shared <- TRUE
        showNotification("Set as this district's current health area map.",
                         type = 'message', duration = 3)
        invisible(TRUE)
      }, error = function(e) {
        showNotification(paste0('Could not set as current: ', e$message), type = 'error', duration = 6)
        invisible(FALSE)
      })
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

    list(
      active      = reactive(rv$active),
      version_id  = reactive(rv$version_id),
      is_shared   = reactive(rv$is_shared),

      restore_snapshot = reactive({
        rv$restore_counter
        if (rv$restore_counter < 1L) return(NULL)
        rv$restore_snap
      }),

      activate_version_id = activate_version_id,
      deactivate           = deactivate,
      submit_stage         = submit_stage,
      make_current         = make_current,
      refresh_snapshot     = refresh_snapshot,

      # For the submit-flow prompt to decide, BEFORE offering "make
      # current", whether to show it plainly or show the locked
      # explanation instead. Cheap, standalone check (own pool checkout) —
      # safe to call from a renderUI without an open transaction.
      is_locked_for_publish = reactive({
        req(!is.null(district_r()), !is.null(campaign_id_r()))
        isTRUE(db_district_has_locked_team_areas(pool, campaign_id_r(), district_r()))
      })
    )
  })
}


# =============================================================================
# Team areas
#
# Same shape as healthAreaSessionServer, with one addition: staleness. A
# team-area version's pinned health-area boundary can stop being current
# while the user is away from it — is_stale is checked once at activation
# (draft-open time, per the "applies uniformly to published and
# unpublished drafts" rule) and exposed so the calling tab can explain
# the situation before the user does any more work on top of an outdated
# boundary. There is no reconciliation — the pinned health-area version
# has to be made current again to un-stale a version. make_current()
# ALSO gets this check authoritatively, server-side, right before
# publishing — is_stale here is for UI display, not the actual guard.
# =============================================================================

# health_area_name_r — reactive, which health area this session's teams are for
teamAreaSessionServer <- function(id, username_r, district_r, campaign_id_r, health_area_name_r) {
  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues(
      active           = FALSE,
      team_version_id  = NULL,
      is_shared        = FALSE,
      is_stale         = FALSE,
      restore_snap     = NULL,
      restore_counter  = 0L
    )

    activate_team_version_id <- function(team_version_id) {
      full <- tryCatch(db_get_team_area_version_by_id(pool, team_version_id), error = function(e) NULL)
      if (is.null(full)) {
        showNotification('Could not load that team-area version.', type = 'error', duration = 5)
        return(invisible(FALSE))
      }
      stale <- isTRUE(tryCatch(db_check_team_area_staleness(pool, team_version_id), error = function(e) NA))
      rv$active           <- TRUE
      rv$team_version_id    <- team_version_id
      rv$is_shared            <- isTRUE(full$is_shared)
      rv$is_stale               <- stale
      rv$restore_snap             <- full$snap
      rv$restore_counter             <- rv$restore_counter + 1L
      shinyjs::show(session$ns('session_bar'))
      invisible(TRUE)
    }

    deactivate <- function() {
      rv$active <- FALSE; rv$team_version_id <- NULL
      rv$is_shared <- FALSE; rv$is_stale <- FALSE
      shinyjs::hide(session$ns('session_bar'))
    }

    output$session_label <- renderUI({
      req(rv$active)
      badge <- if (isTRUE(rv$is_stale))
        .session_badge('OUTDATED HEALTH AREA', '#fff7ed', '#9a3412', '#fed7aa')
      else if (isTRUE(rv$is_shared))
        .session_badge('CURRENT', '#f0fdf4', '#166534', '#bbf7d0')
      else
        .session_badge('DRAFT', '#fef9c3', '#854d0e', '#fde68a')
      tagList(
        tags$span(style = 'color:#64748b;font-size:12px;',
                  sprintf('%s  \u00b7  %s  \u00b7  %s',
                         district_r() %||% '', health_area_name_r() %||% '', username_r() %||% '')),
        badge
      )
    })

    # The ONLY function that writes team-area progress to the DB. Same
    # fork-on-edit-of-shared behavior as the health-area side.
    submit_stage <- function(data, on_forked = NULL) {
      req(!is.null(rv$team_version_id))
      tryCatch({
        new_id <- db_submit_team_area_stage(pool, rv$team_version_id, username_r() %||% '', data)
        if (!identical(new_id, rv$team_version_id)) {
          rv$team_version_id <- new_id
          rv$is_shared        <- FALSE
          showNotification('Your edit created a new version (the previous one stays current).',
                           type = 'message', duration = 4)
          if (!is.null(on_forked)) on_forked(new_id)
        }
        showNotification('Team areas submitted successfully.', type = 'message', duration = 3)
      }, error = function(e) {
        showNotification(paste0('Submit failed: ', e$message), type = 'error', duration = 6)
      })
      invisible(NULL)
    }

    # "Make current" — called from the submit-flow prompt in
    # mod_team_area_tab.R. No role-gating (team-area publish never
    # invalidates anything else downstream, unlike health-area publish) —
    # but db_publish_team_area() itself still refuses, for ANY caller, if
    # the pinned health-area version is no longer current. That refusal
    # (surfaced here as an error notification) is exactly the signal the
    # calling tab shows to the user as-is — there's no fix to offer
    # beyond making the pinned health-area version current again.
    make_current <- function() {
      req(!is.null(rv$team_version_id))
      tryCatch({
        db_publish_team_area(pool, rv$team_version_id)
        rv$is_shared <- TRUE
        showNotification("Set as this health area's current team map.",
                         type = 'message', duration = 3)
        invisible(TRUE)
      }, error = function(e) {
        showNotification(paste0('Could not set as current: ', e$message), type = 'error', duration = 6)
        invisible(FALSE)
      })
    }

    list(
      active           = reactive(rv$active),
      team_version_id  = reactive(rv$team_version_id),
      is_shared        = reactive(rv$is_shared),
      is_stale         = reactive(rv$is_stale),

      restore_snapshot = reactive({
        rv$restore_counter
        if (rv$restore_counter < 1L) return(NULL)
        rv$restore_snap
      }),

      activate_team_version_id = activate_team_version_id,
      deactivate                = deactivate,
      submit_stage               = submit_stage,
      make_current                = make_current
    )
  })
}
