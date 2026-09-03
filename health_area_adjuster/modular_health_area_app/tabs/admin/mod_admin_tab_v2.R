# =============================================================================
# mod_admin_tab_v2.R  —  Admin panel (v2)
#
# Replaces mod_admin_tab.R entirely (not adapted in place — user-district
# scoping, single-lineage-per-district versioning, and practice/actual mode
# are all gone, so most of the old file's logic has no equivalent to adapt).
#
# New admin action semantics under the ownership/publish/branch model,
# worth stating explicitly since there's no v1 equivalent to point to:
#   - "Unshare"  — the version stops being the district+campaign's official
#     one. Nothing is deleted; the owner keeps their row and can keep
#     editing it (which, per the fork-on-edit rule, would create yet
#     another new version next time they save). Use this when a shared
#     version needs to be pulled without necessarily having a replacement
#     ready yet.
#   - "Archive"  — soft-deletes a specific version (any version, shared or
#     not). It disappears from every picker (continue/branch/carry-forward/
#     progress) but the row isn't hard-deleted, so it can still be found by
#     a direct version_id lookup if ever needed.
#   - There is no "reject" — that concept assumed one lineage per district
#     that could be sent back for rework. Under this model an owner's own
#     draft is never touched by anyone else; the admin levers are Unshare
#     and Archive, applied to whichever version needs it.
# =============================================================================

adminTabUI <- function(id) {
  ns <- NS(id)

  div(
    style = 'padding: 20px 28px; width: 100%;',

    div(
      style = 'display: flex; align-items: center; margin-bottom: 20px;',
      div(
        style = 'flex: 1;',
        tags$h3(style = 'font-size: 18px; font-weight: 700; color: #0f172a; margin: 0 0 3px;',
                'Admin Panel'),
        tags$p(style = 'font-size: 13px; color: #64748b; margin: 0;',
               'Manage users, campaigns, and review district progress.')
      )
    ),

    .admin_section('Users',
                   action  = actionButton(ns('add_user_btn'), 'Add user',
                                          icon = icon('plus'), class = 'btn btn-primary btn-sm'),
                   content = DT::DTOutput(ns('user_table'), width = '100%')
    ),

    .admin_section('Campaigns',
                   action  = actionButton(ns('add_campaign_btn'), 'New campaign',
                                          icon = icon('plus'), class = 'btn btn-primary btn-sm'),
                   content = DT::DTOutput(ns('campaign_table'), width = '100%')
    ),

    .admin_section('District progress',
                   action = div(
                     style = 'display:flex;gap:8px;align-items:center;',
                     selectInput(ns('progress_campaign'), NULL, choices = character(0), width = '220px'),
                     actionButton(ns('refresh_progress'), NULL,
                                  icon = icon('rotate'), class = 'btn btn-default btn-sm', title = 'Refresh'),
                     actionButton(ns('download_campaign_btn'), 'Download all',
                                  icon = icon('download'), class = 'btn btn-default btn-sm')
                   ),
                   content = DT::DTOutput(ns('progress_table'), width = '100%')
    ),

    .admin_section('Version review',
                   action = div(
                     style = 'display:flex;gap:8px;align-items:center;',
                     selectInput(ns('review_district'), NULL, choices = character(0), width = '220px')
                   ),
                   content = DT::DTOutput(ns('version_table'), width = '100%')
    ),

    .admin_section('Generation settings',
                   action = div(
                     style = 'display:flex;gap:8px;align-items:center;',
                     tags$span(style = 'font-size:11px;color:#64748b;', 'Edit values, then Save changes.'),
                     actionButton(ns('save_settings_btn'), 'Save changes', class = 'btn btn-primary btn-sm')
                   ),
                   content = rhandsontable::rHandsontableOutput(ns('settings_table'))
    ),

    .admin_section('Data source URLs',
                   content = div(
                     style = 'padding: 14px 16px;',
                     tags$p(style = 'font-size: 11px; color: #64748b; margin-bottom: 12px;',
                            'Health-facility (ODK/Kobo) endpoints are configured in .env, not here.'),
                     div(class = 'mini-label', 'Subdivisions source URL'),
                     textInput(ns('subdivisions_url'), NULL, width = '100%'),
                     div(class = 'mini-label', style = 'margin-top: 10px;', 'IDP settlements source URL'),
                     textInput(ns('idp_url'), NULL, width = '100%'),
                     actionButton(ns('save_sources_btn'), 'Save', class = 'btn btn-primary btn-sm', style = 'margin-top: 10px;')
                   )
    )
  )
}

.admin_section <- function(title, content, action = NULL) {
  div(
    style = 'margin-bottom: 28px;',
    div(
      style = 'display: flex; align-items: center; margin-bottom: 10px;',
      div(style = 'flex: 1;',
          tags$h4(style = 'font-size: 14px; font-weight: 700; color: #1e293b; margin: 0;', title)),
      if (!is.null(action)) action
    ),
    div(style = 'background:#fff;border:1px solid #e2e8f0;border-radius:8px;overflow:hidden;',
        content)
  )
}


# =============================================================================
# Server
# =============================================================================

adminTabServer <- function(id, districts_shp, username_r = reactive('admin')) {
  moduleServer(id, function(input, output, session) {

    current_user <- reactive(username_r() %||% 'admin')
    all_district_names <- sort(unique(as.character(stats::na.omit(districts_shp$district_name))))

    # =========================================================================
    # SECTION 1: Users — no district assignment (users are no longer
    # district-scoped; any authenticated user can work on any district)
    # =========================================================================

    users_rv <- reactiveVal(NULL)
    refresh_users <- function() users_rv(tryCatch(db_get_users(pool), error = function(e) NULL))
    refresh_users()

    output$user_table <- DT::renderDT({
      df <- users_rv()
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = 'No users found. Click "Add user" to create one.'),
                             rownames = FALSE, options = list(dom = 't'), selection = 'none'))
      }
      ns_str    <- session$ns('')
      btn_style <- 'font-size:11px;padding:2px 8px;cursor:pointer;border-radius:3px;'

      display <- data.frame(
        Username       = df$username,
        `Display name` = df$display_name,
        Role           = df$role,
        Edit           = seq_len(nrow(df)),
        Delete         = seq_len(nrow(df)),
        stringsAsFactors = FALSE, check.names = FALSE
      )

      DT::datatable(display, rownames = FALSE, selection = 'none',
                    options = list(dom = 'ft', pageLength = 200, autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = 'calc(100vh - 320px)', scrollCollapse = TRUE,
                                  columnDefs = list(
                                    list(targets = 3, render = DT::JS(sprintf(
                                      "function(data,type,row){ if(type!=='display') return data;
                                        return '<button style=\"%sborder:1px solid #1565C0;background:#fff;color:#1565C0;\"'+
                                          ' onclick=\"Shiny.setInputValue(\\'%sedit_user_row\\','+data+',{priority:\\'event\\'})\">'+'Edit</button>'; }",
                                      btn_style, ns_str))),
                                    list(targets = 4, render = DT::JS(sprintf(
                                      "function(data,type,row){ if(type!=='display') return data;
                                        return '<button style=\"%sborder:1px solid #ef4444;background:#fff;color:#ef4444;\"'+
                                          ' onclick=\"Shiny.setInputValue(\\'%sdelete_user_row\\','+data+',{priority:\\'event\\'})\">'+'Delete</button>'; }",
                                      btn_style, ns_str)))
                                  )),
                    escape = FALSE)
    })

    editing_user   <- reactiveVal(NULL)
    deleting_uname <- reactiveVal(NULL)

    .show_user_modal <- function(row_idx) {
      df <- users_rv()
      is_edit <- !is.null(row_idx)
      row <- if (is_edit) df[row_idx, ] else NULL
      showModal(modalDialog(
        title = if (is_edit) 'Edit user' else 'Add user', easyClose = FALSE,
        textInput(session$ns('user_username'), 'Username', value = row$username %||% ''),
        if (is_edit) tags$p(style = 'font-size:11px;color:#94a3b8;', 'Username cannot be changed.'),
        textInput(session$ns('user_display_name'), 'Display name', value = row$display_name %||% ''),
        passwordInput(session$ns('user_password'), if (is_edit) 'New password (leave blank to keep current)' else 'Password'),
        selectInput(session$ns('user_role'), 'Role', choices = c('user', 'admin'), selected = row$role %||% 'user'),
        footer = tagList(
          modalButton('Cancel'),
          actionButton(session$ns('save_user'), if (is_edit) 'Save' else 'Create', class = 'btn btn-primary')
        )
      ))
    }

    observeEvent(input$add_user_btn, { editing_user(NULL); .show_user_modal(NULL) }, ignoreInit = TRUE)
    observeEvent(input$edit_user_row, {
      editing_user(as.integer(input$edit_user_row)); .show_user_modal(as.integer(input$edit_user_row))
    }, ignoreInit = TRUE)

    observeEvent(input$delete_user_row, {
      idx <- as.integer(input$delete_user_row); df <- users_rv()
      req(!is.na(idx), idx >= 1L, idx <= nrow(df))
      uname <- df$username[idx]
      if (uname == 'admin') { showNotification('Cannot delete the admin account.', type = 'error', duration = 4); return() }
      deleting_uname(uname)
      showModal(modalDialog(
        title = 'Confirm delete', size = 's', easyClose = TRUE,
        footer = tagList(modalButton('Cancel'),
                         actionButton(session$ns('confirm_delete'), 'Delete', class = 'btn btn-danger')),
        tags$p(style = 'font-size:14px;', paste0('Delete user "', uname, '"? Cannot be undone.'))
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_delete, {
      db_delete_user(pool, deleting_uname()); deleting_uname(NULL); refresh_users(); removeModal()
    }, ignoreInit = TRUE)

    observeEvent(input$save_user, {
      uname <- trimws(input$user_username %||% '')
      dname <- trimws(input$user_display_name %||% '')
      if (!nzchar(uname) || !nzchar(dname)) {
        showNotification('Username and display name are required.', type = 'warning', duration = 4); return()
      }
      is_edit <- !is.null(editing_user())
      pwd <- input$user_password %||% ''
      if (!is_edit && !nzchar(pwd)) {
        showNotification('Password is required for a new user.', type = 'warning', duration = 4); return()
      }
      tryCatch({
        db_upsert_user(pool, uname, pwd, dname, input$user_role %||% 'user')
        removeModal(); refresh_users()
        showNotification('User saved.', type = 'message', duration = 3)
      }, error = function(e) showNotification(paste('Failed:', e$message), type = 'error', duration = 6))
    }, ignoreInit = TRUE)

    # =========================================================================
    # SECTION 2: Campaigns
    # =========================================================================

    campaigns_rv <- reactiveVal(NULL)
    refresh_campaigns <- function() {
      df <- tryCatch(db_get_campaigns(pool, active_only = FALSE), error = function(e) NULL)
      campaigns_rv(df)
      choices <- if (!is.null(df) && nrow(df) > 0) setNames(as.character(df$campaign_id), df$campaign_name) else character(0)
      updateSelectInput(session, 'progress_campaign', choices = choices)
    }
    refresh_campaigns()

    output$campaign_table <- DT::renderDT({
      df <- campaigns_rv()
      if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(Message = 'No campaigns yet.'),
                                                              rownames = FALSE, options = list(dom = 't')))
      ns_str <- session$ns('')
      df$Action <- vapply(seq_len(nrow(df)), function(i) {
        label <- if (isTRUE(df$is_active[i])) 'Deactivate' else 'Activate'
        sprintf('<button class="btn btn-default btn-xs" onclick="Shiny.setInputValue(\'%stoggle_campaign_row\', %d, {priority:\'event\'})">%s</button>',
                ns_str, df$campaign_id[i], label)
      }, character(1))
      display <- data.frame(
        Campaign = df$campaign_name, Description = df$description %||% '',
        `Created by` = df$created_by, Created = format(df$created_at, '%d %b %Y'),
        Active = ifelse(df$is_active, 'Yes', 'No'), Action = df$Action,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(display, escape = FALSE, rownames = FALSE, selection = 'none',
                    options = list(dom = 't', paging = FALSE))
    })

    observeEvent(input$add_campaign_btn, {
      showModal(modalDialog(
        title = 'New campaign', easyClose = FALSE,
        textInput(session$ns('new_campaign_name'), 'Campaign name'),
        textAreaInput(session$ns('new_campaign_desc'), 'Description (optional)', rows = 3),
        footer = tagList(modalButton('Cancel'),
                         actionButton(session$ns('confirm_add_campaign'), 'Create', class = 'btn btn-primary'))
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_add_campaign, {
      nm <- trimws(input$new_campaign_name %||% '')
      if (!nzchar(nm)) { showNotification('Campaign name is required.', type = 'warning', duration = 3); return() }
      res <- tryCatch(db_create_campaign(pool, nm, input$new_campaign_desc %||% '', current_user()),
                      error = function(e) { showNotification(paste('Failed:', e$message), type = 'error', duration = 6); NULL })
      if (!is.null(res)) { removeModal(); refresh_campaigns(); showNotification('Campaign created.', type = 'message', duration = 3) }
    }, ignoreInit = TRUE)

    observeEvent(input$toggle_campaign_row, {
      df <- campaigns_rv(); row <- df[df$campaign_id == input$toggle_campaign_row, , drop = FALSE]
      req(nrow(row) == 1)
      tryCatch({ db_set_campaign_active(pool, row$campaign_id[1], !isTRUE(row$is_active[1])); refresh_campaigns() },
              error = function(e) showNotification(paste('Failed:', e$message), type = 'error', duration = 6))
    }, ignoreInit = TRUE)

    # =========================================================================
    # SECTION 3: District progress (per campaign — shared versions only)
    # =========================================================================

    progress_rv <- reactiveVal(NULL)
    refresh_progress <- function() {
      cid <- input$progress_campaign %||% ''
      if (!nzchar(cid)) { progress_rv(NULL); return() }
      progress_rv(tryCatch(db_get_campaign_progress(pool, as.integer(cid)), error = function(e) NULL))
    }
    observeEvent(input$progress_campaign, refresh_progress(), ignoreInit = TRUE)
    observeEvent(input$refresh_progress,  refresh_progress(), ignoreInit = TRUE)

    output$progress_table <- DT::renderDT({
      df <- progress_rv()
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = 'No published districts for this campaign yet.'),
                             rownames = FALSE, options = list(dom = 't')))
      }
      .flag <- function(x) ifelse(isTRUE(x), '\u2713', '\u2013')
      ns_str <- session$ns('')
      print_action <- vapply(seq_len(nrow(df)), function(i) {
        sprintf('<button class="btn btn-default btn-xs" onclick="Shiny.setInputValue(\'%sprint_maps_row\', %d, {priority:\'event\'})">Print maps</button>',
                ns_str, df$version_id[i])
      }, character(1))
      display <- data.frame(
        District      = df$district_name,
        `Published by` = df$owner_username,
        Version        = df$version_number,
        `Published`     = format(df$shared_at, '%d %b %Y'),
        Landmarks         = vapply(df$has_landmarks, .flag, character(1)),
        Facilities          = vapply(df$has_facilities, .flag, character(1)),
        IDP                   = vapply(df$has_idp, .flag, character(1)),
        `Health Areas`          = vapply(df$has_health_areas, .flag, character(1)),
        `Team Areas`              = vapply(df$has_team_areas, .flag, character(1)),
        Print                       = print_action,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(display, escape = FALSE, rownames = FALSE, selection = 'none',
                    options = list(dom = 'ft', pageLength = 200, scrollX = TRUE))
    })

    print_maps_path <- reactiveVal(NULL)

    observeEvent(input$print_maps_row, {
      vid <- input$print_maps_row
      version <- tryCatch(db_get_version_by_id(pool, vid), error = function(e) NULL)
      req(!is.null(version))
      cname <- { cdf <- campaigns_rv(); row <- cdf[cdf$campaign_id == version$campaign_id, ]
                if (nrow(row) == 1) row$campaign_name[1] else '' }
      tmp <- tempfile(fileext = '.pdf')
      tryCatch({
        build_printable_maps_pdf(tmp, version, version$district_name, campaign_name = cname)
        print_maps_path(tmp)
        showModal(modalDialog(
          title = 'Printable maps ready', easyClose = TRUE, footer = modalButton('Close'),
          tags$p(sprintf('One overview page plus one page per health area for %s.', version$district_name)),
          downloadButton(session$ns('download_print_maps'), 'Download PDF')
        ))
      }, error = function(e) showNotification(paste('Failed to build printable maps:', e$message), type = 'error', duration = 6))
    }, ignoreInit = TRUE)

    output$download_print_maps <- downloadHandler(
      filename = function() paste0(gsub('[^A-Za-z0-9]', '_', input$print_maps_row %||% 'maps'), '_print.pdf'),
      content = function(file) {
        src <- print_maps_path()
        if (!is.null(src) && file.exists(src)) file.copy(src, file)
      },
      contentType = 'application/pdf'
    )

    campaign_download_path <- reactiveVal(NULL)

    observeEvent(input$download_campaign_btn, {
      cid <- input$progress_campaign %||% ''
      req(nzchar(cid))
      tmp <- tempfile(fileext = '.zip')
      tryCatch({
        build_campaign_download_v2(tmp, as.integer(cid))
        campaign_download_path(tmp)
        showModal(modalDialog(
          title = 'Download ready', easyClose = TRUE, footer = modalButton('Close'),
          tags$p('The campaign download has been prepared on the server.'),
          downloadButton(session$ns('download_campaign_file'), 'Download zip')
        ))
      }, error = function(e) showNotification(paste('Download failed:', e$message), type = 'error', duration = 6))
    }, ignoreInit = TRUE)

    output$download_campaign_file <- downloadHandler(
      filename = function() paste0('campaign_', input$progress_campaign %||% 'export', '_', Sys.Date(), '.zip'),
      content = function(file) {
        src <- campaign_download_path()
        if (!is.null(src) && file.exists(src)) file.copy(src, file)
        else build_campaign_download_v2(file, as.integer(input$progress_campaign))
      },
      contentType = 'application/zip'
    )

    # =========================================================================
    # SECTION 4: Version review (unshare / archive)
    # =========================================================================

    observe({
      updateSelectInput(session, 'review_district', choices = c(setNames('', 'Select district...'), all_district_names))
    })

    versions_rv <- reactiveVal(NULL)
    refresh_versions <- function() {
      dname <- input$review_district %||% ''
      if (!nzchar(dname)) { versions_rv(NULL); return() }
      versions_rv(tryCatch(db_get_version_history(pool, dname), error = function(e) NULL))
    }
    observeEvent(input$review_district, refresh_versions(), ignoreInit = TRUE)

    output$version_table <- DT::renderDT({
      df <- versions_rv()
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = 'Select a district to review its versions.'),
                             rownames = FALSE, options = list(dom = 't')))
      }
      ns_str <- session$ns('')
      df$Action <- vapply(seq_len(nrow(df)), function(i) {
        btns <- character(0)
        if (isTRUE(df$is_shared[i]))
          btns <- c(btns, sprintf(
            '<button class="btn btn-default btn-xs" onclick="Shiny.setInputValue(\'%sunshare_row\', %d, {priority:\'event\'})">Unshare</button>',
            ns_str, df$version_id[i]))
        btns <- c(btns, sprintf(
          '<button class="btn btn-default btn-xs" style="color:#ef4444;border-color:#ef4444;" onclick="Shiny.setInputValue(\'%sarchive_row\', %d, {priority:\'event\'})">Archive</button>',
          ns_str, df$version_id[i]))
        paste(btns, collapse = ' ')
      }, character(1))

      display <- data.frame(
        Owner    = df$owner_username,
        Version  = df$version_number,
        Shared   = ifelse(df$is_shared, 'Yes', 'No'),
        Created  = format(df$created_at, '%d %b %Y %H:%M'),
        Updated  = format(df$last_updated_at, '%d %b %Y %H:%M'),
        Action   = df$Action,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(display, escape = FALSE, rownames = FALSE, selection = 'none',
                    options = list(dom = 'ft', pageLength = 200, scrollX = TRUE))
    })

    observeEvent(input$unshare_row, {
      tryCatch({ db_unshare_version(pool, input$unshare_row); refresh_versions(); refresh_progress() },
              error = function(e) showNotification(paste('Failed:', e$message), type = 'error', duration = 6))
    }, ignoreInit = TRUE)

    observeEvent(input$archive_row, {
      showModal(modalDialog(
        title = 'Archive this version?', size = 's', easyClose = TRUE,
        tags$p(style = 'font-size:13px;',
               'This removes it from every picker (continue/branch/carry-forward/progress). It is not permanently deleted.'),
        footer = tagList(
          modalButton('Cancel'),
          actionButton(session$ns('confirm_archive'), 'Archive', class = 'btn btn-danger')
        )
      ))
      pending_archive_id(input$archive_row)
    }, ignoreInit = TRUE)

    pending_archive_id <- reactiveVal(NULL)
    observeEvent(input$confirm_archive, {
      vid <- pending_archive_id(); req(!is.null(vid))
      tryCatch({
        db_archive_version(pool, vid); pending_archive_id(NULL)
        removeModal(); refresh_versions(); refresh_progress()
      }, error = function(e) showNotification(paste('Failed:', e$message), type = 'error', duration = 6))
    }, ignoreInit = TRUE)

    # =========================================================================
    # SECTION 5: Generation settings (global values; per-campaign overrides
    # exist in the schema but aren't exposed in this UI yet)
    # =========================================================================

    settings_rv <- reactiveVal(NULL)
    refresh_settings <- function() {
      df <- tryCatch(db_get_all_generation_settings(pool), error = function(e) NULL)
      if (!is.null(df)) df <- df[is.na(df$campaign_id), , drop = FALSE]
      settings_rv(df)
    }
    refresh_settings()

    output$settings_table <- rhandsontable::renderRHandsontable({
      df <- settings_rv()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      display <- data.frame(Setting = df$setting_key, Value = as.numeric(df$setting_value),
                            Description = df$description, stringsAsFactors = FALSE, check.names = FALSE)
      rhandsontable::rhandsontable(display, rowHeaders = NULL, stretchH = 'all', height = 260) |>
        rhandsontable::hot_col('Setting', readOnly = TRUE) |>
        rhandsontable::hot_col('Description', readOnly = TRUE) |>
        rhandsontable::hot_col('Value', readOnly = FALSE)
    })

    observeEvent(input$save_settings_btn, {
      hot <- input$settings_table; req(!is.null(hot))
      edited <- rhandsontable::hot_to_r(hot); original <- settings_rv()
      req(!is.null(original), nrow(edited) == nrow(original))
      n_ok <- 0L
      for (i in seq_len(nrow(edited))) {
        new_val <- suppressWarnings(as.numeric(edited$Value[i]))
        if (is.na(new_val) || isTRUE(all.equal(new_val, as.numeric(original$setting_value[i])))) next
        tryCatch({
          db_set_generation_setting(pool, original$setting_key[i], new_val, current_user(), campaign_id = NULL)
          n_ok <- n_ok + 1L
        }, error = function(e) cat('[admin] failed to save', original$setting_key[i], ':', e$message, '\n'))
      }
      refresh_settings()
      showNotification(sprintf('%d setting(s) updated.', n_ok), type = 'message', duration = 3)
    }, ignoreInit = TRUE)

    # =========================================================================
    # SECTION 6: Data source URLs
    # =========================================================================

    observe({
      updateTextInput(session, 'subdivisions_url',
                      value = tryCatch(db_get_data_source_url(pool, 'subdivisions_url'), error = function(e) '') %||% '')
      updateTextInput(session, 'idp_url',
                      value = tryCatch(db_get_data_source_url(pool, 'idp_settlements_url'), error = function(e) '') %||% '')
    })

    observeEvent(input$save_sources_btn, {
      ok <- TRUE
      tryCatch(db_set_data_source_url(pool, 'subdivisions_url', trimws(input$subdivisions_url %||% ''), current_user()),
              error = function(e) { ok <<- FALSE; showNotification(paste('Failed (subdivisions):', e$message), type = 'error', duration = 6) })
      tryCatch(db_set_data_source_url(pool, 'idp_settlements_url', trimws(input$idp_url %||% ''), current_user()),
              error = function(e) { ok <<- FALSE; showNotification(paste('Failed (IDP):', e$message), type = 'error', duration = 6) })
      if (ok) showNotification('Data source URLs saved.', type = 'message', duration = 3)
    }, ignoreInit = TRUE)

    invisible(NULL)
  })
}
