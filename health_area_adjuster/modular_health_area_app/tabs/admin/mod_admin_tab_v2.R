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
                                  icon = icon('rotate'), class = 'btn btn-default btn-sm', title = 'Refresh')
                   ),
                   content = tagList(
                     tags$p(style = 'font-size:11px;color:#94a3b8;padding:8px 12px 0;margin:0;',
                            'Click a row to review and manage that district\'s health-area and team-area versions.'),
                     DT::DTOutput(ns('progress_table'), width = '100%')
                   )
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
                     div(class = 'mini-label', style = 'margin-top: 10px;', 'Urban areas source URL',
                         title = 'Used for urban-only mapping scope specifically -- separate from the subdivisions URL above, even though it starts out pointing at the same layer. Replace with the real urban-areas ArcGIS layer once available.'),
                     textInput(ns('urban_areas_url'), NULL, width = '100%'),
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
        toggle_btn <- sprintf('<button class="btn btn-default btn-xs" onclick="Shiny.setInputValue(\'%stoggle_campaign_row\', %d, {priority:\'event\'})">%s</button>',
                ns_str, df$campaign_id[i], label)
        manage_btn <- sprintf('<button class="btn btn-default btn-xs" onclick="Shiny.setInputValue(\'%smanage_districts_row\', %d, {priority:\'event\'})">Manage districts</button>',
                ns_str, df$campaign_id[i])
        paste(toggle_btn, manage_btn)
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

    # ── Manage districts (assignment + bundled carry-forward) ────────────────
    manage_districts_campaign_id <- reactiveVal(NULL)

    observeEvent(input$manage_districts_row, {
      manage_districts_campaign_id(as.integer(input$manage_districts_row))
      .show_manage_districts_modal(as.integer(input$manage_districts_row))
    }, ignoreInit = TRUE)

    .show_manage_districts_modal <- function(cid) {
      assigned <- tryCatch(db_get_campaign_districts(pool, cid), error = function(e) NULL)
      assigned_names  <- if (!is.null(assigned)) assigned$district_name else character(0)
      assigned_scopes <- if (!is.null(assigned)) setNames(assigned$mapping_scope, assigned$district_name) else character(0)

      by_region <- districts_shp |> sf::st_drop_geometry() |>
        dplyr::distinct(district_name, region_name) |>
        dplyr::arrange(region_name, district_name)

      # Individual inputs per district (not one checkboxGroupInput per
      # region, as this used to be) -- needed so each district can carry
      # its own inline scope choice next to its checkbox. Subdivision
      # availability is deliberately NOT checked here for every district
      # up front (that would mean an ArcGIS call per district just to
      # open this modal, for potentially 70+ districts at once) -- both
      # options are always shown, and "Urban areas only" is validated
      # lazily, only for districts actually requesting it, when Save is
      # clicked (see .validate_urban_scope() in the save handler below).
      region_blocks <- lapply(sort(unique(by_region$region_name)), function(rn) {
        dists <- by_region$district_name[by_region$region_name == rn]
        rows <- lapply(dists, function(dn) {
          dist_id  <- paste0('manage_dist_', gsub('[^A-Za-z0-9]+', '_', dn))
          scope_id <- paste0('manage_scope_', gsub('[^A-Za-z0-9]+', '_', dn))
          div(
            style = 'display:flex;align-items:center;gap:10px;padding:3px 0;',
            div(style = 'flex:0 0 210px;',
                checkboxInput(session$ns(dist_id), dn, value = dn %in% assigned_names, width = '100%')),
            div(style = 'flex:1;',
                radioButtons(session$ns(scope_id), NULL, inline = TRUE,
                            choices = c('Full district' = 'full', 'Urban areas only' = 'urban_only'),
                            # [[ throws "subscript out of bounds" for a name
                            # not present -- most districts aren't yet
                            # assigned to this campaign, so aren't in
                            # assigned_scopes at all. [ returns NA instead of
                            # erroring, but %||% alone wouldn't catch that --
                            # it only guards NULL/length-0, and a single-
                            # bracket miss is a named, length-1 NA, not
                            # either of those -- hence the explicit is.na()
                            # check rather than relying on %||% here.
                            selected = { s <- unname(assigned_scopes[dn]); if (is.na(s)) 'full' else s },
                            width = '100%'))
          )
        })
        tagList(
          tags$div(style = 'font-size:11px;font-weight:700;color:#64748b;margin:10px 0 4px;', rn),
          rows
        )
      })

      showModal(modalDialog(
        title = 'Manage districts for this campaign', size = 'l', easyClose = TRUE,
        footer = tagList(
          modalButton('Cancel'),
          actionButton(session$ns('save_manage_districts'), 'Save', class = 'btn btn-primary')
        ),
        tags$p(style = 'font-size:11px;color:#94a3b8;margin-bottom:8px;',
               '"Urban areas only" restricts landmarks, facilities, and health/team area mapping to a ',
               'buffered outline of that district\'s subdivisions -- only takes effect where subdivision ',
               'data is actually available; otherwise it\'s kept as Full district automatically.'),
        div(style = 'max-height:60vh;overflow-y:auto;', region_blocks)
      ))
    }

    # If requesting 'urban_only', fetches subdivisions live for this one
    # district and falls back to 'full' (with a warning) if none are
    # returned -- the actual enforcement of "no-subdivision districts
    # can't be set to urban only", applied per-district only when
    # actually requested, not to every district up front.
    .validate_urban_scope <- function(district_name, requested_scope) {
      if (!identical(requested_scope, 'urban_only')) return(requested_scope)
      dinfo <- districts_shp |> dplyr::filter(district_name == !!district_name)
      if (nrow(dinfo) == 0) return('full')
      dsf <- dinfo |>
        dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
        sf::st_as_sf() |> safe_make_valid() |> sf::st_transform(4326)
      cat(sprintf('[urban_scope_debug] %s: resolved urban_areas_url = %s\n', district_name,
                 tryCatch(db_get_data_source_url(pool, 'urban_areas_url'), error = function(e) sprintf('<error: %s>', e$message)) %||% '<NULL -- falls back to ARCGIS_SUBDIVISIONS_URL>'))
      subs <- tryCatch(fetch_urban_areas_for_district(dsf),
                       error = function(e) { cat(sprintf('[urban_scope_debug] %s: fetch_urban_areas_for_district ERRORED: %s\n', district_name, e$message)); NULL })
      cat(sprintf('[urban_scope_debug] %s: subs is.null=%s, nrow=%s\n', district_name,
                 is.null(subs), if (is.null(subs)) 'NA' else nrow(subs)))
      if (is.null(subs) || nrow(subs) == 0) {
        showNotification(
          sprintf('%s has no urban-area data available -- kept as Full district.', district_name),
          type = 'warning', duration = 6
        )
        return('full')
      }
      'urban_only'
    }

    observeEvent(input$save_manage_districts, {
      cid <- manage_districts_campaign_id(); req(!is.null(cid))
      all_dists <- districts_shp |> sf::st_drop_geometry() |>
        dplyr::distinct(district_name) |> dplyr::pull(district_name)

      checked <- character(0)
      requested_scopes <- list()
      for (dn in all_dists) {
        dist_id  <- paste0('manage_dist_', gsub('[^A-Za-z0-9]+', '_', dn))
        scope_id <- paste0('manage_scope_', gsub('[^A-Za-z0-9]+', '_', dn))
        if (isTRUE(input[[dist_id]])) {
          checked <- c(checked, dn)
          requested_scopes[[dn]] <- input[[scope_id]] %||% 'full'
        }
      }

      assigned <- tryCatch(db_get_campaign_districts(pool, cid), error = function(e) NULL)
      assigned_names  <- if (!is.null(assigned)) assigned$district_name else character(0)
      assigned_scopes <- if (!is.null(assigned)) setNames(assigned$mapping_scope, assigned$district_name) else character(0)

      newly_added   <- setdiff(checked, assigned_names)
      newly_removed <- setdiff(assigned_names, checked)
      still_present <- intersect(checked, assigned_names)

      for (dn in newly_removed) db_remove_district_from_campaign(pool, cid, dn)

      for (dn in newly_added) {
        final_scope <- .validate_urban_scope(dn, requested_scopes[[dn]] %||% 'full')
        db_assign_district_to_campaign(pool, cid, dn, current_user(), mapping_scope = final_scope)
      }

      # Already-assigned districts only get touched if their requested
      # scope actually changed -- db_assign_district_to_campaign()'s own
      # ON CONFLICT DO NOTHING wouldn't apply a scope change on its own,
      # which is why this uses db_set_district_mapping_scope() instead.
      for (dn in still_present) {
        requested <- requested_scopes[[dn]] %||% 'full'
        # Same [[ vs [ distinction as the modal-building code above --
        # assigned_scopes is an atomic named vector (via setNames()), where
        # [[ on a missing name errors; requested_scopes is a plain list
        # (via requested_scopes[[dn]] <- ... above), where [[ on a missing
        # name safely returns NULL instead, so that one line's %||% was
        # already correct. dn is only actually guaranteed present here
        # (still_present = intersect(checked, assigned_names)), but this
        # stays defensive for consistency rather than relying on that.
        current_val <- unname(assigned_scopes[dn])
        if (!identical(requested, if (is.na(current_val)) 'full' else current_val)) {
          final_scope <- .validate_urban_scope(dn, requested)
          db_set_district_mapping_scope(pool, cid, dn, final_scope)
        }
      }

      removeModal()
      refresh_campaigns()

      if (length(newly_added) > 0) .show_carry_forward_modal(cid, newly_added)
    }, ignoreInit = TRUE)

    # For each newly-added district, offer bundled carry-forward (health +
    # team areas together, marked current) from the most recent OTHER
    # campaign that district was published in. One checkbox per district
    # with a prior version available; districts with nothing to carry
    # forward from are shown but not offered a checkbox -- they simply
    # start blank in this campaign, same as any district with no prior
    # published work anywhere.
    .show_carry_forward_modal <- function(cid, district_names) {
      candidates <- lapply(district_names, function(dn) {
        rows <- tryCatch(db_get_shareable_versions(pool, dn, campaign_id = NULL), error = function(e) NULL)
        has_source <- !is.null(rows) && nrow(rows[rows$campaign_id != cid, , drop = FALSE]) > 0
        list(district_name = dn, has_source = has_source)
      })

      rows_ui <- lapply(candidates, function(c) {
        div(
          style = 'display:flex;align-items:center;gap:10px;padding:6px 4px;border-bottom:1px solid #f1f5f9;',
          if (c$has_source) {
            checkboxInput(session$ns(paste0('carry_fwd_', gsub('[^A-Za-z0-9]+', '_', c$district_name))),
                         c$district_name, value = TRUE, width = '100%')
          } else {
            tagList(
              tags$span(style = 'font-size:13px;color:#94a3b8;', c$district_name),
              tags$span(style = 'font-size:11px;color:#94a3b8;margin-left:8px;', '(no prior version to carry forward — starts blank)')
            )
          }
        )
      })

      showModal(modalDialog(
        title = 'Carry forward prior work?', size = 'm', easyClose = TRUE,
        footer = tagList(
          modalButton('Skip — start all blank'),
          actionButton(session$ns('confirm_carry_forward'), 'Carry forward checked districts', class = 'btn btn-primary')
        ),
        tags$p(style = 'font-size:12px;color:#64748b;margin-bottom:10px;',
              'For each district below, carrying forward brings in its most recently published health-area ',
              'map and any current team-area maps from another campaign, and marks them current here too.'),
        div(rows_ui)
      ))
      pending_carry_forward_cid(cid)
      pending_carry_forward_districts(district_names)
    }

    pending_carry_forward_cid       <- reactiveVal(NULL)
    pending_carry_forward_districts <- reactiveVal(NULL)

    observeEvent(input$confirm_carry_forward, {
      cid   <- pending_carry_forward_cid(); req(!is.null(cid))
      dists <- pending_carry_forward_districts() %||% character(0)
      removeModal()
      n_ok <- 0L
      for (dn in dists) {
        checked <- isTRUE(input[[paste0('carry_fwd_', gsub('[^A-Za-z0-9]+', '_', dn))]])
        if (!checked) next
        res <- tryCatch(
          db_carry_forward_district_to_campaign(pool, cid, dn, current_user()),
          error = function(e) { cat('[admin] carry_forward error for', dn, ':', e$message, '\n'); NULL }
        )
        if (!is.null(res)) n_ok <- n_ok + 1L
      }
      showNotification(sprintf('Carried forward %d district(s).', n_ok), type = 'message', duration = 3)
      pending_carry_forward_cid(NULL); pending_carry_forward_districts(NULL)
      refresh_progress()
    }, ignoreInit = TRUE)

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
      n_health_areas <- vapply(df$dfa_names, function(x) {
        parsed <- tryCatch(.from_json_vec_db(x), error = function(e) NULL)
        length(setdiff(parsed, c('Inaccessible', 'Unpopulated')))
      }, integer(1))
      display <- data.frame(
        District      = df$district_name,
        `Published by` = df$owner_username,
        Version        = df$version_number,
        `Published`     = format(df$shared_at, '%d %b %Y'),
        Landmarks         = vapply(df$has_landmarks, .flag, character(1)),
        Facilities          = vapply(df$has_facilities, .flag, character(1)),
        IDP                   = vapply(df$has_idp, .flag, character(1)),
        `Health Areas`          = ifelse(is.na(n_health_areas), '\u2013', as.character(n_health_areas)),
        `Team Areas`              = ifelse(is.na(n_health_areas), '\u2013',
                                          sprintf('%d of %d', df$team_areas_mapped_count, n_health_areas)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(display, escape = FALSE, rownames = FALSE, selection = 'single',
                    options = list(dom = 'ft', pageLength = 200, scrollX = TRUE))
    })

    # Row click on District progress opens the review/manage modal for
    # that specific (campaign, district) -- replaces the old standalone
    # District review section's own dropdown-driven selection entirely.
    observeEvent(input$progress_table_rows_selected, {
      row_idx <- input$progress_table_rows_selected
      req(length(row_idx) == 1)
      df <- progress_rv(); req(!is.null(df), row_idx <= nrow(df))
      .open_district_review_modal(df$campaign_id[row_idx], df$district_name[row_idx])
    }, ignoreNULL = TRUE)

    # restoring an old version is just making it current, same as any
    # other publish. For team areas, db_publish_team_area() itself refuses
    # if the target's pinned health-area version is no longer current
    # (staleness never gets bypassed just because an admin clicked it) —
    # there is no automatic fix for that; the pinned health-area version
    # has to be made current again first (below, in this same section),
    # same as any user would have to.
    # =========================================================================

    # review_target replaces the old input$review_district/input$review_campaign
    # dropdowns entirely -- set once when a District progress row is clicked
    # (see .open_district_review_modal below), not by standalone UI controls.
    review_target  <- reactiveVal(list(campaign_id = NULL, district_name = NULL))
    ha_versions_rv <- reactiveVal(NULL)
    ta_versions_rv <- reactiveVal(NULL)

    # Same unwrapping team_targets needs after a DB round-trip as
    # mod_health_area_tab.R's own .unwrap_num()/.unwrap_team_targets() --
    # .from_json_db()'s simplifyVector = FALSE means every scalar leaf
    # comes back wrapped in its own length-1 list. Duplicated here rather
    # than shared since the original is defined locally inside that
    # module's own moduleServer, not exported.
    .unwrap_num <- function(x) {
      if (is.null(x)) return(NA_real_)
      if (is.list(x)) x <- if (length(x) == 0) NA else x[[1]]
      if (is.null(x) || length(x) == 0) return(NA_real_)
      suppressWarnings(as.numeric(x))
    }
    .unwrap_team_targets <- function(tt) {
      if (is.null(tt)) return(list())
      lapply(tt, function(entry) list(
        target_pop      = .unwrap_num(entry$target_pop),
        requested_teams = .unwrap_num(entry$requested_teams)
      ))
    }

    # Picks smoothed_dfa_sf when it actually has rows, falling back to
    # saved_dfa_sf otherwise -- NOT %||%, since that checks length() (an
    # sf object's column count, always >= 1 for a non-NULL sf object even
    # with zero rows), not row count, so it wouldn't correctly fall back
    # on an empty-but-non-NULL smoothed_dfa_sf. Both fields live under
    # ver$snap (per .parse_mv_row() in mod_db_v2.R), not on ver directly
    # -- ver$saved_dfa_sf alone is always NULL, a mistake this function
    # actually shipped with once already (caught and fixed here).
    .current_boundary_sf <- function(ver) {
      if (!is.null(ver$snap$smoothed_dfa_sf) && nrow(ver$snap$smoothed_dfa_sf) > 0) ver$snap$smoothed_dfa_sf
      else ver$snap$saved_dfa_sf
    }

    refresh_review <- function() {
      rt <- review_target(); dname <- rt$district_name; cid <- rt$campaign_id
      if (is.null(dname) || !nzchar(dname)) { ha_versions_rv(NULL); ta_versions_rv(NULL); return() }
      ha_versions_rv(tryCatch(db_get_version_history(pool, dname, campaign_id = cid), error = function(e) NULL))
      ta_versions_rv(tryCatch(db_get_team_area_version_history(pool, dname, campaign_id = cid, health_area_name = NULL),
                              error = function(e) NULL))
    }

    # The CURRENT health-area version for whichever district/campaign is
    # under review -- falls back to the most recent version if none is
    # shared yet (e.g. a district still in draft), so the boundary
    # preview and population table below still show something rather
    # than going blank.
    .current_ha_version <- reactive({
      df <- ha_versions_rv(); req(!is.null(df), nrow(df) > 0)
      shared_row <- df[isTRUE(df$is_shared), , drop = FALSE]
      pick <- if (nrow(shared_row) > 0) shared_row[1, ] else df[1, ]
      tryCatch(db_get_version_by_id(pool, pick$version_id), error = function(e) NULL)
    })

    # Called by the District progress row-click observer above. size='xl'
    # since this now holds a map plus three tables where the old
    # standalone section had room to spread across the full page width.
    .open_district_review_modal <- function(campaign_id, district_name) {
      review_target(list(campaign_id = as.integer(campaign_id), district_name = district_name))
      refresh_review()
      showModal(modalDialog(
        title = sprintf('%s — District Review', district_name), size = 'xl', easyClose = TRUE,
        footer = modalButton('Close'),
        div(
          div(class = 'mini-label', style = 'margin: 4px 0 4px;', 'Boundary preview (current health-area version)'),
          leaflet::leafletOutput(ns('review_map'), height = '320px'),
          div(class = 'mini-label', style = 'margin: 16px 0 4px;', 'Population & team targets (current health-area version)'),
          DT::DTOutput(ns('boundary_review_table'), width = '100%'),
          div(class = 'mini-label', style = 'margin: 16px 0 4px;', 'Health area versions'),
          DT::DTOutput(ns('ha_version_table'), width = '100%'),
          div(class = 'mini-label', style = 'margin: 16px 0 4px;', 'Team area versions, by health area'),
          DT::DTOutput(ns('ta_version_table'), width = '100%')
        )
      ))
    }

    # ── Boundary preview map ─────────────────────────────────────────────
    output$review_map <- leaflet::renderLeaflet({
      ver <- .current_ha_version()
      boundary_sf <- .current_boundary_sf(ver)
      req(!is.null(boundary_sf), nrow(boundary_sf) > 0)
      area_col <- if ('dfa_name' %in% names(boundary_sf)) 'dfa_name' else names(boundary_sf)[1]
      areas_u <- unique(as.character(boundary_sf[[area_col]]))
      pal <- leaflet::colorFactor(grDevices::rainbow(max(length(areas_u), 1), s = 0.6, v = 0.9), domain = areas_u)
      leaflet::leaflet(boundary_sf) |>
        leaflet::addProviderTiles('CartoDB.Positron') |>
        leaflet::addPolygons(fillColor = ~pal(get(area_col)), fillOpacity = 0.6, weight = 1, color = '#000',
                            label = ~get(area_col))
    })

    # ── Population & team targets table -- both WorldPop-derived and
    # field-set values, per health area ─────────────────────────────────
    output$boundary_review_table <- DT::renderDT({
      ver <- .current_ha_version()
      req(!is.null(ver))
      boundary_sf <- .current_boundary_sf(ver)
      if (is.null(boundary_sf) || nrow(boundary_sf) == 0)
        return(DT::datatable(data.frame(Message = 'No boundaries saved for this version yet.'),
                             rownames = FALSE, options = list(dom = 't')))

      area_col <- if ('dfa_name' %in% names(boundary_sf)) 'dfa_name' else names(boundary_sf)[1]
      areas <- setdiff(unique(as.character(boundary_sf[[area_col]])), c('Inaccessible', 'Unpopulated'))
      if (length(areas) == 0)
        return(DT::datatable(data.frame(Message = 'No health areas in this version yet.'),
                             rownames = FALSE, options = list(dom = 't')))

      targets <- tryCatch(.unwrap_team_targets(ver$snap$team_targets), error = function(e) list())

      worldpop_pop <- vapply(areas, function(a) {
        sub_sf <- boundary_sf[boundary_sf[[area_col]] == a, ]
        tryCatch({
          sub_proj <- sf::st_transform(sub_sf, sf::st_crs(terra::crs(u5_rast)))
          sum(exactextractr::exact_extract(raster::raster(u5_rast), sub_proj, fun = 'sum'), na.rm = TRUE)
        }, error = function(e) NA_real_)
      }, numeric(1))

      field_pop   <- vapply(areas, function(a) targets[[a]]$target_pop %||% NA_real_, numeric(1))
      field_teams <- vapply(areas, function(a) targets[[a]]$requested_teams %||% NA_real_, numeric(1))
      recommended_teams <- vapply(worldpop_pop, function(p) {
        tryCatch(as.integer(compute_n_teams(p, campaign_id = review_target()$campaign_id)), error = function(e) NA_integer_)
      }, integer(1))

      display <- data.frame(
        `Health Area`                   = areas,
        `WorldPop Population`            = ifelse(is.na(worldpop_pop), '\u2013', format(round(worldpop_pop), big.mark = ',')),
        `Field Target Population`         = ifelse(is.na(field_pop), '\u2013', format(round(field_pop), big.mark = ',')),
        `Recommended Teams (WorldPop)`     = ifelse(is.na(recommended_teams), '\u2013', as.character(recommended_teams)),
        `Field Requested Teams`             = ifelse(is.na(field_teams), '\u2013', as.character(field_teams)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(display, rownames = FALSE, selection = 'none', options = list(dom = 't', scrollX = TRUE))
    })

    output$ha_version_table <- DT::renderDT({
      df <- ha_versions_rv()
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = 'Select a district to review its health-area versions.'),
                             rownames = FALSE, options = list(dom = 't')))
      }
      ns_str <- session$ns('')
      df$Action <- vapply(seq_len(nrow(df)), function(i) {
        btns <- character(0)
        if (!isTRUE(df$is_shared[i]))
          btns <- c(btns, sprintf(
            '<button class="btn btn-default btn-xs" style="color:#166534;border-color:#166534;" onclick="Shiny.setInputValue(\'%sha_make_current_row\', %d, {priority:\'event\'})">Make current</button>',
            ns_str, df$version_id[i]))
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
        Campaign = df$campaign_id,
        Version  = df$version_number,
        Current  = ifelse(df$is_shared, 'Yes', 'No'),
        Created  = format(df$created_at, '%d %b %Y %H:%M'),
        Updated  = format(df$last_updated_at, '%d %b %Y %H:%M'),
        Action   = df$Action,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(display, escape = FALSE, rownames = FALSE, selection = 'none',
                    options = list(dom = 'ft', pageLength = 200, scrollX = TRUE))
    })

    observeEvent(input$ha_make_current_row, {
      tryCatch({
        db_publish_version(pool, input$ha_make_current_row, actor_role = 'admin')
        refresh_review(); refresh_progress()
        showNotification('Set as current health-area version.', type = 'message', duration = 3)
      }, error = function(e) showNotification(paste('Failed:', e$message), type = 'error', duration = 6))
    }, ignoreInit = TRUE)

    observeEvent(input$unshare_row, {
      tryCatch({ db_unshare_version(pool, input$unshare_row); refresh_review(); refresh_progress() },
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
        removeModal(); refresh_review(); refresh_progress()
      }, error = function(e) showNotification(paste('Failed:', e$message), type = 'error', duration = 6))
    }, ignoreInit = TRUE)

    output$ta_version_table <- DT::renderDT({
      df <- ta_versions_rv()
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = 'No team-area versions for this district.'),
                             rownames = FALSE, options = list(dom = 't')))
      }
      ns_str <- session$ns('')
      df$Action <- vapply(seq_len(nrow(df)), function(i) {
        btns <- character(0)
        if (!isTRUE(df$is_shared[i]))
          btns <- c(btns, sprintf(
            '<button class="btn btn-default btn-xs" style="color:#166534;border-color:#166534;" onclick="Shiny.setInputValue(\'%sta_make_current_row\', %d, {priority:\'event\'})">Make current</button>',
            ns_str, df$team_version_id[i]))
        if (isTRUE(df$is_shared[i]))
          btns <- c(btns, sprintf(
            '<button class="btn btn-default btn-xs" onclick="Shiny.setInputValue(\'%sta_unshare_row\', %d, {priority:\'event\'})">Unshare</button>',
            ns_str, df$team_version_id[i]))
        btns <- c(btns, sprintf(
          '<button class="btn btn-default btn-xs" style="color:#ef4444;border-color:#ef4444;" onclick="Shiny.setInputValue(\'%sta_archive_row\', %d, {priority:\'event\'})">Archive</button>',
          ns_str, df$team_version_id[i]))
        paste(btns, collapse = ' ')
      }, character(1))

      display <- data.frame(
        `Health area` = df$health_area_name,
        Owner          = df$owner_username,
        Campaign        = df$campaign_id,
        Version          = df$version_number,
        Current           = ifelse(df$is_shared, 'Yes', 'No'),
        Updated             = format(df$last_updated_at, '%d %b %Y %H:%M'),
        Action               = df$Action,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(display, escape = FALSE, rownames = FALSE, selection = 'none',
                    options = list(dom = 'ft', pageLength = 200, scrollX = TRUE))
    })

    observeEvent(input$ta_make_current_row, {
      tryCatch({
        db_publish_team_area(pool, input$ta_make_current_row)
        refresh_review()
        showNotification('Set as current team-area version.', type = 'message', duration = 3)
      }, error = function(e) {
        # This is where a stale-publish refusal from db_publish_team_area()
        # surfaces. Admin restore never bypasses that check -- there is no
        # automatic fix. The pinned health-area version has to be made
        # current again first (Health area versions, above), which will
        # then let this same action succeed.
        showNotification(
          paste0('Could not set as current: ', e$message,
                ' Make that health-area version current in the table above, then try again.'),
          type = 'error', duration = 8
        )
      })
    }, ignoreInit = TRUE)

    observeEvent(input$ta_unshare_row, {
      tryCatch({ db_unshare_team_area_version(pool, input$ta_unshare_row); refresh_review() },
              error = function(e) showNotification(paste('Failed:', e$message), type = 'error', duration = 6))
    }, ignoreInit = TRUE)

    pending_ta_archive_id <- reactiveVal(NULL)
    observeEvent(input$ta_archive_row, {
      showModal(modalDialog(
        title = 'Archive this team-area version?', size = 's', easyClose = TRUE,
        tags$p(style = 'font-size:13px;', 'This removes it from every picker. It is not permanently deleted.'),
        footer = tagList(
          modalButton('Cancel'),
          actionButton(session$ns('confirm_ta_archive'), 'Archive', class = 'btn btn-danger')
        )
      ))
      pending_ta_archive_id(input$ta_archive_row)
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_ta_archive, {
      vid <- pending_ta_archive_id(); req(!is.null(vid))
      tryCatch({
        db_archive_team_area_version(pool, vid); pending_ta_archive_id(NULL)
        removeModal(); refresh_review()
      }, error = function(e) showNotification(paste('Failed:', e$message), type = 'error', duration = 6))
    }, ignoreInit = TRUE)

    # =========================================================================
    # SECTION 5: Generation settings (global values; per-campaign overrides
    # exist in the schema but aren't exposed in this UI yet)
    # =========================================================================

    settings_rv <- reactiveVal(NULL)
    # Only target_pop_per_health_area and target_pop_per_team are ever
    # actually read back via db_get_generation_setting() anywhere in the
    # app (confirmed by searching every call site) -- n_start_dfas,
    # pop_sat_pct/weight/max, and subdivision_boundary_penalty are already
    # fully hardcoded elsewhere (global.R's own n_start_dfas constant,
    # function defaults, or literal call-site values), so editing them
    # here currently has no effect on actual generation behavior at all.
    # Filtered out of the editable table for that reason -- not a
    # functional change, just removing dead, misleading controls.
    HARDCODED_SETTING_KEYS <- c('n_start_dfas', 'pop_sat_max', 'pop_sat_pct',
                               'pop_sat_weight', 'subdivision_boundary_penalty')
    refresh_settings <- function() {
      df <- tryCatch(db_get_all_generation_settings(pool), error = function(e) NULL)
      if (!is.null(df)) {
        df <- df[is.na(df$campaign_id), , drop = FALSE]
        df <- df[!(df$setting_key %in% HARDCODED_SETTING_KEYS), , drop = FALSE]
      }
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
      # subdivisions_url falls back to ARCGIS_SUBDIVISIONS_URL (the actual
      # hardcoded constant fetch_subdivisions_for_district() uses in
      # subdivision_helpers.R) when the DB has no override saved yet.
      # Both this and urban_areas_url below ARE now genuinely wired into
      # their respective fetches (server.R's subdivisions_rv observer
      # resolves this one; fetch_urban_areas_for_district() resolves its
      # own internally) -- saving a new value in either really does
      # change what gets fetched, including for a district that was
      # already visited this session, since save_sources_btn's handler
      # clears the shared fetch cache on every save.
      current_subdiv_url <- tryCatch(db_get_data_source_url(pool, 'subdivisions_url'), error = function(e) '') %||% ''
      hardcoded_subdiv_url <- tryCatch(ARCGIS_SUBDIVISIONS_URL, error = function(e) '')
      resolved_subdiv_url <- if (nzchar(current_subdiv_url)) current_subdiv_url else hardcoded_subdiv_url
      updateTextInput(session, 'subdivisions_url', value = resolved_subdiv_url)
      # urban_areas_url pre-fills with whatever the subdivisions URL
      # resolves to (its own saved override, or the hardcoded constant)
      # until an admin has saved a distinct urban-areas override of its
      # own -- exactly the "same layer for now, replace later" the admin
      # was told to expect. Unlike subdivisions_url, this one IS actually
      # wired into the fetch (fetch_urban_areas_for_district(),
      # subdivision_helpers.R) -- saving a new value here really does
      # change what urban-only mapping scope uses.
      current_urban_url <- tryCatch(db_get_data_source_url(pool, 'urban_areas_url'), error = function(e) '') %||% ''
      updateTextInput(session, 'urban_areas_url',
                      value = if (nzchar(current_urban_url)) current_urban_url else resolved_subdiv_url)
      updateTextInput(session, 'idp_url',
                      value = tryCatch(db_get_data_source_url(pool, 'idp_settlements_url'), error = function(e) '') %||% '')
    })

    observeEvent(input$save_sources_btn, {
      ok <- TRUE
      tryCatch(db_set_data_source_url(pool, 'subdivisions_url', trimws(input$subdivisions_url %||% ''), current_user()),
              error = function(e) { ok <<- FALSE; showNotification(paste('Failed (subdivisions):', e$message), type = 'error', duration = 6) })
      tryCatch(db_set_data_source_url(pool, 'urban_areas_url', trimws(input$urban_areas_url %||% ''), current_user()),
              error = function(e) { ok <<- FALSE; showNotification(paste('Failed (urban areas):', e$message), type = 'error', duration = 6) })
      tryCatch(db_set_data_source_url(pool, 'idp_settlements_url', trimws(input$idp_url %||% ''), current_user()),
              error = function(e) { ok <<- FALSE; showNotification(paste('Failed (IDP):', e$message), type = 'error', duration = 6) })
      # Both subdivisions_url and urban_areas_url are read by
      # fetch_subdivisions_for_district() (subdivision_helpers.R), whose
      # cache is keyed only by district, not by URL -- without clearing
      # it here, a district visited before this save would keep
      # returning its old, stale fetch until the app restarted, even
      # though the URL setting itself changed correctly. IDP has no
      # such cache, so it needs no equivalent step.
      tryCatch(clear_subdivision_cache(), error = function(e) NULL)
      if (ok) showNotification('Data source URLs saved.', type = 'message', duration = 3)
    }, ignoreInit = TRUE)

    invisible(NULL)
  })
}
