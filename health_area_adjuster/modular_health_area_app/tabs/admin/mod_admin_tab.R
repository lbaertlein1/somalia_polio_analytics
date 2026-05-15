# =============================================================================
# mod_admin_tab.R  —  Admin panel
# =============================================================================

adminTabUI <- function(id) {
  ns <- NS(id)
  
  div(
    style = 'padding: 20px 28px; max-width: 1200px;',
    
    div(
      style = 'display: flex; align-items: center; margin-bottom: 20px;',
      div(
        style = 'flex: 1;',
        tags$h3(style = 'font-size: 18px; font-weight: 700; color: #0f172a; margin: 0 0 3px;',
                'Admin Panel'),
        tags$p(style = 'font-size: 13px; color: #64748b; margin: 0;',
               'Manage users and review district progress.')
      ),
      downloadButton(ns('download_all'), 'Download all data',
                     class = 'btn btn-default', icon = icon('download'))
    ),
    
    .admin_section('Users',
                   action = actionButton(ns('add_user_btn'), 'Add user',
                                         icon = icon('plus'), class = 'btn btn-primary btn-sm'),
                   content = DT::DTOutput(ns('user_table'), width = '100%')
    ),
    
    .admin_section('District progress',
                   action = actionButton(ns('refresh_progress'), NULL,
                                         icon = icon('rotate'), class = 'btn btn-default btn-sm',
                                         title = 'Refresh'),
                   content = DT::DTOutput(ns('progress_table'), width = '100%')
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

adminTabServer <- function(id, districts_shp) {
  moduleServer(id, function(input, output, session) {
    
    # ── Storage helpers ───────────────────────────────────────────────────────
    .read_users  <- function()       db_get_users(pool)
    .write_users <- function(df) {
      for (i in seq_len(nrow(df)))
        db_upsert_user(pool, df$username[i], df$password[i],
                       df$display_name[i], df$role[i])
    }
    .read_ud  <- function()          db_get_user_districts(pool)
    .write_ud <- function(df) {
      for (uname in unique(df$username)) {
        dists <- df$district_name[df$username == uname]
        db_set_user_districts(pool, uname, dists)
      }
    }
    store <- reactiveValues(users_df = NULL, ud_df = NULL, refreshed = 0L)
    
    observe({
      store$refreshed
      store$users_df <- .read_users()
      store$ud_df    <- .read_ud()
    })
    
    all_zones <- sort(unique(as.character(stats::na.omit(districts_shp$zone_name))))
    
    .slug <- function(x) gsub('[^A-Za-z0-9]', '_', tolower(trimws(x)))
    
    
    # =========================================================================
    # SECTION 1: User table
    # =========================================================================
    
    output$user_table <- DT::renderDT({
      df <- store$users_df %||% data.frame(
        username = character(0), display_name = character(0),
        password = character(0), role = character(0),
        stringsAsFactors = FALSE
      )
      ud <- store$ud_df %||% data.frame(
        username = character(0), district_name = character(0),
        stringsAsFactors = FALSE
      )
      
      if (nrow(df) == 0) {
        return(DT::datatable(
          data.frame(Message = 'No users found. Click "Add user" to create one.'),
          rownames = FALSE, options = list(dom = 't'), selection = 'none'
        ))
      }
      
      n_dists <- vapply(df$username, function(u) {
        if (isTRUE(df$role[df$username == u] == 'admin')) {
          nrow(sf::st_drop_geometry(districts_shp) |> dplyr::distinct(district_name))
        } else {
          sum(ud$username == u)
        }
      }, integer(1))
      
      # Show temp_password (plain text for sharing) — clears after first login
      share_pw <- vapply(seq_len(nrow(df)), function(i) {
        tp <- df$temp_password[i] %||% ''
        if (nzchar(tp)) tp else '— logged in'
      }, character(1))
      
      display <- data.frame(
        Username       = df$username,
        `Display name` = df$display_name,
        `Share password` = share_pw,
        Role           = df$role,
        Districts      = ifelse(df$role == 'admin',
                                paste0(n_dists, ' (all)'),
                                as.character(n_dists)),
        Edit           = seq_len(nrow(df)),
        Delete         = seq_len(nrow(df)),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      
      ns_str    <- session$ns('')
      btn_style <- 'font-size:11px;padding:2px 8px;cursor:pointer;border-radius:3px;'
      
      DT::datatable(display, rownames = FALSE, selection = 'none',
                    options = list(
                      dom        = 'ft',
                      pageLength = 200,
                      scrollY    = 'calc(100vh - 320px)',
                      scrollCollapse = TRUE,
                      columnDefs = list(
                        list(targets = 5, render = DT::JS(sprintf("
                          function(data,type,row){ if(type!=='display') return data;
                            return '<button style=\"%sborder:1px solid #1565C0;background:#fff;color:#1565C0;\"'+
                              ' onclick=\"Shiny.setInputValue(\\'%sedit_user_row\\','+data+',{priority:\\'event\\'})\">'+'Edit</button>'; }",
                                                                  btn_style, ns_str))),
                        list(targets = 6, render = DT::JS(sprintf("
                          function(data,type,row){ if(type!=='display') return data;
                            return '<button style=\"%sborder:1px solid #ef4444;background:#fff;color:#ef4444;\"'+
                              ' onclick=\"Shiny.setInputValue(\\'%sdelete_user_row\\','+data+',{priority:\\'event\\'})\">'+'Delete</button>'; }",
                                                                  btn_style, ns_str)))
                      )
                    ), escape = FALSE)
    })
    
    editing_user   <- reactiveVal(NULL)
    deleting_uname <- reactiveVal(NULL)
    
    observeEvent(input$add_user_btn,  { editing_user(NULL); .show_user_modal(NULL) })
    observeEvent(input$edit_user_row, {
      editing_user(as.integer(input$edit_user_row))
      .show_user_modal(as.integer(input$edit_user_row))
    }, ignoreInit = TRUE)
    
    observeEvent(input$delete_user_row, {
      idx   <- as.integer(input$delete_user_row)
      df    <- store$users_df
      req(!is.na(idx), idx >= 1L, idx <= nrow(df))
      uname <- df$username[idx]
      if (uname == 'admin') {
        showNotification('Cannot delete the admin account.', type = 'error', duration = 4)
        return()
      }
      deleting_uname(uname)
      showModal(modalDialog(
        title = 'Confirm delete', size = 's', easyClose = TRUE,
        footer = tagList(
          modalButton('Cancel'),
          actionButton(session$ns('confirm_delete'), 'Delete', class = 'btn btn-danger')
        ),
        tags$p(style = 'font-size:14px;',
               paste0('Delete user "', uname, '"? Cannot be undone.'))
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$confirm_delete, {
      db_delete_user(pool, deleting_uname())
      deleting_uname(NULL)
      store$refreshed <- store$refreshed + 1L
      removeModal()
    }, ignoreInit = TRUE)
    
    # ── User modal with hierarchical district tree ────────────────────────────
    
    .show_user_modal <- function(idx) {
      is_edit        <- !is.null(idx)
      df             <- store$users_df
      ud             <- store$ud_df
      current_uname  <- if (is_edit) df$username[idx]     else ''
      current_dname  <- if (is_edit) df$display_name[idx] else ''
      current_pass   <- if (is_edit) df$temp_password[idx] %||% '' else ''
      current_role   <- if (is_edit) df$role[idx]         else 'user'
      current_dists  <- if (is_edit && current_role != 'admin')
        ud$district_name[ud$username == current_uname]
      else character(0)
      
      showModal(modalDialog(
        title     = if (is_edit) paste0('Edit — ', current_uname) else 'Add user',
        size      = 'l', easyClose = FALSE,
        footer    = tagList(
          modalButton('Cancel'),
          actionButton(session$ns('save_user'),
                       if (is_edit) 'Save changes' else 'Add user',
                       class = 'btn btn-primary', style = 'font-weight:600;')
        ),
        fluidRow(
          column(4,
                 textInput(session$ns('new_username'),  'Username',     value = current_uname, width = '100%'),
                 textInput(session$ns('new_dispname'),  'Display name', value = current_dname, width = '100%'),
                 textInput(session$ns('new_password'),  'Password',     value = current_pass, placeholder = 'Enter password', width = '100%'),
                 selectInput(session$ns('new_role'), 'Role',
                             choices = c('user', 'admin'), selected = current_role, width = '100%')
          ),
          column(8,
                 conditionalPanel(
                   condition = sprintf("input['%s'] !== 'admin'", session$ns('new_role')),
                   tags$label('Assigned districts',
                              style = 'font-size:12px;font-weight:600;color:#334155;display:block;margin-bottom:6px;'),
                   div(style = 'max-height:380px;overflow-y:auto;border:1px solid #e2e8f0;border-radius:7px;padding:8px;',
                       .build_district_tree(districts_shp, session$ns, current_dists)
                   ),
                   div(style = 'margin-top:6px;font-size:11px;color:#94a3b8;',
                       uiOutput(session$ns('dist_count_label'), inline = TRUE))
                 )
          )
        )
      ))
    }
    
    output$dist_count_label <- renderUI({
      n <- length(input$selected_districts %||% character(0))
      if (n == 0) tags$span('No districts selected')
      else tags$span(style = 'color:#0d9488;font-weight:600;', paste0(n, ' district(s) selected'))
    })
    
    observeEvent(input$save_user, {
      uname    <- trimws(input$new_username %||% '')
      dispname <- trimws(input$new_dispname %||% '')
      password <- trimws(input$new_password %||% '')
      role     <- input$new_role %||% 'user'
      dists    <- input$selected_districts %||% character(0)
      idx      <- isolate(editing_user())
      
      if (!nzchar(uname) || !nzchar(password)) {
        showNotification('Username and password are required.', type = 'error', duration = 4)
        return()
      }
      
      df <- store$users_df
      ud <- store$ud_df
      
      if (is.null(idx) && uname %in% df$username) {
        showNotification(paste0('Username "', uname, '" already exists.'), type = 'error', duration = 4)
        return()
      }
      
      if (is.null(idx)) {
        if (!nzchar(password)) {
          showNotification('Password is required for new users.', type = 'error', duration = 4)
          return()
        }
        # New user — db_upsert_user handles the write; no local df manipulation needed
      } else {
        # For edits, only need old_uname to fix ud if the username changed
        old_uname <- df$username[idx]
        if (old_uname != uname) {
          ud$username[ud$username == old_uname] <- uname
        }
        ud <- ud[ud$username != uname, , drop = FALSE]
      }
      
      if (role != 'admin' && length(dists) > 0) {
        ud <- rbind(ud, data.frame(
          username      = uname,
          district_name = dists,
          stringsAsFactors = FALSE
        ))
      }
      
      # Only upsert the user that was changed — not all users.
      # Upserting all users would re-hash already-hashed passwords for unchanged users.
      db_upsert_user(pool, uname, password, if (nzchar(dispname)) dispname else uname, role)
      .write_ud(ud)
      store$refreshed <- store$refreshed + 1L
      removeModal()
      showNotification(
        if (is.null(idx)) paste0('User "', uname, '" added.')
        else               paste0('User "', uname, '" updated.'),
        type = 'message', duration = 3)
    }, ignoreInit = TRUE)
    
    
    # =========================================================================
    # SECTION 2: Progress table  (reads from district_submissions)
    # =========================================================================
    
    progress_data <- reactive({
      input$refresh_progress; store$refreshed
      
      tryCatch({
        # All districts as the base (so unsubmitted districts still appear)
        all_dists <- sf::st_drop_geometry(districts_shp) |>
          dplyr::distinct(district_name, region_name) |>
          dplyr::arrange(region_name, district_name)
        
        subs <- db_get_all_submissions(pool)
        
        if (is.null(subs) || nrow(subs) == 0) {
          return(all_dists |> dplyr::mutate(
            has_landmarks  = FALSE,
            has_facilities = FALSE,
            has_areas      = FALSE,
            has_microplan  = FALSE,
            status         = 'Not started',
            submitted_by   = '',
            last_submitted = ''
          ))
        }
        
        subs_clean <- subs |>
          dplyr::mutate(
            last_submitted = tryCatch(
              format(last_submitted_at, '%d %b %Y, %H:%M'),
              error = function(e) ''
            ),
            status = dplyr::case_when(
              has_microplan               ~ 'Complete',
              has_areas | has_facilities | has_landmarks ~ 'In progress',
              TRUE                        ~ 'Not started'
            )
          ) |>
          dplyr::select(district_name, submitted_by, last_submitted,
                        has_landmarks, has_facilities, has_areas, has_microplan, status)
        
        all_dists |>
          dplyr::left_join(subs_clean, by = 'district_name') |>
          dplyr::mutate(
            has_landmarks  = tidyr::replace_na(has_landmarks,  FALSE),
            has_facilities = tidyr::replace_na(has_facilities, FALSE),
            has_areas      = tidyr::replace_na(has_areas,      FALSE),
            has_microplan  = tidyr::replace_na(has_microplan,  FALSE),
            status         = tidyr::replace_na(status,         'Not started'),
            submitted_by   = tidyr::replace_na(submitted_by,   ''),
            last_submitted = tidyr::replace_na(last_submitted, '')
          )
      }, error = function(e) {
        cat('[progress] ERROR:', e$message, '\n')
        NULL
      })
    })
    
    output$progress_table <- DT::renderDT({
      pd <- progress_data()
      
      if (is.null(pd) || nrow(pd) == 0) {
        return(DT::datatable(
          data.frame(Message = 'No districts found.'),
          rownames = FALSE, options = list(dom = 't'), selection = 'none'
        ))
      }
      
      .stage_html <- function(flag) {
        if (isTRUE(flag))
          '<span style="color:#166534;font-weight:700;font-size:15px;">\u2713</span>'
        else
          '<span style="color:#cbd5e1;font-size:14px;">\u2013</span>'
      }
      
      .status_html <- function(status) {
        vapply(status, function(s) {
          cfg <- switch(s,
                        'Complete'    = list(bg='#f0fdf4', col='#166534', bd='#bbf7d0'),
                        'In progress' = list(bg='#fefce8', col='#854d0e', bd='#fde68a'),
                        list(bg='#f8fafc', col='#64748b', bd='#e2e8f0')
          )
          sprintf('<span style="background:%s;color:%s;border:1px solid %s;border-radius:20px;padding:2px 9px;font-size:11px;font-weight:600;">%s</span>',
                  cfg$bg, cfg$col, cfg$bd, s)
        }, character(1))
      }
      
      ns_str    <- session$ns('')
      ns_btn <- session$ns
      
      .review_btn <- function(dname, status) {
        if (status == 'Not started') return('')
        paste0(
          '<button style="font-size:11px;padding:2px 8px;cursor:pointer;',
          'border-radius:3px;border:1px solid #1565C0;background:#fff;color:#1565C0;" ',
          'onclick="Shiny.setInputValue(\'', ns_btn('review_row'), '\',\'', dname, '\',',
          '{priority:\'event\'})">', 'Review</button>'
        )
      }
      
      .reject_btn <- function(dname, status) {
        if (status == 'Not started') return('')
        paste0(
          '<button style="font-size:11px;padding:2px 8px;cursor:pointer;',
          'border-radius:3px;border:1px solid #ef4444;background:#fff;color:#ef4444;" ',
          'onclick="Shiny.setInputValue(\'', ns_btn('reject_row'), '\',\'', dname, '\',',
          '{priority:\'event\'})">', 'Reject</button>'
        )
      }
      
      display <- data.frame(
        District         = pd$district_name,
        Region           = pd$region_name,
        Landmarks        = vapply(pd$has_landmarks,  .stage_html, character(1)),
        Facilities       = vapply(pd$has_facilities, .stage_html, character(1)),
        `Health Areas`   = vapply(pd$has_areas,      .stage_html, character(1)),
        Microplan        = vapply(pd$has_microplan,  .stage_html, character(1)),
        Status           = .status_html(pd$status),
        `Last submitted` = pd$last_submitted,
        User             = pd$submitted_by,
        Review           = mapply(.review_btn, pd$district_name, pd$status, SIMPLIFY = TRUE),
        Reject           = mapply(.reject_btn, pd$district_name, pd$status, SIMPLIFY = TRUE),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      
      DT::datatable(
        display,
        rownames  = FALSE,
        selection = 'none',
        escape    = FALSE,
        options   = list(
          dom            = 'ft',
          pageLength     = 200,
          scrollY        = 'calc(100vh - 320px)',
          scrollCollapse = TRUE,
          columnDefs     = list(
            list(targets = 3:8, orderable = FALSE)   # stage + status + buttons not sortable
          )
        )
      )
    })
    
    
    # =========================================================================
    # SECTION 3: Review modal  (loads from district_submissions)
    # =========================================================================
    
    review_session    <- reactiveVal(NULL)
    rejecting_district <- reactiveVal(NULL)
    
    observeEvent(input$reject_row, {
      dname <- input$reject_row
      req(!is.null(dname), nzchar(dname))
      rejecting_district(dname)
      showModal(modalDialog(
        title     = 'Confirm rejection',
        size      = 's', easyClose = TRUE,
        footer    = tagList(
          modalButton('Cancel'),
          actionButton(session$ns('confirm_reject'), 'Reject & delete',
                       class = 'btn btn-danger', style = 'font-weight:600;')
        ),
        tags$p(
          style = 'font-size:14px;',
          paste0('Delete all submitted data for "', dname, '"? This cannot be undone.')
        )
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$confirm_reject, {
      dname <- rejecting_district()
      req(!is.null(dname))
      tryCatch({
        db_delete_district_submission(pool, dname)
        store$refreshed <- store$refreshed + 1L
        showNotification(
          paste0('Submission for "', dname, '" deleted.'),
          type = 'message', duration = 3
        )
      }, error = function(e) {
        showNotification(paste('Delete failed:', e$message), type = 'error', duration = 5)
      })
      rejecting_district(NULL)
      removeModal()
    }, ignoreInit = TRUE)
    
    observeEvent(input$review_row, {
      dname <- input$review_row
      req(!is.null(dname), nzchar(dname))
      pd    <- isolate(progress_data())
      req(!is.null(pd))
      sub   <- tryCatch(
        db_get_submission_for_review(pool, dname),
        error = function(e) NULL
      )
      
      if (is.null(sub)) {
        showNotification('No submitted data for this district yet.',
                         type = 'warning', duration = 3)
        return()
      }
      
      review_session(list(district = dname, snap = sub$snap))
      
      showModal(modalDialog(
        title = tags$span(
          tags$span(style = 'font-weight:700;color:#0f172a;', dname),
          tags$span(style = 'color:#94a3b8;font-size:12px;margin-left:8px;',
                    '— District Review')
        ),
        size = 'xl', easyClose = TRUE,
        footer = tagList(
          downloadButton(session$ns('download_district'), 'Download district data',
                         class = 'btn btn-default'),
          modalButton('Close')
        ),
        tags$style(HTML('.modal-dialog.modal-xl { max-width: 80vw !important; width: 80vw !important; }')),
        fluidRow(
          column(6,
                 div(style = 'height:440px;border-radius:7px;overflow:hidden;',
                     leaflet::leafletOutput(session$ns('review_map'),
                                            width = '100%', height = '440px'))
          ),
          column(6,
                 div(class = 'rightbar-title', 'Health Areas'),
                 div(style = 'overflow-y:auto;max-height:420px;',
                     DT::DTOutput(session$ns('review_table')))
          )
        )
      ))
    }, ignoreInit = TRUE)
    
    output$review_map <- leaflet::renderLeaflet({
      rs <- review_session(); req(!is.null(rs))
      snap <- rs$snap
      
      m <- leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                                  group = 'Satellite') |>
        leaflet::addLayersControl(
          baseGroups = c('OpenStreetMap', 'Satellite'),
          options    = leaflet::layersControlOptions(collapsed = TRUE)
        )
      
      if (!is.null(snap$saved_dfa_sf) && nrow(snap$saved_dfa_sf) > 0) {
        sf_obj <- snap$saved_dfa_sf
        plan   <- snap$planning_data %||% list()
        cols   <- vapply(sf_obj$dfa_name, function(a) {
          if (a == 'Inaccessible') '#D7301F'
          else if (a == 'Unpopulated') '#e2e8f0'
          else if (isTRUE(plan[[a]]$complete)) '#0d9488'
          else '#94a3b8'
        }, character(1))
        
        m <- m |>
          leaflet::addPolygons(
            data        = sf_obj,
            fillColor   = cols,
            fillOpacity = 0.55,
            color       = '#334155',
            weight      = 1.5,
            label       = sf_obj$dfa_name,
            labelOptions = leaflet::labelOptions(
              noHide = TRUE, direction = 'center', textOnly = TRUE,
              style = list('font-size' = '11px', 'font-weight' = '600',
                           'background' = 'none', 'border' = 'none',
                           'box-shadow' = 'none')
            )
          )
        
        bbox <- sf::st_bbox(sf::st_transform(sf_obj, 4326))
        m <- m |> leaflet::fitBounds(
          lng1 = bbox[['xmin']], lat1 = bbox[['ymin']],
          lng2 = bbox[['xmax']], lat2 = bbox[['ymax']]
        )
      }
      
      # SIA coordination sites
      fac_parts <- Filter(Negate(is.null), list(snap$odk_sf, snap$app_sf))
      fac_parts <- Filter(function(x) nrow(x) > 0, fac_parts)
      if (length(fac_parts) > 0) {
        all_fac <- do.call(rbind, fac_parts)
        sia <- all_fac[!is.na(all_fac$polio_sia_coordination_site) &
                         all_fac$polio_sia_coordination_site == 'Yes', ]
        if (nrow(sia) > 0)
          m <- m |>
          leaflet::addCircleMarkers(
            data        = sia,
            radius      = 6,
            color       = '#fff',
            weight      = 2,
            fillColor   = '#0d9488',
            fillOpacity = 1,
            label       = sia$facility_name
          )
      }
      
      m
    })
    
    output$review_table <- DT::renderDT({
      rs   <- review_session(); req(!is.null(rs))
      plan <- rs$snap$planning_data %||% list()
      
      if (length(plan) == 0) {
        return(DT::datatable(
          data.frame(Message = 'No microplan data yet.'),
          rownames = FALSE, options = list(dom = 't')
        ))
      }
      
      rows <- dplyr::bind_rows(lapply(names(plan), function(a) {
        d <- plan[[a]]
        data.frame(
          Area        = a,
          `U5 pop`    = d$u5_pop        %||% 0,
          Teams       = d$n_teams       %||% 0,
          Supervisors = d$n_supervisors %||% 0,
          Complete    = if (isTRUE(d$complete)) '\u2713' else '\u2013',
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }))
      
      DT::datatable(rows, rownames = FALSE, selection = 'none',
                    options = list(
                      dom           = 'ft',
                      pageLength    = 200,
                      scrollY       = '360px',
                      scrollCollapse = TRUE,
                      scrollX       = FALSE,
                      autoWidth     = TRUE
                    ))
    })
    
    output$download_district <- downloadHandler(
      filename = function() {
        d <- gsub('[^A-Za-z0-9]', '_', review_session()$district %||% 'district')
        paste0('microplan_', d, '_', format(Sys.Date(), '%Y%m%d'), '.zip')
      },
      content = function(file) {
        rs   <- review_session()
        snap <- rs$snap
        di   <- sf::st_drop_geometry(districts_shp) |>
          dplyr::filter(district_name == rs$district) |>
          dplyr::slice(1)
        
        fac_parts <- Filter(Negate(is.null), list(snap$odk_sf, snap$app_sf))
        fac_parts <- Filter(function(x) nrow(x) > 0, fac_parts)
        fac_df    <- if (length(fac_parts) > 0)
          sf::st_drop_geometry(do.call(rbind, fac_parts)) else NULL
        
        build_district_download(
          file          = file,
          district_name = rs$district,
          zone          = di$zone_name[1]   %||% '',
          region        = di$region_name[1] %||% '',
          saved_dfa_sf  = snap$saved_dfa_sf,
          planning_data = snap$planning_data %||% list(),
          facility_data = fac_df
        )
      }
    )
    
    
    # =========================================================================
    # SECTION 4: Download all  (reads from district_submissions)
    # =========================================================================
    
    output$download_all <- downloadHandler(
      filename = function()
        paste0('all_districts_', format(Sys.Date(), '%Y%m%d'), '.zip'),
      content = function(file) {
        
        pd <- progress_data()
        pd <- pd[pd$status != 'Not started', , drop = FALSE]
        
        if (is.null(pd) || nrow(pd) == 0) {
          write.csv(data.frame(message = 'No submitted data yet.'),
                    file, row.names = FALSE)
          return()
        }
        
        tmp <- tempfile(); dir.create(tmp)
        on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
        
        # ── Accumulators ───────────────────────────────────────────────────────
        all_areas_sf    <- list()   # sf objects — combined to health_areas.geojson
        all_sia_sf      <- list()   # sf point objects — combined to sia_coordination_sites.geojson
        all_microplan   <- list()   # data.frames — combined to microplan.csv
        all_facilities  <- list()   # data.frames — combined to facilities.csv
        
        for (i in seq_len(nrow(pd))) {
          dname  <- pd$district_name[i]
          zone   <- pd$zone_name[i]   %||% ''
          region <- pd$region_name[i] %||% ''
          
          sub <- tryCatch(
            db_get_submission_for_review(pool, dname),
            error = function(e) NULL
          )
          if (is.null(sub)) next
          snap <- sub$snap
          
          # ── Health area boundaries ──────────────────────────────────────────
          if (!is.null(snap$saved_dfa_sf) && nrow(snap$saved_dfa_sf) > 0) {
            sf_i <- snap$saved_dfa_sf |>
              dplyr::mutate(
                zone_name     = zone,
                region_name   = region,
                district_name = dname,
                uid = paste0(
                  gsub('[^A-Za-z0-9]', '_', tolower(trimws(dname))), '__',
                  gsub('[^A-Za-z0-9]', '_', tolower(trimws(dfa_name)))
                )
              ) |>
              dplyr::select(zone_name, region_name, district_name,
                            area_name = dfa_name, uid, geometry)
            all_areas_sf[[length(all_areas_sf) + 1]] <- sf_i
          }
          
          # ── Microplan rows ──────────────────────────────────────────────────
          plan <- snap$planning_data %||% list()
          if (length(plan) > 0) {
            rows <- lapply(names(plan), function(area_name) {
              d   <- plan[[area_name]]
              sup <- d$supervisors %||% list()
              
              # Flatten supervisors into wide columns (up to 10)
              sup_cols <- list()
              for (s_i in seq_len(min(length(sup), 10L))) {
                s <- sup[[s_i]] %||% list()
                sup_cols[[paste0('supervisor_', s_i, '_name')]]  <- s$name  %||% ''
                sup_cols[[paste0('supervisor_', s_i, '_role')]]  <- s$role  %||% ''
                sup_cols[[paste0('supervisor_', s_i, '_phone')]] <- s$phone %||% ''
                sup_cols[[paste0('supervisor_', s_i, '_email')]] <- s$email %||% ''
              }
              
              base <- data.frame(
                zone_name     = zone,
                region_name   = region,
                district_name = dname,
                area_name     = area_name,
                u5_pop        = as.numeric(d$u5_pop        %||% NA),
                n_teams       = as.integer(d$n_teams       %||% NA),
                n_supervisors = as.integer(d$n_supervisors %||% NA),
                complete      = isTRUE(d$complete),
                notes         = trimws(d$notes %||% ''),
                stringsAsFactors = FALSE
              )
              if (length(sup_cols) > 0)
                base <- cbind(base, as.data.frame(sup_cols, stringsAsFactors = FALSE))
              base
            })
            all_microplan[[length(all_microplan) + 1]] <- dplyr::bind_rows(rows)
          }
          
          # ── Facility rows + SIA coordination sites shapefile ──────────────────
          fac_parts <- Filter(Negate(is.null), list(snap$odk_sf, snap$app_sf))
          fac_parts <- Filter(function(x) inherits(x, 'sf') && nrow(x) > 0, fac_parts)
          if (length(fac_parts) > 0) {
            all_fac_sf <- do.call(rbind, fac_parts)
            
            # Flat CSV of all facilities
            fac_df <- sf::st_drop_geometry(all_fac_sf) |>
              dplyr::mutate(zone_name = zone, region_name = region,
                            district_name = dname) |>
              dplyr::select(zone_name, region_name, district_name,
                            dplyr::any_of(c('facility_id', 'facility_name',
                                            'facility_type', 'hf_ownership',
                                            'polio_sia_coordination_site',
                                            'operational', 'lat', 'lon')))
            all_facilities[[length(all_facilities) + 1]] <- fac_df
            
            # SIA coordination sites as point SF for shapefile
            sia_i <- all_fac_sf[
              !is.na(all_fac_sf$polio_sia_coordination_site) &
                all_fac_sf$polio_sia_coordination_site == 'Yes', ]
            if (nrow(sia_i) > 0) {
              sia_i <- sia_i |>
                dplyr::mutate(zone_name = zone, region_name = region,
                              district_name = dname) |>
                dplyr::select(zone_name, region_name, district_name,
                              dplyr::any_of(c('facility_id', 'facility_name',
                                              'facility_type', 'lat', 'lon')),
                              geometry)
              all_sia_sf[[length(all_sia_sf) + 1]] <- sia_i
            }
          }
        }
        
        # ── Nothing to export ─────────────────────────────────────────────────
        if (length(all_areas_sf) == 0 && length(all_microplan) == 0) {
          write.csv(data.frame(message = 'No data could be exported.'),
                    file, row.names = FALSE)
          return()
        }
        
        # ── Write health areas GeoJSON ─────────────────────────────────────────────────────────────────────────────
        if (length(all_areas_sf) > 0) {
          combined_sf <- tryCatch({
            do.call(rbind, all_areas_sf) |> sf::st_transform(4326)
          }, error = function(e) NULL)
          
          if (!is.null(combined_sf)) {
            tryCatch(
              sf::st_write(combined_sf,
                           file.path(tmp, 'health_areas.geojson'),
                           driver = 'GeoJSON', delete_dsn = TRUE, quiet = TRUE),
              error = function(e)
                cat('[download_all] health_areas GeoJSON error:', e$message, '\n')
            )
          }
        }
        
        # ── Write SIA coordination sites GeoJSON ────────────────────────────
        if (length(all_sia_sf) > 0) {
          combined_sia <- tryCatch({
            do.call(rbind, all_sia_sf) |> sf::st_transform(4326)
          }, error = function(e) NULL)
          if (!is.null(combined_sia)) {
            tryCatch(
              sf::st_write(combined_sia,
                           file.path(tmp, 'sia_coordination_sites.geojson'),
                           driver = 'GeoJSON', delete_dsn = TRUE, quiet = TRUE),
              error = function(e)
                cat('[download_all] sia GeoJSON error:', e$message, '\n')
            )
          }
        }
        
        # ── Write microplan CSV ───────────────────────────────────────────────
        if (length(all_microplan) > 0) {
          mp <- dplyr::bind_rows(all_microplan)
          write.csv(mp, file.path(tmp, 'microplan.csv'), row.names = FALSE)
        }
        
        # ── Write facilities CSV ──────────────────────────────────────────────
        if (length(all_facilities) > 0) {
          fac <- dplyr::bind_rows(all_facilities)
          write.csv(fac, file.path(tmp, 'facilities.csv'), row.names = FALSE)
        }
        
        # ── Zip all output files flat ─────────────────────────────────────────
        out_files <- list.files(tmp, full.names = TRUE)
        if (length(out_files) == 0) {
          write.csv(data.frame(message = 'No data could be exported.'),
                    file, row.names = FALSE)
          return()
        }
        tryCatch(
          zip::zip(zipfile = file, files = out_files, mode = 'cherry-pick'),
          error = function(e) zip(zipfile = file, files = out_files, flags = '-j')
        )
      }
    )
    
  })
}


# =============================================================================
# District tree builder  (unchanged)
# =============================================================================

.build_district_tree <- function(districts_shp, ns_fn, selected_dists = character(0)) {
  
  .slug_css <- function(x) gsub('[^A-Za-z0-9]', '_', tolower(trimws(x)))
  
  regions  <- sort(unique(as.character(stats::na.omit(districts_shp$region_name))))
  input_id <- ns_fn('selected_districts')
  
  region_blocks <- lapply(regions, function(region) {
    region_cls <- .slug_css(region)
    dists      <- sort(unique(districts_shp$district_name[
      districts_shp$region_name == region
    ]))
    
    dist_items <- lapply(dists, function(dist) {
      tags$label(
        style = 'display:block;padding:2px 0 2px 24px;font-size:12px;font-weight:400;cursor:pointer;',
        tags$input(
          type    = 'checkbox',
          class   = paste('dist-cb', region_cls),
          value   = dist,
          style   = 'margin-right:6px;cursor:pointer;vertical-align:middle;',
          checked = if (dist %in% selected_dists) 'checked' else NULL
        ),
        dist
      )
    })
    
    tags$details(
      style = 'border:1px solid #e2e8f0;border-radius:6px;margin-bottom:6px;overflow:hidden;',
      tags$summary(
        style = paste0('padding:8px 10px;background:#f8fafc;cursor:pointer;',
                       'list-style:none;display:flex;align-items:center;gap:8px;'),
        tags$input(
          type        = 'checkbox',
          class       = 'region-cb',
          `data-rcls` = region_cls,
          style       = 'cursor:pointer;flex-shrink:0;vertical-align:middle;',
          onclick     = 'event.stopPropagation();'
        ),
        tags$span(style = 'font-size:12px;font-weight:700;color:#1e293b;', region),
        tags$span(style = 'font-size:11px;color:#94a3b8;margin-left:auto;',
                  paste0(length(dists), ' districts'))
      ),
      div(style = 'padding:6px 8px;', do.call(tagList, dist_items))
    )
  })
  
  div(
    class         = 'district-tree',
    `data-inp-id` = input_id,
    do.call(tagList, region_blocks),
    
    tags$script(HTML(sprintf("
      (function() {
        var INP = '%s';

        function sync(tree) {
          var sel = [];
          $(tree).find('.dist-cb:checked').each(function() { sel.push($(this).val()); });
          Shiny.setInputValue(INP, sel, {priority:'event'});
        }

        $(document).on('change', '.district-tree[data-inp-id=\\\"'+INP+'\\\"] .region-cb', function(e) {
          e.stopPropagation();
          var $t = $(this).closest('.district-tree');
          var rcls = $(this).data('rcls');
          var chk  = $(this).is(':checked');
          $t.find('.'+rcls).prop('checked', chk);
          sync($t);
        });

        $(document).on('change', '.district-tree[data-inp-id=\\\"'+INP+'\\\"] .dist-cb', function() {
          var $t = $(this).closest('.district-tree');
          sync($t);
        });

        setTimeout(function() {
          var $t = $('.district-tree[data-inp-id=\\\"'+INP+'\\\"]');
          if ($t.length) sync($t);
        }, 150);

      })();
    ", input_id)))
  )
}