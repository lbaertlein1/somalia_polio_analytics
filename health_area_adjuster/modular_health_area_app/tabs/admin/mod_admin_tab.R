# =============================================================================
# mod_admin_tab.R  —  Admin panel
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
               'Manage users and review district progress.')
      ),
      actionButton(ns('download_all_btn'), 'Download all data',
                   icon = icon('download'), class = 'btn btn-default')
    ),
    
    .admin_section('Users',
                   action  = actionButton(ns('add_user_btn'), 'Add user',
                                          icon = icon('plus'), class = 'btn btn-primary btn-sm'),
                   content = DT::DTOutput(ns('user_table'), width = '100%')
    ),
    
    .admin_section('District progress',
                   action = div(
                     style = 'display:flex;gap:8px;align-items:center;',
                     radioButtons(ns('progress_mode_click'), label = NULL,
                                  choices  = c('Actual' = 'actual', 'Practice' = 'practice'),
                                  selected = 'actual', inline = TRUE),
                     actionButton(ns('refresh_progress'), NULL,
                                  icon = icon('rotate'), class = 'btn btn-default btn-sm',
                                  title = 'Refresh')
                   ),
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
    .read_users  <- function()      db_get_users(pool)
    .read_ud     <- function()      db_get_user_districts(pool)
    .write_ud    <- function(df) {
      for (uname in unique(df$username))
        db_set_user_districts(pool, uname, df$district_name[df$username == uname])
    }
    store <- reactiveValues(users_df = NULL, ud_df = NULL, refreshed = 0L)
    
    observe({
      store$refreshed
      store$users_df <- .read_users()
      store$ud_df    <- .read_ud()
    })
    
    all_zones <- sort(unique(as.character(stats::na.omit(districts_shp$zone_name))))
    .slug     <- function(x) gsub('[^A-Za-z0-9]', '_', tolower(trimws(x)))
    
    # ── Progress mode (actual vs practice) ───────────────────────────────────
    
    progress_practice_rv <- reactiveVal(FALSE)
    
    observeEvent(input$progress_mode_click, {
      progress_practice_rv(input$progress_mode_click == 'practice')
    }, ignoreInit = TRUE)
    
    
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
      
      share_pw <- vapply(seq_len(nrow(df)), function(i) {
        tp <- df$temp_password[i] %||% ''
        if (nzchar(tp)) tp else '— logged in'
      }, character(1))
      
      display <- data.frame(
        Username         = df$username,
        `Display name`   = df$display_name,
        `Share password` = share_pw,
        Role             = df$role,
        Districts        = ifelse(df$role == 'admin',
                                  paste0(n_dists, ' (all)'),
                                  as.character(n_dists)),
        Edit             = seq_len(nrow(df)),
        Delete           = seq_len(nrow(df)),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      
      ns_str    <- session$ns('')
      btn_style <- 'font-size:11px;padding:2px 8px;cursor:pointer;border-radius:3px;'
      
      DT::datatable(display, rownames = FALSE, selection = 'none',
                    options = list(
                      dom        = 'ft',
                      pageLength = 200,
                      autoWidth      = FALSE,
                      scrollX        = TRUE,
                      scrollY        = 'calc(100vh - 320px)',
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
    
    # ── User modal ────────────────────────────────────────────────────────────
    
    .show_user_modal <- function(idx) {
      is_edit       <- !is.null(idx)
      df            <- store$users_df
      ud            <- store$ud_df
      current_uname <- if (is_edit) df$username[idx]      else ''
      current_dname <- if (is_edit) df$display_name[idx]  else ''
      current_pass  <- if (is_edit) df$temp_password[idx] %||% '' else ''
      current_role  <- if (is_edit) df$role[idx]          else 'user'
      current_dists <- if (is_edit && current_role != 'admin')
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
                 textInput(session$ns('new_username'), 'Username',     value = current_uname, width = '100%'),
                 textInput(session$ns('new_dispname'), 'Display name', value = current_dname, width = '100%'),
                 textInput(session$ns('new_password'), 'Password',     value = current_pass,
                           placeholder = 'Enter password', width = '100%'),
                 selectInput(session$ns('new_role'), 'Role',
                             choices = c('user', 'admin'), selected = current_role, width = '100%')
          ),
          column(8,
                 conditionalPanel(
                   condition = sprintf("input['%s'] !== 'admin'", session$ns('new_role')),
                   tags$label('Assigned districts',
                              style = 'font-size:12px;font-weight:600;color:#334155;display:block;margin-bottom:6px;'),
                   div(style = 'max-height:380px;overflow-y:auto;border:1px solid #e2e8f0;border-radius:7px;padding:8px;',
                       .build_district_tree(districts_shp, session$ns, current_dists)),
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
      
      if (!is.null(idx)) {
        old_uname <- df$username[idx]
        if (old_uname != uname) ud$username[ud$username == old_uname] <- uname
        ud <- ud[ud$username != uname, , drop = FALSE]
      }
      
      if (role != 'admin' && length(dists) > 0) {
        ud <- rbind(ud, data.frame(username = uname, district_name = dists,
                                   stringsAsFactors = FALSE))
      }
      
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
    # SECTION 2: Progress table
    # =========================================================================
    
    progress_data <- reactive({
      input$refresh_progress; store$refreshed; progress_practice_rv()
      
      tryCatch({
        all_dists <- sf::st_drop_geometry(districts_shp) |>
          dplyr::distinct(district_name, region_name) |>
          dplyr::arrange(region_name, district_name)
        
        subs <- db_get_all_submissions(pool, is_practice = progress_practice_rv())
        
        if (is.null(subs) || nrow(subs) == 0) {
          return(
            all_dists |>
              dplyr::mutate(
                planning_label = district_name,
                version        = NA_integer_,
                has_landmarks  = FALSE, has_facilities = FALSE,
                has_areas      = FALSE, has_microplan  = FALSE,
                status         = 'Not started',
                submitted_by   = '', last_submitted = '', started = ''
              )
          )
        }
        
        subs_clean <- subs |>
          dplyr::mutate(
            planning_label = district_name,
            district_name  = sub(' — .*$', '', district_name),
            started        = tryCatch(
              format(first_submitted_at, '%d %b %Y, %H:%M'),
              error = function(e) ''
            ),
            last_submitted = tryCatch(
              format(last_submitted_at, '%d %b %Y, %H:%M'),
              error = function(e) ''
            ),
            status = dplyr::case_when(
              has_microplan                              ~ 'Complete',
              has_areas | has_facilities | has_landmarks ~ 'In progress',
              TRUE                                       ~ 'Not started'
            )
          ) |>
          dplyr::select(planning_label, district_name, version,
                        submitted_by, started, last_submitted,
                        has_landmarks, has_facilities, has_areas, has_microplan, status)
        
        submitted_districts <- unique(subs_clean$district_name)
        
        unsubmitted <- all_dists |>
          dplyr::filter(!district_name %in% submitted_districts) |>
          dplyr::mutate(
            planning_label = district_name,
            version        = NA_integer_,
            has_landmarks  = FALSE, has_facilities = FALSE,
            has_areas      = FALSE, has_microplan  = FALSE,
            status         = 'Not started',
            submitted_by   = '', last_submitted = '', started = ''
          )
        
        subs_with_region <- subs_clean |>
          dplyr::left_join(
            sf::st_drop_geometry(districts_shp) |> dplyr::distinct(district_name, region_name),
            by = 'district_name'
          )
        
        dplyr::bind_rows(subs_with_region, unsubmitted) |>
          dplyr::arrange(region_name, district_name, planning_label)
        
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
                        'Complete'    = list(bg = '#f0fdf4', col = '#166534', bd = '#bbf7d0'),
                        'In progress' = list(bg = '#fefce8', col = '#854d0e', bd = '#fde68a'),
                        list(bg = '#f8fafc', col = '#64748b', bd = '#e2e8f0')
          )
          sprintf(
            '<span style="background:%s;color:%s;border:1px solid %s;border-radius:20px;padding:2px 9px;font-size:11px;font-weight:600;">%s</span>',
            cfg$bg, cfg$col, cfg$bd, s
          )
        }, character(1))
      }
      
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
      
      .history_btn <- function(dname, status) {
        if (status == 'Not started') return('')
        paste0(
          '<button style="font-size:11px;padding:2px 8px;cursor:pointer;',
          'border-radius:3px;border:1px solid #7c3aed;background:#fff;color:#7c3aed;" ',
          'onclick="Shiny.setInputValue(\'', ns_btn('history_row'), '\',\'', dname, '\',',
          '{priority:\'event\'})">', 'History</button>'
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
        District        = pd$planning_label,
        Region          = pd$region_name %||% '',
        Ver             = ifelse(is.na(pd$version), '', paste0('v', pd$version)),
        Landmarks       = vapply(pd$has_landmarks,  .stage_html, character(1)),
        Facilities      = vapply(pd$has_facilities, .stage_html, character(1)),
        `Health Areas`  = vapply(pd$has_areas,      .stage_html, character(1)),
        Microplan       = vapply(pd$has_microplan,  .stage_html, character(1)),
        Status          = .status_html(pd$status),
        Started         = pd$started,
        `Last submitted`= pd$last_submitted,
        User            = pd$submitted_by,
        Review          = mapply(.review_btn,  pd$planning_label, pd$status, SIMPLIFY = TRUE),
        History         = mapply(.history_btn, pd$planning_label, pd$status, SIMPLIFY = TRUE),
        Reject          = mapply(.reject_btn,  pd$planning_label, pd$status, SIMPLIFY = TRUE),
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
          autoWidth      = FALSE,
          scrollX        = TRUE,
          scrollY        = 'calc(100vh - 320px)',
          scrollCollapse = TRUE,
          columnDefs     = list(
            list(targets = 3:13, orderable = FALSE)
          )
        )
      )
    })
    
    
    # =========================================================================
    # SECTION 3: Reject
    # =========================================================================
    
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
          actionButton(session$ns('confirm_reject'), 'Reject & delete all versions',
                       class = 'btn btn-danger', style = 'font-weight:600;')
        ),
        tags$p(
          style = 'font-size:14px;',
          paste0('Delete ALL versions of submitted data for "', dname,
                 '"? This cannot be undone.')
        )
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$confirm_reject, {
      dname    <- rejecting_district()
      practice <- progress_practice_rv()
      req(!is.null(dname))
      tryCatch({
        db_delete_district_submission(pool, dname, practice)
        store$refreshed <- store$refreshed + 1L
        showNotification(
          paste0('All versions for "', dname, '" deleted.'),
          type = 'message', duration = 3
        )
      }, error = function(e) {
        showNotification(paste('Delete failed:', e$message), type = 'error', duration = 5)
      })
      rejecting_district(NULL)
      removeModal()
    }, ignoreInit = TRUE)
    
    
    # =========================================================================
    # SECTION 4: Review modal
    # =========================================================================
    
    review_session <- reactiveVal(NULL)
    
    observeEvent(input$review_row, {
      dname    <- input$review_row
      practice <- progress_practice_rv()
      req(!is.null(dname), nzchar(dname))
      
      sub <- tryCatch(
        db_get_submission_for_review(pool, dname, is_practice = practice),
        error = function(e) NULL
      )
      
      if (is.null(sub)) {
        showNotification('No submitted data for this district yet.',
                         type = 'warning', duration = 3)
        return()
      }
      
      review_session(list(district = dname, snap = sub$snap,
                          version = sub$version, is_practice = practice))
      .show_review_modal(dname, sub$version, practice)
    }, ignoreInit = TRUE)
    
    .show_review_modal <- function(dname, ver, practice) {
      mode_badge <- if (practice)
        tags$span(style = paste0(
          'background:#fef9c3;color:#854d0e;border:1px solid #fde68a;',
          'border-radius:20px;padding:1px 8px;font-size:10px;font-weight:700;margin-left:6px;'),
          'PRACTICE')
      else NULL
      
      showModal(modalDialog(
        title = tagList(
          tags$span(style = 'font-weight:700;color:#0f172a;', dname),
          tags$span(style = 'color:#94a3b8;font-size:12px;margin-left:8px;',
                    paste0('— v', ver, ' (current)')),
          mode_badge
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
                     leaflet::leafletOutput(session$ns('review_map'), width = '100%', height = '440px'))
          ),
          column(6,
                 div(class = 'rightbar-title', 'Health Areas'),
                 div(style = 'overflow-y:auto;max-height:420px;',
                     DT::DTOutput(session$ns('review_table')))
          )
        )
      ))
    }
    
    output$review_map <- leaflet::renderLeaflet({
      rs <- review_session(); req(!is.null(rs))
      snap <- rs$snap
      
      m <- leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = 'Satellite') |>
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
              style  = list('font-size' = '11px', 'font-weight' = '600',
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
      
      fac_parts <- Filter(Negate(is.null), list(snap$odk_sf, snap$app_sf))
      fac_parts <- Filter(function(x) nrow(x) > 0, fac_parts)
      if (length(fac_parts) > 0) {
        all_fac <- do.call(rbind, fac_parts)
        sia <- all_fac[!is.na(all_fac$polio_sia_coordination_site) &
                         all_fac$polio_sia_coordination_site == 'Yes', ]
        if (nrow(sia) > 0)
          m <- m |> leaflet::addCircleMarkers(
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
                    options = list(dom = 'ft', pageLength = 200,
                                   scrollY = '360px', scrollCollapse = TRUE))
    })
    
    output$download_district <- downloadHandler(
      filename = function() {
        d <- gsub('[^A-Za-z0-9]', '_', review_session()$district %||% 'district')
        paste0('microplan_', d, '_', format(Sys.Date(), '%Y%m%d'), '.zip')
      },
      content = function(file) {
        rs            <- review_session()
        snap          <- rs$snap
        base_district <- sub(' — .*$', '', rs$district %||% '')
        di <- sf::st_drop_geometry(districts_shp) |>
          dplyr::filter(district_name == base_district) |>
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
    # SECTION 5: Version history modal
    # =========================================================================
    
    history_district <- reactiveVal(NULL)
    restoring_ver    <- reactiveVal(NULL)
    
    observeEvent(input$history_row, {
      dname    <- input$history_row
      practice <- progress_practice_rv()
      req(!is.null(dname), nzchar(dname))
      history_district(list(name = dname, practice = practice))
      
      versions <- tryCatch(
        db_get_submission_versions(pool, dname, is_practice = practice),
        error = function(e) NULL
      )
      
      if (is.null(versions) || nrow(versions) == 0) {
        showNotification('No version history found.', type = 'warning', duration = 3)
        return()
      }
      
      .show_history_modal(dname, practice, versions)
    }, ignoreInit = TRUE)
    
    .show_history_modal <- function(dname, practice, versions) {
      ns_btn <- session$ns
      
      mode_badge <- if (practice)
        tags$span(style = paste0(
          'background:#fef9c3;color:#854d0e;border:1px solid #fde68a;',
          'border-radius:20px;padding:1px 8px;font-size:10px;font-weight:700;margin-left:6px;'),
          'PRACTICE')
      else NULL
      
      # Build version rows
      ver_rows <- lapply(seq_len(nrow(versions)), function(i) {
        v       <- versions[i, ]
        is_cur  <- isTRUE(v$is_current)
        ver_num <- v$version
        
        started_str  <- tryCatch(format(v$first_submitted_at, '%d %b %Y, %H:%M'),
                                 error = function(e) '—')
        archived_str <- if (!is.na(v$archived_at))
          tryCatch(format(v$archived_at, '%d %b %Y, %H:%M'), error = function(e) '—')
        else '—'
        
        .chk <- function(f) if (isTRUE(f))
          tags$span(style = 'color:#166534;font-weight:700;', '\u2713')
        else
          tags$span(style = 'color:#cbd5e1;', '\u2013')
        
        restored_note <- if (!is.na(v$restored_from_version))
          tags$span(style = 'font-size:10px;color:#7c3aed;margin-left:6px;',
                    paste0('(restored from v', v$restored_from_version, ')'))
        else NULL
        
        div(
          style = paste0(
            'border:1px solid ', if (is_cur) '#0d9488' else '#e2e8f0', ';',
            'border-radius:7px;padding:12px 14px;margin-bottom:8px;',
            'background:', if (is_cur) '#f0fdf4' else '#f8fafc', ';'
          ),
          div(
            style = 'display:flex;align-items:center;gap:8px;margin-bottom:6px;',
            tags$span(
              style = 'font-size:13px;font-weight:700;color:#0f172a;',
              paste0('Version ', ver_num)
            ),
            if (is_cur)
              tags$span(style = paste0(
                'background:#0d9488;color:#fff;border-radius:20px;',
                'padding:1px 8px;font-size:10px;font-weight:700;'),
                'CURRENT')
            else
              tags$span(style = paste0(
                'background:#f1f5f9;color:#64748b;border-radius:20px;',
                'padding:1px 8px;font-size:10px;'),
                'Archived'),
            restored_note
          ),
          div(
            style = 'display:flex;gap:16px;font-size:12px;color:#475569;margin-bottom:8px;',
            div(tags$span(style='font-weight:600;', 'Started: '), started_str),
            div(tags$span(style='font-weight:600;', 'Archived: '), archived_str),
            div(tags$span(style='font-weight:600;', 'By: '), v$submitted_by %||% '—')
          ),
          div(
            style = 'display:flex;gap:12px;font-size:12px;margin-bottom:if(!is_cur)8px else 0px;',
            div(style='display:flex;gap:4px;align-items:center;', .chk(v$has_landmarks),  ' Landmarks'),
            div(style='display:flex;gap:4px;align-items:center;', .chk(v$has_facilities), ' Facilities'),
            div(style='display:flex;gap:4px;align-items:center;', .chk(v$has_areas),      ' Health Areas'),
            div(style='display:flex;gap:4px;align-items:center;', .chk(v$has_microplan),  ' Microplan')
          ),
          # Restore button — only shown for archived versions
          if (!is_cur)
            div(
              style = 'margin-top:8px;',
              tags$button(
                style = paste0(
                  'font-size:11px;padding:3px 10px;cursor:pointer;',
                  'border-radius:4px;border:1px solid #7c3aed;',
                  'background:#fff;color:#7c3aed;font-weight:600;'
                ),
                onclick = paste0(
                  "Shiny.setInputValue('", ns_btn('restore_version_click'), "',",
                  "{'district':'", dname, "','version':", ver_num, "},",
                  "{priority:'event'})"
                ),
                '\u21ba Restore this version'
              )
            )
        )
      })
      
      showModal(modalDialog(
        title     = tagList(
          tags$span(style = 'font-weight:700;color:#0f172a;', dname),
          tags$span(style = 'color:#94a3b8;font-size:12px;margin-left:8px;',
                    '— Version History'),
          mode_badge
        ),
        size      = 'm',
        easyClose = TRUE,
        footer    = modalButton('Close'),
        div(style = 'max-height:60vh;overflow-y:auto;padding-right:4px;',
            do.call(tagList, ver_rows))
      ))
    }
    
    observeEvent(input$restore_version_click, {
      info  <- input$restore_version_click
      req(!is.null(info))
      dname    <- info$district
      ver      <- as.integer(info$version)
      practice <- isolate(progress_practice_rv())
      restoring_ver(list(district = dname, version = ver, practice = practice))
      
      showModal(modalDialog(
        title     = 'Confirm restore',
        size      = 's',
        easyClose = FALSE,
        footer    = NULL,
        div(
          style = 'font-size:13px;color:#475569;margin-bottom:16px;',
          paste0(
            'Restore version ', ver, ' of "', dname, '"? ',
            'The current version will be archived and version ', ver,
            ' will become the new active version.'
          )
        ),
        div(
          style = 'display:flex;gap:10px;justify-content:flex-end;',
          actionButton(session$ns('restore_cancel'),  'Cancel', class = 'btn btn-default'),
          actionButton(session$ns('restore_confirm'), 'Restore',
                       class = 'btn btn-primary', style = 'font-weight:600;')
        )
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$restore_cancel, {
      restoring_ver(NULL); removeModal()
    }, ignoreInit = TRUE)
    
    observeEvent(input$restore_confirm, {
      rv <- restoring_ver()
      req(!is.null(rv))
      tryCatch({
        new_ver <- db_restore_submission_version(
          pool, rv$district, rv$version, is_practice = rv$practice
        )
        store$refreshed <- store$refreshed + 1L
        removeModal()
        showNotification(
          paste0('Version ', rv$version, ' restored as v', new_ver, '.'),
          type = 'message', duration = 4
        )
      }, error = function(e) {
        showNotification(paste('Restore failed:', e$message), type = 'error', duration = 5)
      })
      restoring_ver(NULL)
    }, ignoreInit = TRUE)
    
    
    # =========================================================================
    # SECTION 6: Download all — modal with options
    # =========================================================================
    
    download_opts <- reactiveValues(
      mode     = 'actual',    # 'actual' | 'practice'
      versions = 'current'    # 'current' | 'all'
    )
    
    observeEvent(input$download_all_btn, {
      showModal(modalDialog(
        title     = 'Download all data',
        size      = 's',
        easyClose = TRUE,
        footer    = tagList(
          modalButton('Cancel'),
          downloadButton(session$ns('download_all'), 'Download',
                         class = 'btn btn-primary', style = 'font-weight:600;')
        ),
        div(style = 'display:flex;flex-direction:column;gap:14px;',
            div(
              tags$label(style = 'font-size:12px;font-weight:600;color:#334155;display:block;margin-bottom:4px;',
                         'Session type'),
              radioButtons(session$ns('dl_mode'), label = NULL,
                           choices  = c('Actual' = 'actual', 'Practice' = 'practice'),
                           selected = 'actual', inline = TRUE)
            ),
            div(
              tags$label(style = 'font-size:12px;font-weight:600;color:#334155;display:block;margin-bottom:4px;',
                         'Versions to include'),
              radioButtons(session$ns('dl_versions'), label = NULL,
                           choices  = c('Current version only' = 'current',
                                        'All versions'         = 'all'),
                           selected = 'current', inline = FALSE)
            )
        )
      ))
    }, ignoreInit = TRUE)
    
    output$download_all <- downloadHandler(
      filename = function() {
        mode <- input$dl_mode     %||% 'actual'
        vers <- input$dl_versions %||% 'current'
        paste0('submissions_', mode, '_', vers, '_', format(Sys.Date(), '%Y%m%d'), '.zip')
      },
      content = function(file) {
        mode        <- input$dl_mode     %||% 'actual'
        vers        <- input$dl_versions %||% 'current'
        is_practice <- mode == 'practice'
        
        # Fetch rows to export
        if (vers == 'current') {
          subs_meta <- db_get_all_submissions(pool, is_practice = is_practice)
          if (is.null(subs_meta) || nrow(subs_meta) == 0) {
            write.csv(data.frame(message = 'No submitted data.'), file, row.names = FALSE)
            return()
          }
          # Each row is (district_name, version) — version is the current one
          export_rows <- data.frame(
            district_name = subs_meta$district_name,
            version       = subs_meta$version,
            stringsAsFactors = FALSE
          )
        } else {
          # All versions — query all rows for this mode
          all_rows <- tryCatch(
            .db_query(pool, "
              SELECT district_name, version
              FROM district_submissions
              WHERE is_practice = ?p
              ORDER BY district_name, version
            ", list(p = is_practice)),
            error = function(e) NULL
          )
          if (is.null(all_rows) || nrow(all_rows) == 0) {
            write.csv(data.frame(message = 'No submitted data.'), file, row.names = FALSE)
            return()
          }
          export_rows <- all_rows
        }
        
        tmp <- tempfile(); dir.create(tmp)
        on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
        
        all_areas_sf   <- list()
        all_sia_sf     <- list()
        all_microplan  <- list()
        all_facilities <- list()
        
        for (i in seq_len(nrow(export_rows))) {
          dname         <- export_rows$district_name[i]
          ver           <- export_rows$version[i]
          base_district <- sub(' — .*$', '', dname)
          di   <- sf::st_drop_geometry(districts_shp) |>
            dplyr::filter(district_name == base_district) |>
            dplyr::slice(1)
          zone   <- di$zone_name[1]   %||% ''
          region <- di$region_name[1] %||% ''
          
          sub <- tryCatch(
            db_get_submission_version(pool, dname, ver, is_practice = is_practice),
            error = function(e) NULL
          )
          if (is.null(sub)) next
          snap <- sub$snap
          
          # Version label suffix for 'all versions' exports
          ver_suffix <- if (vers == 'all') paste0('_v', ver) else ''
          
          if (!is.null(snap$saved_dfa_sf) && nrow(snap$saved_dfa_sf) > 0) {
            sf_i <- snap$saved_dfa_sf |>
              dplyr::mutate(
                zone_name = zone, region_name = region,
                district_name = dname, version = ver,
                uid = paste0(
                  gsub('[^A-Za-z0-9]', '_', tolower(trimws(dname))),
                  ver_suffix, '__',
                  gsub('[^A-Za-z0-9]', '_', tolower(trimws(dfa_name)))
                )
              ) |>
              dplyr::select(zone_name, region_name, district_name, version,
                            area_name = dfa_name, uid, geometry)
            all_areas_sf[[length(all_areas_sf) + 1]] <- sf_i
          }
          
          plan <- snap$planning_data %||% list()
          if (length(plan) > 0) {
            rows <- lapply(names(plan), function(area_name) {
              d       <- plan[[area_name]]
              sup     <- d$supervisors %||% list()
              sup_cols <- list()
              for (s_i in seq_len(min(length(sup), 10L))) {
                s <- sup[[s_i]] %||% list()
                sup_cols[[paste0('supervisor_', s_i, '_name')]]  <- s$name  %||% ''
                sup_cols[[paste0('supervisor_', s_i, '_role')]]  <- s$role  %||% ''
                sup_cols[[paste0('supervisor_', s_i, '_phone')]] <- s$phone %||% ''
                sup_cols[[paste0('supervisor_', s_i, '_email')]] <- s$email %||% ''
              }
              base <- data.frame(
                zone_name = zone, region_name = region,
                district_name = dname, version = ver,
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
          
          fac_parts <- Filter(Negate(is.null), list(snap$odk_sf, snap$app_sf))
          fac_parts <- Filter(function(x) inherits(x, 'sf') && nrow(x) > 0, fac_parts)
          if (length(fac_parts) > 0) {
            all_fac_sf <- do.call(rbind, fac_parts)
            fac_df <- sf::st_drop_geometry(all_fac_sf) |>
              dplyr::mutate(zone_name = zone, region_name = region,
                            district_name = dname, version = ver) |>
              dplyr::select(zone_name, region_name, district_name, version,
                            dplyr::any_of(c('facility_id', 'facility_name', 'facility_type',
                                            'hf_ownership', 'polio_sia_coordination_site',
                                            'operational', 'lat', 'lon')))
            all_facilities[[length(all_facilities) + 1]] <- fac_df
            
            sia_i <- all_fac_sf[
              !is.na(all_fac_sf$polio_sia_coordination_site) &
                all_fac_sf$polio_sia_coordination_site == 'Yes', ]
            if (nrow(sia_i) > 0) {
              sia_i <- sia_i |>
                dplyr::mutate(zone_name = zone, region_name = region,
                              district_name = dname, version = ver) |>
                dplyr::select(zone_name, region_name, district_name, version,
                              dplyr::any_of(c('facility_id', 'facility_name',
                                              'facility_type', 'lat', 'lon')),
                              geometry)
              all_sia_sf[[length(all_sia_sf) + 1]] <- sia_i
            }
          }
        }
        
        if (length(all_areas_sf) == 0 && length(all_microplan) == 0) {
          write.csv(data.frame(message = 'No data could be exported.'), file, row.names = FALSE)
          return()
        }
        
        if (length(all_areas_sf) > 0) {
          combined_sf <- tryCatch(do.call(rbind, all_areas_sf) |> sf::st_transform(4326),
                                  error = function(e) NULL)
          if (!is.null(combined_sf))
            tryCatch(sf::st_write(combined_sf, file.path(tmp, 'health_areas.geojson'),
                                  driver = 'GeoJSON', delete_dsn = TRUE, quiet = TRUE),
                     error = function(e) cat('[download_all] health_areas error:', e$message, '\n'))
        }
        
        if (length(all_sia_sf) > 0) {
          combined_sia <- tryCatch(do.call(rbind, all_sia_sf) |> sf::st_transform(4326),
                                   error = function(e) NULL)
          if (!is.null(combined_sia))
            tryCatch(sf::st_write(combined_sia, file.path(tmp, 'sia_coordination_sites.geojson'),
                                  driver = 'GeoJSON', delete_dsn = TRUE, quiet = TRUE),
                     error = function(e) cat('[download_all] sia error:', e$message, '\n'))
        }
        
        if (length(all_microplan) > 0)
          write.csv(dplyr::bind_rows(all_microplan), file.path(tmp, 'microplan.csv'),
                    row.names = FALSE)
        
        if (length(all_facilities) > 0)
          write.csv(dplyr::bind_rows(all_facilities), file.path(tmp, 'facilities.csv'),
                    row.names = FALSE)
        
        out_files <- list.files(tmp, full.names = TRUE)
        if (length(out_files) == 0) {
          write.csv(data.frame(message = 'No data could be exported.'), file, row.names = FALSE)
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
    dists      <- sort(unique(districts_shp$district_name[districts_shp$region_name == region]))
    
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
          $t.find('.'+rcls).prop('checked', $(this).is(':checked'));
          sync($t);
        });
        $(document).on('change', '.district-tree[data-inp-id=\\\"'+INP+'\\\"] .dist-cb', function() {
          sync($(this).closest('.district-tree'));
        });
        setTimeout(function() {
          var $t = $('.district-tree[data-inp-id=\\\"'+INP+'\\\"]');
          if ($t.length) sync($t);
        }, 150);
      })();
    ", input_id)))
  )
}