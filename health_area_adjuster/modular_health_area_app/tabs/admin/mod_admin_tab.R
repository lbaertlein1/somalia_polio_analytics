# =============================================================================
# mod_admin_tab.R  —  Admin panel
#
# Fixes applied:
#  1. District assignment uses hierarchical tree (zone → region → district)
#     Checking zone/region cascades to children via JS
#  2. User table works with zero users
#  3. Download produces a .zip with microplan CSV + health areas shapefile
#     CSV includes SIA coordination site coordinates per health area
#     Shapefile includes UID, area name, zone/region/district, geometry
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
                                         icon = icon('rotate'), class = 'btn btn-default btn-sm', title = 'Refresh'),
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
    
    # ── Storage helpers (Phase 6: replace with DB queries) ───────────────────
    .read_users <- function()       db_get_users(pool)
    .write_users <- function(df) {
      for (i in seq_len(nrow(df))) {
        db_upsert_user(pool, df$username[i], df$password[i],
                       df$display_name[i], df$role[i])
      }
    }
    .read_ud <- function()          db_get_user_districts(pool)
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
    
    # ── Helper: slug for use as CSS class ─────────────────────────────────────
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
          rownames = FALSE, options = list(dom = 't'),
          selection = 'none'
        ))
      }
      
      n_dists <- vapply(df$username, function(u) {
        if (isTRUE(df$role[df$username == u] == 'admin')) {
          nrow(sf::st_drop_geometry(districts_shp) |> dplyr::distinct(district_name))
        } else {
          sum(ud$username == u)
        }
      }, integer(1))
      
      display <- data.frame(
        Username       = df$username,
        `Display name` = df$display_name,
        Password       = df$password,
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
                    options = list(dom        = 'ft',
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
    
    editing_user <- reactiveVal(NULL)
    
    observeEvent(input$add_user_btn, { editing_user(NULL); .show_user_modal(NULL) })
    observeEvent(input$edit_user_row, {
      editing_user(as.integer(input$edit_user_row)); .show_user_modal(as.integer(input$edit_user_row))
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
      showModal(modalDialog(
        title = 'Confirm delete', size = 's', easyClose = TRUE,
        footer = tagList(
          modalButton('Cancel'),
          actionButton(session$ns('confirm_delete'), 'Delete', class = 'btn btn-danger')
        ),
        tags$p(style = 'font-size:14px;', paste0('Delete user "', uname, '"? Cannot be undone.'))
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$confirm_delete, {
      db_delete_user(pool, uname)
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
      current_pass   <- if (is_edit) df$password[idx]     else ''
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
                 textInput(session$ns('new_password'),  'Password',     value = current_pass,  width = '100%'),
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
        df <- rbind(df, data.frame(
          username     = uname,
          password     = password,
          display_name = if (nzchar(dispname)) dispname else uname,
          role         = role,
          stringsAsFactors = FALSE
        ))
      } else {
        old_uname            <- df$username[idx]
        df$username[idx]     <- uname
        df$display_name[idx] <- if (nzchar(dispname)) dispname else uname
        df$password[idx]     <- password
        df$role[idx]         <- role
        ud$username[ud$username == old_uname] <- uname
        ud <- ud[ud$username != uname, , drop = FALSE]
      }
      
      if (role != 'admin' && length(dists) > 0) {
        ud <- rbind(ud, data.frame(
          username      = uname,
          district_name = dists,
          stringsAsFactors = FALSE
        ))
      }
      
      .write_users(df); .write_ud(ud)
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
      input$refresh_progress; store$refreshed
      tryCatch(.load_progress(), error = function(e) {
        cat('[progress] ERROR:', e$message, '\n')
        NULL
      })
    })
    
    .load_progress <- function() {
      cat('[progress] .load_progress called\n')
      
      all_dists <- sf::st_drop_geometry(districts_shp) |>
        dplyr::distinct(district_name, region_name, zone_name) |>
        dplyr::arrange(zone_name, region_name, district_name)
      cat('[progress] all_dists:', nrow(all_dists), '\n')
      
      saved_rows <- tryCatch(
        db_get_progress(pool),
        error = function(e) { cat('[progress] db_get_progress error:', e$message, '\n'); NULL }
      )
      cat('[progress] saved_rows:', if (is.null(saved_rows)) 'NULL' else nrow(saved_rows), '\n')
      
      saved_map <- list()
      if (!is.null(saved_rows) && nrow(saved_rows) > 0) {
        for (i in seq_len(nrow(saved_rows))) {
          r     <- saved_rows[i, ]
          dname <- as.character(r$district_name)
          
          dfa_sf <- tryCatch({
            x <- r$saved_dfa_sf
            if (is.null(x) || is.na(x) || !nzchar(trimws(as.character(x)))) NULL
            else geojsonsf::geojson_sf(x)
          }, error = function(e) NULL)
          
          plan_data <- tryCatch({
            x <- r$planning_data
            if (is.null(x) || is.na(x) || !nzchar(trimws(as.character(x)))) list()
            else jsonlite::fromJSON(x, simplifyVector = FALSE)
          }, error = function(e) list())
          
          has_areas  <- !is.null(dfa_sf) && nrow(dfa_sf) > 0
          n_areas    <- if (has_areas) sum(!dfa_sf$dfa_name %in% extra_dfa_names) else 0L
          regular    <- names(plan_data)[!names(plan_data) %in% extra_dfa_names]
          n_complete <- sum(vapply(regular, function(a) isTRUE(plan_data[[a]]$complete), logical(1)))
          status     <- if (!has_areas) "Not started" else
            if (n_areas > 0L && n_complete == n_areas) "Complete" else "In progress"
          
          saved_map[[dname]] <- list(
            status        = status,
            n_areas       = n_areas,
            n_complete    = n_complete,
            last_activity = tryCatch(format(r$saved_at, "%d %b %Y, %H:%M"), error = function(e) ""),
            username      = as.character(r$username  %||% ""),
            session_id    = as.character(r$session_id %||% "")
          )
        }
      }
      cat("[progress] saved_map entries:", length(saved_map), "\n")
      
      rows <- lapply(seq_len(nrow(all_dists)), function(i) {
        dname <- as.character(all_dists$district_name[i])
        s     <- saved_map[[dname]]
        data.frame(
          district_name = dname,
          region_name   = as.character(all_dists$region_name[i]),
          zone_name     = as.character(all_dists$zone_name[i]),
          status        = if (!is.null(s)) s$status        else "Not started",
          n_areas       = if (!is.null(s)) s$n_areas       else 0L,
          n_complete    = if (!is.null(s)) s$n_complete    else 0L,
          last_activity = if (!is.null(s)) s$last_activity else "",
          username      = if (!is.null(s)) s$username      else "",
          session_id    = if (!is.null(s)) s$session_id    else "",
          stringsAsFactors = FALSE
        )
      })
      out <- dplyr::bind_rows(rows)
      cat("[progress] output rows:", nrow(out), "\n")
      out
    }
    
    output$progress_table <- DT::renderDT({
      pd <- progress_data()
      cat("[progress] renderDT rows:", if (is.null(pd)) "NULL" else nrow(pd), "\n")
      
      if (is.null(pd) || nrow(pd) == 0) {
        return(DT::datatable(
          data.frame(Message = "No districts found."),
          rownames = FALSE, options = list(dom = "t"), selection = "none"))
      }
      
      ns_str    <- session$ns("")
      btn_style <- "font-size:11px;padding:2px 8px;cursor:pointer;border-radius:3px;"
      
      display <- data.frame(
        District     = pd$district_name,
        Region       = pd$region_name,
        Zone         = pd$zone_name,
        Status       = pd$status,
        Areas        = pd$n_areas,
        Complete     = pd$n_complete,
        `Last saved` = pd$last_activity,
        User         = pd$username,
        Review       = seq_len(nrow(pd)),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      
      DT::datatable(display, rownames = FALSE, selection = "none",
                    options = list(dom        = 'ft',
                                   pageLength = 200,
                                   scrollY    = 'calc(100vh - 320px)',
                                   scrollCollapse = TRUE,
                                   columnDefs = list(
                                     list(targets = 3, render = DT::JS(
                                       "function(data,type,row){ if(type!=='display') return data; var c={'Complete':{bg:'#f0fdf4',col:'#166534',bd:'#bbf7d0'},'In progress':{bg:'#fefce8',col:'#854d0e',bd:'#fde68a'},'Not started':{bg:'#f8fafc',col:'#64748b',bd:'#e2e8f0'}}; var s=c[data]||c['Not started']; return '<span style=\"background:'+s.bg+';color:'+s.col+';border:1px solid '+s.bd+';border-radius:20px;padding:2px 9px;font-size:11px;font-weight:600;\">'+data+'</span>'; }")),
                                     list(targets = 8, render = DT::JS(sprintf(
                                       "function(data,type,row){ if(type!=='display') return data; if(row[3]==='Not started') return ''; return '<button style=\"%sborder:1px solid #1565C0;background:#fff;color:#1565C0;\" onclick=\"Shiny.setInputValue(\\'%sreview_row\\','+data+',{priority:\\'event\\'})\" >Review</button>'; }",
                                       btn_style, ns_str)))
                                   )
                    ), escape = FALSE)
    })
    
    # ── Review modal ──────────────────────────────────────────────────────────
    review_session <- reactiveVal(NULL)
    
    observeEvent(input$review_row, {
      idx <- as.integer(input$review_row)
      pd  <- isolate(progress_data())
      req(!is.null(pd), idx >= 1L, idx <= nrow(pd))
      
      sid   <- pd$session_id[idx]
      dname <- pd$district_name[idx]
      
      if (!nzchar(sid %||% '')) {
        showNotification('No saved data for this district yet.', type = 'warning', duration = 3)
        return()
      }
      
      snaps <- tryCatch(db_get_snapshots(pool, sid), error = function(e) list())
      snap  <- if (length(snaps) > 0) snaps[[length(snaps)]] else NULL
      review_session(list(district = dname, snap = snap))
      
      showModal(modalDialog(
        title     = tags$span(
          tags$span(style = 'font-weight:700;color:#0f172a;', dname),
          tags$span(style = 'color:#94a3b8;font-size:12px;margin-left:8px;', '— District Review')
        ),
        size = 'xl', easyClose = TRUE,
        footer = tagList(
          downloadButton(session$ns('download_district'), 'Download district data',
                         class = 'btn btn-default'),
          modalButton('Close')
        ),
        fluidRow(
          column(7,
                 div(style = 'height:420px;border-radius:7px;overflow:hidden;',
                     leaflet::leafletOutput(session$ns('review_map'), width = '100%', height = '420px'))
          ),
          column(5,
                 div(class = 'rightbar-title', 'Health Areas'),
                 div(style = 'overflow-y:auto;max-height:400px;',
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
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = 'Satellite') |>
        leaflet::addLayersControl(baseGroups = c('OpenStreetMap', 'Satellite'),
                                  options = leaflet::layersControlOptions(collapsed = TRUE))
      
      if (!is.null(snap) && !is.null(snap$saved_dfa_sf) && nrow(snap$saved_dfa_sf) > 0) {
        sf_obj <- snap$saved_dfa_sf
        plan   <- snap$planning_data %||% list()
        cols   <- vapply(sf_obj$dfa_name, function(a) {
          if (a == 'Inaccessible') '#D7301F'
          else if (a == 'Unpopulated') '#e2e8f0'
          else if (isTRUE(plan[[a]]$complete)) '#0d9488'
          else '#94a3b8'
        }, character(1))
        m <- m |>
          leaflet::addPolygons(data = sf_obj, fillColor = cols, fillOpacity = 0.55,
                               color = '#334155', weight = 1.5, label = sf_obj$dfa_name,
                               labelOptions = leaflet::labelOptions(noHide = TRUE, direction = 'center',
                                                                    textOnly = TRUE, style = list('font-size' = '11px', 'font-weight' = '600',
                                                                                                  'background' = 'none', 'border' = 'none', 'box-shadow' = 'none')))
        # Zoom to district
        bbox <- sf::st_bbox(sf::st_transform(sf_obj, 4326))
        m <- m |> leaflet::fitBounds(
          lng1 = bbox[['xmin']], lat1 = bbox[['ymin']],
          lng2 = bbox[['xmax']], lat2 = bbox[['ymax']]
        )
      }
      if (!is.null(snap$odk_sf) && nrow(snap$odk_sf) > 0) {
        sia <- snap$odk_sf[!is.na(snap$odk_sf$polio_sia_coordination_site) &
                             snap$odk_sf$polio_sia_coordination_site == 'Yes', ]
        if (nrow(sia) > 0)
          m <- m |> leaflet::addCircleMarkers(data = sia, radius = 6,
                                              color = '#fff', weight = 2, fillColor = '#0d9488', fillOpacity = 1,
                                              label = sia$facility_name)
      }
      m
    })
    
    output$review_table <- DT::renderDT({
      rs   <- review_session(); req(!is.null(rs))
      plan <- rs$snap$planning_data %||% list()
      if (length(plan) == 0) {
        return(DT::datatable(data.frame(Message = 'No microplan data yet.'),
                             rownames = FALSE, options = list(dom = 't')))
      }
      rows <- dplyr::bind_rows(lapply(names(plan), function(a) {
        d <- plan[[a]]
        data.frame(Area = a, `U5 pop` = d$u5_pop %||% 0,
                   Teams = d$n_teams %||% 0, Supervisors = d$n_supervisors %||% 0,
                   Complete = if (isTRUE(d$complete)) '\u2713' else '\u2013',
                   stringsAsFactors = FALSE, check.names = FALSE)
      }))
      DT::datatable(rows, rownames = FALSE, selection = 'none',
                    options = list(dom        = 'ft',
                                   pageLength = 200,
                                   scrollY    = 'calc(100vh - 320px)',
                                   scrollCollapse = TRUE))
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
          dplyr::filter(district_name == rs$district) |> dplyr::slice(1)
        
        # Combine odk + app facility data from snapshot
        fac_parts <- Filter(Negate(is.null), list(snap$odk_sf, snap$app_sf))
        fac_parts <- Filter(function(x) nrow(x) > 0, fac_parts)
        fac_df    <- if (length(fac_parts) > 0)
          sf::st_drop_geometry(do.call(rbind, fac_parts)) else NULL
        
        build_district_zip(
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
    
    # ── Download ALL data ─────────────────────────────────────────────────────
    output$download_all <- downloadHandler(
      filename = function()
        paste0('all_districts_', format(Sys.Date(), '%Y%m%d'), '.zip'),
      content = function(file) {
        pd <- progress_data()
        if (is.null(pd) || nrow(pd) == 0) {
          write.csv(data.frame(message = 'No data.'), file, row.names = FALSE)
          return()
        }
        
        tmp_master <- tempfile(); dir.create(tmp_master)
        on.exit(unlink(tmp_master, recursive = TRUE), add = TRUE)
        
        for (i in seq_len(nrow(pd))) {
          sid   <- pd$session_id[i]
          dname <- pd$district_name[i]
          
          files_i <- list.files(SESSIONS_DIR, pattern = paste0(sid, '\\.rds'),
                                recursive = TRUE, full.names = TRUE)
          if (length(files_i) == 0) next
          
          s    <- tryCatch(readRDS(files_i[1]), error = function(e) NULL)
          snap <- if (!is.null(s) && length(s$history) > 0)
            s$history[[length(s$history)]] else NULL
          if (is.null(snap)) next
          
          fac_parts <- Filter(Negate(is.null), list(snap$odk_sf, snap$app_sf))
          fac_parts <- Filter(function(x) nrow(x) > 0, fac_parts)
          fac_df    <- if (length(fac_parts) > 0)
            sf::st_drop_geometry(do.call(rbind, fac_parts)) else NULL
          
          dist_zip <- file.path(tmp_master,
                                paste0(gsub('[^A-Za-z0-9]', '_', tolower(dname)), '.zip'))
          
          build_district_zip(
            file          = dist_zip,
            district_name = dname,
            zone          = pd$zone_name[i]   %||% '',
            region        = pd$region_name[i] %||% '',
            saved_dfa_sf  = snap$saved_dfa_sf,
            planning_data = snap$planning_data %||% list(),
            facility_data = fac_df
          )
        }
        
        all_zips <- list.files(tmp_master, full.names = TRUE)
        if (length(all_zips) == 0) {
          write.csv(data.frame(message = 'No data.'), file, row.names = FALSE)
          return()
        }
        tryCatch(
          zip::zip(zipfile = file, files = all_zips, mode = 'cherry-pick'),
          error = function(e) zip(zipfile = file, files = all_zips, flags = '-j')
        )
      }
    )
    
  })
}


# =============================================================================
# District tree builder
# =============================================================================

.build_district_tree <- function(districts_shp, ns_fn, selected_dists = character(0)) {
  
  .slug_css <- function(x) gsub('[^A-Za-z0-9]', '_', tolower(trimws(x)))
  
  zones     <- sort(unique(as.character(stats::na.omit(districts_shp$zone_name))))
  input_id  <- ns_fn('selected_districts')
  
  zone_blocks <- lapply(zones, function(zone) {
    zone_cls <- .slug_css(zone)
    regions  <- sort(unique(districts_shp$region_name[districts_shp$zone_name == zone]))
    n_z_dist <- length(unique(districts_shp$district_name[districts_shp$zone_name == zone]))
    
    region_blocks <- lapply(regions, function(region) {
      region_cls <- .slug_css(region)
      dists      <- sort(unique(districts_shp$district_name[
        districts_shp$zone_name == zone & districts_shp$region_name == region
      ]))
      
      dist_items <- lapply(dists, function(dist) {
        tags$label(
          style = 'display:block;padding:2px 0 2px 24px;font-size:12px;font-weight:400;cursor:pointer;',
          tags$input(
            type    = 'checkbox',
            class   = paste('dist-cb', region_cls, zone_cls),
            value   = dist,
            style   = 'margin-right:6px;cursor:pointer;vertical-align:middle;',
            checked = if (dist %in% selected_dists) 'checked' else NULL
          ),
          dist
        )
      })
      
      tags$details(
        style = 'margin-bottom:2px;',
        tags$summary(
          style = paste0('padding:4px 8px;cursor:pointer;list-style:none;',
                         'display:flex;align-items:center;gap:6px;'),
          tags$input(
            type         = 'checkbox',
            class        = paste('region-cb', zone_cls),
            `data-rcls`  = region_cls,
            style        = 'cursor:pointer;flex-shrink:0;vertical-align:middle;',
            onclick      = 'event.stopPropagation();'
          ),
          tags$span(style = 'font-size:12px;font-weight:600;color:#334155;', region),
          tags$span(style = 'font-size:11px;color:#94a3b8;margin-left:auto;', length(dists))
        ),
        div(style = 'padding-left:6px;', do.call(tagList, dist_items))
      )
    })
    
    tags$details(
      style = 'border:1px solid #e2e8f0;border-radius:6px;margin-bottom:6px;overflow:hidden;',
      tags$summary(
        style = paste0('padding:8px 10px;background:#f8fafc;cursor:pointer;',
                       'list-style:none;display:flex;align-items:center;gap:8px;'),
        tags$input(
          type        = 'checkbox',
          class       = 'zone-cb',
          `data-zcls` = zone_cls,
          style       = 'cursor:pointer;flex-shrink:0;vertical-align:middle;',
          onclick     = 'event.stopPropagation();'
        ),
        tags$span(style = 'font-size:12px;font-weight:700;color:#1e293b;', zone),
        tags$span(style = 'font-size:11px;color:#94a3b8;margin-left:auto;',
                  paste0(n_z_dist, ' districts'))
      ),
      div(style = 'padding:6px 8px;', do.call(tagList, region_blocks))
    )
  })
  
  div(
    class         = 'district-tree',
    `data-inp-id` = input_id,
    do.call(tagList, zone_blocks),
    
    # Inline JS — cascade logic + initial sync
    tags$script(HTML(sprintf("
      (function() {
        var INP = '%s';

        function sync(tree) {
          var sel = [];
          $(tree).find('.dist-cb:checked').each(function() { sel.push($(this).val()); });
          Shiny.setInputValue(INP, sel, {priority:'event'});
        }

        // Zone checkbox → check all regions + districts in that zone
        $(document).on('change', '.district-tree[data-inp-id=\"'+INP+'\"] .zone-cb', function(e) {
          e.stopPropagation();
          var $t = $(this).closest('.district-tree');
          var zcls = $(this).data('zcls');
          var chk  = $(this).is(':checked');
          $t.find('.'+zcls).prop('checked', chk);
          sync($t);
        });

        // Region checkbox → check all districts in that region
        $(document).on('change', '.district-tree[data-inp-id=\"'+INP+'\"] .region-cb', function(e) {
          e.stopPropagation();
          var $t = $(this).closest('.district-tree');
          var rcls = $(this).data('rcls');
          var chk  = $(this).is(':checked');
          $t.find('.'+rcls).prop('checked', chk);
          sync($t);
        });

        // Individual district checkbox
        $(document).on('change', '.district-tree[data-inp-id=\"'+INP+'\"] .dist-cb', function() {
          var $t = $(this).closest('.district-tree');
          sync($t);
        });

        // Initial sync (pre-selected districts when editing)
        setTimeout(function() {
          var $t = $('.district-tree[data-inp-id=\"'+INP+'\"]');
          if ($t.length) sync($t);
        }, 150);

      })();
    ", input_id)))
  )
}
