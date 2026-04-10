healthAreaTabUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      id = ns('app_row'),
      class = 'app-row',
      div(
        id = ns('leftbar'),
        class = 'app-leftbar',
        healthAreaControlsUI(ns('controls'))
      ),
      div(
        id = ns('mapwrap'),
        class = 'app-mapwrap',
        healthAreaMapUI(ns('map'))
      ),
      div(
        id = ns('rightbar'),
        class = 'app-rightbar',
        healthAreaPopulationUI(ns('population'))
      )
    )
  )
}

healthAreaTabServer <- function(id, zone, region, district, district_ready, active_tab) {
  moduleServer(id, function(input, output, session) {
    controls <- healthAreaControlsServer('controls')
    map_mod <- healthAreaMapServer('map')
    
    pending_action <- reactiveVal(NULL)
    help_shown <- reactiveVal(FALSE)
    
    observeEvent(controls$help_click(), {
      show_help_modal(session)
    })
    
    tab_active <- reactive({
      identical(active_tab(), 'tab_health_area_mapping')
    })
    
    observeEvent(district_ready(), {
      if (isTRUE(district_ready()) && !isTRUE(help_shown())) {
        show_help_modal(session)
        help_shown(TRUE)
      }
    }, ignoreInit = TRUE)
    
    rv <- reactiveValues(
      district_sf = NULL,
      district_base_sf = NULL,
      grid_sf = NULL,
      initial_assignments = NULL,
      current_assignments = NULL,
      saved_dfa_sf = NULL,
      neighbors_list = NULL,
      edge_list = NULL,
      pop_overlay_sf = NULL,
      pop_table = NULL,
      max_dim_m = NULL,
      grid_limits = NULL,
      brush_limits = NULL,
      seed_points = NULL
    )
    
    current_fill_colors <- reactive({
      make_fill_colors(controls$active_dfa())
    })
    
    healthAreaPopulationServer(
      'population',
      active_dfa = controls$active_dfa,
      show_pop_raster = controls$show_pop_raster,
      pop_table = reactive(rv$pop_table)
    )
    
    u5_worldpop_rv <- reactiveVal(NULL)
    
    get_u5_worldpop <- function() {
      if (is.null(u5_worldpop_rv())) {
        u5_worldpop_rv(
          load_worldpop_u5_raster(
            t_u1_1to4_file = worldpop_t_u1_1to4_file
          )
        )
      }
      u5_worldpop_rv()
    }
    
    send_paint_message <- function(type, payload = list()) {
      session$sendCustomMessage(
        type,
        c(
          list(
            mapId = map_mod$map_id,
            loadingOverlayId = map_mod$loading_overlay_id,
            readyInputId = map_mod$ready_input_id,
            assignmentsInputId = map_mod$assignments_input_id
          ),
          payload
        )
      )
    }
    
    recompute_population_table <- function(assignments) {
      req(!is.null(rv$grid_sf), length(assignments) == nrow(rv$grid_sf))
      req('u5_pop' %in% names(rv$grid_sf))
      
      df <- data.frame(
        area_name = assignments,
        est_u5_pop = rv$grid_sf$u5_pop,
        stringsAsFactors = FALSE
      ) |>
        dplyr::group_by(area_name) |>
        dplyr::summarise(
          est_u5_pop = round(sum(est_u5_pop, na.rm = TRUE), 0),
          .groups = 'drop'
        )
      
      missing_classes <- setdiff(all_dfa_names, df$area_name)
      if (length(missing_classes) > 0) {
        df <- dplyr::bind_rows(
          df,
          data.frame(
            area_name = missing_classes,
            est_u5_pop = 0,
            stringsAsFactors = FALSE
          )
        )
      }
      
      df <- df |>
        dplyr::mutate(area_name = factor(area_name, levels = all_dfa_names)) |>
        dplyr::arrange(area_name) |>
        dplyr::mutate(area_name = as.character(area_name))
      
      district_total <- round(sum(rv$grid_sf$u5_pop, na.rm = TRUE), 0)
      
      rv$pop_table <- dplyr::bind_rows(
        df,
        data.frame(
          area_name = 'District Total',
          est_u5_pop = district_total,
          stringsAsFactors = FALSE
        )
      )
      
      invisible(NULL)
    }
    
    district_base <- reactive({
      req(isTRUE(district_ready()))
      req(zone(), region(), district())
      
      district_sf <- districts_shp |>
        dplyr::filter(
          zone_name == zone(),
          region_name == region(),
          district_name == district()
        ) |>
        dplyr::select(
          admin_id,
          district_name,
          region_id,
          region_name,
          zone_id,
          zone_name,
          geometry
        )
      
      req(nrow(district_sf) >= 1)
      
      district_sf <- district_sf |>
        dplyr::summarise(
          admin_id = dplyr::first(admin_id),
          district_name = dplyr::first(district_name),
          region_id = dplyr::first(region_id),
          region_name = dplyr::first(region_name),
          zone_id = dplyr::first(zone_id),
          zone_name = dplyr::first(zone_name),
          geometry = sf::st_union(geometry),
          .groups = 'drop'
        ) |>
        sf::st_as_sf()
      
      district_sf <- safe_make_valid(district_sf)
      max_dim_m <- calc_district_max_dim(district_sf)
      
      list(
        district_sf = district_sf,
        max_dim_m = max_dim_m,
        grid_limits = calc_grid_limits(max_dim_m),
        brush_limits = calc_brush_limits(max_dim_m)
      )
    })
    
    observeEvent(district_base(), {
      db <- district_base()
      
      rv$district_base_sf <- db$district_sf
      rv$grid_limits <- db$grid_limits
      rv$brush_limits <- db$brush_limits
      
      controls$set_brush_limits(db$brush_limits)
    }, ignoreInit = TRUE)
    
    observeEvent(controls$brush_minus_click(), {
      bl <- rv$brush_limits
      req(!is.null(bl), !is.null(controls$brush_m()))
      updateSliderInput(
        session,
        'controls-brush_m_ui',
        value = clamp_num(controls$brush_m() - bl$step, bl$min, bl$max)
      )
    })
    
    observeEvent(controls$brush_plus_click(), {
      bl <- rv$brush_limits
      req(!is.null(bl), !is.null(controls$brush_m()))
      updateSliderInput(
        session,
        'controls-brush_m_ui',
        value = clamp_num(controls$brush_m() + bl$step, bl$min, bl$max)
      )
    })
    
    observeEvent(controls$brush_m(), {
      req(tab_active(), isTRUE(district_ready()))
      send_paint_message('paint_set_brush', list(value = controls$brush_m()))
    }, ignoreInit = TRUE)
    
    observeEvent(controls$boundary_only(), {
      req(tab_active(), isTRUE(district_ready()))
      send_paint_message('paint_set_boundary_only', list(value = controls$boundary_only()))
    }, ignoreInit = TRUE)
    
    observeEvent(controls$show_pop_raster(), {
      req(tab_active(), isTRUE(district_ready()))
      send_paint_message('paint_toggle_population', list(show = controls$show_pop_raster()))
    }, ignoreInit = TRUE)
    
    selected_scene <- reactive({
      cat("selected_scene triggered\n")
      
      req(isTRUE(district_ready()))
      cat("passed district_ready\n")
      
      req(zone(), region(), district())
      cat("passed zone/region/district req\n")
      cat("zone:", zone(), "\n")
      cat("region:", region(), "\n")
      cat("district:", district(), "\n")
      
      db <- district_base()
      cat("district_base returned\n")
      cat("district rows:", nrow(db$district_sf), "\n")
      
      district_sf <- db$district_sf
      
      grid_info <- make_paint_grid(district_sf, grid_n = db$grid_limits$value)
      cat("make_paint_grid returned\n")
      
      grid_sf <- grid_info$grid_sf
      cat("grid rows:", nrow(grid_sf), "\n")
      
      req(nrow(grid_sf) > 0)
      cat("passed grid row req\n")
      
      district_seed <- sum(utf8ToInt(district()))
      cat("district_seed computed\n")
      
      start_info <- make_start_assignment(
        grid_sf = grid_sf,
        district_sf = district_sf,
        n_dfa = n_start_dfas,
        seed = district_seed
      )
      cat("make_start_assignment returned\n")
      
      initial_assignments <- as.character(start_info$assignments)
      cat("initial assignments length:", length(initial_assignments), "\n")
      
      seed_pts <- sf::st_transform(start_info$seeds_sf, 4326)
      cat("seed points transformed\n")
      
      seed_coords <- sf::st_coordinates(seed_pts)
      cat("seed coords computed\n")
      
      seed_points_df <- data.frame(
        dfa_name = seed_pts$dfa_name,
        lon = seed_coords[, 1],
        lat = seed_coords[, 2],
        stringsAsFactors = FALSE
      )
      cat("seed_points_df built\n")
      
      seed_points_list <- lapply(seq_len(nrow(seed_points_df)), function(i) {
        list(
          dfa_name = as.character(seed_points_df$dfa_name[i]),
          lon = unname(seed_points_df$lon[i]),
          lat = unname(seed_points_df$lat[i])
        )
      })
      cat("seed_points_list built\n")
      
      touch_list <- sf::st_touches(grid_sf)
      cat("st_touches done\n")
      
      neighbors_list <- lapply(touch_list, as.integer)
      names(neighbors_list) <- as.character(grid_sf$cell_id)
      cat("neighbors_list built\n")
      
      grid_sf_3857 <- sf::st_transform(grid_sf, 3857)
      district_3857 <- sf::st_transform(district_sf, 3857)
      cat("transformed to 3857\n")
      
      cell_bbox <- sf::st_bbox(grid_sf_3857[1, ])
      cell_w <- as.numeric(cell_bbox['xmax'] - cell_bbox['xmin'])
      cell_h <- as.numeric(cell_bbox['ymax'] - cell_bbox['ymin'])
      edge_buffer <- max(cell_w, cell_h) * 0.05
      district_boundary_3857 <- sf::st_boundary(district_3857) |> sf::st_buffer(edge_buffer)
      edge_flag <- lengths(sf::st_intersects(grid_sf_3857, district_boundary_3857)) > 0
      cat("edge flags built\n")
      
      edge_list <- as.list(edge_flag)
      names(edge_list) <- as.character(grid_sf$cell_id)
      
      pop_overlay_sf <- NULL
      if (isTRUE(controls$show_pop_raster())) {
        cat("building population overlay\n")
        pop_overlay_sf <- tryCatch(
          make_population_overlay_sf(district_sf, get_u5_worldpop()),
          error = function(e) {
            cat("population overlay error:", e$message, "\n")
            NULL
          }
        )
      }
      
      cat("selected_scene returning\n")
      
      list(
        district_sf = district_sf,
        grid_sf = grid_sf,
        initial_assignments = initial_assignments,
        neighbors_list = neighbors_list,
        edge_list = edge_list,
        pop_overlay_sf = pop_overlay_sf,
        max_dim_m = grid_info$max_dim_m,
        seed_points = seed_points_list
      )
    })
    
    send_current_scene <- function() {
      req(tab_active())
      req(!is.null(rv$district_sf), !is.null(rv$grid_sf), !is.null(rv$current_assignments))
      
      init_named <- setNames(
        as.list(rv$current_assignments),
        as.character(rv$grid_sf$cell_id)
      )
      
      pop_geojson <- NULL
      if (!is.null(rv$pop_overlay_sf) && nrow(rv$pop_overlay_sf) > 0) {
        pop_geojson <- as_geojson_text(rv$pop_overlay_sf)
      }
      
      saved_sf <- rv$saved_dfa_sf
      if (is.null(saved_sf)) {
        saved_sf <- build_saved_dfa_sf(
          grid_sf = rv$grid_sf,
          assignments = rv$current_assignments,
          district_sf = rv$district_sf
        )
      }
      
      send_paint_message('show_loading')
      
      send_paint_message(
        'paint_load_scene',
        list(
          districtGeojson = as_geojson_text(rv$district_sf),
          gridGeojson = as_geojson_text(rv$grid_sf),
          popGeojson = pop_geojson,
          showPop = controls$show_pop_raster(),
          initialAssignments = init_named,
          dfaColors = as.list(current_fill_colors()),
          activeDfa = controls$active_dfa(),
          neighbors = rv$neighbors_list,
          edgeCells = rv$edge_list,
          brushSize = controls$brush_m(),
          boundaryOnly = controls$boundary_only(),
          seedPoints = rv$seed_points,
          savedGeojson = as_geojson_text(saved_sf)
        )
      )
    }
    
    observeEvent(selected_scene(), {
      sc <- selected_scene()
      
      rv$district_sf <- sc$district_sf
      rv$grid_sf <- sc$grid_sf
      
      if (!'u5_pop' %in% names(rv$grid_sf)) {
        rv$grid_sf$u5_pop <- calculate_grid_cell_population(
          rv$grid_sf,
          get_u5_worldpop()
        )
      }
      
      rv$initial_assignments <- sc$initial_assignments
      rv$current_assignments <- sc$initial_assignments
      rv$saved_dfa_sf <- NULL
      rv$neighbors_list <- sc$neighbors_list
      rv$edge_list <- sc$edge_list
      rv$pop_overlay_sf <- sc$pop_overlay_sf
      rv$pop_table <- NULL
      rv$max_dim_m <- sc$max_dim_m
      rv$seed_points <- sc$seed_points
      
      if (!is.null(rv$brush_limits)) {
        controls$set_brush_limits(rv$brush_limits)
      }
      
      recompute_population_table(sc$initial_assignments)
      
      if (tab_active()) {
        send_current_scene()
      }
    }, ignoreInit = FALSE)
    
    observeEvent(active_tab(), {
      if (tab_active() && isTRUE(district_ready()) && !is.null(rv$grid_sf)) {
        send_current_scene()
      }
    }, ignoreInit = TRUE)
    
    observeEvent(map_mod$map_ready(), {
      if (tab_active()) {
        send_paint_message('hide_loading')
      }
    }, ignoreInit = TRUE)
    
    observeEvent(controls$save_click(), {
      req(isTRUE(district_ready()), tab_active())
      pending_action('save')
      send_paint_message('paint_request_assignments')
    })
    
    observeEvent(controls$reset_click(), {
      req(isTRUE(district_ready()), tab_active())
      req(!is.null(rv$initial_assignments))
      
      rv$current_assignments <- rv$initial_assignments
      rv$saved_dfa_sf <- NULL
      pending_action(NULL)
      
      send_paint_message('paint_reset')
      send_paint_message(
        'paint_set_colors',
        list(
          colors = as.list(current_fill_colors()),
          activeDfa = controls$active_dfa()
        )
      )
      
      recompute_population_table(rv$initial_assignments)
    })
    
    observeEvent(map_mod$assignments(), {
      payload <- map_mod$assignments()
      req(!is.null(payload$assignments))
      req(!is.null(rv$grid_sf), !is.null(rv$district_sf), !is.null(rv$initial_assignments))
      
      js_assignments <- payload$assignments
      
      ordered_assignments <- vapply(
        as.character(rv$grid_sf$cell_id),
        function(cell_id) {
          val <- js_assignments[[cell_id]]
          if (is.null(val) || !nzchar(val)) {
            rv$initial_assignments[as.integer(cell_id)]
          } else {
            as.character(val)
          }
        },
        character(1)
      )
      
      rv$current_assignments <- ordered_assignments
      
      act <- pending_action()
      
      if (identical(act, 'save')) {
        saved <- tryCatch(
          build_saved_dfa_sf(
            grid_sf = rv$grid_sf,
            assignments = ordered_assignments,
            district_sf = rv$district_sf
          ),
          error = function(e) e
        )
        
        if (inherits(saved, 'error')) {
          showNotification(
            paste('Save failed:', saved$message),
            type = 'error',
            duration = 8
          )
          pending_action(NULL)
          return()
        }
        
        rv$saved_dfa_sf <- saved
        send_paint_message('paint_show_saved', list(geojson = as_geojson_text(saved)))
      }
      
      if (identical(act, 'save') || identical(act, 'refresh')) {
        recompute_population_table(ordered_assignments)
      }
      
      pending_action(NULL)
    }, ignoreInit = TRUE)
    
    observeEvent(controls$active_dfa(), {
      req(isTRUE(district_ready()), tab_active())
      req(!is.null(rv$current_assignments), !is.null(rv$grid_sf), !is.null(rv$district_sf))
      
      send_paint_message(
        'paint_set_colors',
        list(
          colors = as.list(current_fill_colors()),
          activeDfa = controls$active_dfa()
        )
      )
      
      pending_action('save')
      send_paint_message('paint_request_assignments')
    }, ignoreInit = TRUE)
    
    list(
      has_scene = reactive(!is.null(rv$grid_sf))
    )
  })
}
