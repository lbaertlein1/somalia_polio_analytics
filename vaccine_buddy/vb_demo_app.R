# =============================================================================
# RAAD Somalia
# Quick-look demo dashboard for "Vaccine Buddy" carrier-tracker telemetry
# (ODK Central form: vaccine_buddy_events_aug2025_nopv2_snid, EMRO project 32)
#
# Pulls data live from ODK Central via ruODK. Credentials are hardcoded
# placeholders below (ODK_USERNAME / ODK_PASSWORD) for quick-demo simplicity
# -- fill them in before running/deploying.
#
# Deploy target: shinyapps.io
# =============================================================================

library(shiny)
library(ruODK)
library(leaflet)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)

# --- ODK Central / form location -------------------------------------------
ODK_BASE_URL <- "https://emro.nafundi.com"
ODK_PROJECT_ID <- 32
ODK_FORM_ID <- "vaccine_buddy_events_aug2025_nopv2_snid"
ODK_TZ <- "Africa/Mogadishu"

# --- Credentials (demo only) -------------------------------------------
# Quick-demo shortcut: fill these in directly. Since this gets deployed to
# shinyapps.io, anyone with the app URL can see the data (not the code/
# credentials themselves, but be aware this is not a secure pattern for
# anything beyond a throwaway demo).
ODK_USERNAME <- "username"
ODK_PASSWORD <- "password"

# Default value for the "Max GPS accuracy to include" slider (meters). Field
# data for this form ranges from ~0.5m to ~645m, with the bulk of fixes under
# 10m and a long tail of poor fixes past ~75m; 50m is a reasonable starting
# line, adjustable live via the slider.
ACCURACY_THRESHOLD_DEFAULT <- 50

# Name of the nested repeat table ("entry" group) as exposed in the OData
# service document for this form (visible in ruODK::odata_service_get()$name
# after connecting). If ODK Central names it differently, update this.
ENTRY_TABLE <- "Submissions.entry"

# --- small helper: find a column by fuzzy name match ------------------------
# Used so the app degrades gracefully (with a clear error) instead of failing
# silently if ruODK's auto-generated column names differ slightly from what
# we saw in a CSV export (e.g. "gps_point-Latitude" vs "gps_point_latitude").
pick_col <- function(df, patterns) {
  nms <- names(df)
  for (p in patterns) {
    hit <- nms[grepl(p, nms, ignore.case = TRUE)]
    if (length(hit) >= 1) return(hit[1])
  }
  NA_character_
}

# --- small helper: aggregate coincident events -----------------------------
# Lid-open/close events and temperature excursions are often placed at the
# exact same carried-forward GPS fix (see NOTE ON GEOLOCATION below), so
# several events can land on the identical point. Rather than nudging them
# apart (which misrepresents location), we count how many events share a
# point and size the marker accordingly, then layer marker types so the
# most important one (temperature excursions) always renders on top.
#
# Rounded lat/lon are used only as a grouping KEY (to detect "same point");
# the marker itself is plotted at the exact original coordinate of one of
# the grouped rows (via first()), so it lands precisely on the real fix
# rather than on a rounded approximation of it.
aggregate_events <- function(df, extra_group = character(0), value_col = NULL) {
  df$lat_key <- round(df$active_lat, 5)
  df$lon_key <- round(df$active_lon, 5)
  grp_cols <- c("device_id", "lat_key", "lon_key", extra_group)
  df %>%
    group_by(across(all_of(grp_cols))) %>%
    summarise(
      n = dplyr::n(),
      active_lat = dplyr::first(active_lat),
      active_lon = dplyr::first(active_lon),
      first_ts = min(ts, na.rm = TRUE),
      last_ts = max(ts, na.rm = TRUE),
      min_val = if (!is.null(value_col)) min(.data[[value_col]], na.rm = TRUE) else NA_real_,
      max_val = if (!is.null(value_col)) max(.data[[value_col]], na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    select(-lat_key, -lon_key)
}

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      html { font-size: 13px; }
      h2 { font-size: 1.5rem; margin-top: 4px; margin-bottom: 2px; }
      h5 { font-size: 0.8rem; color: #555; margin-bottom: 4px; text-transform: uppercase; letter-spacing: 0.02em; }
      hr { margin: 8px 0; }
      .well { padding: 12px; }
      .form-group { margin-bottom: 8px; }
      #device_table_wrap table.dataTable { font-size: 10px; }
      #device_table_wrap table.dataTable td {
        padding: 2px 4px !important;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      #device_table_wrap table.dataTable th {
        padding: 2px 4px !important;
        font-size: 10px;
        white-space: normal;
        line-height: 1.15;
        vertical-align: bottom;
      }
    "))
  ),
  
  titlePanel("RAAD Somalia"),
  tags$p(
    style = "color:#666; margin-top:-6px; margin-bottom:8px; font-size:0.85rem;",
    "Vaccine carrier tracker telemetry \u2014 quick-look demo (pulled live from ODK Central)"
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      actionButton("connect_btn", "Load / refresh data", class = "btn-primary", width = "100%"),
      uiOutput("status_box"),
      hr(),
      
      dateRangeInput("date_range", "Date range", start = NULL, end = NULL, width = "100%"),
      hr(),
      
      h5("TEMPERATURE EXCURSION THRESHOLDS (\u00b0C)"),
      sliderInput("temp_range", NULL, min = -10, max = 50, value = c(20, 35), step = 1, width = "100%"),
      hr(),
      
      h5("MAP LAYERS"),
      checkboxInput("show_lid", "Lid open/close events", value = TRUE),
      checkboxInput("show_excursion", "Temperature excursions", value = TRUE),
      hr(),
      
      h5("GPS ACCURACY FILTER"),
      sliderInput("accuracy_threshold", "Max accuracy to include (m)",
                  min = 0, max = 1000, value = ACCURACY_THRESHOLD_DEFAULT, step = 5, width = "100%"),
      hr(),
      
      fluidRow(
        column(6, actionButton("show_all_btn", "Show all", width = "100%")),
        column(6, actionButton("hide_all_btn", "Hide all", width = "100%"))
      )
    ),
    
    mainPanel(
      width = 9,
      uiOutput("headline_ui"),
      tags$p(
        style = "color:#888; font-size:0.75rem; margin-top:-4px;",
        "Marker size reflects how many events share a point (common since events are placed at the nearest GPS fix). Excursions render on top of lid events."
      ),
      fluidRow(
        column(6, leafletOutput("map", height = 520)),
        column(6,
               div(id = "device_table_wrap", DTOutput("device_table"))
        )
      )
    )
  )
)



# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    main = NULL,
    entry_raw = NULL,
    error = NULL,
    connected = FALSE
  )
  
  # named logical vector: device_id -> currently shown on map?
  visible_devices <- reactiveVal(setNames(logical(0), character(0)))
  
  # --- Connect & pull ---------------------------------------------------
  pull_data <- function() {
    withProgress(message = "Pulling data from ODK Central...", value = 0.1, {
      
      tryCatch({
        
        svc_url <- sprintf("%s/v1/projects/%s/forms/%s.svc",
                           ODK_BASE_URL, ODK_PROJECT_ID, ODK_FORM_ID)
        
        ruODK::ru_setup(
          svc = svc_url,
          un  = ODK_USERNAME,
          pw  = ODK_PASSWORD,
          tz  = ODK_TZ
        )
        
        incProgress(0.3, detail = "Fetching main submissions table")
        main_df <- ruODK::odata_submission_get()
        
        incProgress(0.3, detail = "Fetching entry (repeat) table")
        entry_df <- ruODK::odata_submission_get(table = ENTRY_TABLE)
        
        incProgress(0.2, detail = "Done")
        
        rv$main <- main_df
        rv$entry_raw <- entry_df
        rv$connected <- TRUE
        rv$error <- NULL
        
      }, error = function(e) {
        rv$connected <- FALSE
        rv$error <- conditionMessage(e)
      })
    })
  }
  
  observeEvent(input$connect_btn, { pull_data() })
  
  # Auto-load once when the app starts
  pull_data()
  
  output$status_box <- renderUI({
    if (!is.null(rv$error)) {
      tags$div(style = "color:#b00; margin-top:8px;",
               paste("Connection/pull failed:", rv$error))
    } else if (isTRUE(rv$connected)) {
      tags$div(style = "color:#0a0; margin-top:8px;", "Connected \u2014 data loaded.")
    } else {
      tags$div(style = "color:#666; margin-top:8px;", "Not connected yet.")
    }
  })
  
  # --- Clean / join main + entry tables ----------------------------------
  # This is where we resolve ruODK's actual column names. If a pull
  # succeeds but this step errors, the message below will list the actual
  # column names found so you can adjust the `pick_col` patterns above.
  entry_clean <- reactive({
    req(rv$main, rv$entry_raw)
    
    main <- rv$main
    entry <- rv$entry_raw
    
    id_col_main       <- pick_col(main, c("^id$", "instance_id", "uuid"))
    parent_col_entry  <- pick_col(entry, c("submissions_id", "parent_key", "^parent"))
    
    # GPS coordinates: ODK Central's OData feed encodes geopoints as GeoJSON,
    # which ruODK flattens to coordinates_1/2/3 in [longitude, latitude,
    # altitude] order (GeoJSON convention) rather than separate lat/lon
    # columns. Prefer flat lat/lon names if they ever appear; otherwise use
    # the GeoJSON coordinate columns.
    lat_col <- pick_col(entry, c("^lat$", "^latitude$", "_latitude$"))
    lon_col <- pick_col(entry, c("^lon$", "^longitude$", "_longitude$"))
    geo_lon_col <- pick_col(entry, c("coordinates_1$"))
    geo_lat_col <- pick_col(entry, c("coordinates_2$"))
    if (is.na(lat_col) && !is.na(geo_lat_col)) lat_col <- geo_lat_col
    if (is.na(lon_col) && !is.na(geo_lon_col)) lon_col <- geo_lon_col
    
    ts_col            <- pick_col(entry, c("^timestamp$", "entry_timestamp"))
    temp_col          <- pick_col(entry, c("^temp_c$", "temp"))
    lidopen_col       <- pick_col(entry, c("lid_open"))
    lidclose_col      <- pick_col(entry, c("lid_close"))
    battery_col       <- pick_col(entry, c("battery"))
    accuracy_col      <- pick_col(entry, c("^gps_accuracy_m$", "properties_accuracy", "accuracy"))
    
    validate(need(!is.na(id_col_main),
                  paste("Could not find a submission id column in the main table. Columns found:",
                        paste(names(main), collapse = ", "))))
    validate(need(!is.na(parent_col_entry),
                  paste("Could not find the parent-link column in the entry table. Columns found:",
                        paste(names(entry), collapse = ", "))))
    validate(need(!is.na(lat_col) && !is.na(lon_col),
                  paste("Could not find latitude/longitude columns in the entry table. Columns found:",
                        paste(names(entry), collapse = ", "))))
    validate(need(!is.na(ts_col),
                  paste("Could not find a timestamp column in the entry table. Columns found:",
                        paste(names(entry), collapse = ", "))))
    
    # Rename to stable internal names (base R rename, avoids tidy-eval quirks)
    names(main)[names(main) == id_col_main] <- "parent_id"
    names(entry)[names(entry) == parent_col_entry] <- "parent_id"
    names(entry)[names(entry) == lat_col] <- "lat"
    names(entry)[names(entry) == lon_col] <- "lon"
    names(entry)[names(entry) == ts_col] <- "ts"
    if (!is.na(temp_col))     names(entry)[names(entry) == temp_col]     <- "temp_c"
    if (!is.na(lidopen_col))  names(entry)[names(entry) == lidopen_col]  <- "lid_open"
    if (!is.na(lidclose_col)) names(entry)[names(entry) == lidclose_col] <- "lid_close"
    if (!is.na(battery_col))  names(entry)[names(entry) == battery_col]  <- "battery_percent"
    if (!is.na(accuracy_col)) names(entry)[names(entry) == accuracy_col] <- "gps_accuracy_m"
    
    if (!"temp_c" %in% names(entry)) entry$temp_c <- NA_real_
    if (!"lid_open" %in% names(entry)) entry$lid_open <- NA_character_
    if (!"lid_close" %in% names(entry)) entry$lid_close <- NA_character_
    if (!"battery_percent" %in% names(entry)) entry$battery_percent <- NA_real_
    if (!"gps_accuracy_m" %in% names(entry)) entry$gps_accuracy_m <- NA_real_
    
    device_col <- pick_col(main, c("^device_id$", "device"))
    team_col   <- pick_col(main, c("^team_name$", "team"))
    validate(need(!is.na(device_col),
                  paste("Could not find device_id column in the main table. Columns found:",
                        paste(names(main), collapse = ", "))))
    
    main_small <- main[, c("parent_id", device_col, if (!is.na(team_col)) team_col)]
    names(main_small)[names(main_small) == device_col] <- "device_id"
    if (!is.na(team_col)) names(main_small)[names(main_small) == team_col] <- "team_name"
    
    merged <- entry %>%
      left_join(main_small, by = "parent_id")
    
    if (!"team_name" %in% names(merged)) merged$team_name <- NA_character_
    
    # Make sure timestamp is a real datetime
    if (!inherits(merged$ts, "POSIXct")) {
      merged$ts <- lubridate::ymd_hms(as.character(merged$ts), tz = ODK_TZ, quiet = TRUE)
    }
    
    # Coordinates/temperature/battery must be numeric
    merged$lat <- suppressWarnings(as.numeric(merged$lat))
    merged$lon <- suppressWarnings(as.numeric(merged$lon))
    merged$temp_c <- suppressWarnings(as.numeric(merged$temp_c))
    merged$battery_percent <- suppressWarnings(as.numeric(merged$battery_percent))
    merged$gps_accuracy_m <- suppressWarnings(as.numeric(merged$gps_accuracy_m))
    
    # NOTE ON GEOLOCATION:
    # GPS fixes only arrive on a subset of log rows; lid-open/close events and
    # most temperature readings land on separate rows with no coordinates of
    # their own. We carry forward each device's most recent GPS fix (sorted
    # by time) so those events can still be placed on the map approximately.
    # The route LINE below uses only rows with a genuine GPS fix.
    #
    # lat/lon here are carried forward from ANY fix, regardless of accuracy.
    # lat_raw/lon_raw keep the un-filled originals (NA where there's no fix)
    # so entry_thresholded() below can redo the carry-forward using only
    # fixes at or better than the user's chosen accuracy threshold.
    merged <- merged %>%
      filter(!is.na(ts), !is.na(device_id)) %>%
      arrange(device_id, ts) %>%
      mutate(
        has_gps_fix = !is.na(lat) & !is.na(lon),
        lat_raw = lat,
        lon_raw = lon
      ) %>%
      group_by(device_id) %>%
      tidyr::fill(lat, lon, .direction = "down") %>%
      ungroup()
    
    merged
  })
  
  # --- Apply the GPS accuracy threshold (slider) --------------------------
  # Recomputes which fixes count as "good" and carries those forward,
  # separately from the "any fix" version kept in entry_clean(). Done on the
  # full dataset (not yet date-filtered) so carry-forward can reach back
  # before the visible date range if needed.
  entry_thresholded <- reactive({
    df <- entry_clean()
    req(input$accuracy_threshold)
    
    df %>%
      mutate(
        is_good_fix = has_gps_fix & (is.na(gps_accuracy_m) | gps_accuracy_m <= input$accuracy_threshold),
        lat_good = ifelse(is_good_fix, lat_raw, NA_real_),
        lon_good = ifelse(is_good_fix, lon_raw, NA_real_)
      ) %>%
      arrange(device_id, ts) %>%
      group_by(device_id) %>%
      tidyr::fill(lat_good, lon_good, .direction = "down") %>%
      ungroup()
  })
  
  # --- Set date range bounds once data loads -----------------------------
  # Min/max reflect the full pulled dataset (so the picker can navigate to
  # any date with data), but the initial start/end both default to July 9.
  observeEvent(entry_clean(), {
    df <- entry_clean()
    if (nrow(df) == 0) return()
    rng <- range(df$ts, na.rm = TRUE)
    default_date <- as.Date("2026-07-09")
    updateDateRangeInput(session, "date_range",
                         start = default_date, end = default_date,
                         min = as.Date(rng[1]), max = as.Date(rng[2]))
  })
  
  # --- Filter to selected date range --------------------------------------
  entry_filtered <- reactive({
    df <- entry_thresholded()
    req(input$date_range)
    if (any(is.na(input$date_range))) return(df)
    start <- as.POSIXct(input$date_range[1], tz = ODK_TZ)
    end   <- as.POSIXct(input$date_range[2] + 1, tz = ODK_TZ)  # inclusive of end day
    df %>% filter(ts >= start, ts < end)
  })
  
  # Aliases the accuracy-filtered lat/lon/fix-flag to stable "active_*"
  # names so the map + table code below doesn't need to know about the
  # thresholding detail.
  entry_active <- reactive({
    df <- entry_filtered()
    df$active_lat <- df$lat_good
    df$active_lon <- df$lon_good
    df$active_fix <- df$is_good_fix
    df
  })
  
  # --- Device list for the currently selected date range -------------------
  # Deliberately built from entry_clean() + date range only (not
  # entry_thresholded()/entry_filtered()), so it does NOT recompute every
  # time the accuracy slider moves -- only when the date range changes.
  date_filtered_devices <- reactive({
    df <- entry_clean()
    req(input$date_range)
    if (any(is.na(input$date_range))) return(sort(unique(df$device_id)))
    start <- as.POSIXct(input$date_range[1], tz = ODK_TZ)
    end   <- as.POSIXct(input$date_range[2] + 1, tz = ODK_TZ)
    sort(unique(df$device_id[df$ts >= start & df$ts < end]))
  })
  
  # --- Initialize / update device visibility ------------------------------
  # Tied to date_filtered_devices() so the default (top 3) always reflects
  # devices actually present in the current date range, and resets whenever
  # that set changes (e.g. the date range is adjusted) rather than carrying
  # forward stale selections from a previous filter.
  observeEvent(date_filtered_devices(), {
    devs <- date_filtered_devices()
    new_vec <- setNames(rep(FALSE, length(devs)), devs)
    if (length(devs) > 0) new_vec[seq_len(min(3, length(devs)))] <- TRUE
    visible_devices(new_vec)
  }, ignoreInit = FALSE)
  
  observeEvent(input$device_toggle, {
    cur <- visible_devices()
    cur[[input$device_toggle$device]] <- isTRUE(input$device_toggle$checked)
    visible_devices(cur)
  })
  
  observeEvent(input$show_all_btn, {
    cur <- visible_devices()
    if (length(cur) > 0) visible_devices(setNames(rep(TRUE, length(cur)), names(cur)))
  })
  
  observeEvent(input$hide_all_btn, {
    cur <- visible_devices()
    if (length(cur) > 0) visible_devices(setNames(rep(FALSE, length(cur)), names(cur)))
  })
  
  # --- Per-device summary table --------------------------------------------
  device_summary <- reactive({
    df <- entry_active()
    req(nrow(df) > 0)
    
    df %>%
      mutate(
        is_excursion = !is.na(temp_c) & (temp_c < input$temp_range[1] | temp_c > input$temp_range[2])
      ) %>%
      group_by(device_id) %>%
      summarise(
        team_name = dplyr::first(na.omit(team_name)),
        start_time = min(ts, na.rm = TRUE),
        end_time = max(ts, na.rm = TRUE),
        n_pings = sum(active_fix, na.rm = TRUE),
        n_lid_open = sum(lid_open == "y", na.rm = TRUE),
        n_excursions = sum(is_excursion, na.rm = TRUE),
        min_temp = round(min(temp_c, na.rm = TRUE), 1),
        max_temp = round(max(temp_c, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      mutate(
        duration_hr = round(as.numeric(difftime(end_time, start_time, units = "hours")), 1),
        start_time = format(start_time, "%m-%d %H:%M"),
        end_time = format(end_time, "%m-%d %H:%M")
      ) %>%
      arrange(device_id)
  })
  
  # --- Headline indicator ---------------------------------------------------
  output$headline_ui <- renderUI({
    df <- entry_filtered()
    if (is.null(df) || nrow(df) == 0) {
      return(tags$p(style = "color:#666;", "Connect and pull data to begin."))
    }
    n_total <- length(unique(df$device_id))
    vis <- visible_devices()
    n_shown <- sum(vis, na.rm = TRUE)
    tags$p(
      style = "font-size:16px;",
      tags$b(n_total), " device(s) reporting in the selected date range \u2014 ",
      tags$b(n_shown), " currently shown on the map."
    )
  })
  
  # --- Device table with per-row map-visibility toggle ----------------------
  output$device_table <- renderDT({
    summ <- device_summary()
    vis <- visible_devices()
    
    summ$Show <- sprintf(
      '<input type="checkbox" class="device-toggle" data-device="%s" %s />',
      summ$device_id,
      ifelse(vis[summ$device_id], "checked", "")
    )
    
    display <- summ[, c("Show", "device_id", "start_time", "end_time", "duration_hr",
                        "n_pings", "n_lid_open", "n_excursions", "min_temp", "max_temp")]
    names(display) <- c("Show", "Device", "Start", "End", "Hrs",
                        "GPS Pings", "Lid Opens", "Excursions", "Temp Min", "Temp Max")
    
    datatable(
      display,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      class = "compact stripe",
      caption = "Devices (check to show/hide on map) \u2014 Temp Min/Max in \u00b0C",
      options = list(
        pageLength = 50,
        dom = "t",
        ordering = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(orderable = FALSE, targets = 0, width = "18px"),
          list(targets = 1, width = "44px"),
          list(targets = c(2, 3), width = "58px"),
          list(targets = 4, width = "28px"),
          list(targets = 5, width = "36px"),
          list(targets = c(6, 7), width = "28px"),
          list(targets = c(8, 9), width = "36px")
        )
      ),
      callback = JS(
        "table.on('change', 'input.device-toggle', function() {",
        "  var device = $(this).data('device');",
        "  var checked = $(this).is(':checked');",
        "  Shiny.setInputValue('device_toggle',",
        "    {device: device, checked: checked, nonce: Math.random()},",
        "    {priority: 'event'});",
        "});"
      )
    )
  })
  
  # --- Map -------------------------------------------------------------------
  output$map <- renderLeaflet({
    df <- entry_active()
    validate(need(nrow(df) > 0, "No data for the selected date range yet."))
    
    vis <- visible_devices()
    shown_devices <- names(vis)[vis]
    df <- df %>% filter(device_id %in% shown_devices)
    
    validate(need(nrow(df) > 0, "No devices currently toggled on \u2014 check a box in the table."))
    
    # Devices actually present after the date range + accuracy threshold are
    # applied (a "selected" device can still have zero rows here, e.g. if it
    # didn't report any good fixes in the current window) -- the legend
    # should only list devices that actually have something drawn.
    present_devices <- sort(unique(df$device_id))
    
    all_devices <- sort(unique(entry_active()$device_id))
    pal <- colorFactor(palette = "Set1", domain = all_devices)
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
      addLayersControl(
        baseGroups = c("Light", "OpenStreetMap", "Satellite", "Dark"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topright"
      )
    
    # --- route lines: one per visible device, real GPS fixes only, in time order
    # A thin line connects the pings so the path is readable, with a small
    # dot at every individual GPS fix so ping density/frequency is visible too.
    for (d in shown_devices) {
      route_pts <- df %>%
        filter(device_id == d, active_fix) %>%
        arrange(ts)
      if (nrow(route_pts) >= 2) {
        map <- map %>%
          addPolylines(
            data = route_pts, lng = ~active_lon, lat = ~active_lat,
            color = pal(d), weight = 1.5, opacity = 0.7,
            group = "routes"
          )
      }
      if (nrow(route_pts) >= 1) {
        map <- map %>%
          addCircleMarkers(
            data = route_pts, lng = ~active_lon, lat = ~active_lat,
            radius = 2, color = pal(d), stroke = FALSE, fillOpacity = 0.9,
            popup = ~sprintf("Device %s<br>%s", device_id, format(ts, "%Y-%m-%d %H:%M")),
            group = "routes"
          )
      }
    }
    
    # --- lid open/close markers (location = carried-forward GPS fix)
    # Drawn before excursions so excursions always render on top.
    if (isTRUE(input$show_lid)) {
      lid_open_pts <- df %>% filter(lid_open == "y", !is.na(active_lat), !is.na(active_lon))
      lid_close_pts <- df %>% filter(lid_close == "y", !is.na(active_lat), !is.na(active_lon))
      
      if (nrow(lid_open_pts) > 0) {
        lid_open_agg <- aggregate_events(lid_open_pts) %>%
          mutate(radius = pmin(18, 8 + 2.5 * sqrt(n)))
        map <- map %>% addCircleMarkers(
          data = lid_open_agg, lng = ~active_lon, lat = ~active_lat,
          radius = ~radius, color = "#2ca02c", stroke = FALSE, fillOpacity = 0.5,
          popup = ~sprintf("Device %s<br>Lid OPEN x%d<br>%s%s", device_id, n,
                           format(first_ts, "%Y-%m-%d %H:%M"),
                           ifelse(n > 1, paste0(" to ", format(last_ts, "%Y-%m-%d %H:%M")), "")),
          group = "lid"
        )
      }
      if (nrow(lid_close_pts) > 0) {
        lid_close_agg <- aggregate_events(lid_close_pts) %>%
          mutate(radius = pmin(16, 7 + 2.5 * sqrt(n)))
        map <- map %>% addCircleMarkers(
          data = lid_close_agg, lng = ~active_lon, lat = ~active_lat,
          radius = ~radius, color = "#ff7f0e", stroke = FALSE, fillOpacity = 0.5,
          popup = ~sprintf("Device %s<br>Lid CLOSE x%d<br>%s%s", device_id, n,
                           format(first_ts, "%Y-%m-%d %H:%M"),
                           ifelse(n > 1, paste0(" to ", format(last_ts, "%Y-%m-%d %H:%M")), "")),
          group = "lid"
        )
      }
    }
    
    # --- temperature excursion markers (drawn last -> always on top, bold outline)
    if (isTRUE(input$show_excursion)) {
      excursion_pts <- df %>%
        filter(!is.na(temp_c), !is.na(active_lat), !is.na(active_lon),
               temp_c < input$temp_range[1] | temp_c > input$temp_range[2]) %>%
        mutate(kind = ifelse(temp_c < input$temp_range[1], "Low", "High"))
      
      if (nrow(excursion_pts) > 0) {
        excursion_agg <- aggregate_events(excursion_pts, extra_group = "kind", value_col = "temp_c") %>%
          mutate(radius = pmin(14, 5 + 2 * sqrt(n)))
        map <- map %>% addCircleMarkers(
          data = excursion_agg, lng = ~active_lon, lat = ~active_lat,
          radius = ~radius, color = "#000000", weight = 1.5,
          fillColor = "#d62728", stroke = TRUE, fillOpacity = 0.9,
          popup = ~sprintf("Device %s<br>%s excursion x%d (%.1f\u2013%.1f\u00b0C)<br>%s%s",
                           device_id, kind, n, min_val, max_val,
                           format(first_ts, "%Y-%m-%d %H:%M"),
                           ifelse(n > 1, paste0(" to ", format(last_ts, "%Y-%m-%d %H:%M")), "")),
          group = "excursions"
        )
      }
    }
    
    # --- legends
    map <- map %>%
      addLegend(
        position = "bottomleft",
        pal = pal, values = present_devices,
        title = "Device (route)",
        opacity = 0.9
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#2ca02c", "#ff7f0e", "#d62728"),
        labels = c("Lid open", "Lid close", "Temp excursion"),
        title = "Event markers",
        opacity = 1
      )
    
    map
  })
}

shinyApp(ui, server)
