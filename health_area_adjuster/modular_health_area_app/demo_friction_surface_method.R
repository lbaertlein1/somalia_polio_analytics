suppressPackageStartupMessages({
  library(shiny)
  library(sf)
  library(terra)
  library(dplyr)
  library(leaflet)
  library(RColorBrewer)
  library(htmltools)
})

# -----------------------------------------------------------------------------
# FILES
# -----------------------------------------------------------------------------
districts_file <- "data/districts_shp.Rds"
friction_index_file <- "data/friction/district_standardized/district_friction_index.csv"
population_file <- "data/som_u5_population_2025_100m.tif"

# -----------------------------------------------------------------------------
# HELPERS
# -----------------------------------------------------------------------------
safe_read_districts <- function(path) {
  x <- readRDS(path)
  if (!inherits(x, "sf")) {
    stop("districts_file must contain an sf object.")
  }
  
  sf::st_as_sf(x) |>
    sf::st_make_valid()
}

build_district_template <- function(district_sf, friction_r) {
  district_sf <- st_transform(district_sf, crs(friction_r))
  district_v  <- vect(district_sf)
  
  template <- crop(friction_r, district_v)
  template <- mask(template, district_v)
  
  template
}

safe_read_friction_index <- function(path) {
  if (!file.exists(path)) {
    stop("friction_index_file not found: ", path)
  }
  
  x <- read.csv(path, stringsAsFactors = FALSE)
  
  required_cols <- c("zone_name", "region_name", "district_name", "friction_file")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop(
      "friction index is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  x
}

safe_read_population <- function(path) {
  if (!file.exists(path)) {
    stop("Population raster not found: ", path)
  }
  terra::rast(path)
}

get_selected_district <- function(districts_shp, zone, region, district) {
  out <- districts_shp |>
    dplyr::filter(
      zone_name == zone,
      region_name == region,
      district_name == district
    )
  
  if (nrow(out) == 0) {
    return(NULL)
  }
  
  out |>
    dplyr::summarise(
      zone_name = dplyr::first(zone_name),
      region_name = dplyr::first(region_name),
      district_name = dplyr::first(district_name),
      geometry = sf::st_union(geometry),
      .groups = "drop"
    ) |>
    sf::st_as_sf() |>
    sf::st_make_valid()
}

get_selected_friction_file <- function(friction_index, zone, region, district) {
  out <- friction_index |>
    dplyr::filter(
      zone_name == zone,
      region_name == region,
      district_name == district
    )
  
  if (nrow(out) == 0) {
    return(NULL)
  }
  
  out$friction_file[1]
}

make_area_palette <- function(n) {
  if (n <= 8) {
    RColorBrewer::brewer.pal(max(3, n), "Set2")[seq_len(n)]
  } else {
    grDevices::hcl.colors(n, "Set 2")
  }
}

# -----------------------------------------------------------------------------
# DATA PREP
# -----------------------------------------------------------------------------
aggregate_district_inputs <- function(district_sf, pop_r, friction_r, agg_factor = 5) {
  district_sf <- sf::st_transform(district_sf, terra::crs(friction_r))
  district_v <- terra::vect(district_sf)
  
  friction_d <- terra::crop(friction_r, district_v)
  friction_d <- terra::mask(friction_d, district_v)
  
  friction_vals <- terra::values(friction_d, mat = FALSE)
  if (all(is.na(friction_vals))) {
    stop("District does not overlap friction raster after CRS transform.")
  }
  
  max_val <- max(friction_vals, na.rm = TRUE)
  
  friction_d <- terra::ifel(
    is.na(friction_d),
    max_val * 10,
    friction_d
  )
  
  if (!terra::same.crs(pop_r, friction_r)) {
    pop_r <- terra::project(pop_r, friction_r, method = "bilinear")
  }
  pop_r <- terra::resample(pop_r, friction_r, method = "bilinear")
  
  pop_d <- terra::crop(pop_r, district_v)
  pop_d <- terra::mask(pop_d, district_v)
  
  pop_vals <- terra::values(pop_d, mat = FALSE)
  if (all(is.na(pop_vals))) {
    stop("District does not overlap population raster after reprojection.")
  }
  
  max_fact_row <- max(1, floor(terra::nrow(pop_d) / 5))
  max_fact_col <- max(1, floor(terra::ncol(pop_d) / 5))
  safe_fact <- max(1, min(as.integer(agg_factor), max_fact_row, max_fact_col))
  
  pop_a <- terra::aggregate(pop_d, fact = safe_fact, fun = sum, na.rm = TRUE)
  friction_a <- terra::aggregate(friction_d, fact = safe_fact, fun = mean, na.rm = TRUE)
  
  # Keep the full district template, not only places with non-NA population,
  # so allocation can cover the whole district footprint represented in the raster.
  district_template_a <- terra::aggregate(
    !is.na(friction_d),
    fact = safe_fact,
    fun = max,
    na.rm = TRUE
  )
  
  friction_a <- terra::mask(friction_a, district_template_a, maskvalues = 0, updatevalue = NA)
  pop_a <- terra::mask(pop_a, district_template_a, maskvalues = 0, updatevalue = NA)
  
  names(pop_a) <- "u5_pop"
  names(friction_a) <- "friction"
  
  list(
    pop = pop_a,
    friction = friction_a,
    pop_full = pop_d,
    friction_full = friction_d,
    agg_factor_used = safe_fact
  )
}

generate_placeholder_facilities <- function(district_sf, pop_r) {
  district_sf <- sf::st_transform(district_sf, terra::crs(pop_r))
  district_v <- terra::vect(district_sf)
  
  pop_d <- terra::crop(pop_r, district_v)
  pop_d <- terra::mask(pop_d, district_v)
  
  vals <- terra::values(pop_d, mat = FALSE)
  total_u5 <- sum(vals, na.rm = TRUE)
  
  if (!is.finite(total_u5) || total_u5 <= 0) {
    total_u5 <- 1
  }
  
  n_facilities <- ceiling(total_u5 / 20000)
  n_facilities <- max(1, n_facilities)
  
  valid_cells <- which(
    is.finite(vals) &
      !is.na(vals) &
      vals > 0
  )
  
  if (length(valid_cells) == 0) {
    valid_cells <- which(!is.na(vals))
  }
  
  if (length(valid_cells) == 0) {
    stop("No valid cells in district population raster.")
  }
  
  n_facilities <- min(n_facilities, length(valid_cells))
  
  weights <- vals[valid_cells]
  weights[!is.finite(weights)] <- 0
  
  if (sum(weights) <= 0) {
    weights <- rep(1, length(valid_cells))
  }
  
  sampled_cells <- sample(
    valid_cells,
    size = n_facilities,
    prob = weights,
    replace = FALSE
  )
  
  coords <- terra::xyFromCell(pop_d, sampled_cells)
  coords <- as.matrix(coords)
  
  if (nrow(coords) != n_facilities) {
    stop("Failed to derive coordinate matrix for sampled facility cells.")
  }
  
  facilities_df <- data.frame(
    facility_id = seq_len(n_facilities),
    u5_population = rep(total_u5, n_facilities),
    x = coords[, 1],
    y = coords[, 2]
  )
  
  facilities_sf <- sf::st_as_sf(
    facilities_df,
    coords = c("x", "y"),
    crs = terra::crs(pop_d)
  )
  
  facilities_sf
}

get_district_facilities <- function(district_sf, pop_r) {
  message("Using population-based sampled facilities.")
  generate_placeholder_facilities(
    district_sf = district_sf,
    pop_r = pop_r
  )
}

facility_points_to_seed_cells <- function(facilities_sf, template_r) {
  fac <- sf::st_transform(facilities_sf, terra::crs(template_r))
  coords <- sf::st_coordinates(fac)
  
  cells <- terra::cellFromXY(template_r, coords)
  cells <- unique(cells[!is.na(cells)])
  
  if (length(cells) == 0) {
    stop("No facility points fall on valid raster cells.")
  }
  
  cells
}

seed_cells_to_sf <- function(cell_ids, template_r) {
  xy <- terra::xyFromCell(template_r, cell_ids)
  
  sf::st_as_sf(
    data.frame(
      id = seq_along(cell_ids),
      cell = cell_ids,
      x = xy[, 1],
      y = xy[, 2]
    ),
    coords = c("x", "y"),
    crs = terra::crs(template_r)
  )
}

compute_cost_allocation <- function(seed_cells, friction_r) {
  seeds_sf <- seed_cells_to_sf(seed_cells, friction_r)
  seeds_vect <- terra::vect(seeds_sf)
  
  cost_stack <- vector("list", length(seed_cells))
  
  for (i in seq_along(seed_cells)) {
    origin_r <- terra::rasterize(
      seeds_vect[i],
      friction_r,
      field = 0,
      background = NA
    )
    
    combined <- terra::ifel(!is.na(origin_r), 0, friction_r)
    cost_i <- terra::gridDist(combined, target = 0)
    names(cost_i) <- paste0("seed_", i)
    cost_stack[[i]] <- cost_i
  }
  
  cost_rast <- terra::rast(cost_stack)
  allocation <- terra::which.min(cost_rast)
  names(allocation) <- "health_area"
  
  # Use the function argument, not an out-of-scope object
  allocation <- terra::mask(allocation, friction_r)
  
  list(
    allocation = allocation,
    cost_stack = cost_rast,
    seeds_sf = seeds_sf
  )
}

summarize_area_population <- function(allocation_r, pop_r, n_seeds) {
  alloc_vals <- terra::values(allocation_r, mat = FALSE)
  pop_vals <- terra::values(pop_r, mat = FALSE)
  
  ok <- is.finite(alloc_vals) & !is.na(alloc_vals) & is.finite(pop_vals) & !is.na(pop_vals)
  alloc_vals <- alloc_vals[ok]
  pop_vals <- pop_vals[ok]
  
  out <- data.frame(
    id = seq_len(n_seeds),
    children = 0,
    n_cells = 0
  )
  
  if (length(alloc_vals) > 0) {
    pop_sum <- tapply(pop_vals, alloc_vals, sum, na.rm = TRUE)
    cell_sum <- tapply(pop_vals, alloc_vals, length)
    
    idx1 <- as.integer(names(pop_sum))
    idx2 <- as.integer(names(cell_sum))
    
    out$children[idx1] <- as.numeric(pop_sum)
    out$n_cells[idx2] <- as.numeric(cell_sum)
  }
  
  out
}

relabel_allocation_polygons <- function(allocation_r) {
  alloc_poly <- terra::as.polygons(allocation_r, values = TRUE, na.rm = TRUE) |>
    sf::st_as_sf() |>
    sf::st_make_valid()
  
  names(alloc_poly)[1] <- "health_area"
  
  alloc_poly <- alloc_poly |>
    dplyr::group_by(health_area) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
    sf::st_make_valid()
  
  alloc_poly$health_area <- paste("Health Area", alloc_poly$health_area)
  alloc_poly
}

check_no_gaps <- function(allocation_r, template_r) {
  alloc_vals <- terra::values(allocation_r, mat = FALSE)
  template_vals <- terra::values(template_r, mat = FALSE)
  
  inside_template <- !is.na(template_vals)
  
  if (any(is.na(alloc_vals[inside_template]))) {
    stop("Allocation contains gaps inside allocation template.")
  }
}

# -----------------------------------------------------------------------------
# CORE ALLOCATION
# -----------------------------------------------------------------------------
make_capacity_balanced_health_areas <- function(
    district_sf,
    friction_r,
    pop_r,
    target_children = 2000,
    tolerance = 0.20,
    agg_factor = 5,
    max_iter = 15
) {
  stopifnot(inherits(district_sf, "sf"), nrow(district_sf) == 1)
  
  agg <- aggregate_district_inputs(
    district_sf = district_sf,
    pop_r = pop_r,
    friction_r = friction_r,
    agg_factor = agg_factor
  )
  
  pop_d <- agg$pop
  friction_d <- agg$friction
  
  district_facilities <- get_district_facilities(
    district_sf = district_sf,
    pop_r = agg$pop_full
  )
  
  if (is.null(district_facilities) || nrow(district_facilities) == 0) {
    stop("No facility points found inside selected district.")
  }
  
  seed_cells <- facility_points_to_seed_cells(
    facilities_sf = district_facilities,
    template_r = friction_d
  )
  
  alloc_obj <- compute_cost_allocation(
    seed_cells = seed_cells,
    friction_r = friction_d
  )
  
  area_summary <- summarize_area_population(
    allocation_r = alloc_obj$allocation,
    pop_r = pop_d,
    n_seeds = length(seed_cells)
  )
  
  district_sf <- sf::st_transform(district_sf, terra::crs(friction_r))
  
  friction_crop <- agg$friction_full
  pop_crop <- agg$pop_full
  
  alloc_final <- alloc_obj$allocation
  alloc_poly <- relabel_allocation_polygons(alloc_final)
  
  check_no_gaps(alloc_final, friction_d)
  
  total_children <- sum(terra::values(pop_crop, mat = FALSE), na.rm = TRUE)
  
  list(
    district = district_sf,
    points = alloc_obj$seeds_sf,
    facilities = district_facilities,
    friction_crop = friction_crop,
    population_crop = pop_crop,
    allocation = alloc_final,
    allocation_polygons = alloc_poly,
    area_summary = area_summary,
    total_children = total_children,
    target_children = target_children,
    tolerance = tolerance,
    n_areas = length(seed_cells),
    iterations_used = 1,
    max_iter = max_iter,
    agg_factor_used = agg$agg_factor_used
  )
}

# -----------------------------------------------------------------------------
# LOAD STATIC DATA
# -----------------------------------------------------------------------------
districts_shp <- safe_read_districts(districts_file)
friction_index <- safe_read_friction_index(friction_index_file)
population_r <- safe_read_population(population_file)

required_cols <- c("zone_name", "region_name", "district_name")
missing_cols <- setdiff(required_cols, names(districts_shp))
if (length(missing_cols) > 0) {
  stop(
    "districts_shp is missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .control-label { font-weight: 600; }
      .leaflet-container { background: #f8f8f8; }
      .app-title { margin-bottom: 12px; }
      .small-note { font-size: 12px; color: #666; }
      .map-title { font-size: 18px; font-weight: 700; margin-bottom: 8px; }
      .summary-box {
        background: #fafafa;
        border: 1px solid #dddddd;
        border-radius: 6px;
        padding: 10px 12px;
        margin-top: 12px;
        font-size: 13px;
      }
      .summary-line { margin-bottom: 4px; }
    "))
  ),
  
  titlePanel(div(class = "app-title", "District Friction and Health Area Viewer")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("zone", "Zone", choices = NULL),
      selectInput("region", "Region", choices = NULL),
      selectInput("district", "District", choices = NULL),
      
      hr(),
      
      numericInput("target_children", "Target children per health area", value = 2000, min = 100, step = 100),
      numericInput("tolerance", "Tolerance proportion", value = 0.20, min = 0.01, max = 1, step = 0.01),
      numericInput("agg_factor", "Planning aggregation factor", value = 5, min = 1, max = 20, step = 1),
      numericInput("max_iter", "Max balancing iterations", value = 15, min = 1, max = 100, step = 1),
      
      actionButton("generate", "Generate health areas", class = "btn-primary"),
      
      hr(),
      
      div(
        class = "small-note",
        "Left map shows the district-specific friction surface used for allocation. Right map shows sampled-facility-seeded health areas generated from that friction surface."
      ),
      
      div(
        class = "summary-box",
        htmlOutput("summary_text")
      )
    ),
    
    mainPanel(
      fluidRow(
        column(
          width = 6,
          div(class = "map-title", "District friction surface"),
          leafletOutput("map_friction", height = 780)
        ),
        column(
          width = 6,
          div(class = "map-title", "Generated health areas"),
          leafletOutput("map_health", height = 780)
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  zones <- districts_shp |>
    sf::st_drop_geometry() |>
    dplyr::pull(zone_name) |>
    unique() |>
    sort()
  
  updateSelectInput(session, "zone", choices = zones, selected = zones[1])
  
  observeEvent(input$zone, {
    req(input$zone)
    
    regions <- districts_shp |>
      sf::st_drop_geometry() |>
      dplyr::filter(zone_name == input$zone) |>
      dplyr::pull(region_name) |>
      unique() |>
      sort()
    
    req(length(regions) > 0)
    
    updateSelectInput(
      session,
      "region",
      choices = regions,
      selected = regions[1]
    )
  }, ignoreInit = FALSE)
  
  observeEvent(input$region, {
    req(input$zone, input$region)
    
    dists <- districts_shp |>
      sf::st_drop_geometry() |>
      dplyr::filter(
        zone_name == input$zone,
        region_name == input$region
      ) |>
      dplyr::pull(district_name) |>
      unique() |>
      sort()
    
    req(length(dists) > 0)
    
    updateSelectInput(
      session,
      "district",
      choices = dists,
      selected = dists[1]
    )
  }, ignoreInit = FALSE)
  
  selected_district_sf <- reactive({
    req(input$zone, input$region, input$district)
    
    district_sf <- get_selected_district(
      districts_shp = districts_shp,
      zone = input$zone,
      region = input$region,
      district = input$district
    )
    
    req(!is.null(district_sf), nrow(district_sf) > 0)
    district_sf
  })
  
  selected_friction_r <- reactive({
    req(input$zone, input$region, input$district)
    
    friction_file <- get_selected_friction_file(
      friction_index = friction_index,
      zone = input$zone,
      region = input$region,
      district = input$district
    )
    
    req(!is.null(friction_file), nzchar(friction_file), file.exists(friction_file))
    terra::rast(friction_file)
  })
  
  result_rv <- reactiveVal(NULL)
  
  output$map_friction <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      addScaleBar(position = "bottomleft")
  })
  
  output$map_health <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      addScaleBar(position = "bottomleft")
  })
  
  observe({
    district_sf <- selected_district_sf() |>
      sf::st_transform(4326)
    
    bb <- sf::st_bbox(district_sf)
    
    leafletProxy("map_friction") |>
      clearShapes() |>
      clearMarkers() |>
      clearImages() |>
      clearControls() |>
      fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]) |>
      addPolygons(
        data = district_sf,
        color = "black",
        weight = 2,
        fill = FALSE
      )
    
    leafletProxy("map_health") |>
      clearShapes() |>
      clearMarkers() |>
      clearImages() |>
      clearControls() |>
      fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]) |>
      addPolygons(
        data = district_sf,
        color = "black",
        weight = 2,
        fill = FALSE
      )
  })
  
  observeEvent(input$generate, {
    district_sf <- selected_district_sf()
    friction_r <- selected_friction_r()
    
    res <- make_capacity_balanced_health_areas(
      district_sf = district_sf,
      friction_r = friction_r,
      pop_r = population_r,
      target_children = input$target_children,
      tolerance = input$tolerance,
      agg_factor = input$agg_factor,
      max_iter = input$max_iter
    )
    
    result_rv(res)
  }, ignoreInit = TRUE)
  
  observe({
    res <- result_rv()
    req(res)
    
    district_sf_4326 <- sf::st_transform(res$district, 4326)
    pts_sf_4326 <- sf::st_transform(res$points, 4326)
    area_sf_4326 <- sf::st_transform(res$allocation_polygons, 4326)
    fac_sf_4326 <- sf::st_transform(res$facilities, 4326)
    
    bb <- sf::st_bbox(district_sf_4326)
    
    friction_vals <- terra::values(res$friction_crop, mat = FALSE)
    friction_vals <- friction_vals[is.finite(friction_vals)]
    req(length(friction_vals) > 0)
    
    friction_lower <- min(friction_vals, na.rm = TRUE)
    friction_upper <- as.numeric(stats::quantile(friction_vals, 0.98, na.rm = TRUE))
    
    if (!is.finite(friction_upper) || friction_upper <= friction_lower) {
      friction_upper <- max(friction_vals, na.rm = TRUE)
    }
    if (!is.finite(friction_upper) || friction_upper <= friction_lower) {
      friction_upper <- friction_lower + 1e-6
    }
    
    friction_vis <- terra::clamp(
      res$friction_crop,
      lower = friction_lower,
      upper = friction_upper,
      values = TRUE
    )
    
    friction_r_4326 <- terra::project(
      friction_vis,
      "EPSG:4326",
      method = "bilinear"
    )
    
    pal_friction <- colorNumeric(
      palette = "viridis",
      domain = c(friction_lower, friction_upper),
      na.color = "transparent"
    )
    
    leafletProxy("map_friction") |>
      clearShapes() |>
      clearMarkers() |>
      clearImages() |>
      clearControls() |>
      fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]) |>
      addRasterImage(
        friction_r_4326,
        colors = pal_friction,
        opacity = 0.85,
        project = FALSE
      ) |>
      addPolygons(
        data = district_sf_4326,
        color = "black",
        weight = 2,
        fill = FALSE
      ) |>
      addCircleMarkers(
        data = fac_sf_4326,
        radius = 4,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillColor = "yellow",
        fillOpacity = 1,
        popup = "Sampled facility"
      ) |>
      addCircleMarkers(
        data = pts_sf_4326,
        radius = 4,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillColor = "red",
        fillOpacity = 1,
        label = ~paste("Seed", id),
        popup = ~paste("Seed", id)
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal_friction,
        values = c(friction_lower, friction_upper),
        title = HTML("Friction<br/>(district-scaled)"),
        opacity = 0.85
      ) |>
      addControl(
        html = sprintf(
          paste0(
            "<div style='background: rgba(255,255,255,0.92); ",
            "padding: 6px 10px; border: 1px solid #ccc; font-weight: 600;'>",
            "District friction surface<br>",
            "<span style='font-weight:400;'>Display scale: %.2f to %.2f</span>",
            "</div>"
          ),
          friction_lower, friction_upper
        ),
        position = "topright"
      )
    
    pal_area <- colorFactor(
      palette = make_area_palette(nrow(area_sf_4326)),
      domain = area_sf_4326$health_area
    )
    
    leafletProxy("map_health") |>
      clearShapes() |>
      clearMarkers() |>
      clearImages() |>
      clearControls() |>
      fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]) |>
      addPolygons(
        data = area_sf_4326,
        fillColor = ~pal_area(health_area),
        fillOpacity = 0.25,
        color = "#444444",
        weight = 1.2,
        smoothFactor = 0,
        label = ~health_area,
        popup = ~health_area
      ) |>
      addPolygons(
        data = district_sf_4326,
        color = "black",
        weight = 2,
        fill = FALSE
      ) |>
      addCircleMarkers(
        data = fac_sf_4326,
        radius = 4,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillColor = "yellow",
        fillOpacity = 1,
        popup = "Sampled facility"
      ) |>
      addCircleMarkers(
        data = pts_sf_4326,
        radius = 4,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillColor = "red",
        fillOpacity = 1,
        label = ~paste("Seed", id),
        popup = ~paste("Seed", id)
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal_area,
        values = area_sf_4326$health_area,
        title = "Health areas",
        opacity = 0.85
      ) |>
      addControl(
        html = "<div style='background: rgba(255,255,255,0.92); padding: 6px 10px; border: 1px solid #ccc; font-weight: 600;'>Sampled-facility-seeded health areas</div>",
        position = "topright"
      )
  })
  
  output$summary_text <- renderUI({
    res <- result_rv()
    
    if (is.null(res)) {
      return(HTML("<div class='summary-line'>No result yet.</div>"))
    }
    
    friction_vals <- terra::values(res$friction_crop, mat = FALSE)
    friction_vals <- friction_vals[is.finite(friction_vals)]
    
    min_friction <- if (length(friction_vals) > 0) round(min(friction_vals), 4) else NA
    max_friction <- if (length(friction_vals) > 0) round(max(friction_vals), 2) else NA
    
    area_lines <- paste0(
      "<div class='summary-line'><b>Area populations:</b><br>",
      paste(
        paste0(
          "Area ", res$area_summary$id, ": ",
          round(res$area_summary$children)
        ),
        collapse = "<br>"
      ),
      "</div>"
    )
    
    HTML(paste0(
      "<div class='summary-line'><b>Zone:</b> ", input$zone, "</div>",
      "<div class='summary-line'><b>Region:</b> ", input$region, "</div>",
      "<div class='summary-line'><b>District:</b> ", input$district, "</div>",
      "<div class='summary-line'><b>Facilities used:</b> ", res$n_areas, "</div>",
      "<div class='summary-line'><b>Total children:</b> ", round(res$total_children), "</div>",
      "<div class='summary-line'><b>Target children per area:</b> ", round(res$target_children), "</div>",
      "<div class='summary-line'><b>Tolerance:</b> ", round(100 * res$tolerance, 1), "%</div>",
      "<div class='summary-line'><b>Planning iterations used:</b> ", res$iterations_used, "</div>",
      "<div class='summary-line'><b>Max iterations setting:</b> ", res$max_iter, "</div>",
      "<div class='summary-line'><b>Aggregation factor used:</b> ", res$agg_factor_used, "</div>",
      "<div class='summary-line'><b>District friction range:</b> ", min_friction, " to ", max_friction, "</div>",
      area_lines
    ))
  })
}

shinyApp(ui, server)