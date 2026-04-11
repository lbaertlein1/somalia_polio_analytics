suppressPackageStartupMessages({
  library(shiny)
  library(sf)
  library(terra)
  library(dplyr)
  library(leaflet)
  library(RColorBrewer)
})

friction_file  <- "data/friction/somalia_friction_100m.tif"
districts_file <- "data/districts_shp.Rds"

safe_read_districts <- function(path) {
  x <- readRDS(path)
  if (!inherits(x, "sf")) stop("districts_file must contain an sf object.")
  x |>
    sf::st_as_sf() |>
    sf::st_make_valid()
}

safe_polygonize <- function(x) {
  x |>
    sf::st_make_valid() |>
    sf::st_collection_extract("POLYGON", warn = FALSE) |>
    sf::st_cast("MULTIPOLYGON", warn = FALSE) |>
    sf::st_make_valid()
}

get_selected_district <- function(districts_shp, zone, region, district) {
  out <- districts_shp |>
    dplyr::filter(
      zone_name == zone,
      region_name == region,
      district_name == district
    )
  
  if (nrow(out) == 0) {
    stop("No district found for current selection.")
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
    safe_polygonize()
}

make_random_health_areas <- function(district_sf, friction_r, n_points = 5, seed = 123) {
  stopifnot(inherits(district_sf, "sf"), nrow(district_sf) == 1)
  
  set.seed(seed)
  
  district_sf <- sf::st_transform(district_sf, terra::crs(friction_r))
  
  friction_d <- terra::crop(friction_r, terra::vect(district_sf))
  friction_d <- terra::mask(friction_d, terra::vect(district_sf))
  
  if (all(is.na(terra::values(friction_d)))) {
    stop("Cropped friction raster contains no valid cells.")
  }
  
  pts <- sf::st_sample(district_sf, size = n_points, type = "random")
  
  if (length(pts) == 0) {
    stop("No random points could be generated in the selected district.")
  }
  
  pts <- sf::st_as_sf(pts)
  pts$id <- seq_len(n_points)
  pts_vect <- terra::vect(pts)
  
  cost_stack <- vector("list", n_points)
  
  for (i in seq_len(n_points)) {
    origin_r <- terra::rasterize(
      pts_vect[i],
      friction_d,
      field = 0,
      background = NA
    )
    
    combined <- terra::ifel(!is.na(origin_r), 0, friction_d)
    cost_i <- terra::gridDist(combined, target = 0)
    names(cost_i) <- paste0("p", i)
    cost_stack[[i]] <- cost_i
  }
  
  cost_rast <- terra::rast(cost_stack)
  
  allocation <- terra::which.min(cost_rast)
  names(allocation) <- "health_area"
  allocation <- terra::mask(allocation, terra::vect(district_sf))
  
  alloc_poly <- terra::as.polygons(allocation, values = TRUE, na.rm = TRUE) |>
    sf::st_as_sf() |>
    sf::st_make_valid()
  
  names(alloc_poly)[1] <- "health_area"
  
  alloc_poly <- alloc_poly |>
    dplyr::group_by(health_area) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
    safe_polygonize()
  
  alloc_poly$health_area <- paste("Health Area", alloc_poly$health_area)
  
  list(
    district = district_sf,
    points = pts,
    allocation = allocation,
    allocation_polygons = alloc_poly,
    friction_crop = friction_d
  )
}

make_area_palette <- function(n) {
  if (n <= 8) {
    RColorBrewer::brewer.pal(max(3, n), "Set2")[seq_len(n)]
  } else {
    grDevices::hcl.colors(n, "Set 2")
  }
}

friction <- terra::rast(friction_file)
districts_shp <- safe_read_districts(districts_file)

required_cols <- c("zone_name", "region_name", "district_name")
missing_cols <- setdiff(required_cols, names(districts_shp))
if (length(missing_cols) > 0) {
  stop("districts_shp is missing required columns: ", paste(missing_cols, collapse = ", "))
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .control-label { font-weight: 600; }
      .leaflet-container { background: #f8f8f8; }
      .app-title { margin-bottom: 12px; }
    "))
  ),
  titlePanel(div(class = "app-title", "Random Initial Health Areas Tester")),
  sidebarLayout(
    sidebarPanel(
      selectInput("zone", "Zone", choices = NULL),
      selectInput("region", "Region", choices = NULL),
      selectInput("district", "District", choices = NULL),
      numericInput("n_points", "Random seed points", value = 5, min = 2, max = 20, step = 1),
      numericInput("seed", "Random seed", value = 123, min = 1, step = 1),
      actionButton("generate", "Generate health areas", class = "btn-primary"),
      hr(),
      helpText("This app picks random points inside the selected district and draws initial health areas using the friction surface."),
      helpText("Area fill is intentionally light so boundaries remain easy to inspect.")
    ),
    mainPanel(
      leafletOutput("map", height = 750)
    )
  )
)

server <- function(input, output, session) {
  zones <- districts_shp |>
    sf::st_drop_geometry() |>
    dplyr::pull(zone_name) |>
    unique() |>
    sort()
  
  updateSelectInput(session, "zone", choices = zones, selected = zones[[1]])
  
  observeEvent(input$zone, {
    req(input$zone)
    
    regions <- districts_shp |>
      sf::st_drop_geometry() |>
      dplyr::filter(zone_name == input$zone) |>
      dplyr::pull(region_name) |>
      unique() |>
      sort()
    
    updateSelectInput(session, "region", choices = regions, selected = regions[[1]])
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
    
    updateSelectInput(session, "district", choices = dists, selected = dists[[1]])
  }, ignoreInit = FALSE)
  
  result_rv <- reactiveVal(NULL)
  
  selected_district_sf <- reactive({
    req(input$zone, input$region, input$district)
    
    get_selected_district(
      districts_shp = districts_shp,
      zone = input$zone,
      region = input$region,
      district = input$district
    )
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      addScaleBar(position = "bottomleft")
  })
  
  observe({
    district_sf <- selected_district_sf() |>
      sf::st_transform(4326)
    
    bb <- sf::st_bbox(district_sf)
    
    leafletProxy("map") |>
      clearShapes() |>
      clearMarkers() |>
      fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]) |>
      addPolygons(
        data = district_sf,
        color = "black",
        weight = 2,
        fill = FALSE,
        group = "District"
      )
  })
  
  observeEvent(input$generate, {
    district_sf <- selected_district_sf()
    
    out <- make_random_health_areas(
      district_sf = district_sf,
      friction_r = friction,
      n_points = input$n_points,
      seed = input$seed
    )
    
    result_rv(out)
  }, ignoreInit = TRUE)
  
  observe({
    res <- result_rv()
    req(res)
    
    district_sf <- sf::st_transform(res$district, 4326)
    area_sf <- sf::st_transform(res$allocation_polygons, 4326)
    pts_sf <- sf::st_transform(res$points, 4326)
    
    bb <- sf::st_bbox(district_sf)
    
    pal <- colorFactor(
      palette = make_area_palette(nrow(area_sf)),
      domain = area_sf$health_area
    )
    
    leafletProxy("map") |>
      clearShapes() |>
      clearMarkers() |>
      fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]) |>
      addPolygons(
        data = area_sf,
        fillColor = ~pal(health_area),
        fillOpacity = 0.20,
        color = "#444444",
        weight = 1.1,
        smoothFactor = 0.2,
        label = ~health_area,
        popup = ~health_area,
        group = "Health Areas"
      ) |>
      addPolygons(
        data = district_sf,
        color = "black",
        weight = 2,
        fill = FALSE,
        group = "District"
      ) |>
      addCircleMarkers(
        data = pts_sf,
        radius = 5,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillColor = "red",
        fillOpacity = 1,
        label = ~paste("Point", id),
        popup = ~paste("Point", id),
        group = "Seed Points"
      )
  })
}

shinyApp(ui, server)