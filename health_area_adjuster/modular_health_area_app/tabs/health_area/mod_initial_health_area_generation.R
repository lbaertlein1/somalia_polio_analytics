initialHealthAreaGenerationServer <- function(
    id,
    district_sf,
    grid_n,
    n_dfa = 5,
    seed = 1,
    facility_seed_sf = reactive(NULL),
    facility_name_col = NULL
) {
  moduleServer(id, function(input, output, session) {

    safe_make_valid <- function(x) {
      sf::st_make_valid(x)
    }

    make_paint_grid <- function(district_sf, grid_n = 150) {
      district_sf <- safe_make_valid(district_sf)
      district_3857 <- sf::st_transform(district_sf, 3857)

      bbox <- sf::st_bbox(district_3857)
      width_m <- bbox$xmax - bbox$xmin
      height_m <- bbox$ymax - bbox$ymin
      max_dim <- max(width_m, height_m)

      cellsize <- max_dim / grid_n

      raw_grid <- sf::st_make_grid(
        district_3857,
        cellsize = cellsize,
        what = 'polygons',
        square = TRUE
      )

      grid_sf <- sf::st_sf(
        cell_id = seq_along(raw_grid),
        geometry = raw_grid,
        crs = sf::st_crs(district_3857)
      )

      cent_3857 <- suppressWarnings(sf::st_centroid(grid_sf))
      inside <- lengths(sf::st_within(cent_3857, district_3857)) > 0

      grid_sf <- grid_sf |>
        dplyr::filter(inside) |>
        dplyr::mutate(cell_id = seq_len(dplyr::n()))

      cent_wgs84 <- sf::st_transform(cent_3857[inside, ], 4326)
      coords <- sf::st_coordinates(cent_wgs84)

      grid_sf <- sf::st_transform(grid_sf, 4326)
      grid_sf$centroid_lon <- coords[, 1]
      grid_sf$centroid_lat <- coords[, 2]

      list(
        grid_sf = grid_sf,
        max_dim_m = as.numeric(max_dim)
      )
    }

    make_start_assignment <- function(grid_sf, district_sf, n_dfa = 5, seed = 1) {
      set.seed(seed)

      pts <- sf::st_sample(district_sf, size = n_dfa, exact = TRUE)

      pts_sf <- sf::st_sf(
        dfa_name = paste('Health Area', seq_len(n_dfa)),
        geometry = pts,
        crs = sf::st_crs(district_sf)
      )

      cent <- suppressWarnings(sf::st_centroid(grid_sf))
      idx <- sf::st_nearest_feature(cent, pts_sf)

      list(
        assignments = as.character(pts_sf$dfa_name[idx]),
        seeds_sf = pts_sf
      )
    }

    make_start_assignment_from_sites <- function(grid_sf, site_sf, name_col = NULL) {
      stopifnot(!is.null(grid_sf), nrow(grid_sf) > 0)
      stopifnot(!is.null(site_sf), nrow(site_sf) > 0)

      site_sf <- sf::st_as_sf(site_sf)
      site_sf <- sf::st_transform(site_sf, sf::st_crs(grid_sf))

      if (is.null(name_col)) {
        if ('dfa_name' %in% names(site_sf)) {
          name_col <- 'dfa_name'
        } else if ('facility_name' %in% names(site_sf)) {
          name_col <- 'facility_name'
        }
      }

      if (is.null(name_col)) {
        site_sf$dfa_name <- paste('Health Area', seq_len(nrow(site_sf)))
      } else {
        site_sf$dfa_name <- as.character(site_sf[[name_col]])
      }

      cent <- suppressWarnings(sf::st_centroid(grid_sf))
      idx <- sf::st_nearest_feature(cent, site_sf)

      list(
        assignments = as.character(site_sf$dfa_name[idx]),
        seeds_sf = site_sf
      )
    }

    build_seed_points_outputs <- function(seed_points_sf) {
      seed_pts <- sf::st_transform(seed_points_sf, 4326)
      seed_coords <- sf::st_coordinates(seed_pts)

      seed_points_df <- data.frame(
        dfa_name = as.character(seed_pts$dfa_name),
        lon = seed_coords[, 1],
        lat = seed_coords[, 2],
        stringsAsFactors = FALSE
      )

      seed_points_list <- lapply(seq_len(nrow(seed_points_df)), function(i) {
        list(
          dfa_name = seed_points_df$dfa_name[i],
          lon = unname(seed_points_df$lon[i]),
          lat = unname(seed_points_df$lat[i])
        )
      })

      list(
        seed_points_sf = seed_pts,
        seed_points_df = seed_points_df,
        seed_points_list = seed_points_list
      )
    }

    build_neighbors_list <- function(grid_sf) {
      touch_list <- sf::st_touches(grid_sf)
      neighbors_list <- lapply(touch_list, as.integer)
      names(neighbors_list) <- as.character(grid_sf$cell_id)
      neighbors_list
    }

    build_edge_list <- function(grid_sf, district_sf) {
      grid_sf_3857 <- sf::st_transform(grid_sf, 3857)
      district_3857 <- sf::st_transform(district_sf, 3857)

      cell_bbox <- sf::st_bbox(grid_sf_3857[1, ])
      cell_w <- as.numeric(cell_bbox['xmax'] - cell_bbox['xmin'])
      cell_h <- as.numeric(cell_bbox['ymax'] - cell_bbox['ymin'])
      edge_buffer <- max(cell_w, cell_h) * 0.05

      district_boundary_3857 <- sf::st_boundary(district_3857) |>
        sf::st_buffer(edge_buffer)

      edge_flag <- lengths(sf::st_intersects(grid_sf_3857, district_boundary_3857)) > 0
      edge_list <- as.list(edge_flag)
      names(edge_list) <- as.character(grid_sf$cell_id)
      edge_list
    }

    scene <- reactive({
      req(district_sf())
      req(nrow(district_sf()) > 0)
      req(grid_n())

      district_sf_value <- safe_make_valid(district_sf())

      grid_info <- make_paint_grid(
        district_sf = district_sf_value,
        grid_n = grid_n()
      )

      grid_sf_value <- grid_info$grid_sf
      facility_sf_value <- facility_seed_sf()

      if (!is.null(facility_sf_value) && nrow(facility_sf_value) > 0) {
        start_info <- make_start_assignment_from_sites(
          grid_sf = grid_sf_value,
          site_sf = facility_sf_value,
          name_col = facility_name_col
        )
      } else {
        start_info <- make_start_assignment(
          grid_sf = grid_sf_value,
          district_sf = district_sf_value,
          n_dfa = n_dfa,
          seed = seed
        )
      }

      seed_outputs <- build_seed_points_outputs(start_info$seeds_sf)

      list(
        district_sf = district_sf_value,
        grid_sf = grid_sf_value,
        initial_assignments = as.character(start_info$assignments),
        seed_points_sf = seed_outputs$seed_points_sf,
        seed_points_df = seed_outputs$seed_points_df,
        seed_points_list = seed_outputs$seed_points_list,
        neighbors_list = build_neighbors_list(grid_sf_value),
        edge_list = build_edge_list(grid_sf_value, district_sf_value),
        max_dim_m = grid_info$max_dim_m
      )
    })

    list(
      scene = scene,
      district_sf = reactive(scene()$district_sf),
      grid_sf = reactive(scene()$grid_sf),
      initial_assignments = reactive(scene()$initial_assignments),
      seed_points_sf = reactive(scene()$seed_points_sf),
      seed_points_df = reactive(scene()$seed_points_df),
      seed_points_list = reactive(scene()$seed_points_list),
      neighbors_list = reactive(scene()$neighbors_list),
      edge_list = reactive(scene()$edge_list),
      max_dim_m = reactive(scene()$max_dim_m)
    )
  })
}
