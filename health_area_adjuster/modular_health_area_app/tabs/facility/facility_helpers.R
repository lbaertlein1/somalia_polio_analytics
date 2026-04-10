make_starter_facilities <- function(district_sf, district_name, n_facilities = n_start_dfas, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  pts <- sf::st_sample(district_sf, size = n_facilities, exact = TRUE)
  
  pts_sf <- sf::st_sf(
    facility_id = paste0(gsub("\\s+", "_", tolower(district_name)), "_hf_", seq_len(n_facilities)),
    facility_name = paste("Health Facility", seq_len(n_facilities)),
    operational = "Operational",
    ri_services = "Yes",
    facility_type = "Health Center",
    polio_sia_coordination_site = "Yes",
    geometry = pts,
    crs = sf::st_crs(district_sf)
  ) |>
    sf::st_transform(4326)
  
  coords <- sf::st_coordinates(pts_sf)
  
  pts_sf$lon <- coords[, 1]
  pts_sf$lat <- coords[, 2]
  
  pts_sf |>
    dplyr::select(
      facility_id,
      facility_name,
      lon,
      lat,
      operational,
      ri_services,
      facility_type,
      polio_sia_coordination_site,
      geometry
    )
}

facility_sf_to_df <- function(facility_sf) {
  if (is.null(facility_sf) || nrow(facility_sf) == 0) {
    return(data.frame(
      facility_id = character(),
      facility_name = character(),
      lon = numeric(),
      lat = numeric(),
      operational = character(),
      ri_services = character(),
      facility_type = character(),
      polio_sia_coordination_site = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  sf::st_drop_geometry(facility_sf) |>
    dplyr::mutate(
      lon = as.numeric(lon),
      lat = as.numeric(lat)
    )
}

facility_df_to_sf <- function(facility_df) {
  if (is.null(facility_df) || nrow(facility_df) == 0) {
    return(NULL)
  }
  
  sf::st_as_sf(
    facility_df,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
}