library(sf)
library(dplyr)
library(terra)

# =========================================================
# Demo script: create health_centers.Rds from district polygons
# and a U5 population raster
#
# Goal:
# - roughly 1 health center per 2,000 U5 population in each district
# - place centers near the largest population concentrations
# - spread them across the district rather than stacking them together
#
# Output:
# - health_centers.Rds (sf POINT object in EPSG:4326)
# =========================================================

# -----------------------------
# User config
# -----------------------------

districts_file <- "districts_shp.Rds"
worldpop_u5_file <- "som_u5_population_2025_100m.tif"
output_file <- "health_centers.Rds"

# Coarsen raster to make this fast/simple for demo purposes.
# Larger number = fewer candidate cells.
agg_factor <- 20

# Controls how strongly selected centers are forced to spread out.
# This is multiplied by the district size.
spread_fraction <- 0.20

# -----------------------------
# Helpers
# -----------------------------

safe_make_valid <- function(x) {
  tryCatch(st_make_valid(x), error = function(e) x)
}

calc_n_health_centers <- function(total_u5) {
  if (is.na(total_u5) || total_u5 <= 0) return(1L)
  max(1L, as.integer(round(total_u5 / 2000)))
}

bbox_max_dim_m <- function(x_sf) {
  x_3857 <- st_transform(x_sf, 3857)
  bb <- st_bbox(x_3857)
  max(as.numeric(bb$xmax - bb$xmin), as.numeric(bb$ymax - bb$ymin))
}

pick_spread_points <- function(cand_sf, n_points, min_dist_m) {
  if (nrow(cand_sf) == 0) return(cand_sf[0, ])
  if (nrow(cand_sf) <= n_points) return(cand_sf)

  cand_3857 <- st_transform(cand_sf, 3857)
  selected_idx <- integer(0)

  for (i in seq_len(nrow(cand_3857))) {
    if (length(selected_idx) == 0) {
      selected_idx <- i
    } else {
      d <- as.numeric(st_distance(cand_3857[i, ], cand_3857[selected_idx, ]))
      if (all(d >= min_dist_m)) {
        selected_idx <- c(selected_idx, i)
      }
    }
    if (length(selected_idx) >= n_points) break
  }

  # If strict distance rule leaves too few points, fill remaining slots
  if (length(selected_idx) < n_points) {
    remaining <- setdiff(seq_len(nrow(cand_sf)), selected_idx)
    selected_idx <- c(selected_idx, head(remaining, n_points - length(selected_idx)))
  }

  cand_sf[selected_idx, ]
}

make_demo_centers_for_district <- function(district_sf, u5_rast, agg_factor = 20, spread_fraction = 0.20) {
  district_sf <- safe_make_valid(district_sf)
  district_vect <- terra::vect(st_transform(district_sf, crs(u5_rast)))

  r_crop <- terra::crop(u5_rast, district_vect, snap = "out")
  r_mask <- terra::mask(r_crop, district_vect)

  vals <- terra::values(r_mask)
  vals <- vals[!is.na(vals)]
  total_u5 <- sum(vals)
  n_hc <- calc_n_health_centers(total_u5)

  # Aggregate to a coarser surface so high-density clusters dominate.
  r_small <- terra::aggregate(r_mask, fact = agg_factor, fun = sum, na.rm = TRUE)
  pts <- terra::as.points(r_small, na.rm = TRUE)

  if (is.null(pts) || nrow(pts) == 0) {
    # fallback: one point at district centroid
    pt <- suppressWarnings(st_point_on_surface(st_transform(district_sf, 4326)))
    out <- st_sf(
      health_center_name = paste0(district_sf$district_name[1], " Health Center 1"),
      zone_name = district_sf$zone_name[1],
      region_name = district_sf$region_name[1],
      district_name = district_sf$district_name[1],
      est_district_u5 = round(total_u5),
      est_n_health_centers = n_hc,
      demo_source = "district_centroid_fallback",
      geometry = st_geometry(pt),
      crs = 4326
    )
    return(out)
  }

  cand_sf <- st_as_sf(pts)
  value_col <- names(cand_sf)[names(cand_sf) != attr(cand_sf, "sf_column")][1]
  names(cand_sf)[names(cand_sf) == value_col] <- "pop_value"

  cand_sf <- cand_sf |>
    filter(!is.na(pop_value), pop_value > 0) |>
    st_transform(4326)

  if (nrow(cand_sf) == 0) {
    pt <- suppressWarnings(st_point_on_surface(st_transform(district_sf, 4326)))
    out <- st_sf(
      health_center_name = paste0(district_sf$district_name[1], " Health Center 1"),
      zone_name = district_sf$zone_name[1],
      region_name = district_sf$region_name[1],
      district_name = district_sf$district_name[1],
      est_district_u5 = round(total_u5),
      est_n_health_centers = n_hc,
      demo_source = "district_centroid_fallback",
      geometry = st_geometry(pt),
      crs = 4326
    )
    return(out)
  }

  # Rank candidate population centers from highest to lowest.
  cand_sf <- cand_sf |>
    arrange(desc(pop_value))

  # Use district size to define a simple spreading threshold.
  district_size_m <- bbox_max_dim_m(district_sf)
  min_dist_m <- max(500, district_size_m * spread_fraction)

  selected <- pick_spread_points(cand_sf, n_points = n_hc, min_dist_m = min_dist_m)

  # Clip again just in case any point lies slightly outside after raster conversion.
  selected <- st_transform(selected, st_crs(district_sf))
  inside <- lengths(st_within(selected, district_sf)) > 0
  selected <- selected[inside, ]

  # If too few remain after clipping, fill with next-best candidates inside district.
  if (nrow(selected) < n_hc) {
    cand_sf2 <- st_transform(cand_sf, st_crs(district_sf))
    inside2 <- lengths(st_within(cand_sf2, district_sf)) > 0
    cand_sf2 <- cand_sf2[inside2, ]
    if (nrow(cand_sf2) > 0) {
      already_wkt <- if (nrow(selected) > 0) st_as_text(st_geometry(selected)) else character(0)
      cand_sf2$wkt <- st_as_text(st_geometry(cand_sf2))
      cand_sf2 <- cand_sf2 |> filter(!wkt %in% already_wkt) |> dplyr::select(-wkt)
      if (nrow(cand_sf2) > 0) {
        selected <- rbind(selected, head(cand_sf2, n_hc - nrow(selected)))
      }
    }
  }

  # Final fallback if still empty.
  if (nrow(selected) == 0) {
    pt <- suppressWarnings(st_point_on_surface(st_transform(district_sf, 4326)))
    selected <- st_sf(pop_value = total_u5, geometry = st_geometry(pt), crs = 4326)
  }

  selected <- st_transform(selected, 4326)
  selected <- selected |>
    mutate(
      health_center_name = paste0(district_sf$district_name[1], " Health Center ", seq_len(n())),
      zone_name = district_sf$zone_name[1],
      region_name = district_sf$region_name[1],
      district_name = district_sf$district_name[1],
      est_district_u5 = round(total_u5),
      est_n_health_centers = n_hc,
      demo_source = "population_peak"
    ) |>
    dplyr::select(
      health_center_name,
      zone_name,
      region_name,
      district_name,
      est_district_u5,
      est_n_health_centers,
      pop_value,
      demo_source,
      geometry
    )

  selected
}

# -----------------------------
# Load inputs
# -----------------------------

if (!file.exists(districts_file)) {
  stop("districts_file not found: ", districts_file)
}
if (!file.exists(worldpop_u5_file)) {
  stop("worldpop_u5_file not found: ", worldpop_u5_file)
}

districts_shp <- readRDS(districts_file)
districts_shp <- safe_make_valid(districts_shp)

required_cols <- c("zone_name", "region_name", "district_name")
missing_cols <- setdiff(required_cols, names(districts_shp))
if (length(missing_cols) > 0) {
  stop("districts_shp is missing required column(s): ", paste(missing_cols, collapse = ", "))
}

u5_rast <- terra::rast(worldpop_u5_file)

# -----------------------------
# Build one district polygon per district
# -----------------------------

districts_one <- districts_shp |>
  group_by(zone_name, region_name, district_name) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_as_sf() |>
  safe_make_valid()

# -----------------------------
# Generate demo health centers
# -----------------------------

hc_list <- vector("list", nrow(districts_one))

for (i in seq_len(nrow(districts_one))) {
  message(sprintf(
    "[%s/%s] %s | %s | %s",
    i,
    nrow(districts_one),
    districts_one$zone_name[i],
    districts_one$region_name[i],
    districts_one$district_name[i]
  ))

  hc_list[[i]] <- make_demo_centers_for_district(
    district_sf = districts_one[i, ],
    u5_rast = u5_rast,
    agg_factor = agg_factor,
    spread_fraction = spread_fraction
  )
}

health_centers_sf <- do.call(rbind, hc_list) |>
  st_as_sf() |>
  st_transform(4326) |>
  mutate(
    hc_id = paste0("hc_", seq_len(n())),
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2],
    correct_location = TRUE,
    operational = TRUE,
    coordination_site = TRUE,
    is_new = FALSE
  ) |>
  dplyr::select(
    hc_id,
    health_center_name,
    zone_name,
    region_name,
    district_name,
    lon,
    lat,
    est_district_u5,
    est_n_health_centers,
    pop_value,
    demo_source,
    correct_location,
    operational,
    coordination_site,
    is_new,
    geometry
  )

saveRDS(health_centers_sf, output_file)

message("Saved: ", normalizePath(output_file, winslash = "/", mustWork = FALSE))
message("Total demo health centers created: ", nrow(health_centers_sf))

