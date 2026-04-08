library(sf)
library(dplyr)
library(tibble)

set.seed(42)

# -------------------------------------------------------
# Mogadishu district boundary
# Covers the main peninsula + inland urban area
# Approx bbox: 45.28–45.40E, 2.00–2.12N
# -------------------------------------------------------
district_coords <- rbind(
  c(45.282, 2.038),   # west coast, south
  c(45.288, 2.018),   # Hamarweyne tip (south peninsula)
  c(45.305, 2.008),   # tip of peninsula
  c(45.325, 2.012),   # east coast south
  c(45.345, 2.028),   # east coast
  c(45.362, 2.048),   # northeast
  c(45.372, 2.068),   # north
  c(45.368, 2.092),   # northwest
  c(45.350, 2.108),   # inland north
  c(45.325, 2.115),   # inland northwest
  c(45.300, 2.110),   # inland west
  c(45.280, 2.092),   # west
  c(45.272, 2.070),   # west coast mid
  c(45.275, 2.050),   # west coast
  c(45.282, 2.038)    # close
)

district <- st_sfc(st_polygon(list(district_coords)), crs = 4326)
district_sf <- st_sf(name = "Mogadishu District", geometry = district)

# -------------------------------------------------------
# 12 health facilities — real facility names / types used
# in Mogadishu, locations placed to give good spatial spread
# -------------------------------------------------------
facilities <- tibble(
  facility_id = paste0("MGS_", sprintf("%02d", 1:12)),
  name = c(
    "Banadir Hospital",
    "Martini Hospital",
    "Medina Hospital",
    "Karan MCH Centre",
    "Hodan Health Centre",
    "Wadajir Dispensary",
    "Yaqshid MCH Centre",
    "Dharkenley Health Centre",
    "Hamarweyne PHU",
    "Shangani Dispensary",
    "Bondhere Health Centre",
    "Waberi MCH Centre"
  ),
  type = c(
    "Hospital", "Hospital", "Hospital",
    "MCH Centre", "Health Centre", "Dispensary",
    "MCH Centre", "Health Centre", "PHU",
    "Dispensary", "Health Centre", "MCH Centre"
  ),
  # Placed to give good Voronoi spread across the district
  lng = c(
    45.342, 45.318, 45.365,   # hospitals
    45.350, 45.308, 45.290,   # karan, hodan, wadajir
    45.358, 45.285, 45.300,   # yaqshid, dharkenley, hamarweyne
    45.320, 45.335, 45.310    # shangani, bondhere, waberi
  ),
  lat = c(
    2.068, 2.048, 2.088,      # hospitals
    2.095, 2.072, 2.055,      # karan, hodan, wadajir
    2.075, 2.082, 2.030,      # yaqshid, dharkenley, hamarweyne
    2.022, 2.058, 2.040       # shangani, bondhere, waberi
  ),
  population_served = c(
    62000, 48000, 55000,
    28000, 31000, 19000,
    24000, 22000, 17000,
    15000, 26000, 21000
  ),
  staff_count = c(
    85, 72, 78,
    32, 38, 21,
    28, 25, 18,
    16, 30, 24
  )
)

# -------------------------------------------------------
# Voronoi tessellation clipped to district
# Using UTM zone 38N (EPSG:32638) — correct for Mogadishu
# -------------------------------------------------------
pts_sf        <- st_as_sf(facilities, coords = c("lng", "lat"), crs = 4326)
pts_proj      <- st_transform(pts_sf, 32638)
district_proj <- st_transform(district_sf, 32638)
district_buf  <- st_buffer(district_proj, 3000)   # 3 km envelope buffer

voronoi_raw   <- st_voronoi(st_union(pts_proj), envelope = st_geometry(district_buf))
voronoi_polys <- st_collection_extract(voronoi_raw, "POLYGON")
voronoi_sf    <- st_sf(geometry = voronoi_polys)

# Clip each polygon to district
voronoi_clipped <- st_intersection(voronoi_sf, district_proj)

# Match each clipped polygon to its nearest facility
nearest_idx <- st_nearest_feature(voronoi_clipped, pts_proj)

catchments <- bind_cols(
  facilities[nearest_idx, ],
  st_sf(geometry = st_geometry(voronoi_clipped))
) %>%
  st_as_sf() %>%
  st_transform(4326)

# -------------------------------------------------------
# Validation
# -------------------------------------------------------
stopifnot(length(unique(catchments$facility_id)) == nrow(facilities))

up   <- st_transform(catchments, 32638)
dist <- st_transform(district_sf, 32638)

overlap_pct <- abs(
  as.numeric(st_area(st_union(up))) - sum(as.numeric(st_area(up)))
) / as.numeric(st_area(dist)) * 100

coverage_pct <- as.numeric(st_area(st_union(up))) /
  as.numeric(st_area(dist)) * 100

cat(sprintf("Facilities : %d\n",   nrow(catchments)))
cat(sprintf("Overlap    : %.5f%% (target ~0)\n",   overlap_pct))
cat(sprintf("Coverage   : %.2f%% (target ~100)\n", coverage_pct))

# -------------------------------------------------------
# Save
# -------------------------------------------------------
dir.create("data", showWarnings = FALSE)
saveRDS(catchments,  "data/catchments.Rds")
saveRDS(district_sf, "data/district.Rds")

# Remove any stale edits
if (file.exists("data/catchments_edited.Rds"))
  file.remove("data/catchments_edited.Rds")

cat("Saved data/catchments.Rds and data/district.Rds\n")
print(catchments[, c("facility_id", "name", "type")], n = 20)