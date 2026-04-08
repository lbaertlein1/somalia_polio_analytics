library(rio)
library(dplyr)
library(tidyr)
library(sf)
library(janitor)
library(stringr)
library(ggplot2)
library(ggnewscale)
library(grid)
library(terra)
library(raster)
library(rosm)
library(sp)

# -----------------------------
# Import data
# -----------------------------
genetic_match_data <- rio::import(
  "data/sOM_BAN-1_2023_present_Demo.xlsx",
  which = "SOM-BAN-1 since 2023"
)

virus_data <- rio::import(
  "data/Viruses_Detailed_Dataset_may_contain_sensitive_data_25-03-2026_13-29-40.xlsx"
)

# -----------------------------
# Read WHO geodatabase
# -----------------------------
gdb_path <- "C:/Users/epuser/Downloads/WHO_POLIO_GLOBAL_GEODATABASE.gdb/WHO_POLIO_GLOBAL_GEODATABASE.gdb"

who_gdb_adm1_province <- sf::st_read(
  gdb_path,
  "GLOBAL_ADM1",
  quiet = TRUE
) %>%
  janitor::clean_names()

who_gdb_adm0_country <- sf::st_read(
  gdb_path,
  "GLOBAL_ADM0",
  quiet = TRUE
) %>%
  janitor::clean_names()

# -----------------------------
# Keep Horn / nearby countries used in your map extent
# -----------------------------
map_countries <- c(
  "DJIBOUTI",
  "ERITREA",
  "ETHIOPIA",
  "KENYA",
  "SOMALIA",
  "UGANDA"
)

adm1_map <- who_gdb_adm1_province %>%
  filter(toupper(adm0_name) %in% map_countries)

adm0_map <- who_gdb_adm0_country %>%
  filter(toupper(adm0_name) %in% map_countries)

# -----------------------------
# Project admin layers
# -----------------------------
plot_crs <- 3857

adm1_map_proj <- st_transform(adm1_map, plot_crs)
adm0_map_proj <- st_transform(adm0_map, plot_crs)

# -----------------------------
# Carto Light basemap helper
# -----------------------------
get_cartolight_df_3857 <- function(bbox_vals, zoom = 6) {
  bb <- raster::extent(
    bbox_vals["xmin"],
    bbox_vals["xmax"],
    bbox_vals["ymin"],
    bbox_vals["ymax"]
  )
  
  r <- rosm::osm.raster(
    x = bb,
    zoom = zoom,
    type = "cartolight",
    crop = TRUE,
    projection = sp::CRS("EPSG:4326")
  )
  
  rr <- terra::rast(r)
  terra::crs(rr) <- "EPSG:4326"
  rr_3857 <- terra::project(rr, "EPSG:3857", method = "near")
  
  df <- as.data.frame(rr_3857, xy = TRUE, na.rm = FALSE)
  
  band_cols <- setdiff(names(df), c("x", "y"))
  if (length(band_cols) < 3) {
    stop("Basemap raster did not return at least 3 color bands.")
  }
  
  names(df)[match(band_cols[1:3], names(df))] <- c("red", "green", "blue")
  
  df$fill <- grDevices::rgb(
    df$red, df$green, df$blue,
    maxColorValue = 255
  )
  
  df
}

bbox_geo <- st_bbox(adm0_map %>% filter(adm0_name != "UGANDA"))
carto_df_3857 <- get_cartolight_df_3857(bbox_geo, zoom = 6)

# -----------------------------
# EPID cleaning helper
# -----------------------------
clean_epid <- function(x) {
  x %>%
    str_to_upper() %>%
    str_trim() %>%
    str_replace_all("/", "-") %>%
    str_replace_all("\\s+", "") %>%
    str_replace_all("-{2,}", "-") %>%
    str_replace_all("^-|-$", "")
}

# -----------------------------
# Clean genetic match dataset
# -----------------------------
genetic_match_data <- genetic_match_data %>%
  janitor::clean_names()

genetic_match_data2 <- genetic_match_data %>%
  mutate(
    epid_clean = clean_epid(epid),
    closest_match_epid_clean = closest_match_epid_at_time_of_reporting %>%
      str_trim() %>%
      na_if("") %>%
      na_if("confidential") %>%
      str_extract("^[A-Z0-9/-]+") %>%
      clean_epid()
  )

# -----------------------------
# Clean virus dataset
# -----------------------------
virus_data2 <- virus_data %>%
  mutate(
    epid_clean = clean_epid(EPID),
    epid_closest_clean = clean_epid(EPIDClosestMatch)
  )

# -----------------------------
# Manual harmonization for known mismatches
# -----------------------------
virus_epid_recode <- c(
  "ETH-HAR-HAR-25-0238" = "ETH-ORO-HAR-25-0238",
  "ETH-HAR-HAR-24-1605" = "ETH-ORO-HAR-24-1605",
  "ETH-ORO-WGU-24-1633" = "ETH-ORO-WGJ-24-1633",
  "ETH-ORO-WGU-24-1634" = "ETH-ORO-WGJ-24-1634",
  "ETH-ORO-WGU-24-1635" = "ETH-ORO-WGJ-24-1635",
  "ETH-SOM-GOD-25-0055" = "ETH-SOM-SHB-25-0055",
  "SOM-BRI-BOS-24-014" = "SOM-BRI-BOS-25-001",
  "SOM-BRI-BOS-24-015" = "SOM-BRI-BOS-25-002"
)

virus_data2 <- virus_data2 %>%
  mutate(
    epid_clean_for_match = recode(
      epid_clean,
      !!!virus_epid_recode,
      .default = epid_clean
    )
  )

# -----------------------------
# Build genetic links using cleaned IDs
# -----------------------------
epid_links <- genetic_match_data2 %>%
  filter(!is.na(closest_match_epid_clean)) %>%
  distinct(
    epid_clean,
    closest_match_epid_clean
  )

linked_epids <- epid_links %>%
  pivot_longer(
    cols = c(epid_clean, closest_match_epid_clean),
    values_to = "epid_clean_for_match"
  ) %>%
  distinct(epid_clean_for_match)

# -----------------------------
# Keep all virus records that are:
# 1) SOM-BAN-1
# 2) or linked through the genetic match table
# -----------------------------
virus_subset <- virus_data2 %>%
  filter(
    VdpvEmergenceGroupCode == "SOM-BAN-1" |
      epid_clean_for_match %in% linked_epids$epid_clean_for_match
  )

# -----------------------------
# Keep all virus records that are:
# 1) SOM-BAN-1
# 2) or linked through the genetic match table
# -----------------------------
virus_subset <- virus_data2 %>%
  filter(
    VdpvEmergenceGroupCode == "SOM-BAN-1" |
      epid_clean_for_match %in% linked_epids$epid_clean_for_match
  )

# -----------------------------
# Build point dataset
# -----------------------------
virus_points <- virus_subset %>%
  transmute(
    EPID,
    epid_clean,
    epid_clean_for_match,
    emergence_group = VdpvEmergenceGroupCode,
    virus_date = as.Date(`Virus Date`),
    surveillance_type = `Surveillance Type`,
    lon = X,
    lat = Y
  ) %>%
  filter(
    !is.na(EPID),
    !is.na(lon),
    !is.na(lat),
    !is.na(virus_date)
  ) %>%
  distinct(epid_clean_for_match, .keep_all = TRUE) %>%
  mutate(
    period_group = if_else(
      virus_date < as.Date("2023-01-01"),
      "Before 2023",
      "2023-Present"
    ),
    surveillance_group = case_when(
      str_to_lower(surveillance_type) %in% c("afp", "afp case") ~ "AFP",
      str_to_lower(surveillance_type) %in% c("environmental", "env") ~ "Environmental",
      TRUE ~ as.character(surveillance_type)
    )
  )

# -----------------------------
# Project points to EPSG:3857 and jitter ~5 km
# -----------------------------
set.seed(123)

jitter_distance_m <- 5000

virus_points_sf <- virus_points %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3857) %>%
  st_jitter(amount = jitter_distance_m)

virus_points_plot <- virus_points_sf %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# -----------------------------
# Keep only valid links where both ends exist in plotted virus data
# -----------------------------
epid_links_subset <- epid_links %>%
  filter(
    epid_clean %in% virus_points_plot$epid_clean_for_match,
    closest_match_epid_clean %in% virus_points_plot$epid_clean_for_match
  ) %>%
  distinct(epid_clean, closest_match_epid_clean)

# -----------------------------
# Join jittered coordinates and dates for both ends
# -----------------------------
line_df_raw <- epid_links_subset %>%
  left_join(
    virus_points_plot %>%
      dplyr::select(
        epid_clean_for_match,
        epid1_display = EPID,
        epid1_x = x,
        epid1_y = y,
        epid1_date = virus_date
      ),
    by = c("epid_clean" = "epid_clean_for_match")
  ) %>%
  left_join(
    virus_points_plot %>%
      dplyr::select(
        epid_clean_for_match,
        epid2_display = EPID,
        epid2_x = x,
        epid2_y = y,
        epid2_date = virus_date
      ),
    by = c("closest_match_epid_clean" = "epid_clean_for_match")
  ) %>%
  filter(
    !is.na(epid1_x),
    !is.na(epid1_y),
    !is.na(epid1_date),
    !is.na(epid2_x),
    !is.na(epid2_y),
    !is.na(epid2_date)
  )

# -----------------------------
# Force arrow direction to run from earlier virus date to later virus date
# -----------------------------
line_df <- line_df_raw %>%
  mutate(
    from_epid = if_else(epid1_date <= epid2_date, epid1_display, epid2_display),
    to_epid   = if_else(epid1_date <= epid2_date, epid2_display, epid1_display),
    
    from_date = pmin(epid1_date, epid2_date),
    to_date   = pmax(epid1_date, epid2_date),
    
    from_x = if_else(epid1_date <= epid2_date, epid1_x, epid2_x),
    from_y = if_else(epid1_date <= epid2_date, epid1_y, epid2_y),
    to_x   = if_else(epid1_date <= epid2_date, epid2_x, epid1_x),
    to_y   = if_else(epid1_date <= epid2_date, epid2_y, epid1_y)
  ) %>%
  distinct(from_epid, to_epid, .keep_all = TRUE)

# -----------------------------
# Shorten arrows so they stop before the target point
# -----------------------------
line_plot <- line_df %>%
  mutate(
    dx = to_x - from_x,
    dy = to_y - from_y,
    seg_len = sqrt(dx^2 + dy^2)
  ) %>%
  filter(seg_len > 0) %>%
  mutate(
    arrow_gap = 14000,
    arrow_gap = pmin(arrow_gap, seg_len * 0.35),
    xend_short = to_x - (dx / seg_len) * arrow_gap,
    yend_short = to_y - (dy / seg_len) * arrow_gap,
    curvature = if_else(row_number() %% 2 == 0, 0.18, -0.18)
  )

# -----------------------------
# Plot
# -----------------------------
p_genetic <- ggplot() +
  geom_raster(
    data = carto_df_3857,
    aes(x = x, y = y, fill = fill)
  ) +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  geom_sf(
    data = adm1_map_proj %>% filter(enddate >= "2026-12-31") %>% filter(adm0_name != "UGANDA") ,
    color = "grey80",
    fill = NA,
    linewidth = 0.25
  ) +
  geom_sf(
    data = adm0_map_proj %>% filter(enddate >= "2026-12-31") %>% filter(adm0_name != "UGANDA") ,
    fill = NA,
    color = "grey50",
    linewidth = 1
  ) +
  geom_curve(
    data = line_plot,
    aes(
      x = from_x,
      y = from_y,
      xend = xend_short,
      yend = yend_short
      # , curvature = curvature   # uncomment if line_plot already has a curvature column
    ),
    curvature = 0.18,
    linewidth = 1,
    alpha = 0.6,
    arrow = arrow(
      length = unit(0.08, "inches"),
      type = "open"
    ),
    lineend = "round"
  ) +
  geom_point(
    data = virus_points_plot %>%
      filter(period_group == "Before 2023"),
    aes(
      x = x,
      y = y,
      shape = surveillance_group,
      fill = period_group
    ),
    color = "black",
    size = 3,
    stroke = 0.4
  ) +
  geom_point(
    data = virus_points_plot %>%
      filter(period_group == "2023-Present"),
    aes(
      x = x,
      y = y,
      shape = surveillance_group,
      fill = period_group
    ),
    color = "black",
    size = 3,
    stroke = 0.4,
    alpha = 0.8
  ) +
  scale_shape_manual(
    values = c(
      "AFP" = 21,
      "Environmental" = 24,
      "Community" = 22,
      "Contact" = 23,
      "Healthy" = 25
    )
  ) +
  scale_fill_manual(
    values = c(
      "Before 2023" = "#A68A64",
      "2023-Present" = "firebrick"
    ),
    name = "Virus date",
    guide = guide_legend(
      override.aes = list(
        alpha = c(1, 0.8),
        color = "black",
        shape = 21,
        size = 3,
        stroke = 0.4
      )
    )
  ) +
  coord_sf(
    crs = st_crs(3857),
    expand = FALSE
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.98, 0.02),
    legend.justification = c(1, 0),
    legend.background = element_rect(
      fill = "#D4DADC",
      color = "#D4DADC",
      linewidth = 0
    ),
    plot.subtitle = element_text(size = 9)
  ) +
  labs(
    title = "Genetic linkages among cVDPV2 detections in the SOM-BAN-1 emergence group",
    subtitle = paste0(
      "Arrows connect each detection to its genetically closest known relative; ",
      "arrow direction runs from earlier to later virus detection dates.\n",
      "Genetic matching is available only for detections from 2023 onward; ",
      "earlier detections are shown for epidemiologic context."
    ),
    shape = "Surveillance type",
    fill = "Virus detection period",
    caption = "Source: POLIS virus dataset and CDC genetic sequencing linkage data."
  )

p_genetic

ggsave(
  filename = "genetic_linkage_map.png",
  plot = p_genetic,
  width = 8.5,
  height = 11,
  units = "in",
  dpi = 600,
  bg = "white"
)


# source_sink -------------------------------------------------------------


library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(ggnewscale)
library(grid)

# -----------------------------
# Helper to standardize admin text
# -----------------------------
clean_admin <- function(x) {
  x %>%
    stringr::str_to_upper() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s+", " ")
}

# -----------------------------
# Keep all virus records that are:
# 1) SOM-BAN-1
# 2) or linked through the genetic match table
# -----------------------------
virus_subset <- virus_data2 %>%
  filter(
    VdpvEmergenceGroupCode == "SOM-BAN-1" |
      epid_clean_for_match %in% linked_epids$epid_clean_for_match
  )

# -----------------------------
# Build virus point/admin dataset
# -----------------------------
virus_points <- virus_subset %>%
  transmute(
    EPID,
    epid_clean,
    epid_clean_for_match,
    virus_date = as.Date(`Virus Date`),
    surveillance_type = `Surveillance Type`,
    admin0 = Admin0OfficialName,
    admin1 = Admin1OfficialName,
    lon = X,
    lat = Y
  ) %>%
  filter(
    !is.na(EPID),
    !is.na(epid_clean_for_match),
    !is.na(virus_date),
    !is.na(admin0),
    !is.na(admin1)
  ) %>%
  distinct(epid_clean_for_match, .keep_all = TRUE) %>%
  mutate(
    admin0_clean = clean_admin(admin0),
    admin1_clean = clean_admin(admin1),
    adm1_key = paste(admin0_clean, admin1_clean, sep = " | ")
  ) %>%
  mutate(adm1_key = case_when(adm1_key == "ETHIOPIA | OROMIIA" ~ "ETHIOPIA | OROMIYA",
                              TRUE ~ adm1_key)) %>%
  mutate(
    adm1_key = case_when(
      adm1_key == "ETHIOPIA | OROMIIA"      ~ "ETHIOPIA | OROMIYA",
      adm1_key == "ETHIOPIA | SOUTH ETHIOPIA" ~ "ETHIOPIA | SNNP",
      TRUE ~ adm1_key
    )
  )
  

# -----------------------------
# Build current ADM1 polygons with matching keys
# -----------------------------
adm1_polygons <- adm1_map_proj %>%
  filter(enddate >= "2026-12-31") %>%
  mutate(
    admin0_clean = clean_admin(adm0_name),
    admin1_clean = clean_admin(adm1_name),
    adm1_key = paste(admin0_clean, admin1_clean, sep = " | ")
  )

adm0_polygons <- adm0_map_proj %>%
  filter(enddate >= "2026-12-31")

# -----------------------------
# Aggregate detections by ADM1
# -----------------------------
adm1_detection_counts <- virus_points %>%
  filter(virus_date >= as.Date("2023-01-01")) %>%
  count(adm1_key, name = "n_detections")

# -----------------------------
# Create ADM1 centroid layer for mapped detections
# -----------------------------
adm1_centroids <- adm1_polygons %>%
  left_join(adm1_detection_counts, by = "adm1_key") %>%
  mutate(
    n_detections = if_else(is.na(n_detections), 0L, n_detections)
  ) %>%
  filter(n_detections > 0) %>%
  st_point_on_surface() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  )

# -----------------------------
# Keep only valid EPID links where both ends exist in virus data
# -----------------------------
epid_links_subset <- epid_links %>%
  filter(
    epid_clean %in% virus_points$epid_clean_for_match,
    closest_match_epid_clean %in% virus_points$epid_clean_for_match
  ) %>%
  distinct(epid_clean, closest_match_epid_clean)

# -----------------------------
# Join ADM1 and date metadata to both ends of each EPID pair
# -----------------------------
flow_links_raw <- epid_links_subset %>%
  left_join(
    virus_points %>%
      dplyr::select(
        epid_clean_for_match,
        epid1_display = EPID,
        epid1_date = virus_date,
        epid1_adm1_key = adm1_key
      ),
    by = c("epid_clean" = "epid_clean_for_match")
  ) %>%
  left_join(
    virus_points %>%
      dplyr::select(
        epid_clean_for_match,
        epid2_display = EPID,
        epid2_date = virus_date,
        epid2_adm1_key = adm1_key
      ),
    by = c("closest_match_epid_clean" = "epid_clean_for_match")
  ) %>%
  filter(
    !is.na(epid1_date),
    !is.na(epid2_date),
    !is.na(epid1_adm1_key),
    !is.na(epid2_adm1_key)
  )

# -----------------------------
# Force flow direction from earlier to later virus date
# -----------------------------
flow_links_directed <- flow_links_raw %>%
  mutate(
    from_adm1_key = if_else(epid1_date <= epid2_date, epid1_adm1_key, epid2_adm1_key),
    to_adm1_key   = if_else(epid1_date <= epid2_date, epid2_adm1_key, epid1_adm1_key),
    from_date     = pmin(epid1_date, epid2_date),
    to_date       = pmax(epid1_date, epid2_date)
  )

# -----------------------------
# Aggregate flows between ADM1 areas
# Exclude within-ADM1 links from flow lines
# -----------------------------
adm1_flows <- flow_links_directed %>%
  filter(from_adm1_key != to_adm1_key) %>%
  count(from_adm1_key, to_adm1_key, name = "flow_n")

# -----------------------------
# Build ADM1 centroid lookup for lines
# -----------------------------
adm1_centroid_lookup <- adm1_polygons %>%
  st_point_on_surface() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  dplyr::select(adm1_key, x, y)

# -----------------------------
# Join centroid coordinates to flow table
# -----------------------------
flow_plot <- adm1_flows %>%
  left_join(
    adm1_centroid_lookup %>%
      rename(
        from_x = x,
        from_y = y
      ),
    by = c("from_adm1_key" = "adm1_key")
  ) %>%
  left_join(
    adm1_centroid_lookup %>%
      rename(
        to_x = x,
        to_y = y
      ),
    by = c("to_adm1_key" = "adm1_key")
  ) %>%
  filter(
    !is.na(from_x),
    !is.na(from_y),
    !is.na(to_x),
    !is.na(to_y)
  ) %>%
  mutate(
    dx = to_x - from_x,
    dy = to_y - from_y,
    seg_len = sqrt(dx^2 + dy^2)
  ) %>%
  filter(seg_len > 0) %>%
  mutate(
    base_gap = 35000,
    
    arrow_gap = case_when(
      from_adm1_key == "SOMALIA | BANADIR" &
        to_adm1_key == "KENYA | GARISSA" ~ base_gap * 1.8,
      TRUE ~ base_gap
    ),
    
    arrow_gap = pmin(arrow_gap, seg_len * 0.5),
    
    xend_short = to_x - (dx / seg_len) * arrow_gap,
    yend_short = to_y - (dy / seg_len) * arrow_gap,
    
    xend_short = case_when(
      from_adm1_key == "SOMALIA | BANADIR" &
        to_adm1_key == "KENYA | GARISSA" ~ xend_short,
      TRUE ~ xend_short
    ),
    
    yend_short = case_when(
      from_adm1_key == "SOMALIA | BANADIR" &
        to_adm1_key == "KENYA | GARISSA" ~ yend_short - 35000,
      TRUE ~ yend_short
    )
  ) %>%
  mutate(
    curvature = case_when(
      from_adm1_key == "SOMALIA | BANADIR"   & to_adm1_key == "KENYA | GARISSA"  ~ -0.5,
      from_adm1_key == "SOMALIA | BAY"       & to_adm1_key == "KENYA | GARISSA"  ~ 0,
      from_adm1_key == "SOMALIA | LOWER SHABELLE"  & to_adm1_key == "KENYA | GARISSA"    ~ -0.5,
      from_adm1_key == "KENYA | NAIROBI"     & to_adm1_key == "ETHIOPIA | SOMALI"  ~ -0.2,
      from_adm1_key == "ETHIOPIA | SOMALI"   & to_adm1_key == "ETHIOPIA | OROMIYA"  ~ -0.2,
      from_adm1_key == "ETHIOPIA | OROMIYA"  & to_adm1_key == "SOMALIA | BARI"     ~ -0.2,
      from_adm1_key == "ETHIOPIA | OROMIYA"  & to_adm1_key == "SOMALIA | MIDDLE JUBA"    ~ -0.3,
      from_adm1_key == "SOMALIA | BANADIR"  & to_adm1_key == "SOMALIA | GEDO"    ~ 0.5,
      from_adm1_key == "SOMALIA | BAY"  & to_adm1_key == "SOMALIA | GEDO"    ~ -0.4,
      from_adm1_key == "SOMALIA | BAY"  & to_adm1_key == "SOMALIA | BANADIR"    ~ -0.4,
      TRUE ~ 0.08
    )
  )

# -----------------------------
# Plot
# -----------------------------


adm1_centroids <- adm1_centroids %>%
  mutate(
    detection_category = case_when(
      n_detections <= 2  ~ "1–2",
      n_detections <= 6  ~ "3–6",
      n_detections <= 19 ~ "7–19",
      TRUE               ~ "20+"
    )
  ) %>%
  mutate(detection_category = factor(detection_category, levels=c("1–2", "3–6", "7–19", "20+")))

# -----------------------------
# Build curve layers functionally
# -----------------------------
curve_layers <- flow_plot %>%
  split(.$curvature) %>%
  imap(~ geom_curve(
    data = .x,
    aes(
      x = from_x,
      y = from_y,
      xend = xend_short,
      yend = yend_short,
      linewidth = flow_n
    ),
    curvature = as.numeric(.y),
    color = "black",
    alpha = 1,
    lineend = "butt",
    arrow = arrow(
      length = unit(0.10, "inches"),
      type = "closed"
    ),
    show.legend = FALSE
  ))

p_adm1_flow <- ggplot() +
  geom_raster(
    data = carto_df_3857,
    aes(x = x, y = y, fill = fill)
  ) +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  geom_sf(
    data = adm1_polygons %>% filter(adm0_name != "UGANDA"),
    color = "grey70",
    fill = NA,
    linewidth = 0.25
  ) +
  geom_sf(
    data = adm0_polygons  %>% filter(adm0_name != "UGANDA"),
    fill = NA,
    color = "grey30",
    linewidth = 1
  )

# add all curvature-specific arrow layers
for (layer in curve_layers) {
  p_adm1_flow <- p_adm1_flow + layer
}

p_adm1_flow <- p_adm1_flow +
  # invisible legend-only line layer
  geom_segment(
    data = flow_plot,
    aes(
      x = from_x,
      y = from_y,
      xend = from_x + 1,
      yend = from_y,
      linewidth = flow_n
    ),
    color = "black",
    alpha = 0,
    show.legend = TRUE
  ) +
  scale_linewidth_continuous(
    range = c(0.5, 3),
    breaks = c(1, 2, 3, 4),
    name = "Number of linked detections",
    guide = guide_legend(
      override.aes = list(
        alpha = 1,
        color = "black",
        linetype = 1
      ),
      keywidth = unit(1.5, "cm")
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_point(
    data = adm1_centroids,
    aes(
      x = x,
      y = y,
      size = detection_category
    ),
    shape = 21,
    fill = "firebrick",
    color = "black",
    stroke = 0.35,
    alpha = 0.9
  ) +
  scale_size_manual(
    values = c(
      "1–2" = 3,
      "3–6" = 5,
      "7–19" = 7,
      "20+" = 10
    ),
    name = "Number of detections"
  ) +
  coord_sf(
    crs = st_crs(3857),
    expand = FALSE
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.subtitle = element_text(size = 9),
    legend.position = c(0.98, 0.02),
    legend.justification = c(1, 0),
    legend.spacing.y = unit(0.1, "cm"),
    legend.box.spacing = unit(0.05, "cm"),
    legend.background = element_rect(
      fill = "#D4DADC",
      color = "#D4DADC",
      linewidth = 0
    )
  ) +
  labs(
    title = "Aggregated cVDPV2 detections of SOM-BAN-1 emergence group at the Admin-1 Level since 2023,\nwith arrows indicating number of linked detections from other Admin-1 areas.",
    subtitle = paste0(
      "Points show the number of human and environmental detections aggregated within each Admin-1 area. ",
      "Arrows represent aggregated genetic\nlinkages among 2023-present detections, ",
      "with arrow direction running from earlier to later virus detection dates and line width proportional\nto the number of linked pairs."
    ),
    caption = "Source: POLIS virus dataset and CDC genetic sequencing linkage data."
  )

# p_adm1_flow

ggsave(
  filename = "adm1_aggregated_flow_map.png",
  plot = p_adm1_flow,
  width = 8.5,
  height = 11,
  units = "in",
  dpi = 600,
  bg = "white"
)



#Check for bidirectional pairs

bidirectional_pairs <- adm1_flows %>%
  inner_join(
    adm1_flows,
    by = c(
      "from_adm1_key" = "to_adm1_key",
      "to_adm1_key" = "from_adm1_key"
    ),
    suffix = c("_ab", "_ba")
  ) %>%
  filter(from_adm1_key < to_adm1_key)

bidirectional_pairs
nrow(bidirectional_pairs)
