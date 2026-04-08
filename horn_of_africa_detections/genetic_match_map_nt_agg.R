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
library(scales)

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

who_gdb_adm1_province <- sf::st_read(gdb_path, "GLOBAL_ADM1", quiet = TRUE) %>%
  janitor::clean_names()

who_gdb_adm0_country <- sf::st_read(gdb_path, "GLOBAL_ADM0", quiet = TRUE) %>%
  janitor::clean_names()

# -----------------------------
# Keep Horn / nearby countries
# -----------------------------
map_countries <- c("DJIBOUTI", "ERITREA", "ETHIOPIA", "KENYA", "SOMALIA", "UGANDA")

adm1_map <- who_gdb_adm1_province %>% filter(toupper(adm0_name) %in% map_countries)
adm0_map <- who_gdb_adm0_country  %>% filter(toupper(adm0_name) %in% map_countries)

plot_crs      <- 3857
adm1_map_proj <- st_transform(adm1_map, plot_crs)
adm0_map_proj <- st_transform(adm0_map, plot_crs)

# -----------------------------
# Carto Light basemap
# -----------------------------
get_cartolight_df_3857 <- function(bbox_vals, zoom = 6) {
  bb <- raster::extent(bbox_vals["xmin"], bbox_vals["xmax"],
                       bbox_vals["ymin"], bbox_vals["ymax"])
  r        <- rosm::osm.raster(x = bb, zoom = zoom, type = "cartolight",
                               crop = TRUE, projection = sp::CRS("EPSG:4326"))
  rr       <- terra::rast(r)
  terra::crs(rr) <- "EPSG:4326"
  rr_3857  <- terra::project(rr, "EPSG:3857", method = "near")
  df       <- as.data.frame(rr_3857, xy = TRUE, na.rm = FALSE)
  band_cols <- setdiff(names(df), c("x", "y"))
  names(df)[match(band_cols[1:3], names(df))] <- c("red", "green", "blue")
  df$fill  <- grDevices::rgb(df$red, df$green, df$blue, maxColorValue = 255)
  df
}

bbox_geo      <- st_bbox(adm0_map %>% filter(adm0_name != "UGANDA"))
carto_df_3857 <- get_cartolight_df_3857(bbox_geo, zoom = 6)

# -----------------------------
# EPID cleaning helper
# -----------------------------
clean_epid <- function(x) {
  x %>%
    str_to_upper() %>% str_trim() %>%
    str_replace_all("/", "-") %>%
    str_replace_all("\\s+", "") %>%
    str_replace_all("-{2,}", "-") %>%
    str_replace_all("^-|-$", "")
}

clean_admin <- function(x) {
  x %>% str_to_upper() %>% str_trim() %>% str_replace_all("\\s+", " ")
}

# -----------------------------
# Clean genetic match dataset — keep NT diff
# -----------------------------
genetic_match_data <- genetic_match_data %>% janitor::clean_names()

genetic_match_data2 <- genetic_match_data %>%
  mutate(
    epid_clean = clean_epid(epid),
    closest_match_epid_clean = closest_match_epid_at_time_of_reporting %>%
      str_trim() %>%
      na_if("") %>%
      na_if("confidential") %>%
      str_extract("^[A-Z0-9/-]+") %>%
      clean_epid(),
    nt_diff = suppressWarnings(
      as.integer(nt_diff_from_closest_match_at_time_of_reporting)
    )
  )

# -----------------------------
# Clean virus dataset
# -----------------------------
virus_data2 <- virus_data %>%
  mutate(
    epid_clean         = clean_epid(EPID),
    epid_closest_clean = clean_epid(EPIDClosestMatch)
  )

virus_epid_recode <- c(
  "ETH-HAR-HAR-25-0238" = "ETH-ORO-HAR-25-0238",
  "ETH-HAR-HAR-24-1605" = "ETH-ORO-HAR-24-1605",
  "ETH-ORO-WGU-24-1633" = "ETH-ORO-WGJ-24-1633",
  "ETH-ORO-WGU-24-1634" = "ETH-ORO-WGJ-24-1634",
  "ETH-ORO-WGU-24-1635" = "ETH-ORO-WGJ-24-1635",
  "ETH-SOM-GOD-25-0055" = "ETH-SOM-SHB-25-0055",
  "SOM-BRI-BOS-24-014"  = "SOM-BRI-BOS-25-001",
  "SOM-BRI-BOS-24-015"  = "SOM-BRI-BOS-25-002"
)

virus_data2 <- virus_data2 %>%
  mutate(
    epid_clean_for_match = recode(epid_clean, !!!virus_epid_recode, .default = epid_clean)
  )

# -----------------------------
# Build genetic links (with NT diff)
# -----------------------------
epid_links <- genetic_match_data2 %>%
  filter(!is.na(closest_match_epid_clean)) %>%
  distinct(epid_clean, closest_match_epid_clean, nt_diff)

linked_epids <- epid_links %>%
  pivot_longer(cols = c(epid_clean, closest_match_epid_clean),
               values_to = "epid_clean_for_match") %>%
  distinct(epid_clean_for_match)

# -----------------------------
# Virus subset & admin dataset
# -----------------------------
virus_subset <- virus_data2 %>%
  filter(
    VdpvEmergenceGroupCode == "SOM-BAN-1" |
      epid_clean_for_match %in% linked_epids$epid_clean_for_match
  )

virus_points <- virus_subset %>%
  transmute(
    EPID, epid_clean, epid_clean_for_match,
    virus_date        = as.Date(`Virus Date`),
    surveillance_type = `Surveillance Type`,
    admin0 = Admin0OfficialName,
    admin1 = Admin1OfficialName,
    lon = X, lat = Y
  ) %>%
  filter(
    !is.na(EPID), !is.na(epid_clean_for_match),
    !is.na(virus_date), !is.na(admin0), !is.na(admin1)
  ) %>%
  distinct(epid_clean_for_match, .keep_all = TRUE) %>%
  mutate(
    admin0_clean = clean_admin(admin0),
    admin1_clean = clean_admin(admin1),
    adm1_key     = paste(admin0_clean, admin1_clean, sep = " | ")
  ) %>%
  mutate(
    adm1_key = case_when(
      adm1_key == "ETHIOPIA | OROMIIA" ~ "ETHIOPIA | OROMIYA",
      TRUE ~ adm1_key
    )
  ) %>%
  mutate(
    adm1_key = case_when(
      adm1_key == "ETHIOPIA | OROMIIA"      ~ "ETHIOPIA | OROMIYA",
      adm1_key == "ETHIOPIA | SOUTH ETHIOPIA" ~ "ETHIOPIA | SNNP",
      TRUE ~ adm1_key
    )
  )

# -----------------------------
# ADM1 polygon layers
# -----------------------------
adm1_polygons <- adm1_map_proj %>%
  filter(enddate >= "2026-12-31") %>%
  mutate(
    admin0_clean = clean_admin(adm0_name),
    admin1_clean = clean_admin(adm1_name),
    adm1_key     = paste(admin0_clean, admin1_clean, sep = " | ")
  )

adm0_polygons <- adm0_map_proj %>%
  filter(enddate >= "2026-12-31")

# -----------------------------
# Aggregate detections by ADM1 (2023+)
# -----------------------------
adm1_detection_counts <- virus_points %>%
  filter(virus_date >= as.Date("2023-01-01")) %>%
  count(adm1_key, name = "n_detections")

adm1_centroids <- adm1_polygons %>%
  left_join(adm1_detection_counts, by = "adm1_key") %>%
  mutate(n_detections = if_else(is.na(n_detections), 0L, n_detections)) %>%
  filter(n_detections > 0) %>%
  st_point_on_surface() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2],
    detection_category = case_when(
      n_detections <= 2  ~ "1–2",
      n_detections <= 6  ~ "3–6",
      n_detections <= 19 ~ "7–19",
      TRUE               ~ "20+"
    ),
    detection_category = factor(detection_category, levels = c("1–2", "3–6", "7–19", "20+"))
  )

# -----------------------------
# Build flow links with NT diff
# -----------------------------
epid_links_subset <- epid_links %>%
  filter(
    epid_clean %in% virus_points$epid_clean_for_match,
    closest_match_epid_clean %in% virus_points$epid_clean_for_match
  ) %>%
  distinct(epid_clean, closest_match_epid_clean, nt_diff)

flow_links_raw <- epid_links_subset %>%
  left_join(
    virus_points %>%
      dplyr::select(epid_clean_for_match, epid1_display = EPID,
                    epid1_date = virus_date, epid1_adm1_key = adm1_key),
    by = c("epid_clean" = "epid_clean_for_match")
  ) %>%
  left_join(
    virus_points %>%
      dplyr::select(epid_clean_for_match, epid2_display = EPID,
                    epid2_date = virus_date, epid2_adm1_key = adm1_key),
    by = c("closest_match_epid_clean" = "epid_clean_for_match")
  ) %>%
  filter(
    !is.na(epid1_date), !is.na(epid2_date),
    !is.na(epid1_adm1_key), !is.na(epid2_adm1_key)
  )

# Force flow direction: earlier → later date
flow_links_directed <- flow_links_raw %>%
  mutate(
    from_adm1_key = if_else(epid1_date <= epid2_date, epid1_adm1_key, epid2_adm1_key),
    to_adm1_key   = if_else(epid1_date <= epid2_date, epid2_adm1_key, epid1_adm1_key),
    from_date     = pmin(epid1_date, epid2_date),
    to_date       = pmax(epid1_date, epid2_date)
  )

# -----------------------------
# Aggregate flows: count + median NT diff per ADM1 pair
# -----------------------------
adm1_flows <- flow_links_directed %>%
  filter(from_adm1_key != to_adm1_key) %>%
  group_by(from_adm1_key, to_adm1_key) %>%
  summarise(
    flow_n  = n(),
    med_nt  = median(nt_diff, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# ADM1 centroid coordinate lookup
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
# Join centroids to flow table
# -----------------------------
flow_plot <- adm1_flows %>%
  left_join(
    adm1_centroid_lookup %>% rename(from_x = x, from_y = y),
    by = c("from_adm1_key" = "adm1_key")
  ) %>%
  left_join(
    adm1_centroid_lookup %>% rename(to_x = x, to_y = y),
    by = c("to_adm1_key" = "adm1_key")
  ) %>%
  filter(!is.na(from_x), !is.na(from_y), !is.na(to_x), !is.na(to_y)) %>%
  mutate(
    dx      = to_x - from_x,
    dy      = to_y - from_y,
    seg_len = sqrt(dx^2 + dy^2)
  ) %>%
  filter(seg_len > 0) %>%
  mutate(
    base_gap  = 35000,
    arrow_gap = case_when(
      from_adm1_key == "SOMALIA | BANADIR" &
        to_adm1_key == "KENYA | GARISSA" ~ base_gap * 1.8,
      TRUE ~ base_gap
    ),
    arrow_gap  = pmin(arrow_gap, seg_len * 0.5),
    xend_short = to_x - (dx / seg_len) * arrow_gap,
    yend_short = to_y - (dy / seg_len) * arrow_gap,
    yend_short = case_when(
      from_adm1_key == "SOMALIA | BANADIR" &
        to_adm1_key == "KENYA | GARISSA" ~ yend_short - 35000,
      TRUE ~ yend_short
    ),
    curvature = case_when(
      from_adm1_key == "SOMALIA | BANADIR"      & to_adm1_key == "KENYA | GARISSA"       ~ -0.5,
      from_adm1_key == "SOMALIA | BAY"           & to_adm1_key == "KENYA | GARISSA"       ~  0,
      from_adm1_key == "SOMALIA | LOWER SHABELLE"& to_adm1_key == "KENYA | GARISSA"       ~ -0.5,
      from_adm1_key == "KENYA | NAIROBI"         & to_adm1_key == "ETHIOPIA | SOMALI"     ~ -0.2,
      from_adm1_key == "ETHIOPIA | SOMALI"       & to_adm1_key == "ETHIOPIA | OROMIYA"    ~ -0.2,
      from_adm1_key == "ETHIOPIA | OROMIYA"      & to_adm1_key == "SOMALIA | BARI"        ~ -0.2,
      from_adm1_key == "ETHIOPIA | OROMIYA"      & to_adm1_key == "SOMALIA | MIDDLE JUBA" ~ -0.3,
      from_adm1_key == "SOMALIA | BANADIR"       & to_adm1_key == "SOMALIA | GEDO"        ~  0.5,
      from_adm1_key == "SOMALIA | BAY"           & to_adm1_key == "SOMALIA | GEDO"        ~ -0.4,
      from_adm1_key == "SOMALIA | BAY"           & to_adm1_key == "SOMALIA | BANADIR"     ~ -0.4,
      TRUE ~ 0.08
    )
  ) %>%
  # Add to flow_plot mutate chain, after curvature:
  mutate(med_nt_bin = case_when(
    is.na(med_nt)   ~ "Unknown",
    med_nt == 0     ~ "0",
    med_nt <= 5     ~ "1–5",
    med_nt <= 14     ~ "6–14",
    TRUE            ~ "15+"
  ),
med_nt_bin = factor(med_nt_bin, levels = c("0", "1–5", "6–14", "15+", "Unknown")))

# -----------------------------
# Build curve layers split by curvature, colored by median NT diff
# -----------------------------
curve_layers <- flow_plot %>%
  split(.$curvature) %>%
  imap(~ geom_curve(
    data = .x,
    aes(
      x     = from_x,
      y     = from_y,
      xend  = xend_short,
      yend  = yend_short,
      linewidth = flow_n,
      color = med_nt_bin
    ),
    curvature = as.numeric(.y),
    alpha     = 0.9,
    lineend   = "butt",
    arrow     = arrow(length = unit(0.10, "inches"), type = "closed"),
    show.legend = TRUE
  ))

# -----------------------------
# Plot
# -----------------------------
p_adm1_flow <- ggplot() +
  geom_raster(data = carto_df_3857, aes(x = x, y = y, fill = fill)) +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  
  geom_sf(
    data = adm1_polygons %>% filter(adm0_name != "UGANDA"),
    color = "grey70", fill = NA, linewidth = 0.25
  ) +
  geom_sf(
    data = adm0_polygons %>% filter(adm0_name != "UGANDA"),
    fill = NA, color = "grey30", linewidth = 1
  )

# Add curvature-specific arrow layers
for (layer in curve_layers) {
  p_adm1_flow <- p_adm1_flow + layer
}

p_adm1_flow <- p_adm1_flow +
  
  # NT diff color scale (continuous green → red)
  scale_color_manual(
    values = c(
      "0"       = "#2c7bb6",
      "1–5"     = "#1a9641",
      "6–14"     = "orange",
      "15+"     = "firebrick",
      "Unknown" = "#bdbdbd"
    ),
    name  = "Median nucleotide\ndifferences",
    guide = guide_legend(
      override.aes = list(linewidth = 1.5),
      keywidth     = unit(1.2, "cm")
    ),
    drop = FALSE
  ) +
  
  # Arrow width scale (flow count) — invisible dummy for legend
  scale_linewidth_continuous(
    range  = c(0.5, 3),
    breaks = c(1, 2, 3, 4),
    name   = "Number of linked\ndetections",
    guide  = guide_legend(
      override.aes = list(alpha = 1, color = "grey40", linetype = 1),
      keywidth     = unit(1.5, "cm")
    )
  ) +
  
  ggnewscale::new_scale_fill() +
  
  # Detection bubbles
  geom_point(
    data  = adm1_centroids,
    aes(x = x, y = y, size = detection_category),
    shape  = 21,
    fill   = "pink",
    color  = "black",
    stroke = 0.35,
    alpha  = 0.9
  ) +
  scale_size_manual(
    values = c("1–2" = 3, "3–6" = 5, "7–19" = 7, "20+" = 10),
    name   = "Number of\ndetections"
  ) +
  
  coord_sf(crs = st_crs(3857), expand = FALSE) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid           = element_blank(),
    axis.title           = element_blank(),
    axis.text            = element_blank(),
    axis.ticks           = element_blank(),
    plot.subtitle        = element_text(size = 9),
    legend.position      = c(0.98, 0.02),
    legend.justification = c(1, 0),
    legend.spacing.y     = unit(0.15, "cm"),
    legend.box.spacing   = unit(0.05, "cm"),
    legend.background    = element_rect(
      fill = "#D4DADC", color = "#D4DADC", linewidth = 0
    )
  ) +
  labs(
    title    = "Aggregated cVDPV2 detections of SOM-BAN-1 emergence group at the Admin-1 level since 2023,\nwith arrows indicating genetic linkages between Admin-1 areas.",
    subtitle = paste0(
      "Points show aggregated human and environmental detections per Admin-1 area. ",
      "Arrows represent aggregated genetic linkages among 2023-present detections;\n",
      "arrow direction runs from earlier to later virus detection dates, ",
      "line width is proportional to the number of linked pairs, ",
      "and arrow color indicates\nthe median nucleotide (NT) differences between linked detections ",
      "(green = genetically close, red = divergent)."
    ),
    caption = "Source: POLIS virus dataset and CDC genetic sequencing linkage data."
  )

p_adm1_flow

ggsave(
  filename = "adm1_aggregated_flow_map_nt_diff.png",
  plot     = p_adm1_flow,
  width    = 11,
  height   = 11,
  units    = "in",
  dpi      = 600,
  bg       = "white"
)


flow_links_directed %>%
  mutate(
    detection_country = if_else(epid1_date >= epid2_date, epid1_adm1_key, epid2_adm1_key),
    source_country    = if_else(epid1_date >= epid2_date, epid2_adm1_key, epid1_adm1_key),
    detection_country = str_extract(detection_country, "^[^|]+") %>% str_trim(),
    source_country    = str_extract(source_country,    "^[^|]+") %>% str_trim(),
    nt_bin = case_when(
      is.na(nt_diff) ~ "Unknown",
      nt_diff == 0   ~ "0",
      nt_diff <= 5   ~ "1–5",
      nt_diff <= 14  ~ "6–14",
      TRUE           ~ "15+"
    ),
    nt_bin = factor(nt_bin, levels = c("0", "1–5", "6–14", "15+", "Unknown"))
  ) %>%
  count(detection_country, source_country, nt_bin) %>%
  pivot_wider(names_from = nt_bin, values_from = n, values_fill = 0) %>%
  arrange(detection_country, source_country) %>%
  dplyr::select(detection_country, source_country, `0`, `1–5`, `6–14`, `15+`) %>%
  flextable::flextable() %>%
  flextable::set_header_labels(
    detection_country = "Detection country",
    source_country    = "Source country",
    `0`    = "0",
    `1–5`  = "1–5",
    `6–14` = "6–14",
    `15+`  = "15+"
  ) %>%
  flextable::add_header_row(
    values    = c("", "Number of Detections by nucleotide differences from closest match"),
    colwidths = c(2, 4)
  ) %>%
  flextable::merge_h(part = "header") %>%
  flextable::align(part = "header", align = "center") %>%
  flextable::align(j = 1:6, align = "center", part = "header") %>%
  flextable::align(j = 3:6, align = "center", part = "body") %>%
  flextable::bold(part = "header") %>%
  flextable::theme_vanilla() %>%
  flextable::autofit()


ft <- flow_links_directed %>%
  mutate(
    detection_country = if_else(epid1_date >= epid2_date, epid1_adm1_key, epid2_adm1_key),
    source_country    = if_else(epid1_date >= epid2_date, epid2_adm1_key, epid1_adm1_key),
    detection_country = str_extract(detection_country, "^[^|]+") %>% str_trim(),
    source_country    = str_extract(source_country,    "^[^|]+") %>% str_trim(),
    link_type = if_else(detection_country == source_country, detection_country, "Cross-border")
  ) %>%
  filter(link_type != "Cross-border" & link_type != "DJIBOUTI") %>%
  group_by(link_type) %>%
  summarise(
    n_links   = n(),
    med_nt    = median(nt_diff, na.rm = TRUE),
    pct_high  = round(100 * mean(nt_diff >= 15, na.rm = TRUE)),
    .groups   = "drop"
  ) %>%
  arrange(desc(med_nt)) %>%
  rename(
    "Country"                        = link_type,
    "Within-country\nlinks (n)"       = n_links,
    "Median\nNt diff"                 = med_nt,
    "% links with\n≥15 Nt diff"        = pct_high
  ) %>%
  flextable::flextable() %>%
  flextable::add_header_row(
    values    = c("Within-country genetic linkages by country"),
    colwidths = c(4)
  ) %>%
  flextable::merge_h(part = "header") %>%
  flextable::theme_vanilla() %>%
  flextable::align(part = "header", align = "center") %>%
  flextable::align(j = 2:4, align = "center", part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::color(
    j = "Median\nNt diff",
    color = function(x) if_else(x >= 6, "firebrick", "darkgreen")
  ) %>%
  flextable::bold(j = "Median\nNt diff") %>%
  flextable::autofit()

flextable::save_as_image(ft, path = "within_country_nt_table.png",
                         webshot = "webshot2",
                         zoom = 3,
                         vwidth = 3 * 96,
                         vheight = 2 * 96)
