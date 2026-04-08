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

plot_crs <- 3857
adm1_map_proj <- st_transform(adm1_map, plot_crs)
adm0_map_proj <- st_transform(adm0_map, plot_crs)

# -----------------------------
# Carto Light basemap
# -----------------------------
get_cartolight_df_3857 <- function(bbox_vals, zoom = 6) {
  bb <- raster::extent(bbox_vals["xmin"], bbox_vals["xmax"],
                       bbox_vals["ymin"], bbox_vals["ymax"])
  r   <- rosm::osm.raster(x = bb, zoom = zoom, type = "cartolight",
                          crop = TRUE, projection = sp::CRS("EPSG:4326"))
  rr  <- terra::rast(r)
  terra::crs(rr) <- "EPSG:4326"
  rr_3857 <- terra::project(rr, "EPSG:3857", method = "near")
  df      <- as.data.frame(rr_3857, xy = TRUE, na.rm = FALSE)
  band_cols <- setdiff(names(df), c("x", "y"))
  names(df)[match(band_cols[1:3], names(df))] <- c("red", "green", "blue")
  df$fill <- grDevices::rgb(df$red, df$green, df$blue, maxColorValue = 255)
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

# -----------------------------
# Clean genetic match dataset — keep NT diff column
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
    # coerce NT diff to integer
    nt_diff = suppressWarnings(
      as.integer(nt_diff_from_closest_match_at_time_of_reporting)
    )
  )

# -----------------------------
# Clean virus dataset
# -----------------------------
virus_data2 <- virus_data %>%
  mutate(
    epid_clean       = clean_epid(EPID),
    epid_closest_clean = clean_epid(EPIDClosestMatch)
  )

# Manual harmonization
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
# Virus subset & points
# -----------------------------
virus_subset <- virus_data2 %>%
  filter(VdpvEmergenceGroupCode == "SOM-BAN-1" |
           epid_clean_for_match %in% linked_epids$epid_clean_for_match)

virus_points <- virus_subset %>%
  transmute(
    EPID, epid_clean, epid_clean_for_match,
    emergence_group   = VdpvEmergenceGroupCode,
    virus_date        = as.Date(`Virus Date`),
    surveillance_type = `Surveillance Type`,
    lon = X, lat = Y
  ) %>%
  filter(!is.na(EPID), !is.na(lon), !is.na(lat), !is.na(virus_date)) %>%
  distinct(epid_clean_for_match, .keep_all = TRUE) %>%
  mutate(
    period_group = if_else(virus_date < as.Date("2023-01-01"), "Before 2023", "2023-Present"),
    surveillance_group = case_when(
      str_to_lower(surveillance_type) %in% c("afp", "afp case") ~ "AFP",
      str_to_lower(surveillance_type) %in% c("environmental", "env") ~ "Environmental",
      TRUE ~ as.character(surveillance_type)
    )
  )

set.seed(123)
virus_points_sf <- virus_points %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3857) %>%
  st_jitter(amount = 5000)

virus_points_plot <- virus_points_sf %>%
  mutate(x = st_coordinates(.)[, 1], y = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

# -----------------------------
# Join orphan flag to plot points
# -----------------------------
orphan_flags <- genetic_match_data2 %>%
  distinct(epid_clean, orphan_after_time_order) %>%
  mutate(is_orphan = orphan_after_time_order == TRUE)

virus_points_plot <- virus_points_plot %>%
  left_join(orphan_flags, by = c("epid_clean_for_match" = "epid_clean")) %>%
  mutate(is_orphan = replace_na(is_orphan, FALSE))

# -----------------------------
# Build arrow table — join NT diff
# -----------------------------
epid_links_subset <- epid_links %>%
  filter(
    epid_clean %in% virus_points_plot$epid_clean_for_match,
    closest_match_epid_clean %in% virus_points_plot$epid_clean_for_match
  ) %>%
  distinct(epid_clean, closest_match_epid_clean, nt_diff)

line_df_raw <- epid_links_subset %>%
  left_join(
    virus_points_plot %>%
      dplyr::select(epid_clean_for_match, epid1_display = EPID,
                    epid1_x = x, epid1_y = y, epid1_date = virus_date),
    by = c("epid_clean" = "epid_clean_for_match")
  ) %>%
  left_join(
    virus_points_plot %>%
      dplyr::select(epid_clean_for_match, epid2_display = EPID,
                    epid2_x = x, epid2_y = y, epid2_date = virus_date),
    by = c("closest_match_epid_clean" = "epid_clean_for_match")
  ) %>%
  filter(!is.na(epid1_x), !is.na(epid1_y), !is.na(epid1_date),
         !is.na(epid2_x), !is.na(epid2_y), !is.na(epid2_date))

# Force arrow direction: earlier → later date
line_df <- line_df_raw %>%
  mutate(
    from_epid = if_else(epid1_date <= epid2_date, epid1_display, epid2_display),
    to_epid   = if_else(epid1_date <= epid2_date, epid2_display, epid1_display),
    from_date = pmin(epid1_date, epid2_date),
    to_date   = pmax(epid1_date, epid2_date),
    from_x    = if_else(epid1_date <= epid2_date, epid1_x, epid2_x),
    from_y    = if_else(epid1_date <= epid2_date, epid1_y, epid2_y),
    to_x      = if_else(epid1_date <= epid2_date, epid2_x, epid1_x),
    to_y      = if_else(epid1_date <= epid2_date, epid2_y, epid1_y)
  ) %>%
  distinct(from_epid, to_epid, .keep_all = TRUE)

# Shorten arrows + curvature
line_plot <- line_df %>%
  mutate(
    dx = to_x - from_x, dy = to_y - from_y,
    seg_len = sqrt(dx^2 + dy^2)
  ) %>%
  filter(seg_len > 0) %>%
  mutate(
    arrow_gap   = pmin(14000, seg_len * 0.35),
    xend_short  = to_x - (dx / seg_len) * arrow_gap,
    yend_short  = to_y - (dy / seg_len) * arrow_gap,
    curvature   = if_else(row_number() %% 2 == 0, 0.18, -0.18),
    # bin NT diff for a clean legend
    nt_bin = case_when(
      is.na(nt_diff)  ~ "Unknown",
      nt_diff == 0    ~ "0",
      nt_diff <= 5    ~ "1–5",
      nt_diff <= 14    ~ "6–14",
      nt_diff > 14            ~ "15+",
      TRUE ~ "Unknown"
    ),
    nt_bin = factor(nt_bin, levels = c("0", "1–5", "6–14", "15+"))
  )

# -----------------------------
# Color palette for NT diff bins
# Diverging: green (close) → yellow → orange → red (distant)
# -----------------------------
nt_palette <- c(
  "0"       = "#2c7bb6",
  "1–5"     = "#1a9641",
  "6–14"     = "orange",
  "15+"     = "firebrick"
)

# -----------------------------
# Plot
# -----------------------------
p_genetic <- ggplot() +
  # Basemap
  geom_raster(data = carto_df_3857, aes(x = x, y = y, fill = fill)) +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  
  # Admin boundaries
  geom_sf(
    data = adm1_map_proj %>%
      filter(enddate >= "2026-12-31", adm0_name != "UGANDA"),
    color = "grey80", fill = NA, linewidth = 0.25
  ) +
  geom_sf(
    data = adm0_map_proj %>%
      filter(enddate >= "2026-12-31", adm0_name != "UGANDA"),
    fill = NA, color = "grey50", linewidth = 1
  ) +
  
  # ── Arrows colored by NT diff bin ──────────────────────────────────────────
  geom_curve(
    data = line_plot,
    aes(
      x = from_x, y = from_y,
      xend = xend_short, yend = yend_short,
      color = nt_bin
    ),
    curvature = 0.18,
    linewidth  = 1,
    alpha      = 0.80,
    arrow      = arrow(length = unit(0.08, "inches"), type = "open"),
    lineend    = "round"
  ) +
  scale_color_manual(
    values = nt_palette,
    name   = "Nucleotide differences\nfrom closest match",
    guide  = guide_legend(
      override.aes = list(linewidth = 1.5),
      keywidth = unit(1.2, "cm")
    ),
    drop = FALSE
  ) +
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  
  # ── Points: Before 2023 ────────────────────────────────────────────────────
  geom_point(
    data = virus_points_plot %>% filter(period_group == "Before 2023"),
    aes(x = x, y = y, shape = surveillance_group, fill = period_group),
    color = "black", size = 3, stroke = 0.4
  ) +
  # ── Points: 2023-Present ───────────────────────────────────────────────────
  geom_point(
    data = virus_points_plot %>% filter(period_group == "2023-Present"),
    aes(x = x, y = y, shape = surveillance_group, fill = period_group),
    color = "black", size = 3, stroke = 0.4, alpha = 0.8
  ) +
  # ── Orphan highlight ring ──────────────────────────────────────────────────
  # ── Orphan highlight ring ──────────────────────────────────────────────────
  geom_point(
    data = virus_points_plot %>% filter(is_orphan),
    aes(x = x, y = y, shape = "Orphan"),
    fill   = NA,
    color  = "red",
    size   = 5.5,
    stroke = 1.2
  ) +
  scale_shape_manual(
    values = c(
      "AFP"           = 21,
      "Environmental" = 24,
      "Community"     = 22,
      "Contact"       = 23,
      "Orphan"        = 21
    ),
    guide = guide_legend(
      override.aes = list(
        fill   = c(NA, NA, NA, NA, NA),
        color  = c("black", "black", "black", "black", "red"),
        size   = c(3, 3, 3, 3, 5.5),
        stroke = c(0.4, 0.4, 0.4, 0.4, 1.2)
      )
    )
  ) +
  scale_fill_manual(
    values = c("Before 2023" = "#A68A64", "2023-Present" = "pink"),
    name   = "Virus detection period",
    guide  = guide_legend(
      override.aes = list(
        alpha  = c(1, 0.8), color = "black",
        shape  = 21, size = 3, stroke = 0.4
      )
    )
  ) +
  
  coord_sf(crs = st_crs(3857), expand = FALSE) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    legend.position  = c(0.98, 0.02),
    legend.justification = c(1, 0),
    legend.background = element_rect(
      fill = "#D4DADC", color = "#D4DADC", linewidth = 0
    ),
    plot.subtitle    = element_text(size = 9),
    # stack legends top-to-bottom cleanly
    legend.box       = "vertical",
    legend.spacing.y = unit(0.15, "cm")
  ) +
  labs(
    title    = "Genetic linkages among cVDPV2 detections in the SOM-BAN-1 emergence group",
    subtitle = paste0(
      "Arrows connect each detection to its genetically closest known relative; ",
      "arrow direction runs from earlier to later virus detection dates.\n",
      "Arrow color indicates nucleotide (NT) differences from the closest match. ",
      "Genetic matching available only for detections from 2023 onward;\n",
      "earlier detections are shown for epidemiologic context."
    ),
    shape   = "Surveillance type",
    caption = "Source: POLIS virus dataset and CDC genetic sequencing linkage data."
  )

p_genetic

ggsave(
  filename = "genetic_linkage_map_nt_diff.png",
  plot     = p_genetic,
  width    = 11,
  height   = 11,
  units    = "in",
  dpi      = 600,
  bg       = "white"
)

