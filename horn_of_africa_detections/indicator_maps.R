library(rio)
library(dplyr)
library(lubridate)
library(tidyverse)
library(sf)
library(janitor)
library(stringr)
library(ggplot2)
library(scatterpie)
library(ggnewscale)

#----------------------------
# Import AFP datasets
#----------------------------
three_plus_dose_afp <- rio::import("data/Indicators_Detailed_Dataset_may_contain_sensitive_data_26-03-2026_02-17-45.xlsx")
one_two_dose_afp    <- rio::import("data/Indicators_Detailed_Dataset_may_contain_sensitive_data_26-03-2026_02-16-49.xlsx")
zero_dose_afp       <- rio::import("data/Indicators_Detailed_Dataset_may_contain_sensitive_data_26-03-2026_02-15-49.xlsx")

human <- rio::import("data/Human_Detailed_Dataset_may_contain_sensitive_data_26-03-2026_01-26-33.xlsx")

iom_data <- rio::import("data/IOM_DTM_ETT_SOM_Tracker_sinceFeb2025_w55.xlsx")

head(iom_data)
colnames(iom_data)

#----------------------------
# Select period that best covers 2025
#----------------------------
select_period_covering_2025 <- function(df) {
  target_start <- as.Date("2025-01-01")
  target_end   <- as.Date("2025-12-31")
  
  df %>%
    dplyr::mutate(
      period_start  = as.Date(`Period date start`),
      period_end    = as.Date(`Period date end`),
      overlap_start = pmax(period_start, target_start),
      overlap_end   = pmin(period_end, target_end),
      overlap_days  = as.numeric(overlap_end - overlap_start + 1),
      overlap_days  = ifelse(overlap_days < 0, 0, overlap_days)
    ) %>%
    dplyr::group_by(Country, Province) %>%
    dplyr::slice_max(order_by = overlap_days, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
}

area_id_cols <- c(
  "Country",
  "Province",
  "District",
  "Admin 0 Guid",
  "Admin 1 Guid",
  "Admin 2 Guid",
  "Admin 0 ShapeId",
  "Admin 1 ShapeId",
  "Admin 2 ShapeId"
)

zero_2025 <- select_period_covering_2025(zero_dose_afp) %>%
  dplyr::select(dplyr::all_of(area_id_cols), Numerator) %>%
  dplyr::rename(doses_0 = Numerator)

one_two_2025 <- select_period_covering_2025(one_two_dose_afp) %>%
  dplyr::select(dplyr::all_of(area_id_cols), Numerator) %>%
  dplyr::rename(doses_1_2 = Numerator)

three_plus_2025 <- select_period_covering_2025(three_plus_dose_afp) %>%
  dplyr::select(dplyr::all_of(area_id_cols), Numerator) %>%
  dplyr::rename(doses_3_plus = Numerator)

afp_dose_summary_2025 <- zero_2025 %>%
  dplyr::full_join(one_two_2025, by = area_id_cols) %>%
  dplyr::full_join(three_plus_2025, by = area_id_cols) %>%
  dplyr::mutate(
    dplyr::across(c(doses_0, doses_1_2, doses_3_plus), ~ tidyr::replace_na(as.numeric(.x), 0)),
    total_afp = rowSums(dplyr::across(c(doses_0, doses_1_2, doses_3_plus)), na.rm = TRUE),
    admin1_shapeid_clean = tolower(`Admin 1 ShapeId`),
    admin0_shapeid_clean = tolower(`Admin 0 ShapeId`)
  )

#----------------------------
# Read WHO geodatabase
#----------------------------
gdb_path <- "C:/Users/wxf7/Downloads/WHO_POLIO_GLOBAL_GEODATABASE.gdb/WHO_POLIO_GLOBAL_GEODATABASE.gdb"

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

#----------------------------
# Clean IDs for reference
#----------------------------
who_gdb_adm1_province <- who_gdb_adm1_province %>%
  dplyr::mutate(
    shapeid_clean = tolower(adm1_shape_id),
    guid_clean    = tolower(stringr::str_remove_all(guid, "[{}]"))
  )

who_gdb_adm0_country <- who_gdb_adm0_country %>%
  dplyr::mutate(
    shapeid_clean = tolower(adm0_shape_id),
    guid_clean    = tolower(stringr::str_remove_all(guid, "[{}]"))
  )

#----------------------------
# Join AFP data to polygons
# Keep your edited name-based joins and enddate deduping
#----------------------------
adm1_afp <- who_gdb_adm1_province %>%
  dplyr::inner_join(
    afp_dose_summary_2025,
    by = c(
      "adm0_name" = "Country",
      "adm1_name" = "Province"
    )
  ) %>%
  dplyr::group_by(adm0_name, adm1_name) %>%
  dplyr::arrange(desc(enddate)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

adm0_afp <- who_gdb_adm0_country %>%
  dplyr::semi_join(
    afp_dose_summary_2025,
    by = c("adm0_name" = "Country")
  ) %>%
  dplyr::group_by(adm0_name) %>%
  dplyr::arrange(desc(enddate)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

#----------------------------
# Project to planar CRS for scatterpie plotting
#----------------------------
plot_crs <- 3857

adm1_afp_proj <- sf::st_transform(adm1_afp, plot_crs)
adm0_afp_proj <- sf::st_transform(adm0_afp, plot_crs)

#----------------------------
# Compute province centroids for pies
#----------------------------
pie_points <- adm1_afp_proj %>%
  sf::st_centroid() %>%
  dplyr::mutate(
    x = sf::st_coordinates(.)[, 1],
    y = sf::st_coordinates(.)[, 2]
  ) %>%
  sf::st_drop_geometry()

#----------------------------
# Categorical AFP size groups
#----------------------------
pie_points <- pie_points %>%
  dplyr::mutate(
    afp_size_cat = dplyr::case_when(
      total_afp <= 10  ~ "1-10",
      total_afp <= 50  ~ "11-50",
      total_afp <= 100 ~ "51-100",
      total_afp > 100  ~ "100+",
      TRUE ~ NA_character_
    ),
    afp_size_cat = factor(
      afp_size_cat,
      levels = c("1-10", "11-50", "51-100", "100+"),
      ordered = TRUE
    )
  )

#----------------------------
# Pie radius lookup in map units
# These are true scatterpie radii, because scatterpie uses map units
#----------------------------
bbox <- sf::st_bbox(adm0_afp_proj)
map_span <- max(
  as.numeric(bbox["xmax"] - bbox["xmin"]),
  as.numeric(bbox["ymax"] - bbox["ymin"])
)

radius_lookup <- c(
  "1-10"   = map_span * 0.008,
  "11-50"  = map_span * 0.018,
  "51-100" = map_span * 0.028,
  "100+"   = map_span * 0.038
)

pie_points <- pie_points %>%
  dplyr::mutate(
    afp_size_cat = factor(
      afp_size_cat,
      levels = c("1-10", "11-50", "51-100", "100+"),
      ordered = TRUE
    ),
    pie_radius = unname(radius_lookup[as.character(afp_size_cat)]),
    
    # dummy mapped size just to create the legend
    legend_size_cat = afp_size_cat
  )

#----------------------------
# Invisible point size lookup for legend
# This is proportional to the same category scaling used for pie radii
#----------------------------
point_size_lookup <- setNames(
  unname(radius_lookup / max(radius_lookup) * 8),
  names(radius_lookup)
)


library(terra)

get_cartolight_df_3857 <- function(bbox_vals, zoom = 5) {
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
  
  # convert raster -> terra
  rr <- terra::rast(r)
  
  # assign CRS if needed
  terra::crs(rr) <- "EPSG:4326"
  
  # reproject the raster itself
  rr_3857 <- terra::project(rr, "EPSG:3857", method = "near")
  
  df <- as.data.frame(rr_3857, xy = TRUE, na.rm = FALSE)
  
  band_cols <- setdiff(names(df), c("x", "y"))
  names(df)[1:5]
  
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
bbox_geo <- sf::st_bbox(adm0_afp)
carto_df_3857 <- get_cartolight_df_3857(bbox_geo, zoom = 6)

#----------------------------
# Build map
#----------------------------
p <- ggplot() +
  # Carto basemap in lon/lat
  geom_raster(
    data = carto_df_3857,
    aes(x = x, y = y, fill = fill)
  ) +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  geom_sf(
    data = adm1_afp_proj,
    color = "grey50",
    fill = NA,
    linewidth = 0.25
  ) +
  geom_sf(
    data = adm0_afp_proj,
    fill = NA,
    color = "black",
    linewidth = 1
  ) +
  
  # Invisible points to generate the size legend
  geom_point(
    data = pie_points,
    aes(x = x, y = y, size = legend_size_cat),
    alpha = 0,
    color = NA,
    show.legend = TRUE
  ) +
  
  scatterpie::geom_scatterpie(
    data = pie_points,
    aes(x = x, y = y, r = pie_radius),
    cols = c("doses_0", "doses_1_2", "doses_3_plus"),
    color = "black",
    linewidth = 0.2,
    alpha = 0.95
  ) +
  
  scale_fill_manual(
    values = c(
      doses_0 = "#d7191c",
      doses_1_2 = "#ffffbf",
      doses_3_plus = "#1a9641"
    ),
    labels = c(
      doses_0 = "0 doses",
      doses_1_2 = "1-2 doses",
      doses_3_plus = "3+ doses"
    ),
    name = "Distribution of number\nof polio vaccine doses\namong AFP detections"
  ) +
  
  scale_size_manual(
    values = c(
      "1-10" = 1,
      "11-50" = 1,
      "51-100" = 1,
      "100+" = 1
    ),
    breaks = c("1-10", "11-50", "51-100", "100+"),
    drop = FALSE,
    name = "Number of AFP detections",
    guide = guide_legend(
      override.aes = list(
        size = c(3, 6, 9, 12),
        shape = 21,
        fill = NA,
        color = "black",
        alpha = 1,
        stroke = 0.4
      )
    )
  ) +
  
  coord_sf() +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "Distribution of polio vaccine dose history among AFP cases aged 6–59 months,",
    subtitle = "by Province in Djibouti, Eritrea, Ethiopia, Kenya, and Somalia, 2025",
    caption = "Data Source: POLIS indicator table, downloaded 2026-03-25")

p <- p +
  theme(
    legend.key = element_blank(),
    legend.key.size = unit(10, "pt"),
    legend.position = c(0.95, 0.05),   # bottom right inside panel
    legend.justification = c(1, 0),     # anchor legend corner
    legend.direction = "vertical",
    legend.box = "vertical",
    
    legend.background = element_rect(
      fill = "#D4DADC",
      color = "#D4DADC",
      linewidth = 0
    ),
    
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) 

p

ggsave(
  filename = "afp_dose_map.png",
  plot = p,
  width = 8.5,
  height = 11,
  units = "in",
  dpi = 300,
  bg = "white"
)


table(human$`Place Admin 0`)
table(human$`Virus Type(s)`)
table(human$`Surveillance Type`)
table(human$`Doses OPV Number`, exclude=NULL)


human_map <- human %>%
  filter(
    grepl("cVDPV", `Virus Type(s)`),
    !is.na(X),
    !is.na(Y)
  ) %>%
  mutate(
    opv_dose_cat = case_when(
      `Doses OPV Number` == 0 ~ "0 OPV doses",
      `Doses OPV Number` %in% c(1, 2) ~ "1–2 OPV doses",
      `Doses OPV Number` >= 3 & `Doses OPV Number` != 99 ~ "3+ OPV doses",
      TRUE ~ "Missing Data"
    ),
    opv_dose_cat = factor(
      opv_dose_cat,
      levels = c(
        "0 OPV doses",
        "1–2 OPV doses",
        "3+ OPV doses",
        "Missing Data"
      )
    )
  ) %>%
  filter(`Stool 1 Collection Date` >= as.Date("2020-01-01") &
           `Stool 1 Collection Date` <= as.Date("2025-12-31"))

human_sf <- st_as_sf(
  human_map,
  coords = c("X", "Y"),
  crs = 4326
)

human_proj <- st_transform(
  human_sf,
  3857
)


p_cases <- ggplot() +
  
  geom_raster(
    data = carto_df_3857,
    aes(x = x, y = y, fill = fill)
  ) +
  
  scale_fill_identity() +
  
  ggnewscale::new_scale_fill() +
  
  geom_sf(
    data = adm1_afp_proj,
    color = "grey50",
    fill = NA,
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = adm0_afp_proj,
    fill = NA,
    color = "black",
    linewidth = 0.7
  ) +
  
  geom_sf(
    data = human_proj,
    aes(fill = opv_dose_cat),
    shape = 21,
    color = "black",
    size = 3.5,
    stroke = 0.4,
    alpha = 0.8
  ) +
  
  scale_fill_manual(
    values = c(
      "0 OPV doses" = "#d7191c",
      "1–2 OPV doses" = "#ffffbf",
      "3+ OPV doses" = "#1a9641",
      "Missing Data" = "grey70"
    ),
    name = "OPV dose history"
  ) +
  
  coord_sf(
    crs = st_crs(3857),
    expand = FALSE
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    legend.position = c(0.98, 0.02),
    legend.justification = c(1, 0),
    
    legend.key = element_blank(),
    
    legend.background = element_rect(
      fill = "#D4DADC",
      color = "#D4DADC",
      linewidth = 0
    )
  ) +
  
  labs(
    title = "cVDPV detections by OPV dose history, 2020-2025",
    subtitle = "Human detections from AFP, Contact, and Community Surveillance;\nDjibouti, Eritrea, Ethiopia, Kenya, and Somalia",
    caption = "Data Source: POLIS indicator table, downloaded 2026-03-25"
  )

p_cases

ggsave(
  filename = "cVDPV_dose_map.png",
  plot = p_cases,
  width = 8.5,
  height = 11,
  units = "in",
  dpi = 300,
  bg = "white"
)



library(dplyr)

flows_district <- iom_data %>%
  mutate(
    inflow = `Total new arrivals since last week`,
    outflow = `Total number of departures since last week`,
    
    origin_country =
      `Origin of the new arrivals (Somalia, Ethiopia or other country)`,
    
    origin_district = case_when(
      origin_country == "Somalia" ~
        `Somalia District of Origin`,
      
      origin_country == "Ethiopia" ~
        "ETHIOPIA",
      
      origin_country == "Other country" ~
        "OTHER COUNTRY"
    ),
    
    destination_district =
      toupper(`District Name`)
  ) %>%
  group_by(
    origin_district,
    destination_district
  ) %>%
  summarise(
    inflow = sum(inflow, na.rm = TRUE),
    outflow = sum(outflow, na.rm = TRUE),
    net_flow = inflow - outflow,
    .groups = "drop"
  ) %>%
  mutate(origin_district = toupper(origin_district))

gdb_path <- "C:/Users/wxf7/Downloads/WHO_POLIO_GLOBAL_GEODATABASE.gdb/WHO_POLIO_GLOBAL_GEODATABASE.gdb"

who_gdb_adm2_district <- sf::st_read(
  gdb_path,
  "GLOBAL_ADM2",
  quiet = TRUE
) %>%
  janitor::clean_names() %>%
  filter(adm0_name == "SOMALIA")

standardize_district <- function(x) {
  x %>%
    toupper() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s+", " ") %>%
    dplyr::recode(
      "ABDUL AZIZ" = "ABDI AZIS",
      "ABUDWAQ" = "CABUDWAAQ",
      "ADADO" = "CADAADO",
      "ADALE" = "CADALE",
      "ADEN YABAL" = "ADAN YABAAL",
      "AFGOI" = "AFGOOYE",
      "AINABO" = "CAYNABO",
      "ALULA" = "CALUULA",
      "BADADE" = "BADHAADHE",
      "BALAD" = "BALCAD",
      "BAARDHEERE" = "BARDERA",
      "BELET HAWA" = "BELET XAAWO",
      "BENDER BAYLA" = "BANDARBEYLA",
      "BONDERE" = "BONDHEERE",
      "BRAVA" = "BARAAWE",
      "BUAALE" = "BU'AALE",
      "BULO BURTI" = "BULO BURTO",
      "BURHAKABA" = "BUUR HAKABA",
      "CEERIGAABO" = "ERIGAVO",
      "DANYILE" = "DAYNIILE",
      "DINSOR" = "DIINSOOR",
      "DOLO" = "DOOLOW",
      "DUSAMREB" = "DHUUSAMARREEB",
      "EL BARDE" = "CEEL BARDE",
      "EL BUR" = "CEEL BUUR",
      "EL DHERE" = "CEEL DHEER",
      "EL WAQ" = "CEEL WAAQ",
      "GARBAHARE" = "GARBAHAAREY",
      "GARDO" = "QARDHO",
      "GAROOWE" = "GAROWE",
      "GOLDOGOB" = "GALDOGOB",
      "GALKAYU" = "GAALKACYO",
      "GALKAYU NORTH" = "GAALKACYO",
      "GALKAYU SOUTH" = "GAALKACYO",
      "HAWALWADAG" = "HAWL WADAAG",
      "HELIWA" = "HURIWAA",
      "HARA DHERE" = "XARARDHEERE",
      "HUDUN" = "XUDUN",
      "HUDUR" = "XUDUR",
      "JAMAAME EAST" = "JAMAAME",
      "JAMAAME WEST" = "JAMAAME",
      "JARIBAN" = "JARIIBAN",
      "JILIB EAST" = "JILIB",
      "JILIB WEST" = "JILIB",
      "KARAN" = "KARAAN",
      "KISMAYO" = "KISMAAYO",
      "LAS ANOD" = "LAAS CAANOOD",
      "MEDINA" = "WADAJIR (MEDINA)",
      "QANSAH DERE" = "QANSAX DHEERE",
      "QORYOOLEY" = "QORYOLEY",
      "RABDURE" = "RAB DHUURE",
      "SAKOW" = "SAAKOW",
      "SABLAALE" = "SABLALE",
      "SHANGAANI" = "SHANGANI",
      "TALEH" = "TALEEX",
      "TIEGLO" = "TAYEEGLOW",
      "WABERI" = "WAABERI",
      "WAJID" = "WAAJID",
      "WANLEWEYNE" = "WANLA WEYN",
      "WARDEGLY" = "WARDHIGLEY",
      "YAQSHID" = "YAAQSHIID",
      "ZEILA" = "ZEYLAC",
      "LAASQORAY" = "BADAN"
    )
}

who_adm2_std <- who_gdb_adm2_district %>%
  mutate(
    district_std = standardize_district(adm2_name)
  ) %>%
  group_by(district_std) %>%
  summarise(
    center_lon = mean(center_lon, na.rm = TRUE),
    center_lat = mean(center_lat, na.rm = TRUE),
    .groups = "drop"
  )

flows_district <- flows_district %>%
  mutate(
    origin_district = standardize_district(origin_district),
    destination_district = standardize_district(destination_district)
  )


setdiff(
  sort(unique(c(flows_district$origin_district, flows_district$destination_district))),
  sort(unique(who_adm2_std$district_std))
)

setdiff(
  sort(unique(who_adm2_std$district_std)),
  sort(unique(c(flows_district$origin_district, flows_district$destination_district)))
)


library(dplyr)
library(sf)
library(ggplot2)
library(grid)

# district lookup from standardized WHO districts
district_lookup <- who_adm2_std %>%
  st_drop_geometry() %>%
  transmute(
    district_std,
    lon = center_lon,
    lat = center_lat
  )

# add external origins
district_lookup <- bind_rows(
  district_lookup,
  tibble::tibble(
    district_std = c("ETHIOPIA", "OTHER COUNTRY"),
    lon = c(43.8, 38.0),
    lat = c(7.5, 6.0)
  )
)

library(dplyr)
library(sf)
library(ggplot2)
library(grid)
library(terra)
library(raster)
library(rosm)
library(sp)

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
  names(df)[match(band_cols[1:3], names(df))] <- c("red", "green", "blue")
  
  df$fill <- grDevices::rgb(
    df$red, df$green, df$blue,
    maxColorValue = 255
  )
  
  df
}

# -----------------------------
# Join coordinates onto flows
# -----------------------------
flows_geo <- flows_district %>%
  left_join(
    district_lookup,
    by = c("origin_district" = "district_std")
  ) %>%
  rename(
    origin_lon = lon,
    origin_lat = lat
  ) %>%
  left_join(
    district_lookup,
    by = c("destination_district" = "district_std")
  ) %>%
  rename(
    dest_lon = lon,
    dest_lat = lat
  ) %>%
  filter(
    !is.na(origin_lon), !is.na(origin_lat),
    !is.na(dest_lon), !is.na(dest_lat)
  ) %>%
  mutate(
    inflow = ifelse(inflow < 100, NA, inflow),
    outflow = ifelse(outflow < 100, NA, outflow)
  )

# -----------------------------
# Node points: only districts with IOM sites
# -----------------------------
iom_site_districts <- iom_data %>%
  transmute(
    district_std = standardize_district(`District Name`)
  ) %>%
  distinct()

node_df <- district_lookup %>%
  filter(district_std %in% iom_site_districts$district_std)

# -----------------------------
# Project districts and nodes to 3857
# -----------------------------
who_adm2_std_3857 <- st_transform(who_adm2_std, 3857)

district_lookup_3857 <- district_lookup %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3857) %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

node_df_3857 <- node_df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3857) %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# -----------------------------
# Rejoin projected coordinates to flows
# -----------------------------
flows_geo_3857 <- flows_geo %>%
  dplyr::select(origin_district, destination_district, inflow, outflow) %>%
  left_join(
    district_lookup_3857 %>%
      dplyr::select(district_std, origin_x = x, origin_y = y),
    by = c("origin_district" = "district_std")
  ) %>%
  left_join(
    district_lookup_3857 %>%
      dplyr::select(district_std, dest_x = x, dest_y = y),
    by = c("destination_district" = "district_std")
  ) %>%
  filter(
    !is.na(origin_x), !is.na(origin_y),
    !is.na(dest_x), !is.na(dest_y)
  )

# -----------------------------
# Build facetted plotting data
# -----------------------------
flow_map_df <- bind_rows(
  flows_geo_3857 %>%
    transmute(
      facet = "New arrivals to tracked IOM sites",
      origin_district,
      destination_district,
      x = origin_x,
      y = origin_y,
      xend = dest_x,
      yend = dest_y,
      value = inflow
    ),
  flows_geo_3857 %>%
    transmute(
      facet = "Departures from tracked IOM sites",
      origin_district = destination_district,
      destination_district = origin_district,
      x = dest_x,
      y = dest_y,
      xend = origin_x,
      yend = origin_y,
      value = outflow
    )
) %>%
  filter(value > 0) %>%
  filter(!(x == xend & y == yend))

# -----------------------------
# Shorten lines/arrows before nodes
# Keep your exact gap settings
# -----------------------------
line_gap  <- 0.3
arrow_gap <- 0.35

flow_map_df <- flow_map_df %>%
  rowwise() %>%
  mutate(
    dx = xend - x,
    dy = yend - y,
    dist = sqrt(dx^2 + dy^2),
    
    x_line_start = x + ifelse(dist > 2 * line_gap, line_gap * dx / dist, 0),
    y_line_start = y + ifelse(dist > 2 * line_gap, line_gap * dy / dist, 0),
    x_line_end   = xend - ifelse(dist > 2 * line_gap, line_gap * dx / dist, 0),
    y_line_end   = yend - ifelse(dist > 2 * line_gap, line_gap * dy / dist, 0),
    
    x_arrow_start = x + ifelse(dist > 2 * arrow_gap, arrow_gap * dx / dist, 0),
    y_arrow_start = y + ifelse(dist > 2 * arrow_gap, arrow_gap * dy / dist, 0),
    x_arrow_end   = xend - ifelse(dist > 2 * arrow_gap, arrow_gap * dx / dist, 0),
    y_arrow_end   = yend - ifelse(dist > 2 * arrow_gap, arrow_gap * dy / dist, 0)
  ) %>%
  ungroup()

# -----------------------------
# Basemap
# -----------------------------
bbox_geo <- st_bbox(who_adm2_std)
carto_df_3857 <- get_cartolight_df_3857(bbox_geo, zoom = 7)

# -----------------------------
# Plot
# -----------------------------
iom_data <- iom_data %>%
  mutate(
    week_end_date = str_extract(
      `Data Collection Week`,
      "\\d{1,2} [A-Za-z]+, \\d{4}"
    ),
    week_end_date = dmy(week_end_date)
  )
max_date <- max(iom_data$week_end_date, na.rm = TRUE)

subtitle_text <- paste0(
  "District-level flows aggregated across all reporting weeks from February 2025 to ",
  format(max_date, "%d %B %Y"),
  ".\nArrows summarize total counts of new arrivals and departures reported at tracked IOM sites;\n",
  "Line width is shown on a log scale. Ethiopia arrivals and departures indicate country only."
)

iom_map <- ggplot() +
  geom_raster(
    data = carto_df_3857,
    aes(x = x, y = y, fill = fill)
  ) +
  scale_fill_identity() +
  geom_sf(
    data = who_adm2_std_3857,
    fill = NA,
    color = "grey70",
    linewidth = 0.2
  ) +
  geom_curve(
    data = flow_map_df,
    aes(
      x = x_line_start,
      y = y_line_start,
      xend = x_line_end,
      yend = y_line_end,
      linewidth = log10(value)
    ),
    curvature = 0.22,
    color = "#2C7FB8",
    alpha = 0.5,
    lineend = "round"
  ) +
  geom_curve(
    data = flow_map_df,
    aes(
      x = x_arrow_start,
      y = y_arrow_start,
      xend = x_arrow_end,
      yend = y_arrow_end
    ),
    curvature = 0.22,
    color = "#2C7FB8",
    linewidth = 0.35,
    alpha = 1,
    arrow = arrow(
      length = unit(0.28, "cm"),
      type = "closed",
      ends = "last"
    ),
    lineend = "round"
  ) +
  geom_point(
    data = node_df_3857,
    aes(
      x = x,
      y = y,
      shape = "District with IOM site"
    ),
    size = 1.8,
    stroke = 0.35,
    fill = "white",
    color = "black"
  ) +
  scale_linewidth_continuous(
    name = "People moved",
    range = c(0.4, 5),
    breaks = log10(c(100, 300, 1000, 3000, 10000)),
    labels = c("100", "300", "1,000", "3,000", "10,000")
  ) +
  scale_shape_manual(
    name = NULL,
    values = c("District with IOM site" = 21)
  ) +
  coord_sf(crs = st_crs(3857), expand = FALSE) +
  facet_wrap(~facet) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  labs(
    title = "Population movements reported by IOM Displacement Tracking Matrix - Emergency Trends Tracking sites in Somalia",
    subtitle = subtitle_text,
    caption = paste(
      "Data source: IOM Displacement Tracking Matrix (DTM) Emergency Trends Tracking (ETT), Somalia.",
      "Weekly settlement/site reports aggregated from February 2025 through",
      format(max_date, "%d %B %Y"),
      "."
    )
  ) +
  guides(
    linewidth = guide_legend(order = 1),
    shape = guide_legend(
      order = 2,
      override.aes = list(
        size = 3,
        fill = "white",
        color = "black",
        stroke = 0.35
      )
    )
  )

ggsave(
  filename = "iom_map.png",
  plot = iom_map,
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

