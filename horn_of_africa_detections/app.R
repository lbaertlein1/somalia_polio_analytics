# app.R
# Interactive cVDPV emergence-group mapping app using ggplot + Shiny
# Single display CRS: EPSG:4326
# Basemap: Carto Light only
library(tidyverse)
library(dplyr)
library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)
library(janitor)
library(stringr)
library(purrr)
library(rio)
library(igraph)
library(scales)
library(grid)
library(officer)
library(png)
library(patchwork)
library(rosm)
library(raster)
library(ggrepel)
library(geosphere)
library(ggnewscale)
library(colorspace)

# ============================================================
# 1) INPUTS
# ============================================================

emergence_path <- "data/Viruses_Detailed_Dataset_may_contain_sensitive_data_25-03-2026_18-02-53.xlsx"
data_path      <- "data/Viruses_Detailed_Dataset_may_contain_sensitive_data_25-03-2026_13-29-40.xlsx"

plot_crs  <- 4326
tile_zoom <- 5

min_extent_countries <- c("Somalia", "Djibouti", "Ethiopia", "Kenya")

shape_values <- c(
  "AFP" = 16,
  "Environmental" = 17,
  "Community" = 15,
  "Contact" = 18,
  "Healthy" = 19
)

# ============================================================
# 2) LOAD DATA ONCE
# ============================================================

adm0 <- readRDS("data/adm0.Rds")

raw_data <- suppressWarnings(rio::import(data_path))
emergence_file <- suppressWarnings(rio::import(emergence_path)) %>%
  janitor::clean_names()

if ("emergence_group_s" %in% names(emergence_file)) {
  emergence_keep <- emergence_file %>%
    filter(!is.na(emergence_group_s)) %>%
    pull(emergence_group_s) %>%
    unique()
} else if ("emergence_group_s_" %in% names(emergence_file)) {
  emergence_keep <- emergence_file %>%
    filter(!is.na(emergence_group_s_)) %>%
    pull(emergence_group_s_) %>%
    unique()
} else {
  stop("Could not identify emergence group field in emergence file.")
}

data_clean <- raw_data %>%
  clean_names() %>%
  mutate(
    virus_date = as.Date(virus_date),
    emergence_group_s = str_trim(emergence_group_s),
    surveillance_type = as.character(surveillance_type),
    virus_type_s = as.character(virus_type_s),
    vdpv_classification_s = as.character(vdpv_classification_s),
    admin0official_name = as.character(admin0official_name)
  ) %>%
  filter(
    emergence_group_s %in% emergence_keep,
    !is.na(virus_date),
    !is.na(x),
    !is.na(y),
    !is.na(admin0official_name)
  ) %>%
  ungroup() %>%
  arrange(desc(virus_date))

all_surveillance_types <- sort(unique(data_clean$surveillance_type))
shape_breaks <- intersect(names(shape_values), all_surveillance_types)

global_min_date <- min(data_clean$virus_date, na.rm = TRUE)
global_max_date <- max(data_clean$virus_date, na.rm = TRUE)

# ============================================================
# 3) HELPERS
# ============================================================

make_about_text <- function() {
  paste(
    "This map uses cVDPV detections from the POLIS database, downloaded 2025-03-25.",
    "Data are restricted to cVDPV detections and to emergence groups with at least one detection in the Horn of Africa.",
    
    "Within each emergence group, detections are assigned to the same cluster when they fall within the selected maximum geographic distance and maximum time window.",
    
    "Arrows show inferred links between clusters within the same emergence group.",
    "For each later cluster, the most plausible earlier source cluster is identified using only timing and geographic proximity.",
    "A prior cluster is treated as an ongoing possible source if the later cluster begins within the selected time window after the prior cluster's last detection.",
    "If multiple prior clusters are eligible, the nearest one is selected.",
    
    "These arrows are heuristic and are based only on virus dates and geographic distance.",
    "They do not represent confirmed transmission pathways.",
    
    sep = " "
  )
}

make_emergence_meta <- function(df) {
  if (nrow(df) == 0) {
    return(
      tibble(
        emergence_group_s = character(0),
        min_year = integer(0),
        max_year = integer(0),
        latest_date = as.Date(character(0)),
        n = integer(0),
        detected_in_somalia = logical(0),
        display_label = character(0)
      )
    )
  }
  
  meta_years <- df %>%
    mutate(year = lubridate::year(virus_date)) %>%
    group_by(emergence_group_s) %>%
    summarise(
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      latest_date = max(virus_date, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  meta_somalia <- df %>%
    group_by(emergence_group_s) %>%
    summarise(
      detected_in_somalia = any(admin0official_name == "Somalia", na.rm = TRUE),
      .groups = "drop"
    )
  
  meta_years %>%
    left_join(meta_somalia, by = "emergence_group_s") %>%
    arrange(desc(latest_date), emergence_group_s) %>%
    mutate(
      display_label = if_else(
        min_year == max_year,
        paste0(emergence_group_s, ": ", min_year, "; N=", n),
        paste0(emergence_group_s, ": ", min_year, "-", max_year, "; N=", n)
      )
    )
}

make_emergence_title <- function(selected_groups, meta_df) {
  meta_sub <- meta_df %>%
    filter(emergence_group_s %in% selected_groups) %>%
    arrange(desc(latest_date), emergence_group_s)
  
  parts <- ifelse(
    meta_sub$min_year == meta_sub$max_year,
    paste0(meta_sub$emergence_group_s, ": ", meta_sub$min_year, "; N=", meta_sub$n),
    paste0(meta_sub$emergence_group_s, ": ", meta_sub$min_year, "-", meta_sub$max_year, "; N=", meta_sub$n)
  )
  
  paste(parts, collapse = " | ")
}

make_group_palette <- function(groups) {
  groups <- sort(unique(groups))
  pal <- hue_pal()(length(groups))
  names(pal) <- groups
  pal
}

make_darker_palette <- function(pal, amount = 0.35) {
  colorspace::darken(pal, amount)
}

make_cluster_label_text <- function(first_date, last_date, n_detections, mode) {
  if (identical(mode, "cluster_summary")) {
    if (n_detections == 1) {
      format(first_date, "%Y-%m-%d")
    } else {
      paste0(
        "First: ", format(first_date, "%Y-%m-%d"), "\n",
        "Last: ", format(last_date, "%Y-%m-%d"), "\n",
        "N: ", n_detections
      )
    }
  } else if (identical(mode, "cluster_years")) {
    y1 <- format(first_date, "%Y")
    y2 <- format(last_date, "%Y")
    if (y1 == y2) y1 else paste0(y1, "-", y2)
  } else {
    NA_character_
  }
}

get_distance_matrix_km <- function(coords) {
  coords <- as.matrix(coords)
  
  if (!is.numeric(coords)) {
    stop("Coordinates must be numeric.")
  }
  
  if (ncol(coords) != 2) {
    stop("Points matrix must have exactly 2 columns (x/y or lon/lat).")
  }
  
  if (nrow(coords) == 0) {
    stop("No points available to compute distance matrix.")
  }
  
  geosphere::distm(
    coords,
    fun = geosphere::distHaversine
  ) / 1000
}

cluster_one_group <- function(df_group, spatial_eps_km, time_gap_days) {
  n <- nrow(df_group)
  
  if (n == 0) return(df_group[0, ])
  if (n == 1) return(df_group %>% mutate(cluster_num = 1L))
  
  coords <- df_group |>
    sf::st_drop_geometry() |>
    dplyr::select(x, y) |>
    as.matrix()
  
  dist_mat_km <- get_distance_matrix_km(coords)
  
  time_mat_days <- abs(outer(
    df_group$virus_date,
    df_group$virus_date,
    function(a, b) as.numeric(a - b)
  ))
  
  adj <- (dist_mat_km <= spatial_eps_km) & (time_mat_days <= time_gap_days)
  diag(adj) <- FALSE
  
  g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected", diag = FALSE)
  comps <- igraph::components(g)$membership
  
  df_group %>%
    mutate(cluster_num = comps)
}

build_clustered_data <- function(df,
                                 spatial_eps_km,
                                 time_gap_days,
                                 source_ongoing_days) {
  if (nrow(df) == 0) {
    return(list(
      det_clustered = st_sf(),
      cluster_pts = st_sf(),
      cluster_poly = st_sf(),
      cluster_labels = tibble(),
      edges_df = tibble()
    ))
  }
  
  det_sf <- df %>%
    st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE)
  
  det_clustered <- det_sf %>%
    group_split(emergence_group_s) %>%
    map_dfr(
      cluster_one_group,
      spatial_eps_km = spatial_eps_km,
      time_gap_days = time_gap_days
    ) %>%
    st_as_sf() %>%
    group_by(emergence_group_s) %>%
    mutate(cluster_id = paste0(emergence_group_s, "__C", dense_rank(cluster_num))) %>%
    ungroup()
  
  cluster_summary <- det_clustered %>%
    st_drop_geometry() %>%
    group_by(emergence_group_s, cluster_id) %>%
    summarise(
      first_date   = min(virus_date, na.rm = TRUE),
      last_date    = max(virus_date, na.rm = TRUE),
      n_detections = n(),
      mean_x       = mean(x, na.rm = TRUE),
      mean_y       = mean(y, na.rm = TRUE),
      countries    = paste(sort(unique(admin0official_name)), collapse = ", "),
      .groups = "drop"
    )
  
  cluster_pts <- st_as_sf(
    cluster_summary,
    coords = c("mean_x", "mean_y"),
    crs = 4326
  ) %>%
    mutate(
      cx = st_coordinates(.)[, 1],
      cy = st_coordinates(.)[, 2]
    )
  
  assign_sources_one_group <- function(df_group, source_ongoing_days = 120) {
    n <- nrow(df_group)
    if (n <= 1) return(tibble())
    
    df_group <- df_group %>%
      arrange(first_date, cluster_id)
    
    min_first_date <- min(df_group$first_date, na.rm = TRUE)
    coords <- df_group %>%
      st_drop_geometry() %>%
      dplyr::select(cx, cy) %>%
      as.matrix()
    
    dist_mat_km <- get_distance_matrix_km(coords)
    
    edge_list <- vector("list", n)
    
    for (i in seq_len(n)) {
      target <- df_group[i, ]
      
      if (target$first_date == min_first_date) {
        edge_list[[i]] <- NULL
        next
      }
      
      candidates <- df_group %>%
        mutate(
          dist_km = dist_mat_km[, i],
          earlier_cluster =
            cluster_id != target$cluster_id &
            first_date < target$first_date,
          ongoing_source =
            earlier_cluster &
            target$first_date <= (last_date + lubridate::days(source_ongoing_days))
        )
      
      ongoing_candidates <- candidates %>%
        filter(ongoing_source) %>%
        arrange(dist_km, first_date, desc(last_date), desc(n_detections), cluster_id)
      
      if (nrow(ongoing_candidates) > 0) {
        source <- ongoing_candidates %>% slice(1)
      } else {
        earlier_candidates <- candidates %>%
          filter(earlier_cluster) %>%
          arrange(dist_km, first_date, desc(last_date), desc(n_detections), cluster_id)
        
        if (nrow(earlier_candidates) == 0) {
          stop(
            paste0(
              "No valid source found for non-root cluster ",
              target$cluster_id,
              " in emergence group ",
              target$emergence_group_s,
              "."
            )
          )
        }
        
        source <- earlier_candidates %>% slice(1)
      }
      
      edge_list[[i]] <- tibble(
        emergence_group_s = target$emergence_group_s,
        from_cluster_id   = source$cluster_id,
        from_x            = source$cx,
        from_y            = source$cy,
        to_cluster_id     = target$cluster_id,
        to_x              = target$cx,
        to_y              = target$cy
      )
    }
    
    bind_rows(edge_list)
  }
  
  edges_df <- cluster_pts %>%
    group_split(emergence_group_s) %>%
    map_dfr(assign_sources_one_group, source_ongoing_days = source_ongoing_days)
  
  cluster_poly <- det_clustered %>%
    group_split(cluster_id) %>%
    map_dfr(function(g) {
      n_det <- nrow(g)
      first_dt <- min(g$virus_date, na.rm = TRUE)
      last_dt  <- max(g$virus_date, na.rm = TRUE)
      
      buffer_km <- case_when(
        n_det == 1 ~ 0,
        n_det > 1 ~ 30
      )
      
      geom_combined <- st_combine(g$geometry)
      
      cluster_geom <- if (n_det == 1) {
        geom_combined
      } else {
        st_convex_hull(geom_combined)
      }
      
      if (buffer_km > 0) {
        cluster_geom <- st_buffer(cluster_geom, dist = buffer_km * 1000)
      }
      
      cluster_geom <- st_make_valid(cluster_geom)
      
      st_sf(
        emergence_group_s = g$emergence_group_s[1],
        cluster_id        = g$cluster_id[1],
        first_date        = first_dt,
        last_date         = last_dt,
        n_detections      = n_det,
        geometry          = st_sfc(cluster_geom, crs = 4326)
      )
    }) %>%
    st_as_sf()
  
  cluster_labels <- cluster_poly %>%
    mutate(label_geom = st_point_on_surface(geometry)) %>%
    st_set_geometry("label_geom") %>%
    mutate(
      lx = st_coordinates(.)[, 1],
      ly = st_coordinates(.)[, 2]
    ) %>%
    st_drop_geometry()
  
  list(
    det_clustered = det_clustered,
    cluster_pts = cluster_pts,
    cluster_poly = cluster_poly,
    cluster_labels = cluster_labels,
    edges_df = edges_df
  )
}

build_arrow_segments <- function(edges_df,
                                 head_length_km = 40,
                                 head_angle_deg = 25) {
  if (nrow(edges_df) == 0) {
    return(list(shafts = tibble(), heads = tibble()))
  }
  
  make_one <- function(x1, y1, x2, y2, grp) {
    len_m <- geosphere::distHaversine(c(x1, y1), c(x2, y2))
    if (len_m == 0) return(NULL)
    
    shaft <- tibble(
      emergence_group_s = grp,
      x = x1, y = y1,
      xend = x2, yend = y2
    )
    
    head_len_m <- min(head_length_km * 1000, len_m * 0.25)
    bearing <- geosphere::bearing(c(x1, y1), c(x2, y2))
    
    b1 <- bearing + 180 - head_angle_deg
    b2 <- bearing + 180 + head_angle_deg
    
    h1 <- geosphere::destPoint(c(x2, y2), b1, head_len_m)
    h2 <- geosphere::destPoint(c(x2, y2), b2, head_len_m)
    
    heads <- tibble(
      emergence_group_s = grp,
      x = c(x2, x2),
      y = c(y2, y2),
      xend = c(h1[1], h2[1]),
      yend = c(h1[2], h2[2])
    )
    
    list(shaft = shaft, heads = heads)
  }
  
  pieces <- pmap(
    list(edges_df$from_x, edges_df$from_y, edges_df$to_x, edges_df$to_y, edges_df$emergence_group_s),
    make_one
  ) %>%
    compact()
  
  if (length(pieces) == 0) {
    return(list(shafts = tibble(), heads = tibble()))
  }
  
  list(
    shafts = bind_rows(map(pieces, "shaft")),
    heads  = bind_rows(map(pieces, "heads"))
  )
}

make_min_extent_poly <- function(df, min_extent_countries) {
  df_sub <- df %>%
    filter(admin0official_name %in% min_extent_countries)
  
  if (nrow(df_sub) > 0) {
    out <- df_sub %>%
      st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE) %>%
      group_by(admin0official_name) %>%
      summarise(geometry = st_combine(geometry), .groups = "drop") %>%
      mutate(geometry = st_convex_hull(geometry)) %>%
      st_as_sf()
  } else {
    out <- df %>%
      st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE) %>%
      summarise(geometry = st_combine(geometry)) %>%
      mutate(geometry = st_convex_hull(geometry)) %>%
      st_as_sf()
  }
  
  out
}

make_extent_bbox <- function(det_sub, cluster_poly_sub, label_df_sub,
                             edges_sub, min_extent_poly, pad_deg = 3) {
  x_vals <- det_sub$x
  y_vals <- det_sub$y
  
  if (nrow(cluster_poly_sub) > 0) {
    bb_poly <- sf::st_bbox(cluster_poly_sub)
    x_vals <- c(x_vals, unname(bb_poly["xmin"]), unname(bb_poly["xmax"]))
    y_vals <- c(y_vals, unname(bb_poly["ymin"]), unname(bb_poly["ymax"]))
  }
  
  if (nrow(label_df_sub) > 0) {
    x_vals <- c(x_vals, label_df_sub$lx)
    y_vals <- c(y_vals, label_df_sub$ly)
  }
  
  if (nrow(edges_sub) > 0) {
    x_vals <- c(x_vals, edges_sub$from_x, edges_sub$to_x)
    y_vals <- c(y_vals, edges_sub$from_y, edges_sub$to_y)
  }
  
  bb_min <- sf::st_bbox(min_extent_poly)
  x_vals <- c(x_vals, unname(bb_min["xmin"]), unname(bb_min["xmax"]))
  y_vals <- c(y_vals, unname(bb_min["ymin"]), unname(bb_min["ymax"]))
  
  list(
    xmin = min(x_vals, na.rm = TRUE) - pad_deg,
    xmax = max(x_vals, na.rm = TRUE) + pad_deg,
    ymin = min(y_vals, na.rm = TRUE) - pad_deg,
    ymax = max(y_vals, na.rm = TRUE) + pad_deg
  )
}

get_cartolight_df <- function(bbox_vals, zoom = 5) {
  bb <- raster::extent(
    bbox_vals$xmin,
    bbox_vals$xmax,
    bbox_vals$ymin,
    bbox_vals$ymax
  )
  
  r <- rosm::osm.raster(
    x = bb,
    zoom = zoom,
    type = "cartolight",
    crop = TRUE,
    projection = sp::CRS("EPSG:4326")
  )
  
  df <- raster::as.data.frame(r, xy = TRUE)
  
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

make_map_plot <- function(det_sub,
                          cluster_poly_sub,
                          cluster_labels_sub,
                          edges_sub,
                          min_extent_poly,
                          label_mode = "cluster_summary",
                          group_palette,
                          show_arrows = TRUE,
                          point_size = 1.8,
                          tile_zoom = 5) {
  bbox_vals <- make_extent_bbox(
    det_sub = det_sub,
    cluster_poly_sub = cluster_poly_sub,
    label_df_sub = cluster_labels_sub,
    edges_sub = edges_sub,
    min_extent_poly = min_extent_poly,
    pad_deg = 3
  )
  
  carto_df <- get_cartolight_df(bbox_vals, zoom = tile_zoom)
  arrows <- if (isTRUE(show_arrows)) {
    build_arrow_segments(edges_sub)
  } else {
    list(shafts = tibble(), heads = tibble())
  }
  
  cluster_label_plot <- cluster_labels_sub %>%
    dplyr::select(-any_of(c("first_date", "last_date", "n_detections"))) %>%
    left_join(
      cluster_poly_sub %>%
        st_drop_geometry() %>%
        transmute(cluster_id, first_date, last_date, n_detections),
      by = "cluster_id"
    ) %>%
    mutate(
      label_text = purrr::pmap_chr(
        list(first_date, last_date, n_detections),
        ~ make_cluster_label_text(..1, ..2, ..3, label_mode)
      )
    ) %>%
    filter(!is.na(label_text))
  
  det_label_plot <- det_sub %>%
    st_drop_geometry() %>%
    mutate(
      det_label = format(virus_date, "%Y-%m-%d")
    )
  
  p <- ggplot() +
    geom_raster(
      data = carto_df,
      aes(x = x, y = y, fill = fill)
    ) +
    scale_fill_identity() +
    ggnewscale::new_scale_fill() +
    geom_sf(data = adm0, color = "black", fill = NA)
  
  if (nrow(cluster_poly_sub) > 0) {
    p <- p +
      geom_sf(
        data = cluster_poly_sub,
        aes(fill = emergence_group_s, color = emergence_group_s),
        alpha = 0.18,
        linewidth = 0.6,
        show.legend = FALSE,
        inherit.aes = FALSE
      )
  }
  
  arrow_palette <- make_darker_palette(group_palette)
  
  if (nrow(arrows$shafts) > 0 || nrow(arrows$heads) > 0) {
    p <- p + ggnewscale::new_scale_color()
    
    if (nrow(arrows$shafts) > 0) {
      p <- p +
        geom_segment(
          data = arrows$shafts,
          aes(
            x = x,
            y = y,
            xend = xend,
            yend = yend,
            color = emergence_group_s
          ),
          linewidth = 0.7,
          inherit.aes = FALSE
        )
    }
    
    if (nrow(arrows$heads) > 0) {
      p <- p +
        geom_segment(
          data = arrows$heads,
          aes(
            x = x,
            y = y,
            xend = xend,
            yend = yend,
            color = emergence_group_s
          ),
          linewidth = 0.7,
          inherit.aes = FALSE
        )
    }
    
    p <- p +
      scale_color_manual(
        values = arrow_palette,
        guide = "none"
      )
    
    p <- p + ggnewscale::new_scale_color()
  }
  
  p <- p +
    geom_sf(
      data = det_sub,
      aes(color = emergence_group_s, shape = surveillance_type),
      size = point_size,
      alpha = 0.95,
      inherit.aes = FALSE
    ) +
    scale_shape_manual(
      values = shape_values,
      breaks = intersect(names(shape_values), unique(det_sub$surveillance_type)),
      drop = FALSE,
      name = "Surveillance type"
    )
  
  if (identical(label_mode, "all_detection_dates") && nrow(det_label_plot) > 0) {
    p <- p +
      ggrepel::geom_label_repel(
        data = det_label_plot,
        aes(x = x, y = y, label = det_label),
        size = 2.2,
        fill = "white",
        color = "black",
        alpha = 0.7,
        label.size = 0.2,
        point.padding = 0.15,
        box.padding = 0.2,
        min.segment.length = 0,
        max.overlaps = Inf,
        seed = 123
      )
  }
  
  if ((identical(label_mode, "cluster_summary") || identical(label_mode, "cluster_years")) &&
      nrow(cluster_label_plot) > 0) {
    p <- p +
      ggrepel::geom_label_repel(
        data = cluster_label_plot,
        aes(x = lx, y = ly, label = label_text),
        size = 2.5,
        fill = "white",
        color = "black",
        alpha = 0.7,
        label.size = 0.25,
        point.padding = 0.2,
        box.padding = 0.25,
        min.segment.length = 0,
        max.overlaps = Inf,
        lineheight = 0.95,
        seed = 123
      )
  }
  
  p +
    scale_color_manual(values = group_palette, name = "Emergence group") +
    scale_fill_manual(values = group_palette, guide = "none") +
    coord_sf(
      xlim = c(bbox_vals$xmin, bbox_vals$xmax),
      ylim = c(bbox_vals$ymin, bbox_vals$ymax),
      expand = FALSE,
      crs = st_crs(4326)
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      plot.margin = margin(4, 4, 4, 4)
    )
}

make_epicurve_plot <- function(df, group_palette, period_end_date) {
  epi_df <- df %>%
    mutate(month = lubridate::floor_date(virus_date, "month"))
  
  min_month <- min(epi_df$month, na.rm = TRUE)
  max_month <- lubridate::floor_date(as.Date(period_end_date), "month")
  
  month_seq <- seq.Date(min_month, max_month, by = "month")
  n_months <- length(month_seq)
  
  date_break_interval <- dplyr::case_when(
    n_months <= 12 ~ "1 month",
    n_months <= 24 ~ "2 months",
    TRUE ~ "6 months"
  )
  
  if (length(unique(df$emergence_group_s)) == 1) {
    epi_sum <- epi_df %>%
      count(month, admin0official_name, name = "n") %>%
      tidyr::complete(
        month = month_seq,
        admin0official_name,
        fill = list(n = 0)
      )
    
    ggplot(epi_sum, aes(x = month, y = n, fill = admin0official_name)) +
      geom_col() +
      scale_x_date(
        limits = c(min_month, max_month),
        date_breaks = date_break_interval,
        date_labels = "%Y-%m",
        expand = c(0, 0)
      ) +
      labs(
        x = NULL,
        y = "Detections",
        fill = "Country",
        title = "Monthly detections by country"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")
      )
  } else {
    epi_sum <- epi_df %>%
      count(month, emergence_group_s, name = "n") %>%
      tidyr::complete(
        month = month_seq,
        emergence_group_s,
        fill = list(n = 0)
      )
    
    ggplot(epi_sum, aes(x = month, y = n, fill = emergence_group_s)) +
      geom_col() +
      scale_fill_manual(values = group_palette, name = "Emergence group") +
      scale_x_date(
        limits = c(min_month, max_month),
        date_breaks = date_break_interval,
        date_labels = "%Y-%m",
        expand = c(0, 0)
      ) +
      labs(
        x = NULL,
        y = "Detections",
        title = "Monthly detections by emergence group"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")
      )
  }
}

save_combined_to_ppt <- function(title_text,
                                 subtitle_text,
                                 map_plot,
                                 epi_plot,
                                 about_text,
                                 ppt_file,
                                 png_file,
                                 width = 13.333,
                                 height = 7.5,
                                 dpi = 300) {
  
  title_plot <- ggplot() +
    annotate("text", x = 0, y = 1, label = title_text,
             hjust = 0, vjust = 1, size = 6, fontface = "bold") +
    annotate("text", x = 0, y = 0.2, label = subtitle_text,
             hjust = 0, vjust = 1, size = 3.5) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void()
  
  combined <- title_plot / map_plot / epi_plot +
    plot_layout(heights = c(0.12, 0.60, 0.28))
  
  ggsave(
    filename = png_file,
    plot = combined,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )
  
  ppt <- read_pptx()
  slide_dims <- slide_size(ppt)
  
  # Slide 1: Map + Epicurve
  ppt <- ppt %>%
    add_slide(layout = "Blank", master = "Office Theme") %>%
    ph_with(
      value = external_img(
        png_file,
        width = slide_dims$width,
        height = slide_dims$height
      ),
      location = ph_location(
        left = 0,
        top = 0,
        width = slide_dims$width,
        height = slide_dims$height
      )
    )
  
  # Format About text with smaller font
  about_paragraph <- officer::fpar(
    officer::ftext(
      about_text,
      officer::fp_text(
        font.size = 10
      )
    )
  )
  
  # Slide 2: About text
  ppt <- ppt %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(
      value = "About",
      location = ph_location_type(type = "title")
    ) %>%
    ph_with(
      value = about_paragraph,
      location = ph_location_type(type = "body")
    )
  
  print(ppt, target = ppt_file)
  invisible(ppt_file)
}

# ============================================================
# 4) UI
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .control-label { font-weight: 600; }
    "))
  ),
  
  titlePanel(HTML("Horn of Africa cVDPV emergence-group mapping:<br>cVDPV emergence groups detecting in HoA, estimated transmission clusters and spread")),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      actionButton(
        "update_plot",
        "Update plot"
      ),
      
      tags$br(),
      tags$br(),
      
      dateRangeInput(
        "time_period",
        "Detection date range",
        start = global_min_date,
        end = global_max_date,
        min = global_min_date,
        max = global_max_date,
        format = "yyyy-mm-dd",
        separator = " to "
      ),
      
      checkboxGroupInput(
        "surveillance_types",
        "Surveillance types",
        choices = all_surveillance_types,
        selected = intersect(c("AFP", "Environmental"), all_surveillance_types)
      ),
      
      sliderInput(
        "cluster_spatial_eps_km",
        "Maximum distance between detections within a cluster (km)",
        min = 10,
        max = 500,
        value = 150,
        step = 10
      ),
      
      sliderInput(
        "cluster_time_gap_days",
        "Maximum time window for clustering and transmission (days since last detection)",
        min = 30,
        max = 730,
        value = 365,
        step = 5
      ),
      
      tags$hr(),
      
      h4("Detected in Somalia"),
      checkboxGroupInput(
        "emergence_groups_somalia",
        NULL,
        choices = character(0),
        selected = character(0)
      ),
      
      tags$hr(),
      
      h4("Detected only in rest of Horn of Africa"),
      checkboxGroupInput(
        "emergence_groups_rest",
        NULL,
        choices = character(0),
        selected = character(0)
      ),
      
      radioButtons(
        "label_mode",
        "Labels",
        choices = c(
          "None" = "none",
          "All detection dates" = "all_detection_dates",
          "Cluster summary" = "cluster_summary",
          "Cluster years" = "cluster_years"
        ),
        selected = "cluster_summary"
      ),
      checkboxInput(
        "show_arrows",
        "Show inferred transmission between clusters",
        value = TRUE
      ),
      
      tags$hr(),
      
      downloadButton("download_ppt", "Download current slide to PPT")
    ),
    
    mainPanel(
      width = 9,
      wellPanel(
        h4("About"),
        p(make_about_text())
      ),
      br(),
      plotOutput("map_plot", height = 700),
      br(),
      plotOutput("epicurve", height = 260)
    )
  )
)

# ============================================================
# 5) SERVER
# ============================================================

server <- function(input, output, session) {
  
  plot_inputs <- eventReactive(input$update_plot, {
    list(
      time_period = input$time_period,
      surveillance_types = input$surveillance_types,
      emergence_groups = selected_emergence_groups(),
      cluster_spatial_eps_km = input$cluster_spatial_eps_km,
      cluster_time_gap_days = input$cluster_time_gap_days,
      label_mode = input$label_mode,
      show_arrows = input$show_arrows
    )
  }, ignoreInit = FALSE)
  
  choice_pool_df <- reactive({
    
    validate(
      need(length(input$surveillance_types) > 0, "Select at least one surveillance type."),
      need(length(input$time_period) == 2, "Select a valid date range.")
    )
    
    data_clean %>%
      filter(
        surveillance_type %in% input$surveillance_types,
        virus_date >= as.Date(input$time_period[1]),
        virus_date <= as.Date(input$time_period[2])
      )
  })
  
  emergence_group_meta_reactive <- reactive({
    make_emergence_meta(choice_pool_df())
  })
  
  somalia_meta_reactive <- reactive({
    emergence_group_meta_reactive() %>%
      filter(detected_in_somalia)
  })
  
  rest_meta_reactive <- reactive({
    emergence_group_meta_reactive() %>%
      filter(!detected_in_somalia)
  })
  
  observe({
    somalia_meta <- somalia_meta_reactive()
    rest_meta <- rest_meta_reactive()
    
    somalia_choices <- stats::setNames(
      somalia_meta$emergence_group_s,
      somalia_meta$display_label
    )
    
    rest_choices <- stats::setNames(
      rest_meta$emergence_group_s,
      rest_meta$display_label
    )
    
    current_somalia <- input$emergence_groups_somalia
    current_rest <- input$emergence_groups_rest
    
    if (is.null(current_somalia)) current_somalia <- character(0)
    if (is.null(current_rest)) current_rest <- character(0)
    
    valid_somalia <- intersect(current_somalia, somalia_meta$emergence_group_s)
    valid_rest <- intersect(current_rest, rest_meta$emergence_group_s)
    
    if (length(valid_somalia) == 0 && length(valid_rest) == 0) {
      valid_somalia <- somalia_meta$emergence_group_s[
        seq_len(min(3, nrow(somalia_meta)))
      ]
      valid_rest <- character(0)
    }
    
    updateCheckboxGroupInput(
      session,
      "emergence_groups_somalia",
      choices = somalia_choices,
      selected = valid_somalia
    )
    
    updateCheckboxGroupInput(
      session,
      "emergence_groups_rest",
      choices = rest_choices,
      selected = valid_rest
    )
  })
  
  selected_emergence_groups <- reactive({
    
    somalia_sel <- input$emergence_groups_somalia
    rest_sel <- input$emergence_groups_rest
    
    if (is.null(somalia_sel)) somalia_sel <- character(0)
    if (is.null(rest_sel)) rest_sel <- character(0)
    
    unique(c(somalia_sel, rest_sel))
  })
  
  filtered_df <- eventReactive(input$update_plot, {
    plot_args <- plot_inputs()
    
    validate(
      need(length(plot_args$surveillance_types) > 0, "Select at least one surveillance type."),
      need(length(plot_args$emergence_groups) > 0, "Select at least one emergence group."),
      need(length(plot_args$time_period) == 2, "Select a valid date range.")
    )
    
    data_clean %>%
      filter(
        surveillance_type %in% plot_args$surveillance_types,
        virus_date >= as.Date(plot_args$time_period[1]),
        virus_date <= as.Date(plot_args$time_period[2]),
        emergence_group_s %in% plot_args$emergence_groups
      )
  }, ignoreInit = FALSE)
  
  min_extent_poly_reactive <- eventReactive(input$update_plot, {
    plot_args <- plot_inputs()
    
    pool_df <- data_clean %>%
      filter(
        surveillance_type %in% plot_args$surveillance_types,
        virus_date >= as.Date(plot_args$time_period[1]),
        virus_date <= as.Date(plot_args$time_period[2])
      )
    
    validate(
      need(nrow(pool_df) > 0, "No detections match the current surveillance type/date filters.")
    )
    
    make_min_extent_poly(pool_df, min_extent_countries)
  }, ignoreInit = FALSE)
  
  processed <- eventReactive(input$update_plot, {
    df <- filtered_df()
    plot_args <- plot_inputs()
    
    build_clustered_data(
      df = df,
      spatial_eps_km = plot_args$cluster_spatial_eps_km,
      time_gap_days = plot_args$cluster_time_gap_days,
      source_ongoing_days = plot_args$cluster_time_gap_days
    )
  }, ignoreInit = FALSE)
  
  current_palette <- eventReactive(input$update_plot, {
    plot_args <- plot_inputs()
    
    validate(
      need(length(plot_args$emergence_groups) > 0, "Select at least one emergence group.")
    )
    
    make_group_palette(plot_args$emergence_groups)
  }, ignoreInit = FALSE)
  
  current_map_plot <- eventReactive(input$update_plot, {
    df <- filtered_df()
    proc <- processed()
    plot_args <- plot_inputs()
    
    validate(
      need(nrow(df) > 0, "No detections match the current filters.")
    )
    
    make_map_plot(
      det_sub = proc$det_clustered,
      cluster_poly_sub = proc$cluster_poly,
      cluster_labels_sub = proc$cluster_labels,
      edges_sub = proc$edges_df,
      min_extent_poly = min_extent_poly_reactive(),
      label_mode = plot_args$label_mode,
      group_palette = current_palette(),
      show_arrows = plot_args$show_arrows,
      tile_zoom = tile_zoom
    )
  }, ignoreInit = FALSE)
  
  current_epi_plot <- eventReactive(input$update_plot, {
    df <- filtered_df()
    plot_args <- plot_inputs()
    
    validate(
      need(nrow(df) > 0, "No detections match the current filters.")
    )
    
    make_epicurve_plot(
      df = df,
      group_palette = current_palette(),
      period_end_date = plot_args$time_period[2]
    )
  }, ignoreInit = FALSE)
  
  output$map_plot <- renderPlot({
    current_map_plot()
  }, res = 110)
  
  output$epicurve <- renderPlot({
    current_epi_plot()
  }, res = 110)
  
  output$download_ppt <- downloadHandler(
    filename = function() {
      paste0("emergence_map_", format(Sys.Date(), "%Y%m%d"), ".pptx")
    },
    content = function(file) {
      tmpdir <- tempdir()
      png_file <- file.path(tmpdir, "current_slide.png")
      
      plot_args <- plot_inputs()
      
      title_text <- make_emergence_title(
        plot_args$emergence_groups,
        emergence_group_meta_reactive()
      )
      
      subtitle_text <- paste0(
        "Date range: ",
        format(as.Date(plot_args$time_period[1]), "%Y-%m-%d"),
        " to ",
        format(as.Date(plot_args$time_period[2]), "%Y-%m-%d"),
        " | Surveillance types: ",
        paste(plot_args$surveillance_types, collapse = ", "),
        " | Maximum cluster distance: ",
        plot_args$cluster_spatial_eps_km,
        " km",
        " | Maximum time window: ",
        plot_args$cluster_time_gap_days,
        " days"
      )
      
      save_combined_to_ppt(
        title_text = title_text,
        subtitle_text = subtitle_text,
        map_plot = current_map_plot(),
        epi_plot = current_epi_plot(),
        about_text = make_about_text(),
        ppt_file = file,
        png_file = png_file,
        width = 13.333,
        height = 7.5,
        dpi = 300
      )
    }
  )
}

# ============================================================
# 6) RUN
# ============================================================

shinyApp(ui, server)