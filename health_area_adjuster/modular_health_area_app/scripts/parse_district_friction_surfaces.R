# =============================================================================
# cut_district_friction.R
#
# Reads a nationally standardized 0–1 friction raster and district shapefile,
# then:
#   - crops/masks friction by district
#   - replaces outside-district NA cells with 0
#   - writes one GeoTIFF per district
#
# Assumptions about the national raster:
#   - values already standardized to 0–1 nationally
#   - impassable cells already equal 1
#   - no internal NA cells expected
#
# Output rules:
#   - keep national 0–1 scale
#   - outside district -> 0
#   - do NOT re-standardize within district
#
# Example output filename:
#   banadir__banadir__danyile__friction_100m.tif
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
})

# =============================================================================
# USER SETTINGS
# =============================================================================

cfg <- list(
  # Input national friction raster created by build_national_friction_surface.R
  friction_file = "data/friction/somalia_friction_100m.tif",
  
  # District shapefile object saved as RDS
  districts_file = "data/districts_shp.Rds",
  
  # Output folder
  output_dir = "data/friction/district_standardized",
  
  # Write options
  overwrite = TRUE
)

# =============================================================================
# HELPERS
# =============================================================================

message_line <- function(...) {
  message(paste0(...))
}

assert_file_exists <- function(path, label) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    stop(label, " not found: ", path, call. = FALSE)
  }
}

read_districts_safe <- function(path) {
  assert_file_exists(path, "districts_file")
  
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    x <- readRDS(path)
    if (!inherits(x, "sf")) {
      stop("districts_file RDS must contain an sf object.", call. = FALSE)
    }
  } else {
    x <- sf::st_read(path, quiet = TRUE)
  }
  
  x |>
    sf::st_make_valid()
}

find_name_col <- function(x, candidates, label) {
  hit <- intersect(candidates, names(x))
  if (length(hit) == 0) {
    stop(
      "Could not find a ", label, " field. Tried: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }
  hit[[1]]
}

clean_name_for_file <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- gsub("_+", "_", x)
  x
}

make_output_name <- function(zone, region, district) {
  paste0(
    clean_name_for_file(zone), "__",
    clean_name_for_file(region), "__",
    clean_name_for_file(district), "__friction_100m.tif"
  )
}

write_raster_safe <- function(x, filename, overwrite = TRUE) {
  terra::writeRaster(
    x,
    filename,
    overwrite = overwrite,
    gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
}

fill_masked_cells_with_zero <- function(r) {
  vals <- terra::values(r, mat = FALSE)
  
  if (length(vals) == 0) {
    return(r)
  }
  
  vals[is.na(vals)] <- 0
  
  out <- r
  terra::values(out) <- vals
  out
}

# =============================================================================
# LOAD INPUTS
# =============================================================================

message_line("============================================================")
message_line("Cutting district friction rasters from national 0–1 surface")
message_line("============================================================")

assert_file_exists(cfg$friction_file, "friction_file")
assert_file_exists(cfg$districts_file, "districts_file")

friction_r <- terra::rast(cfg$friction_file)
districts_sf <- read_districts_safe(cfg$districts_file)

dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)

# Reproject districts to match raster CRS
districts_sf <- sf::st_transform(districts_sf, terra::crs(friction_r))
districts_sf <- sf::st_make_valid(districts_sf)

message_line("Districts loaded: ", nrow(districts_sf))
message_line("Friction raster loaded: ", basename(cfg$friction_file))

# =============================================================================
# IDENTIFY NAME FIELDS
# =============================================================================

zone_col <- find_name_col(
  districts_sf,
  c("DISP_LS"),
  "zone"
)

region_col <- find_name_col(
  districts_sf,
  c("DISP_L1"),
  "region"
)

district_col <- find_name_col(
  districts_sf,
  c("DISP_L2"),
  "district"
)

message_line("Using zone field: ", zone_col)
message_line("Using region field: ", region_col)
message_line("Using district field: ", district_col)

# =============================================================================
# PROCESS DISTRICTS
# =============================================================================

results <- vector("list", nrow(districts_sf))

for (i in seq_len(nrow(districts_sf))) {
  d <- districts_sf[i, ]
  
  zone_name <- as.character(d[[zone_col]][1])
  region_name <- as.character(d[[region_col]][1])
  district_name <- as.character(d[[district_col]][1])
  
  message_line(
    "[", i, "/", nrow(districts_sf), "] ",
    zone_name, " / ", region_name, " / ", district_name
  )
  
  d_vect <- terra::vect(d)
  
  r_crop <- tryCatch(
    terra::crop(friction_r, d_vect),
    error = function(e) NULL
  )
  
  if (is.null(r_crop)) {
    message_line("  Skipped: crop failed.")
    results[[i]] <- data.frame(
      zone = zone_name,
      region = region_name,
      district = district_name,
      file = NA_character_,
      status = "crop_failed",
      stringsAsFactors = FALSE
    )
    next
  }
  
  r_mask <- tryCatch(
    terra::mask(r_crop, d_vect),
    error = function(e) NULL
  )
  
  if (is.null(r_mask)) {
    message_line("  Skipped: mask failed.")
    results[[i]] <- data.frame(
      zone = zone_name,
      region = region_name,
      district = district_name,
      file = NA_character_,
      status = "mask_failed",
      stringsAsFactors = FALSE
    )
    next
  }
  
  vals <- terra::values(r_mask, mat = FALSE)
  if (length(vals) == 0) {
    message_line("  Skipped: no raster values.")
    results[[i]] <- data.frame(
      zone = zone_name,
      region = region_name,
      district = district_name,
      file = NA_character_,
      status = "empty",
      stringsAsFactors = FALSE
    )
    next
  }
  
  # Keep national 0–1 scale; only fill outside-district cells with 0
  r_out <- fill_masked_cells_with_zero(r_mask)
  
  out_name <- make_output_name(zone_name, region_name, district_name)
  out_file <- file.path(cfg$output_dir, out_name)
  
  write_raster_safe(
    x = r_out,
    filename = out_file,
    overwrite = cfg$overwrite
  )
  
  out_vals <- terra::values(r_out, mat = FALSE)
  out_min <- min(out_vals, na.rm = TRUE)
  out_max <- max(out_vals, na.rm = TRUE)
  out_na <- sum(is.na(out_vals))
  out_zero <- sum(out_vals == 0, na.rm = TRUE)
  out_one <- sum(out_vals == 1, na.rm = TRUE)
  
  message_line(
    "  Saved: ", out_name,
    " | min=", round(out_min, 4),
    " max=", round(out_max, 4),
    " na=", out_na,
    " zeros=", out_zero,
    " ones=", out_one
  )
  
  results[[i]] <- data.frame(
    zone = zone_name,
    region = region_name,
    district = district_name,
    file = out_file,
    status = "saved",
    min_value = out_min,
    max_value = out_max,
    na_cells = out_na,
    zero_cells = out_zero,
    one_cells = out_one,
    stringsAsFactors = FALSE
  )
}

results_df <- dplyr::bind_rows(results)

# =============================================================================
# SAVE LOG
# =============================================================================

log_file <- file.path(cfg$output_dir, "district_friction_export_log.csv")
write.csv(results_df, log_file, row.names = FALSE)

message_line("------------------------------------------------------------")
message_line("Finished.")
message_line("Saved rasters to: ", normalizePath(cfg$output_dir))
message_line("Saved log: ", normalizePath(log_file))
message_line("------------------------------------------------------------")

