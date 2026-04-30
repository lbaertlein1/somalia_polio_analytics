# =========================================================
# DHIS2: create demo DFA points nested within districts
# using district polygons from organisationUnits.geojson
# and upload them back to DHIS2
# =========================================================

library(httr2)
library(jsonlite)
library(sf)
library(dplyr)
library(purrr)
library(tibble)

# -----------------------------
# NULL COALESCE HELPER
# -----------------------------
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

# -----------------------------
# USER SETTINGS
# -----------------------------
base_url <- "https://www.somalia-polio-ims.org"
username <- "admin"
password <- "district"

district_level_name <- "District"
dfa_level_name      <- "DFA"

set.seed(1234)

# If TRUE, only process DFAs with no existing geometry
only_missing_geometry <- FALSE

# First run with TRUE
dry_run <- FALSE

points_csv_out <- "dhis2_dfa_demo_points.csv"
upload_csv_out <- "dhis2_dfa_geometry_upload_results.csv"

# projected CRS for Somalia area sampling
sampling_crs <- 32638

# -----------------------------
# API HELPERS
# -----------------------------
dhis2_get_json <- function(path, query = list(), simplify = TRUE) {
  req <- request(base_url) |>
    req_url_path_append(path) |>
    req_auth_basic(username, password) |>
    req_headers(Accept = "application/json") |>
    req_url_query(!!!query)
  
  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = simplify)
}

dhis2_get_text <- function(path, query = list()) {
  req <- request(base_url) |>
    req_url_path_append(path) |>
    req_auth_basic(username, password) |>
    req_headers(Accept = "application/json") |>
    req_url_query(!!!query)
  
  resp <- req_perform(req)
  resp_body_string(resp)
}

dhis2_post_json <- function(path, body) {
  req <- request(base_url) |>
    req_url_path_append(path) |>
    req_auth_basic(username, password) |>
    req_headers(
      Accept = "application/json",
      `Content-Type` = "application/json"
    ) |>
    req_body_json(body)
  
  resp <- req_perform(req)
  
  list(
    status = resp_status(resp),
    body = tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
  )
}

# -----------------------------
# HELPERS
# -----------------------------
point_to_dhis2_geometry <- function(x, y) {
  list(
    type = "Point",
    coordinates = list(unname(x), unname(y))
  )
}

sample_points_for_polygon <- function(poly_sf, n) {
  if (n == 0) return(NULL)
  
  pts <- tryCatch(
    st_sample(poly_sf, size = n, type = "random", exact = TRUE),
    error = function(e) NULL
  )
  
  if (is.null(pts) || length(pts) < n) {
    fallback <- st_point_on_surface(poly_sf)
    fallback_geom <- st_geometry(fallback)[[1]]
    pts <- st_sfc(rep(list(fallback_geom), n), crs = st_crs(poly_sf))
  }
  
  st_as_sf(
    tibble(sample_index = seq_len(n)),
    geometry = pts
  )
}

# robust extraction of nested parent fields from DHIS2 response
extract_parent_id <- function(parent_obj) {
  if (is.null(parent_obj)) return(NA_character_)
  
  if (is.data.frame(parent_obj) && nrow(parent_obj) > 0) {
    return(as.character(parent_obj$id[[1]] %||% NA_character_))
  }
  
  if (is.list(parent_obj)) {
    return(as.character(parent_obj$id %||% NA_character_))
  }
  
  NA_character_
}

extract_parent_name <- function(parent_obj) {
  if (is.null(parent_obj)) return(NA_character_)
  
  if (is.data.frame(parent_obj) && nrow(parent_obj) > 0) {
    return(as.character(parent_obj$displayName[[1]] %||% parent_obj$name[[1]] %||% NA_character_))
  }
  
  if (is.list(parent_obj)) {
    return(as.character(parent_obj$displayName %||% parent_obj$name %||% NA_character_))
  }
  
  NA_character_
}

# -----------------------------
# VALIDATION
# -----------------------------
if (base_url == "https://YOUR-DHIS2-URL") {
  stop("Please set base_url to your actual DHIS2 instance URL.")
}

if (username == "" || password == "") {
  stop("Please set DHIS2_USERNAME and DHIS2_PASSWORD in your environment.")
}

# -----------------------------
# 1) GET ORG UNIT LEVELS
# -----------------------------
message("Fetching organisation unit levels...")

levels_json <- dhis2_get_json(
  "api/organisationUnitLevels",
  query = list(
    fields = "id,level,displayName,name",
    paging = "false"
  ),
  simplify = TRUE
)

levels_df <- as_tibble(levels_json$organisationUnitLevels)

district_level <- levels_df |>
  filter(displayName == district_level_name | name == district_level_name)

dfa_level <- levels_df |>
  filter(displayName == dfa_level_name | name == dfa_level_name)

if (nrow(district_level) != 1) {
  stop("Could not uniquely identify district level. Check district_level_name.")
}

if (nrow(dfa_level) != 1) {
  stop("Could not uniquely identify DFA level. Check dfa_level_name.")
}

district_level_num <- district_level$level[[1]]
dfa_level_num <- dfa_level$level[[1]]

message("District level = ", district_level_num)
message("DFA level = ", dfa_level_num)

# -----------------------------
# 2) PULL DISTRICT POLYGONS FROM GEOJSON ENDPOINT
# -----------------------------
message("Fetching district polygons from organisationUnits.geojson ...")

district_geojson_txt <- dhis2_get_text(
  "api/organisationUnits.geojson",
  query = list(
    level = district_level_num
  )
)

districts_sf <- st_read(
  dsn = district_geojson_txt,
  quiet = TRUE,
  stringsAsFactors = FALSE
)

if (nrow(districts_sf) == 0) {
  stop("No district features returned from organisationUnits.geojson.")
}

districts_sf <- districts_sf |>
  st_make_valid()

districts_sf <- districts_sf |>
  mutate(geom_type = as.character(st_geometry_type(districts_sf))) |>
  filter(geom_type %in% c("POLYGON", "MULTIPOLYGON"))

if (nrow(districts_sf) == 0) {
  stop("District GeoJSON returned no polygon or multipolygon features.")
}

# DHIS2 GeoJSON features use 'id' for org unit UID
if (!"id" %in% names(districts_sf)) {
  stop("District GeoJSON does not contain an 'id' column.")
}

# Add a clean district_id field for joins
districts_sf <- districts_sf |>
  mutate(district_id = as.character(id))

message("District polygons available: ", nrow(districts_sf))

# -----------------------------
# 3) PULL DFAs FROM JSON ENDPOINT
# -----------------------------
message("Fetching DFA organisation units...")

dfa_json <- dhis2_get_json(
  "api/organisationUnits",
  query = list(
    fields = "id,name,displayName,code,level,parent[id,name,displayName],geometry",
    paging = "false",
    filter = paste0("level:eq:", dfa_level_num)
  ),
  simplify = FALSE
)

dfa_ous <- dfa_json$organisationUnits

if (is.null(dfa_ous) || length(dfa_ous) == 0) {
  stop("No DFA organisation units returned.")
}

dfa_raw <- tibble(
  id = vapply(dfa_ous, function(x) as.character(x$id %||% NA_character_), character(1)),
  name = vapply(dfa_ous, function(x) as.character(x$name %||% NA_character_), character(1)),
  displayName = vapply(dfa_ous, function(x) as.character(x$displayName %||% x$name %||% NA_character_), character(1)),
  code = vapply(dfa_ous, function(x) as.character(x$code %||% NA_character_), character(1)),
  parent_id = vapply(dfa_ous, function(x) extract_parent_id(x$parent), character(1)),
  parent_name = vapply(dfa_ous, function(x) extract_parent_name(x$parent), character(1)),
  has_geometry = vapply(dfa_ous, function(x) !is.null(x$geometry), logical(1))
)

if (only_missing_geometry) {
  dfa_raw <- dfa_raw |>
    filter(!has_geometry)
}

if (nrow(dfa_raw) == 0) {
  stop("No DFA org units left to process after geometry filter.")
}

message("DFAs to process: ", nrow(dfa_raw))

# -----------------------------
# 4) MATCH DFAs TO DISTRICTS
# -----------------------------
dfa_counts <- dfa_raw |>
  count(parent_id, name = "n_dfa")

districts_with_dfas <- districts_sf |>
  left_join(dfa_counts, by = c("district_id" = "parent_id")) |>
  mutate(n_dfa = ifelse(is.na(n_dfa), 0L, n_dfa)) |>
  filter(n_dfa > 0)

if (nrow(districts_with_dfas) == 0) {
  stop("No district polygons matched the DFA parent IDs.")
}

message("Districts matched to DFA parents: ", nrow(districts_with_dfas))

# -----------------------------
# 5) SAMPLE ONE POINT PER DFA INSIDE DISTRICT
# -----------------------------
message("Generating demo points inside district polygons...")

districts_proj <- st_transform(districts_with_dfas, sampling_crs)

sampled_points_list <- lapply(seq_len(nrow(districts_proj)), function(i) {
  poly_i <- districts_proj[i, ]
  n_i <- poly_i$n_dfa[[1]]
  
  pts_i <- sample_points_for_polygon(poly_i, n_i)
  
  pts_i |>
    mutate(parent_id = poly_i$district_id[[1]])
})

sampled_points <- do.call(rbind, sampled_points_list) |>
  st_as_sf()

dfa_ordered <- dfa_raw |>
  group_by(parent_id) |>
  arrange(parent_id, displayName, id, .by_group = TRUE) |>
  mutate(sample_index = row_number()) |>
  ungroup()

sampled_points <- sampled_points |>
  group_by(parent_id) |>
  arrange(parent_id, sample_index, .by_group = TRUE) |>
  ungroup()

dfa_points_sf <- dfa_ordered |>
  left_join(
    sampled_points |>
      st_drop_geometry(),
    by = c("parent_id", "sample_index")
  ) |>
  left_join(
    sampled_points,
    by = c("parent_id", "sample_index")
  ) |>
  st_as_sf()

dfa_points_sf <- st_transform(dfa_points_sf, 4326)

coords <- st_coordinates(dfa_points_sf)

dfa_upload_df <- dfa_points_sf |>
  mutate(
    lon = coords[, "X"],
    lat = coords[, "Y"]
  ) |>
  st_drop_geometry() |>
  select(
    id, name, displayName, code,
    parent_id, parent_name,
    lat, lon
  )

if (nrow(dfa_upload_df) == 0) {
  stop("No DFA points were generated.")
}

message("Prepared ", nrow(dfa_upload_df), " DFA point geometries.")

# -----------------------------
# 6) SAVE / PREVIEW
# -----------------------------
write.csv(dfa_upload_df, points_csv_out, row.names = FALSE)
message("Saved point preview to: ", points_csv_out)

print(
  dfa_upload_df |>
    select(displayName, parent_name, lat, lon) |>
    head(20)
)

# -----------------------------
# 7) UPLOAD TO DHIS2 (RESUMABLE WITH PROGRESS)
# -----------------------------
if (dry_run) {
  
  message("Dry run only. No upload performed.")
  
} else {
  
  message("Uploading DFA point geometries to DHIS2...")
  
  total_n <- nrow(dfa_upload_df)
  
  # -----------------------------
  # RESUME IF FILE EXISTS
  # -----------------------------
  if (file.exists(upload_csv_out)) {
    
    existing_results <- read.csv(upload_csv_out, stringsAsFactors = FALSE)
    
    completed_ids <- existing_results$id
    
    message("Resuming upload...")
    message(length(completed_ids), " already completed")
    
    remaining_df <- dfa_upload_df |>
      filter(!id %in% completed_ids)
    
    upload_results <- existing_results
    
  } else {
    
    remaining_df <- dfa_upload_df
    upload_results <- tibble()
    
  }
  
  remaining_n <- nrow(remaining_df)
  
  message("Remaining uploads: ", remaining_n)
  message("Total DFAs: ", total_n)
  
  if (remaining_n == 0) {
    
    message("All uploads already completed.")
    
  } else {
    
    for (i in seq_len(remaining_n)) {
      
      row <- remaining_df[i, ]
      
      ou_id <- row$id
      lon   <- row$lon
      lat   <- row$lat
      name  <- row$displayName
      
      message(
        sprintf(
          "[%d / %d] Uploading: %s",
          nrow(upload_results) + 1,
          total_n,
          name
        )
      )
      
      body <- point_to_dhis2_geometry(lon, lat)
      
      resp <- tryCatch(
        dhis2_post_json(
          paste0("api/organisationUnits/", ou_id, "/geometry"),
          body = body
        ),
        error = function(e) {
          list(
            status = NA_integer_,
            body = list(message = conditionMessage(e))
          )
        }
      )
      
      new_row <- tibble(
        id = ou_id,
        displayName = name,
        status = resp$status %||% NA_integer_,
        response = toJSON(resp$body, auto_unbox = TRUE, null = "null")
      )
      
      upload_results <- bind_rows(upload_results, new_row)
      
      # -----------------------------
      # CHECKPOINT AFTER EVERY ROW
      # -----------------------------
      write.csv(upload_results, upload_csv_out, row.names = FALSE)
      
    }
    
  }
  
  # -----------------------------
  # SUMMARY
  # -----------------------------
  n_ok <- sum(upload_results$status %in% c(200, 201, 204), na.rm = TRUE)
  n_fail <- nrow(upload_results) - n_ok
  
  message("Upload complete.")
  message("Success: ", n_ok)
  message("Failed: ", n_fail)
  
}






library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)

# -----------------------------
# SETTINGS
# -----------------------------
base_url <- "https://www.somalia-polio-ims.org"
username <- "admin"
password <- "district"

dfa_level_name <- "DFA"

# -----------------------------
# API HELPER
# -----------------------------
dhis2_get <- function(path, query = list()) {
  req <- request(base_url) |>
    req_url_path_append(path) |>
    req_auth_basic(username, password) |>
    req_headers(Accept = "application/json") |>
    req_url_query(!!!query)
  
  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = FALSE)
}

# -----------------------------
# 1) GET DFA LEVEL NUMBER
# -----------------------------
levels_json <- dhis2_get(
  "api/organisationUnitLevels",
  query = list(
    fields = "id,level,displayName,name",
    paging = "false"
  )
)

levels_df <- bind_rows(levels_json$organisationUnitLevels)

dfa_level_num <- levels_df |>
  filter(displayName == dfa_level_name | name == dfa_level_name) |>
  pull(level)

dfa_level_num <- dfa_level_num[1]

cat("DFA level:", dfa_level_num, "\n")

# -----------------------------
# 2) GET ALL DFAs
# -----------------------------
dfa_json <- dhis2_get(
  "api/organisationUnits",
  query = list(
    fields = "id,name,geometry",
    paging = "false",
    filter = paste0("level:eq:", dfa_level_num)
  )
)

dfa_list <- dfa_json$organisationUnits

dfa_df <- tibble(
  id = sapply(dfa_list, function(x) x$id),
  name = sapply(dfa_list, function(x) x$name),
  has_geometry = sapply(dfa_list, function(x) !is.null(x$geometry))
)

# -----------------------------
# 3) SUMMARY
# -----------------------------
total <- nrow(dfa_df)
with_geom <- sum(dfa_df$has_geometry)
without_geom <- total - with_geom

cat("\n")
cat("Total DFAs:        ", total, "\n")
cat("With geometry:     ", with_geom, "\n")
cat("Missing geometry:  ", without_geom, "\n")

if (without_geom == 0) {
  cat("\nUpload appears COMPLETE.\n")
} else {
  cat("\nUpload NOT complete.\n")
}

# -----------------------------
# OPTIONAL: list missing ones
# -----------------------------
missing_df <- dfa_df |>
  filter(!has_geometry)

if (nrow(missing_df) > 0) {
  cat("\nFirst missing DFAs:\n")
  print(head(missing_df, 20))
}

