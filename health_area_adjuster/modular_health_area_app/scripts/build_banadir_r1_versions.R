# =============================================================================
# build_banadir_r1_versions.R
#
# Builds new health-area versions for the 20 Banadir districts in campaign
# "SIA 2026 Round 1" from the BRA coordination-site / U5 workbook
# (data/manual_data/BRA_HA's and U5 Target Population.xlsx).
#
# Per district:
#   * Coordination sites come from the workbook. Each site is matched to the
#     district's MHFL facilities (the same live registry pull and the same
#     district filter the Facilities tab uses):
#       - name match            -> that MHFL facility, MHFL name + coordinates
#       - different name, but   -> that MHFL facility, named
#         within PROX_M of one     "MHFL name (workbook name)"
#       - no match              -> a non-facility coordination site (app_sf)
#                                  at the workbook coordinates
#     Each MHFL facility is used by at most one workbook site.
#   * Health areas are generated with the app's own generation module
#     (initialHealthAreaGenerationServer, run through shiny::testServer)
#     with the same grid size, friction, roads/rivers/subdivision lines,
#     penalty and WorldPop raster the Health Areas tab uses. Polygons are
#     built with build_saved_dfa_sf() -- the app's own grid-to-polygon step.
#     Opening a draft in the app and clicking Submit runs the app's usual
#     boundary refinement on top.
#   * The whole district is in scope; In-Scope Field U5 = workbook U5 target.
#
# Usage (from the app folder):
#   Rscript scripts/build_banadir_r1_versions.R plan  <out_dir>
#       No database writes. Pulls MHFL, matches, generates health areas,
#       writes <out_dir>/site_matches.csv, district_summary.csv,
#       preview_maps.pdf and one <district>.rds per district.
#   Rscript scripts/build_banadir_r1_versions.R write <out_dir>
#       Writes the planned drafts (owner admin, unpublished) from the .rds
#       files made by "plan", sets Waberi's mapping scope to full, and logs
#       to <out_dir>/write_log.csv.
#   Rscript scripts/build_banadir_r1_versions.R refine <out_dir>
#       Boundary refinement for the drafts listed in <out_dir>/write_log.csv:
#       runs the app's own refinement (www/paint-app.js: trace, simplify,
#       snap to the district edge, repair pipeline -- what Submit runs in
#       the browser) in V8 at the app's default settings, repairs any
#       invalid geometry, checks every health area survives with no
#       overlaps, and saves the refined boundaries + their WorldPop U5 onto
#       the same (unpublished) draft. Writes refine_compare.pdf and
#       refine_log.csv. Published versions are never touched.
#
# DB connection: .env credentials; DB_HOST/DB_PORT can be overridden with
# BUILD_DB_HOST/BUILD_DB_PORT (e.g. an SSH tunnel on localhost:15432).
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(sf); library(dplyr); library(jsonlite); library(geojsonsf)
  library(terra); library(exactextractr); library(raster); library(pool); library(DBI)
  library(RPostgres); library(Rcpp); library(readxl); library(stringdist); library(httr)
})

args    <- commandArgs(trailingOnly = TRUE)
mode    <- args[1] %in% c('plan', 'write', 'refine', 'revert')
if (!isTRUE(mode)) stop('First argument must be "plan", "write", "refine" or "revert"')
mode    <- args[1]
out_dir <- args[2]
if (is.na(out_dir)) stop('Second argument: output folder')
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

CAMPAIGN_ID  <- 10L
BOUNDARY_SET <- '2026b'
OWNER        <- if (nzchar(Sys.getenv('BUILD_OWNER'))) Sys.getenv('BUILD_OWNER') else 'admin'   # app user who owns the drafts
XLSX         <- "data/manual_data/BRA_HA's and U5 Target Population.xlsx"
NAME_SIM_MIN <- 0.90   # Jaro-Winkler similarity (after dropping generic words) for a name match
PROX_M       <- 50     # different-name sites this close to an MHFL facility are linked to it
MAX_OUTSIDE_M <- 200   # sites further than this outside their district stop the run
CODE_FIX     <- c('BAN-WBD' = 'BAN-WDA')   # workbook district code -> boundary DIST_UID
MICROPLAN    <- 'data/manual_data/Complete Health Area Microplaning_Draft  November.xlsx'   # teams per health area
# Sites listed under one district's sheet that belong to another district
SITE_MOVES   <- data.frame(sheet = 'Wadajir', excel_name = 'Cumar Garare', to_code = 'BAN-KDA')

# ── App environment (same pieces global.R sets up) ──────────────────────────
dotenv::load_dot_env('.env')
if (nzchar(Sys.getenv('BUILD_DB_HOST'))) Sys.setenv(DB_HOST = Sys.getenv('BUILD_DB_HOST'))
if (nzchar(Sys.getenv('BUILD_DB_PORT'))) Sys.setenv(DB_PORT = Sys.getenv('BUILD_DB_PORT'))

.g <- parse('global.R')
.want <- c('boundary_sets_dir', 'friction_root_dir', 'boundary_lines_file', 'worldpop_t_u1_1to4_file',
           'n_start_dfas', 'starter_dfa_names', 'extra_dfa_names', 'all_dfa_names',
           '%||%', 'primary_lines_for_area', 'combine_line_layers')
for (e in .g) if (is.call(e) && identical(e[[1]], as.name('<-')) && is.name(e[[2]]) &&
                  as.character(e[[2]]) %in% .want) eval(e, globalenv())
source('helpers/app_helpers.R')
source('helpers/mod_db_v2.R')
source('helpers/subdivision_helpers.R')
source('helpers/printable_export.R')
source('tabs/facility/facility_helpers.R')
source('tabs/health_area/health_area_helpers.R')
source('tabs/health_area/mod_initial_health_area_generation.R')
boundary_lines_sf <- readRDS(boundary_lines_file)
districts_2026b   <- safe_make_valid(readRDS(file.path(boundary_sets_dir, BOUNDARY_SET, 'districts_shp.Rds')))
friction_dir      <- file.path(getwd(), friction_root_dir, BOUNDARY_SET, 'district_standardized')
u5_rast           <- load_worldpop_u5_raster(worldpop_t_u1_1to4_file)

# ── Workbook ────────────────────────────────────────────────────────────────
read_workbook <- function(path) {
  sites <- list(); pops <- list()
  for (sh in readxl::excel_sheets(path)) {
    d <- as.data.frame(suppressMessages(readxl::read_excel(path, sheet = sh, col_names = FALSE, col_types = 'text')))
    m <- trimws(as.matrix(d))
    hit <- which(m == 'Under Five Population Target', arr.ind = TRUE)
    if (nrow(hit) != 1) stop('Sheet ', sh, ': expected one "Under Five Population Target" cell')
    pops[[sh]] <- data.frame(sheet = sh, u5_target = as.numeric(d[hit[1, 1], hit[1, 2] + 1]))
    b <- d[-1, 1:5]; names(b) <- c('code', 'type', 'name', 'lat', 'lon')
    b$row <- seq_len(nrow(b)) + 1L
    b <- b[!is.na(b$name) & nzchar(trimws(b$name)), ]
    b$sheet <- sh
    sites[[sh]] <- b
  }
  s <- bind_rows(sites)
  s$code <- ifelse(s$code %in% names(CODE_FIX), CODE_FIX[s$code], s$code)
  s$lat  <- as.numeric(s$lat); s$lon <- as.numeric(s$lon)
  if (anyNA(s$lat) || anyNA(s$lon)) stop('Missing coordinates: ', paste(s$sheet[is.na(s$lat) | is.na(s$lon)], s$row[is.na(s$lat) | is.na(s$lon)], collapse = '; '))
  codes <- unique(s[, c('sheet', 'code')])
  if (any(duplicated(codes$sheet))) stop('A sheet has more than one district code')
  s$sheet_code <- s$code
  for (k in seq_len(nrow(SITE_MOVES))) {
    hit <- s$sheet == SITE_MOVES$sheet[k] & trimws(s$name) == SITE_MOVES$excel_name[k]
    if (sum(hit) != 1) stop('SITE_MOVES: "', SITE_MOVES$excel_name[k], '" not found once on sheet ', SITE_MOVES$sheet[k])
    s$code[hit] <- SITE_MOVES$to_code[k]
  }
  list(sites = s, pops = left_join(bind_rows(pops), codes, by = 'sheet'))
}

# Teams per health area from the microplanning workbook: its "Team" column
# (fractional), rounded to the nearest whole team, at least 1. Joined to the
# coordination-site rows by district code + coordinates, then by name.
read_microplan_teams <- function(path) {
  out <- list()
  for (sh in readxl::excel_sheets(path)) {
    d <- as.data.frame(suppressMessages(readxl::read_excel(path, sheet = sh, col_names = FALSE, col_types = 'text')))
    hdr <- trimws(as.character(d[1, ]))
    tcol <- which(hdr == 'Team')
    if (length(tcol) != 1 || !('Health Area Name' %in% hdr)) next
    b <- d[grepl('^BAN-', trimws(d[[1]])), , drop = FALSE]
    if (nrow(b) == 0) next
    out[[sh]] <- data.frame(mp_sheet = sh, code = CODE_FIX_or(trimws(b[[1]])), mp_name = trimws(b[[which(hdr == 'Health Area Name')]]),
                            mp_lat = as.numeric(b[[which(hdr == 'Latitude')]]), mp_lon = as.numeric(b[[which(hdr == 'Longitude')]]),
                            team_raw = as.numeric(b[[tcol]]))
  }
  t <- bind_rows(out)
  t$requested_teams <- pmax(1L, as.integer(round(t$team_raw)))
  t
}
CODE_FIX_or <- function(x) ifelse(x %in% names(CODE_FIX), CODE_FIX[x], x)

attach_teams <- function(sites, mp) {
  sites$requested_teams <- NA_integer_; sites$team_raw <- NA_real_; sites$mp_name <- NA_character_
  used <- logical(nrow(mp))
  for (i in seq_len(nrow(sites))) {
    cand <- which(mp$code == sites$sheet_code[i] & !used)
    j <- cand[abs(mp$mp_lat[cand] - sites$lat[i]) < 1e-6 & abs(mp$mp_lon[cand] - sites$lon[i]) < 1e-6]
    if (length(j) == 0) j <- cand[norm_name(mp$mp_name[cand]) == norm_name(sites$name[i])]
    if (length(j) == 0) next
    j <- j[1]; used[j] <- TRUE
    sites$requested_teams[i] <- mp$requested_teams[j]; sites$team_raw[i] <- mp$team_raw[j]; sites$mp_name[i] <- mp$mp_name[j]
  }
  attr(sites, 'unused_microplan') <- mp[!used, ]
  sites
}

# Light display-name tidy for non-facility sites (originals kept in the CSV)
tidy_name <- function(x) {
  x <- gsub('\\s+', ' ', trimws(x))
  x <- gsub('(?i)healrh', 'Health', x, perl = TRUE)
  x <- gsub('(?i)\\s*co-?ordination\\s*site', ' Coordination Site', x, perl = TRUE)
  x <- gsub('(?i)\\s*site\\s*coordination', ' Coordination Site', x, perl = TRUE)
  x <- gsub('(?i)\\bhospita\\b', 'Hospital', x, perl = TRUE)
  x <- gsub('(?i)\\bhc$', 'HC', x, perl = TRUE)
  trimws(x)
}

norm_name <- function(x) {
  y <- tolower(x); y <- gsub('[^a-z ]', ' ', y)
  y <- gsub('\\b(health|center|centre|hc|mch|hospital|hospita|coordination|co|ordination|site|sc|cs)\\b', ' ', y)
  y <- gsub('\\s+', ' ', trimws(y))
  ifelse(nzchar(y), y, gsub('\\s+', ' ', trimws(tolower(x))))
}
looks_like_facility <- function(x) grepl('(?i)health|hospital|\\bhc\\b|mch|clinic', x, perl = TRUE)

# ── MHFL candidates and matching for one district ───────────────────────────
match_district <- function(ds, dist_row, registry_alt) {
  dname <- dist_row$district_name
  fresh <- fetch_facilities_odk(dist_row$zone_name, dname, districts = districts_2026b)
  if (is.null(fresh) || nrow(fresh) == 0) fresh <- NULL
  fac <- if (is.null(fresh)) data.frame() else sf::st_drop_geometry(fresh)
  fac$alt <- registry_alt$facility_name_alt[match(fac$facility_id, registry_alt$facility_id)]

  ds$excel_name <- ds$name
  pts  <- sf::st_transform(sf::st_as_sf(ds, coords = c('lon', 'lat'), crs = 4326, remove = FALSE), 32638)
  pairs <- data.frame()
  if (nrow(fac) > 0) {
    fpts <- sf::st_transform(fresh, 32638)
    dmat <- matrix(as.numeric(sf::st_distance(pts, fpts)), nrow = nrow(pts))
    for (i in seq_len(nrow(ds))) for (j in seq_len(nrow(fac))) {
      sim <- max(stringsim(norm_name(ds$name[i]), norm_name(fac$facility_name[j]), 'jw'),
                 if (!is.na(fac$alt[j]) && nzchar(fac$alt[j])) stringsim(norm_name(ds$name[i]), norm_name(fac$alt[j]), 'jw') else 0)
      type <- if (sim >= NAME_SIM_MIN) 'name' else if (dmat[i, j] <= PROX_M) 'proximity' else NA
      if (!is.na(type)) pairs <- rbind(pairs, data.frame(i = i, j = j, type = type, sim = sim, d = dmat[i, j],
                                                         fac_like = looks_like_facility(ds$name[i])))
    }
  }
  ds$match_type <- 'none'; ds$mhfl_id <- NA; ds$mhfl_name <- NA; ds$sim <- NA; ds$dist_m <- NA; ds$note <- ''
  if (nrow(pairs) > 0) {
    pairs <- pairs[order(pairs$type != 'name', -round(pairs$sim, 2), !pairs$fac_like, pairs$d), ]
    used_i <- integer(0); used_j <- integer(0)
    for (k in seq_len(nrow(pairs))) {
      i <- pairs$i[k]; j <- pairs$j[k]
      if (i %in% used_i) next
      if (j %in% used_j) {
        if (pairs$type[k] == 'name' && ds$note[i] == '')
          ds$note[i] <- paste0('name matches MHFL "', fac$facility_name[j], '", already used by another site')
        next
      }
      used_i <- c(used_i, i); used_j <- c(used_j, j)
      ds$match_type[i] <- pairs$type[k]; ds$mhfl_id[i] <- fac$facility_id[j]; ds$mhfl_name[i] <- fac$facility_name[j]
      ds$sim[i] <- round(pairs$sim[k], 2); ds$dist_m[i] <- round(pairs$d[k])
      # A nearby facility whose name is only a spelling variant of the site's
      # (e.g. Issa/Isse Abdi, Shingani/Shangani) or contains it ("District
      # Hospital" -> "Hamar Jabjab District Hospital") is the same facility:
      # MHFL name alone, not "MHFL name (workbook name)".
      a <- norm_name(ds$name[i]); b <- norm_name(fac$facility_name[j])
      if (pairs$type[k] == 'proximity' && (pairs$sim[k] >= 0.80 || grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE)))
        ds$match_type[i] <- 'name'
    }
  }
  ds$final_name <- ifelse(ds$match_type == 'name', ds$mhfl_name,
                   ifelse(ds$match_type == 'proximity', paste0(ds$mhfl_name, ' (', tidy_name(ds$excel_name), ')'),
                          tidy_name(ds$excel_name)))
  ds$final_lat <- ds$lat; ds$final_lon <- ds$lon
  if (nrow(fac) > 0) {
    k <- ds$match_type != 'none'
    ds$final_lat[k] <- fac$lat[match(ds$mhfl_id[k], fac$facility_id)]
    ds$final_lon[k] <- fac$lon[match(ds$mhfl_id[k], fac$facility_id)]
  }

  # Non-facility sites outside the district: small overshoots are moved just
  # inside; anything further stops the run (fix the workbook coordinates).
  dpoly <- sf::st_transform(sf::st_union(sf::st_geometry(dist_row)), 32638)
  inner <- sf::st_buffer(dpoly, -20)
  for (i in which(ds$match_type == 'none')) {
    p <- sf::st_sfc(sf::st_point(c(ds$final_lon[i], ds$final_lat[i])), crs = 4326) |> sf::st_transform(32638)
    if (lengths(sf::st_within(p, dpoly)) > 0) next
    out_m <- as.numeric(sf::st_distance(p, dpoly))
    if (out_m > MAX_OUTSIDE_M) {
      ds$note[i] <- paste0('STOP: ', round(out_m), ' m outside ', dname, ' -- fix coordinates in the workbook')
      next
    }
    np <- sf::st_cast(sf::st_nearest_points(p, inner), 'POINT')[2] |> sf::st_transform(4326) |> sf::st_coordinates()
    ds$final_lon[i] <- np[1]; ds$final_lat[i] <- np[2]
    ds$note[i] <- paste0('moved ', round(out_m) + 20, ' m to just inside ', dname)
  }

  # Health area names must be unique in the district and not reserved
  key <- tolower(gsub('\\s+', ' ', ds$final_name))
  dup <- key %in% key[duplicated(key)] | key %in% tolower(extra_dfa_names)
  if (any(dup)) ds$note[dup] <- paste0(trimws(paste(ds$note[dup], ';')), ' STOP: duplicate or reserved health area name')
  list(sites = ds, fresh = fresh)
}

# ── odk_sf / app_sf exactly as the Facilities tab stores them ───────────────
build_facility_layers <- function(ds, fresh, dist_row) {
  odk_sf <- NULL
  if (!is.null(fresh)) {
    odk_sf <- fresh
    m <- match(odk_sf$facility_id, ds$mhfl_id)
    odk_sf$polio_sia_coordination_site <- ifelse(is.na(m), 'No', 'Yes')
    prox <- !is.na(m) & ds$match_type[m] == 'proximity'
    odk_sf$facility_name[prox] <- ds$final_name[m[prox]]
  }
  nf <- ds[ds$match_type == 'none', ]
  app_sf <- NULL
  if (nrow(nf) > 0) {
    stamp <- format(Sys.time(), '%Y%m%d%H%M%S')
    app_df <- data.frame(
      facility_id = sprintf('app_%s_%04d', stamp, seq_len(nrow(nf))),
      facility_name = nf$final_name, facility_type = NA_character_, hf_ownership = NA_character_,
      region = dist_row$region_name, district = dist_row$district_name, incharge_name = NA_character_,
      lat = nf$final_lat, lon = nf$final_lon, polio_sia_coordination_site = 'Yes',
      odk_edit_link = NA_character_, stringsAsFactors = FALSE)
    app_sf <- sf::st_as_sf(app_df, coords = c('lon', 'lat'), crs = 4326, remove = FALSE)
    ds$app_id <- NA; ds$app_id[ds$match_type == 'none'] <- app_df$facility_id
  }
  list(odk_sf = odk_sf, app_sf = app_sf, sites = ds)
}

# ── Health areas with the app's generation module ───────────────────────────
generate_areas <- function(dist_row, odk_sf, app_sf, subdiv_url) {
  dname <- dist_row$district_name
  dsf <- districts_2026b |>
    dplyr::filter(zone_name == dist_row$zone_name, region_name == dist_row$region_name, district_name == dname) |>
    dplyr::select(admin_id, district_name, region_id, region_name, zone_id, zone_name, geometry) |>
    dplyr::summarise(admin_id = dplyr::first(admin_id), district_name = dplyr::first(district_name),
                     region_id = dplyr::first(region_id), region_name = dplyr::first(region_name),
                     zone_id = dplyr::first(zone_id), zone_name = dplyr::first(zone_name),
                     geometry = sf::st_union(geometry), .groups = 'drop') |>
    sf::st_as_sf() |> safe_make_valid()
  dsf <- tryCatch(sf::st_collection_extract(dsf, 'POLYGON'), error = function(e) dsf)
  max_dim <- calc_district_max_dim(dsf)
  grid_n  <- max(10L, as.integer(round(max_dim / max(100, min(500, max_dim / 160)))))

  full_4326 <- dsf |> dplyr::select(district_name) |> sf::st_transform(4326)
  subs  <- tryCatch(fetch_subdivisions_for_district(full_4326, url = subdiv_url), error = function(e) NULL)
  lines <- combine_line_layers(subdivisions_to_boundary_lines(subs), primary_lines_for_area(dsf))

  fac_df <- dplyr::bind_rows(facility_sf_to_df(odk_sf), facility_sf_to_df(app_sf))
  seeds  <- fac_df[fac_df$polio_sia_coordination_site == 'Yes', , drop = FALSE]
  seeds_sf <- sf::st_as_sf(seeds, coords = c('lon', 'lat'), crs = 4326, remove = FALSE)

  sc <- NULL
  shiny::testServer(initialHealthAreaGenerationServer, args = list(
    district_sf = reactive(sf::st_transform(dsf, 4326)), friction_district_sf = reactive(dsf),
    friction_dir = reactive(friction_dir), grid_n = reactive(grid_n), n_dfa = n_start_dfas,
    seed = reactive(sum(utf8ToInt(dname))), facility_seed_sf = reactive(seeds_sf),
    facility_name_col = 'facility_name', subdivision_lines_sf = reactive(lines),
    subdivision_boundary_penalty = 0.99, u5_rast = u5_rast
  ), { sc <<- session$returned$scene() })

  dfa_names <- c(setdiff(unique(as.character(sc$seed_points_df$dfa_name)), extra_dfa_names), extra_dfa_names)
  saved <- build_saved_dfa_sf(grid_sf = sc$grid_sf, assignments = sc$initial_assignments, district_sf = sc$district_sf)
  wp <- setNames(lapply(setdiff(unique(as.character(saved$dfa_name)), extra_dfa_names), function(nm)
    sum(.polygon_u5_population(saved[saved$dfa_name == nm, , drop = FALSE], u5_rast), na.rm = TRUE)),
    setdiff(unique(as.character(saved$dfa_name)), extra_dfa_names))
  list(saved_dfa_sf = saved, dfa_names = dfa_names, current_assignments = sc$initial_assignments,
       dfa_worldpop_pop = wp, district_wp = sum(.polygon_u5_population(sf::st_transform(dsf, 4326), u5_rast), na.rm = TRUE),
       n_cells = nrow(sc$grid_sf), grid_n = grid_n, district_sf = sf::st_transform(dsf, 4326), seeds = sc$seed_points_df)
}

# ── DB ──────────────────────────────────────────────────────────────────────
pool <- db_connect()
on.exit(pool::poolClose(pool), add = TRUE)
camp <- DBI::dbGetQuery(pool, 'SELECT campaign_name, boundary_set FROM campaigns WHERE campaign_id = $1', list(CAMPAIGN_ID))
if (nrow(camp) != 1 || camp$boundary_set != BOUNDARY_SET) stop('Campaign ', CAMPAIGN_ID, ' not found or not on ', BOUNDARY_SET)
cat('Campaign:', camp$campaign_name, '\n')

if (mode == 'plan') {
  sourceCpp('bfs_propagate.cpp')
  wb <- read_workbook(XLSX)
  wb$sites <- attach_teams(wb$sites, read_microplan_teams(MICROPLAN))
  unused_mp <- attr(wb$sites, 'unused_microplan')
  registry <- .get_registry()
  registry_alt <- data.frame(facility_id = registry$facility_id, facility_name_alt = registry$facility_name_alt)
  subdiv_url <- tryCatch(db_get_data_source_url(pool, 'subdivisions_url', CAMPAIGN_ID), error = function(e) NULL)
  if (is.null(subdiv_url) || !nzchar(subdiv_url)) subdiv_url <- ARCGIS_SUBDIVISIONS_URL

  all_sites <- list(); summary <- list()
  pdf(file.path(out_dir, 'preview_maps.pdf'), width = 11, height = 8.5)
  for (k in seq_len(nrow(wb$pops))) {
    pr <- wb$pops[k, ]
    dist_row <- districts_2026b[districts_2026b$DIST_UID == pr$code, ]
    if (nrow(dist_row) != 1) stop('No 2026b district for code ', pr$code)
    dname <- dist_row$district_name
    cat('\n==== ', dname, ' (sheet ', pr$sheet, ')\n', sep = '')
    mres <- match_district(wb$sites[wb$sites$code == pr$code, ], dist_row, registry_alt)
    fl   <- build_facility_layers(mres$sites, mres$fresh, dist_row)
    ds   <- fl$sites; ds$district <- dname
    stops <- grepl('STOP', ds$note)
    gen <- NULL
    if (!any(stops)) gen <- generate_areas(dist_row, fl$odk_sf, fl$app_sf, subdiv_url)
    cur <- db_get_shared_version(pool, CAMPAIGN_ID, dname)
    # Field Requested Teams per health area (Field Target U5 left empty)
    team_targets <- setNames(lapply(ds$requested_teams, function(n)
      list(target_pop = NA_real_, requested_teams = if (is.na(n)) NA_integer_ else as.integer(n))), ds$final_name)
    if (!is.null(gen)) {
      missing_ha <- setdiff(ds$final_name, gen$saved_dfa_sf$dfa_name)
      if (length(missing_ha) > 0) ds$note[ds$final_name %in% missing_ha] <- 'STOP: no health area generated for this site'
      saveRDS(list(district_name = dname, sheet = pr$sheet, u5_target = pr$u5_target,
                   odk_sf = fl$odk_sf, app_sf = fl$app_sf, gen = gen, sites = ds, team_targets = team_targets,
                   source_version_id = cur$version_id %||% NA),
              file.path(out_dir, paste0(gsub('[^A-Za-z0-9]+', '_', dname), '.rds')))
      ha <- gen$saved_dfa_sf
      wpv <- unlist(gen$dfa_worldpop_pop)
      plot(sf::st_geometry(gen$district_sf), border = 'black', lwd = 2,
           main = sprintf('%s  -- %d health areas | In-Scope Field U5 %s | WorldPop U5 %s',
                          dname, sum(!ha$dfa_name %in% extra_dfa_names), format(pr$u5_target, big.mark = ','),
                          format(round(gen$district_wp), big.mark = ',')))
      cols <- grDevices::hcl.colors(nrow(ha), 'Set 3')
      plot(sf::st_geometry(ha), col = cols, border = 'grey30', add = TRUE)
      plot(sf::st_geometry(gen$district_sf), border = 'black', lwd = 2, add = TRUE)
      sp <- sf::st_as_sf(ds, coords = c('final_lon', 'final_lat'), crs = 4326)
      plot(sf::st_geometry(sp), pch = ifelse(ds$match_type == 'none', 17, 19), cex = 0.9, add = TRUE)
      lab <- sf::st_coordinates(sp)
      text(lab[, 1], lab[, 2], labels = ds$final_name, pos = 3, cex = 0.5)
      legend('bottomleft', bty = 'n', cex = 0.7, pch = c(19, 17), legend = c('MHFL facility', 'non-facility site'))
    }
    all_sites[[dname]] <- ds
    summary[[dname]] <- data.frame(
      district = dname, sheet = pr$sheet, u5_target = pr$u5_target,
      worldpop_u5 = if (is.null(gen)) NA else round(gen$district_wp),
      n_sites = nrow(ds), n_mhfl_name = sum(ds$match_type == 'name'), n_mhfl_proximity = sum(ds$match_type == 'proximity'),
      n_non_facility = sum(ds$match_type == 'none'),
      n_health_areas = if (is.null(gen)) NA else sum(!gen$saved_dfa_sf$dfa_name %in% extra_dfa_names),
      teams_planned = sum(ds$requested_teams, na.rm = TRUE), sites_without_teams = sum(is.na(ds$requested_teams)),
      grid_cells = if (is.null(gen)) NA else gen$n_cells,
      current_version_id = cur$version_id %||% NA, current_n_health_areas = length(setdiff(cur$snap$dfa_names, extra_dfa_names)),
      status = if (any(grepl('STOP', ds$note))) 'STOPPED: see site_matches.csv notes' else 'ready')
  }
  dev.off()
  sites_out <- bind_rows(all_sites) |>
    dplyr::transmute(district, sheet, row, excel_name, excel_lat = lat, excel_lon = lon, match_type,
                     mhfl_id, mhfl_name, name_similarity = sim, excel_to_mhfl_m = dist_m,
                     health_area_name = final_name, site_lat = round(final_lat, 6), site_lon = round(final_lon, 6),
                     microplan_name = mp_name, microplan_teams = round(team_raw, 2), requested_teams, note)
  write.csv(sites_out, file.path(out_dir, 'site_matches.csv'), row.names = FALSE, na = '')
  if (nrow(unused_mp) > 0) {
    cat('\nMicroplanning rows not matched to any coordination site:\n'); print(unused_mp[, c('code', 'mp_name', 'team_raw')])
    write.csv(unused_mp, file.path(out_dir, 'microplan_unmatched.csv'), row.names = FALSE)
  }
  write.csv(bind_rows(summary), file.path(out_dir, 'district_summary.csv'), row.names = FALSE, na = '')
  print(bind_rows(summary)[, c('district', 'n_sites', 'n_mhfl_name', 'n_mhfl_proximity', 'n_non_facility', 'n_health_areas', 'status')])
  cat('\nPlan written to', out_dir, '\n')
}

if (mode == 'write') {
  if (!(OWNER %in% db_get_users(pool)$username)) stop('Owner "', OWNER, '" is not an app user')
  cat('Drafts owned by:', OWNER, '\n')
  files <- list.files(out_dir, pattern = '\\.rds$', full.names = TRUE)
  if (length(files) != 20) stop('Expected 20 district plans in ', out_dir, ', found ', length(files))
  log <- list()
  for (f in files) {
    p <- readRDS(f); dn <- p$district_name
    if (any(grepl('STOP', p$sites$note))) stop(dn, ': plan has STOP notes -- fix and re-run plan')
    cur <- db_get_shared_version(pool, CAMPAIGN_ID, dn)
    if (!identical(cur$version_id %||% NA, p$source_version_id))
      stop(dn, ': current published version changed since the plan was made -- re-run plan')
    vid <- db_create_blank_version(pool, OWNER, CAMPAIGN_ID, dn)
    if (is.null(vid)) stop(dn, ': could not create version')
    if (!is.null(cur$snap$landmarks) && nrow(cur$snap$landmarks) > 0)
      db_submit_stage_v2(pool, vid, OWNER, 'landmarks', list(landmarks = cur$snap$landmarks))
    db_submit_stage_v2(pool, vid, OWNER, 'facilities', list(odk_sf = p$odk_sf, app_sf = p$app_sf))
    if (!is.null(cur$snap$idp_settlements) && nrow(cur$snap$idp_settlements) > 0)
      db_submit_stage_v2(pool, vid, OWNER, 'idp', list(idp_settlements = cur$snap$idp_settlements))
    db_submit_stage_v2(pool, vid, OWNER, 'areas', list(
      saved_dfa_sf = p$gen$saved_dfa_sf, dfa_names = p$gen$dfa_names,
      current_assignments = p$gen$current_assignments, smoothed_dfa_sf = p$gen$saved_dfa_sf,
      team_targets = p$team_targets, dfa_worldpop_pop = p$gen$dfa_worldpop_pop))
    db_set_scope_population(pool, vid, p$u5_target, p$gen$district_wp)
    cat(dn, '-> draft version', vid, '\n')
    log[[dn]] <- data.frame(district = dn, new_version_id = vid, source_version_id = p$source_version_id,
                            u5_target = p$u5_target, at = format(Sys.time()))
  }
  scopes <- db_get_campaign_districts(pool, CAMPAIGN_ID)
  if (any(scopes$district_name == 'Waberi' & scopes$mapping_scope != 'full')) {
    db_set_district_mapping_scope(pool, CAMPAIGN_ID, 'Waberi', 'full')
    cat('Waberi mapping scope set to full\n')
  }
  write.csv(bind_rows(log), file.path(out_dir, 'write_log.csv'), row.names = FALSE)
  cat('\nDone. Log:', file.path(out_dir, 'write_log.csv'), '\n')
}

# ── Boundary refinement (the browser's Submit step, run headless) ───────────
# The geometry functions of www/paint-app.js (everything before getApp()),
# driven through the same steps as enterVertexMode() -> saveVertexBoundaryEdits()
# -> emitVertexGeojson(), at the app's default smoothness/stiffness/snap tolerance.
REFINE_SMOOTHNESS <- 2; REFINE_STIFFNESS <- 6; REFINE_SNAP_TOL <- 20
make_refiner <- function() {
  js <- readLines('www/paint-app.js', warn = FALSE)
  a <- grep('^function traceRealGridBoundaries', js); b <- grep('^function getApp', js) - 1
  if (length(a) != 1 || length(b) != 1) stop('paint-app.js layout changed -- update make_refiner()')
  ctx <- V8::v8()
  ctx$eval(paste(js[a:b], collapse = '\n'))
  ctx$eval("
  function refine(gridGeo, assignments, districtGeo, smoothness, stiffness, snapTol) {
    const cells = {}, cellCorners = {}, asg = {};
    gridGeo.features.forEach(f => {
      const p = f.properties, cid = String(p.cell_id);
      cells[cid] = { row: p.row, col: p.col };
      cellCorners[cid] = f.geometry.coordinates[0].slice(0, 4).map(([lng, lat]) => [lat, lng]);
      asg[cid] = assignments[cid];
    });
    const sample = cells[Object.keys(cells)[0]];
    const raw = traceRealGridBoundaries(cells, cellCorners, asg);
    const simp = simplifyVertexBoundary(raw.rawIdRingsByArea, raw.vertexPos, smoothness);
    const vertexPos = {};
    Object.keys(raw.vertexPos).forEach(k => { vertexPos[k] = { ...raw.vertexPos[k] }; });
    snapOuterBoundaryToTruth(vertexPos, simp.outerVertexIds, districtGeo);
    const constants = computeVertexConstants(raw.vertexPos, sample.row, sample.col, stiffness, snapTol);
    const engine = buildVertexEngine(vertexPos, simp.areaRings, simp.outerVertexIds, constants, () => {}, () => {});
    engine.saveVertexEdits();
    const st = engine.get_state();
    return JSON.stringify(buildAreaGeojson(st.areaRings, st.vertexPos, 'dfa_name'));
  }")
  ctx
}

refine_boundaries <- function(ctx, grid_sf, assignments, district_sf) {
  out <- ctx$call('refine',
                  jsonlite::fromJSON(as_geojson_text(grid_sf), simplifyVector = FALSE),
                  setNames(as.list(assignments), as.character(grid_sf$cell_id)),
                  jsonlite::fromJSON(as_geojson_text(district_sf), simplifyVector = FALSE),
                  REFINE_SMOOTHNESS, REFINE_STIFFNESS, REFINE_SNAP_TOL, auto_unbox = TRUE)
  ref <- geojsonsf::geojson_sf(out); sf::st_crs(ref) <- 4326
  # Planar repair (e.g. a duplicated vertex left by the vertex engine), one
  # MULTIPOLYGON per health area, same shape as the app's saved boundaries.
  old_s2 <- sf::sf_use_s2(FALSE); on.exit(suppressMessages(sf::sf_use_s2(old_s2)))
  ref <- suppressWarnings(sf::st_make_valid(ref))
  ref <- tryCatch(sf::st_collection_extract(ref, 'POLYGON'), error = function(e) ref)
  ref <- ref |> dplyr::group_by(dfa_name) |> dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
    sf::st_as_sf()
  # Fit to the district exactly. Simplifying the outer edge cuts corners in
  # places and overshoots in others, leaving thin slivers (tens of metres)
  # along the district boundary. Clip to the district, then give each
  # uncovered sliver to the health area it shares the longest border with
  # (nearest area if it touches none).
  # 1 cm precision grid for every overlay below: without it, near-coincident
  # vertices from the simplification make GEOS throw or return invalid output.
  fix <- function(x) {
    x <- suppressWarnings(sf::st_make_valid(sf::st_set_precision(x, 100)))
    tryCatch(sf::st_collection_extract(x, 'POLYGON'), error = function(e) x)
  }
  by_name <- function(x) x |> dplyr::group_by(dfa_name) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |> sf::st_as_sf() |> fix()
  d_m   <- sf::st_union(fix(sf::st_transform(safe_make_valid(district_sf), 32638)))
  d_m   <- sf::st_set_precision(d_m, 100)
  ref_m <- fix(sf::st_transform(ref, 32638))
  ref_m <- by_name(fix(suppressWarnings(sf::st_intersection(ref_m, d_m))))
  gaps  <- sf::st_difference(d_m, sf::st_set_precision(sf::st_union(ref_m), 100))
  gaps  <- if (length(gaps) == 0 || all(sf::st_is_empty(gaps))) NULL else
    suppressWarnings(sf::st_cast(sf::st_collection_extract(gaps, 'POLYGON'), 'POLYGON'))
  if (!is.null(gaps) && length(gaps) > 0) {
    gaps <- gaps[as.numeric(sf::st_area(gaps)) > 0]
    owner <- vapply(seq_along(gaps), function(i) {
      shared <- vapply(seq_len(nrow(ref_m)), function(j) {
        b <- suppressWarnings(sf::st_intersection(sf::st_boundary(gaps[i]), sf::st_boundary(sf::st_geometry(ref_m)[j])))
        if (length(b) == 0 || all(sf::st_is_empty(b))) 0 else sum(as.numeric(sf::st_length(b)))
      }, numeric(1))
      if (max(shared) > 0) ref_m$dfa_name[which.max(shared)] else ref_m$dfa_name[sf::st_nearest_feature(gaps[i], ref_m)]
    }, character(1))
    ref_m <- by_name(dplyr::bind_rows(ref_m, sf::st_sf(dfa_name = owner, geometry = gaps)))
  }
  # Back to lon/lat, then repair once more there (reprojection can leave a
  # self-touching ring), one MULTIPOLYGON per health area.
  out <- sf::st_transform(ref_m, 4326)
  out <- suppressWarnings(sf::st_make_valid(out))
  out <- tryCatch(sf::st_collection_extract(out, 'POLYGON'), error = function(e) out)
  out |> dplyr::group_by(dfa_name) |> dplyr::summarise(geometry = sf::st_union(geometry), .groups = 'drop') |>
    sf::st_as_sf() |> sf::st_cast('MULTIPOLYGON', warn = FALSE)
}

if (mode == 'refine') {
  log_path <- file.path(out_dir, 'write_log.csv')
  if (!file.exists(log_path)) stop('No write_log.csv in ', out_dir)
  wl <- read.csv(log_path)
  # BUILD_ONLY="Hodan,Yaqshid" limits the run; BUILD_DRY_RUN=1 skips DB writes
  only <- trimws(strsplit(Sys.getenv('BUILD_ONLY'), ',')[[1]])
  if (length(only) > 0) wl <- wl[wl$district %in% only, , drop = FALSE]
  dry_run <- identical(Sys.getenv('BUILD_DRY_RUN'), '1')
  if (dry_run) cat('DRY RUN -- nothing is written\n')
  ctx <- make_refiner()
  suppressMessages(sf::sf_use_s2(FALSE))
  rlog <- list()
  pdf(file.path(out_dir, 'refine_compare.pdf'), width = 14, height = 7)
  for (k in seq_len(nrow(wl))) {
    vid <- wl$new_version_id[k]
    v <- db_get_version_by_id(pool, vid)
    if (is.null(v)) stop('Version ', vid, ' not found')
    dn <- v$district_name
    if (isTRUE(v$is_shared) || !is.null(v$archived_at) && !is.na(v$archived_at)) {
      cat(dn, ': version', vid, 'is published or archived -- skipped\n'); next
    }
    dist_row <- districts_2026b[districts_2026b$district_name == dn, ]
    dsf <- dist_row |> dplyr::select(district_name) |> dplyr::summarise(geometry = sf::st_union(geometry)) |>
      safe_make_valid() |> sf::st_transform(4326)
    dsf <- tryCatch(sf::st_collection_extract(dsf, 'POLYGON'), error = function(e) dsf)
    max_dim <- calc_district_max_dim(dsf)
    grid_n  <- max(10L, as.integer(round(max_dim / max(100, min(500, max_dim / 160)))))
    grid    <- make_paint_grid(dsf, grid_n = grid_n)$grid_sf
    asg     <- v$snap$current_assignments
    if (length(asg) != nrow(grid)) stop(dn, ': stored assignments (', length(asg), ') do not fit the rebuilt grid (', nrow(grid), ')')

    t0  <- Sys.time()
    ref <- refine_boundaries(ctx, grid, asg, dsf)
    secs <- round(as.numeric(difftime(Sys.time(), t0, units = 'secs')), 1)

    # Checks: same health areas, valid, no overlaps, district still covered
    old <- v$snap$saved_dfa_sf
    ref_m <- sf::st_transform(ref, 32638); d_m <- sf::st_union(sf::st_transform(dsf, 32638))
    inter <- suppressWarnings(sf::st_intersection(ref_m))
    overlap_km2 <- sum(as.numeric(sf::st_area(inter[lengths(inter$origins) > 1, ]))) / 1e6
    gap_km2     <- sum(as.numeric(sf::st_area(sf::st_difference(d_m, sf::st_union(ref_m))))) / 1e6
    dist_km2    <- as.numeric(sf::st_area(d_m)) / 1e6
    problems <- c(
      if (!setequal(ref$dfa_name, old$dfa_name)) paste('health areas changed:', paste(setdiff(union(ref$dfa_name, old$dfa_name), intersect(ref$dfa_name, old$dfa_name)), collapse = ', ')),
      if (!all(sf::st_is_valid(ref))) 'invalid geometry',
      if (overlap_km2 > 0.001) sprintf('overlap %.4f km2', overlap_km2),
      if (gap_km2 / dist_km2 > 0.001) sprintf('gap %.2f%% of district', 100 * gap_km2 / dist_km2))

    nms <- setdiff(unique(as.character(ref$dfa_name)), extra_dfa_names)
    wp  <- setNames(lapply(nms, function(nm) sum(.polygon_u5_population(ref[ref$dfa_name == nm, , drop = FALSE], u5_rast), na.rm = TRUE)), nms)

    cols <- grDevices::hcl.colors(nrow(old), 'Set 3')
    par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
    plot(sf::st_geometry(old), col = cols, border = 'grey25', main = paste(dn, '- grid outlines (before)'))
    plot(sf::st_geometry(ref[match(old$dfa_name, ref$dfa_name), ]), col = cols, border = 'grey25',
         main = paste(dn, '- refined', if (length(problems)) '(NOT SAVED)' else ''))

    status <- if (dry_run) 'ok (dry run)' else 'saved'
    if (length(problems) > 0) {
      status <- paste('NOT SAVED:', paste(problems, collapse = '; '))
    } else if (!dry_run) {
      db_submit_stage_v2(pool, vid, v$owner_username, 'areas', list(
        saved_dfa_sf = ref, dfa_names = v$snap$dfa_names, current_assignments = asg,
        smoothed_dfa_sf = ref, team_targets = v$snap$team_targets, dfa_worldpop_pop = wp))
    }
    cat(sprintf('%-13s v%d  %5d cells  %5.1f s  %s\n', dn, vid, nrow(grid), secs, status))
    rlog[[dn]] <- data.frame(district = dn, version_id = vid, grid_cells = nrow(grid), seconds = secs,
                             n_health_areas = length(nms), overlap_km2 = round(overlap_km2, 5),
                             gap_pct = round(100 * gap_km2 / dist_km2, 3), status = status, at = format(Sys.time()))
  }
  dev.off()
  write.csv(bind_rows(rlog), file.path(out_dir, 'refine_log.csv'), row.names = FALSE)
  cat('\nRefine log:', file.path(out_dir, 'refine_log.csv'), '\n')
}

# Revert: put the grid-outline boundaries from the plan back on the drafts
# (undoes 'refine'). Skips any draft whose assignments changed or that was
# saved after the refine run, so app edits are never overwritten.
if (mode == 'revert') {
  wl <- read.csv(file.path(out_dir, 'write_log.csv'))
  rl_path <- file.path(out_dir, 'refine_log.csv')
  rl <- if (file.exists(rl_path)) read.csv(rl_path) else NULL
  only <- trimws(strsplit(Sys.getenv('BUILD_ONLY'), ',')[[1]])
  if (length(only) > 0) wl <- wl[wl$district %in% only, , drop = FALSE]
  dry_run <- identical(Sys.getenv('BUILD_DRY_RUN'), '1')
  if (dry_run) cat('DRY RUN -- nothing is written\n')
  for (k in seq_len(nrow(wl))) {
    vid <- wl$new_version_id[k]; dn <- wl$district[k]
    v <- db_get_version_by_id(pool, vid)
    p <- readRDS(file.path(out_dir, paste0(gsub('[^A-Za-z0-9]+', '_', dn), '.rds')))
    why <- c(
      if (isTRUE(v$is_shared)) 'published',
      if (!is.null(v$archived_at) && !is.na(v$archived_at)) 'archived',
      if (!identical(as.character(v$snap$current_assignments), as.character(p$gen$current_assignments))) 'painted since plan',
      if (!is.null(rl) && dn %in% rl$district &&
          as.numeric(difftime(as.POSIXct(v$last_updated_at),
                              as.POSIXct(rl$at[rl$district == dn][1]), units = 'mins')) > 2) 'saved in app after refine')
    # BUILD_FORCE=1: app edits are overwritten too (published/archived still skipped);
    # facilities, areas and scope population all go back to the plan as written.
    force  <- identical(Sys.getenv('BUILD_FORCE'), '1')
    locked <- intersect(why, c('published', 'archived'))
    if (length(locked) || (length(why) && !force)) {
      cat(sprintf('%-13s v%d  SKIPPED: %s\n', dn, vid, paste(why, collapse = '; '))); next
    }
    if (!dry_run) {
      if (force) db_submit_stage_v2(pool, vid, v$owner_username, 'facilities', list(odk_sf = p$odk_sf, app_sf = p$app_sf))
      db_submit_stage_v2(pool, vid, v$owner_username, 'areas', list(
        saved_dfa_sf = p$gen$saved_dfa_sf, dfa_names = p$gen$dfa_names,
        current_assignments = p$gen$current_assignments, smoothed_dfa_sf = p$gen$saved_dfa_sf,
        team_targets = p$team_targets, dfa_worldpop_pop = p$gen$dfa_worldpop_pop))
      if (force) db_set_scope_population(pool, vid, p$u5_target, p$gen$district_wp)
    }
    cat(sprintf('%-13s v%d  %s (last saved %s%s)\n', dn, vid, if (dry_run) 'ok (dry run)' else 'reverted',
                format(v$last_updated_at), if (length(why)) paste0('; overwrote: ', paste(why, collapse = '; ')) else ''))
  }
}
