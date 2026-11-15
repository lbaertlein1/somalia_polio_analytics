# Two-level health-area generation for big rural districts with towns (same idea as Kismayo's detailed town draft):
#   1. coarse pass: the app's generator on the whole district at the app's cell size, all sites
#   2. fine pass: the same generator inside each town's urban footprint (URBAN_M around its sites) on a fine grid, with the real town sites;
#      outside the footprints the coarse areas stay; a town site's coarse land cut off from its town area goes to the neighbour it borders most
#   3. both smoothed with the app's Submit step (paint-app.js), merged, tidied (clean_lib), cells re-derived on the app's paint grid
# Writes the health areas into an existing UNPUBLISHED draft (the app's 'areas' stage). Never publishes.
# Env: V (version id), APPLY=1 to write, OUT (folder for maps/rds), TOWN_KM (site spacing that makes a town, default 1.5),
#      TOWN_MIN (sites per town, default 3), URBAN_M (urban footprint around town sites, default 1000), FINE_MAX_CELLS (per town, default 120000), FINE_MIN_M (default 20).
# NOTE: the app's paint grid stays at the district's normal cell size; repainting in the app re-traces the town on that coarse grid.
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 220, warn = -1); source('scripts/cleanup/clean_lib.R')
suppressMessages({ library(shiny); library(dplyr) })
.g <- parse('scripts/build_banadir_r1_versions.R')
for (e in .g) if (is.call(e) && identical(e[[1]], as.name('<-')) && is.name(e[[2]]) &&
  as.character(e[[2]]) %in% c('make_refiner', 'refine_boundaries', 'REFINE_SMOOTHNESS', 'REFINE_STIFFNESS', 'REFINE_SNAP_TOL')) eval(e, globalenv())
V <- as.integer(Sys.getenv('V')); OUT <- Sys.getenv('OUT', tempdir()); dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
TOWN_KM <- as.numeric(Sys.getenv('TOWN_KM', '1.5')); TOWN_MIN <- as.integer(Sys.getenv('TOWN_MIN', '3')); URBAN_M <- as.numeric(Sys.getenv('URBAN_M', '1000'))
FINE_MAX <- as.numeric(Sys.getenv('FINE_MAX_CELLS', '120000')); FINE_MIN <- as.numeric(Sys.getenv('FINE_MIN_M', '20'))
v <- db_get_version_by_id(pool, V); stopifnot(!is.null(v), !isTRUE(v$is_shared)); D <- v$district_name; cid <- v$campaign_id
cat('==', D, '| version', V, '| owner', v$owner_username, '\n')
districts <- safe_make_valid(districts_shp_for_campaign(cid)); friction_dir <- friction_dir_for_campaign(cid)
subdiv_url <- tryCatch(db_get_data_source_url(pool, 'subdivisions_url', cid), error = function(e) NULL)
dsf <- districts[districts$district_name == D, ] |> dplyr::summarise(admin_id = dplyr::first(admin_id), district_name = dplyr::first(district_name),
  region_id = dplyr::first(region_id), region_name = dplyr::first(region_name), zone_id = dplyr::first(zone_id), zone_name = dplyr::first(zone_name),
  geometry = sf::st_union(geometry), .groups = 'drop') |> sf::st_as_sf() |> safe_make_valid()
dsf <- tryCatch(sf::st_collection_extract(dsf, 'POLYGON'), error = function(e) dsf); d4 <- sf::st_transform(dsf, 4326)
sub_cache <- file.path(OUT, paste0(gsub('[^A-Za-z0-9]+', '_', D), '_subdivisions.rds'))
subs <- if (file.exists(sub_cache)) readRDS(sub_cache) else { x <- tryCatch(fetch_subdivisions_for_district(dsf |> dplyr::select(district_name) |> sf::st_transform(4326), url = subdiv_url), error = function(e) NULL); saveRDS(x, sub_cache); x }
lines <- combine_line_layers(subdivisions_to_boundary_lines(subs), primary_lines_for_area(dsf))
app_cell <- function(area_sf) { md <- calc_district_max_dim(area_sf); max(10L, as.integer(round(md / max(100, min(500, md / 160))))) }

# sites
fac <- dplyr::bind_rows(facility_sf_to_df(v$snap$odk_sf), facility_sf_to_df(v$snap$app_sf)); fac <- fac[fac$polio_sia_coordination_site %in% 'Yes', , drop = FALSE]
sites <- sf::st_as_sf(fac, coords = c('lon', 'lat'), crs = 4326, remove = FALSE); stopifnot(!anyDuplicated(sites$facility_name))
crs <- utm_for(d4); su <- sf::st_transform(sites, crs)
# towns: groups of >= TOWN_MIN sites chained within TOWN_KM of each other
comp <- igraph::components(igraph::graph_from_adjacency_matrix(matrix(as.numeric(sf::st_distance(su)), nrow(su)) <= TOWN_KM * 1000, mode = 'undirected'))$membership
tsz <- table(comp); towns <- as.integer(names(tsz)[tsz >= TOWN_MIN]); sites$town <- ifelse(comp %in% towns, match(comp, towns), NA_integer_)
cat('towns:', length(towns), '|', paste(sprintf('town %d: %d sites', seq_along(towns), as.integer(tsz[as.character(towns)])), collapse = '; '), '| rural sites', sum(is.na(sites$town)), '\n')

# one generator run (the app's module, as in the Health Areas tab); area_4326 = canvas, seeds_sf = named seeds
gen_run <- function(area_4326, seeds_sf, grid_n, seed_int) { sc <- NULL
  shiny::testServer(initialHealthAreaGenerationServer, args = list(
    district_sf = reactive(area_4326), friction_district_sf = reactive(dsf), friction_dir = reactive(friction_dir), grid_n = reactive(grid_n), n_dfa = n_start_dfas,
    seed = reactive(seed_int), facility_seed_sf = reactive(seeds_sf), facility_name_col = 'facility_name', subdivision_lines_sf = reactive(lines),
    subdivision_boundary_penalty = 0.99, u5_rast = u5_rast), { sc <<- session$returned$scene() })
  stopifnot(setequal(unique(as.character(sc$initial_assignments)), seeds_sf$facility_name) || all(seeds_sf$facility_name %in% sc$initial_assignments))
  list(grid = sc$grid_sf, asg = sc$initial_assignments) }
ctx <- make_refiner(); sf::sf_use_s2(FALSE)
smooth <- function(run, canvas_4326) { ref <- refine_boundaries(ctx, run$grid, run$asg, canvas_4326); ref }

# 1. coarse pass: all sites at the app's cell size
gn_c <- app_cell(dsf); t0 <- Sys.time(); cr <- gen_run(d4, sites, gn_c, sum(utf8ToInt(D)))
cat(sprintf('coarse pass: %d cells (grid_n %d) %.0fs
', nrow(cr$grid), gn_c, as.numeric(difftime(Sys.time(), t0, units = 'secs'))))
cref <- smooth(cr, d4)
# 2. fine pass per town, on its urban footprint: URBAN_M around each town site, gaps under 1 km closed, in the district
outline0 <- U(polys(sf::st_union(sf::st_transform(d4, crs)), crs), crs); fine <- list(); cat_all <- list()
for (k in seq_along(towns)) { tk <- sites[sites$town %in% k, ]
  ck <- sf::st_buffer(sf::st_buffer(sf::st_union(sf::st_buffer(sf::st_geometry(sf::st_transform(tk, crs)), URBAN_M)), 500), -500)
  ck <- U(polys(sf::st_intersection(ck, outline0), crs), crs); for (j in seq_along(cat_all)) ck <- U(polys(sf::st_difference(ck, cat_all[[j]]), crs), crs)
  ck <- sf::st_buffer(sf::st_buffer(ck, 1), -1); cat_all[[k]] <- ck
  cat_k <- sf::st_sf(district_name = D, geometry = sf::st_transform(ck, 4326)) |> safe_make_valid()
  a <- as.numeric(sf::st_area(sf::st_transform(cat_k, crs))); cs <- max(FINE_MIN, sqrt(a / FINE_MAX)); md <- calc_district_max_dim(sf::st_transform(cat_k, 3857)); gn <- as.integer(ceiling(md / cs))
  t0 <- Sys.time(); fr <- gen_run(cat_k, tk, gn, sum(utf8ToInt(D)) + k)
  dmin <- min(as.numeric(sf::st_distance(sf::st_transform(tk, crs)))[as.numeric(sf::st_distance(sf::st_transform(tk, crs))) > 0])
  cat(sprintf('town %d: catchment %.1f km2, fine cell ~%.0f m (closest two sites %.0f m), %d cells, %.0fs\n', k, a / 1e6, cs, dmin, nrow(fr$grid), as.numeric(difftime(Sys.time(), t0, units = 'secs'))))
  fine[[k]] <- smooth(fr, cat_k) }
# 3. merge + tidy against the district outline
outline <- U(polys(sf::st_union(sf::st_transform(d4, crs)), crs), crs)
town_u <- if (length(cat_all)) U(do.call(c, cat_all), crs) else NULL
rural <- cref[, 'dfa_name']
if (!is.null(town_u)) { ru <- sf::st_transform(rural, crs); sf::st_geometry(ru) <- do.call(c, lapply(seq_len(nrow(ru)), function(i) U(polys(sf::st_difference(sf::st_geometry(ru)[i], town_u), crs), crs))); rural <- sf::st_transform(ru, 4326) }
parts <- rbind(rural, do.call(rbind, lapply(fine, function(f) f[, 'dfa_name'])))
gn <- c(setdiff(fac$facility_name, extra_dfa_names)); gn <- gn[gn %in% parts$dfa_name]
miss <- setdiff(fac$facility_name, parts$dfa_name); if (length(miss)) cat('WARNING sites without an area:', paste(miss, collapse = ', '), '\n')
areas <- setNames(lapply(gn, function(n) U(polys(sf::st_union(sf::st_transform(parts[parts$dfa_name == n, ], crs)), crs), crs)), gn)
# a town site's coarse land that ended up cut off from its own area goes to the neighbour with the longest shared border
free <- list(); fown <- character(); sxy0 <- sf::st_transform(sites, crs)
for (n in gn) { p <- polys(areas[[n]], crs); if (length(p) < 2) next; j <- which(sxy0$facility_name == n)
  keep <- which(lengths(sf::st_intersects(p, sf::st_geometry(sxy0)[j])) > 0); if (!length(keep)) keep <- which.max(as.numeric(sf::st_area(p)))
  for (q in setdiff(seq_along(p), keep)) { free[[length(free) + 1]] <- p[q]; fown <- c(fown, n) }; areas[[n]] <- U(p[keep], crs) }
for (pass in 1:30) { if (!length(free)) break; lf <- list(); lo <- character()
  for (f in seq_along(free)) { bl <- vapply(gn, function(o) if (o == fown[f]) 0 else border_len(free[[f]], areas[[o]]), 0)
    if (max(bl) > 0.01) { w <- gn[which.max(bl)]; areas[[w]] <- U(polys(sf::st_union(areas[[w]], free[[f]]), crs), crs) } else { lf[[length(lf) + 1]] <- free[[f]]; lo <- c(lo, fown[f]) } }
  if (length(lf) == length(free)) { for (f in seq_along(lf)) areas[[lo[f]]] <- U(polys(sf::st_union(areas[[lo[f]]], lf[[f]]), crs), crs); lf <- list() }
  free <- lf; fown <- lo }
cl <- clean_partition(areas, outline, extra = extra_dfa_names)
# every site inside its own area: a small disk around it (25 m, or under half the distance to the nearest other site) goes to its area
sxy <- sf::st_transform(sites, crs); dd <- matrix(as.numeric(sf::st_distance(sxy)), nrow(sxy)); diag(dd) <- Inf
for (j in seq_len(nrow(sxy))) { n <- sxy$facility_name[j]; if (!n %in% gn) next
  if (lengths(sf::st_intersects(sf::st_geometry(sxy)[j], cl$areas[[n]]))) next
  disk <- sf::st_intersection(sf::st_buffer(sf::st_geometry(sxy)[j], min(25, 0.45 * min(dd[j, ]))), outline)
  for (o in setdiff(gn, n)) cl$areas[[o]] <- U(polys(sf::st_difference(cl$areas[[o]], disk), crs), crs); cl$areas[[n]] <- U(polys(sf::st_union(cl$areas[[n]], disk), crs), crs) }
# extra vertices every 100 m first, so long straight edges read the same on the sphere (s2) as flat (else the app can reject the shapes)
new <- sf::st_sf(dfa_name = gn, geometry = finalize_partition(lapply(cl$areas, function(g) U(polys(sf::st_segmentize(g, 100), crs), crs)), crs)); m <- partition_metrics(new, outline, crs); print(round(m, 2))
inn <- sf::st_intersects(sf::st_transform(sites, 4326), new); out_site <- sites$facility_name[!vapply(seq_len(nrow(sites)), function(j) length(inn[[j]]) && new$dfa_name[inn[[j]][1]] == sites$facility_name[j], TRUE)]
if (length(out_site)) cat('WARNING sites outside their own area:', paste(out_site, collapse = ', '), '\n')
# app paint grid (normal cell size): each cell takes the area holding its centre
grid <- make_paint_grid(district_sf = d4, grid_n = gn_c)$grid_sf; pts <- sf::st_as_sf(data.frame(x = grid$centroid_lon, y = grid$centroid_lat), coords = c('x', 'y'), crs = 4326)
k_ <- sf::st_intersects(pts, new); cells <- vapply(k_, function(z) if (length(z)) as.character(new$dfa_name[z[1]]) else NA_character_, '')
if (anyNA(cells)) cells[is.na(cells)] <- as.character(new$dfa_name[sf::st_nearest_feature(pts[is.na(cells), ], new)])
# areas too small for the app's cells still get the cell holding their site (if that cell's owner keeps others)
gpoly <- sf::st_transform(grid, 4326)
for (n in setdiff(gn, cells)) { j <- which(sites$facility_name == n); ci <- unlist(sf::st_intersects(sf::st_transform(sites[j, ], 4326), gpoly)); if (!length(ci)) next
  ow <- cells[ci[1]]; if (sum(cells == ow) > 1 && !(ow %in% sites$facility_name && sum(cells == ow) <= 1)) cells[ci[1]] <- n }
wp <- setNames(lapply(gn, function(n) sum(.polygon_u5_population(new[new$dfa_name == n, ], u5_rast), na.rm = TRUE)), gn)
tt <- v$snap$team_targets
tab <- data.frame(health_area = gn, town = sites$town[match(gn, sites$facility_name)], km2 = round(vapply(gn, function(n) A(sf::st_transform(new[new$dfa_name == n, ], crs)), 0) / 1e6, 2),
  worldpop_u5 = round(unlist(wp)), field_u5 = vapply(gn, function(n) as.numeric(tt[[n]]$target_pop %||% NA), 0), app_cells = as.integer(table(factor(cells, levels = gn))))
print(tab[order(tab$town, tab$health_area), ], row.names = FALSE)
f <- file.path(OUT, gsub('[^A-Za-z0-9]+', '_', D)); saveRDS(list(new = new, cref = cref, fine = fine, cells = cells, tab = tab), paste0(f, '_two_level.rds'))
cols <- grDevices::hcl.colors(nrow(new), 'Set 3')[sample(nrow(new))]
png(paste0(f, '_district.png'), 1600, 1300); plot(sf::st_geometry(new), col = cols, border = 'grey30', main = paste(D, '- two-level health areas')); plot(sf::st_geometry(sites), add = TRUE, pch = 19, cex = .8); dev.off()
for (k in seq_along(towns)) { bb <- sf::st_bbox(sf::st_buffer(sf::st_union(sf::st_transform(sites[sites$town %in% k, ], crs)), 800)) |> sf::st_as_sfc() |> sf::st_transform(4326) |> sf::st_bbox()
  png(sprintf('%s_town%d.png', f, k), 1600, 1600); plot(sf::st_geometry(new), col = cols, border = 'grey20', xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], main = sprintf('%s - town %d', D, k))
  plot(sf::st_geometry(sites), add = TRUE, pch = 19); text(sf::st_coordinates(sites), labels = sites$facility_name, cex = .75, pos = 3); dev.off() }
ok <- !length(miss) && !length(out_site) && m['overlap'] < 25 && m['s2_valid'] == 1 && m['empty'] == 0
if (!identical(Sys.getenv('APPLY'), '1')) { cat('CHECK ONLY', if (ok) '(would write)' else '(would NOT write)', '\n'); quit(save = 'no') }
if (!ok) stop('checks failed -- not written')
cur <- DBI::dbGetQuery(pool, 'SELECT is_shared FROM mapping_versions WHERE version_id = $1', list(V)); stopifnot(!cur$is_shared)
dn <- c(gn, extra_dfa_names)
db_submit_stage_v2(pool, V, v$owner_username, 'areas', list(saved_dfa_sf = new, dfa_names = dn, current_assignments = cells, smoothed_dfa_sf = new, team_targets = tt, dfa_worldpop_pop = wp))
print(DBI::dbGetQuery(pool, "SELECT version_id, owner_username, version_number, is_shared, has_health_areas, jsonb_array_length(saved_dfa_sf->'features') n_ha,
  jsonb_array_length(current_assignments) n_cells FROM mapping_versions WHERE version_id = $1", list(V)))
