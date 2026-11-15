# Write a checked generate_two_level.R result (<OUT>/<district>_two_level.rds) into its UNPUBLISHED draft, without regenerating.
# Env: V (version id), OUT, APPLY=1 to write, PUBLISH=1 to also publish it.
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 220, warn = -1); source('scripts/cleanup/clean_lib.R')
V <- as.integer(Sys.getenv('V')); OUT <- Sys.getenv('OUT'); v <- db_get_version_by_id(pool, V); stopifnot(!is.null(v), !isTRUE(v$is_shared)); D <- v$district_name
r <- readRDS(file.path(OUT, paste0(gsub('[^A-Za-z0-9]+', '_', D), '_two_level.rds'))); new <- r$new; cells <- r$cells; gn <- as.character(new$dfa_name)
# same checks as the generator: every selected site has an area and sits in it, partition sound, every area has a paint cell
fac <- dplyr::bind_rows(facility_sf_to_df(v$snap$odk_sf), facility_sf_to_df(v$snap$app_sf)); fac <- fac[fac$polio_sia_coordination_site %in% 'Yes', , drop = FALSE]
sites <- sf::st_as_sf(fac, coords = c('lon', 'lat'), crs = 4326); stopifnot(setequal(sites$facility_name, gn))
inn <- sf::st_intersects(sites, new); bad <- sites$facility_name[!vapply(seq_len(nrow(sites)), function(j) length(inn[[j]]) && gn[inn[[j]][1]] == sites$facility_name[j], TRUE)]
d4 <- sf::st_transform(safe_make_valid(districts_shp_for_campaign(v$campaign_id))[districts_shp_for_campaign(v$campaign_id)$district_name == D, ], 4326); crs <- utm_for(d4)
m <- partition_metrics(new, U(polys(sf::st_union(sf::st_transform(d4, crs)), crs), crs), crs); print(round(m, 2))
cat('sites outside own area:', paste(bad, collapse = ', '), '| areas without a paint cell:', paste(setdiff(gn, cells), collapse = ', '), '\n')
if (length(bad) || m['overlap'] >= 25 || m['s2_valid'] != 1 || m['empty'] != 0) stop('checks failed -- not written')
if (!identical(Sys.getenv('APPLY'), '1')) { cat('CHECK ONLY\n'); quit(save = 'no') }
wp <- setNames(lapply(gn, function(n) sum(.polygon_u5_population(new[new$dfa_name == n, ], u5_rast), na.rm = TRUE)), gn)
db_submit_stage_v2(pool, V, v$owner_username, 'areas', list(saved_dfa_sf = new, dfa_names = c(gn, extra_dfa_names), current_assignments = cells,
  smoothed_dfa_sf = new, team_targets = v$snap$team_targets, dfa_worldpop_pop = wp))
if (identical(Sys.getenv('PUBLISH'), '1')) db_publish_version(pool, V, actor_role = 'admin')
print(DBI::dbGetQuery(pool, "SELECT version_id, district_name, owner_username, version_number, is_shared, has_health_areas, scope_target_pop,
  jsonb_array_length(saved_dfa_sf->'features') n_ha, jsonb_array_length(current_assignments) n_cells FROM mapping_versions WHERE version_id = $1", list(V)))
