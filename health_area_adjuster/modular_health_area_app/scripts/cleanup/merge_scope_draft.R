# Merge a detailed In-Scope draft (e.g. a town) into a district's published health areas:
#   inside the draft's In Scope outline -> the draft's health areas; outside -> the published ones (clipped at the outline).
#   Published areas mostly (>50%) inside the outline are replaced; ones straddling it keep their outside part, joined to the
#   draft area of the same name if there is one (so e.g. a town+rural health area stays one area).
#   ADD_BACK: published town areas the draft dropped, re-added with their published shape (inside the outline), taken from the draft areas around them.
#   Names follow the published spelling (RENAME: draft=published pairs, ';'-separated). Sites = published site records
#   (+ draft-only Yes sites, renamed via SITE_RENAME draftSite=area pairs). A published town site lying outside its merged area uses the draft's position.
#   Figures: Field Target U5 from the draft, Requested Teams from the published version; In-Scope Field U5 kept from the published version.
#   Written as a new draft of OWNER branched from the published version (app's own db_branch_version / db_submit_stage_v2); PUBLISH=1 publishes it.
# Env: WORK, D, PUB (published version_id), DRAFT (draft version_id), OWNER, RENAME, ADD_BACK, SITE_RENAME, APPLY=1, PUBLISH=1
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 220, warn = -1); S <- Sys.getenv('WORK'); if (!nzchar(S)) stop('set WORK'); source('scripts/cleanup/clean_lib.R')
D <- Sys.getenv('D'); PUB <- as.integer(Sys.getenv('PUB')); DR <- as.integer(Sys.getenv('DRAFT')); OWNER <- Sys.getenv('OWNER')
pairs <- function(s) { p <- strsplit(strsplit(s, ';')[[1]], '='); p <- p[lengths(p) == 2]; setNames(trimws(vapply(p, function(z) z[2], '')), trimws(vapply(p, function(z) z[1], ''))) }
RN <- pairs(Sys.getenv('RENAME')); SRN <- pairs(Sys.getenv('SITE_RENAME')); ADD <- trimws(strsplit(Sys.getenv('ADD_BACK'), ',')[[1]])
row <- function(v) DBI::dbGetQuery(pool, "SELECT version_id, district_name, is_shared, COALESCE(smoothed_dfa_sf, saved_dfa_sf)::text g, scope_saved_dfa_sf::text sc, odk_sf::text o, app_sf::text a,
  team_targets::text tt, current_assignments::text ca, scope_target_pop, last_updated_at FROM mapping_versions WHERE version_id = $1", list(v))
P <- row(PUB); E <- row(DR); stopifnot(isTRUE(P$is_shared), P$district_name == D, E$district_name == D)
rd <- function(t) if (is.na(t)) NULL else sf::st_make_valid(geojsonsf::geojson_sf(t))
pha <- rd(P$g); eha <- rd(E$g); crs <- utm_for(pha); eha$dfa_name <- ifelse(eha$dfa_name %in% names(RN), RN[eha$dfa_name], eha$dfa_name)
shp <- districts_shp_for_campaign(10L); outline <- U(polys(sf::st_union(sf::st_transform(sf::st_make_valid(shp[shp$district_name == D, ]), crs)), crs), crs)
esc <- rd(E$sc); town <- U(polys(sf::st_intersection(sf::st_union(sf::st_transform(esc[esc$dfa_name == 'In Scope', ], crs)), outline), crs), crs)
byname <- function(x) { n <- unique(as.character(x$dfa_name)); setNames(lapply(n, function(k) U(polys(sf::st_union(sf::st_transform(x[x$dfa_name == k, ], crs)), crs), crs)), n) }
pa <- byname(pha); ea <- byname(eha)
share <- vapply(pa, function(g) A(polys(sf::st_intersection(g, town), crs)) / max(A(g), 1), 0)
replaced <- names(pa)[share > 0.5]; kept <- setdiff(names(pa), replaced)
areas <- lapply(ea, function(g) U(polys(sf::st_intersection(g, town), crs), crs))
for (n in kept) { out <- U(polys(sf::st_difference(pa[[n]], town), crs), crs); areas[[n]] <- if (n %in% names(areas)) U(polys(sf::st_union(areas[[n]], out), crs), crs) else out }
for (n in intersect(ADD, names(pa))) { g <- U(polys(sf::st_intersection(pa[[n]], town), crs), crs)
  for (o in names(areas)) areas[[o]] <- U(polys(sf::st_difference(areas[[o]], g), crs), crs); areas[[n]] <- g }
cat('published areas replaced by the draft:', length(replaced), '| kept (outside part):', length(kept), '| draft areas:', length(ea), '| added back:', paste(intersect(ADD, names(pa)), collapse = ', '), '\n')
cat('published names not in the merge:', paste(setdiff(names(pa), names(areas)), collapse = ', '), '\n')
cl <- clean_partition(areas, outline, extra = extra_dfa_names); gn <- names(cl$areas)
new <- sf::st_sf(dfa_name = gn, geometry = finalize_partition(cl$areas, crs)); m <- partition_metrics(new, outline, crs); print(round(m, 2))
# sites
po <- rd(P$o); pp <- rd(P$a); eo <- rd(E$o); ep <- rd(E$a)
newsites <- if (!is.null(ep)) ep[ep$polio_sia_coordination_site %in% 'Yes' & !ep$facility_id %in% pp$facility_id & !ep$facility_name %in% names(RN), ] else NULL
if (!is.null(newsites) && nrow(newsites)) newsites$facility_name <- ifelse(newsites$facility_name %in% names(SRN), SRN[newsites$facility_name], newsites$facility_name)
app <- if (!is.null(newsites) && nrow(newsites)) rbind(pp, newsites[, names(pp)]) else pp
inside <- function(pts) { k <- sf::st_intersects(sf::st_transform(pts, 4326), new); vapply(k, function(z) if (length(z)) new$dfa_name[z[1]] else NA_character_, '') }
for (i in which(po$polio_sia_coordination_site %in% 'Yes' & po$facility_name %in% gn)) { if (identical(inside(po[i, ]), po$facility_name[i])) next
  j <- match(po$facility_id[i], eo$facility_id); if (!is.na(j) && identical(inside(eo[j, ]), po$facility_name[i])) { sf::st_geometry(po)[i] <- sf::st_geometry(eo)[j]; cat('site moved to draft position:', po$facility_name[i], '\n') } }
yes <- rbind(po[po$polio_sia_coordination_site %in% 'Yes', 'facility_name'], app[app$polio_sia_coordination_site %in% 'Yes', 'facility_name'])
real <- setdiff(gn, extra_dfa_names)
cat('areas without a site:', paste(setdiff(real, yes$facility_name), collapse = ', '), '| sites without an area:', paste(setdiff(yes$facility_name, real), collapse = ', '), '\n')
ins <- inside(yes); bad <- yes$facility_name[!is.na(match(yes$facility_name, gn)) & (is.na(ins) | ins != yes$facility_name)]
cat('sites outside their own area:', paste(sprintf('%s (in %s)', bad, ins[match(bad, yes$facility_name)]), collapse = ', '), '\n')
# figures
ptt <- jsonlite::fromJSON(P$tt %||% '{}', simplifyVector = FALSE); ett <- jsonlite::fromJSON(E$tt %||% '{}', simplifyVector = FALSE)
names(ett) <- ifelse(names(ett) %in% names(RN), RN[names(ett)], names(ett))
num <- function(x) suppressWarnings(as.numeric(x %||% NA))
tt <- setNames(lapply(real, function(n) list(target_pop = num(ett[[n]]$target_pop), requested_teams = num(ptt[[n]]$requested_teams))), real)
wp <- setNames(lapply(real, function(n) sum(.polygon_u5_population(new[new$dfa_name == n, ], u5_rast), na.rm = TRUE)), real)
km2 <- setNames(as.numeric(sf::st_area(sf::st_transform(new, crs))) / 1e6, gn)
wh <- ifelse(real %in% c(names(ea), ADD), ifelse(real %in% kept & !real %in% ADD, 'town + outside', 'town'), 'outside')
tab <- data.frame(health_area = real, where = wh, km2 = round(km2[real], 2), worldpop_u5 = round(unlist(wp)),
  field_u5 = vapply(tt, function(z) z$target_pop, 0), teams = vapply(tt, function(z) z$requested_teams, 0))
print(tab[order(tab$where, tab$health_area), ], row.names = FALSE)
# painted cells on the published version's grid (canvas = its In Scope area)
psc <- rd(P$sc); canvas <- if (!is.null(psc)) sf::st_transform(psc[psc$dfa_name == 'In Scope', ], 4326) else sf::st_transform(shp[shp$district_name == D, ], 4326)
c3857 <- tryCatch(sf::st_collection_extract(safe_make_valid(sf::st_transform(canvas, 3857)), 'POLYGON'), error = function(e) sf::st_transform(canvas, 3857))
md <- calc_district_max_dim(c3857); grid <- make_paint_grid(district_sf = sf::st_transform(c3857, 4326), grid_n = max(10L, as.integer(round(md / max(100, min(500, md / 160))))))$grid_sf
old <- unlist(jsonlite::fromJSON(P$ca)); stopifnot(length(old) == nrow(grid))
pts <- sf::st_as_sf(data.frame(x = grid$centroid_lon, y = grid$centroid_lat), coords = c('x', 'y'), crs = 4326); sp <- inside(pts)
cells <- ifelse(is.na(sp), ifelse(old %in% gn, old, new$dfa_name[sf::st_nearest_feature(pts, new)]), sp); cat('cells:', length(cells), '| changed:', sum(cells != old), '\n')
tag <- gsub('[^A-Za-z0-9]+', '_', D); saveRDS(list(new = new, tab = tab, cells = cells), file.path(S, paste0('merge_', tag, '.rds')))
png(file.path(S, paste0('merge_', tag, '.png')), 1800, 1100); par(mfrow = c(1, 2)); bb <- sf::st_bbox(sf::st_buffer(town, 1500)); nu <- sf::st_transform(new, crs)
plot(sf::st_geometry(nu), col = grDevices::hcl.colors(nrow(nu), 'Set 3'), xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], main = 'merged - town'); plot(town, add = TRUE, border = 'red', lwd = 2)
plot(sf::st_geometry(sf::st_transform(yes, crs)), add = TRUE, pch = 19, cex = .8); text(sf::st_coordinates(sf::st_point_on_surface(nu)), labels = nu$dfa_name, cex = .75)
plot(sf::st_geometry(nu), col = grDevices::hcl.colors(nrow(nu), 'Set 3'), main = 'merged - district'); plot(town, add = TRUE, border = 'red', lwd = 2); dev.off()
if (!identical(Sys.getenv('APPLY'), '1')) quit(save = 'no')
if (!isTRUE(as.logical(m['s2_valid'])) || m['overlap'] > 25 || m['gap'] > 1000 || length(setdiff(real, yes$facility_name)) || length(setdiff(yes$facility_name, real))) stop('checks failed -- not written')
chk <- row(PUB); stopifnot(isTRUE(chk$is_shared), identical(as.numeric(chk$last_updated_at), as.numeric(P$last_updated_at)))
vid <- db_branch_version(pool, source_version_id = PUB, owner_username = OWNER, target_campaign_id = 10L, district_name = D); stopifnot(!is.null(vid))
db_submit_stage_v2(pool, vid, OWNER, 'facilities', list(odk_sf = po, app_sf = app))
db_submit_stage_v2(pool, vid, OWNER, 'areas', list(saved_dfa_sf = new, dfa_names = c(real, extra_dfa_names), current_assignments = cells, smoothed_dfa_sf = new, team_targets = tt, dfa_worldpop_pop = wp))
if (!is.na(P$scope_target_pop)) db_set_scope_population(pool, vid, P$scope_target_pop, sum(.polygon_u5_population(sf::st_transform(sf::st_sf(geometry = outline), 4326), u5_rast), na.rm = TRUE))
if (identical(Sys.getenv('PUBLISH'), '1')) db_publish_version(pool, vid, actor_role = 'admin')
print(DBI::dbGetQuery(pool, "SELECT version_id, owner_username, version_number, is_shared, has_health_areas, scope_target_pop, jsonb_array_length(saved_dfa_sf->'features') n_ha,
  (SELECT count(*) FROM jsonb_object_keys(team_targets)) n_targets FROM mapping_versions WHERE version_id = $1", list(vid)))
