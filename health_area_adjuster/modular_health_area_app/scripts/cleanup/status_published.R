# READ-ONLY campaign status (non-geometry): published versions, sites vs health areas, figures, team layouts.
# Env: WORK (writes status_*.csv there), CAMPAIGN (default 10).
suppressMessages(library(dotenv)); trace('load_dot_env', where = asNamespace('dotenv'), print = FALSE, exit = quote(Sys.setenv(DB_HOST = 'localhost', DB_PORT = '15432')))
suppressMessages(suppressWarnings(source('global.R'))); options(width = 250, warn = -1); sf::sf_use_s2(FALSE)
S <- Sys.getenv('WORK'); if (!nzchar(S)) stop('set WORK'); CID <- as.integer(Sys.getenv('CAMPAIGN', '10'))
q <- function(sql, p = list()) if (length(p)) DBI::dbGetQuery(pool, sql, p) else DBI::dbGetQuery(pool, sql)
CD <- q("SELECT district_name, mapping_scope FROM campaign_districts WHERE campaign_id = $1 ORDER BY 1", list(CID))
H <- q("SELECT version_id v, district_name d, owner_username own, version_number n, branched_from_id bf, shared_at, last_updated_at lu, COALESCE(smoothed_dfa_sf, saved_dfa_sf)::text g,
        odk_sf::text o, app_sf::text a, team_targets::text tt, scope_target_pop stp FROM mapping_versions WHERE campaign_id = $1 AND is_shared AND archived_at IS NULL", list(CID))
TV <- q("SELECT team_version_id tv, district_name d, health_area_name h, based_on_health_area_version_id hv, owner_username own, jsonb_array_length(COALESCE(smoothed_team_sf, saved_team_sf)->'features') nf,
         team_names::text tn FROM team_area_versions WHERE campaign_id = $1 AND is_shared AND archived_at IS NULL", list(CID))
issues <- list(); I <- function(d, kind, detail) issues[[length(issues) + 1]] <<- data.frame(district = d, issue = kind, detail = detail)
for (d in setdiff(CD$district_name, H$d)) I(d, 'no published health-area version', '')
summ <- list()
for (i in seq_len(nrow(H))) { d <- H$d[i]
  ha <- if (!is.na(H$g[i])) sf::st_make_valid(geojsonsf::geojson_sf(H$g[i])) else NULL
  if (is.null(ha)) { I(d, 'published version has no health areas', sprintf('%s v%d', H$own[i], H$n[i])); next }
  gn <- unique(as.character(ha$dfa_name)); real <- setdiff(gn, extra_dfa_names)
  st <- NULL; for (z in c(H$o[i], H$a[i])) if (!is.na(z)) { x <- geojsonsf::geojson_sf(z); if ('polio_sia_coordination_site' %in% names(x)) st <- rbind(st, x[x$polio_sia_coordination_site %in% 'Yes', 'facility_name']) }
  for (x in setdiff(real, st$facility_name)) I(d, 'health area with no selected site of that name', x)
  for (x in setdiff(st$facility_name, real)) I(d, 'selected site with no health area', x)
  if (!is.null(st) && nrow(st)) { k <- sf::st_intersects(sf::st_transform(st, 4326), sf::st_transform(ha, 4326))
    for (j in seq_len(nrow(st))) { inn <- unique(ha$dfa_name[k[[j]]]); if (st$facility_name[j] %in% real && !st$facility_name[j] %in% inn) I(d, 'site outside its own health area', sprintf('%s (in %s)', st$facility_name[j], paste(inn, collapse = '/') %||% 'none')) } }
  tt <- if (!is.na(H$tt[i])) jsonlite::fromJSON(H$tt[i], simplifyVector = FALSE) else list()
  num <- function(x) suppressWarnings(as.numeric(x %||% NA))
  u5 <- vapply(real, function(n) num(tt[[n]]$target_pop), 0); tm <- vapply(real, function(n) num(tt[[n]]$requested_teams), 0)
  if (any(is.na(u5))) I(d, 'missing Field U5', paste(real[is.na(u5)], collapse = '; '))
  if (any(is.na(tm))) I(d, 'missing Requested Teams', paste(real[is.na(tm)], collapse = '; '))
  if (is.na(H$stp[i])) I(d, 'missing In-Scope Field U5', '')
  t <- TV[TV$d == d, ]
  for (k in which(t$hv != H$v[i])) I(d, 'team layout drawn on an older health-area version', sprintf('%s (on v%d, published v%d)', t$h[k], t$hv[k], H$v[i]))
  for (k in which(!t$h %in% real)) I(d, 'team layout for a health area not in the published version', t$h[k])
  for (k in seq_len(nrow(t))) { nt <- length(tryCatch(unlist(jsonlite::fromJSON(t$tn[k])), error = function(e) character())); r <- tm[t$h[k]]
    if (!is.na(r) && nt != r) I(d, 'team count differs from Requested Teams', sprintf('%s: %d teams, %g requested', t$h[k], nt, r)) }
  summ[[length(summ) + 1]] <- data.frame(district = d, version = H$v[i], owner = H$own[i], owner_v = H$n[i], branched_from = H$bf[i], published = format(H$shared_at[i], '%m-%d %H:%M'),
    areas = length(real), sites = NROW(st), u5_missing = sum(is.na(u5)), teams_missing = sum(is.na(tm)), team_layouts = nrow(t), areas_without_layout = sum(!real %in% t$h)) }
SU <- do.call(rbind, summ); IS <- do.call(rbind, issues)
print(SU, row.names = FALSE); cat('\n'); print(IS, row.names = FALSE)
write.csv(SU, file.path(S, 'status_summary.csv'), row.names = FALSE); write.csv(IS, file.path(S, 'status_issues.csv'), row.names = FALSE)
