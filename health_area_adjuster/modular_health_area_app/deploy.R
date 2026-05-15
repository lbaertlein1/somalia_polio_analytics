all_files <- rsconnect::listBundleFiles(getwd())$contents

rscignore <- c(
  "data/osm_inputs",
  "data/land_surface_cache",
  "data/friction/01_population_cost.tif",
  "data/friction/01b_after_land_surface.tif",
  "data/friction/02_after_roads.tif",
  "data/friction/03_after_rivers.tif",
  "data/friction/04_after_bridges.tif",
  "data/friction/05_after_water.tif",
  "data/friction/06_after_boundary.tif",
  "data/friction/somalia_friction_100m.tif",
  "data/friction/somalia_population_cost_100m.tif",
  "data/friction/somalia_template_100m.tif"
)

exclude <- all_files[sapply(all_files, function(f) {
  any(sapply(rscignore, function(pattern) startsWith(f, pattern)))
})]

app_files <- setdiff(all_files, exclude)

cat("Excluded:", length(exclude), "\n")
cat("Deploying:", length(app_files), "\n")

rsconnect::deployApp(
  appFiles = app_files,
  appName = "health_area_adjuster",
  forceUpdate = TRUE,
  lint = FALSE
)
