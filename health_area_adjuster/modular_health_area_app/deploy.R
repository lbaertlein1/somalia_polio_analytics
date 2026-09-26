all_files <- rsconnect::listBundleFiles(getwd())$contents

# Folders that are pipeline inputs/scratch only -- never deployed.
rscignore <- c(
  "data/osm_inputs",
  "data/land_surface_cache",
  "data/terra_temp"
)

exclude <- all_files[sapply(all_files, function(f) {
  any(sapply(rscignore, function(pattern) startsWith(f, pattern)))
})]

# Friction: the app only reads the per-district files of each boundary set,
# data/friction/<set>/district_standardized/*.tif. Everything else under
# data/friction/ (national surfaces, intermediate steps, and the old
# un-versioned data/friction/district_standardized/) is left out.
friction_files <- all_files[startsWith(all_files, "data/friction/")]
friction_keep  <- grepl("^data/friction/[^/]+/district_standardized/[^/]+\\.tif$", friction_files)
exclude <- union(exclude, friction_files[!friction_keep])

app_files <- setdiff(all_files, exclude)

cat("Excluded:", length(exclude), "\n")
cat("Deploying:", length(app_files), "\n")

rsconnect::deployApp(
  appFiles = app_files,
  appName = "health_area_adjuster",
  forceUpdate = TRUE,
  lint = FALSE
)
