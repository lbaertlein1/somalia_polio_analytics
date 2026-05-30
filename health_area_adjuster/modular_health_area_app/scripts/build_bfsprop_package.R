# Make sure you're in the app directory first
getwd()  # should be .../modular_health_area_app


pkg_dir <- file.path(getwd(), "bfsprop")
dir.create(file.path(pkg_dir, "src"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(pkg_dir, "R"),   recursive = TRUE, showWarnings = FALSE)

writeLines(c(
  'Package: bfsprop',
  'Type: Package',
  'Title: BFS Propagation',
  'Version: 1.0.0',
  'Imports: Rcpp (>= 1.0.0)',
  'LinkingTo: Rcpp'
), file.path(pkg_dir, "DESCRIPTION"))

file.copy("bfs_propagate.cpp",
          file.path(pkg_dir, "src", "bfs_propagate.cpp"),
          overwrite = TRUE)

writeLines(c(
  'useDynLib(bfsprop, .registration = TRUE)',
  'importFrom(Rcpp, evalCpp)',
  'export(bfs_propagate_cpp)'
), file.path(pkg_dir, "NAMESPACE"))

Rcpp::compileAttributes(pkg_dir)
install.packages(pkg_dir, repos = NULL, type = "source")
