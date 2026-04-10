`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

safe_make_valid <- function(x) {
  tryCatch(sf::st_make_valid(x), error = function(e) x)
}

clamp_num <- function(x, lo, hi) {
  max(lo, min(hi, x))
}

round_to_step <- function(x, step) {
  round(x / step) * step
}
