# R/utils.R
# Shared helpers and crosswalk tables

# ── District name crosswalk ────────────────────────────────────────────────────
# Left  = raw value as it may appear in ODK data
# Right = canonical name matching ArcGIS PHB 2026 shapefile
# Add / correct entries here after inspecting your actual data
DISTRICT_CROSSWALK <- c(
  "Balathawa"        = "Belet-hawa",
  "Belet Hawa"       = "Belet-hawa",
  "Belet-Hawa"       = "Belet-hawa",
  "Belethawa"        = "Belet-hawa",
  "Bardheere"        = "Baardhere",
  "Bardheer"         = "Baardhere",
  "Bardhere"         = "Baardhere",
  "Dollow"           = "Dolow",
  "Doolow"           = "Dolow",
  "Luq"              = "Luuq",
  "Luuq District"    = "Luuq",
  "Garbahaare"       = "Garbaharey",
  "Elbarde"          = "El Barde",
  "Beled Weyne"      = "Beledweyne"
)

normalise_district <- function(x) {
  x_trimmed <- trimws(x)
  ifelse(x_trimmed %in% names(DISTRICT_CROSSWALK),
         DISTRICT_CROSSWALK[x_trimmed],
         x_trimmed)
}

month_to_season <- function(m) {
  dplyr::case_when(
    m %in% c(10, 11)      ~ "Deyr",
    m %in% c(3, 4, 5, 6) ~ "Guul",
    m %in% c(12, 1, 2)   ~ "Jilaal",
    m %in% c(7, 8, 9)    ~ "Xaaga",
    TRUE                  ~ NA_character_
  )
}

COL_TEAL   <- "#0d9488"
COL_RED    <- "#dc2626"
COL_ORANGE <- "#d97706"
COL_BLUE   <- "#2563eb"
COL_GREEN  <- "#059669"
COL_MUTED  <- "#718096"

safe_pct <- function(num, den, digits = 1) {
  ifelse(den > 0, round(100 * num / den, digits), NA_real_)
}
