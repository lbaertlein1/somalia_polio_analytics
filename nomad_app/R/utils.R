# R/utils.R
# Shared helpers and crosswalk tables

# ── District name crosswalk ────────────────────────────────────────────────────
# Left  = raw value as it may appear in ODK data
# Right = canonical name matching ArcGIS PHB 2026 shapefile
# Add / correct entries here after inspecting your actual data
DISTRICT_CROSSWALK <- c(
  # ── Baardhere / Bardera ──────────────────────────────────────────────────
  "Baardheer"          = "Baardhere",
  "baardheere"         = "Baardhere",
  "Baardheere"         = "Baardhere",
  "Baardheere "        = "Baardhere",
  "Baardhera"          = "Baardhere",
  "Baardhera "         = "Baardhere",
  "Baardhere "         = "Baardhere",
  "Baardhere garilley" = "Baardhere",
  "Bardheere"          = "Baardhere",
  "Bardheere "         = "Baardhere",
  "Bardhere"           = "Baardhere",
  "Bardhere "          = "Baardhere",
  "Bardera"            = "Baardhere",
  "BARDERA"            = "Baardhere",
  "bardera"            = "Baardhere",
  "bardera "           = "Baardhere",
  "Bardera "           = "Baardhere",
  "Bardera hospital"   = "Baardhere",
  "Bardere"            = "Baardhere",
  "Bardhale"           = "Baardhere",
  "Bardhale "          = "Baardhere",
  "Bardaale"           = "Baardhere",
  "Bardale"            = "Baardhere",
  "Bardale "           = "Baardhere",
  "Bardalle"           = "Baardhere",
  "Bardheera"          = "Baardhere",
  "Bardhera"           = "Baardhere",
  "Bardhera "          = "Baardhere",
  "Badheere"           = "Baardhere",
  # ── Badhadhe ─────────────────────────────────────────────────────────────
  "Badhadhe"           = "Badhadhe",
  "Badhadhe "          = "Badhadhe",
  "BADHADHE"           = "Badhadhe",
  "Badhaadhe"          = "Badhadhe",
  "Badhaade"           = "Badhadhe",
  "Badhaadha"          = "Badhadhe",
  "BADHAADHE"          = "Badhadhe",
  # ── Belet-hawa ───────────────────────────────────────────────────────────
  "Belet-hawa"         = "Belet-hawa",
  "Belet-hawa "        = "Belet-hawa",
  "belet-hawa"         = "Belet-hawa",
  "BELET HAWA"         = "Belet-hawa",
  "Balat-hawa"         = "Belet-hawa",
  "Balat-hawa "        = "Belet-hawa",
  "balat-hawa "        = "Belet-hawa",
  "Balet-hawa"         = "Belet-hawa",
  "Balet xaw"          = "Belet-hawa",
  "Balet xawa"         = "Belet-hawa",
  "Balad hawa"         = "Belet-hawa",
  "Balad hawo "        = "Belet-hawa",
  "baladhawo"          = "Belet-hawa",
  "Baladxaawo"         = "Belet-hawa",
  "baladxawo"          = "Belet-hawa",
  "Baladxawo"          = "Belet-hawa",
  "Baladxawo "         = "Belet-hawa",
  "Bele-hawo"          = "Belet-hawa",
  "Bele xawa"          = "Belet-hawa",
  "Beled xawo"         = "Belet-hawa",
  "belet cawo"         = "Belet-hawa",
  "belet hawa"         = "Belet-hawa",
  "Belet-hawo"         = "Belet-hawa",
  "Belet-xawa"         = "Belet-hawa",
  "Belet -hawa"        = "Belet-hawa",
  "belet xawo"         = "Belet-hawa",
  "Belet xawa"         = "Belet-hawa",
  "Belet xawo"         = "Belet-hawa",
  "Belet_hawa"         = "Belet-hawa",
  "Belete xawo"        = "Belet-hawa",
  "belethawa"          = "Belet-hawa",
  "Belethawa"          = "Belet-hawa",
  "Belethawa "         = "Belet-hawa",
  "Beletxawa"          = "Belet-hawa",
  "beletxawa "         = "Belet-hawa",
  "Bled xawa"          = "Belet-hawa",
  "Bulaxawo"           = "Belet-hawa",
  "Beledxawa"          = "Belet-hawa",
  " Belet-hawa "       = "Belet-hawa",
  "Balat-hawa "        = "Belet-hawa",
  # ── Beledweyne ───────────────────────────────────────────────────────────
  "Beledweyne"         = "Beledweyne",
  "Beledweyne "        = "Beledweyne",
  "Beledweyn"          = "Beledweyne",
  "Beledweyn "         = "Beledweyne",
  "Beletweyn"          = "Beledweyne",
  "Beletweyn "         = "Beledweyne",
  "BELET WEYNE"        = "Beledweyne",
  "Beled Weyne"        = "Beledweyne",
  "Baladweyne"         = "Beledweyne",
  "Baladweyne "        = "Beledweyne",
  # ── Dolow ─────────────────────────────────────────────────────────────────
  "Dolow"              = "Dolow",
  "Dolow "             = "Dolow",
  "dolow"              = "Dolow",
  "DOLOW"              = "Dolow",
  "Dollow"             = "Dolow",
  "dollow"             = "Dolow",
  "Doolow"             = "Dolow",
  "DOLO"               = "Dolow",
  "Doloow"             = "Dolow",
  "Doloow "            = "Dolow",
  "Dolw"               = "Dolow",
  "DOLW"               = "Dolow",
  "Dalaw"              = "Dolow",
  "Dalaw "             = "Dolow",
  "Dalow"              = "Dolow",
  "Dalow "             = "Dolow",
  "Dlow"               = "Dolow",
  "DLOW"               = "Dolow",
  "Dlow "              = "Dolow",
  "Dolaw"              = "Dolow",
  "Dolaw "             = "Dolow",
  " Doloow "           = "Dolow",
  # ── Elwak ────────────────────────────────────────────────────────────────
  "Elwak"              = "Elwak",
  "Elwak "             = "Elwak",
  "ELWAK"              = "Elwak",
  "EL WAK"             = "Elwak",
  "Elwaq"              = "Elwak",
  "Elewak"             = "Elwak",
  "Elwaka"             = "Elwak",
  "Ceel waaq"          = "Elwak",
  "Celwaaq"            = "Elwak",
  "Celwaaq "           = "Elwak",
  "Celwaq"             = "Elwak",
  "Celwaq "            = "Elwak",
  "Celwa"              = "Elwak",
  # ── El Barde ──────────────────────────────────────────────────────────────
  "El Barde"           = "El Barde",
  "EL BARDE"           = "El Barde",
  "Elbarde"            = "El Barde",
  "Elbarde "           = "El Barde",
  "elbarde"            = "El Barde",
  "El-berde"           = "El Barde",
  "Ceelbarde"          = "El Barde",
  "Celbarde"           = "El Barde",
  "Celbarde "          = "El Barde",
  # ── Garbaharey ────────────────────────────────────────────────────────────
  "Garbaharey"         = "Garbaharey",
  "Garbaharey "        = "Garbaharey",
  "GARBAHAREY"         = "Garbaharey",
  "Gabaharey"          = "Garbaharey",
  "Garbaherey"         = "Garbaharey",
  "Garbahrey"          = "Garbaharey",
  "Garbareey"          = "Garbaharey",
  "Garbareey "         = "Garbaharey",
  "Karbareey"          = "Garbaharey",
  "Gaebaharey"         = "Garbaharey",
  # ── Luuq ──────────────────────────────────────────────────────────────────
  "Luuq"               = "Luuq",
  "Luuq "              = "Luuq",
  "LUUQ"               = "Luuq",
  "Luq"                = "Luuq",
  "Luup"               = "Luuq",
  "Luuuq"              = "Luuq",
  " Luuq "             = "Luuq",
  "Luuq District"      = "Luuq",
  "Galweyn luuq"       = "Luuq",
  # ── Baidoa ────────────────────────────────────────────────────────────────
  "Baidoa"             = "Baidoa",
  "Baidoa "            = "Baidoa",
  "BAIDOA"             = "Baidoa",
  "Baidoa Somalia"     = "Baidoa",
  "Baydhabo"           = "Baidoa",
  "Baydhabo "          = "Baidoa",
  # ── Burdhubo ──────────────────────────────────────────────────────────────
  "Burdhubo"           = "Burdhubo",
  "Burdhubo "          = "Burdhubo",
  "Buurdhuubo"         = "Burdhubo",
  "Buurdhuubo "        = "Burdhubo",
  # ── Aden Yabaal ───────────────────────────────────────────────────────────
  "Adan yabal"         = "Aden Yabaal",
  "Aden Yabaal"        = "Aden Yabaal",
  " Adan yabal"        = "Aden Yabaal",
  "Aden yabaal"        = "Aden Yabaal",
  # ── Afgooye ───────────────────────────────────────────────────────────────
  "Afgooye"            = "Afgooye",
  "Afgoye"             = "Afgooye",
  # ── Junk / unresolvable → NA ──────────────────────────────────────────────
  "N/A"                = NA_character_,
  "NA"                 = NA_character_,
  "N a"                = NA_character_,
  "Na"                 = NA_character_,
  "n"                  = NA_character_,
  "B"                  = NA_character_,
  "H"                  = NA_character_,
  "MA"                 = NA_character_,
  "Yg"                 = NA_character_,
  "Because"            = NA_character_,
  "Heheheh"            = NA_character_,
  "Hhh"                = NA_character_,
  "0623344554"         = NA_character_,
  "Elw-Gr-001"         = NA_character_,
  "BLW-"               = NA_character_,
  "Sinai"              = NA_character_,
  "Hiiraan"            = NA_character_,
  "Gedo"               = NA_character_,
  "Gedo "              = NA_character_,
  "Bay"                = NA_character_,
  "Bakol"              = NA_character_
)

normalise_district <- function(x) {
  x_trimmed <- trimws(as.character(x))
  mapped <- dplyr::case_when(
    x_trimmed %in% names(DISTRICT_CROSSWALK) ~ unname(DISTRICT_CROSSWALK[x_trimmed]),
    TRUE ~ x_trimmed
  )
  # Title-case fallback for ALL CAPS or all lowercase (length > 2)
  needs_case <- !is.na(mapped) &
    (mapped == toupper(mapped) | mapped == tolower(mapped)) &
    nchar(mapped) > 2
  mapped[needs_case] <- tools::toTitleCase(tolower(mapped[needs_case]))
  as.character(mapped)
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