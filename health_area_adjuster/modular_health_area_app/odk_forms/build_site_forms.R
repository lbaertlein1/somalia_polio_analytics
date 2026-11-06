# =============================================================================
# build_site_forms.R
#
# Generates the XLSForm that maintains the microplan site registry -- an ODK
# Central Entities list named "sites" in project 36 "Somalia - Polio SIA
# Microplanning" (kept out of the MoH/WHO project 9 and the MHFL project 31
# so site editors and MHFL users stay separate):
#
#   site_form.xlsx (form id somalia_sites) -- one form that either registers
#   a new site or updates / removes an existing one (entities create_if /
#   update_if). status = "removed" is the soft delete (the app does the same,
#   so syncs and bulk loads never resurrect a site someone removed).
#
# Site types: idp, nomadic, fixed, cross_border, transit. Subtype lists are
# DRAFTS -- edit .subtypes below and re-run this script. IDP and nomadic
# settlements have no subtype question.
#
# Entity properties written by the form (see SITE_PROPS in central_helpers.R):
#   region_code, district_code, district_name, site_type, site_subtype,
#   subtype_other, site_name, geometry, status, removal_reason, notes,
#   u5_population, last_confirmed_date,          (idp / nomadic)
#   mhfl_match, linked_facility_id,              (fixed at a health facility)
#   crossing_name,                               (cross_border)
#   last_edited_by, last_edited_at, source
#
# district_code is DIST_UID from data/districts_shp.Rds (e.g. "AWL-BOR") --
# a stable key the app matches on, with district_name kept for display.
#
# Run from the app folder:  Rscript odk_forms/build_site_forms.R
# =============================================================================

out_dir <- "odk_forms"
form_version <- format(Sys.time(), "%Y%m%d%H%M")

# ---- choice lists ------------------------------------------------------------

districts <- sf::st_drop_geometry(readRDS("data/districts_shp.Rds"))
districts <- districts[order(districts$region_name, districts$district_name), ]

region_choices <- unique(data.frame(
  list_name = "region", name = districts$REG_CODE, label = districts$region_name,
  site_type = NA_character_, region = NA_character_
))
district_choices <- data.frame(
  list_name = "district", name = districts$DIST_UID, label = districts$district_name,
  site_type = NA_character_, region = districts$REG_CODE
)

.ch <- function(list_name, name, label) {
  data.frame(list_name = list_name, name = name, label = label,
             site_type = NA_character_, region = NA_character_)
}

action_choices <- .ch("action", c("new", "existing"),
  c("Register a new site", "Update or remove an existing site"))

site_type_choices <- .ch("site_type",
  c("idp", "nomadic", "fixed", "cross_border", "transit"),
  c("IDP settlement", "Nomadic settlement", "Fixed vaccination site",
    "Cross-border vaccination site", "Transit vaccination site"))

# DRAFT subtype lists. Names are prefixed with the site type so they stay
# unique within the one list; every type ends in <type>_other, which opens
# the free-text subtype_other question.
.subtypes <- list(
  # health_facility opens the MHFL facility picker.
  fixed = c(health_facility = "Health facility", school = "School", mosque = "Mosque",
            market = "Market", community_centre = "Community centre / leader's house",
            outreach_post = "Outreach post"),
  cross_border = c(official_crossing = "Official border crossing", informal_crossing = "Informal crossing",
                   border_town_market = "Border town / market"),
  transit = c(bus_station = "Bus station", road_checkpoint = "Road checkpoint", market = "Market",
              port = "Port", airport = "Airport", livestock_route = "Livestock route")
)
subtype_choices <- do.call(rbind, lapply(names(.subtypes), function(t) {
  s <- c(.subtypes[[t]], other = "Other")
  data.frame(list_name = "site_subtype", name = paste0(t, "_", names(s)), label = unname(s),
             site_type = t, region = NA_character_)
}))

# The registry is not campaign-specific, so there is no "inactive": a site
# is either in the registry or removed (soft delete).
keep_choices <- .ch("keep_remove", c("keep", "remove"),
  c("Keep - the site exists", "Remove from the registry"))
removal_choices <- .ch("removal_reason", c("no_longer_exists", "duplicate", "entered_by_mistake"),
  c("No longer exists", "Duplicate of another site", "Entered by mistake"))

all_choices <- rbind(region_choices, district_choices, action_choices, site_type_choices,
                     subtype_choices, keep_choices, removal_choices)

# ---- survey ----------------------------------------------------------------------
# Columns: type, name, label, hint, required, relevant, constraint,
#          constraint_message, choice_filter, appearance, default,
#          calculation, trigger, save_to

.row <- function(type, name, label = NA, hint = NA, required = NA, relevant = NA,
                 constraint = NA, constraint_message = NA, choice_filter = NA,
                 appearance = NA, default = NA, calculation = NA, trigger = NA, save_to = NA) {
  data.frame(type, name, label, hint, required, relevant, constraint, constraint_message,
             choice_filter, appearance, default, calculation, trigger, save_to)
}

is_new   <- "selected(${action}, 'new')"
is_upd   <- "selected(${action}, 'existing')"
is_pop   <- "selected(${site_type}, 'idp') or selected(${site_type}, 'nomadic')"
is_hf    <- "selected(${site_subtype}, 'fixed_health_facility')"
is_xb    <- "selected(${site_type}, 'cross_border')"
picked   <- "${mhfl_facility} != ''"
# Saved value of the selected existing site (pre-fill on update; fires only
# when ${site} changes, so never on a new site).
cur      <- function(prop) sprintf("instance('sites')/root/item[name=${site}]/%s", prop)
trg      <- "${site}"
# Chosen MHFL facility (project-36 mirror of project 31's master_hf_registry).
.hf      <- function(prop) sprintf("instance('master_hf_registry')/root/item[name=${mhfl_facility}]/%s", prop)
hf_point <- paste0("if(", .hf("gps_latitude"), " != '', concat(", .hf("gps_latitude"), ", ' ', ",
                   .hf("gps_longitude"), ", ' 0 0'), '')")

survey <- rbind(
  .row("start", "start"), .row("end", "end"), .row("username", "username"),
  .row("deviceid", "deviceid"),

  .row("select_one region", "region", "Region", required = "yes", save_to = "region_code"),
  .row("select_one district", "district", "District", required = "yes",
       choice_filter = "region=${region}", save_to = "district_code"),
  .row("calculate", "district_name", calculation = "jr:choice-name(${district}, '${district}')",
       save_to = "district_name"),

  .row("select_one action", "action", "What do you want to do?", required = "yes"),
  .row("select_one_from_file sites.csv", "site", "Which site?", required = "yes",
       relevant = is_upd, choice_filter = "district_code=${district} and status != 'removed'",
       appearance = "autocomplete", hint = "Only sites in the selected district are listed. Type to search."),

  .row("select_one site_type", "site_type", "Site type", required = "yes",
       calculation = cur("site_type"), trigger = trg, save_to = "site_type"),
  .row("select_one site_subtype", "site_subtype", "What kind of site is it?", required = "yes",
       relevant = "not(selected(${site_type}, 'idp') or selected(${site_type}, 'nomadic'))",
       choice_filter = "site_type=${site_type}",
       calculation = cur("site_subtype"), trigger = trg, save_to = "site_subtype"),
  .row("text", "subtype_other", "Describe the kind of site", required = "yes",
       relevant = "contains(${site_subtype}, '_other')",
       calculation = cur("subtype_other"), trigger = trg, save_to = "subtype_other"),

  # Fixed sites at a health facility: optional pick from the MHFL mirror
  # (filtered to the district). Blank = not listed. The ORIGINAL project-31
  # UUID (mirror property mhfl_uuid) is stored as linked_facility_id.
  .row("select_one_from_file master_hf_registry.csv", "mhfl_facility", "Select the health facility",
       hint = "Type to search. If it is not in the list, leave this blank and type the name below.",
       relevant = is_hf, choice_filter = "district_code=${district}", appearance = "autocomplete",
       calculation = sprintf("instance('master_hf_registry')/root/item[mhfl_uuid=%s]/name", cur("linked_facility_id")),
       trigger = trg),
  .row("calculate", "mhfl_match", relevant = is_hf,
       calculation = sprintf("if(%s, 'listed', 'not_listed')", picked), save_to = "mhfl_match"),
  .row("calculate", "linked_facility_id", relevant = paste(is_hf, "and", picked),
       calculation = .hf("mhfl_uuid"), save_to = "linked_facility_id"),

  # New site: name/location pre-fill from the chosen facility. Existing
  # site: from the saved values (a facility re-pick keeps them).
  .row("text", "site_name", "Site name", required = "yes",
       calculation = sprintf("if(%s, %s, %s)", is_new, .hf("facility_name"), cur("site_name")),
       trigger = "${site}, ${mhfl_facility}", save_to = "site_name"),
  .row("geopoint", "location", "Site location",
       hint = "For a saved site this shows the saved location -- re-capture only if it is wrong.",
       required = "yes", appearance = "maps",
       calculation = sprintf("if(%s, %s, %s)", is_new, hf_point, cur("geometry")),
       trigger = "${site}, ${mhfl_facility}", save_to = "geometry"),

  # IDP / nomadic
  .row("integer", "u5_population", "Estimated population aged under 5",
       hint = "Children 0-59 months. Best available estimate; leave blank if unknown.",
       relevant = is_pop, constraint = ". >= 0",
       calculation = cur("u5_population"), trigger = trg, save_to = "u5_population"),
  .row("date", "last_confirmed_date", "Date presence at this location was last confirmed",
       relevant = is_pop, required = "yes", default = "today()",
       constraint = ". <= today()", constraint_message = "Cannot be in the future.",
       save_to = "last_confirmed_date"),

  # Cross-border
  .row("text", "crossing_name", "Crossing point name", relevant = is_xb,
       calculation = cur("crossing_name"), trigger = trg, save_to = "crossing_name"),

  .row("text", "notes", "Notes", appearance = "multiline",
       calculation = cur("notes"), trigger = trg, save_to = "notes"),

  # Keep / remove (existing sites only); new sites are always active.
  .row("select_one keep_remove", "keep_remove", "Should this site stay in the registry?", required = "yes",
       relevant = is_upd, calculation = sprintf("if(%s = 'removed', 'remove', 'keep')", cur("status")),
       trigger = trg),
  .row("select_one removal_reason", "removal_reason", "Why is it being removed?", required = "yes",
       relevant = "selected(${keep_remove}, 'remove')", save_to = "removal_reason"),
  .row("calculate", "status",
       calculation = sprintf("if(%s and selected(${keep_remove}, 'remove'), 'removed', 'active')", is_upd),
       save_to = "status"),

  # Audit / provenance -- the app writes its own user here; the form records
  # the Collect/web user (which also marks a synced site as locally edited).
  .row("calculate", "last_edited_by", calculation = "${username}", save_to = "last_edited_by"),
  .row("calculate", "last_edited_at", calculation = "now()", save_to = "last_edited_at"),
  .row("calculate", "source", calculation = sprintf("if(%s, 'odk_form', %s)", is_new, cur("source")),
       save_to = "source")
)

entities <- data.frame(
  list_name = "sites",
  create_if = "${action} = 'new'",
  update_if = "${action} = 'existing'",
  entity_id = "coalesce(${site}, uuid())",
  label = "concat(${site_name}, ' - ', jr:choice-name(${site_type}, '${site_type}'), ' (', ${district_name}, ')')"
)

# ---- write -------------------------------------------------------------------------

.drop_empty_cols <- function(df) df[, colSums(!is.na(df)) > 0, drop = FALSE]

# writexl, not openxlsx: openxlsx workbooks carry a dangling drawing
# relationship that openpyxl (and so pyxform / ODK Central's converter)
# can't open -- Central then silently stores an empty form.
path <- file.path(out_dir, "site_form.xlsx")
writexl::write_xlsx(list(
  survey   = .drop_empty_cols(survey),
  choices  = .drop_empty_cols(all_choices),
  settings = data.frame(form_title = "Microplan Sites", form_id = "somalia_sites", version = form_version),
  entities = entities
), path)
cat("wrote", path, "\n")
