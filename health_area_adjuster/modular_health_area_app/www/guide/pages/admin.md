# Admin

Admin functions are for setting up users, campaigns, and data sources — most day-to-day mapping work doesn't need this tab.

![The Admin Panel with the Users table (admin and Luke accounts), the Campaigns table (Practice Campaign and Aug SNID 2026, both Active), and the top of the District progress table showing districts with their Health Areas and Team Areas counts](images/admin-panel-overview.png)
<p class="img-caption">The three main tables on the Admin Panel: Users, Campaigns, and District progress.</p>

## Users

Add, edit, or delete users. Each user has a username, display name, role (**user** or **admin**), and password. Users aren't scoped to specific districts — any authenticated user can work on any district. The built-in `admin` account can't be deleted.

## Campaigns

Create new campaigns (name + optional description) and toggle each one **Active**/**Inactive** from the campaign table. **Manage districts** is where each campaign's districts and their scope are configured — see below.

### Manage districts

![The Manage districts modal for a campaign, showing the Campaign extent URL field at the top, and a scrollable list of districts grouped by region, each with a checkbox and Full district / Partial (paint scope) radio buttons](images/admin-manage-districts-modal.png)
<p class="img-caption">Every district in the country is listed here — check the ones assigned to this campaign, and set each one Full or Partial.</p>

The **Campaign extent URL** field is the outer boundary for this campaign as a whole — an admin sets it once per campaign, as a URL to a feature service, and it defines the outer bound of scope for every district in that campaign that's set to Partial.

For each district assigned to the campaign, choose:

- **Full district** — the entire district's geography is in scope. The [Campaign Scope](campaign-scope) stage is skipped entirely for this district — its full boundary is used as-is and goes straight to [Facilities](facilities).
- **Partial (paint scope)** — adds the [Campaign Scope](campaign-scope) stage for that district, between Landmarks and Facilities. When that stage is painted, the starting canvas is clipped to this district from the campaign extent URL above, where available, or the whole blank district otherwise — from there it's the campaign team that paints in/out the actual in-scope boundary by hand.

### Carry forward

When you assign a district to a campaign for the first time, if that district has a published health/team area map in some *other* campaign, you'll be offered the option to carry that work forward — bringing in the most recent published health area map (and any current team area maps) and marking them current in the new campaign too. Districts with nothing to carry forward from just start blank.

## District progress

A table of every district in the selected campaign with its current status. **Click a row** to open the District Review modal for that district.

![The Belet Weyne District Review modal, showing a boundary preview map colored by health area, a Population & team targets table, and a Health area versions table with two rows — one Current (Unshare/Archive) and one not (Make current/Archive)](images/admin-district-review-modal.png)
<p class="img-caption">This is where an admin sees every version anyone has worked on for a district, and controls which one is actually published.</p>

The modal shows:

- A boundary preview map of the current health-area version.
- Population and team targets for that version (WorldPop Population, Field Target Population, Recommended Teams, Field Requested Teams — per health area).
- Full health-area version history, one row per version, with:
  - **Owner** — who created that version.
  - **Current** — whether this is the version currently published and live in the tool.
  - **Make current** — publishes this version instead, replacing whichever one was current. Only shown on versions that aren't already current.
  - **Unshare** — un-publishes the current version, taking it back out of "current" status without deleting it. This is the action needed before a different health-area version can be published for a district whose team areas are locked (see the 🔒 locked indicator on [Introduction](dashboard)) — unsharing first, then publishing the new version, avoids orphaning team-area work built on the old boundary.
  - **Archive** — retires a version so it's no longer offered in the version picker, without deleting its data outright.
- Full team-area version history, broken out by health area, with the same Owner/Current/Make current/Unshare/Archive controls.

In short: any user can draft and submit a version, but it's the admin, from here, who decides which submitted version actually counts as the district's official, current one.

## Generation settings

![The Generation settings table, showing target_pop_per_health_area (4000.00, "Target under-5 population per health area") and target_pop_per_team (800.00, "Target under-5 population per team area"), and the Data source URLs section below it with Subdivisions, Settlement extents, and IDP settlements source URL fields](images/admin-generation-settings.png)
<p class="img-caption">These two settings are what the Recommended Teams / Recommended Sites calculations on other tabs are based on.</p>

An editable table of campaign-wide settings that drive the recommended-count calculations seen elsewhere in the tool — **target_pop_per_health_area** (used on [Facilities](facilities)'s Recommended sites figure and [Health Areas](health-areas)'s Recommended Teams) and **target_pop_per_team** (used on [Team Areas](team-areas)'s Recommended teams). Edit values directly in the table, then **Save changes**.

## Data source URLs

Global default URLs for external data feeds:

- **Subdivisions source URL**
- **Settlement extents source URL** — WHO-defined built-up-area extents shown as context on maps; this never affects scope, propagation, or population figures.
- **IDP settlements source URL**

Health-facility (ODK/Kobo) endpoints are configured outside the tool (in the server's `.env` file), not here. The **campaign extent URL** is *not* set globally — it's configured per campaign, in each campaign's own Manage Districts modal (see above).

## Access

If you don't see an Admin tab, you don't have admin access — reach out to whoever manages the tool for your team if you need something changed here.
