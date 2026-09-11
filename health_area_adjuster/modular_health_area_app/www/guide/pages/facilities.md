# Facilities

![The Facilities tab with no coordination sites selected yet: the map shows grey pins for every facility, a yellow highlighted pin for a facility being added as a non-facility site, and the right-hand table listing all health facilities with empty Coord. Site checkboxes](images/facilities-select-nonfacility.png)
<p class="img-caption">Before selecting: Recommended shows 4, Selected shows 0. The highlighted row/pin is a facility about to be marked.</p>

This tab is where you identify **outreach coordination sites** — the facilities where vaccination teams assemble, collect supplies, and report back. One coordination site anchors one health area, so this list is what [Health Areas](health-areas) and [Team Areas](team-areas) build on.

## The recommended number of sites

The **Recommended** card is calculated automatically: `ceil(district population ÷ target population per health area)`. That target (4,000 in the screenshot below) isn't hard-coded — it's the `target_pop_per_health_area` setting on the [Admin](admin) tab, so it can be changed campaign-wide. For a district of 13,739 people at the default 4,000, that's 4 recommended sites. This is a planning target, not a hard limit — the **Selected** card next to it just counts how many you've actually checked so far, and there's nothing stopping that number from landing above or below the recommendation if the facilities on the ground call for it.

## Selecting a facility as a coordination site

Tick the checkbox in the **Coord. Site** column next to a facility's name in the **All Health Facilities** table. Once checked:

- The facility's pin on the map turns from grey to **green**.
- The facility moves up into the **Outreach Coordination Sites** table at the top of the right-hand panel, which lists only the sites you've selected so far.
- The **Selected** count updates.

Unchecking it removes the facility from the Outreach Coordination Sites table and turns its pin back to grey.

Clicking a facility's pin on the map opens a small info popup (Type, Ownership, Incharge, Coord. Site) so you can check the details before deciding.

![The Facilities tab after four sites have been selected: their pins are green and they're listed in the Outreach Coordination Sites table, with a popup open on Shacab Health Center showing its Type, Ownership, Incharge, and Coord. Site fields](images/facilities-selected-sites.png)
<p class="img-caption">Four sites selected — Selected now reads 4, matching Recommended. The open popup shows a facility's details before you decide.</p>

## Adjusting a site's location

If a facility's plotted GPS location is off, drag its pin on the map to the correct spot. This corrects the location used for that facility going forward — it doesn't affect any other facility's position.

## Renaming a facility

Rename a facility directly in the **Facility Name** column of the table.

## Adding a non-facility site

Use **Add Non-Facility Site** when the right coordination site for an area isn't an existing health facility — for example, if there's no eligible facility nearby, or the facility that would normally serve the role isn't suitable (no cold storage, no assembly space, etc.) and the team is coordinating from somewhere else instead, like a school or a community building.

Click the button, then click on the map to place the new site. It's added the same way a regular facility is, and you check its Coord. Site box the same way to mark it as an active coordination site.

## What to consider when choosing sites

When deciding which facilities to select, take into account:

- Location and the population it would need to cover (catchment population).
- Cold storage and electricity availability.
- Assembly space for teams to gather and collect supplies.
- Whether there's an outreach coordinator or supervisor available at that site.

## Submitting

**Show WorldPop U5 Population** overlays the population-density layer on the map, the same toggle used on other tabs. **Submit** saves your coordination site selections (and any IDP settlement data) to the database. **Continue →** moves on to [Health Areas](health-areas).
