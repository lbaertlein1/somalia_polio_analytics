# Health Areas

![The Health Areas tab with three health areas already painted in distinct colors (pink, teal, purple) covering most of the district, a hatched pattern over the whole district indicating no boundaries submitted yet, brush diameter set to 5,000m, and the Legend & Population table on the right listing each health area's WorldPop U5 population plus Inaccessible/Unpopulated/District Total rows](images/health-areas-overview.png)
<p class="img-caption">Rough painting in progress — each health area gets its own color as you paint. The hatched texture means these boundaries haven't been submitted yet.</p>

Each health area is coordinated by one outreach coordination site (see [Facilities](facilities)), ideally covering **~4,000 children with ~5 outreach teams**. This tab is where you draw and refine each area's boundary.

## Starting from a generated boundary

Rather than painting every health area from a blank grid, boundaries can generate automatically with a brief animated reveal: each health area propagates outward from its SIA coordination site across the district — this is a door-to-door outreach campaign, so health workers travel out *from* the site, not the other way around. The main driver of how territory splits between sites is population distribution — the WorldPop under-5 estimate at 100m resolution — so each site's area ends up carrying a proportionate population load, not just an even share of land. Terrain and travel conditions (roads, rivers, slope, land cover — viewable via **Show Friction Surface**) shape this further, so a location goes to whichever site's team can practically reach it rather than whichever is closest as the crow flies.

Treat the generated result as a starting point, not a final answer — it's then adjusted by the group using the painting and refining tools below.

## Selecting a health area to paint

Clicking a row in the **Health Area** table on the right selects that health area — its row highlights, and the map draws a colored fill over whatever cells are currently assigned to it (yellow in the screenshots below, though the actual saved color may differ once multiple areas are visible at once).

![The Siigalow /hilaah Health Center row selected in the table, highlighted yellow, with its corresponding area on the map also shaded yellow while every other health area shows as plain grey](images/health-areas-select-row.png)
<p class="img-caption">Selecting a row highlights that health area both in the table and on the map.</p>

## Painting and refining

Painting and refining use the same brush, sliders, and vertex tools as every other mapping tab — see [Mapping Mechanics](mapping-mechanics) for the full rundown. **Save** to confirm painting, **Save Refinements** to confirm refining, then **Submit** when done.

### Marking Inaccessible and Unpopulated areas

**Inaccessible** and **Unpopulated** are special categories, not real health areas — select either one from the table the same way you'd select a health area, then paint it the same way. Use them for areas that genuinely can't be reached or genuinely have no population, not as a placeholder for "not sure yet."

Once saved, a painted **Inaccessible** area shows filled and outlined in red on the map, and its row in the table updates from 0 to the actual population count that's now excluded.

![An eastern area saved as Inaccessible, shown filled and outlined in red on the map, with the Inaccessible row in the table updated to reflect the population now excluded](images/health-areas-inaccessible-saved.png)
<p class="img-caption">Inaccessible area saved — filled red on the map.</p>

**Unpopulated** works the same way: select it, paint over the areas with no population, and save. Saved Unpopulated cells show as small white gaps cut out of the surrounding health area's color.

![Hiran Regional Hospital's health area selected and shown in yellow, with white Unpopulated gaps and the red Inaccessible area both visible on the map, and the table now showing non-zero population figures for both Inaccessible and Unpopulated](images/health-areas-unpopulated-saved.png)
<p class="img-caption">With Inaccessible and Unpopulated both painted, the table's Inaccessible and Unpopulated rows show the population figures now excluded from every health area.</p>

## Submitting

**Submit Health Areas** saves boundaries to the database. Submitting creates a new **version** of this district's health areas — older versions stay in history, and an admin can review or roll back to a prior version if needed. Once submitted, a health area's boundary is locked for [Team Areas](team-areas) to be based on.

## Team Planning Targets

Each health area's row shows its **WorldPop U5 Population** — the estimated under-5 population inside that area's boundary, from the WorldPop dataset. From this, the tool calculates a **Recommended Teams** figure for each health area, based on that population and the population-per-team setting configured on the [Admin](admin) tab.

The **Team Planning Targets** dialog lets you override this for individual health areas if the WorldPop estimate or the standard formula doesn't match what's actually needed on the ground:

- **Field Target Population** — enter a different population figure for that health area if the WorldPop estimate needs correcting.
- **Field Requested Teams** — enter a specific number of teams directly, overriding the recommended figure.

Leave either field blank to keep using the calculated default. Click **Done** to close the dialog once you've made any adjustments.

![The Team Planning Targets dialog listing each health area with its WorldPop U5 Population, an empty Field Target Population input, the calculated Recommended Teams figure, and an empty Field Requested Teams input](images/health-areas-team-planning-targets.png)
<p class="img-caption">Both override fields are optional — leaving them blank keeps the WorldPop-based recommendation.</p>

## Context layers

These appear on the map for reference while you paint — none of them are things you edit here:

- **SIA Coordination Site** (teal dot) — health facilities marked as coordination sites on the [Facilities](facilities) tab.
- **IDP Settlement** (red dot) — displaced-persons settlement locations.
- **Subdivision** (purple dashed line) — existing administrative subdivision boundaries, where available.
- **Settlement Extent** (teal dashed line) — the built-up area boundary, where available.
- **Show Friction Surface** — an optional overlay of the terrain/travel-conditions surface (roads, rivers, slope, land cover) used when generating starting boundaries.
- **Show WorldPop U5 Population** — an optional shaded layer showing estimated under-5 population density.
- **Boundaries only** — a display option to show outlines without the filled color, for a clearer view while painting.
