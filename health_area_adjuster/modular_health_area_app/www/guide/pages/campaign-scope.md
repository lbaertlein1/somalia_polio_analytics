# Campaign Scope

![The Campaign Scope tab, mid-painting: brush controls on the left, the district canvas with an Out of Scope area painted around the edge, legend and population table on the right](images/campaign-scope-painting.png)
<p class="img-caption">Painting Out of Scope with a 2,000m brush — In Scope population (14,450) and Out of Scope (0) are still showing the saved state, not this unsaved paint.</p>

Every district goes through this tab between [Landmarks](orientation) and [Facilities](facilities). If an admin has marked your district as **full district** (see [Admin](admin)), the whole district starts In Scope and you only need to paint out anything that's excluded. If it's marked **partial**, the canvas starts from the campaign extent, as described below.

## What you're deciding

Which parts of this district are actually in scope for this campaign. **Everything left Out of Scope is excluded from facilities, health areas, and every population figure downstream** — this isn't a cosmetic distinction, it's the actual operational boundary the rest of the workflow builds on.

The canvas starts with everything marked **In Scope** by default — you're painting to carve *out* the parts that don't belong, not painting *in* from a blank canvas.

## Painting and refining

Pick **In Scope** or **Out of Scope**, then paint and refine the boundary using the same brush-and-vertex mechanics used on every mapping tab. **Save** to confirm painting, **Save Refinements** to confirm refining, then **Submit** when done.

<div class="mechanics-box">
<span class="mechanics-label">Mapping mechanics</span>
<p>Select a category, size the brush, and drag on the map to paint a rough shape. Then use the Smoothness/Stiffness/Snap tolerance sliders, Clean Up Boundaries, or drag individual vertices to refine it. See <a href="mapping-mechanics">Mapping Mechanics</a> for the full walkthrough, with screenshots of both steps.</p>
</div>

A couple of display options specific to this tab:

- **Show WorldPop U5 Population** — toggles the population-density overlay on the map.
- **Boundaries only** — a display option, likely to show outlines without the filled color, for a clearer view while painting.

![The same boundary after refining, now a smooth curve instead of a jagged edge](images/campaign-scope-refine-smoothed.png)
<p class="img-caption">This tab's boundary after refining (Smoothness 6 / Stiffness 14 / Snap tolerance 20%).</p>

## The legend and population table

The legend on the right shows every color used on the map: Out of Scope, Other Health Areas, Inaccessible, Unpopulated, IDP Settlement, and Settlement Extent.

Below it, a table shows the WorldPop under-5 population currently sitting in each scope category (**In Scope**, **Out of Scope**, and the **District Total**) — click a row to select that category for painting, which is the same effect as using the In Scope/Out of Scope radio buttons above the map. Watching this table update as you paint is a fast way to sanity-check that your scope decision matches the population you expect to actually include.

## Submitting

**Submit Campaign Scope** first asks for the **In-Scope Field U5**: the field figure for children under 5 in the In Scope area, if you have one (microplan enumeration is the best source). The dialog shows the WorldPop U5 estimate for the same area for comparison.

- **If you enter a figure**, WorldPop is scaled to match it. Health areas and teams then show **Scaled WorldPop U5**, and the Facilities recommendation, recommended teams and exports all use it. WorldPop only decides how your figure is split between health areas and teams.
- **If you leave it blank**, everything stays on WorldPop U5, as before.

It then saves scope to the database and continues to [Facilities](facilities).

Scope locks once Facilities work begins, but the In-Scope Field U5 doesn't: you can add or change it later from the **Population figures** box at the top of the [Health Areas](health-areas) population panel, including for districts whose health or team areas are already mapped.

### Population names

| Name | What it is |
|---|---|
| **WorldPop U5** | The raw WorldPop under-5 estimate. |
| **In-Scope Field U5** | The field figure for the district's In Scope area, entered here. It isn't scaled WorldPop: it's the figure WorldPop is scaled *to*. |
| **Scaled WorldPop U5** | WorldPop for a health area or team, scaled so the district adds up to the In-Scope Field U5. |
| **Field Target U5** | A field figure entered for one health area (Team Planning Targets) or one team. |
| **Target Population** | The figure actually used: Field Target U5 if entered, otherwise Scaled WorldPop U5, otherwise WorldPop U5. |

Areas you later paint **Inaccessible** or **Unpopulated** on [Health Areas](health-areas) are still part of the In Scope area, so they keep their share of the In-Scope Field U5 — the health areas themselves then add up to a little less than your figure. If those areas shouldn't count, paint them Out of Scope here instead.

Field figures stay at the level they were entered: a health area's Field Target U5 doesn't change the district's In-Scope Field U5, and the district's figure doesn't override a health area's own. Exports show WorldPop U5, Scaled WorldPop U5 and Field Target U5 side by side, plus a `scope_population_summary.csv` with each district's WorldPop U5 (In Scope), In-Scope Field U5 and WorldPop scale factor.
