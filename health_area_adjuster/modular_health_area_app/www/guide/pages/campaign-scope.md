# Campaign Scope

![The Campaign Scope tab, mid-painting: brush controls on the left, the district canvas with an Out of Scope area painted around the edge, legend and population table on the right](images/campaign-scope-painting.png)
<p class="img-caption">Painting Out of Scope with a 2,000m brush — In Scope population (14,450) and Out of Scope (0) are still showing the saved state, not this unsaved paint.</p>

This tab only applies to districts an admin has marked as **partial coverage** (see [Admin](admin) for where that's decided). If your district is fully covered, you won't see this tab and go straight to [Facilities](facilities).

## What you're deciding

Which parts of this district are actually in scope for this campaign. **Everything left Out of Scope is excluded from facilities, health areas, and every population figure downstream** — this isn't a cosmetic distinction, it's the actual operational boundary the rest of the workflow builds on.

The canvas starts with everything marked **In Scope** by default — you're painting to carve *out* the parts that don't belong, not painting *in* from a blank canvas.

## Painting and refining

Pick **In Scope** or **Out of Scope**, then paint and refine the boundary using the same brush-and-vertex mechanics used on every mapping tab — see [Mapping Mechanics](mapping-mechanics) for the full rundown of the brush, the smoothing sliders, and vertex editing. **Save** to confirm painting, **Save Refinements** to confirm refining, then **Submit** when done.

A couple of display options specific to this tab:

- **Show WorldPop U5 Population** — toggles the population-density overlay on the map.
- **Boundaries only** — a display option, likely to show outlines without the filled color, for a clearer view while painting.

![The same boundary after refining, now a smooth curve instead of a jagged edge](images/campaign-scope-refine-smoothed.png)
<p class="img-caption">A painted edge after refining (Smoothness 6 / Stiffness 14 / Snap tolerance 20%) — see Mapping Mechanics for what these sliders do.</p>

## The legend and population table

The legend on the right shows every color used on the map: Out of Scope, Other Health Areas, Inaccessible, Unpopulated, IDP Settlement, and Settlement Extent.

Below it, a table shows the WorldPop under-5 population currently sitting in each scope category (**In Scope**, **Out of Scope**, and the **District Total**) — click a row to select that category for painting, which is the same effect as using the In Scope/Out of Scope radio buttons above the map. Watching this table update as you paint is a fast way to sanity-check that your scope decision matches the population you expect to actually include.

## Submitting

**Submit Campaign Scope** saves scope to the database and continues to [Facilities](facilities).
