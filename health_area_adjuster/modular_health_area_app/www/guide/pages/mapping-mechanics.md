# Mapping Mechanics

Painting and refining boundaries works the same way on every mapping tab — [Campaign Scope](campaign-scope), [Health Areas](health-areas), and [Team Areas](team-areas). This page covers the shared mechanics once, so each tab's own page can focus on what's specific to it.

The general idea: painting gives you a **coarse**, rough boundary quickly; refining then gives you **fine** control to clean that rough shape up into something precise.

## Painting: coarse adjustment

- Select what you're painting from the table on the right — a category or area (In Scope/Out of Scope on Campaign Scope; a specific health area, or the Inaccessible/Unpopulated categories, on Health Areas; a specific team on Team Areas). Whatever's selected is what your brush paints into.
- **Brush Diameter** — a slider controlling brush size in meters (range varies by tab). A larger brush covers ground fast; a smaller one gives finer control near an edge, useful when painting close to something you don't want to cross.
- Click and drag on the map to paint. Painted cells are deliberately blocky at this stage — a jagged, pixel-grid edge is expected, and doesn't need cleaning up by hand while painting.
- **Undo** steps back one paint action. **Reset** clears everything back to the starting state. **Save** confirms your painting without submitting — you can keep working after saving, and come back to it later.

## Refining: fine adjustment

Once a rough shape is painted, **Refine Boundaries** switches into vertex-editing mode (**Back to Painting** returns to painting). Three sliders control automatic smoothing:

- **Smoothness** — how much the jagged, painted edge gets rounded off.
- **Stiffness** — how closely the smoothed line still has to follow the original painted shape. Higher keeps the refined line closer to what was actually painted; lower lets it deviate more in the name of a cleaner curve.
- **Snap tolerance** — how aggressively nearby points snap together when cleaning up the line, useful for closing small gaps or eliminating stray jitter.

**Clean Up Boundaries** runs an automatic pass on top of the sliders — removing small artifacts, self-intersections, and similar noise.

Beyond the automatic sliders, refine mode also shows the boundary as a line studded with small draggable vertex handles. Dragging a vertex directly gives precise, manual control — useful for lining a boundary up with something specific on the map, like making it follow a road, rather than relying on the smoothing sliders alone. Zooming in makes it much easier to grab and place an individual vertex precisely.

![Refine Boundaries mode zoomed into a section of the map, showing a boundary line with draggable vertices being adjusted to follow Belet Weyne-Taageelow Road, with Smoothness at 4, Stiffness at 17, and Snap tolerance at 20%](images/health-areas-vertex-refine-road.png)
<p class="img-caption">Dragging a vertex to snap a boundary to a road, rather than cutting across it — the same mechanic works on any mapping tab.</p>

**Save Refinements** confirms the smoothed/vertex-edited result, the same way **Save** does for painting.

## Where this shows up

- [Campaign Scope](campaign-scope) — painting/refining In Scope vs. Out of Scope.
- [Health Areas](health-areas) — painting/refining individual health areas, plus the Inaccessible and Unpopulated categories.
- [Team Areas](team-areas) — painting/refining individual teams within a health area.
