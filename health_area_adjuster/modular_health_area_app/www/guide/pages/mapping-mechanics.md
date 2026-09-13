# Mapping Mechanics

Painting and refining boundaries works the same way on every mapping tab — [Campaign Scope](campaign-scope), [Health Areas](health-areas), and [Team Areas](team-areas). This page is the one place that walks through those shared mechanics in full detail, so each tab's own page can stay focused on what's specific to it and link back here instead of repeating the whole explanation.

The general idea behind the two-step process: painting gives you a **coarse**, rough boundary quickly, covering ground fast without worrying about precision. Refining then gives you **fine** control to turn that rough shape into something precise — smoothed automatically, or adjusted by hand, or both. Every boundary in the app goes through painting first; refining is optional but recommended before submitting.

## Navigating the map

Before painting anything, it helps to get comfortable moving around the map itself:

- **Pan** — right-click and drag to move the map around.
- **Zoom** — use the scroll wheel to zoom in and out.

**Use a mouse rather than a laptop trackpad** for this tab if you can. Painting relies on precise click-and-drag strokes and right-click panning, both of which are noticeably harder to control accurately on a trackpad than with a mouse.

## Step 1: Painting

![The Campaign Scope tab mid-painting: the Brush Diameter slider set to 2,000m on the left, an Out of Scope area painted around the edge of the district in progress, and the Undo / Reset / Save buttons below the brush controls](images/campaign-scope-painting.png)
<p class="img-caption">Painting in progress — the brush controls, category selection, and Undo/Reset/Save buttons look the same on every mapping tab.</p>

### Choosing what you're painting

Every mapping tab has a table on the right listing the things you can paint into — categories on Campaign Scope (In Scope / Out of Scope), health areas plus the Inaccessible/Unpopulated categories on Health Areas, or individual teams on Team Areas. **Click a row in that table to select it** — the selected row highlights, and whatever you paint next gets assigned to it. You can only paint into one selection at a time; switching your paint target means clicking a different row first.

### The brush

**Brush Diameter** is a slider controlling how large an area each click-and-drag stroke covers, measured in meters. The exact range differs by tab (Campaign Scope and Health Areas go up to 10,000m; Team Areas tops out lower, since team areas are smaller). A few practical points:

- A **larger brush** covers ground quickly — good for the bulk of a shape, when you don't need precision yet.
- A **smaller brush** gives finer control — switch down when you're working close to an edge you don't want to cross, like a district boundary, a river, or another area's territory.
- The brush size can be changed at any point mid-painting; there's no need to commit to one size for an entire session.

### Painting itself

Click and drag on the map to paint. Every cell your brush passes over while the mouse button is held gets assigned to whatever's currently selected — painting into a cell that already belongs to something else reassigns it. Painted shapes are deliberately blocky at this stage: a jagged, pixel-grid edge is normal and expected, not a mistake to fix by hand while painting. That cleanup is what refining (below) is for.

- **Undo** steps back one paint action (one click-and-drag stroke) at a time.
- **Reset** clears everything back to the starting state — use this to start over completely, not for undoing a single stroke.
- **Save** confirms your painting without submitting. Saving lets you close the tab and come back to exactly where you left off later; nothing is lost until you explicitly reset it.

## Step 2: Refining

Once a rough shape is painted, click **Refine Boundaries** to switch into vertex-editing mode (**Back to Painting** returns you to Step 1 if you need to paint more first). Refining offers two ways to clean up a boundary, and most people use both together: automatic smoothing via three sliders, and direct manual adjustment by dragging individual points.

### The smoothing sliders

- **Smoothness** — how much the jagged, painted edge gets rounded off. Higher values produce a more visibly curved, less blocky line.
- **Stiffness** — how closely the smoothed line still has to follow the original painted shape. Higher stiffness keeps the refined line close to what was actually painted; lower stiffness allows more deviation in exchange for a cleaner-looking curve.
- **Snap tolerance** — how aggressively nearby points snap together when cleaning up the line. Useful for closing small gaps between segments or eliminating stray jitter left over from painting.

There's no single correct setting for these three — they interact, and the right combination depends on how clean the underlying paint job already was. Adjusting one slider updates the boundary live, so it's easiest to nudge each one and watch the effect rather than guessing values up front.

**Clean Up Boundaries** runs an automatic pass on top of whatever the three sliders produce — removing small leftover artifacts, self-intersections, and similar noise that sliders alone don't always catch.

### Manual vertex editing

Beyond the automatic sliders, refine mode also shows the boundary as a line studded with small draggable circular handles — one at each vertex. Dragging a vertex directly gives precise, manual control that no slider combination can replicate exactly — most commonly used to line a boundary up with something specific on the map, like making it follow a road or a river instead of cutting across it. Zooming in makes it much easier to grab and place an individual vertex precisely, since vertices can sit close together once a line has been smoothed.

![Refine Boundaries mode zoomed into a section of the map, showing a boundary line with draggable vertices being adjusted to follow Belet Weyne-Taageelow Road, with Smoothness at 4, Stiffness at 17, and Snap tolerance at 20%](images/health-areas-vertex-refine-road.png)
<p class="img-caption">Dragging a vertex to snap a boundary to a road, rather than cutting across it — the sliders and vertex handles both stay active throughout Step 2.</p>

A practical workflow that works well: adjust the three sliders first to get the overall shape smoothed out, then zoom into any specific spots that still need correcting and drag individual vertices by hand.

**Save Refinements** confirms the smoothed and/or vertex-edited result, the same way **Save** does for painting in Step 1. Refining is optional — a painted-but-unrefined boundary can still be submitted — but a refined one is easier for others to read on a map and easier to align with real landmarks.

## Where this shows up

<div class="mechanics-box">
<span class="mechanics-label">Quick recap</span>
<p><strong>Paint</strong> a rough shape by selecting a row, sizing the brush, and dragging on the map. <strong>Refine</strong> it with the Smoothness/Stiffness/Snap tolerance sliders, Clean Up Boundaries, and/or by dragging individual vertices. <strong>Save</strong> (or <strong>Save Refinements</strong>) to keep your work without submitting.</p>
</div>

- [Campaign Scope](campaign-scope) — painting/refining In Scope vs. Out of Scope.
- [Health Areas](health-areas) — painting/refining individual health areas, plus the Inaccessible and Unpopulated categories.
- [Team Areas](team-areas) — painting/refining individual teams within a health area.
