# Export

![The Export tab with a campaign selected, showing the Boundary export section (Scope, Format, Prepare download) and the Printable maps section below it (District picker, Basemap, zoom, page dimensions, and an empty preview panel)](images/export-overview.png)
<p class="img-caption">Two separate export tools on one tab: Boundary export for data files, Printable maps for PDFs.</p>

Downloads current, published boundaries and data. For a specific past version or a draft, an admin can pull it from the Admin panel instead — this tab only exports what's currently live.

## Picking a campaign

Use the **Campaign** dropdown at the top. Everything below is scoped to whichever campaign is selected, the same as elsewhere in the tool.

## Boundary export — data files

Downloads the actual GIS boundary data for either the whole campaign or a single district.

- **Scope** — **Whole campaign** exports every health area and team area mapped so far across the entire campaign, not just one district. **Single district** narrows it to one.
- **Format** — GeoJSON, Shapefile, or KML.

Along with the boundary geometry itself, the export includes the underlying data attached to each area — population figures, IDP settlements, and team counts — not just the shapes.

Click **Prepare download** to generate the file.

## Printable maps — PDF map books

Generates a PDF for one district, sized for A3/A4 landscape printing. From a real generated example (Belet Weyne, 4 health areas), the book runs:

1. **District overview page** — the whole-district map plus stat cards for Health Areas, IDP Settlements, Target Population, and Teams, shown in the live preview below.
2. **Health Area Summary table page** — one row per health area, listing Target Pop (WorldPop), Target Pop (Field, if a field figure was entered), IDP Settlements, Recommended Teams, and Field Requested Teams.
3. **One page per health area** — a map zoomed to just that health area's boundary, its own stat cards (IDP Settlements, Target Population, Teams), and, if that health area's team areas have been mapped, the Team Area Boundary layer drawn on the map alongside the Health Area Boundary, plus a **Team Summary table** below it (one row per team, same Target Pop WorldPop/Field columns as the health area summary).

- **District** — pick which district's map book to generate.
- **Basemap** — None, OpenStreetMap, Satellite, or Topo.
- **Basemap zoom level** — only relevant once a basemap other than None is selected.
- **Page dimensions (inches)** — set width and height directly.
- **Include WorldPop population overlay** — optionally shade the map with the WorldPop layer.

### Preview

The preview panel on the right shows the district overview page and updates as you change the settings above, so you can see roughly what the PDF will look like before generating it. It includes the district name and campaign banner, the boundary map itself with a legend (SIA Coordination Site, IDP Settlement, Health Area Boundary, Subdivision, Urban Extent), and summary stat cards for Health Areas, IDP Settlements, Target Population, and Teams.

![The Printable maps preview for Danyile district, showing the district overview map with health area boundaries and coordination sites, a legend, and stat cards reading 15 Health Areas, 0 IDP Settlements, 55,787 Target Population, and 77 Teams](images/export-printable-preview.png)
<p class="img-caption">The live preview for Danyile's overview page, updating as basemap and dimension settings change.</p>

### Generating the PDF

Once you're happy with the settings, click **Generate PDF** to build the full map book and download it.

Every page carries a footer with the production date, a disclaimer that figures are for campaign planning purposes only (not official statistics), and its data sources (OpenStreetMap, WorldPop, the IDP dataset, and MFHL).
