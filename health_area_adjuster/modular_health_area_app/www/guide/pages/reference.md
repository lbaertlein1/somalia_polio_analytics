# Glossary & Troubleshooting

## Glossary

| Term | Meaning |
|---|---|
| Health area (DFA) | A named area assigned to a health facility for campaign planning. |
| Team area | A subdivision of a health area, sized for one vaccination team to cover. |
| SIA | Supplementary Immunization Activity — the campaign type this tool supports. |
| Coordination site | A health facility marked as an active site for this campaign — see [Facilities](facilities). |
| IDP settlement | An internally displaced persons settlement location. |
| Inaccessible / Unpopulated | Special painting categories for areas with no real population or that can't be reached — not ordinary health areas. |
| WorldPop | The under-5 population raster dataset used for population estimates throughout the tool. |

## Map legend colors

These are used consistently across the health area, team area, and export maps:

- **Teal dot** — SIA coordination site (health facility)
- **Red dot** — IDP settlement
- **Black/neon-blue line** — health area boundary (neon blue is used automatically on satellite/topo basemaps for visibility)
- **Dark grey line** — team area boundary
- **Purple dashed line** — subdivision
- **Teal dashed line** — settlement/urban extent

## Troubleshooting

**A health area I need isn't showing up in Team Area Mapping.** It needs to be submitted in [Health Area Mapping](health-areas) first — only submitted health areas are available there.

**The printable export failed for one page but the rest worked.** A per-page render failure shows a "Map could not be rendered for this page" placeholder rather than stopping the whole export — the rest of the PDF should still be usable.

**Nothing happens when I try to open a district.** Make sure a campaign is selected first — the district picker is scoped to whichever campaign is currently active.
