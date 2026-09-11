# Getting Started

## What this tool is for

This tool is for digitizing health area and team area boundaries, district by district, for SIA vaccination campaigns.

It suggests coordination sites from the master health facility list, and health area boundaries from population distribution ([WorldPop](https://www.worldpop.org)) and satellite imagery. WorldPop estimates also drive each area's recommended team count and population total. Every suggestion and boundary can be revised manually within the tool.

A **SIA coordination site** oversees a single health area, coordinating the outreach teams working within it — teams assemble there, collect supplies, and report back. In most cases a coordination site is a health facility, though not always, and not all health facilities will be SIA coordination sites (see [Facilities](facilities)).

It does **not** cover team daily route planning — the most granular level of microplanning, below team areas — which stays outside this tool.

## How this fits into field microplanning

This tool doesn't produce a final product on its own — it works together with field microplanning, in both directions:

- Initial health area boundaries need to exist *before* field-level microplanning can start, since coordinators need something concrete to plan against.
- Those same boundaries — health areas and team areas alike — often change *through* that same process, as district and health-facility-level SIA coordinators weigh in, and as population and facility data gets updated.

In practice this plays out as an iterative loop: map a district, get it in front of the people running the campaign on the ground, revise based on what comes back, and repeat as needed — rather than a one-time draw-it-and-done step.

## How boundaries get generated

Health areas and team areas can be generated automatically rather than painted from a blank grid, using how vaccination teams actually operate: teams are based at a coordination site and travel out from there, door to door.

- **Health areas** grow outward from every SIA coordination site at once, across the whole district. The main driver of how territory splits between sites is population distribution — specifically the WorldPop under-5 population estimate at 100m resolution — so that each site's area ends up carrying a proportionate population load. Terrain and travel conditions (roads, rivers, bridges, slope, land cover) shape this further: a location is assigned to whichever coordination site's team can practically reach it, rather than whichever is geographically closest. A river without a bridge, or steep terrain, makes an area harder to reach from a site even when it's close as the crow flies.

- **Team areas** work the same way, one level down, starting from seed points within a health area instead of the whole district. Those seeds are placed using the health area's own WorldPop population distribution, so that once areas grow out from them, each team ends up with a roughly even population load — not just an even share of land.

- **Population estimates** are calculated for any boundary, generated or hand-drawn, and feed the recommended team counts and summary tables used throughout the tool.

Generated boundaries are a starting point for review and revision, not a final answer — in practice, most boundaries end up being some combination of generated and hand-adjusted.

## The workflow

For each district, work through these steps in order:

1. **[Landmarks](orientation)** — add reference landmarks and review context before mapping.
2. **[Campaign Scope](campaign-scope)** *(partial-coverage districts only)* — paint exactly which area is in scope.
3. **[Facilities](facilities)** — mark which facilities are active coordination sites this round.
4. **[Health Area Mapping](health-areas)** — generate or draw, then submit, health area boundaries.
5. **[Team Area Mapping](team-areas)** — generate or draw, then submit, team-level areas within each health area.
6. **[Export](export)** — pull boundary data and printed maps for field microplanning discussions, or for a finalized round.

The [Campaign Dashboard](dashboard) tracks where every district stands, so this can be picked up, revised, and handed off across sessions without losing track of what's done.

## If you get stuck

See [Glossary & Troubleshooting](reference) for common terms and known issues.
