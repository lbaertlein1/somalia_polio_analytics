# Team Areas

This works the same way as [Health Areas](health-areas), but one level down — you're dividing a single, already-submitted health area into team-level territories, one per outreach team, each covering **~800 children**.

**Team areas are mapped one health area at a time.** Each health area gets its own separate team map, worked on and submitted independently — if a district has four health areas, you go through the full process below four times, once per health area, not once for the whole district.

## Getting to a health area's team map

From the [Introduction](dashboard) tab, click the **Team Areas** button on the district's row. Since a district can have several health areas, you're first asked which one to work on:

![The "choose a health area" modal listing Belet Weyne's four health areas, each with an Open button](images/team-areas-choose-health-area.png)
<p class="img-caption">One health area = one team map. Pick which one you're working on.</p>

Clicking **Open** on a health area brings up the same kind of version picker used on the [Introduction](dashboard) tab — **Continue with current** if a team map already exists for that health area, a dropdown of prior drafts, or **Start blank**. Before the map opens, you're shown a confirmation step for the population estimate and team count (see below), then **Continue** takes you into the map itself.

## How team allocation works

Team areas are generated the same way health areas are — by propagating outward across terrain and travel conditions (roads, rivers, slope, land cover) from a set of seed points, not by straight-line distance. The difference is where the seeds come from and what they're trying to balance:

- **Health areas** propagate outward from each SIA coordination site.
- **Team areas** propagate outward from seed points placed using the health area's own population distribution, so that once each team's area has grown outward from its seed, every team ends up with a roughly *even population load* — not just an even share of land.

Before generating, you confirm or adjust the population estimate and team count that this split is based on:

![The "Team planning for Hawa Tako Health Center" dialog, showing a WorldPop estimate of 3,917, an editable Population estimate field, Recommended teams of 5 (calculated as ceil(3,917 ÷ 800)), and an editable Number of teams field](images/team-areas-planning-confirm.png)
<p class="img-caption">Recommended teams = ceil(population estimate ÷ 800 target per team). Both fields are editable before generating.</p>

- **Population estimate (under-5)** — pre-filled from WorldPop, but editable if you have a better field figure.
- **Number of teams** — pre-filled with the recommended count, but editable directly — this is what actually controls how many team areas get generated, regardless of what's recommended.

## Painting and renaming

Same tools as [Health Areas](health-areas), and the same shared brush/refine mechanics as every mapping tab — see [Mapping Mechanics](mapping-mechanics) for the full rundown. Select a team from the table on the right, paint cells onto it with the brush, then refine.

![The Team Areas map for Hiran Regional Hospital, with Team 1, Team 2, and Team 3 already painted, Team 3 selected and highlighted yellow, and the Legend & Population table showing each team's WorldPop U5 population](images/team-areas-painting.png)
<p class="img-caption">Selecting a team's row (Team 3, highlighted) shows its painted area on the map — same interaction as health area painting.</p>

**Renaming a team** — click a team's name directly in the table to rename it, the same way facilities are renamed on the [Facilities](facilities) tab.

![The same team map with "Team 3" renamed to "Shabelle PHU Team" — the renamed team's row is highlighted yellow in the table and its area is still shown yellow on the map](images/team-areas-renamed.png)
<p class="img-caption">Team 3 renamed to "Shabelle PHU Team" — renaming doesn't change which cells belong to the team, just its label.</p>

**Show WorldPop U5 Population** and **Boundaries only** work the same as on other tabs — useful together to see population density under the team boundaries without the fill color obscuring it. **Save** and **Save Refinements** work exactly as they do for health areas — confirming your work without submitting yet.

## Submitting and setting as current

**Submit Team Areas** saves the team-area boundaries for all teams worked on so far to the database. After submitting, you're asked whether to publish this version as the health area's active team map:

![The "Team areas submitted" dialog asking "Set this submission as Hiran Regional Hospital's current team map?" with Not now and Set as current buttons](images/team-areas-set-as-current.png)
<p class="img-caption">Submitting saves the draft either way — Set as current is what actually publishes it.</p>

- **Set as current** — publishes this version as the health area's active team map. This is also what triggers the 🔒 **locked** indicator on the [Introduction](dashboard) tab, since a published team map locks its parent health-area boundary against being swapped for a different version.
- **Not now** — the submission is still saved as a draft you (or someone else) can pick up later from the version picker, but it doesn't become the active team map yet.

Each health area has its own independent team-area version history, separate from every other health area in the district.
