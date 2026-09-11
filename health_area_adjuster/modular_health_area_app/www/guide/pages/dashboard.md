# Campaign Dashboard

The Introduction tab is your home base — pick a campaign, then use it to navigate into whichever district (or health area, for team areas) needs work, and get a rough sense of overall progress along the way.

![The Campaign Dashboard, showing the status cards, region-grouped district table, and shaded campaign map](images/dashboard-overview.png)
<p class="img-caption">The dashboard for a campaign with 121 districts, most not yet associated.</p>

## Picking a campaign

Use the campaign selector at the top right. Everything else on this page (and the rest of the tool) is scoped to whichever campaign is currently selected. The status cards and shaded map summarize where the selected campaign's districts stand — not something you need to interact with to get your work done.

## Navigating to work

The district table, grouped by region, is how you actually get into a district's work:

- Click **Health Areas** on a district's row to work on that district's health-area boundaries.
- Click **Team Areas** on a district's row to work on team areas — since team areas are mapped one health area at a time (see [Team Areas](team-areas)), you're first asked which of the district's health areas to open.
- The **Team Areas** column shows **"Needs health areas first"** (greyed out) until that district's health areas are actually mapped — team areas can't start before that.

Either path leads to the same kind of choice:

![The version-picker modal, showing "Continue with current", a dropdown of prior drafts, and "Start blank"](images/dashboard-version-picker.png)
<p class="img-caption">Opened via a district's Health Areas or Team Areas button.</p>

- **Continue with current** — the version currently published and live in the tool, shown with who published it and when.
- **Continue a previous version** — pick a specific earlier draft from the dropdown (e.g. your own unpublished work) and resume it.
- **Start blank** — begin from nothing, ignoring any existing version.

Picking one of these takes you into the corresponding tab — [Health Areas](health-areas) or [Team Areas](team-areas) — with that version loaded and ready to work on. Submitting there creates a new version rather than overwriting anything; it's an admin, from the [Admin](admin) panel, who decides which submitted version actually becomes "current."

## Locked

A **🔒 locked** label next to a health-area count means at least one team area has been published for that district. While that's true, the health-area boundaries themselves are locked — a different health-area version can't be published without an admin first explicitly un-sharing the team-area work, since that would otherwise orphan team areas built on a boundary that no longer exists (see [Admin](admin) for how that's un-done).

![A district row showing "1 of 4 health areas · locked" under Team Areas](images/dashboard-locked-indicator.png)
<p class="img-caption">Hodan's Team Areas column shows the locked indicator — one health area already has a published team area.</p>
