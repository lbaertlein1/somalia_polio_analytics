# Boundary clean-up scripts (campaign 10, published layers)

Working scripts used to clean the published health areas and team layouts directly in the database. They have not been standardized yet.

Run them from the app folder (`modular_health_area_app`) with Rscript, with the SSH tunnel open (localhost:15432). Every script needs `WORK`, a folder for outputs and backups that sits outside the repo.

| Script | What it does | Writes to DB? |
|---|---|---|
| `clean_lib.R` | Shared geometry code: `clean_partition()` (clip spill, resolve overlaps, move thin spikes and small islands, fill gaps by longest shared border), `finalize_partition()` (1 cm snap, valid on plane and sphere, no re-introduced overlaps), metrics | – |
| `scan_published.R` | Gaps, spill, overlaps and islands for every published health-area version and team layout | no |
| `check_s2_published.R` | Sphere validity (s2) of every published team layout (invalid shapes make the app drop teams) | no |
| `clean_published.R` | Dry run: cleans health areas and re-fits each published team layout to its health area; writes `WORK/clean_out/<district>.rds` and lists anomalies. With `ROUTE_DIR`, the health areas come from `street_follow.R` instead | no |
| `apply_cleaned.R` | Writes the dry-run results (saved and smoothed shapes, WorldPop, painted cells) after guards: version unchanged since the dry run, still published, change limits, partition and s2 checks. Backs up to `WORK/clean_out/backup_*.rds` | `APPLY=1` |
| `street_follow.R` | Street-following proposal: each boundary between two health areas is re-routed along the cheapest OpenStreetMap street/track/river route between its ends (main roads preferred, kept near the current line) | no |
| `street_follow_review_html.R` | One Leaflet HTML per district: current (red) vs proposal (blue dashed), land changing hands, WorldPop table | no |
| `merge_scope_draft.R` | Puts a detailed In-Scope draft (e.g. a town) into the published version: draft areas inside its In Scope outline, published areas outside, published spelling, sites and figures merged; saved as the owner's new draft (and published with `PUBLISH=1`) | `APPLY=1` |
| `unpublish_team_layouts.R` | Unpublishes all team layouts of district `D` so its health areas can be republished | yes |

## Typical runs

```sh
export WORK=/path/outside/repo
# clean everything published
Rscript scripts/cleanup/clean_published.R            # ONLY="Karan,Hodan" to limit; SNAP=1 to snap to 1 cm first
Rscript scripts/cleanup/apply_cleaned.R              # check only
APPLY=1 Rscript scripts/cleanup/apply_cleaned.R

# street-following boundaries
ONLY="Karan" Rscript scripts/cleanup/street_follow.R
DISTRICTS="Karan" OUTDIR=boundary_review Rscript scripts/cleanup/street_follow_review_html.R
ROUTE_DIR=$WORK/route_out ONLY="Karan" Rscript scripts/cleanup/clean_published.R
MAX_HA_CHANGE=15 MAX_TEAM_CHANGE=60 APPLY=1 Rscript scripts/cleanup/apply_cleaned.R
```

`apply_cleaned.R` applies every `.rds` in `WORK/clean_out`, so use a fresh `WORK` for each batch.

Backups contain the previous shapes. Keep them outside the repo.
