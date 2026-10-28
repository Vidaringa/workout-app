# 5/3/1 training app

A personal R Shiny app for Wendler 5/3/1 (5's PRO): it generates each round from
the training maxes and an editable template, logs sets at the gym from a phone,
stores everything in a Google Sheet, and shows e1RM trends, TM health, running
metrics and an optional weekly review from Claude.

| Tab | What it does |
|---|---|
| Today | Picks the next unfinished session and shows one card per set, pre-filled. Tap ✓ to log as planned; use −/+ to change kg or reps; pick RIR 0–4 on top sets and jokers. Warm-ups and jokers are folded. A rest timer starts after each set. "Log a run" is one short form. |
| Plan | The current round and the projected next round, laid out like the old spreadsheet, plus TM health. **Finish round → generate next round** lives here. |
| Progress | e1RM trend per lift with an uncertainty band, TM / e1RM, goal tracker with a projected date range, weekly volume, accessory progression. |
| Run | Race predictions against your goals, weekly km and ascent, pace and grade-adjusted pace, combined lifting + running load. |
| Review | A coaching note from Claude based on a summary computed in R. Advice only: it never changes the plan. |
| Settings | Training maxes, the main settings, bodyweight, and the template (read from the Sheet; **Refresh** reloads). |

## 1. Local setup

```r
# in R, from the project folder
renv::restore()            # installs the locked package versions
renv::install("testthat")  # only needed to run the tests
```

Copy `.Renviron.example` to `.Renviron` and fill it in (sections 2–5 below),
then restart R and run:

```r
shiny::runApp()
```

Tests: `testthat::test_dir("tests/testthat")`.

## 2. Google Sheet setup (one time)

1. In the [Google Cloud console](https://console.cloud.google.com/), create a
   project and enable the **Google Sheets API**.
2. Under *IAM & Admin → Service accounts*, create a service account, then
   *Keys → Add key → JSON*. Save the file as `secrets/service-account.json`
   (git-ignored).
3. In Google Drive, create a **new, blank Google Sheet**. Don't upload the old
   .xlsx: the app needs a native Sheet, and it creates its own tabs.
   Under *File → Settings*, set the locale to one that uses a decimal point
   (e.g. United Kingdom), so `2.5` stays a number.
4. Share the Sheet with the service account's e-mail address (in the JSON,
   `client_email`) as **Editor**.
5. Put the Sheet ID (the long part of its URL between `/d/` and `/edit`) in
   `GSHEET_ID` in `.Renviron`.
6. Create the tabs and default settings and template:

   ```sh
   Rscript scripts/setup_sheet.R
   ```

The tabs are `settings`, `template`, `training_max`, `sets`, `runs`,
`bodyweight` and `ai_reviews`. Don't rename, reorder or insert columns. You can
edit values, and add rows to `template`. Avoid sorting or deleting rows of
`sets` while the app is open, or press **Refresh** in Settings afterwards.

## 3. Import the old workbook (one time)

```sh
Rscript scripts/import_workbook.R "wendler_2026 (1).xlsx"
```

This writes three files to `data/` (git-ignored) and doesn't touch the Sheet:

- `import_review.csv`: every parsed set next to the original cell text, with a
  `flags` column for anything guessed. Read the flagged rows.
- `import_sets.csv`: the rows for the `sets` tab. Fix anything here by hand.
- `import_training_max.csv`: rounds 1–5 (180/80/52.5, then +5/+2.5/+2.5).

What it does with the quirks:

- Multi-set text such as `70-70-72.5`, `.10-8-8`, `12x20+11x20`, `12*20-9*20`
  and `12kg x 12-10-10` becomes one row per set.
- Date-mangled rep ranges (46310 → 10–15) are converted back.
- Swapped kg and reps (5 kg × 180) are swapped back.
- Blank reps stay blank.
- A single number logged for a multi-set accessory is repeated for the planned
  number of sets (flagged).
- The stray 72.5 under round 5's curls is assigned to the chest-supported row
  (flagged).

**Dates are estimated.** The workbook has none, so round 5 week 1 Monday is
taken as 2026-10-26 and earlier rounds are counted back three weeks each
(round 1 starts 2026-08-03). Every imported row has `date_estimated = TRUE`. If
you had breaks between rounds, change `round1_start` in the script, or edit the
dates in `import_sets.csv`.

When you're happy with it:

```sh
Rscript scripts/upload_import.R
```

It skips rows that are already in the Sheet, so it's safe to run twice.

## 4. Password and API key

The hosted app is on the public internet, so it asks for a password and reads
no data before you log in. Each phone remembers the login.

```r
source("R/auth.R")
password_hash("my gym password", "any-random-salt-string")
```

Put the salt in `APP_PASSWORD_SALT` and the result in `APP_PASSWORD_HASH`.
Changing either one logs all devices out. For local use only, `APP_AUTH=off`
skips the gate.

`ANTHROPIC_API_KEY` is used only by the Review tab. The model is the
`claude_model` setting (default `claude-sonnet-5-5`). Requests enable the
API's server-side refusal fallback (`fallbacks: "default"`), so a rare
safety-classifier refusal is retried on another model instead of failing.

## 5. Garmin runs (optional)

Runs recorded on the watch can come in automatically through
[intervals.icu](https://intervals.icu), a free training site that syncs with
Garmin Connect and has a personal API key. (Garmin's own API is only for
approved businesses, and Strava's API terms don't allow its data in AI tools,
which the Review tab is.)

1. Create an intervals.icu account and connect Garmin under *Settings →
   Connections*. Connect Garmin directly, not through Strava: activities that
   come from Strava aren't available through the API.
2. Under *Settings → Developer Settings*, create an API key and put it in
   `INTERVALS_API_KEY`. Leave `INTERVALS_ATHLETE_ID=0` (your own account).
3. Restart the app. The Run tab gets a **Sync runs from Garmin** button, and the
   app syncs once every time it opens.

The first sync looks back `garmin_sync_days` (90) days; later syncs start a few
days before the last imported run. Runs, trail runs and treadmill runs are
imported (trail runs as `trail`, the rest as `road`) with distance, moving time,
ascent, average HR, RPE and the activity description as notes. Each run keeps
its intervals.icu id in `external_id`, so nothing is imported twice. If you also
log the same run by hand, delete one of the two rows in the `runs` tab.

## 6. Deploy to Posit Connect Cloud

Connect Cloud's free plan deploys from a **public** GitHub repo, so the code
is public. Your data isn't: it lives in the Sheet, which only the service
account can open, and the workbook and `data/` are git-ignored. Check the
current free-plan limits on [connect.posit.cloud/plans](https://connect.posit.cloud/plans).

1. If you added or updated packages, regenerate the manifest (Connect Cloud
   builds from `manifest.json`, not `renv.lock`):

   ```r
   rsconnect::writeManifest(
     appFiles = c("app.R", list.files("R", full.names = TRUE), list.files("www", full.names = TRUE)),
     appPrimaryDoc = "app.R"
   )
   ```

2. Push to GitHub.
3. In Connect Cloud, choose **Publish → Shiny**, then the repo, the branch, and
   `app.R` as the primary file.
4. In the content's settings, add these variables and mark them secret:
   `GSHEET_ID`, `GOOGLE_SA_JSON` (paste the whole key JSON as the value),
   `APP_PASSWORD_SALT`, `APP_PASSWORD_HASH`, `ANTHROPIC_API_KEY`, and, for
   Garmin, `INTERVALS_API_KEY`. Don't set `APP_AUTH` there.
5. Open the URL on your phone and add it to the home screen.

## 7. Day-to-day

**Start a new round.** On Plan, press **Finish round → generate next round**.
The dialog shows each lift's current TM, its e1RM and the proposed new TM with
the reason. You can switch the progression mode or type over any number.
Saving appends the new TMs to `training_max`, and Today moves to week 1 of the
new round. Overridden numbers are saved with method `manual`.

Progression modes (setting `progression_mode`):

- **standard**: +5 kg deadlift, +2.5 kg bench and OHP (per-lift `increment_*`).
- **conditional**: the increment is added only if the week-3 top set hit all
  reps with at least `conditional_min_rir` in reserve (a top set logged without
  RIR counts on reps alone). Otherwise the TM holds. After
  `reset_after_misses` (2) misses in a row, it suggests a reset to
  `reset_pct` (90 %) of the e1RM trend.
- **e1rm**: the standard increment, capped at `e1rm_cap_pct` (90 %) of the e1RM
  trend.

**Week 4.** `deload_frequency` (`off` / `every` / `every_2`, default
`every_2`) adds a 7th-week-protocol week to a round. `deload_type` is `deload`
(70/80/90/100 % × 5/5/1/1) or `tm_test` (70/80/90/100 % × 5/5/5/3–5). It's a
suggestion: finish the round early to skip it.

**TM health** is TM ÷ e1RM trend: *Watch* above `tm_warn` (92 %), *Too high*
above `tm_flag` (95 %).

**Edit the template.** Change the `template` tab in the Sheet, then press
**Refresh** in Settings. One row is one line of a session:

| column | meaning |
|---|---|
| `day`, `day_label` | `mon` … `sun`, and the name shown in the app |
| `order` | position within the day |
| `exercise`, `lift` | display name; `lift` (`deadlift`/`bench`/`ohp`/`front_squat`) links a row to a training max and to e1RM tracking. Leave it blank for accessories |
| `set_type` | `warmup`, `work`, `top`, `joker`, `backoff`, `accessory` |
| `scheme` | `pct` (% of TM per week), `fixed` (`weight_kg`), `effort` (pre-filled from last session, double progression) |
| `sets`, `reps_min`, `reps_max` | e.g. 2 sets of 8–12 |
| `pct_w1`, `pct_w2`, `pct_w3` | for `pct` rows; leave a week blank to skip that set that week (week-3 jokers) |
| `weight_kg`, `rest_s`, `notes` | fixed weight, rest-timer seconds, a note shown under the exercise |

Every logged set stores its own planned kg and reps, so editing the template
never changes history. Round 1–3's old split stays as it was logged.

**Double progression** (`effort` rows): Today pre-fills last session's weight
and reps. Once every set reached the top of the rep range, it offers
"+`accessory_increment` kg" (2.5) as a one-tap button.

## 8. How the numbers are computed

- **e1RM**: Epley, `kg × (1 + reps / 30)` (a single counts as itself), using
  only sets of ≤ `e1rm_max_reps` (10) reps. When RIR is logged, reps + RIR is
  used. A set without RIR is a lower bound (hollow dot): you could have done
  more.
- **Trend**: a local linear trend state-space model (level + weekly slope) on
  log e1RM, fitted with a Kalman filter and RTS smoother. Each session's best
  e1RM is treated as a noisy reading (about 3 %, `trend_obs_sd`) of a slowly
  drifting level, and uncertainty widens across gaps. Lower bounds count fully
  when above the trend. Below it they pull with 3× the noise
  (`trend_lower_bound_factor`), because a set with unknown reps in reserve is
  weak evidence of being weaker. The band is the 80 % interval.
- **Goal projection**: when the trend reaches the goal, using the 80 % range
  of the current slope. It assumes the trend continues, so treat it as a rough
  guide.
- **Grade-adjusted pace**: every metre climbed counts as `gap_ascent_factor`
  (8) metres of flat running. Descents are ignored.
- **Race predictions**: Riegel, `T2 = T1 × (D2 / D1)^1.06`, from runs of 5 km
  or more in the last 8 weeks, on grade-adjusted distance. Training runs aren't
  races, so the predictions are conservative.
- **Combined load**: run km + `session_load_km` (8) per lifting session.
  A week is flagged when it's above `load_spike_ratio` (1.3) × the previous
  4-week average.

## 9. Code layout

```
app.R                 page, bottom navigation, password gate, modules
R/db.R                the only file that talks to storage (db_read_* / db_write_*)
R/engine_*.R          round generation, progression, e1RM trend, running, Today pre-fill
R/parse_actuals.R     parser for the workbook's free-text actuals
R/garmin.R            Garmin runs via the intervals.icu API
R/review_summary.R    the JSON summary and the Claude call (ellmer)
R/mod_*.R             one Shiny module per tab
www/                  CSS and the JS for steppers, RIR chips and the rest timer
scripts/              Sheet setup, workbook import, import upload
tests/testthat/       engine, parser and stats tests
```

To move off Google Sheets (Postgres, SQLite, DuckDB), rewrite `db_read`,
`db_append`, `db_overwrite`, `db_upsert` and `db_setup_tabs` in `R/db.R`.
Nothing else reads or writes data.

Columns added beyond the original brief:

- `sets`: `lift`, `planned_reps_max`, `date_estimated`
- `runs`: `run_id`, `source` (`manual` or `garmin`), `external_id` (the
  intervals.icu activity id), `logged_at`
- `settings`: `description`
