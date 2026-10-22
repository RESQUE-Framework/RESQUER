# Changelog

## RESQUER 0.10.22 (2026-10-22)

### Added

- New demo profile
  `inst/extdata/demo_profiles/resque_Schoenbrodt_2026-03.json` in the
  current scheme (meta 0.3.0, pub 0.9.0; 10 publications). The profile
  templates fall back to it when no JSON path is given.

### Fixed

- Profile templates (`RESQUE_profile.qmd`,
  `RESQUE_profile_section_test.qmd`) rendered directly without a
  `json_path`: an empty path is now treated like a missing one, and the
  demo profile is used.

### Changed

- `inst/ref_set_psy/get_new_refset.R` (script to rebuild the psychology
  reference set) can now resume after a crash. Intermediate results are
  no longer written to `~/refset_temp`. They go to a folder with a fixed
  name in the system temp directory,
  e.g. `RESQUER_refset_C15744967_article_n100000_seed42`; the name
  encodes the sampling parameters. The script downloads one year at a
  time and skips years whose `refset_<year>.RDS` already exists and can
  be read. A re-run after a crash therefore continues with the first
  missing year, and the sample is identical to that of a single run.
  `options(RESQUER.refset_dir = ...)` sets another location.

### Removed

- Removed hardcoded local file paths in comments and debugging code.

## RESQUER 0.10.21 (2026-10-21)

### Removed

- The deprecated, unfinished Shiny dashboard
  (`inst/dashboard/dashboard_shiny.qmd`, `custom.scss`), its
  (commented-out) launch instructions in the README, and the archived
  publication-table code that only the dashboard used
  (`_archive/preprocessing_archive.R`).

## RESQUER 0.10.20 (2026-10-21)

### Fixed

- Profile report, “Rare samples used in studies”: the sample size is now
  read from `P_Methods_PowerAnalysis_SampleSize` (the field of the
  current scheme) instead of the obsolete `P_SampleSize`, which made the
  report always show “not provided”. Line breaks in the entry are
  collapsed; a missing or whitespace-only entry is shown as “not
  provided”.
- Profile report: rendering no longer aborts in the impact table when no
  BIP! data are available (e.g. `preprocess(get_BIP = FALSE)`). The
  unused FNPR column was formatted before being dropped, which failed on
  an empty table.
- Profile report, “Location of Data Collection”: the filter on studies
  with data (`P_Data == "Yes"`) made a inconsistent selection. Whenever
  some studies had no data (or a missing `P_Data`), location categories
  could silently disappear from the chart (e.g. “Naturalistic”).
- `render_overview(clear_cache = TRUE)` now takes effect: the cached
  `applicant_data.RData` in the JSON folder is ignored, all applicants
  are recomputed, and the cache file is overwritten. Previously the
  argument was passed to the template but never used, so an existing
  (possibly stale) cache was always reused.
- [`RRS_category_summary()`](https://resque-framework.github.io/RESQUER/reference/RRS_category_summary.md)
  now finds the score categories in `forms$pub$config` as well as in
  `forms$config` (same precedence as the scorer). Previously it stopped
  with an error for exports that store the categories in
  `forms$pub$config`.
- [`preprocess()`](https://resque-framework.github.io/RESQUER/reference/preprocess.md):
  the derived `replication` column (preregistered replication) is now
  built from the current field `P_Preregistration_Replication` instead
  of the legacy name `P_PreregisteredReplication`, after the “not
  applicable” justification checks; studies without preregistration get
  `No`. Previously the column was always `NA`. It is not shown in the
  reports, so nothing changed. As a side effect, the share of missing
  indicators (`ind_missing`) no longer counts the empty legacy
  placeholder column; eligibility for the rigor score is unchanged for
  all test cases.

## RESQUER 0.10.19 (2026-10-21)

### Added

- Profile report: new light-grey, struck-through badge images
  (`inst/profile_qmd/assets/*_small_grey_strike.png`) for open data,
  open materials, open code, and preregistration.

### Changed

- Profile report, “Open Practices” column: a practice answered as not
  available (`NotAvailable`/`No`) now shows the grey struck-through
  badge; “not applicable” (or no answer) shows no badge. Badges are
  smaller (20px high).
- Profile report layout: body width increased to 1100px (sidebar 250px,
  margin 300px).
- Badge images are now the same size (71 x 78 px); the green open-code
  badge was cropped and rescaled so it no longer appears smaller than
  the others.

## RESQUER 0.10.18 (2026-10-21)

### Fixed

- [`preprocess()`](https://resque-framework.github.io/RESQUER/reference/preprocess.md):
  the “not verified” count of the independent-verification pie data
  (`OS_pie$Repro$No`) now counts both the current answer code
  `NotAvailable` and the older code `No`; previously `NotAvailable` was
  never counted. Expect `OS_pie$Repro` to differ from earlier regression
  baselines.
- [`preprocess()`](https://resque-framework.github.io/RESQUER/reference/preprocess.md):
  claims of independent verification without a link are now also set to
  `NotAvailable` for `AllResultsReproducible`, and when the identifier
  is missing (`NA`) or blank (whitespace only). Scores are not affected.
- Scorer (`inst/js/score2.js`):
  [`exists()`](https://rdrr.io/r/base/exists.html) in scoring rules now
  treats whitespace-only strings as missing. A whitespace-only
  identifier no longer earns points, and a whitespace-only “not
  applicable” explanation (`*_NAExplanation`, ZK2 justification) no
  longer removes the item from the maximum. Scores of the test-suite
  JSONs are unchanged (none contains such values).
- [`preprocess()`](https://resque-framework.github.io/RESQUER/reference/preprocess.md):
  “not applicable” answers now follow the same justification rule as the
  scorer. A whitespace-only `*_NAExplanation`, or an explanation field
  that is absent from the whole file, no longer counts as a
  justification; the answer is then shown as `NotAvailable`/`No` with a
  preprocessing note. Scores are not affected.

## RESQUER 0.10.17 (2026-10-21)

### Changed

- Removed the unused internal helpers `%and%`, `%or%`, `not()` and
  [`exists()`](https://rdrr.io/r/base/exists.html) (`R/operators.R`),
  leftovers of the former R scorer. The package-internal
  [`exists()`](https://rdrr.io/r/base/exists.html) masked
  [`base::exists()`](https://rdrr.io/r/base/exists.html) inside the
  package namespace.

## RESQUER 0.10.16 (2026-10-20)

### Added

- The profile report publication table has a new “Open Practices” column
  with small badges for open data, open materials, open code, and
  preregistration. Badges link to the respective material when an
  identifier is available.

### Changed

- The profile report layout is wider (sidebar 250px, body 1000px, margin
  300px).

## RESQUER 0.10.15 (2026-10-20)

### Fixed

- [`preprocess()`](https://resque-framework.github.io/RESQUER/reference/preprocess.md)
  now ensures that `Title` and `Year` columns always exist in the
  indicators data, so downstream RRS code and profile reports no longer
  fail when they are missing.
- [`compute_RRS()`](https://resque-framework.github.io/RESQUER/reference/compute_RRS.md)
  and
  [`RRS_radarchart()`](https://resque-framework.github.io/RESQUER/reference/RRS_radarchart.md)
  handle missing or `NULL` publication years without error.
- [`read_RESQUE()`](https://resque-framework.github.io/RESQUER/reference/read_RESQUE.md)
  now completes partial DOIs (e.g., `10.1177/...`) to full
  `https://doi.org/` URLs, so title links work.

## RESQUER 0.10.14 (2026-10-20)

### Fixed

- [`preprocess()`](https://resque-framework.github.io/RESQUER/reference/preprocess.md)
  and profile reports now handle unavailable RRS, BIP!, and OpenAlex
  results without aborting rendering.
- [`compute_RRS()`](https://resque-framework.github.io/RESQUER/reference/compute_RRS.md)
  and
  [`RRS_radarchart()`](https://resque-framework.github.io/RESQUER/reference/RRS_radarchart.md)
  now render the radar chart when publication years are unavailable,
  omitting only the year histogram.

## RESQUER 0.10.13 (2026-10-20)

### Fixed

- RRS calculations now consistently exclude inapplicable scoring
  categories, preventing mismatches between category summaries,
  per-publication scores, and radar charts.

### Changed

- [`compute_RRS()`](https://resque-framework.github.io/RESQUER/reference/compute_RRS.md)
  is now documented as the authoritative, reportable RRS calculation; it
  restricts scoring to eligible publications with DOIs.
- The minimum required version of `OAmetrics` is now 0.5.4.

## RESQUER 0.10.12 (2026-10-14)

### Changed

- The minimum required version of `openalexR` is now 3.1.2 to avoid
  compatibility issues.

## RESQUER 0.10.11 (2026-10-01)

### Fixed

- Profile and overview reports now exclude outputs without data or
  scoring indicators from sample and data-type summaries.
- Relative Rigor Score calculations now handle outputs without scorable
  indicators or DOIs correctly, avoiding invalid scores and errors in
  profile radar charts.

## RESQUER 0.10.10 (2026-08-04)

### Added

- New `check_quarto()` helper that verifies a minimum required Quarto
  version is installed and errors informatively if not; called at the
  start of
  [`render_profile()`](https://resque-framework.github.io/RESQUER/reference/render_profile.md)
  and
  [`render_overview()`](https://resque-framework.github.io/RESQUER/reference/render_overview.md).
- `DT` added as a suggested package.

### Changed

- [`render_profile()`](https://resque-framework.github.io/RESQUER/reference/render_profile.md)
  now restores the working directory via
  [`on.exit()`](https://rdrr.io/r/base/on.exit.html), so it is reset
  even if rendering fails.

## RESQUER 0.10.9 (2026-07-30)

### Changed

- [`render_overview()`](https://resque-framework.github.io/RESQUER/reference/render_overview.md)
  now defaults to using the *median* FNCS (Field-Normalized Citation
  Score) instead of the mean, with a new `FNCS_type` argument
  (`"median"` or `"mean"`) to switch between them.

## RESQUER 0.10.8 (2026-07-07)

### Added

- Enable multi-study support

## RESQUER 0.10.7 (2026-07-06)

### Added

- Scientific impact tables now include a new first column
  (`Top 3 & Merit Statement`).
- `Top 3` selection (`P_TopPaper_Select`) is displayed with a star icon.
- Merit statements are now shown directly in the first column as an info
  popover icon (below the star when both are present).
- An optional `Applicant's Comments` popover column is shown when any
  `*_Comment` fields contain content.

### Changed

- Scientific impact tables no longer use striped row styling.
- Rows for selected top papers are highlighted in pastel yellow.
- Impact table logic now keeps papers even when BIP returned no values.
- Method-type summary in the profile now excludes `*_Comment` fields.
- OpenAlex team-science summaries now use unique papers (DOIs) for
  retrieval counts and percentages.
- Waffle charts in the RRS overview now use dynamic scaling with a
  minimum reference size (avoiding a warning)
- Preregistration sector lookup now matches category labels more
  robustly.
- Several profile labels/texts were harmonized from “papers” to
  “studies” where multi-study submissions are shown.

### Fixed

- Popover content for comments and merit statements is now HTML-escaped,
  preventing broken markup when comments contain quotes or special
  characters.
- `sum_MC()` now excludes all `*_Comment` columns and sums only logical
  indicator columns, improving robustness across indicator sets.
- RRS score joins now deduplicate per DOI before merging, avoiding
  many-to-many join inflation for multi-study papers.
- BIP paper counters now deduplicate per DOI, so paper-level counts are
  no longer inflated by multi-study entries.
- `high_impact` and `low_impact` tables now both explicitly suppress row
  names.

### Known issues

- In some multi-study situations, the impact tables have duplicates.
