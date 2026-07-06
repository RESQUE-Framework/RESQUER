# RESQUER News

## RESQUER 0.10.7 (2026-07-06)

### Added

- Scientific impact tables now include a new first column (`Top 3 & Merit Statement`).
- `Top 3` selection (`P_TopPaper_Select`) is displayed with a star icon.
- Merit statements are now shown directly in the first column as an info popover icon (below the star when both are present).
- An optional `Applicant's Comments` popover column is shown when any `*_Comment` fields contain content.

### Changed

- Scientific impact tables no longer use striped row styling.
- Rows for selected top papers are highlighted in pastel yellow.
- Impact table logic now keeps papers even when BIP returned no values.
- Method-type summary in the profile now excludes `*_Comment` fields.
- OpenAlex team-science summaries now use unique papers (DOIs) for retrieval counts and percentages.
- Waffle charts in the RRS overview now use dynamic scaling with a minimum reference size (avoiding a warning)
- Preregistration sector lookup now matches category labels more robustly.
- Several profile labels/texts were harmonized from "papers" to "studies" where multi-study submissions are shown.

### Fixed

- Popover content for comments and merit statements is now HTML-escaped, preventing broken markup when comments contain quotes or special characters.
- `sum_MC()` now excludes all `*_Comment` columns and sums only logical indicator columns, improving robustness across indicator sets.
- RRS score joins now deduplicate per DOI before merging, avoiding many-to-many join inflation for multi-study papers.
- BIP paper counters now deduplicate per DOI, so paper-level counts are no longer inflated by multi-study entries.
- `high_impact` and `low_impact` tables now both explicitly suppress row names.

### Known issues:

- In some multi-study situations, the impact tables have duplicates.