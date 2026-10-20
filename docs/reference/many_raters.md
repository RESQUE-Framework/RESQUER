# Create a wide rater–indicator table across many RESQUE applicants

Imports and preprocesses RESQUE JSON files from a directory via
[`get_jsons`](https://resque-framework.github.io/RESQUER/reference/get_jsons.md),
extracts indicator ratings, reshapes them into a wide format with one
row per (paper \\\times\\ indicator), and one column for each rater. It
computes consistency and scoring-relevance flags for each indicator.

## Usage

``` r
many_raters(path, selector = c("P_"), verbose = TRUE, ...)
```

## Arguments

- path:

  Character scalar. Directory containing RESQUE JSON files to be passed
  to
  [`get_jsons`](https://resque-framework.github.io/RESQUER/reference/get_jsons.md).

- selector:

  Character vector. One or more prefixes identifying the indicator
  columns to be extracted (e.g. `"P_"`). Passed to
  [`extract_indicators`](https://resque-framework.github.io/RESQUER/reference/extract_indicators.md)
  and used in
  [`pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  via
  [`starts_with`](https://tidyselect.r-lib.org/reference/starts_with.html).
  Defaults to `c("P_")`.

- verbose:

  Logical scalar. If `TRUE`, the function prints summary information
  about the number of files, papers, authors, and consistency
  statistics. Defaults to `TRUE`.

- ...:

  Additional arguments passed directly to
  [`get_jsons`](https://resque-framework.github.io/RESQUER/reference/get_jsons.md),
  such as `replace_form`, `recursive`, `get_OpenAlex`, `get_BIP`,
  `anonymize_raters`, and `verbose`.

## Value

A data frame in wide format with one row per (paper \\\times\\
indicator) combination. The table contains, among others, the following
columns:

- `ORCID`, `doi`, `Title`: publication and author identifiers/metadata.

- `Indicator`: the indicator name.

- `R1`, `R2`, `R3`, ...: one column per rater containing the rating
  values, with missing values converted to the character `"_NA_"`.

- `is_consistent`: logical flag indicating whether all rater ratings for
  this indicator are identical.

- `scoring_relevant`: logical flag indicating whether the indicator is
  part of the scoring specification in `meta$forms$pub$scoring`.

- `consensus_rating`: consensus rating (typically the value in `R1`) for
  indicators where `is_consistent` is `TRUE`, otherwise `NA`.

## Details

The function first calls
[`get_jsons`](https://resque-framework.github.io/RESQUER/reference/get_jsons.md)
to obtain a list of preprocessed applicants. It then uses
[`extract_indicators`](https://resque-framework.github.io/RESQUER/reference/extract_indicators.md)
to build a long-format table of indicator ratings for each paper/rater,
and reshapes this into a wide table with one column per rater (`R1`,
`R2`, `R3`, ...).

An indicator is flagged as *consistent* if all raters gave the same
rating. Indicators whose names appear in the scoring specification in
the meta-data are additionally flagged as *scoring relevant*. For
consistent indicators, the consensus rating is prepopulated.

## See also

[`get_jsons`](https://resque-framework.github.io/RESQUER/reference/get_jsons.md),
[`extract_indicators`](https://resque-framework.github.io/RESQUER/reference/extract_indicators.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Create a wide rater–indicator table from all RESQUE files in a directory
  wide <- many_raters(
    path     = "raw_data/Study2/calibration_papers",
    selector = c("P_"),
    replace_form     = "includes/v0.8.1forms.json",
    anonymize_raters = TRUE
  )

  # Inspect inconsistent, scoring-relevant indicators
  subset(wide, !is_consistent & scoring_relevant)
} # }
```
