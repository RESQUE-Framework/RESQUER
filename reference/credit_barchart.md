# Display a barchart with CRediT roles

Display a barchart with CRediT roles

## Usage

``` r
credit_barchart(credit_dat, ordered = FALSE, compact = FALSE)
```

## Arguments

- credit_dat:

  The raw data in the format of the `applicant()$credit` field. This is
  a data frame with two relevant columns: `Role` (the CRediT role) and
  `Degree` (one of "Lead", "Equal", "Support", "NoRole", or "not
  applicable"). Roles can appear multiple times (e.g., for multiple
  papers) and will be aggregated in the plot.

- ordered:

  If `FALSE`, keep alphabetical order. If `TRUE`, sort by strongest
  contribution.

- compact:

  If `FALSE`, show all 14 roles. If `TRUE`, aggregate the 14 roles into
  4 categories.

## Examples

``` r
credit_dat <- data.frame(
  Role = rep(c("Conceptualization", "Data Curation", "Formal Analysis",
    "Funding Acquisition", "Investigation", "Methodology",
    "Project Administration", "Resources", "Software", "Supervision",
    "Validation", "Visualization", "Writing: Original draft",
    "Writing: Review & Editing"), 10),
  Degree = sample(c("Lead", "Equal", "Support", "NoRole", "not applicable"),
    prob = c(0.1, 0.2, 0.3, 0.3, 0.1), size=140, replace=TRUE)
)
credit_barchart(credit_dat)

credit_barchart(credit_dat, ordered=TRUE)
```
