# Compute Relative Rigor Score (RRS)

This function computes the reportable Relative Rigor Score (RRS) based
on the indicators provided from an applicant. Unlike the preliminary
`applicant$scores$overall` calculated by
[`read_RESQUE()`](https://resque-framework.github.io/RESQUER/reference/read_RESQUE.md),
it restricts the calculation to eligible publications and excludes
records without a DOI. Use the returned `overall_score` for reporting
the RRS.

## Usage

``` r
compute_RRS(applicant, sectors = c("weighted", "equal"), verbose = FALSE)
```

## Arguments

- applicant:

  The applicant data that has been imported by the
  [`read_RESQUE()`](https://resque-framework.github.io/RESQUER/reference/read_RESQUE.md)
  and preprocessed by the
  [`preprocess()`](https://resque-framework.github.io/RESQUER/reference/preprocess.md)
  function.

- sectors:

  Should sectors be all equally sized ("equal") or weighted by the
  maximum sum of attainable points in each category ("weighted")?

- verbose:

  Print extra information.

## Value

A tibble containing the dimension (category), maximum points, and
relative score for each category, suitable for creating a radar chart.
