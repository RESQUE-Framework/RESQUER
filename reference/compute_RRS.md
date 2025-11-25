# Compute Relative Rigor Score (RRS)

This function computes a relative rigor score based on the indicators
provided from an applicant.

## Usage

``` r
compute_RRS(applicant, sectors = c("weighted", "equal"))
```

## Arguments

- applicant:

  The applicant data that has been preprocessed by the `read_RESQUE`
  function.

- sectors:

  Should sectors be all equally sized ("equal") or weighted by the
  maximum sum of attainable points in each category ("weighted")?

## Value

A tibble containing the dimension (category), maximum points, and
relative score for each category, suitable for creating a radar chart.
