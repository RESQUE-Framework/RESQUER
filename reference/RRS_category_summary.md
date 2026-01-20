# Display information about the scoring categories

This function shows a table how many indicators are assigned to each
scoring category. This counts all indicators that are defined in the
packs.json and contribute to scoring (regardless of whether an indicator
is missing in that specific applicant).

## Usage

``` r
RRS_category_summary(applicant, selector = c("P_"))
```

## Arguments

- applicant:

  The applicant data that has been imported by the `read_RESQUE`
  function.

- selector:

  The prefix(es) of indicators that should be selected with
  `starts_with()`

## Value

Nothing (called for it's print output)
