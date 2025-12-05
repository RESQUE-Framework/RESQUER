# Display information about the scoring categories

This function shows a table how many indicators

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
