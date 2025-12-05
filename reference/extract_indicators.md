# Extract Indicators from Applicant List

This function processes a list of applicants, extracting specific
indicators from each applicant's data and compiling them into a single
data frame. It also includes metadata such as ORCID, LastName, and
ExternalRaterName from the applicant's profile.

## Usage

``` r
extract_indicators(applicant_list, selector = c("P_"))
```

## Arguments

- applicant_list:

  A list containing the RESQUE jsons from multiple applicants.

- selector:

  The prefix(es) of indicators that should be selected with
  `starts_with()`

## Value

The function returns a data frame where each row corresponds to one
publication and indicators are in columns.
