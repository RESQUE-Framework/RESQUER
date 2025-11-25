# Extract scores on indicator level

This function processes a list of applicants, extracting the Relative
Rigor Score (RRS) for each single indicator of each applicant and
compiling them into a single long-format data frame.

## Usage

``` r
extract_indicator_scores(applicant_list)
```

## Arguments

- applicant_list:

  A list containing the RESQUE jsons from multiple applicants.

## Value

A data frame containing the indicator scores for each applicant.
