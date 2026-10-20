# Extract RRS Overall Score for Each Applicant

This function processes a list of applicants, extracting the Relative
Rigor Score (RRS) or each applicant and compiling them into a single
data frame. It includes metadata such as ORCID, LastName, and
ExternalRaterName from the applicant's profile.

## Usage

``` r
extract_overall_scores(applicant_list)
```

## Arguments

- applicant_list:

  A list containing the RESQUE jsons from multiple applicants.

## Value

A data frame containing the RRS overall score for each applicant.
