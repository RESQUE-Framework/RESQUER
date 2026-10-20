# Extract RRS Scores for Each Paper of each applicant

This function processes a list of applicants, extracting the Relative
Rigor Score (RRS) for each sector for each paper and compiling them into
a single data frame. It includes metadata such as ORCID, LastName, and
ExternalRaterName from the applicant's profile. The function can return
the data in either "long" or "wide" format based on the `format`
parameter. In the long format, each row is one sector score for each
applicant and rater. In the wide format, the relative rigor scores
(`rel_score`) are displayed as separate columns for each rater,
side-by-side.

## Usage

``` r
extract_paper_scores(applicant_list)
```

## Arguments

- applicant_list:

  A list containing the RESQUE jsons from multiple applicants.

## Value

A data frame containing the RRS sector scores for each paper of each
applicant.
