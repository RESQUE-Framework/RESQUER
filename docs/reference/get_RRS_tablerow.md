# Generate RRS (Relative Rigor Score) table row for an applicant

Creates a data frame row containing the applicant's name, overall RRS
score, and sector-specific scores with visualizations for Open Data,
Open Materials, Preregistration, and Reproducible Code categories. For
the overview report, multiple rows can be combined into a summary table.

## Usage

``` r
get_RRS_tablerow(applicant, add_scores = FALSE)
```

## Arguments

- applicant:

  The applicant information, as returned by the `preprocess` function.

- add_scores:

  Shall the scores for the sectors be added as columns? (This is needed
  for sorting in the overview report.)

## Value

A data frame with one row containing:

- applicant:

  Character string of the applicant's last name

- Relative Rigor Score:

  HTML visualization of the overall RRS score as a circle

- Open Data:

  HTML visualization combining waffle chart and ministack, or "N/A" if
  not applicable

- Open Material:

  HTML visualization combining waffle chart and ministack, or "N/A" if
  not applicable

- Preregistration:

  HTML visualization combining waffle chart and ministack, or "N/A" if
  not applicable

- Reproducible Code/Reproducible Code & Verification:

  HTML visualization combining waffle chart and ministack, or "N/A" if
  not applicable

## Details

The function extracts sector-specific scores from the applicant's RRS
data and creates HTML visualizations for each category. The
visualizations consist of:

- A circular visualization for the overall RRS score

- Waffle charts showing the distribution of practices

- Ministack visualizations showing the relative scores

The function handles missing data gracefully by returning "N/A" for
categories without scores.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example applicant structure
applicant <- list(
  meta = list(FullName = "John Doe"),
  RRS = list(
    overall_score = 0.75,
    sector_scores = data.frame(
      category = c("Open Data", "Open Materials", "Preregistration", "Reproducible Code"),
      rel_score = c(0.8, 0.7, 0.6, 0.9),
      max_points = c(10, 10, 10, 10)
    )
  ),
  OS_pie = list(
    OpenData = c(Yes = 5, Aggregate = 2, Partial = 1, No = 1, notApplicable = 1),
    OpenMaterial = c(Yes = 3, Partial = 2, No = 3, notApplicable = 2),
    Prereg = c("Registered Report" = 2, Preregistration = 3,
               "Not preregistered" = 3, "Not Applicable" = 2),
    OpenCode = c(Yes = 6, Partial = 2, No = 1, notApplicable = 1)
  )
)

result <- get_RRS_tablerow(applicant)
} # }
```
