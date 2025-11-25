# Convert OpenAlex works into formatted reference list (in HTML)

This function generates a reference list for a given set of OpenAlex
works. The formatting is inspired by APA style, but it does (by default)
omit the journal name and can re-order authors alphabetically

## Usage

``` r
get_reference_list(
  works,
  alphabetical = TRUE,
  journal_name = FALSE,
  bullet = TRUE
)
```

## Arguments

- works:

  A data frame containing information about the works to be referenced.
  Each row of the data frame represents a single work, and columns
  represent different attributes of the works.

- alphabetical:

  Logical value indicating whether the references should be sorted
  alphabetically by author's last name. Default is TRUE.

- journal_name:

  Logical value indicating whether the journal name should be displayed.
  Default is FALSE.

- bullet:

  Logical value indicating whether the references should be formatted as
  an unordered list (

## Value

A character vector containing the formatted APA references.
