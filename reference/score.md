# Score a single research output

Returns a list with the following elements:

- `max_score`: the maximum score that can be reached

- `score`: the score that was reached

- `relative_score`: the score that was reached divided by the maximum
  score

## Usage

``` r
score(research_output, verbose = FALSE, meta)
```

## Arguments

- research_output:

  The research output to be scored. This is the latter part of the
  export json with the actual item values of one specific publication.

- verbose:

  A logical value indicating whether to print verbose output (default is
  `FALSE`)

- meta:

  The meta object from the export json

## Value

A list with the scoring information
