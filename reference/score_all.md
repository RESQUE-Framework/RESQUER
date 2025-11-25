# Score multiple research outputs

Returns a list with the following elements:

- scores: a list of scores for each research output

- scored_research_outputs: the number of research outputs that were
  scored

- overall_score: the average score of all scored research outputs. Each
  output is weighted equally.

## Usage

``` r
score_all(research_outputs, verbose = FALSE)
```

## Arguments

- research_outputs:

  A list of research outputs to be scored

- verbose:

  A logical value indicating if verbose output should be printed

## Value

A list with scores, number of scored research outputs, and overall score
