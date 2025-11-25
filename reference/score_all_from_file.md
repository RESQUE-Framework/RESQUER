# Score all research outputs from a file

Reads the research outputs from the specified file and scores them using
[`score_all`](https://resque-framework.github.io/RESQUER/reference/score_all.md).
Returns a list with the following elements:

- scores:

  a list of scores for each research output

- scored_research_outputs:

  the number of research outputs that were scored

- overall_score:

  the average score of all scored research outputs

## Usage

``` r
score_all_from_file(file, verbose = FALSE)
```

## Arguments

- file:

  The file path where the research outputs are stored.

- verbose:

  Logical value indicating whether to print additional information.
  Default is `FALSE`.

## Value

A list with elements scores, scored_research_outputs, and overall_score.

## Examples

``` r
if (FALSE) { # \dontrun{
score_all_from_file("research_outputs.json", verbose = TRUE)
} # }
```
