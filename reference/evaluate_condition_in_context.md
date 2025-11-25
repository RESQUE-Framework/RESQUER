# Evaluate a condition in a context

Evaluate a condition in a context

## Usage

``` r
evaluate_condition_in_context(condition, research_output)
```

## Arguments

- condition:

  The logical conditions in the `scoring` section of core-pub.json. This
  can either be the `score` -\> `not_applicable` condition or the
  `score` -\> `condition` condition

- research_output:

  The research output to be scored. This is the latter part of the
  export json with the actual item values of one specific publication.
