# Wiggle a number by a random ±margin%

Adds a uniformly-distributed random perturbation of up to ±`margin`% to
`x`, returning the result rounded to the same number of decimal places
as the original value.

## Usage

``` r
wiggle(x, margin = 10)
```

## Arguments

- x:

  A numeric vector (or scalar) to wiggle.

- margin:

  Maximum percentage perturbation (default: 10).

## Value

A numeric vector with the same number of decimal places as `x`.

## Examples

``` r
set.seed(42)
wiggle(0.25)        # e.g. 0.27 (still 2 decimal places)
#> [1] 0.27
wiggle(1.5, margin = 20)  # e.g. 1.4 (still 1 decimal place)
#> [1] 1.8
wiggle(c(4.4, 0.7, 3.1, NA), margin = 10)
#> [1] 4.2 0.7 3.2  NA
wiggle(3)           # e.g. 3 (still 0 decimal places)
#> [1] 3
```
