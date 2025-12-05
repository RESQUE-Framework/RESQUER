# Check if an object is NULL, NA, or length zero

This utility function returns `TRUE` if the input is `NULL`, has length
zero, or is an `NA` of length one. Otherwise returns `FALSE`.

## Usage

``` r
is.nulla(x)
```

## Arguments

- x:

  Any R object.

## Value

A logical scalar: `TRUE` if `x` is considered "null-like", otherwise
`FALSE`.

## Examples

``` r
is.nulla(NULL)
#> [1] TRUE
is.nulla(NA)
#> [1] TRUE
is.nulla(character(0))
#> [1] TRUE
is.nulla(1)   # FALSE
#> [1] FALSE
```
