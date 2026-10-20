# Sparkline pie chart function.

The default colors are grey (for not applicable in the first value slot)
and then a color ramp from red to green

## Usage

``` r
sparkpie(values, colors = NA, ...)
```

## Arguments

- values:

  Absolute numbers.

- colors:

  Overwrite the default colors with a vector of hex color values. Must
  have the same length as `values`.

- ...:

  Additional arguments passed to the `sparkline` function.
