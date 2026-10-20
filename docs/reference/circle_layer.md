# Four layered circles, as a complex way to visualize 4 rigor score components as concentric rings.

TODO: Provide color ramp breakpoints as parameters

## Usage

``` r
circle_layer(
  value,
  colors,
  outer_width = 60,
  weights = c(1, 1, 1, 1),
  sharp_boundaries = FALSE
)
```

## Arguments

- value:

  A value between 0 and 1 (which then is transformed into %)

- colors:

  A vector of four colors, from innermost to outermost color

- outer_width:

  Circle radius in pixels.

- weights:

  A vector of 4 numbers specifying the relative weight of each layer.
  The weights are normalized to percentages automatically. Default is
  c(1, 1, 1, 1) which gives equal 25% spacing.

- sharp_boundaries:

  Whether to create sharp color transitions (TRUE) or smooth blends
  (FALSE)
