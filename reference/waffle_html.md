# Generate an HTML

tag embedding a Waffle plot using R with optional emoji support Source:
[`R/sparkcharts.R`](https://github.com/RESQUE-Framework/RESQUER/R/sparkcharts.R)

`waffle_html.Rd`

This is a workaround to embed small waffle plots in a sparkline style
into a kable. Now supports displaying custom emojis instead of colored
blocks.

## Usage

``` r
waffle_html(
  values,
  max_value = 10,
  rows = 1,
  colors = c("#1da412", "#1da412", "#c51819", "#C7C7C7"),
  width_px = 120,
  gap_px = 2,
  emojis = NULL,
  font_size_px = 20
)
```

## Arguments

- values:

  A numeric vector of specifying the counts of waffle blocks

- max_value:

  A numeric scalar indicating the maximum number of waffle blocks (so
  that multiple displays have the same size). If any element of `values`
  exceeds `max_value`, a warning is issued and `max_value` is updated to
  the maximum value found. Defaults to 10.

- rows:

  An integer specifying the number of rows in the Waffle plot

- colors:

  A character vector containing colors for each category (default:
  c('green', 'green', 'red', 'grey')) for categories "yes", "partial",
  "no", "notApplicable" (implicitly merging "yes" and "partial").

- width_px:

  An integer specifying the width of the output image in pixels

- gap_px:

  The gap between blocks in pixels

- emojis:

  A character vector of emojis to use instead of colored blocks. If NULL
  (default), regular colored blocks are used.

- font_size_px:

  An integer specifying the font size for emojis in pixels. Only used if
  emojis are provided.

## Value

A character string containing an HTML string representing the waffle
plot

## Examples

``` r
# Standard colored blocks
values <- c(10, 20, 15)
waffle_html(values = values)
#> Warning: More values provided than `max_value`. Increasing `max_value` to 45
#> [1] "<div style=\"margin: 2px 0;\">\n  <div style=\"display: flex; flex-wrap: wrap; width: 120px; height: 0.711111111111111px; gap: 2px;\">\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #1da412; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n    <div style=\"width: 0.711111111111111px; height: 0.711111111111111px; background-color: #c51819; margin: 0; padding: 0;\"></div>\n  </div>\n</div>"

# Using emojis instead
waffle_html(values = c(3, 7), emojis = c("\u2B50\uFE0F", "\u25A2"))
#> [1] "<span style=\"font-size: 20px;\">⭐️⭐️⭐️▢▢▢▢▢▢▢</span>"
```

## On this page

Developed by Felix D. Schönbrodt, Alp Kaan Aksu.

Site built with [pkgdown](https://pkgdown.r-lib.org/) 2.2.0.
