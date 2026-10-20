# Create a mini horizontal stacked bar chart in pure HTML

This function creates a mini horizontal stacked bar chart in pure
HTML/CSS. The scale represents a value within a range, divided into
color-coded steps and highlighting a threshold value.

## Usage

``` r
ministack(
  value,
  breaks = c(0.15, 0.3),
  colors = c("red", "#f7cf07", "#3af72c"),
  width = 120,
  height = 12
)
```

## Arguments

- value:

  numeric value to be represented on the gauge (must be between 0 and 1)

- breaks:

  numeric vector specifying the break points for coloring the gauge
  steps (must be two elements)

- colors:

  character vector specifying the colors for each step in the gauge
  (must be three elements)

- width:

  Width in pixels

- height:

  Height in pixels

## Value

a HTML string representing the mini gauge plot

## Examples

``` r
ministack(0.25)
#> [1] "\n<div style=\"position: relative; width: 120px; height: 12px; background-color: white;\">\n  <!-- The stacked bar -->\n  <div style=\"\n    position: absolute;\n    top: 2.4px;\n    left: 0;\n    width: 120px;\n    height: 7.2px;\n  \">\n    <!-- Red segment -->\n    <div style=\"\n      position: absolute;\n      top: 0;\n      left: 0;\n      width: 18.0px;\n      height: 100%;\n      background-color: red;\n    \"></div>\n    <!-- Yellow segment -->\n    <div style=\"\n      position: absolute;\n      top: 0;\n      left: 18.0px;\n      width: 18.0px;\n      height: 100%;\n      background-color: #f7cf07;\n    \"></div>\n    <!-- Green segment -->\n    <div style=\"\n      position: absolute;\n      top: 0;\n      left: 36.0px;\n      width: 84.0px;\n      height: 100%;\n      background-color: #3af72c;\n    \"></div>\n  </div>\n  <!-- Downward-pointing triangle indicator -->\n  <div style=\"\n    position: absolute;\n    top: 2.4px;\n    left: 26.4px;\n    width: 0;\n    height: 0;\n    border-left: 3.6px solid transparent;\n    border-right: 3.6px solid transparent;\n    border-top: 7.2px solid black;\n  \"></div>\n</div>\n"
```
