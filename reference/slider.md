# Generate a Slider Indicator in HTML

This function creates an HTML representation of a slider with a triangle
indicator that moves between two anchors. The position of the triangle
is based on a value between 0 and 1, which determines its placement on
the scale.

## Usage

``` r
slider(value, left_anchor, right_anchor)
```

## Arguments

- value:

  A numeric value between 0 and 1 indicating the position of the
  triangle on the scale. A value of 0 places the triangle at the far
  left, while a value of 1 places it at the far right.

- left_anchor:

  A character string specifying the label for the left end of the scale.

- right_anchor:

  A character string specifying the label for the right end of the
  scale.

## Value

A character string containing the HTML code for the slider.

## Examples

``` r
# Generate a slider with the triangle at the middle of the scale
slider(0.5, "Start", "End")
#> [1] "\n<div style=\"display: flex; align-items: center;\">\n   <!-- Left Anchor -->\n    <div style=\"margin-right: 10px; font-size: 75%\">Start</div>\n    <!-- Scale Container -->\n    <div style=\"width: 100%; height: 10px; background-color: #e0e0e0; position: relative;\">\n    <div style=\"width: 0; height: 0; border-left: 5px solid transparent; border-right: 5px solid transparent; border-bottom: 10px solid #007bff; position: absolute; left: 50%; transform: translateX(-50%);\"></div>\n    </div>\n    <!-- Right Anchor -->\n    <div style=\"margin-left: 10px; font-size: 75%\">End</div>\n</div>\n"

# Generate a slider with the triangle near the start of the scale
slider(0.1, "Low", "High")
#> [1] "\n<div style=\"display: flex; align-items: center;\">\n   <!-- Left Anchor -->\n    <div style=\"margin-right: 10px; font-size: 75%\">Low</div>\n    <!-- Scale Container -->\n    <div style=\"width: 100%; height: 10px; background-color: #e0e0e0; position: relative;\">\n    <div style=\"width: 0; height: 0; border-left: 5px solid transparent; border-right: 5px solid transparent; border-bottom: 10px solid #007bff; position: absolute; left: 10%; transform: translateX(-50%);\"></div>\n    </div>\n    <!-- Right Anchor -->\n    <div style=\"margin-left: 10px; font-size: 75%\">High</div>\n</div>\n"
```
