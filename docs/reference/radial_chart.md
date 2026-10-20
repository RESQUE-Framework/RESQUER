# Create a radial chart in pure HTML/CSS with equal sector angles

This function creates a radial chart visualization using only HTML and
CSS. Each sector has the same angular size and a radius proportional to
its value.

## Usage

``` r
radial_chart(
  values,
  colors = NULL,
  width = 300,
  height = 300,
  border_width = 1,
  border_color = "#cccccc",
  center_size = 0
)
```

## Arguments

- values:

  numeric vector of values (0-1) representing how filled each sector
  should be

- colors:

  character vector of colors for each sector

- width:

  numeric width of the chart in pixels

- height:

  numeric height of the chart in pixels

- border_width:

  numeric width of the outer circle border

- border_color:

  character color of the outer circle border

- center_size:

  numeric size of the empty center (as percentage of total radius)

## Value

a HTML string representing the radial chart

## Examples

``` r
radial_chart(c(80, 60, 40, 90, 30))
#> [1] "<div style=\"position: relative; width: 300px; height: 300px; margin: 0 auto;\">\n  <div style=\"\n    position: absolute;\n    top: 0;\n    left: 0;\n    width: 300px;\n    height: 300px;\n    border-radius: 50%;\n    border: 1px solid #cccccc;\n    box-sizing: border-box;\n  \"></div>\n    <div style=\"\n      position: absolute;\n      top: 0;\n      left: 0;\n      width: 300px;\n      height: 300px;\n      background-color: NA;\n      clip-path: polygon(150.0px 150.0px, 300.0px 150.0px, 297.6px 176.8px, 290.4px 202.7px, 278.8px 226.9px, 263.0px 248.7px, 243.5px 267.3px, 221.1px 282.1px, 196.4px 292.7px, 196.4px 292.7px);\n    \"></div>\n    <div style=\"\n      position: absolute;\n      top: 0;\n      left: 0;\n      width: 300px;\n      height: 300px;\n      background-color: NA;\n      clip-path: polygon(150.0px 150.0px, 196.4px 292.7px, 170.1px 298.6px, 143.3px 299.8px, 116.6px 296.2px, 91.0px 287.9px, 67.4px 275.2px, 46.3px 258.4px, 28.6px 238.2px, 28.6px 238.2px);\n    \"></div>\n    <div style=\"\n      position: absolute;\n      top: 0;\n      left: 0;\n      width: 300px;\n      height: 300px;\n      background-color: NA;\n      clip-path: polygon(150.0px 150.0px, 28.6px 238.2px, 14.9px 215.1px, 5.4px 189.9px, 0.6px 163.4px, 0.6px 136.6px, 5.4px 110.1px, 14.9px 84.9px, 28.6px 61.8px, 28.6px 61.8px);\n    \"></div>\n    <div style=\"\n      position: absolute;\n      top: 0;\n      left: 0;\n      width: 300px;\n      height: 300px;\n      background-color: NA;\n      clip-path: polygon(150.0px 150.0px, 28.6px 61.8px, 46.3px 41.6px, 67.4px 24.8px, 91.0px 12.1px, 116.6px 3.8px, 143.3px 0.2px, 170.1px 1.4px, 196.4px 7.3px, 196.4px 7.3px);\n    \"></div>\n    <div style=\"\n      position: absolute;\n      top: 0;\n      left: 0;\n      width: 300px;\n      height: 300px;\n      background-color: NA;\n      clip-path: polygon(150.0px 150.0px, 196.4px 7.3px, 221.1px 17.9px, 243.5px 32.7px, 263.0px 51.3px, 278.8px 73.1px, 290.4px 97.3px, 297.6px 123.2px, 300.0px 150.0px, 300.0px 150.0px);\n    \"></div></div>"
radial_chart(values=c(1, .5, .25, .1), center_size = 0)
#> [1] "<div style=\"position: relative; width: 300px; height: 300px; margin: 0 auto;\">\n  <div style=\"\n    position: absolute;\n    top: 0;\n    left: 0;\n    width: 300px;\n    height: 300px;\n    border-radius: 50%;\n    border: 1px solid #cccccc;\n    box-sizing: border-box;\n  \"></div>\n    <div style=\"\n      position: absolute;\n      top: 0;\n      left: 0;\n      width: 300px;\n      height: 300px;\n      background-color: NA;\n      clip-path: polygon(150.0px 150.0px, 300.0px 150.0px, 297.7px 176.0px, 291.0px 201.3px, 279.9px 225.0px, 264.9px 246.4px, 246.4px 264.9px, 225.0px 279.9px, 201.3px 291.0px, 176.0px 297.7px, 150.0px 300.0px, 150.0px 300.0px);\n    \"></div>\n    <div style=\"\n      position: absolute;\n      top: 0;\n      left: 0;\n      width: 300px;\n      height: 300px;\n      background-color: NA;\n      clip-path: polygon(150.0px 150.0px, 150.0px 300.0px, 124.0px 297.7px, 98.7px 291.0px, 75.0px 279.9px, 53.6px 264.9px, 35.1px 246.4px, 20.1px 225.0px, 9.0px 201.3px, 2.3px 176.0px, 0.0px 150.0px, 0.0px 150.0px);\n    \"></div>\n    <div style=\"\n      position: absolute;\n      top: 0;\n      left: 0;\n      width: 300px;\n      height: 300px;\n      background-color: NA;\n      clip-path: polygon(150.0px 150.0px, 0.0px 150.0px, 2.3px 124.0px, 9.0px 98.7px, 20.1px 75.0px, 35.1px 53.6px, 53.6px 35.1px, 75.0px 20.1px, 98.7px 9.0px, 124.0px 2.3px, 150.0px 0.0px, 150.0px 0.0px);\n    \"></div>\n    <div style=\"\n      position: absolute;\n      top: 0;\n      left: 0;\n      width: 300px;\n      height: 300px;\n      background-color: NA;\n      clip-path: polygon(150.0px 150.0px, 150.0px 0.0px, 176.0px 2.3px, 201.3px 9.0px, 225.0px 20.1px, 246.4px 35.1px, 264.9px 53.6px, 279.9px 75.0px, 291.0px 98.7px, 297.7px 124.0px, 300.0px 150.0px, 300.0px 150.0px);\n    \"></div></div>"
```
