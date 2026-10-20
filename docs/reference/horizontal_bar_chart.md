# Create a Horizontal Bar Chart in HTML

Generates an HTML snippet representing a horizontal bar chart. Each
bar's length is proportional to its corresponding value relative to a
maximum value, and each bar is filled with a specified color.
Optionally, labels can be placed on the bars, and an x-axis displaying
the scale can be shown.

## Usage

``` r
horizontal_bar_chart(
  values,
  colors,
  max_value = 10,
  width = 140,
  height = 60,
  labels = NULL,
  show_x_label = TRUE
)
```

## Arguments

- values:

  A numeric vector of values representing the lengths of the bars.

- colors:

  A character vector of colors (in any valid CSS format) for each bar.
  Must be the same length as `values`.

- max_value:

  A numeric scalar indicating the maximum value for scaling the bars. If
  any element of `values` exceeds `max_value`, a warning is issued and
  `max_value` is updated to the maximum value found. Defaults to 10.

- width:

  An integer specifying the overall width (in pixels) of the chart
  container. Defaults to 140.

- height:

  An integer specifying the overall height (in pixels) of the chart
  container. Defaults to 60.

- labels:

  An optional character vector of labels to display on top of each bar.
  If provided, its length must match that of `values`. Defaults to
  `NULL`.

- show_x_label:

  Logical indicating whether to display the x-axis labels (showing 0 and
  `max_value`) below the bars. Defaults to `TRUE`.

## Value

A character string containing HTML code that renders the horizontal bar
chart.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example with labels and custom colors:
  values <- c(3, 7, 5)
  colors <- c("#FF5733", "#33FF57", "#3357FF")
  labels <- c("Low", "Medium", "High")
  html_chart <- horizontal_bar_chart(values, colors, max_value = 10,
                                     width = 140, height = 60,
                                     labels = labels, show_x_label = TRUE)
  cat(html_chart)
} # }
```
