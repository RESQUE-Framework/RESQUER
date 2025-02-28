## This file collect several mini-chart ("sparkline style") to be used in overview tables

#' Sparkline pie chart function.
#'
#' The default colors are grey (for not applicable in the first value slot) and then a color ramp from red to green
#' @param values Absolute numbers.
#' @param sliceColors Overwrite the default colors with a vector of hex color values. Must have the same length as `values`.
#' @param ... Additional arguments passed to the `sparkline` function.
#' @importFrom sparkline sparkline
#' @importFrom grDevices colorRampPalette
#' @export

sparkpie <- function(values, sliceColors=NA, ...) {
  if (length(values) == 0) {
    stop("No values provided.")
  }
  if (all(is.na(sliceColors))) {
    sliceColors <- c("#EEEEEE", colorRampPalette(c("red", "yellowgreen", "green"))(length(values)-1))
  } else {
    if (length(values) != length(sliceColors)) {
      stop("The number of provided colors does not match the  number of pie sectors.")
    }
  }

  sparkline(values, type="pie", sliceColors=sliceColors, borderWidth=1, ...)
}



#' Create a mini gauge plot
#'
#' This function creates a mini gauge plot using the plotly package. The gauge represents a value within a range, divided into color-coded steps and highlighting a threshold value.
#'
#' @param value numeric value to be represented on the gauge (must be between 0 and 1)
#' @param breaks numeric vector specifying the break points for coloring the gauge steps (must be two elements)
#' @param colors character vector specifying the colors for each step in the gauge (must be three elements)
#' @return a plotly object representing the mini gauge plot
#' @import plotly
#' @export
#' @examples
#' minigauge(0.25)
minigauge <- function(value, breaks=c(.15, .30), colors=c("red", "#f7cf07", "#3af72c")) {
  plot_ly(
  type = "indicator",
  mode = "gauge",
  value = value,
  gauge = list(
    axis = list(
      range = list(0, 1),
      visible = FALSE
    ),
    bar = list(
      thickness = 0.2,
      color = "rgba(0.2,0.2,0.2,1)"
    ),
    steps = list(
      list(range = c(0, breaks[1]), color = colors[1]),
      list(range = c(breaks[1], breaks[2]), color = colors[2]),
      list(range = c(breaks[2], 1), color = colors[3])
    ),
    threshold = list(
      line = list(color = "black", width = 5),    # Increased width from 2 to 4
      thickness = 1.5,                            # Increased thickness from 0.5 to 0.8
      value = value
    ),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "#bbb"
  ),
  width = 80,
  height = 30
) %>% layout(
  margin = list(l = 0, r = 0, t = 0, b = 0),
  paper_bgcolor = "rgba(0,0,0,0)",
  showlegend = FALSE
)
}


#' Create a mini horizontal stacked bar chart
#'
#' This function creates a mini horizontal stacked bar chart using the plotly package. The gauge represents a value within a range, divided into color-coded steps and highlighting a threshold value.
#'
#' @param value numeric value to be represented on the gauge (must be between 0 and 1)
#' @param breaks numeric vector specifying the break points for coloring the gauge steps (must be two elements)
#' @param colors character vector specifying the colors for each step in the gauge (must be three elements)
#' @return a plotly object representing the mini gauge plot
#' @import plotly
#' @export
#' @examples
#' ministack(0.25)
ministack <- function(value, breaks=c(.15, .30), colors=c("red", "#f7cf07", "#3af72c")) {
  plot_ly(width = 120, height = 30) %>%
  config(displayModeBar = FALSE) %>%
  # Add an invisible dummy trace to avoid warnings
  add_trace(
    x = 0,
    y = 0,
    type = "scatter",
    mode = "markers",
    marker = list(opacity = 0)
  ) %>%
  layout(
    shapes = list(
      # Red segment: from 0 to 0.15
      list(
        type = "rect",
        x0 = 0, x1 = breaks[1],
        y0 = 0.2, y1 = 0.8,
        fillcolor = RED,
        line = list(width = 0)
      ),
      # Yellow segment: from 0.15 to 0.30
      list(
        type = "rect",
        x0 = breaks[1], x1 = breaks[2],
        y0 = 0.2, y1 = 0.8,
        fillcolor = YELLOW,
        line = list(width = 0)
      ),
      # Green segment: from 0.30 to 1.0
      list(
        type = "rect",
        x0 = breaks[2], x1 = 1.0,
        y0 = 0.2, y1 = 0.8,
        fillcolor = GREEN,
        line = list(width = 0)
      ),
      # Triangle indicator at value pointing downward
      list(
        type = "path",
        # The triangle is defined with a horizontal base from (0.38,0.65) to (0.46,0.65)
        # and the tip at (0.42,0.6), so the triangle points down.
        path = paste0("M ", value-0.03,",0.65 L ", value+0.03, ",0.65 L ", value, ",0.4 Z"),
        fillcolor = "black",
        line = list(color = "black")
      )
    ),
    # Hide the axes for a clean look
    xaxis = list(range = c(0, 1), showgrid = FALSE, zeroline = FALSE, visible = FALSE),
    yaxis = list(range = c(0, 1), showgrid = FALSE, zeroline = FALSE, visible = FALSE),
    margin = list(l = 5, r = 5, t = 5, b = 5),
    paper_bgcolor = "white"
  )
}



#' Create a mini horizontal stacked bar chart in pure HTML
#'
#' This function creates a mini horizontal stacked bar chart in pure HTML/CSS. The scale represents a value within a range, divided into color-coded steps and highlighting a threshold value.
#'
#' @param value numeric value to be represented on the gauge (must be between 0 and 1)
#' @param breaks numeric vector specifying the break points for coloring the gauge steps (must be two elements)
#' @param colors character vector specifying the colors for each step in the gauge (must be three elements)
#' @return a HTML string representing the mini gauge plot
#' @export
#' @examples
#' ministack_html(0.25)
ministack_html <- function(
    value,
    breaks = c(0.15, 0.30),
    colors = c("red", "#f7cf07", "#3af72c"),
    width = 120,
    height = 25
) {
  # Calculate segment widths (in pixels)
  # E.g., if breaks = c(0.15, 0.30), then widths = (0.15, 0.15, 0.70) of total width
  w1 <- (breaks[1] - 0)        * width
  w2 <- (breaks[2] - breaks[1]) * width
  w3 <- (1        - breaks[2]) * width

  # Vertical positioning for the colored bar (just like y0=0.2 to y1=0.8 in Plotly)
  bar_top    <- 0.2 * height      # 20% down
  bar_height <- 0.6 * height      # 60% tall

  # Triangle placement
  # We'll mimic y=0.4 to y=0.65 in the original (about 12px to 19.5px in a 30px height).
  # For simplicity, define the triangle’s top edge near y=0.4*height.
  # The triangle's left is centered on 'value * width'.
  triangle_left  <- (value * width) - 6  # half the base is 6px
  triangle_top   <- 0.4 * height         # ~12px if height=30
  triangle_color <- "black"

  # Build the HTML in a single string
  # Outer container
  html <- sprintf('
<div style="position: relative; width: %dpx; height: %dpx; background-color: white;">

  <!-- The stacked bar -->
  <div style="
    position: absolute;
    top: %.1fpx;
    left: 0;
    width: %dpx;
    height: %.1fpx;
  ">
    <!-- Red segment -->
    <div style="
      position: absolute;
      top: 0;
      left: 0;
      width: %.1fpx;
      height: 100%%;
      background-color: %s;
    "></div>

    <!-- Yellow segment -->
    <div style="
      position: absolute;
      top: 0;
      left: %.1fpx;
      width: %.1fpx;
      height: 100%%;
      background-color: %s;
    "></div>

    <!-- Green segment -->
    <div style="
      position: absolute;
      top: 0;
      left: %.1fpx;
      width: %.1fpx;
      height: 100%%;
      background-color: %s;
    "></div>
  </div>

  <!-- Downward-pointing triangle indicator -->
  <div style="
    position: absolute;
    top: %.1fpx;
    left: %.1fpx;
    width: 0;
    height: 0;
    border-left: 6px solid transparent;
    border-right: 6px solid transparent;
    border-top: 8px solid %s;
  "></div>

</div>
',
                  width, height,
                  bar_top, width, bar_height,
                  w1, colors[1],
                  w1, w2, colors[2],
                  w1 + w2, w3, colors[3],
                  triangle_top, triangle_left, triangle_color
  )

  html
}



#' Generate an HTML <img> tag embedding a Waffle plot using R
#'
#' This is a workaround to embed small waffle plots in a sparkline style into
#' a kable.
#'
#' @param values A numeric vector of specifying the counts of waffle blocks
#' @param rows An integer specifying the number of rows in the Waffle plot
#' @param colors A character vector containing colors for each category
#'              (default: c('green', 'green', 'red', 'grey')) for categories "yes",
#'              "partial", "no", "notApplicable" (implicitly merging "yes" and "partial").
#' @param width_px An integer specifying the width of the output image in pixels
#' @param gap_px The gap between blocks in pixels
#'
#' @return A character string containing an HTML <img> tag embedding the Waffle plot
#'
#' @import waffle
#' @import ragg
#' @import png
#' @import base64enc
#'
#' @examples
#' values <- c(10, 20, 15)
#' waffle_html(values = values)
#'
#' @export
waffle_html <- function(
    values,
    max_values = 10,
    rows = 1,
    colors = c("#3af72c", "#3af72c", "red", "grey"),
    width_px = 120,
    gap_px = 2
) {
  # Validate inputs
  if(sum(values) > max_values) {
    stop("Sum of values cannot exceed max_values")
  }

  if(length(values) > length(colors)) {
    stop("Not enough colors provided for the number of values")
  }

  # Calculate number of columns
  cols <- max_values / rows

  # Calculate the block size to ensure squares
  # First calculate block width based on container width and gaps
  block_width <- (width_px - (gap_px * (cols - 1))) / cols

  # Recalculate actual container width and height
  actual_width <- (block_width * cols) + (gap_px * (cols - 1))
  actual_height <- (block_width * rows) + (gap_px * (rows - 1))

  # Start HTML output - add a wrapper div with top and bottom margin
  html <- paste0('<div style="margin: ', gap_px, 'px 0;">\n')

  # Add the flex container
  html <- paste0(html, '  <div style="display: flex; flex-wrap: wrap; width: ', actual_width, 'px; height: ', actual_height, 'px; gap: ', gap_px, 'px;">\n')

  # Counter for blocks
  block_count <- 0

  # Add colored blocks for each value
  for(i in 1:length(values)) {
    if (values[i] > 0) {
      for(j in 1:values[i]) {
        html <- paste0(html, '    <div style="width: ', block_width, 'px; height: ', block_width,
                       'px; background-color: ', colors[i], '; margin: 0; padding: 0;"></div>\n')
        block_count <- block_count + 1
      }
    }
  }

  # Add invisible blocks to fill remaining space
  remaining_blocks <- max_values - block_count
  if(remaining_blocks > 0) {
    for(i in 1:remaining_blocks) {
      html <- paste0(html, '    <div style="width: ', block_width, 'px; height: ', block_width,
                     'px; background-color: transparent; margin: 0; padding: 0;"></div>\n')
    }
  }

  # Close the flex container
  html <- paste0(html, '  </div>\n')

  # Close the wrapper div
  html <- paste0(html, '</div>')

  return(html)
}
