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

sparkpie <- function(values, colors=NA, ...) {
  if (length(values) == 0) {
    stop("No values provided.")
  }
  if (all(is.na(colors))) {
    colors <- c("#EEEEEE", colorRampPalette(c("red", "yellowgreen", "green"))(length(values)-1))
  } else {
    if (length(values) != length(colors)) {
      stop("The number of provided colors does not match the  number of pie sectors.")
    }
  }

  sparkline(values, type="pie", sliceColors=colors, borderWidth=1, ...)
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
    height = 12
) {
  # Calculate segment widths (in pixels)
  # E.g., if breaks = c(0.15, 0.30), then widths = (0.15, 0.15, 0.70) of total width
  w1 <- (breaks[1] - 0)        * width
  w2 <- (breaks[2] - breaks[1]) * width
  w3 <- (1        - breaks[2]) * width
  # Vertical positioning for the colored bar (just like y0=0.2 to y1=0.8 in Plotly)
  bar_top    <- 0.2 * height      # 20% down
  bar_height <- 0.6 * height      # 60% tall

  # Calculate triangle dimensions based on bar height
  # Set triangle height to be equal to the bar height
  triangle_height <- bar_height
  # Set triangle base width proportionally to height (keeping a reasonable aspect ratio)
  triangle_base_half <- triangle_height * 0.5  # Each half is 0.5x the height

  # Triangle placement
  triangle_left  <- (value * width) - triangle_base_half  # center on value
  triangle_top   <- bar_top
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
    border-left: %.1fpx solid transparent;
    border-right: %.1fpx solid transparent;
    border-top: %.1fpx solid %s;
  "></div>
</div>
',
                  width, height,
                  bar_top, width, bar_height,
                  w1, colors[1],
                  w1, w2, colors[2],
                  w1 + w2, w3, colors[3],
                  triangle_top, triangle_left, triangle_base_half, triangle_base_half, triangle_height, triangle_color
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
#' @examples
#' values <- c(10, 20, 15)
#' waffle_html(values = values)
#'
#' @export
waffle_html <- function(
    values,
    max_values = 10,
    rows = 1,
    colors = c("#1da412", "#1da412", "#c51819", "#C7C7C7"),
    width_px = 120,
    gap_px = 2
) {
  # Validate inputs
  if(sum(values) > max_values) {
    max_values <- sum(values)
    warning(paste0("More values provided than `max_values`. Increasing `max_values` to ", sum(values)))
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





#' Convert RRS values to green-red color colding (vectorized)
#'
#' TODO: Provide breakpoints as parameters
#' @param values RRS values (ranging from 0 to 1)
#' @importFrom scales gradient_n_pal
#' @export
get_color <- function(values) {

  my_pal <- gradient_n_pal(
    colours = c("#c51819", "#f7cf07", "#f7cf07", "#3af72c", "#3af72c"),
    values = c(0, 0.15, 0.30, 0.4, 0.6, 1)
  )

  # Generate a vector of colors for 101 steps
  colors_101 <- my_pal(seq(0, 1, length.out = 101))

  # Visualize the palette:
  #barplot(rep(1, 100), col = colors_101, border = NA, space = 0, axes = FALSE)

  colors_101[round(values*100)+1]
}





#' Create a radial chart in pure HTML/CSS with equal sector angles
#'
#' This function creates a radial chart visualization using only HTML and CSS.
#' Each sector has the same angular size and a radius proportional to its value.
#'
#' @param values numeric vector of values (0-1) representing how filled each sector should be
#' @param colors character vector of colors for each sector
#' @param width numeric width of the chart in pixels
#' @param height numeric height of the chart in pixels
#' @param border_width numeric width of the outer circle border
#' @param border_color character color of the outer circle border
#' @param center_size numeric size of the empty center (as percentage of total radius)
#' @return a HTML string representing the radial chart
#' @export
#' @examples
#' radial_chart(c(80, 60, 40, 90, 30))
#' radial_chart(values=c(1, .5, .25, .1), center_size = 0)
radial_chart <- function(
    values,
    colors = NULL,
    full_radius = TRUE,
    width = 300,
    height = 300,
    border_width = 1,
    border_color = "#cccccc",
    center_size = 0
) {
  if (length(values) == 0) {
    stop("No values provided.")
  }

  # Normalize values to percentages (0-100)
  values <- pmin(pmax(values*100, 0), 100)

  # Calculate equal angles for each sector
  sector_count <- length(values)
  angle_per_sector <- 360 / sector_count

  # Generate default colors if none provided
  if (is.null(colors)) {
    colors <- colors_100[round(values)]
  } else if (sector_count != length(colors)) {
    stop("The number of provided colors does not match the number of sectors.")
  }

  # Calculate the radius of the chart (half of width or height, whichever is smaller)
  max_radius <- min(width, height) / 2

  # Begin building HTML
  html <- sprintf('<div style="position: relative; width: %dpx; height: %dpx; margin: 0 auto;">', width, height)

  # Add the outer circle border
  html <- paste0(html, sprintf('
  <div style="
    position: absolute;
    top: 0;
    left: 0;
    width: %dpx;
    height: %dpx;
    border-radius: 50%%;
    border: %dpx solid %s;
    box-sizing: border-box;
  "></div>',
                               width, height, border_width, border_color
  ))

  # Center coordinates
  center_x <- width / 2
  center_y <- height / 2

  # Create each sector with individual clip-path polygons
  for (i in 1:sector_count) {
    # Calculate angles in radians for this sector
    start_angle_deg <- (i - 1) * angle_per_sector
    end_angle_deg <- i * angle_per_sector

    start_angle <- start_angle_deg * pi / 180
    end_angle <- end_angle_deg * pi / 180

    # Calculate sector radius based on the value (percentage filled)
    if (full_radius == FALSE) {
      sector_radius <- max_radius * (values[i] / 100)
    } else {
      sector_radius <- max_radius
    }


    # If sector_radius is too small, skip this sector
    if (sector_radius < 1) next

    # Calculate points for clip-path polygon
    # Start at center
    clip_points <- c(sprintf("%.1fpx %.1fpx", center_x, center_y))

    # Add point at start angle
    x1 <- center_x + cos(start_angle) * sector_radius
    y1 <- center_y + sin(start_angle) * sector_radius
    clip_points <- c(clip_points, sprintf("%.1fpx %.1fpx", x1, y1))

    # Add arc points (approximating the arc with multiple points)
    arc_steps <- max(3, round(angle_per_sector / 10))  # More points for larger angles
    for (j in 1:arc_steps) {
      angle <- start_angle + (end_angle - start_angle) * j / arc_steps
      x_arc <- center_x + cos(angle) * sector_radius
      y_arc <- center_y + sin(angle) * sector_radius
      clip_points <- c(clip_points, sprintf("%.1fpx %.1fpx", x_arc, y_arc))
    }

    # Add point at end angle
    x2 <- center_x + cos(end_angle) * sector_radius
    y2 <- center_y + sin(end_angle) * sector_radius
    clip_points <- c(clip_points, sprintf("%.1fpx %.1fpx", x2, y2))

    # Join all points
    polygon <- paste(clip_points, collapse = ", ")

    # Add the sector with a polygon clip-path
    html <- paste0(html, sprintf('
    <div style="
      position: absolute;
      top: 0;
      left: 0;
      width: %dpx;
      height: %dpx;
      background-color: %s;
      clip-path: polygon(%s);
    "></div>',
                                 width, height, colors[i], polygon
    ))
  }

  # Add a circle in the center to create a "donut" effect if requested
  if (center_size > 0) {
    center_px <- (max_radius * 2 * center_size / 100)
    html <- paste0(html, sprintf('
    <div style="
      position: absolute;
      top: %.1fpx;
      left: %.1fpx;
      width: %.1fpx;
      height: %.1fpx;
      background: white;
      border-radius: 50%%;
    "></div>',
                                 (height - center_px) / 2, (width - center_px) / 2,
                                 center_px, center_px
    ))
  }

  # Close the container div
  html <- paste0(html, '</div>')

  return(html)
}




#' Four layered circles
#'
#' TODO: Provide color ramp breakpoints as parameters
#' @param outer_width Circle radius in pixels.
#' @param value A value between 0 and 1 (which then is transformed into %)
#' @param colors A vector of four colors, from innermost to outermost color
#' @param weight A vector of 4 numbers specifying the relative weight of each layer.
#'        The weights are normalized to percentages automatically.
#'        Default is c(1, 1, 1, 1) which gives equal 25% spacing.
#' @param sharp_boundaries Whether to create sharp color transitions (TRUE) or smooth blends (FALSE)
#' @export
circle_layer_html <- function(value, colors, outer_width = 60,
                              weights = c(1, 1, 1, 1), sharp_boundaries = FALSE) {

  # Ensure we have 4 weights
  if (length(weights) != 4) {
    stop("Weight parameter must be a vector of 4 values")
  }

  # Ensure colors is of length 4
  if (length(colors) != 4) {
    stop("Colors parameter must be a vector of 4 values")
  }

  value100 <- round(value*100)

  # Calculate the 3 breakpoints between the 4 colors
  # The first color starts at 0%, the last color ends at 100%
  total_weight <- sum(weights)
  breakpoint1 <- (weights[1] / total_weight * 100) |> round()
  breakpoint2 <- ((weights[1] + weights[2]) / total_weight * 100) |> round()
  breakpoint3 <- ((weights[1] + weights[2] + weights[3]) / total_weight * 100) |> round()



  if (sharp_boundaries) {
    # Create gradient with sharp boundaries
    gradient <- sprintf('radial-gradient(circle closest-side,
      %s 0%%, %s %g%%,
      %s %g%%, %s %g%%,
      %s %g%%, %s %g%%,
      %s %g%%, %s 100%%)',
                        colors[1], colors[1], breakpoint1,
                        colors[2], breakpoint1, colors[2], breakpoint2,
                        colors[3], breakpoint2, colors[3], breakpoint3,
                        colors[4], breakpoint3, colors[4]
    )

  } else {
    # Create gradient with smooth transitions
    gradient <- sprintf('radial-gradient(circle closest-side,
         %s 0%%,
      %s %g%%,
      %s %g%%,
      %s %g%%,
      %s 100%%)',
                        colors[1],
                        colors[2], breakpoint1,
                        colors[3], breakpoint2,
                        colors[4], breakpoint3,
                        colors[4]  # Add explicit 100% stop for the outermost color
    )
  }




  html <- sprintf('
<div style="
  --div-dim: %dpx;  /* Outer dimension */
  width: var(--div-dim);
  height: var(--div-dim);
  border-radius: 50%%;
  background: %s;
  display: flex;
  align-items: center;
  justify-content: center;
  position: relative;
">
 <span style="
    color: white;
    font-size: calc(var(--div-dim) * 0.35); /* 30%% of outer div size */
    font-weight: bold;
    text-shadow:
      -1px -1px 0 black,
       1px -1px 0 black,
      -1px  1px 0 black,
       1px  1px 0 black;
  ">%s%%</span>
</div>',
                  outer_width, gradient,
                  value100
  )
  html
}


# circle_layer_html(value=0.51, colors=c("#ff0000", "#00ff00", "#0000ff", "#ffff00"), weights=c(1, 1, 1, 4)) |> cat()
# circle_layer_html(value=0.51, colors=c("#ff0000", "#00ff00", "#0000ff", "#ffff00"), weights=c(1, 10, 1, 10), sharp_boundaries = FALSE) |> cat()
