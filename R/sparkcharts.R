## This file collect several mini-chart ("sparkline style") to be used in overview tables

#' Sparkline pie chart function.
#'
#' The default colors are grey (for not applicable in the first value slot) and then a color ramp from red to green
#' @param values Absolute numbers.
#' @param colors Overwrite the default colors with a vector of hex color values. Must have the same length as `values`.
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

  sparkline(values, type="pie", sliceColors=colors, borderWidth=1, width = 40, height = 40, ...)
}





#' Create a mini horizontal stacked bar chart in pure HTML
#'
#' This function creates a mini horizontal stacked bar chart in pure HTML/CSS. The scale represents a value within a range, divided into color-coded steps and highlighting a threshold value.
#'
#' @param value numeric value to be represented on the gauge (must be between 0 and 1)
#' @param breaks numeric vector specifying the break points for coloring the gauge steps (must be two elements)
#' @param colors character vector specifying the colors for each step in the gauge (must be three elements)
#' @param width Width in pixels
#' @param height Height in pixels
#' @return a HTML string representing the mini gauge plot
#' @export
#' @examples
#' ministack(0.25)
ministack <- function(
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
  # Vertical positioning for the colored bar
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


#' Generate an HTML <div> tag embedding a Waffle plot using R with optional emoji support
#'
#' This is a workaround to embed small waffle plots in a sparkline style into
#' a kable. Now supports displaying custom emojis instead of colored blocks.
#'
#' @param values A numeric vector of specifying the counts of waffle blocks
#' @param max_value A numeric scalar indicating the maximum number of waffle blocks (so that multiple displays have the same size). If any element of \code{values} exceeds \code{max_value}, a warning is issued and \code{max_value} is updated to the maximum value found. Defaults to 10.
#' @param rows An integer specifying the number of rows in the Waffle plot
#' @param colors A character vector containing colors for each category
#'              (default: c('green', 'green', 'red', 'grey')) for categories "yes",
#'              "partial", "no", "notApplicable" (implicitly merging "yes" and "partial").
#' @param width_px An integer specifying the width of the output image in pixels
#' @param gap_px The gap between blocks in pixels
#' @param emojis A character vector of emojis to use instead of colored blocks. If NULL (default), regular colored blocks are used.
#' @param font_size_px An integer specifying the font size for emojis in pixels. Only used if emojis are provided.
#'
#' @return A character string containing an HTML string representing the waffle plot
#'
#' @examples
#' # Standard colored blocks
#' values <- c(10, 20, 15)
#' waffle_html(values = values)
#'
#' # Using emojis instead
#' waffle_html(values = c(3, 7), emojis = c("\u2B50\uFE0F", "\u25A2"))
#'
#' @export
waffle_html <- function(
    values,
    max_value = 10,
    rows = 1,
    colors = c("#1da412", "#1da412", "#c51819", "#C7C7C7"),
    width_px = 120,
    gap_px = 2,
    emojis = NULL,
    font_size_px = 20
) {
  # Validate inputs
  if(sum(values) > max_value) {
    max_value <- sum(values)
    warning(paste0("More values provided than `max_value`. Increasing `max_value` to ", sum(values)))
  }

  # Check if using emojis or colored blocks
  using_emojis <- !is.null(emojis)

  if(using_emojis) {
    if(length(values) > length(emojis)) {
      stop("Not enough emojis provided for the number of values")
    }

    # For emojis, we'll use a simple string concatenation approach instead of flexbox
    result <- ""

    # Add emojis for each value
    for (i in 1:length(values)) {
      if (values[i] > 0) {
        for (j in 1:values[i]) {
          result <- paste0(result, emojis[i])
        }
      }
    }

    # Add empty blocks for remaining space
    remaining_blocks <- max_value - sum(values)
    if (remaining_blocks > 0) {
      # If the last value has an emoji, use that for empty spaces (typically an empty square)
      empty_emoji <- if(length(emojis) > 0) emojis[length(emojis)] else "\u25A2"
      for (i in 1:remaining_blocks) {
        result <- paste0(result, empty_emoji)
      }
    }

    # Wrap in a span with appropriate font size
    html <- paste0('<span style="font-size: ', font_size_px, 'px;">', result, '</span>')
    return(html)

  } else {
    # Original block-based implementation for colored blocks
    # Calculate number of columns
    cols <- max_value / rows

    # Calculate the block size to ensure squares
    # First calculate block width based on container width and gaps
    block_width <- (width_px - (gap_px * (cols - 1))) / cols

    # Recalculate actual container width and height
    actual_width <- (block_width * cols) + (gap_px * (cols - 1))
    actual_height <- (block_width * rows) + (gap_px * (rows - 1))

    # Start HTML output - add a wrapper div with top and bottom margin
    html <- paste0('<div style="margin: ', gap_px, 'px 0;">\n')

    # Add the flex container
    html <- paste0(html, '  <div style="display: flex; flex-wrap: wrap; width: ',
                   actual_width, 'px; height: ', actual_height, 'px; gap: ', gap_px, 'px;">\n')

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
    remaining_blocks <- max_value - block_count
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
}





#' Convert RRS values to green-red color coding (vectorized)
#'
#' TODO: Provide breakpoints as parameters
#' @param values RRS values (ranging from 0 to 1)
#' @importFrom scales gradient_n_pal
#' @export
get_color <- function(values) {

  my_pal <- gradient_n_pal(
    colours = c("#c51819", "#f7cf07", "#f7cf07", "#3af72c", "#3af72c"),
    values = c(0, 0.08, 0.20, 0.35, 0.6, 1)
  )

  # Generate a vector of colors for 101 steps
  colors_101 <- my_pal(seq(0, 1, length.out = 101))

  # Visualize the palette:
  # barplot(rep(1, 100), col = colors_101, border = NA, space = 0, axes = FALSE)

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
    colors <- get_color(round(values))
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

    sector_radius <- max_radius

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




#' Four layered circles, as a complex way to visualize 4 rigor score components
#' as concentric rings.
#'
#' TODO: Provide color ramp breakpoints as parameters
#' @param outer_width Circle radius in pixels.
#' @param value A value between 0 and 1 (which then is transformed into %)
#' @param colors A vector of four colors, from innermost to outermost color
#' @param weights A vector of 4 numbers specifying the relative weight of each layer.
#'        The weights are normalized to percentages automatically.
#'        Default is c(1, 1, 1, 1) which gives equal 25% spacing.
#' @param sharp_boundaries Whether to create sharp color transitions (TRUE) or smooth blends (FALSE)
#' @export
circle_layer <- function(value, colors, outer_width = 60,
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


# circle_layer(value=0.51, colors=c("#ff0000", "#00ff00", "#0000ff", "#ffff00"), weights=c(1, 1, 1, 4)) |> htmltools::HTML() |> htmltools::html_print()
# circle_layer(value=0.51, colors=c("#ff0000", "#00ff00", "#0000ff", "#ffff00"), weights=c(1, 1, 1, 1)) |> htmltools::HTML() |> htmltools::html_print()
# circle_layer(value=0.51, colors=c("#ff0000", "#00ff00", "#0000ff", "#ffff00"), weights=c(1, 10, 1, 10), sharp_boundaries = FALSE) |> cat()




#' Solid circle with provided background color and centered value
#'
#' @param value Numeric between 0 and 1 (printed as a percentage)
#' @param color Single CSS color string (e.g., "#ff0000" or "red")
#' @param outer_width Circle diameter in pixels (default 60)
#' @export
circle_simple <- function(value, color = NA, outer_width = 60) {
  if (is.na(color)) {
    color <- get_color(value)
  }

  if (value < 0 | value > 1) stop("value must be between 0 and 1.")

  value100 <- round(value * 100)

  html <- sprintf('
<div style="
  width: %dpx;
  height: %dpx;
  border-radius: 50%%;
  background-color: %s;
  display: flex;
  align-items: center;
  justify-content: center;
  position: relative;
">
  <span style="
    color: white;
    font-size: calc(%dpx * 0.35);
    font-weight: bold;
    text-shadow:
      -1px -1px 0 black,
       1px -1px 0 black,
      -1px  1px 0 black,
       1px  1px 0 black;
  ">%s%%</span>
</div>',
                  outer_width, outer_width, color, outer_width, value100
  )

  html
}


# circle_simple(0.97, outer_width = 80) |> htmltools::HTML() |> htmltools::html_print()



#' Create a Horizontal Bar Chart in HTML
#'
#' Generates an HTML snippet representing a horizontal bar chart. Each bar's length is proportional to its corresponding value relative to a maximum value, and each bar is filled with a specified color. Optionally, labels can be placed on the bars, and an x-axis displaying the scale can be shown.
#'
#' @param values A numeric vector of values representing the lengths of the bars.
#' @param colors A character vector of colors (in any valid CSS format) for each bar. Must be the same length as \code{values}.
#' @param max_value A numeric scalar indicating the maximum value for scaling the bars. If any element of \code{values} exceeds \code{max_value}, a warning is issued and \code{max_value} is updated to the maximum value found. Defaults to 10.
#' @param width An integer specifying the overall width (in pixels) of the chart container. Defaults to 140.
#' @param height An integer specifying the overall height (in pixels) of the chart container. Defaults to 60.
#' @param labels An optional character vector of labels to display on top of each bar. If provided, its length must match that of \code{values}. Defaults to \code{NULL}.
#' @param show_x_label Logical indicating whether to display the x-axis labels (showing 0 and \code{max_value}) below the bars. Defaults to \code{TRUE}.
#'
#' @return A character string containing HTML code that renders the horizontal bar chart.
#'
#' @examples
#' \dontrun{
#'   # Example with labels and custom colors:
#'   values <- c(3, 7, 5)
#'   colors <- c("#FF5733", "#33FF57", "#3357FF")
#'   labels <- c("Low", "Medium", "High")
#'   html_chart <- horizontal_bar_chart(values, colors, max_value = 10,
#'                                      width = 140, height = 60,
#'                                      labels = labels, show_x_label = TRUE)
#'   cat(html_chart)
#' }
#'
#' @export
horizontal_bar_chart <- function(values,
                                 colors,
                                 max_value = 10,
                                 width = 140,
                                 height = 60,
                                 labels = NULL,
                                 show_x_label = TRUE) {
  # Validate inputs
  if (length(values) != length(colors)) {
    stop("values and colors must have the same length")
  }
  if (!is.null(labels) && length(labels) != length(values)) {
    stop("If provided, labels must have the same length as values")
  }
  if (max(values) > max_value) {
    warning(paste0("Longer bars than max_value detected; increasing max_value to ", max(values)))
    max_value <- max(values)
  }

  n_bars <- length(values)

  # --- Outer container ---
  # display:inline-block so it can fit inside a table cell.
  # box-sizing:border-box so width/height includes padding.
  outer_style <- paste0(
    "display:inline-block; ",
    "width:", width, "px; ",
    "height:", height, "px; ",
    "font-family:Arial, sans-serif; ",
    "box-sizing:border-box; ",
    "padding:5px; ",
    # Remove overflow:hidden or reduce it if you need to see everything
    # but keep it if you really want to clip any overflow
    "overflow:hidden;"
  )

  # --- Decide how much space to reserve for axis row ---
  axis_height <- if (show_x_label) 12 else 0

  # The bars container will occupy (height - padding - axis) in vertical space
  # We used 5px top + 5px bottom padding = 10 total
  bars_container_height <- height - 10 - axis_height
  if (bars_container_height < 1) {
    bars_container_height <- 1  # fallback
  }

  # --- Layout for bars container ---
  bars_container_style <- paste0(
    "display:flex; flex-direction:column; ",
    # Align them from the top so none get clipped
    "justify-content:flex-start; ",
    "height:", bars_container_height, "px; ",
    "width:100%; ",
    "box-sizing:border-box;"
  )

  # --- Compute each bar's height + spacing ---
  # Suppose we want a small margin between bars (2px). We need to fit
  # all bars plus (n_bars-1)*2px inside bars_container_height.
  # So bar_height = (bars_container_height - 2*(n_bars-1)) / n_bars
  margin_bottom <- 2
  total_margin <- margin_bottom * (n_bars - 1)
  bar_height <- (bars_container_height - total_margin) / n_bars
  if (bar_height < 1) {
    bar_height <- 1  # minimal height if space is tight
  }

  # --- Compute fill fraction for each bar ---
  fractions <- values / max_value

  # Start building HTML
  html <- paste0("<div style='", outer_style, "'>")

  # Bars container
  html <- paste0(html, "<div style='", bars_container_style, "'>")

  # Generate each bar
  for (i in seq_len(n_bars)) {
    # A parent div for the gray "shadow" plus the colored fill
    bar_container_style <- paste0(
      "position:relative; ",
      "background-color:#EEEEEE; ",
      "width:100%; ",
      "height:", bar_height, "px; ",
      # add bottom margin except on last bar
      if (i < n_bars) paste0("margin-bottom:", margin_bottom, "px;")
    )

    # The colored fill as a percentage
    fill_width_percent <- fractions[i] * 100
    fill_style <- paste0(
      "background-color:", colors[i], "; ",
      "width:", fill_width_percent, "%; ",
      "height:100%;"
    )

    # Optional label *on top of* the bar
    label_html <- ""
    if (!is.null(labels)) {
      label_style <- paste0(
        "position:absolute; ",
        "left:10px; ",
        "top:50%; ",
        "transform:translateY(-50%); ",
        "font-size:", max(1, round(bar_height * 0.6)), "px; ",
        "color:black; ",
        "white-space:nowrap;"
      )
      label_html <- paste0("<div style='", label_style, "'>", labels[i], "</div>")
    }

    # Combine bar + fill + label
    bar_html <- paste0(
      "<div style='", bar_container_style, "'>",
      "<div style='", fill_style, "'></div>",
      label_html,
      "</div>"
    )
    html <- paste0(html, bar_html)
  }

  # Close the bars container
  html <- paste0(html, "</div>")

  # Axis row at the bottom (no absolute positioning).
  if (show_x_label) {
    axis_style <- paste0(
      "display:flex; ",
      "justify-content:space-between; ",
      "font-size:9px; ",
      "color:#444444; ",
      "margin-top:2px; ",    # a little gap above the axis
      "height:", axis_height - 2, "px; ",
      "align-items:flex-end;"
    )
    axis_html <- paste0(
      "<div style='", axis_style, "'>",
      "<span>0</span>",
      "<span>", max_value, "</span>",
      "</div>"
    )
    html <- paste0(html, axis_html)
  }

  # Close the outer
  html <- paste0(html, "</div>")
  return(html)
}




# values <- c(45, 23, 67, 12, 39)
# colors <- c("#ff6384", "#36a2eb", "#ffce56", "#4bc0c0", "#9966ff")
# labels <- c("Category A", "Category B", "Category C", "Category D", "Category E")
#
# horizontal_bar_chart(c(1, 1, 2, 3, 20), colors, 20, 20, labels=labels) |> htmltools::HTML() |> htmltools::html_print()
# horizontal_bar_chart(c(1, 4, 2, 10, 6), colors, max_value = 10, width = 120, height = 65) |> htmltools::HTML() |> htmltools::html_print()

# horizontal_bar_chart(c(0,0, 3), colors = brewer.pal(3, "Set1"), width=120, height=60) |> htmltools::HTML() |> htmltools::html_print()
