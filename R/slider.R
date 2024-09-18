#' Generate a Slider Indicator in HTML
#'
#' This function creates an HTML representation of a slider with a triangle indicator that moves between two anchors. The position of the triangle is based on a value between 0 and 1, which determines its placement on the scale.
#'
#' @param value A numeric value between 0 and 1 indicating the position of the triangle on the scale. A value of 0 places the triangle at the far left, while a value of 1 places it at the far right.
#' @param left_anchor A character string specifying the label for the left end of the scale.
#' @param right_anchor A character string specifying the label for the right end of the scale.
#'
#' @return A character string containing the HTML code for the slider.
#' @export
#'
#' @examples
#' # Generate a slider with the triangle at the middle of the scale
#' slider(0.5, "Start", "End")
#'
#' # Generate a slider with the triangle near the start of the scale
#' slider(0.1, "Low", "High")

slider <- function(value, left_anchor, right_anchor) {
  paste0('
<div style="display: flex; align-items: center;">
   <!-- Left Anchor -->
    <div style="margin-right: 10px; font-size: 75%">', left_anchor, '</div>
    <!-- Scale Container -->
    <div style="width: 100%; height: 10px; background-color: #e0e0e0; position: relative;">
    <div style="width: 0; height: 0; border-left: 5px solid transparent; border-right: 5px solid transparent; border-bottom: 10px solid #007bff; position: absolute; left: ', value*100, '%; transform: translateX(-50%);"></div>
    </div>
    <!-- Right Anchor -->
    <div style="margin-left: 10px; font-size: 75%">', right_anchor, '</div>
</div>
')
}
