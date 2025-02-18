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
