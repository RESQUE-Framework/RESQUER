% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slider.R
\name{slider}
\alias{slider}
\title{Generate a Slider Indicator in HTML}
\usage{
slider(value, left_anchor, right_anchor)
}
\arguments{
\item{value}{A numeric value between 0 and 1 indicating the position of the triangle on the scale. A value of 0 places the triangle at the far left, while a value of 1 places it at the far right.}

\item{left_anchor}{A character string specifying the label for the left end of the scale.}

\item{right_anchor}{A character string specifying the label for the right end of the scale.}
}
\value{
A character string containing the HTML code for the slider.
}
\description{
This function creates an HTML representation of a slider with a triangle indicator that moves between two anchors. The position of the triangle is based on a value between 0 and 1, which determines its placement on the scale.
}
\examples{
# Generate a slider with the triangle at the middle of the scale
slider(0.5, "Start", "End")

# Generate a slider with the triangle near the start of the scale
slider(0.1, "Low", "High")
}
