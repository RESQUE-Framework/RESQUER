#' Radar Chart for Relative Rigor Score
#'
#' This function generates a radar chart that visualizes the rigor profile for a given applicant's data.
#'
#' @param RRS The output from the `compute_RRS` function.
#' @param overall_score Show the overall score in the plot?
#' @param base_size Base font size
#' @param minimal If TRUE, a very simple chart without any legends is drawn
#' @param show.legend Show the legend to the right?
#' @param show.years Show a mini histogram to the right with the publication years?
#' @param show.n_pub Show the number of publication on which the chart is based?
#' @param scale Should the sectors be filled in a `"linear"` way (i.e., 50% is 50% of the radial height), or to the `"area"` (i.e., 50% is 50% of the area). Default = `"area"`.
#'
#' @return A radar chart visualizing the rigor profile for the applicant, showing the distribution of scores across different dimensions.
#'
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import patchwork
#'
#' @examples
#' radar_dat <- data.frame(
#'   dimension = c("Open Data", "Open Materials", "Preregistration",
#'     "Reproducible Code \n& Verification", "Theorizing \n& Formal Modeling"),
#'   max_points = rep(10, 5),
#'   rel_score=c(1, 0.75, 0.5, 0.25, 0.1),
#'   xstart = 0:4,
#'   xend = 1:5,
#'   xmid=0:4 + 0.5
#' )
#' RRS <- list(radar_dat=radar_dat, overall_score=0.5, n_papers=4,
#'           publication_years=c(2004, 2007, 2015, 2015))
#' RRS_radarchart(RRS)
#' RRS_radarchart(RRS, scale="linear")
#'
#' @importFrom ggplot2 ggplot geom_rect coord_polar geom_hline geom_text xlab ylab ggtitle scale_x_continuous scale_y_continuous theme_void guides scale_fill_brewer
#' @importFrom dplyr %>%
#' @importFrom scales brewer_pal
#'
#' @export
RRS_radarchart <- function(RRS, overall_score=FALSE, minimal=FALSE, show.legend=FALSE, show.n_pub=TRUE, show.years=TRUE, base_size=14, scale="area") {

  # Standardized and categorized rigor scores:
  # TODO: Standarize within category, not over all
  # Quantiles in Franka's study were:
  # 0.63 (top 1%), 0.54 (top 10%), 0.50 (top 20%), 0.30 (top 50%)
  # radar_applicant$indicators$norm_rigor <- cut(radar_applicant$indicators$rel_score, breaks=c(0, 0.30, .50, .54, 1),
  #  labels=c("low50", "top50", "top20", "top10"))

  # p1: Radar chart

  # transform to scale to area
  RRS$radar_dat$rel_score2 <- RRS$radar_dat$rel_score
  if (scale=="area") {
    RRS$radar_dat$rel_score2 <- sqrt(RRS$radar_dat$rel_score)
  }

  p1 <- RRS$radar_dat %>% ggplot() +
    geom_rect(aes(xmin = xstart, xmax = xend, ymin = 0, ymax = rel_score2, fill = dimension), show.legend = show.legend) +
    coord_polar("x", start = 0) +
    xlab("") + ylab("") +
    scale_x_continuous(labels = NULL, breaks = NULL) +
    scale_y_continuous(labels = NULL, breaks = NULL, limits = c(0, 1.2)) +
    theme_void(base_size = base_size)

  subtitle <- ""

  if (!minimal) {
    p1 <- p1 +
      geom_rect(aes(xmin = xstart, xmax = xend, ymin = rel_score2, ymax = 1), fill = "#EFEFEF", color = "#DEDEDE") +
      guides(fill = guide_legend("Rigor Domain")) +
      scale_fill_brewer(palette = "Set3") +
      geom_text(aes(x = xmid, y = 1, label = dimension), vjust = 0, hjust = 0.5, size = base_size / 5)

    if (overall_score == TRUE) {
      subtitle <- paste0(subtitle, "Overall score = ", round(RRS$overall_score * 100), "%. ")
    }
    if (show.n_pub == TRUE) {
      subtitle <- paste0(subtitle, "Chart is based on ", RRS$n_papers, ifelse(RRS$n_papers == 1, " publication", " publications"), ".")
    }
    if (!show.n_pub & !overall_score) {
      p1 <- p1 + theme(plot.margin = unit(c(-2.5, 0, -1, 0), 'cm'))
    }
  } else {
    p1 <- p1 + guides(fill = "none") +
      geom_hline(yintercept = 1, col = "grey80") +
      theme(plot.margin = unit(rep(-2.5, 4), 'cm'))
    return(p1)
  }

  # Set the title for p1 and adjust its theme
  p1 <- p1 +
    labs(title = "Relative Rigor Score") +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.5, vjust = 0, margin = margin(b = 10))
    )


  if (show.years == FALSE) {
    return(p1)
  }

  #------------------------------------------------------------------------
  # Year histogram

  years <- na.omit(RRS$publication_years)

  # Ensure middle year is a full year
  median_year <- floor(median(years))

  year_breaks <- c(min(years), median_year, max(years))
  if (any(diff(year_breaks) <= 2)) {
    year_breaks <- c(min(years), max(years))
  }

  year_chart <- ggplot(data.frame(Year = years), aes(x = Year)) +
    geom_histogram(binwidth = 1, fill = "#4285F4", color = "white", alpha = 0.8) +
    scale_x_continuous(breaks = year_breaks) +
    labs(
      title = "Years of publication",
      x = "",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 11, margin = margin(t = 0, b = 0)),
      plot.margin = unit(c(-2.5, 0, -1, 0), 'cm'),
      axis.ticks.length.x = unit(0, "pt"),
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.5, vjust = 0, margin = margin(b = 10))
    )

  # Wrap year_chart in a 3-row layout:
  # Row 1: spacer
  # Row 2: the histogram
  # Row 3: spacer
  # The heights=c(1,2,1) means the middle row (the histogram) is 2 units tall
  # and each spacer row is 1 unit tall. Adjust to taste.
  years_plot_centered <- plot_spacer() / year_chart / plot_spacer() +
    plot_layout(heights = c(1, 3, 1))

  # Now place p1 (left) and the centered histogram (right) side by side.
  # widths = c(4, 1) controls the relative width of the two columns.
  combined_plot <- p1 + years_plot_centered +
    plot_layout(widths = c(4, 1)) +
    plot_annotation(caption = subtitle) &
    theme(plot.caption = element_text(size = 12, hjust = 0.5))

  combined_plot

}




# Full Circles at specific sector heights
#geom_hline(yintercept=0.30, col="grey30") +  # top 50%
#geom_hline(yintercept=0.54, col="grey50") + # top 10%
#geom_hline(yintercept=0.63, col="grey70") +  # top  1%

# For sector specific norm values
#geom_segment(x=0, xend=10, y=0.30, yend=0.30, col="grey30") +

#geom_text(x=0, y=0.30, label = "Top 50%", col="grey30", size=base_size/4, vjust=-0.2) +
#geom_text(x=0, y=0.54, label = "Top 10%", col="grey50", size=base_size/4, vjust=-0.2) +
#geom_text(x=0, y=0.63, label = "Top 1%" , col="grey50", size=base_size/4, vjust=-0.2) +
