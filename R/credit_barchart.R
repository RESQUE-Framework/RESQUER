#' Display a barchart with CRediT roles
#'
#' @param credit_dat The raw data in the format of the `applicant()$credit` field. This is a data frame with two relevant columns: `Role` (the CRediT role) and `Degree` (one of "Lead", "Equal", "Support", "NoRole", or "not applicable"). Roles can appear multiple times (e.g., for multiple papers) and will be aggregated in the plot.
#' @param ordered If `FALSE`, keep alphabetical order. If `TRUE`, sort by strongest contribution.
#' @param compact If `FALSE`, show all 14 roles. If `TRUE`, aggregate the 14 roles into 4 categories.
#' @import ggplot2
#' @export
#' @examples
#' credit_dat <- data.frame(
#'   Role = rep(c("Conceptualization", "Data Curation", "Formal Analysis",
#'     "Funding Acquisition", "Investigation", "Methodology",
#'     "Project Administration", "Resources", "Software", "Supervision",
#'     "Validation", "Visualization", "Writing: Original draft",
#'     "Writing: Review & Editing"), 10),
#'   Degree = sample(c("Lead", "Equal", "Support", "NoRole", "not applicable"),
#'     prob = c(0.1, 0.2, 0.3, 0.3, 0.1), size=140, replace=TRUE)
#' )
#' credit_barchart(credit_dat)
#' credit_barchart(credit_dat, ordered=TRUE)

credit_barchart <- function(credit_dat, ordered=FALSE, compact=FALSE){
  credit_dat$Degree <- factor(credit_dat$Degree, levels=rev(c("Lead", "Equal", "Support", "NoRole", "not applicable")), ordered=TRUE)

  # Bring CRediT roles either in alphabetical order, or order by strongest contribution
  if (ordered==FALSE) {
    credit_dat$Role <- factor(credit_dat$Role,
        levels=unique(credit_dat$Role) |> as.character() |> sort() |> rev(), ordered=TRUE)
  } else {

    credit_dat_ordered <- credit_dat |> group_by(Role) |>
      summarise(
        n_Lead = sum(Degree == "Lead"),
        n_Equal = sum(Degree == "Equal"),
        n_Support = sum(Degree == "Support"),
        n_LeadEqual = n_Lead + n_Equal
      ) |>
      arrange(-n_LeadEqual, -n_Support)

    credit_dat$Role <- factor(credit_dat$Role,
        levels=credit_dat_ordered$Role |> rev(), ordered=TRUE)
  }

  if (compact == TRUE) {

    # remove the "not applicable" level - this distorts the plot
    dat <- credit_dat %>% filter(Degree != "not applicable")

    p <- ggplot(dat, aes(x = Role_collapsed, fill = Degree)) +
      geom_bar(stat = "count", position = "fill") +
      ylab("% of publications")
  } else {
    p <- ggplot(credit_dat, aes(x = Role, fill = Degree)) +
      geom_bar(stat = "count") +
      ylab("# of publications")
  }

   p <- p +
    coord_flip() +
    scale_fill_manual(values = rev(c("grey95", "grey85", "khaki2", "green3", "green4")), breaks = rev(c("not applicable", "NoRole", "Support", "Equal", "Lead"))) +
    theme_minimal() + xlab("")  +
    theme(
      axis.text.y = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    # force whole integers on x-axis
    scale_y_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1))

  return(p)
}
