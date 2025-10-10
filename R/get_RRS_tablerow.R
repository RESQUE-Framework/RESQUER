#' Generate RRS (Relative Rigor Score) table row for an applicant
#'
#' Creates a data frame row containing the applicant's name, overall RRS score,
#' and sector-specific scores with visualizations for Open Data, Open Materials,
#' Preregistration, and Reproducible Code categories. For the overview report,
#' multiple rows can be combined into a summary table.
#'
#' @param applicant The applicant information, as returned by the `preprocess` function.
#' @param add_scores Shall the scores for the sectors be added as columns?
#' (This is needed for sorting in the overview report.)
#' @return A data frame with one row containing:
#'   \describe{
#'     \item{applicant}{Character string of the applicant's last name}
#'     \item{Relative Rigor Score}{HTML visualization of the overall RRS score as a circle}
#'     \item{Open Data}{HTML visualization combining waffle chart and ministack, or "N/A" if not applicable}
#'     \item{Open Material}{HTML visualization combining waffle chart and ministack, or "N/A" if not applicable}
#'     \item{Preregistration}{HTML visualization combining waffle chart and ministack, or "N/A" if not applicable}
#'     \item{Reproducible Code/Reproducible Code & Verification}{HTML visualization combining waffle chart and ministack, or "N/A" if not applicable}
#'   }
#'
#' @details
#' The function extracts sector-specific scores from the applicant's RRS data and creates
#' HTML visualizations for each category. The visualizations consist of:
#' \itemize{
#'   \item A circular visualization for the overall RRS score
#'   \item Waffle charts showing the distribution of practices
#'   \item Ministack visualizations showing the relative scores
#' }
#'
#' The function handles missing data gracefully by returning "N/A" for categories
#' without scores.
#'
#' @examples
#' \dontrun{
#' # Example applicant structure
#' applicant <- list(
#'   meta = list(FullName = "John Doe"),
#'   RRS = list(
#'     overall_score = 0.75,
#'     sector_scores = data.frame(
#'       category = c("Open Data", "Open Materials", "Preregistration", "Reproducible Code"),
#'       rel_score = c(0.8, 0.7, 0.6, 0.9),
#'       max_points = c(10, 10, 10, 10)
#'     )
#'   ),
#'   OS_pie = list(
#'     OpenData = c(Yes = 5, Aggregate = 2, Partial = 1, No = 1, notApplicable = 1),
#'     OpenMaterial = c(Yes = 3, Partial = 2, No = 3, notApplicable = 2),
#'     Prereg = c("Registered Report" = 2, Preregistration = 3,
#'                "Not preregistered" = 3, "Not Applicable" = 2),
#'     OpenCode = c(Yes = 6, Partial = 2, No = 1, notApplicable = 1)
#'   )
#' )
#'
#' result <- get_RRS_tablerow(applicant)
#' }
#'
#' @importFrom dplyr filter pull
#' @importFrom stringr str_detect
#'
#' @export
get_RRS_tablerow <- function(applicant, add_scores=FALSE) {
  tab_RRS <- data.frame(applicant=applicant$meta$LastName)

  RRS_overall <- applicant$RRS$overall_score

  # Extract the 4 default categories
  RRS_OD <- applicant$RRS$sector_scores %>%
    filter(category == "Open Data") %>%
    pull(rel_score) %>%
    { if (length(.) == 0) NA else . }

  RRS_OM = applicant$RRS$sector_scores %>%
    filter(category == "Open Materials") %>%
    pull(rel_score) %>%
    { if (length(.) == 0) NA else . }

  RRS_PreReg = applicant$RRS$sector_scores %>%
    filter(category == "Preregistration") %>%
    pull(rel_score) %>%
    { if (length(.) == 0) NA else . }

  RRS_OC = applicant$RRS$sector_scores %>%
    filter(str_detect(category, "Reproducible Code")) %>%
    pull(rel_score) %>%
    { if (length(.) == 0) NA else . }


  #TODO: generalize to any number of quality sectors - add additional
  # sectors beyond these standard sectors.
  # Take the information on requested sectors from the config.yaml

  tab_RRS[, "Relative Rigor Score"] <- NA

  # Original version with complex layer circle (which requires that all 4 OS scores are available)
  # RRS_vec <- c(
  #   RRS_OD,
  #   RRS_OM,
  #   RRS_PreReg,
  #   RRS_OC)
  # RRS_vec_sorted <- RRS_vec[order(RRS_vec, decreasing = TRUE)]
  # tab_RRS[1, "Relative Rigor Score"] <- circle_layer(
  #     value = RRS_overall[1],
  #     colors = get_color(RRS_vec_sorted),
  #     weights = applicant$RRS$sector_scores$max_points[order(RRS_vec, decreasing = TRUE)],
  #     outer_width = 60)

  tab_RRS[1, "Relative Rigor Score"] <- circle_simple(
    value = RRS_overall[1], outer_width = 60)

  # tab_RRS[1, "Relative Rigor Score"] |> htmltools::HTML() |> htmltools::html_print()

  # Create the table with the visualizations:
  # 1. The waffle chart for each submitted publication
  # 2. Below that: The stacked bar chart with the triangle indicator of the score.

  if (!is.na(RRS_OD)) {
    # Here we have three "green" squares: Yes, Partial and Aggregate
    tab_RRS[, "Open Data"] <- paste(
      applicant$OS_pie$OpenData[c("Yes", "Aggregate", "Partial", "No", "notApplicable")] |> unlist() |> waffle_html(colors = c("#1da412", "#1da412", "#1da412", "#c51819", "#C7C7C7")),
      sapply(RRS_OD, ministack, height=14)
    )
  } else {
    tab_RRS[, "Open Data"] <- "<span style='color:gray;'>N/A</span>"
  }

  if (!is.na(RRS_OM)) {
    tab_RRS[, "Open Material"] <- paste(
      applicant$OS_pie$OpenMaterial[c("Yes", "Partial", "No", "notApplicable")] |> unlist() |> waffle_html(),
      sapply(RRS_OM, ministack, height=14)
    )
  } else {
    tab_RRS[, "Open Material"] <- "<span style='color:gray;'>N/A</span>"
  }

  if (!is.na(RRS_PreReg)) {
    tab_RRS[, "Preregistration"] <- paste(
      applicant$OS_pie$Prereg[c("Registered Report", "Preregistration", "Not preregistered", "Not Applicable")] |> unlist() |> waffle_html(),
      sapply(RRS_PreReg, ministack, height=14)
    )
  } else {
    tab_RRS[, "Preregistration"] <- "<span style='color:gray;'>N/A</span>"
  }


  if (!is.na(RRS_OC)) {

    # refine the heading, depending on whether "verification" was part of the indicators
    if (all(is.na(applicant$rigor_pubs$P_IndependentVerification))) {
      code_heading <- "Reproducible Code"
    } else {
      code_heading <- "Reproducible Code \n& Verification"
    }

    tab_RRS[, code_heading] <- paste(
      applicant$OS_pie$OpenCode[c("Yes", "Partial", "No", "notApplicable")] |> unlist() |> waffle_html(),
      sapply(RRS_OC, ministack, height=14)
    )
  } else {
    tab_RRS[, "Reproducible Code"] <- "<span style='color:gray;'>N/A</span>"
  }

  if (add_scores == TRUE) {
    tab_RRS$RRS_overall <- RRS_overall
    tab_RRS$RRS_OD <- RRS_OD
    tab_RRS$RRS_OM <- RRS_OM
    tab_RRS$RRS_OC <- RRS_OC
    tab_RRS$RRS_PreReg <- RRS_PreReg
  }

  return(tab_RRS)
}

if (1==0) {
library(DT)

  applicant <- list(
    meta = list(applicant = "Doe"),
    RRS = list(
      overall_score = 0.75,
      sector_scores = data.frame(
        category = c("Open Data", "Preregistration", "Reproducible Code"),
        rel_score = c(0.8, 0.6, 0.9),
        max_points = c(10, 10, 10)
      )
    ),
    OS_pie = list(
      OpenData = c(Yes = 5, Aggregate = 2, Partial = 1, No = 1, notApplicable = 1),
      #OpenMaterial = c(Yes = 3, Partial = 2, No = 3, notApplicable = 2),
      Prereg = c("Registered Report" = 2, Preregistration = 3,
                 "Not preregistered" = 3, "Not Applicable" = 2),
      OpenCode = c(Yes = 6, Partial = 2, No = 1, notApplicable = 1)
    )
  )

get_RRS_tablerow(applicant, add_scores = TRUE) |> datatable(
  escape=FALSE,
  rownames=FALSE,
  width = '100%',
  options = list(
    ordering = FALSE,
    dom = 't',
    scrollX = FALSE)
  )
}
