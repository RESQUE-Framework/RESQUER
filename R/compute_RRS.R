#' Compute Relative Rigor Score (RRS)
#'
#' This function computes a relative rigor score based on the indicators provided from an applicant.
#'
#' @param applicant The applicant data that has been imported by the `read_RESQUE` function.
#' @param sectors Should sectors be all equally sized ("equal") or weighted by the maximum sum of attainable points in each category ("weighted")?
#' @param verbose Print extra information.
#' @return A tibble containing the dimension (category), maximum points, and relative score for each category, suitable for creating a radar chart.
#' @import dplyr
#' @import stringr
#' @import tibble
#' @importFrom janitor clean_names
#' @importFrom stringr str_detect
#'
#' @export
compute_RRS <- function(applicant, sectors = c("weighted", "equal"), verbose=FALSE) {

  sectors <- match.arg(sectors, choices=c("weighted", "equal"))

  # Which outputs should be scored? At the moment, only publications
  score_list <- applicant$scores$scores[applicant$indicators$rigor_pub == TRUE]
  NaN_check <- sapply(score_list, "[[", "relative_score")
  n_scorable <- sum(!is.nan(NaN_check))

  if (n_scorable == 0) {
    warning("No publications suitable for scoring.")
    return(list(
      overall_score = NA,
      paper_scores = NA,
      n_papers = 0,
      sector_scores = NA,
      radar_dat = NA,
      publication_years = NA
    ))
  }

  scores_all <- get_indicators(sc=score_list)

  scores_all$category <- NA

  # retrieve categories from applicant meta-data
  cat_def <- applicant$meta$forms$config$score_categories[[1]]

  for (cat in 1:nrow(cat_def)) {
    # find out the category ...
    candidates <- which(str_detect(scores_all$indicator, cat_def$cue[cat]))

    # ... and ensure that no "double booking" happens when the regexp matches
    # multiple times:
    if (all(is.na(scores_all$category[candidates]))) {
      scores_all$category[candidates] <- cat_def$title[cat]
    } else {
      stop("A scoring category has been assigned multiple times - check the regexp in your config.yaml for multiple matches.")
    }
  }
  effective_categories <- length(unique(scores_all$category))

  # plausibility check: Which indicators have not been categorized?
  # (<NA> should be 0)
  t1 <- table(scores_all$category, useNA="always")

  if (verbose==TRUE) {
    print(paste0("Number of indicators per scoring category, summed across ", n_scorable, " papers:"))
    print(t1)
    print("Note: If an scorable indicator has not been shown because of a filter condition, it does not show up here.")

    if (t1[is.na(names(t1))] == 0) {
      print("Note: The <NA> category has > 0 indicators: There are uncategorized scorable indicators present.")
    }
  }

  n_uncategorized_indicators <- sum(is.na(scores_all$category))
  if (n_uncategorized_indicators > 0) {
    warning(paste0(n_uncategorized_indicators, " scoring indicators have not been categorized:"))
    warning(print(scores_all[is.na(scores_all$category), ]))
  }


  # each row is one publication; show overall RRS and raw points
  RRS_by_paper_overall <- scores_all %>%
    group_by(output) %>%
    summarise(
      scores = sum(value),
      max_points = sum(max),
      rel_score = scores/max_points
    )

  # fill in missing publications (which have a max score of 0)
  for (i in 1:length(score_list)) {
    if (!i %in% RRS_by_paper_overall$output) {
      RRS_by_paper_overall <- rbind(RRS_by_paper_overall, c(i, NA, NA, NA))
    }
  }
  RRS_by_paper_overall <- RRS_by_paper_overall |> arrange(output)

  RRS_by_paper_overall$doi <- normalize_dois(applicant$rigor_pubs$doi)
  RRS_by_paper_overall <- RRS_by_paper_overall %>% relocate(doi) %>% select(-output)


  #--------------------------------------------------------------------------------
  # each row is one publication; show sector scores
  RRS_by_paper_sector <- scores_all %>%
    group_by(output, category) %>%
    summarise(
      scores = sum(value),
      max_points = sum(max),
      rel_score = scores/max_points
    ) %>%
    ungroup() %>%
    select(-max_points, -scores) %>%
    pivot_wider(names_from=category, values_from=rel_score)


  # fill in missing publications (which have a max score of 0)
  for (i in 1:length(score_list)) {
    if (!i %in% RRS_by_paper_sector$output) {
      RRS_by_paper_sector <- rbind(RRS_by_paper_sector, c(i, rep(NA, effective_categories)))
    }
  }
  RRS_by_paper_sector <- RRS_by_paper_sector |> arrange(output)

  RRS_by_paper_sector$doi <- normalize_dois(applicant$rigor_pubs$doi)
  RRS_by_paper_sector <- RRS_by_paper_sector %>% relocate(doi) %>% select(-output)

  # merge the sector and the overall scores
  RRS_by_paper <- full_join(RRS_by_paper_overall %>% select(doi, RRS_overall = rel_score), RRS_by_paper_sector, by="doi") %>% janitor::clean_names()

  colnames(RRS_by_paper)[2] <- "RRS_overall"
  colnames(RRS_by_paper)[3:ncol(RRS_by_paper)] <- paste0("RRS_", colnames(RRS_by_paper)[3:ncol(RRS_by_paper)])

  RRS_by_category <- scores_all %>%
    group_by(category) %>%
    summarise(
      scores = sum(value),
      max_points = sum(max),
      rel_score = scores/max_points
    ) %>%
    filter(!is.na(category))


  # overall rigor score as in webform:
  # (this code does not work anymore, as the raw scores and max_points
  # are not present in this data frame any more)
  #sum(RRS_by_paper$scores)/sum(RRS_by_paper$max_points)

  # overall score averaged across papers (each paper gets same weight)
  overall_score <- mean(RRS_by_paper$RRS_overall, na.rm=TRUE)

  radar_dat <- tibble(
    dimension = factor(RRS_by_category$category),
    max_points = RRS_by_category$max_points,
    rel_score = RRS_by_category$rel_score
  )

  if (sectors == "equal") {
    # Sectors all have the same width
    radar_dat <- radar_dat %>% mutate(
      xstart = 0:(effective_categories-1),
      xend = 1:effective_categories,
      xmid = (xend-xstart)/2 + xstart
    )
  } else if (sectors == "weighted") {
    # Sector width proportional to max_points
    radar_dat <- radar_dat %>% mutate(
      xstart = c(0, cumsum(max_points)[1:(length(max_points)-1)]),
      xend = cumsum(max_points),
      xmid = (xend-xstart)/2 + xstart
    )
  }

  return(list(
    overall_score = overall_score,
    paper_scores = RRS_by_paper,
    n_papers = sum(!is.na(RRS_by_paper$RRS_overall)),
    sector_scores = RRS_by_category,
    radar_dat = radar_dat,
    publication_years = applicant$rigor_pubs$Year |> as.numeric()
  ))
}

#RRS <- compute_RRS(applicant, sectors="weighted")
#RRS_radarchart(RRS, overall_score = TRUE)






#' Display information about the scoring categories
#'
#' This function shows a table how many indicators are assigned to each scoring
#' category. This counts all indicators that are defined in the packs.json and
#' contribute to scoring (regardless of whether an indicator is missing in that
#' specific applicant).
#'
#' @param applicant The applicant data that has been imported by the `read_RESQUE` function.
#' @param selector The prefix(es) of indicators that should be selected with `starts_with()`
#' @return Nothing (called for it's print output)
#' @importFrom stringr str_detect str_starts
#'
#' @export
RRS_category_summary <- function(applicant, selector=c("P_")) {
  # retrieve categories from applicant meta-data
  cat_def <- applicant$meta$forms$config$score_categories[[1]]

  # Select indicators that actually have scores attached.
  # We do this by looking for the "score" node in the forms.
  indicators_with_scores <- names(applicant$meta$forms$pub$scoring)

  # reduce to indicators that match the selector
  selector_match <- paste0("^(", paste(selector, collapse="|"), ")")
  X <- indicators_with_scores[grepl(selector_match, indicators_with_scores)]

  # reduce to indicators that match the category cues

  # 1. Build a logical matrix: rows = X, cols = cues
  match_mat <- sapply(cat_def$cue, function(pat) grepl(pat, X, perl = TRUE))

  # 2. Count how many categories each element of X matches
  n_matches <- rowSums(match_mat)

  # 3. Check for elements that match more than one cue
  if (any(n_matches > 1L)) {
    i <- which(n_matches > 1L)
    stop("Some inficators match to multiple category cues:\n",
         paste0("  X[", i, "] = ", X[i], collapse = "\n"))
  }

  # 4. Map each X to its single matching category (or NA if no match)
  category <- rep(NA_character_, length(X))
  idx_unique <- which(n_matches == 1L)

  # for those with exactly one TRUE, find which cue it is
  category[idx_unique] <- cat_def$title[
    apply(match_mat[idx_unique, , drop = FALSE], 1, function(z) which(z))
  ]

  # 5. Final result
  match_table <- data.frame(
    indicator = X,
    category = category,
    stringsAsFactors = FALSE
  )

  print("The following indicators are defined in the scoring rules of the
        selected indicators (regardless of whether some indicator are missing
        for this specific applicant):")
  print(match_table |> group_by(category) |> count())

  return(match_table)
}
