#' Compute Relative Rigor Score (RRS)
#'
#' This function computes the reportable Relative Rigor Score (RRS) based on
#' the indicators provided from an applicant. Unlike the preliminary
#' `applicant$scores$overall` calculated by [read_RESQUE()], it restricts the
#' calculation to eligible publications and excludes records without a DOI.
#' Use the returned `overall_score` for reporting the RRS.
#'
#' @param applicant The applicant data that has been imported by the `read_RESQUE()` and preprocessed by the `preprocess()` function.
#' @param sectors Should sectors be all equally sized ("equal") or weighted by the maximum sum of attainable points in each category ("weighted")?
#' @param verbose Print extra information.
#' @return A tibble containing the dimension (category), maximum points, and relative score for each category, suitable for creating a radar chart.
#' @import dplyr
#' @import stringr
#' @import tibble
#' @importFrom janitor clean_names
#' @importFrom stringr str_detect
#' @importFrom stats setNames
#'
#' @export
compute_RRS <- function(applicant, sectors = c("weighted", "equal"), verbose=FALSE) {

  if (is.null(applicant$indicators$rigor_pub)) {
    stop("Applicant data does not contain the 'rigor_pub' field. Please ensure that the applicant has been preprocessed correctly by the `preprocess()` function.")
  }

  sectors <- match.arg(sectors, choices=c("weighted", "equal"))

  # Derive the reportable RRS from eligible publications, rather than using
  # applicant$scores$overall, which is the preliminary score across all outputs.
  score_list <- applicant$scores$scores[applicant$indicators$rigor_pub == TRUE, ]

  # Remove outputs without any scoring indicators (these have max=0 and relative=NA)
  # and without doi (these are not reproducible)
  score_list <- score_list[!is.na(score_list$relative) & !is.na(score_list$doi), ]
  n_scorable <- sum(!is.na(score_list$relative))

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


  RRS_by_category <- data.frame()
  for (cat in 1:nrow(score_list$categories[[1]])) {
    max <- sapply(score_list$categories, function(x) x[cat, "max"])
    score <- sapply(score_list$categories, function(x) x[cat, "score"])
    RRS_by_category <- rbind(RRS_by_category, data.frame(
      category = score_list$categories[[1]]$title[cat],
      max_points = sum(max, na.rm=TRUE),
      scores = sum(score, na.rm=TRUE)
    ))
  }

  RRS_by_category$rel_score <- RRS_by_category$scores / RRS_by_category$max_points
  RRS_by_category <- RRS_by_category %>%
    # The JavaScript scorer returns every configured category, including
    # categories for which no scoring indicator applies to any publication.
    # Such categories have a total maximum of zero and cannot define a
    # meaningful relative score or radar-chart sector.
    filter(!is.na(category), max_points > 0)

  # Keep one authoritative list of categories that can actually contribute to
  # the RRS. Reusing this list below ensures that the category summary, the
  # per-publication sector columns, and the radar-chart geometry all describe
  # exactly the same set of sectors.
  effective_categories <- nrow(RRS_by_category)
  scored_categories <- RRS_by_category$category

  # each row is one study (take care of multistudy papers); show overall RRS and raw points
  RRS_by_study_overall <- score_list |> select(doi, P_MultiStudy_Selected, scores=score, max_points=max, rel_score=relative)

  # remove rows with 0 max_points (these are publications that have no scoring indicators)
  RRS_by_study_overall <- RRS_by_study_overall %>% filter(max_points > 0)

  # fill in missing publications (which have a max score of 0)
  for (i in 1:nrow(applicant$scores$scores)) {
    if (!applicant$scores$scores$doi[i] %in% RRS_by_study_overall$doi) {
      RRS_by_study_overall <- rbind(RRS_by_study_overall, data.frame(
        doi = applicant$scores$scores$doi[i], 
        P_MultiStudy_Selected = applicant$scores$scores$P_MultiStudy_Selected[i],
        scores = 0, 
        max_points = 0, 
        rel_score = NA)
      )
    }
  }
  

  #--------------------------------------------------------------------------------
  # each row is one publication; extract sector scores

  RRS_by_study_sector <- data.frame()
  for (i in 1:nrow(score_list)) {
    cat_long <- score_list[i, "categories"][[1]]
    # Drop categories whose maximum is zero across the complete applicant
    # record. Without this filter, pivot_wider() would create columns for
    # categories that were removed from RRS_by_category above, leaving the
    # sector table and the radar data with incompatible schemas.
    cat_long <- cat_long |> filter(title %in% scored_categories)
    cat_long$rel_score <- cat_long$score / cat_long$max
    cat_wide <- pivot_wider(cat_long |> select(title, rel_score), names_from=title, values_from=rel_score)

    RRS_by_study_sector <- rbind(RRS_by_study_sector, data.frame(
      doi = score_list$doi[i],
      P_MultiStudy_Selected = score_list$P_MultiStudy_Selected[i],
      cat_wide
    ))
  }

  # Fill in missing publications (which have a maximum score of zero).
  # Derive each placeholder from the existing sector-table schema instead of
  # calculating its number of columns. This remains correct when configured
  # categories have no applicable indicators and therefore do not contribute a
  # sector column.
  for (i in 1:nrow(applicant$scores$scores)) {
    if (!applicant$scores$scores$doi[i] %in% RRS_by_study_sector$doi) {
      empty_row <- RRS_by_study_sector[0, , drop = FALSE]
      empty_row[1, ] <- NA
      empty_row$doi <- applicant$scores$scores$doi[i]
      empty_row$P_MultiStudy_Selected <- applicant$scores$scores$P_MultiStudy_Selected[i]
      RRS_by_study_sector <- rbind(RRS_by_study_sector, empty_row)
    }
  }

  # merge the sector and the overall scores; bring them into the original order of the publications
  RRS_by_study <- full_join(RRS_by_study_overall %>% select(doi, P_MultiStudy_Selected, RRS_overall = rel_score), RRS_by_study_sector, by=c("doi", "P_MultiStudy_Selected")) %>% janitor::clean_names()

  colnames(RRS_by_study)[2] <- "P_MultiStudy_Selected"
  colnames(RRS_by_study)[3] <- "RRS_overall"
  colnames(RRS_by_study)[4:ncol(RRS_by_study)] <- paste0("RRS_", colnames(RRS_by_study)[4:ncol(RRS_by_study)])

  # bring into original order of studies (doi + study selector)
  order_lookup <- applicant$scores$scores |>
    transmute(
      doi,
      P_MultiStudy_Selected = P_MultiStudy_Selected,
      .order = row_number()
    ) |>
    group_by(doi, P_MultiStudy_Selected) |>
    mutate(.dup_id = row_number()) |>
    ungroup()

  RRS_by_study <- RRS_by_study |>
    group_by(doi, P_MultiStudy_Selected) |>
    mutate(.dup_id = row_number()) |>
    ungroup() |>
    left_join(
      order_lookup |>
        select(doi, P_MultiStudy_Selected, .dup_id, .order),
      by = c("doi", "P_MultiStudy_Selected", ".dup_id")
    ) |>
    arrange(.order) |>
    select(-.dup_id, -.order)


  # overall score averaged across papers (each paper gets same weight)
  overall_score <- mean(RRS_by_study$RRS_overall, na.rm=TRUE)

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
    paper_scores = RRS_by_study,
    n_papers = sum(!is.na(RRS_by_study$RRS_overall)),
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

  x <- applicant$meta$forms$pub$elements[[1]]$options

  # go through all elements; find out which indicators are used for scoring.
  # (whenever the field "not_applicable" in the "score" element is defined, it is used for scoring)
  scoring_indicators <- c()
  for (i in 1:nrow(applicant$meta$forms$pub$elements[[1]])) {
    indicator <- applicant$meta$forms$pub$elements[[1]][i, ]
    if (!is.na(indicator$score$not_applicable)) scoring_indicators <- c(scoring_indicators, indicator$id)
  }

  # reduce to indicators that match the selector
  selector_match <- paste0("^(", paste(selector, collapse="|"), ")")
  scoring_indicators_matched <- scoring_indicators[grepl(selector_match, scoring_indicators)]

  # reduce to indicators that match the category cues

  # 1. Build a logical matrix: rows = scoring_indicators_matched, cols = cues
  match_mat <- sapply(cat_def$cue, function(pat) grepl(pat, scoring_indicators_matched, perl = TRUE))

  # 2. Count how many categories each element of X matches
  n_matches <- rowSums(match_mat)

  # 3. Check for elements that match more than one cue
  if (any(n_matches > 1L)) {
    i <- which(n_matches > 1L)
    stop("Some indicators match to multiple category cues:\n",
         paste0("  scoring_indicators_matched[", i, "] = ", scoring_indicators_matched[i], collapse = "\n"))
  }

  # 4. Map each X to its single matching category (or NA if no match)
  category <- rep(NA_character_, length(scoring_indicators_matched))
  idx_unique <- which(n_matches == 1L)

  # for those with exactly one TRUE, find which cue it is
  category[idx_unique] <- cat_def$title[
    apply(match_mat[idx_unique, , drop = FALSE], 1, function(z) which(z))
  ]

  # 5. Final result
  match_table <- data.frame(
    indicator = scoring_indicators_matched,
    category = category,
    stringsAsFactors = FALSE
  )

  print("The following indicators are defined in the scoring rules of the
        selected indicators (regardless of whether some indicator are missing
        for this specific applicant):")
  print(match_table |> group_by(category) |> count())

  return(match_table)
}
