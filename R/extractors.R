## These helper functions assist dealing with lists of multiple applicants' jsons.
## All of them take the list of jsons and extract information on different levels.

## extract_indicators() returns a wide format, where each column is one indicator
## All other functions return a long-format data frame.

# for testing:
# load(file="/Users/felix/LMU/Research/1 - In Arbeit/RESQUE/RESQUE-Clinical/processed_data/applicant_list_two_raters.RData")

# ------------------------------------------------------------------------
#' Extract Indicators from Applicant List
#'
#' This function processes a list of applicants, extracting specific indicators
#' from each applicant's data and compiling them into a single data frame. It
#' also includes metadata such as ORCID, LastName, and ExternalRaterName from
#' the applicant's profile.
#'
#' @param applicant_list A list containing the RESQUE jsons from multiple applicants.
#'
#' @return The function returns a data frame where each row corresponds to one
#' publication and indicators are in columns.
#'
#' @importFrom plyr rbind.fill
#' @export

extract_indicators <- function(applicant_list) {
  all_indicators <- data.frame()
  for (i in 1:length(applicant_list)) {
    ind <- applicant_list[[i]]$indicators %>%
      select(starts_with("P_"), Title, Year, doi_links) %>%
      select(-starts_with("P_CRediT"), -matches("P_SchemeFit"), -P_TopPaper_Select)

    ind$ORCID <- applicant_list[[i]]$meta$ORCID
    ind$LastName <- applicant_list[[i]]$meta$LastName
    ind$ExternalRaterName <- applicant_list[[i]]$meta$ExternalRaterName
    all_indicators <- plyr::rbind.fill(all_indicators, ind)
  }

  all_indicators <- all_indicators %>%
    relocate(ORCID, LastName, ExternalRaterName)

  return(all_indicators)
}





# ------------------------------------------------------------------------
#' Extract RRS Sector Scores for Each Applicant
#'
#' This function processes a list of applicants, extracting the Relative Rigor Score (RRS)
#' for each sector for each applicant and compiling them into a single data frame. It includes
#' metadata such as ORCID, LastName, and ExternalRaterName from the applicant's profile.
#' The function can return the data in either "long" or "wide" format based on the `format` parameter.
#' In the long format, each row is one sector score for each applicant and rater. In the wide format,
#' the relative rigor scores (`rel_score`) are displayed as separate columns for each rater, side-by-side.
#'
#' @param applicant_list A list containing the RESQUE jsons from multiple applicants.
#' @param format A character string specifying the format of the output data frame.
#' In the long format, each row is one sector score for each applicant and rater. In the wide format,
#' the relative rigor scores (`rel_score`) are displayed as separate columns for each rater, side-by-side.
#'
#' @return A data frame containing the RRS sector scores for each applicant.
#' The structure of the returned data frame depends on the `format` parameter.
#'
#' @export

extract_sector_scores <- function(applicant_list, format="long") {
  RRS_sector_person <- data.frame()
  for (i in 1:length(applicant_list)) {
      sec <- applicant_list[[i]]$RRS$sector_scores
      if (length(sec) > 1) {
        sec$ORCID <- applicant_list[[i]]$meta$ORCID
        sec$LastName <- applicant_list[[i]]$meta$LastName
        sec$ExternalRaterName <- applicant_list[[i]]$meta$ExternalRaterName
        RRS_sector_person <- plyr::rbind.fill(RRS_sector_person, sec)
      } else {
        warning(paste0("No sector scores present for ", applicant_list[[i]]$meta$ORCID))
      }
  }

  r1 <- RRS_sector_person %>%
    select(ORCID, ExternalRaterName, category, scores, max_points, rel_score)

  if (format == "wide") {
      r1 <- r1 %>%
        select(ORCID, ExternalRaterName, category, rel_score) %>%
        pivot_wider(names_from=c(ExternalRaterName), values_from=rel_score)
  }

  return(r1)
}


# ------------------------------------------------------------------------
#' Extract RRS Overall Score for Each Applicant
#'
#' This function processes a list of applicants, extracting the Relative Rigor Score (RRS)
#' or each applicant and compiling them into a single data frame. It includes
#' metadata such as ORCID, LastName, and ExternalRaterName from the applicant's profile.
#'
#' @param applicant_list A list containing the RESQUE jsons from multiple applicants.
#'
#' @return A data frame containing the RRS overall score for each applicant.
#'
#' @export

extract_overall_scores <- function(applicant_list) {
  RRS_overall_person <- data.frame()
  for (i in 1:length(applicant_list)) {
      r0 <- data.frame(
        RRS_overall = applicant_list[[i]]$RRS$overall_score,
        ORCID = applicant_list[[i]]$meta$ORCID,
        LastName = applicant_list[[i]]$meta$LastName,
        ExternalRaterName = ifelse(!is.null(applicant_list[[i]]$meta$ExternalRaterName), applicant_list[[i]]$meta$ExternalRaterName, NA)
      )

      RRS_overall_person <- plyr::rbind.fill(RRS_overall_person, r0)
  }

  RRS_overall_person <- RRS_overall_person %>%
    select(ORCID, ExternalRaterName, LastName, RRS_overall)

  return(RRS_overall_person)
}


# ------------------------------------------------------------------------
#' Extract scores on indicator level
#'
#' This function processes a list of applicants, extracting the Relative Rigor Score (RRS)
#' for each single indicator of each applicant and compiling them into a single long-format data frame.
#'
#' @param applicant_list A list containing the RESQUE jsons from multiple applicants.
#'
#' @return A data frame containing the indicator scores for each applicant.
#'
#' @export

extract_indicator_scores <- function(applicant_list) {
  all_scores <- data.frame()
  for (i in 1:length(applicant_list)) {
      sc <- applicant_list[[i]]$scores

      # loop through all publications
      ind_scores <- data.frame()
      for (j in 1:length(sc$scores)) {
          sc_indicators <- sc$scores[[j]]$indicators

          if (length(sc_indicators) > 0) {
              ind_scores0 <- data.frame()
              for (jj in 1:length(sc_indicators)) {
                  ind_scores00 <- data.frame(
                      indicator   = names(sc_indicators[jj]),
                      value       = sc_indicators[jj][[1]]$value,
                      max         = sc_indicators[jj][[1]]$max)

                  ind_scores0 <- plyr::rbind.fill(ind_scores0, ind_scores00)
              }
              ind_scores0$ORCID <- applicant_list[[i]]$meta$ORCID
              ind_scores0$LastName <- applicant_list[[i]]$meta$LastName
              ind_scores0$ExternalRaterName <- applicant_list[[i]]$meta$ExternalRaterName
              ind_scores0$doi <- applicant_list[[i]]$indicators$doi_links[j]

              ind_scores <- plyr::rbind.fill(ind_scores, ind_scores0)
          }
      }

      all_scores <- plyr::rbind.fill(all_scores, ind_scores)
  }

  all_scores <- all_scores %>%
      select(-LastName) %>%
      mutate(rel_score = value/max) %>%
      relocate(ORCID, doi, ExternalRaterName)

  return(all_scores)
}




# ------------------------------------------------------------------------
#' Extract RRS Scores for Each Paper of each applicant
#'
#' This function processes a list of applicants, extracting the Relative Rigor Score (RRS)
#' for each sector for each paper and compiling them into a single data frame. It includes
#' metadata such as ORCID, LastName, and ExternalRaterName from the applicant's profile.
#' The function can return the data in either "long" or "wide" format based on the `format` parameter.
#' In the long format, each row is one sector score for each applicant and rater. In the wide format,
#' the relative rigor scores (`rel_score`) are displayed as separate columns for each rater, side-by-side.
#'
#' @param applicant_list A list containing the RESQUE jsons from multiple applicants.
#'
#' @return A data frame containing the RRS sector scores for each paper of each applicant.
#'
#' @export

extract_paper_scores <- function(applicant_list) {
  RRS_paper <- data.frame()
  for (i in 1:length(applicant_list)) {
      papers <- applicant_list[[i]]$RRS$paper_scores
      papers$ORCID <- applicant_list[[i]]$meta$ORCID
      papers$LastName <- applicant_list[[i]]$meta$LastName
      papers$ExternalRaterName <- applicant_list[[i]]$meta$ExternalRaterName
      RRS_paper <- plyr::rbind.fill(RRS_paper, papers)
  }

  if (!"ExternalRaterName" %in% colnames(RRS_paper)) {
    RRS_paper$ExternalRaterName <- NA
  }

  r1 <- RRS_paper %>%
    select(-LastName) %>%
    relocate(ORCID, doi, ExternalRaterName)

  return(r1)
}
