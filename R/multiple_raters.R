#' Batch import and preprocess RESQUE JSON files
#'
#' Reads one or more RESQUE JSON files from a directory, optionally replaces
#' their internal \code{"forms"} (scoring) definition with an external JSON
#' specification, preprocesses the resulting objects, and returns them as a
#' list of applicants.
#'
#' If \code{replace_form} is provided, the function first validates the
#' replacement forms JSON via \code{\link{test_forms_json_structure}} and then
#' uses \code{\link{update_forms}} to inject the new \code{"forms"} element
#' into each RESQUE file before calling \code{\link{read_RESQUE}}. Otherwise,
#' the RESQUE files are read as-is.
#'
#' Each applicant object is passed through \code{\link{preprocess}} to compute
#' derived variables (optionally using OpenAlex and/or BIP metadata). The
#' function also creates an anonymized pseudo-ID for each applicant based on
#' their ORCID using \code{digest::digest(algo = "crc32")}.
#'
#' Optionally, external rater names can be anonymized by replacing them with
#' identifiers \code{"R1"}, \code{"R2"}, \code{"R3"}, etc., consistently
#' across all jsons.
#'
#' @param path Character scalar. Directory containing RESQUE JSON files.
#' @param recursive Logical scalar. Should subdirectories be searched
#'   recursively for JSON files? Passed to \code{\link{list.files}}.
#'   Defaults to \code{TRUE}.
#' @param replace_form Optional character scalar. Path to a JSON file that
#'   contains a top-level \code{"forms"} element defining packs/scoring
#'   information. If not \code{NULL}, this forms definition replaces the
#'   \code{"forms"} element in each RESQUE JSON before preprocessing.
#' @param get_OpenAlex Logical scalar. If \code{TRUE}, the preprocessing step
#'   retrieves/uses OpenAlex-based metadata). Defaults to \code{FALSE}.
#' @param get_BIP Logical scalar. If \code{TRUE}, the preprocessing step
#'   retrieves/uses BIP (bibliometric) metadata. Defaults to \code{FALSE}.
#' @param anonymize_raters Logical scalar. If \code{TRUE}, external rater
#'   names are replaced with pseudo-identifiers \code{"R1"}, \code{"R2"},
#'   \code{"R3"}, etc., consistently across the imported applicants.
#'   Defaults to \code{FALSE}.
#' @param verbose Logical scalar. If \code{TRUE}, progress messages about
#'   file processing and optional form replacement are printed to the console.
#'   Defaults to \code{TRUE}.
#'
#' @return
#' A list of preprocessed applicant objects, one entry per JSON file. Each
#' element corresponds to the output of \code{\link{preprocess}} applied to
#' the RESQUE object returned by \code{\link{read_RESQUE}} (with or without
#' form replacement).
#'
#' @seealso
#' \code{\link{test_forms_json_structure}},
#' \code{\link{update_forms}},
#' \code{\link{read_RESQUE}},
#' \code{\link{preprocess}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Basic import of all RESQUE JSON files in a folder
#'   applicants <- get_jsons(
#'     path      = "raw_data/Study2/calibration_papers",
#'     recursive = TRUE
#'   )
#'
#'   # Import with updated scoring forms and rater anonymization
#'   applicants <- get_jsons(
#'     path            = "raw_data/Study2/calibration_papers",
#'     replace_form    = "includes/v0.8.1forms.json",
#'     anonymize_raters = TRUE,
#'     verbose         = TRUE
#'   )
#' }
#'
#' @importFrom digest digest
get_jsons <- function(path, recursive=TRUE, replace_form=NULL, get_OpenAlex=FALSE, get_BIP=FALSE, anonymize_raters=FALSE, verbose=TRUE) {

  json_files <- list.files(path, pattern="*.json", full.names=TRUE, recursive=recursive)

  # Optional: Replace the packs/scoring information with with an external file
  # (e.g. if the scoring rules have been updated)
  if (!is.null(replace_form)) {
    if (verbose) print("External scoring data loaded - testing for valid structure ...")
    test_forms_json_structure(replace_form)
    do_replace <- TRUE
  } else {
    do_replace <- FALSE
  }


  # read all json files and preprocess them
  applicant_list <- list()
  for (i in 1:length(json_files)) {
    print(paste0("Preprocessing file ", i, "/", length(json_files), ": ", json_files[i]))

    if (do_replace) {
      R <- read_RESQUE(file=update_forms(json_files[i], newform=replace_form), verbose=FALSE)
    } else {
      R <- read_RESQUE(file=json_files[i], verbose=FALSE)
    }

    applicant_list[[i]] <- preprocess(applicant=R, verbose=verbose, get_BIP=get_BIP, get_OpenAlex=get_OpenAlex)
    applicant_list[[i]]$meta$pid <- digest::digest(applicant_list[[i]]$meta$ORCID, algo="crc32")
  }

  # if requested: Anonymize raters (replace rater names with R1, R2, R3, etc)
  if (anonymize_raters == TRUE) {
    raterNames <- sapply(applicant_list, function(x) x$meta$ExternalRaterName) |> unique() |> sort()
    newRaterNames <- paste0("R", seq_along(raterNames))
    names(newRaterNames) <- raterNames

    for (i in seq_along(applicant_list)) {
      applicant_list[[i]]$meta$ExternalRaterName <- newRaterNames[applicant_list[[i]]$meta$ExternalRaterName]
      names(applicant_list[[i]]$meta$ExternalRaterName) <- NULL
    }
  }

  return(applicant_list)
}





#' Create a wide rater–indicator table across many RESQUE applicants
#'
#' Imports and preprocesses RESQUE JSON files from a directory via
#' \code{\link{get_jsons}}, extracts indicator ratings, reshapes them into a
#' wide format with one row per (paper \eqn{\times} indicator), and one column
#' for each rater. It computes consistency and scoring-relevance flags for each
#' indicator.
#'
#' The function first calls \code{\link{get_jsons}} to obtain a list of
#' preprocessed applicants. It then uses \code{\link{extract_indicators}} to
#' build a long-format table of indicator ratings for each paper/rater, and
#' reshapes this into a wide table with one column per rater (\code{R1},
#' \code{R2}, \code{R3}, \dots).
#'
#' An indicator is flagged as \emph{consistent} if all raters gave the same
#' rating. Indicators whose names
#' appear in the scoring specification in the meta-data are additionally flagged
#' as \emph{scoring relevant}. For consistent indicators, the consensus rating
#' is prepopulated.
#'
#' @param path Character scalar. Directory containing RESQUE JSON files to be
#'   passed to \code{\link{get_jsons}}.
#' @param selector Character vector. One or more prefixes identifying the
#'   indicator columns to be extracted (e.g. \code{"P_"}). Passed to
#'   \code{\link{extract_indicators}} and used in \code{\link[tidyr]{pivot_longer}}
#'   via \code{\link[dplyr]{starts_with}}. Defaults to \code{c("P_")}.
#' @param verbose Logical scalar. If \code{TRUE}, the function prints summary
#'   information about the number of files, papers, authors, and consistency
#'   statistics. Defaults to \code{TRUE}.
#' @param ... Additional arguments passed directly to \code{\link{get_jsons}},
#'   such as \code{replace_form}, \code{recursive}, \code{get_OpenAlex},
#'   \code{get_BIP}, \code{anonymize_raters}, and \code{verbose}.
#'
#' @return
#' A data frame in wide format with one row per
#' (paper \eqn{\times} indicator) combination. The table contains, among
#' others, the following columns:
#' \itemize{
#'   \item \code{ORCID}, \code{doi}, \code{Title}: publication and author
#'         identifiers/metadata.
#'   \item \code{Indicator}: the indicator name.
#'   \item \code{R1}, \code{R2}, \code{R3}, \dots: one column per rater
#'         containing the rating values, with missing
#'         values converted to the character \code{"_NA_"}.
#'   \item \code{is_consistent}: logical flag indicating whether all rater
#'         ratings for this indicator are identical.
#'   \item \code{scoring_relevant}: logical flag indicating whether the
#'         indicator is part of the scoring specification in
#'         \code{meta$forms$pub$scoring}.
#'   \item \code{consensus_rating}: consensus rating (typically the value in
#'         \code{R1}) for indicators where \code{is_consistent} is \code{TRUE},
#'         otherwise \code{NA}.
#' }
#'
#' @seealso
#' \code{\link{get_jsons}},
#' \code{\link{extract_indicators}}
#'
#' @export
#' @examples
#' \dontrun{
#'   # Create a wide rater–indicator table from all RESQUE files in a directory
#'   wide <- many_raters(
#'     path     = "raw_data/Study2/calibration_papers",
#'     selector = c("P_"),
#'     replace_form     = "includes/v0.8.1forms.json",
#'     anonymize_raters = TRUE
#'   )
#'
#'   # Inspect inconsistent, scoring-relevant indicators
#'   subset(wide, !is_consistent & scoring_relevant)
#' }

many_raters <- function(path, selector=c("P_"), verbose=TRUE, ...) {
  applicant_list <- get_jsons(path, ...)
  #applicant_list <- get_jsons(path, replace_form, recursive=TRUE, get_OpenAlex=FALSE, get_BIP=FALSE, anonymize_raters=TRUE, verbose=TRUE)

  all_indicators <- extract_indicators(applicant_list, selector=selector)
  all_indicators <- all_indicators |> arrange(ORCID, ExternalRaterName)

  long <- all_indicators |> pivot_longer(
    cols = starts_with(selector), names_to = "Indicator", values_to = "Rating",
    values_transform = list(Rating = as.character))

  # change all NAs to the character "_NA_" (to distinguish them from "") in the Excel file
  long$Rating[is.na(long$Rating)] <- "_NA_"

  wide <- long |>
    pivot_wider(
      id_cols=c(ORCID, doi, Title, Indicator),
      names_from = ExternalRaterName, values_from = Rating) |>
    arrange(doi, Indicator)

  # flag inconsistent ratings
  wide$is_consistent <- wide |> select(matches("^R\\d+")) |>
    apply(1, function(x) {length(unique(x)) == 1})

  # add flag whether the indicator is relevant for scoring
  scoring_relevant_cols <- applicant_list[[1]]$meta$forms$pub$scoring |> colnames()
  wide$scoring_relevant <- wide$Indicator %in% scoring_relevant_cols

  # prepopulate the consensus rating (if all raters agree)
  wide$consensus_rating <- NA
  wide$consensus_rating[wide$is_consistent] <- wide$R1[wide$is_consistent]

  # give a summary print
  if (verbose) {
    print(paste0("The ", length(applicant_list), " imported json files contained ", length(unique(wide$doi)), " papers from ", length(unique(wide$ORCID)), " first authors."))

    print(paste0("Of the ", nrow(wide), " assessed indicators, ", sum(wide$is_consistent), " were consistent (", (sum(wide$is_consistent)*100/nrow(wide)) |> round(), "%), ", sum(!wide$is_consistent), " were inconsistent (", (sum(!wide$is_consistent)*100/nrow(wide)) |> round(), "%)."))

    print(paste0("Of the ", sum(!wide$is_consistent), " inconsistent indicators, ", sum(!wide$is_consistent & wide$scoring_relevant), " were relevant for scoring."))
  }

  return(wide)
}
