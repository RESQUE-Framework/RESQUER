# file <- system.file("extdata/demo_profiles/resque_Schoenbrodt.json", package="RESQUER")
# file="/Users/felix/LMU/DGPs Kommission Open Science/RESQUE/Test 0/resque_Schoenbrodt2.json"


#' Read a single RESQUE JSON file and do basic preprocessing
#' (elaborated preprocessing happens in `preprocess()`).
#' TODO: Why is some preprocessing here, and some in the other function?
#' Make a principled design.
#'
#' @param file Path to RESQUE JSON file
#' @param verbose Show diagnostic information?
#' @param update_forms Should the `forms` node in the json (which contains the scoring information) be overwritten with the current version?
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import OAmetrics
#' @importFrom jsonlite parse_json
#' @export
read_RESQUE <- function(file, update_forms=FALSE, verbose=FALSE) {

  # clean up the json (in case that it is an old version)
  fixed_json <- validate_json(file, update_forms=update_forms, verbose=verbose)
  dat0 <- parse_json(fixed_json, simplifyVector = TRUE)

  # create a clean meta object
  meta <- dat0[1, , drop=TRUE]
  meta$forms <- NULL
  meta$queryConfig <- NULL
  meta <- meta[!is.na(meta)]

  # create a clean table of the publications
  dat <- dat0[-1, ]
  dat$forms <- NULL
  dat <- dat %>% select(-any_of(c("date_created", "position", "date_modified", "date_exported", "LastName", "FirstName", "YearPhD", "ORCID", "RaterType", "ExternalRaterName")))

  # clean and create some fields
  meta$FullName <- paste0(meta$FirstName, " ", meta$LastName)
  dat$Title <- clean_title(dat$Title)
  dat$TitleLink <- paste0("[", dat$Title, "](", dat$DOI, ")")

  # remove unnecessary column that breaks the structure of dat (nested data frame)
  dat$queryConfig <- NULL

  # read the scores
  #scores <- score_all_from_file(file=file, verbose=verbose)
  scores <- score_all(parse_json(fixed_json, simplifyVector = FALSE), verbose=verbose)

  # remove the first element: This is the meta-information which has no scores
  # Now each list entry is one publication, in the same order as in `dat`
  scores$scores <- scores$scores[-1]

  # Create nice factor labels
  dat$type <- factor(dat$type, levels=c("pub", "data", "software"), labels=c("Publication", "Data set", "Research software"))

  #dat <- unCamel(dat, "P_TypePublication")
  #dat <- unCamel(dat, "P_ReproducibleScripts")

  dat$dois_normalized <- OAmetrics::normalize_dois(dat$DOI)

  #-----------------------------------------------------------------
  # CRediT

  # If CRediT columns are missing: add them
  dat <- add_variables(dat, c("P_CRediT_Conceptualization", "P_CRediT_DataCuration", "P_CRediT_FormalAnalysis", "P_CRediT_FundingAcquisition", "P_CRediT_Investigation", "P_CRediT_Methodology", "P_CRediT_ProjectAdministration", "P_CRediT_Resources", "P_CRediT_Software", "P_CRediT_Supervision", "P_CRediT_Validation", "P_CRediT_Visualization", "P_CRediT_WritingOriginalDraft", "P_CRediT_WritingReviewEditing"), default="NoRole")

  credit <- dat %>%
    select(contains("CRediT"), -contains("P_CRediT_InJournal")) %>% pivot_longer(everything(), names_prefix = "P_CRediT_")

    colnames(credit) <- c("Role", "Degree")
    credit$Degree <- factor(credit$Degree, levels = rev(c("Lead", "Equal", "Support", "NoRole", "NA")), labels = rev(c("Lead", "Equal", "Support", "NoRole", "not applicable")))

    # add space to camelCase; make nice labels
    credit <- unCamel(credit, "Role")
    credit$Role[credit$Role == "Writing Review Editing"] <- "Writing: Review & Editing"
    credit$Role[credit$Role == "Writing Original Draft"]  <- "Writing: Original draft"

  return(list(meta=meta, indicators=dat, scores=scores, credit=credit))
}

