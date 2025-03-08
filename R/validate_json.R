
# ====== Fetching information on packs ======

#' Get JSON Object
#'
#' Make a GET request to a specified URL and convert the JSON response to an R object using the jsonlite package.
#'
#' @param url The URL to make the GET request to.
#' @return An R object representing the converted JSON response.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom httr timeout
#' @importFrom httr content
#'
#'
#' @export
get_json_object <- function(url) {
  response <- httr::GET(url, timeout(20))
  json <- httr::content(response, as = 'text')
  j = jsonlite::fromJSON(json)
}



# Get the name of the pack that a research output uses
get_pack_name <- function(research_output) {
  if (research_output$type == "pub") {
    "core-pubs"
  } else if (research_output$type == "software") {
    "core-software"
  } else if (research_output$type == "data") {
    "core-data"
  } else if (research_output$type == "meta") {
    "core-meta"
  } else {
    "unknown"
  }
}






#' Fix some problems with old exported json files
#'
#' @param file Path to RESQUE JSON file
#' @param update_forms Should the `forms` node in the json (which contains the scoring information) be overwritten with the current version?
#' @param verbose Show diagnostic information?
#' @importFrom stringr str_replace_all
#' @importFrom jsonlite parse_json toJSON
#' @export
validate_json <- function(file, update_forms = FALSE, verbose = TRUE) {

  # Do variable name conversion (from old -> new) on the raw text of the json.
  # This changes them both in the indicator definitions AND in the conditions
  # and scoring rules.

  json_text <- readLines(file, warn=FALSE)

  # Rename old variables to the new variable names
  # Note: Use negative lookahead so that "P_TypeMethod_Empirical(?!Quantitative)"
  # is only matched when it is not followed by "Quantitative"
  json_text <- json_text |>
    str_replace_all(fixed("P_TypeMethod_Empirical(?!Quantitative)"), "P_TypeMethod_EmpiricalQuantitative") |>
    str_replace_all(fixed("P_TypeMethod_Simulation"), "P_TypeMethod_Computational") |>
    str_replace_all(fixed("P_CoG_ParcipantSample"), "P_Sample_CoG") |>
    str_replace_all(fixed("P_Stimuli_Relevance_NAexplanation"), "P_Stimuli_NAExplanation") |>
    str_replace_all(fixed("P_Sample_RepresentativenessRelevance_NAexplanation"), "P_Sample_RepresentativenessRelevance_NAExplanation") |>
    str_replace_all(fixed("P_CoG_StimulusSample"), "P_Stimuli_CoG") |>
    # respect this order of replacing: P_Stimuli_Relevance should be replaced last to avoid double replacement
    str_replace_all(fixed("P_Stimuli_Relevance"), "P_Stimuli") |>
    str_replace_all(fixed("P_PreregisteredReplication"), "P_Preregistration_Replication") |>
    str_replace_all(fixed("P_PreregisteredReplication_NAExplanation"), "P_Preregistration_Replication_NAExplanation") |>
    str_replace_all(fixed("P_Sample_Type"), "P_Sample")


  # for the extraction of the research outputs, use `simplifyVector = FALSE`
  j1 <- jsonlite::parse_json(json_text, simplifyVector = FALSE)
  scoring <- j1[[1]]$forms$pub$scoring
  research_outputs <- j1[-1]

  # reading with `simplifyVector = TRUE` gives a different data frame structure
  # that makes it easier to extract the meta data with the items
  j2 <- jsonlite::parse_json(json_text, simplifyVector = TRUE)
  meta <- j2[1, , drop=TRUE]
  items <- meta$forms$pub$elements[[1]]
  checkbox_item_ids <- items[items$type == "checkbox", "id"]

  # replace the existing scoring and forms information
  if (update_forms == TRUE) {
    meta_new <- read_json(path=system.file("extdata/forms_0_6_0.json", package="RESQUER"), simplifyVector=FALSE)

    j1[[1]]$forms <- meta_new$forms
  }

  # Add some missing elements in old jsons
  for (r in seq_along(research_outputs)) {
    # Add missing type MetaAnalysis
    if (is.null(research_outputs[[r]]$P_TypeMethod_MetaAnalysis)) {
      research_outputs[[r]]$P_TypeMethod_MetaAnalysis <- FALSE
      if (verbose==TRUE) print(paste0("Added missing 'P_TypeMethod_MetaAnalysis = FALSE' to research output #", r))
    }
  }


  # Manually check all conditional checkbox items, for all research outputs.
  # If the condition applies (i.e., it is not "not applicable" AND they have been shown (i.e., $condition == TRUE)), then missing child checkboxes should be FALSE (and not missing)

  # go through all research outputs
  for (r in seq_along(research_outputs)) {

    # TODO: Add all other dependencies:
    # P_Preregistration_Content items
    # ------------------------------------------
    condition <- "P_Preregistration_Content"
    to_check <- c(
      "P_Preregistration_Content_SampleSizePlanning",
      "P_Preregistration_Content_Hypotheses",
      "P_Preregistration_Content_Operationalizations",
      "P_Preregistration_Content_AnalysisPlan",
      "P_Preregistration_Content_InferentialCriteria"
    )

    item0 <- items[items$id == condition, ]
    not_applicable <- evaluate_condition_in_context(condition=item0$score$not_applicable, research_output=research_outputs[[r]])
    shown <- evaluate_condition_in_context(condition=item0$condition, research_output=research_outputs[[r]])

    if (!not_applicable & shown) {
      if (verbose==TRUE) print(paste0("Research output ", r, ": Condition '", condition, "' DOES apply."))

      for (check in to_check) {
        if (is.null(research_outputs[[r]][check][[1]])) {
          research_outputs[[r]][check] <- FALSE
          if (verbose==TRUE) print(paste0("  |-- Research output ", r, ": Setting missing item '", check, "' to FALSE."))
        }
      }
    } else {
      if (verbose==TRUE) print(paste0("Research output ", r, ": Condition '", condition, "' does not apply."))
    }



    # P_Data_Open_FAIR items
    # ------------------------------------------
    condition <- "P_Data_Open_FAIR"
    to_check <- c(
      "P_Data_Open_FAIR_PersistentIdentifier",
      "P_Data_Open_FAIR_Codebook",
      "P_Data_Open_FAIR_StructuredOpenFormat"
    )

    item0 <- items[items$id == condition, ]
    not_applicable <- evaluate_condition_in_context(condition=item0$score$not_applicable, research_output=research_outputs[[r]])
    shown <- evaluate_condition_in_context(condition=item0$condition, research_output=research_outputs[[r]])

    if (!not_applicable & shown) {
      if (verbose==TRUE) print(paste0("Research output ", r, ": Condition '", condition, "' DOES apply."))

      for (check in to_check) {
        if (is.null(research_outputs[[r]][check][[1]])) {
          research_outputs[[r]][check] <- FALSE
          if (verbose==TRUE) print(paste0("  |-- Research output ", r, ": Setting missing item '", check, "' to FALSE."))
        }
      }
    } else {
      if (verbose==TRUE) print(paste0("Research output ", r, ": Condition '", condition, "' does not apply."))
    }

  }  # of r in seq_along


  # Rebuild the entire json
  j1_new <- c(j1[1], research_outputs)

  # this returns the JSON as text data
  return(jsonlite::toJSON(j1_new, auto_unbox=TRUE, pretty=TRUE))
}


