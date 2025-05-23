library(jsonlite)
library(httr)
library(stringr)
library(purrr)


# ====== Scoring ======


#' Evaluate a condition in a context

#' @param research_output The research output to be scored. This is the latter part of the export json with the actual item values of one specific publication.
#' @param condition The logical conditions in the `scoring` section of core-pub.json. This can either be the `score` -> `not_applicable` condition or the `score` -> `condition` condition
#' @importFrom stringr str_replace_all
evaluate_condition_in_context <- function(condition, research_output) {
  processed_condition <- condition |>
    # replace "$<variable> =|= [<values>]" with "research_output$<variable> %in% c(<values>)"
    str_replace_all("(\\$[a-zA-Z0-9_]+)\\s*=\\|=\\s*\\[(.*?)\\]", "\\1 %in% c(\\2)") |>
    # replace "exists($<variable>)" with "exists(<variable>, research_output)"
    str_replace_all("exists\\(\\$(.*?)\\)", "exists('\\1', research_output)") |>
    # replace "!(<variable>)" with "not(<variable>, research_output)"
    str_replace_all("!\\(\\$([^=]*?)\\)", "not('\\1', research_output)") |>
    # replace "&&" with "%and%"
    str_replace_all("&&", "%and%") |>
    # replace "||" with "%or%"
    str_replace_all("\\|\\|", "%or%") |>
    # replace "===" with "=="
    str_replace_all("===", "==") |>
    # replace "!==" with "!="
    str_replace_all("!==", "!=") |>
    # replace "$<variable>"" with "research_output$<variable>"
    str_replace_all("\\$([a-zA-Z0-9_]+)", "research_output$\\1")

  # In order to parse the expression correctly, all expressions between the
  # %and%s and %or%s must be wrapped in (). In case this was forgotten in the
  # core-pubs.json, here we add those parentheses.

  processed_condition_par <- str_replace_all(processed_condition, "(?<=^|%and%|%or%)\\s*(.*?\\S)\\s*(?=%and%|%or%|$)", " (\\1) ")


  result <- eval(parse(text = processed_condition_par))

  !is.na(result) &&
    !is.null(result) &&
    is.logical(result) &&
    length(result) == 1 &&
    result
}



#' Score a single research output
#'
#' Returns a list with the following elements:
#' - \code{max_score}: the maximum score that can be reached
#' - \code{score}: the score that was reached
#' - \code{relative_score}: the score that was reached divided by the maximum score
#'
#' @param research_output The research output to be scored. This is the latter part of the export json with the actual item values of one specific publication.
#' @param verbose A logical value indicating whether to print verbose output (default is \code{FALSE})
#' @param meta The meta object from the export json
#'
#' @return A list with the scoring information
#'
#' @importFrom purrr map2_dbl map
#' @importFrom stats setNames
#' @export
score <- function(research_output, verbose = FALSE, meta) {
  # for debugging:
  # research_output <- research_outputs[[2]]

  # load the scoring information, which is stored in the exported json file
  scoring <- meta$forms[[research_output$type]]$scoring

  if (verbose == TRUE && is.null(scoring)) {
    print(paste0("scoring skipped (scoring information missing in `meta`)"))
    return(list(
      indicators = NA,
      max_score = NA,
      score = NA,
      relative_score = NA
    ))
  }

  indicators <- list()

  max_score <- 0
  reached_score <- 0

  for (indicator_index in seq_along(scoring)) {

    indicator <- scoring[[indicator_index]]
    # Get indicator name using the index
    indicator_name <- names(scoring)[indicator_index]

    if (verbose == TRUE) {
      print(paste0("indicator_index: ", indicator_index, " (", indicator_name, ")"))
    }

    # Handle 'not applicable' condition
    # Skip this indicator if the 'not applicable' condition is met
    if (evaluate_condition_in_context(
      indicator$not_applicable,
      research_output)) {
      if (verbose == TRUE) {
        print(paste0("This indicator is not applicable; skipping."))
      }
      next
    }

    max_score <- max_score + indicator$max
    if (verbose == TRUE) {
      print(paste0("max_score now is: ", max_score))
    }

    # Evaluate each condition:
    # if it is met, add the corresponding value to the score
    values <- c(0)

    for (p in indicator$points) {
      if (evaluate_condition_in_context(p$condition, research_output)) {
        values <- c(values, p$value)
      }
    }

    # Apply the specified operation to the values
    indicator_score <- if (indicator$op == "sum") {
      sum(values)
    } else if (indicator$op == "select") {
      max(values)
    } else {
      0
    }

    # Add the indicator score to the reached score
    reached_score <- reached_score + indicator_score

    if (verbose == TRUE) {
      print(paste0("reached_score now is: ", reached_score))
    }

    # Add the indicator score to the indicator score list
    indicators <- c(indicators, setNames(list(list(value = indicator_score, max = indicator$max)), indicator_name))
  }

  list(
    indicators = indicators,
    max_score = max_score,
    score = reached_score,
    relative_score = reached_score / max_score
  )
}







#' Score multiple research outputs
#'
#' Returns a list with the following elements:
#' - scores: a list of scores for each research output
#' - scored_research_outputs: the number of research outputs that were scored
#' - overall_score: the average score of all scored research outputs. Each output is weighted equally.
#'
#' @param research_outputs A list of research outputs to be scored
#' @param verbose A logical value indicating if verbose output should be printed
#' @return A list with scores, number of scored research outputs, and overall score
#' @export
score_all <- function(research_outputs, verbose = FALSE) {
  meta <- research_outputs[[1]]
  scores <- map(research_outputs, ~ score(.x, meta = meta, verbose = verbose))

  # Applicant requests manual processing if the max score is 0
  max_scores <- sapply(scores, function(x) x$max_score)
  valid_scores <- scores[max_scores > 0]

  list(
    scores = scores,
    scored_research_outputs = length(valid_scores),
    overall_score = ifelse(
      length(valid_scores) > 0,
      mean(sapply(valid_scores, function(x) x$relative_score)),
      NA)
  )
}


#' Score all research outputs from a file
#'
#' Reads the research outputs from the specified file and scores them using \code{\link{score_all}}.
#' Returns a list with the following elements:
#' \describe{
#'   \item{scores}{a list of scores for each research output}
#'   \item{scored_research_outputs}{the number of research outputs that were scored}
#'   \item{overall_score}{the average score of all scored research outputs}
#' }
#'
#' @param file The file path where the research outputs are stored.
#' @param verbose Logical value indicating whether to print additional information. Default is \code{FALSE}.
#'
#' @return A list with elements scores, scored_research_outputs, and overall_score.
#'
#' @importFrom jsonlite read_json
#'
#' @examples
#' \dontrun{
#' score_all_from_file("research_outputs.json", verbose = TRUE)
#' }
#'
#' @export
score_all_from_file <- function(file, verbose = FALSE) {
  research_outputs <- jsonlite::read_json(file)
  score_all(research_outputs, verbose = verbose)
}

# ====== Example ======

# Example: score multiple research outputs
# research_outputs <- read_json("/Users/felix/LMU/DGPs Kommission Open Science/RESQUE/Mainz Test 2/resque_schönbrodt_after_name_change.json")
# score_all(research_outputs)

# Example: score a single research output
# research_output <- research_outputs[[2]]

# meta <- research_outputs[[1]]

# score(research_outputs[[11]], meta = meta, verbose = TRUE)

# Example: score all research outputs from a file
# scores <- score_all_from_file("/Users/felix/LMU/DGPs Kommission Open Science/RESQUE/Mainz Test 2/resque_schönbrodt_0.6.2.json", verbose = TRUE)

# scores <- score_all_from_file(json_file, verbose = TRUE)

# applicant <- read_RESQUE(file=json_file)
