#' Test structure of a "forms" JSON file
#'
#' Runs a set of \pkg{testthat} checks to verify that a JSON file containing
#' form definitions conforms to the expected schema. These json files are used
#' to replace the metadata of an existing json with exported RESQUE data
#' (e.g. if the scoring rules have been updated).
#'
#' The core checks performed are:
#' \itemize{
#'   \item The file contains valid JSON.
#'   \item The top-level JSON object contains only a single element named
#'         \code{"forms"}.
#'   \item The \code{"forms"} object contains at least the elements
#'         \code{"meta"}, \code{"pub"}, and \code{"config"}.
#'   \item The \code{"pub"} element contains non-empty \code{"versions"} and
#'         \code{"scoring"} elements.
#'   \item The top-level \code{"config"} element contains a non-empty
#'         \code{"score_categories"} element.
#' }
#'
#' In addition, the function performs several **additional** schema checks:
#' \itemize{
#'   \item The file path exists.
#'   \item Core forms (\code{"meta"}, \code{"pub"}, \code{"data"},
#'         \code{"software"}) share a common structure (e.g. must contain
#'         \code{title}, \code{versions}, \code{date}, \code{scoring},
#'         \code{config}, \code{elements}).
#'   \item Every element within \code{elements} has at least
#'         \code{"id"} and \code{"type"} fields.
#'   \item Each entry in \code{score_categories} contains at least
#'         a \code{"title"} field.
#'   \item \code{forms$config} contains meta-entries for
#'         \code{"meta"}, \code{"pubs"}, \code{"data"}, and \code{"software"}.
#' }
#'
#' These additional checks are intended to catch common schema deviations and
#' can be reviewed, modified, or removed as needed.
#'
#' @param path Character scalar. Path to the JSON file to be tested.
#'
#' @return
#' This function is called for its side effect of running
#' \pkg{testthat} expectations. It returns the result of the
#' \code{testthat::test_that()} call (invisibly).
#'
#'@export
#'
#' @examples
#' \dontrun{
#'   test_forms_json_structure("inst/extdata/v0.8.1forms.json")
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom testthat test_that expect_error expect_true

test_forms_json_structure <- function(path) {

  testthat::test_that(paste("Structure of forms JSON:", path), {

    # ---- 1) Valid JSON ----
    json <- NULL
    testthat::expect_error(
      json <- jsonlite::fromJSON(
        path,
        simplifyVector   = FALSE,
        simplifyDataFrame = FALSE,
        simplifyMatrix   = FALSE
      ),
      NA,
      info = "File must contain valid JSON."
    )

    # ---- 2) Only 'forms' at top level ----
    testthat::expect_true(
      identical(names(json), "forms"),
      info = "Top-level JSON object must contain only a 'forms' element."
    )

    forms <- json$forms
    testthat::expect_true(
      is.list(forms),
      info = "'forms' must be an object (JSON object -> named list in R)."
    )

    # ---- 3) 'forms' contains at least 'meta', 'pub', and 'config' ----
    required_forms_keys <- c("meta", "pub", "config")
    testthat::expect_true(
      all(required_forms_keys %in% names(forms)),
      info = "'forms' must contain at least 'meta', 'pub', and 'config'."
    )

    # ---- 4) 'pub' element: non-empty 'versions' and 'scoring' ----
    pub <- forms$pub

    testthat::expect_true(
      is.list(pub$versions) && length(pub$versions) > 0,
      info = "'forms$pub$versions' must be a non-empty object."
    )

    testthat::expect_true(
      is.list(pub$scoring) && length(pub$scoring) > 0,
      info = "'forms$pub$scoring' must be a non-empty object."
    )

    # ---- 5) Top-level 'config' element: non-empty 'score_categories' ----
    cfg <- forms$config

    testthat::expect_true(
      !is.null(cfg$score_categories),
      info = "'forms$config$score_categories' must exist."
    )

    testthat::expect_true(
      is.list(cfg$score_categories) && length(cfg$score_categories) > 0,
      info = "'forms$config$score_categories' must be a non-empty list."
    )

    # ================================================================
    # === ADDITIONAL TESTS (REVIEW / OPTIONAL, AS REQUESTED) ========
    # ================================================================

    # Additional test 1: File exists
    testthat::expect_true(
      file.exists(path),
      info = "JSON file path must exist."
    )

    # Additional test 2: core forms share a common structure
    core_forms <- intersect(names(forms), c("meta", "pub", "data", "software"))
    for (name in core_forms) {
      form <- forms[[name]]

      testthat::expect_true(
        is.list(form),
        info = paste0("Form '", name, "' must be an object.")
      )

      testthat::expect_true(
        all(c("title", "versions", "date", "scoring", "config", "elements") %in% names(form)),
        info = paste0(
          "Form '", name,
          "' must contain 'title', 'versions', 'date', 'scoring', 'config', and 'elements'."
        )
      )

      testthat::expect_true(
        is.list(form$elements) && length(form$elements) > 0,
        info = paste0("Form '", name, "' must have a non-empty 'elements' array.")
      )
    }

    # Additional test 3: every element has at least 'id' and 'type'
    for (name in core_forms) {
      els <- forms[[name]]$elements
      for (i in seq_along(els)) {
        el <- els[[i]]
        testthat::expect_true(
          all(c("id", "type") %in% names(el)),
          info = paste0(
            "Element ", i, " of form '", name,
            "' must contain at least 'id' and 'type'."
          )
        )
      }
    }

    # Additional test 4: 'score_categories' entries have at least a 'title'
    for (i in seq_along(cfg$score_categories)) {
      cat_i <- cfg$score_categories[[i]]

      testthat::expect_true(
        is.list(cat_i),
        info = paste0(
          "score_categories entry ", i,
          " should be an object (JSON object -> list)."
        )
      )

      testthat::expect_true(
        "title" %in% names(cat_i),
        info = paste0(
          "score_categories entry ", i,
          " should contain a 'title' field."
        )
      )
    }

    # Additional test 5: 'forms$config' links to existing forms
    # (meta, pubs, data, software entries referencing actual forms)
    testthat::expect_true(
      "meta" %in% names(cfg),
      info = "'forms$config' should contain a 'meta' configuration."
    )
    testthat::expect_true(
      "pubs" %in% names(cfg),
      info = "'forms$config' should contain a 'pubs' configuration."
    )
    testthat::expect_true(
      "data" %in% names(cfg),
      info = "'forms$config' should contain a 'data' configuration."
    )
    testthat::expect_true(
      "software" %in% names(cfg),
      info = "'forms$config' should contain a 'software' configuration."
    )
  })
}
