#' Update the \code{"forms"} element in a RESQUE JSON file
#'
#' Reads an existing RESQUE JSON file and replaces its \code{"forms"} element
#' with the \code{"forms"} element from another JSON file. The updated JSON is
#' written to a temporary file whose path is returned. The original RESQUE
#' file is not modified.
#'
#' Optionally, the replacement JSON file can be validated against the expected
#' forms schema (via \code{test_forms_json_structure()}) before the update
#' is applied.
#'
#' @param path Character scalar. Path to the original RESQUE JSON file whose
#'   \code{"forms"} element should be replaced.
#' @param newform Character scalar. Path to a JSON file that contains a
#'   top-level \code{"forms"} element. This \code{"forms"} object will be
#'   inserted into the RESQUE JSON.
#' @param test Logical scalar. If \code{TRUE}, the function runs
#'   \code{test_forms_json_structure(newform)} before performing the update
#'   and will fail if the new forms JSON does not pass the structural tests.
#'   Defaults to \code{FALSE}.
#'
#' @return
#' A character scalar giving the path to a temporary JSON file containing the
#' updated RESQUE content. This file is created via \code{\link{tempfile}} and
#' is not automatically deleted.
#'
#' @examples
#' \dontrun{
#'   # Replace forms in an existing RESQUE JSON and validate new forms first
#'   updated_path <- update_forms(
#'     path    = "inst/extdata/resque_original.json",
#'     newform = "inst/extdata/v0.8.1forms.json",
#'     test    = TRUE
#'   )
#'
#'   # Then read via your RESQUE reader
#'   resque_obj <- read_RESQUE(updated_path)
#' }
#'
#' @importFrom jsonlite fromJSON toJSON


update_forms <- function(path, newform, test=FALSE) {

  if (test) {
    test_forms_json_structure(newform)
  }

  # Read original JSON as a raw list (no vector simplification)
  x <- fromJSON(path, simplifyVector = FALSE)

  # Read forms payload (as UTF-8 text)
  forms_txt <- readChar(newform, file.info(newform)$size, useBytes = TRUE)
  forms_obj <- fromJSON(forms_txt, simplifyVector = FALSE)[["forms"]]

  # Replace the first element's "forms"
  stopifnot(is.list(x), length(x) >= 1, is.list(x[[1]]))
  x[[1]][["forms"]] <- forms_obj

  # Serialize -> fix only the '<\/' pattern -> write bytes
  json_txt <- toJSON(x, auto_unbox = TRUE, pretty = TRUE)
  json_txt2 <- gsub("<\\/", "</", json_txt, fixed = TRUE)

  tmpfile <- tempfile(fileext = ".json")
  writeLines(json_txt2, tmpfile, useBytes = TRUE)
  return(tmpfile)
}
