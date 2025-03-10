#' Check URLs in Text for Validity
#'
#' This function searches text for URLs, extracts them, and checks whether they resolve
#' to valid websites. It handles both broken URLs (connection errors, HTTP errors) and
#' URLs that technically resolve but lead to "not found" pages.
#'
#' @param text A character string containing potential URLs to be checked.
#' @param tout The timeout, defaults to 5 seconds
#'
#' @return A data frame with the following columns:
#'   \item{url}{The extracted URLs from the text.}
#'   \item{valid}{Logical value indicating whether each URL is valid.}
#'   \item{status_code}{HTTP status code returned (if available).}
#'   \item{error}{Error message if the URL is invalid, NA otherwise.}
#'
#' @details
#' The function first extracts all URLs from the input text using regular expressions,
#' then cleans them by removing trailing punctuation that might have been incorrectly
#' included during extraction. For each URL, it attempts an HTTP request and checks
#' both the status code and page content to determine validity.
#'
#' URLs are considered invalid if:
#' \itemize{
#'   \item The connection fails
#'   \item They return HTTP error codes (4xx, 5xx)
#'   \item They return a 2xx or 3xx status code but the page content suggests a "not found" page
#' }
#'
#' @note The function requires the 'httr' and 'stringr' packages and will attempt to
#' install them if not present.
#'
#' @examples
#' \dontrun{
#' text <- "Check our website at https://example.com and https://invalid-url-example.xyz"
#' results <- check_urls(text)
#' print(results)
#'
#' # Example with multiple URLs, some valid and some invalid
#' text2 <- "We deliver open data at https://osf.io/f5zjr/; more material is found at
#'           https://osf.io/f5zjl/ and https://osf.oi/asdf."
#' results2 <- check_urls(text2)
#' print(results2)
#' }
#'
#' @importFrom httr HEAD GET content status_code timeout
#' @importFrom stringr str_extract_all str_extract str_squish
#'
#' @export
check_urls <- function(text, tout=5) {

  text <- stringr::str_squish(text)

  # URL regex pattern
  # This pattern captures http:// and https:// URLs but excludes trailing punctuation
  url_pattern <- "https?://[\\w\\-\\.]+\\.[a-zA-Z]{2,}(?:/[\\w\\-\\.~!$&'\\(\\)\\*\\+,;=:/?#\\[\\]@%]*)*"

  # Extract all URLs from the text
  raw_urls <- str_extract_all(text, url_pattern)[[1]]

  # Clean URLs by removing trailing punctuation that shouldn't be part of the URL
  urls <- sapply(raw_urls, function(url) {
    # Remove common punctuation that might appear at the end of URLs in text
    cleaned_url <- gsub("[;,.:!?)]+$", "", url)
    return(cleaned_url)
  })

  if (length(urls) == 0) {
    return(data.frame(url = character(), valid = logical(), status_code = integer(), error = character(), stringsAsFactors = FALSE))
  }

  # Function to check if a URL is valid
  check_url <- function(url) {
    tryCatch({
      # Use HEAD request first (more efficient)
      response <- HEAD(url, timeout(tout))
      status <- status_code(response)

      # If HEAD request returns 405 Method Not Allowed, try GET
      if (status == 405) {
        response <- GET(url, timeout(tout))
        status <- status_code(response)
      }

      # Check if the page indicates a "not found" or error message
      if (status >= 200 && status < 400) {
        # For 2xx and 3xx status codes, do a GET to check content
        page_content <- content(GET(url, timeout(tout)), "text", encoding = "UTF-8")

        # Look for common "not found" patterns in the page content
        not_found_patterns <- c(
          "page not found",
          "404",
          "not available",
          "no longer exists",
          "doesn't exist",
          "could not be found"
        )

        content_lowercase <- tolower(page_content)
        not_found_match <- sapply(not_found_patterns, function(pattern) {
          grepl(pattern, content_lowercase)
        })

        # Check title tag for "not found" indicators
        title_match <- str_extract(content_lowercase, "<title>.*?</title>")
        title_not_found <- FALSE
        if (!is.na(title_match)) {
          title_not_found <- any(sapply(not_found_patterns, function(pattern) {
            grepl(pattern, title_match)
          }))
        }

        # If any not found patterns are detected
        is_not_found_page <- any(not_found_match) || title_not_found

        return(list(
          valid = !is_not_found_page,
          status = status,
          error = if (is_not_found_page) "Page indicates content not found" else NA
        ))
      } else {
        return(list(
          valid = FALSE,
          status = status,
          error = paste("HTTP error:", status)
        ))
      }
    }, error = function(e) {
      return(list(
        valid = FALSE,
        status = NA,
        error = paste("Connection error:", e$message)
      ))
    })
  }

  # Check each URL
  results <- lapply(urls, check_url)

  # Combine results
  data.frame(
    url = urls,
    valid = sapply(results, function(x) x$valid),
    status_code = sapply(results, function(x) x$status),
    error = sapply(results, function(x) x$error),
    stringsAsFactors = FALSE
  )
}
