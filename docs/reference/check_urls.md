# Check URLs in Text for Validity

This function searches text for URLs, extracts them, and checks whether
they resolve to valid websites. It handles both broken URLs (connection
errors, HTTP errors) and URLs that technically resolve but lead to "not
found" pages.

## Usage

``` r
check_urls(text, tout = 5)
```

## Arguments

- text:

  A character string containing potential URLs to be checked.

- tout:

  The timeout, defaults to 5 seconds

## Value

A data frame with the following columns:

- url:

  The extracted URLs from the text.

- valid:

  Logical value indicating whether each URL is valid.

- status_code:

  HTTP status code returned (if available).

- error:

  Error message if the URL is invalid, NA otherwise.

## Details

The function first extracts all URLs from the input text using regular
expressions, then cleans them by removing trailing punctuation that
might have been incorrectly included during extraction. For each URL, it
attempts an HTTP request and checks both the status code and page
content to determine validity.

URLs are considered invalid if:

- The connection fails

- They return HTTP error codes (4xx, 5xx)

- They return a 2xx or 3xx status code but the page content suggests a
  "not found" page

## Note

The function requires the 'httr' and 'stringr' packages and will attempt
to install them if not present.

## Examples

``` r
if (FALSE) { # \dontrun{
text <- "Check our website at https://example.com and https://invalid-url-example.xyz"
results <- check_urls(text)
print(results)

# Example with multiple URLs, some valid and some invalid
text2 <- "We deliver open data at https://osf.io/f5zjr/; more material is found at
          https://osf.io/f5zjl/ and https://osf.oi/asdf."
results2 <- check_urls(text2)
print(results2)
} # }
```
