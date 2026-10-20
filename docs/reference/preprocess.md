# Preprocess and enrich the raw information from an applicant's JSON

This function preprocesses the applicant data and enriches it with
additional data. Needs an internet connection to query the BIP! and
OpenAlex APIs.

## Usage

``` r
preprocess(applicant, get_BIP = TRUE, get_OpenAlex = TRUE, verbose = FALSE)
```

## Arguments

- applicant:

  The applicant data to be preprocessed (as loaded with the
  `read_RESQUE` function).

- get_BIP:

  Should the BIP! API be polled for impact indicators? If FALSE, NA
  values are returned.

- get_OpenAlex:

  Should the papers be polled from OpenAlex? If FALSE, NA values are
  returned. If FALSE, many indexes cannot be computed, including:
  Citation counts, FNCS, author networks

- verbose:

  Show diagnostic information?

## Value

Preprocessed applicant data.
