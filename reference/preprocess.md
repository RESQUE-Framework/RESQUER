# Preprocess and enrich the raw information from an applicant's JSON

This function preprocesses the applicant data and enriches it with
additional data. Needs an internet connection to query the BIP! and
OpenAlex APIs.

## Usage

``` r
preprocess(applicant, verbose = FALSE)
```

## Arguments

- applicant:

  The applicant data to be preprocessed (as loaded with the
  `read_RESQUE` function).

- verbose:

  Show diagnostic information?

## Value

Preprocessed applicant data.
