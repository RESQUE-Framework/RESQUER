# Fix some problems with old exported json files

Fix some problems with old exported json files

## Usage

``` r
validate_json(file, update_forms = FALSE, verbose = TRUE)
```

## Arguments

- file:

  Path to RESQUE JSON file

- update_forms:

  Should the `forms` node in the json (which contains the scoring
  information) be overwritten with the current version?

- verbose:

  Show diagnostic information?
