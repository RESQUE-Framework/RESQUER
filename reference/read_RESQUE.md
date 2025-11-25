# Read a single RESQUE JSON file and do basic preprocessing (elaborated preprocessing happens in `preprocess()`). TODO: Why is some preprocessing here, and some in the other function? Make a principled design.

Read a single RESQUE JSON file and do basic preprocessing (elaborated
preprocessing happens in
[`preprocess()`](https://resque-framework.github.io/RESQUER/reference/preprocess.md)).
TODO: Why is some preprocessing here, and some in the other function?
Make a principled design.

## Usage

``` r
read_RESQUE(file, update_forms = FALSE, verbose = FALSE)
```

## Arguments

- file:

  Path to RESQUE JSON file

- update_forms:

  Should the `forms` node in the json (which contains the scoring
  information) be overwritten with the current version?

- verbose:

  Show diagnostic information?
