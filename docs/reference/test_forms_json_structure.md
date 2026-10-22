# Test structure of a "forms" JSON file

Runs a set of testthat checks to verify that a JSON file containing form
definitions conforms to the expected schema. These json files are used
to replace the metadata of an existing json with exported RESQUE data
(e.g. if the scoring rules have been updated).

## Usage

``` r
test_forms_json_structure(path)
```

## Arguments

- path:

  Character scalar. Path to the JSON file to be tested.

## Value

This function is called for its side effect of running testthat
expectations. It returns the result of the
[`testthat::test_that()`](https://testthat.r-lib.org/reference/test_that.html)
call (invisibly).

## Details

The core checks performed are:

- The file contains valid JSON.

- The top-level JSON object contains only a single element named
  `"forms"`.

- The `"forms"` object contains at least the elements `"meta"`, `"pub"`,
  and `"config"`.

- The `"pub"` element contains non-empty `"versions"` and `"scoring"`
  elements.

- The top-level `"config"` element contains a non-empty
  `"score_categories"` element.

In addition, the function performs several **additional** schema checks:

- The file path exists.

- Core forms (`"meta"`, `"pub"`, `"data"`, `"software"`) share a common
  structure (e.g. must contain `title`, `versions`, `date`, `scoring`,
  `config`, `elements`).

- Every element within `elements` has at least `"id"` and `"type"`
  fields.

- Each entry in `score_categories` contains at least a `"title"` field.

- `forms$config` contains meta-entries for `"meta"`, `"pubs"`, `"data"`,
  and `"software"`.

These additional checks are intended to catch common schema deviations
and can be reviewed, modified, or removed as needed.

## Examples

``` r
if (FALSE) { # \dontrun{
  test_forms_json_structure("inst/extdata/v0.8.1forms.json")
} # }
```
