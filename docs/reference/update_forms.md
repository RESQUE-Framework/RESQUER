# Update the `"forms"` element in a RESQUE JSON file

Reads an existing RESQUE JSON file and replaces its `"forms"` element
with the `"forms"` element from another JSON file. The updated JSON is
written to a temporary file whose path is returned. The original RESQUE
file is not modified.

## Usage

``` r
update_forms(path, newform, test = FALSE)
```

## Arguments

- path:

  Character scalar. Path to the original RESQUE JSON file whose
  `"forms"` element should be replaced.

- newform:

  Character scalar. Path to a JSON file that contains a top-level
  `"forms"` element. This `"forms"` object will be inserted into the
  RESQUE JSON.

- test:

  Logical scalar. If `TRUE`, the function runs
  `test_forms_json_structure(newform)` before performing the update and
  will fail if the new forms JSON does not pass the structural tests.
  Defaults to `FALSE`.

## Value

A character scalar giving the path to a temporary JSON file containing
the updated RESQUE content. This file is created via
[`tempfile`](https://rdrr.io/r/base/tempfile.html) and is not
automatically deleted.

## Details

Optionally, the replacement JSON file can be validated against the
expected forms schema (via
[`test_forms_json_structure()`](https://resque-framework.github.io/RESQUER/reference/test_forms_json_structure.md))
before the update is applied.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Replace forms in an existing RESQUE JSON and validate new forms first
  updated_path <- update_forms(
    path    = "inst/extdata/resque_original.json",
    newform = "inst/extdata/v0.8.1forms.json",
    test    = TRUE
  )

  # Then read via your RESQUE reader
  resque_obj <- read_RESQUE(updated_path)
} # }
```
