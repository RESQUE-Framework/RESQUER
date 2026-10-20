# Batch import and preprocess RESQUE JSON files

Reads one or more RESQUE JSON files from a directory, optionally
replaces their internal `"forms"` (scoring) definition with an external
JSON specification, preprocesses the resulting objects, and returns them
as a list of applicants.

## Usage

``` r
get_jsons(
  path,
  recursive = TRUE,
  replace_form = NULL,
  get_OpenAlex = FALSE,
  get_BIP = FALSE,
  anonymize_raters = FALSE,
  verbose = TRUE
)
```

## Arguments

- path:

  Character scalar. Directory containing RESQUE JSON files.

- recursive:

  Logical scalar. Should subdirectories be searched recursively for JSON
  files? Passed to
  [`list.files`](https://rdrr.io/r/base/list.files.html). Defaults to
  `TRUE`.

- replace_form:

  Optional character scalar. Path to a JSON file that contains a
  top-level `"forms"` element defining packs/scoring information. If not
  `NULL`, this forms definition replaces the `"forms"` element in each
  RESQUE JSON before preprocessing.

- get_OpenAlex:

  Logical scalar. If `TRUE`, the preprocessing step retrieves/uses
  OpenAlex-based metadata). Defaults to `FALSE`.

- get_BIP:

  Logical scalar. If `TRUE`, the preprocessing step retrieves/uses BIP
  (bibliometric) metadata. Defaults to `FALSE`.

- anonymize_raters:

  Logical scalar. If `TRUE`, external rater names are replaced with
  pseudo-identifiers `"R1"`, `"R2"`, `"R3"`, etc., consistently across
  the imported applicants. Defaults to `FALSE`.

- verbose:

  Logical scalar. If `TRUE`, progress messages about file processing and
  optional form replacement are printed to the console. Defaults to
  `TRUE`.

## Value

A list of preprocessed applicant objects, one entry per JSON file. Each
element corresponds to the output of
[`preprocess`](https://resque-framework.github.io/RESQUER/reference/preprocess.md)
applied to the RESQUE object returned by
[`read_RESQUE`](https://resque-framework.github.io/RESQUER/reference/read_RESQUE.md)
(with or without form replacement).

## Details

If `replace_form` is provided, the function first validates the
replacement forms JSON via
[`test_forms_json_structure`](https://resque-framework.github.io/RESQUER/reference/test_forms_json_structure.md)
and then uses
[`update_forms`](https://resque-framework.github.io/RESQUER/reference/update_forms.md)
to inject the new `"forms"` element into each RESQUE file before calling
[`read_RESQUE`](https://resque-framework.github.io/RESQUER/reference/read_RESQUE.md).
Otherwise, the RESQUE files are read as-is.

Each applicant object is passed through
[`preprocess`](https://resque-framework.github.io/RESQUER/reference/preprocess.md)
to compute derived variables (optionally using OpenAlex and/or BIP
metadata). The function also creates an anonymized pseudo-ID for each
applicant based on their ORCID using `digest::digest(algo = "crc32")`.

Optionally, external rater names can be anonymized by replacing them
with identifiers `"R1"`, `"R2"`, `"R3"`, etc., consistently across all
jsons.

## See also

[`test_forms_json_structure`](https://resque-framework.github.io/RESQUER/reference/test_forms_json_structure.md),
[`update_forms`](https://resque-framework.github.io/RESQUER/reference/update_forms.md),
[`read_RESQUE`](https://resque-framework.github.io/RESQUER/reference/read_RESQUE.md),
[`preprocess`](https://resque-framework.github.io/RESQUER/reference/preprocess.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Basic import of all RESQUE JSON files in a folder
  applicants <- get_jsons(
    path      = "raw_data/Study2/calibration_papers",
    recursive = TRUE
  )

  # Import with updated scoring forms and rater anonymization
  applicants <- get_jsons(
    path            = "raw_data/Study2/calibration_papers",
    replace_form    = "includes/v0.8.1forms.json",
    anonymize_raters = TRUE,
    verbose         = TRUE
  )
} # }
```
