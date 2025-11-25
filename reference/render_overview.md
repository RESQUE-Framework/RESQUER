# Render an HTML overview of multiple applicants

Render an HTML overview of multiple applicants

## Usage

``` r
render_overview(
  json_folder,
  output_file = NA,
  template = NA,
  anonymous = FALSE,
  clear_cache = FALSE
)
```

## Arguments

- json_folder:

  The path to a folder with multiple JSON files

- output_file:

  The file name (optionally including a path) of the output report. If
  NA, it uses the folder name plus the current date-time as filename and
  stores it in the same folder as the source json files.

- template:

  The path to the .qmd file with the profile. If set to `NA`(default),
  the package's built-in profile is used.

- anonymous:

  If `TRUE`, all candidate names are replaced by A, B, C, ...

- clear_cache:

  The computations (e.g., citation statistics) are cached in the file
  `applicant_data.RData` (stored in the same folder as the jsons). If
  `clear_cache = TRUE`, the file is deleted and recomputed.

## Value

The path to the rendered file
