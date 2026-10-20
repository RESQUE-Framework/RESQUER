# RESQUER

An R package for analyzing the data from the [RESQUE Research Quality
Evaluation](https://www.resque.info) scheme. This is work in progress,
and likely to change.

Install the development versions from GitHub with:

    install.packages("remotes")

    # OAmetrics has some convenience functions to retrieve data from
    # OpenAlex, such as h-index, JIF, citation counts, etc.
    remotes::install_github("nicebread/OAmetrics", auth_token=NULL)
    remotes::install_github("RESQUE-Framework/RESQUER", auth_token=NULL)

The package is not on CRAN yet.

## How to create individual profiles (html files):

You need [Quarto](https://quarto.org/docs/get-started/) installed on
your system:

``` r

install.packages("quarto")
```

Then run this code:

``` r

library(quarto)
library(RESQUER)

# The rendered html file is created in the same folder as the input json file.
# `outfile` also contains the path to the rendered file

# Render a single profile:
outfile <- render_profile("resque_123456.json", show_inter = TRUE)

# You can also render multiple profiles in a loop:
json_files <- list.files("path_to_folder", pattern="resque_.*\\.json", full.names = TRUE)
for (j in json_files) {
  print(paste0("Rendering ", j))
  render_profile(j, show_inter = TRUE)
}
```

## How to create an overview of multiple candidates (html file):

You need [Quarto](https://quarto.org/docs/get-started/) installed on
your system. The
[`render_overview()`](https://resque-framework.github.io/RESQUER/reference/render_overview.md)
function creates an overview of multiple candidates in a single html
file.

    library(quarto)
    library(RESQUER)

    # The rendered html file is created in the same folder as the input json files.
    # `outfile` also contains the path to the rendered overview file.
    folder_path = "/path/to/json/folder"
    outfile <- render_overview(folder_path)
