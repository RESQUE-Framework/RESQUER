# RESQUER

An R package for analyzing the [RESQUE Research Quality
Evaluation](https://resque-framework.github.io/website/) schemes. This
is work in progress, and likely to change.

Install the development versions from GitHub with:

    install.packages("remotes")
    remotes::install_github("nicebread/OAmetrics", auth_token=NULL)
    remotes::install_github("RESQUE-Framework/RESQUER", auth_token=NULL)

The package is not on CRAN yet.

## How to create an individual profile (html file):

You need [Quarto](https://quarto.org/docs/get-started/) installed on
your system.

    library(quarto)
    library(RESQUER)

    # The rendered html file is created in the same folder as the input json file.
    # `outfile` also contains the path to the rendered file
    json_path = "resque_123456.json"
    outfile <- render_profile(json_path)

    # You can also render multiple profiles in a loop:
    for (f in list.files("path_to_folder", pattern="resque_.*\\.json", full.names = TRUE)) {
     render_profile(f)
    }

## How to create an overview of multiple candidates (html file):

You need [Quarto](https://quarto.org/docs/get-started/) installed on
your system. The
[`render_overview()`](https://resque-framework.github.io/RESQUER/reference/render_overview.md)
function creates an overview of multiple candidates in a single html
file.

    library(quarto)
    library(RESQUER)

    # The rendered html file is created in the same folder as the input json files.
    # `outfile` also contains the path to the rendered file
    folder_path = "/path/to/json/folder"
    outfile <- render_overview(folder_path)

## How to preview the interactive dashboard (Shiny App): THIS IS A WORK IN PROGRESS AND PROBABLY NOT WORKING

For the interactive dashboard, you need
[Quarto](https://quarto.org/docs/get-started/) installed on your system.

Launch the dashboard with the three included demo profiles:

    library(quarto)
    library(RESQUER)
    quarto_serve(system.file("dashboard/dashboard_shiny.qmd", package="RESQUER"))

## Some internal documentation (needs to be properly documented)

- `$indicators` contains all data (also publications without any data).
  - `$impact_pubs` is a subset of `indicators`: contains all
    publications that are eligible for the impact table (i.e., papers
    with sufficient indicator information and papers were a manual
    processing was requested)
    - `$OAlex_papers` is the same set as `$impact_pubs`, but contains
      the full OpenAlex information
  - `$rigor_pubs` is a subset of `$indicators`: contains all
    publications that are eligible for the rigor score and other
    descriptives (i.e., papers with sufficient indicator information)
- There can be `impact_pubs` which are not `rigor_pubs`: E.g., opinion
  papers (which have impact metrics), but where no rigor score could be
  computed.
- There can be `rigor_pubs` which are not `impact_pubs`: E.g., submitted
  papers which have no doi (e.g., a paper on OSF; technically a preprint
  without a doi), but where a rigor could be computed.
