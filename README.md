# RESQUER

An R package for analyzing the [RESQUE Research Quality Evaluation](https://resque-framework.github.io/website/) schemes.
This is work in progress, and likely to change.

Install the development versions from GitHub with:

```
install.packages("remotes")
remotes::install_github("nicebread/OAmetrics", auth_token=NULL)
remotes::install_github("RESQUE-Framework/RESQUER", auth_token=NULL)
```

The package is not on CRAN yet.


## How to create an individual profile (html file):

You need [Quarto](https://quarto.org/docs/get-started/) installed on your system.

```
library(quarto)
library(RESQUER)

# The rendered html file is created in the same folder as the input json file.
# `outfile` also contains the path to the rendered file
json_path = "resque_123456.json"
outfile <- render_profile(json_path)
```


## How to preview the interactive dashboard (Shiny App):

For the interactive dashboard, you need [Quarto](https://quarto.org/docs/get-started/) installed on your system.

Launch the dashboard with the three included demo profiles:

```
library(quarto)
library(RESQUER)
quarto_serve(system.file("dashboard/dashboard_shiny.qmd", package="RESQUER"))
```


