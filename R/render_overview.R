get_directory <- function(path) {
  info <- file.info(path)

  if (is.na(info$isdir)) {
    stop("Path does not exist.")
  }

  if (!info$isdir) {
    warning("The path points to a file. Returning its containing directory.")
    return(dirname(path))
  }

  return(normalizePath(path, mustWork = TRUE))
}


#' Render an HTML overview of multiple applicants
#'
#' @param json_folder The path to a folder with multiple JSON files
# @param show_inter Show/hide the section on internationality and interdisciplinarity
#' @param output_file The file name (optionally including a path) of the output report. If NA, it uses the folder name plus the current date-time as filename and stores it in the same folder as the source json files.
#' @param template The path to the .qmd file with the profile. If set to `NA`(default), the package's built-in profile is used.
#' @param clear_cache The computations (e.g., citation statistics) are cached in the file `applicant_data.RData` (stored in the same folder as the jsons). If `clear_cache = TRUE`, the file is deleted and recomputed.
#' @param anonymous If `TRUE`, all candidate names are replaced by A, B, C, ...
#' @return The path to the rendered file
#' @export
#' @importFrom quarto quarto_render
#' @importFrom jsonlite read_json
#'
render_overview <- function(json_folder, output_file = NA, template = NA, anonymous = FALSE, clear_cache = FALSE) {

  # for debugging:
  # json_folder = "/Users/felix/LMU/DGPs Kommission Open Science/RESQUE/Overview"
  # template="/Users/felix/Documents/Github/RESQUE-Framework/RESQUER/inst/overview_qmd/RESQUE_overview.qmd"

  # Path to the qmd template: prefer local development file if present
  if (is.na(template)) {
    dev_tmpl <- file.path("RESQUER", "inst", "overview_qmd", "RESQUE_overview.qmd")
    if (file.exists(dev_tmpl)) {
      template <- normalizePath(dev_tmpl, mustWork = TRUE)
    } else {
      template <- system.file("overview_qmd", "RESQUE_overview.qmd", package = "RESQUER")
    }
  }
  # Debug: show which template file is used and working directory
  message("[render_overview] working directory: ", getwd())
  message("[render_overview] using template: ", template)

  old_wd <- getwd()
  full_json_folder <- get_directory(json_folder)
  temp_dir <- paste0(tempdir(), "/RESQUEOVERVIEW")
  if (!dir.exists(temp_dir)) {dir.create(temp_dir, recursive = TRUE)}

  # copy template and extra files to the temp_dir, render there (overwrite stale copies)
  file.copy(template, temp_dir, overwrite = TRUE)

  dir.create(paste0(temp_dir, "/assets"), recursive = TRUE, showWarnings = FALSE)
  list.files(paste0(dirname(template), "/assets"), recursive = TRUE, full.names = TRUE) |>
    file.copy(paste0(temp_dir, "/assets"), overwrite = TRUE)

  # get name of folder:
  foldername <- basename(full_json_folder)
  print(paste0("Creating overview for ", foldername))
  if (is.na(output_file)) {
    output_file <- paste0(full_json_folder, "/", foldername, "_overview.html")
  }

  setwd(temp_dir)
  # Render the Rmd file
  quarto::quarto_render(input = basename(template),
                        output_file = basename(output_file),
                        output_format = "html",
                        execute_dir = temp_dir,
                        execute_params = list(
                          json_folder = full_json_folder,
                          clear_cache = clear_cache,
                          anonymous = anonymous
                        )
  )

  setwd(old_wd)

  # Ensure the output directory exists
  if (!dir.exists(dirname(output_file))) {dir.create(dirname(output_file), recursive = TRUE)}

  # Copy the rendered file to the desired output directory
  file.copy(paste0(temp_dir, "/", basename(output_file)), output_file, overwrite=TRUE)

  # Return the path to the final output file
  return(output_file)
}

