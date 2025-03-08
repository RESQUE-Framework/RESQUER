#' Render an HTML or PDF profile of a single applicant
#'
#' @param json_path The path to the applicant's JSON file
#' @param output_file The file name (optionally including a path) of the output report. If NA, it uses the last name from the applicant plus the current date-time as filename.
#' @param template The path to the .Rmd file with the profile. If set to `NA`(default), the package's built-in profile is used.
#' @return The path to the rendered file
#' @export
#' @importFrom quarto quarto_render
#' @importFrom jsonlite read_json
#'
render_profile <- function(json_path, output_file = NA, template = NA) {

  # Path to the qmd template
  if (is.na(template)) {
    template <- system.file("qmds", "RESQUE_profile.qmd", package = "RESQUER")
  }

  old_wd <- getwd()
  full_json_path <- normalizePath(json_path)
  temp_dir <- paste0(tempdir(), "/RESQUEPROFILE")
  if (!dir.exists(temp_dir)) {dir.create(temp_dir, recursive = TRUE)}

  # copy template to the temp_dir, render there
  file.copy(template, temp_dir)

  # get name of applicant:
  js <- read_json(json_path, simplifyVector = TRUE)
  LastName <- js[1, "LastName"]
  FullName <- paste(js[1, "FirstName"], js[1, "LastName"])
  print(paste0("Creating profile report for ", FullName))
  if (is.na(output_file)) {
    output_file <- paste0(dirname(full_json_path), "/", LastName, "_report.html")
  }

  setwd(temp_dir)
  # Render the Rmd file
  quarto::quarto_render(input = basename(template),
                    output_file = basename(output_file),
                    output_format = "html",
                    execute_dir = temp_dir,
                    execute_params = list(
                      FullName = FullName,
                      json_path = full_json_path)
                    )

  setwd(old_wd)

  # Ensure the output directory exists
  if (!dir.exists(dirname(output_file))) {dir.create(dirname(output_file), recursive = TRUE)}

  # Copy the rendered file to the desired output directory
  file.copy(paste0(temp_dir, "/", basename(output_file)), output_file, overwrite=TRUE)

  # Return the path to the final output file
  return(output_file)
}

