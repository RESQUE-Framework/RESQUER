#' Render an HTML profile of a single applicant
#'
#' @param json_path The path to the applicant's JSON file
#' @param show_inter Show/hide the section on internationality and interdisciplinarity
#' @param output_file The file name (optionally including a path) of the output report. If NA, it uses the last name from the applicant plus the current date-time as filename and stores it in the same folder as the source json file.
#' @param template The path to the .qmd file with the profile. If set to `NA`(default), the package's built-in profile is used.
#' @return The path to the rendered file
#' @export
#' @importFrom quarto quarto_render
#' @importFrom jsonlite read_json
#'
#' @examples
#' \dontrun{
#' # Render single profile
#' render_profile("path_to_file/resque_Einstein.json")
#' 
#' # Render all profiles in a folder
#' for (f in list.files("path_to_folder", pattern="resque_.*\\.json", full.names = TRUE)) {
#'  render_profile(f)
#' }
#' }

render_profile <- function(json_path, show_inter=TRUE, output_file = NA, template = NA) {

  # Path to the qmd template
  if (is.na(template)) {
    template <- system.file("profile_qmd", "RESQUE_profile.qmd", package = "RESQUER")
  }
  # Debug: confirm which template file is in use
  message("[render_profile] Using template file: ", template)
  if (!file.exists(template)) {
    stop("[render_profile] ERROR: QMD template not found at: ", template)
  }
  # Preview first few lines of the template to verify patch
  tmpl_preview <- readLines(template, n = 5, warn = FALSE)
  message("[render_profile] Template preview (first 5 lines):\n", paste(tmpl_preview, collapse = "\n"))

  old_wd <- getwd()
  full_json_path <- normalizePath(json_path)
  temp_dir <- paste0(tempdir(), "/RESQUEPROFILE")
  if (!dir.exists(temp_dir)) {dir.create(temp_dir, recursive = TRUE)}

  # copy template and extra files to the temp_dir, render there
  # Copy the QMD template into a fresh render folder (overwrite any stale copy)
  file.copy(template, temp_dir, overwrite = TRUE)

  dir.create(paste0(temp_dir, "/assets"), recursive = TRUE, showWarnings = FALSE)
  list.files(paste0(dirname(template), "/assets"), recursive = TRUE, full.names = TRUE) |>
    file.copy(paste0(temp_dir, "/assets"), overwrite = TRUE)

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
                      json_path = full_json_path,
                      show_inter = show_inter
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

