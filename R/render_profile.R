#' Render an HTML or PDF profile of a single applicant
#'
#' @param json_path The path to the applicant's JSON file
#' @param output_file The file name (optionally including a path) of the output report. If NA, it uses the last name from the applicant as filename.
#' @param output_format The output format ("html" or "pdf")
#' @return The path to the rendered file
#' @export
#' @importFrom quarto quarto_render
#' @importFrom jsonlite read_json
#'
render_profile <- function(json_path, output_file = NA, output_format = "html") {

  # For testing purposes:
  #json_path = "/Users/felix/Documents/Github/RESQUE-Framework/RESQUER/inst/extdata/demo_profiles/resque_Gaertner.json"
  #json_path = "/Users/felix/LMU/DGPs Kommission Open Science/RESQUE/Mainz Test 1/resque_leehr.json"
  #qmd_file = "/Users/felix/Documents/Github/RESQUE-Framework/RESQUER/inst/qmds/RESQUE_profile.qmd"
  # output_file = "~/Downloads/test2.html"

  # Path to the qmd template
  qmd_file <- system.file("qmds", "RESQUE_profile.qmd", package = "RESQUER")

  old_wd <- getwd()
  full_json_path <- normalizePath(json_path)
  temp_dir <- paste0(tempdir(), "/RESQUEPROFILE")
  if (!dir.exists(temp_dir)) {dir.create(temp_dir, recursive = TRUE)}

  # copy template to the temp_dir, render there
  file.copy(qmd_file, temp_dir)

  # get last name of applicant:

  js <- read_json(json_path, simplifyVector = TRUE)
  LastName <- js[1, "LastName"]
  FullName <- paste(js[1, "FirstName"], js[1, "LastName"])
  print(paste0("Creating profile report for ", FullName))
  if (is.na(output_file)) {
    output_file <- paste0(LastName, "_report.", output_format)
  }

  setwd(temp_dir)
  # Render the Rmd file
  quarto::quarto_render(input = "RESQUE_profile.qmd",
                    output_file = basename(output_file),
                    output_format = output_format,
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

