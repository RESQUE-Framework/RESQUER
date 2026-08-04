check_quarto <- function(required = "1.8.0") {
  quarto <- Sys.which("quarto")

  if (!nzchar(quarto)) {
    stop(
      "Quarto >= ", required, " is required but was not found.",
      call. = FALSE
    )
  }

  output <- system2(
    quarto,
    "--version",
    stdout = TRUE,
    stderr = TRUE
  )

  version_text <- trimws(output[[1L]])
  installed <- tryCatch(
    package_version(version_text),
    error = function(e) NULL
  )

  if (is.null(installed)) {
    stop(
      "Could not determine the installed Quarto version: ",
      version_text,
      call. = FALSE
    )
  }

  if (installed < package_version(required)) {
    stop(
      "Quarto >= ", required, " is required; found Quarto ",
      installed, ".",
      call. = FALSE
    )
  }

  invisible(installed)
}