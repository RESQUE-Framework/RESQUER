#' Check a data frame for the existence of columns. If not present, create variables with a default value.
#' @param df Data frame that is checked (and optionally expanded with new variables)
#' @param varnames The variable names that are searched in the df
#' @param default The default value for newly created columns
#' @export
add_variables <- function(df, varnames, default=NA) {
  for (v in varnames) {
    if (!v %in% colnames(df)) df[, v] <- default
  }
  df
}


NA2FALSE <- function(x) {
  x[is.na(x)] <- FALSE
  x
}

#' Replaces CamelCase in a vector with spaces between words
#' @param x A character vector which should be uncameled.
#' @import stringr
#' @export
unCamel0 <- function(x) {
  gsub("([A-Z]){1}", " \\1", x) |> str_trim()
}

#' For all cells in selected columns, replace CamelCase with spaces between words
#' @param df data.frame
#' @param cname Column names in which cells should be uncameled.
#' @import stringr
#' @export
unCamel <- function(df, cname) {
  if (cname %in% colnames(df)) {
    x <-  df[, cname] |> unlist()
    df[, cname] <- unCamel0(x)
  } else {
    df[, cname] <- NA
  }

  return(df)
}

#' @import stringr
clean_title <- function(x) {
  x |>
    str_replace_all("\n", " ") |>
    str_squish()
}


#' Collate the indicator list into a data.frame
#' @param sc Scores object (as provided by `read_RESQUE`)
#' @param pattern regexp pattern that filters the results. The default selects all indicators
#' @export
get_indicators <- function(sc, pattern=".*") {
  res <- data.frame()
  for (i in 1:length(sc)) {
    ind <- sc[[i]]$indicators
    if (!is.null(ind) & is.list(ind) & (length(ind) > 0)) {
      res <- rbind(res, tibble(
        output = i,
        indicator = names(ind),
        value = sapply(ind, "[[", "value"),
        max = sapply(ind, "[[", "max"),
        rel_score = value/max
      ))
    }
  }

  rownames(res) <- NULL
  return(res[grepl(pattern, res$indicator), ])
}


# convert text into numeric, also recognizing German decimal commas
smart_as_numeric <- function(x) {
  nums <- vapply(x, function(z) {
    z <- trimws(z)
    # if it only has commas → assume comma = decimal
    if (grepl("^[-0-9,]+$", z) && grepl(",", z) && !grepl("\\.", z)) {
      z <- sub("^([-+])", "\\1", z)          # preserve sign
      z <- gsub(",", ".", z, fixed = TRUE)
      return(as.numeric(z))
    }
    # if it has both . and , we assume . is thousands sep, , is decimal
    if (grepl("[0-9]\\.[0-9]{3}", z) && grepl(",", z)) {
      z <- gsub("\\.", "", z)                # strip thousands
      z <- gsub(",", ".", z, fixed = TRUE)  # decimal mark
      return( as.numeric(z))
    }
    # otherwise fall back
    suppressWarnings(as.numeric(z))
  }, numeric(1))

  unname(nums)
}


#' Wiggle a number by a random ±margin%
#'
#' Adds a uniformly-distributed random perturbation of up to ±\code{margin}%
#' to \code{x}, returning the result rounded to the same number of decimal
#' places as the original value.
#'
#' @param x A numeric scalar to wiggle.
#' @param margin Maximum percentage perturbation (default: 10).
#' @return A numeric scalar with the same number of decimal places as \code{x}.
#' @examples
#' set.seed(42)
#' wiggle(0.25)        # e.g. 0.27 (still 2 decimal places)
#' wiggle(1.5, margin = 20)  # e.g. 1.4 (still 1 decimal place)
#' wiggle(3)           # e.g. 3 (still 0 decimal places)
#' @export
wiggle <- function(x, margin = 10) {
  if (!is.numeric(x) || is.na(x)) return(x)

  # Detect decimal places in original number
  x_char <- as.character(x)
  decimals <- if (grepl("\\.", x_char)) nchar(sub(".*\\.", "", x_char)) else 0L

  # Apply random perturbation of ±margin%
  result <- x + x * (margin / 100) * runif(1, -1, 1)

  round(result, decimals)
}


#' Check if an object is NULL, NA, or length zero
#'
#' This utility function returns `TRUE` if the input is `NULL`,
#' has length zero, or is an `NA` of length one. Otherwise returns `FALSE`.
#'
#' @param x Any R object.
#'
#' @return A logical scalar: `TRUE` if `x` is considered "null-like",
#'   otherwise `FALSE`.
#'
#' @examples
#' is.nulla(NULL)
#' is.nulla(NA)
#' is.nulla(character(0))
#' is.nulla(1)   # FALSE
#'
#' @export
is.nulla <- function(x) {
  is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))
}



# prefix should be something like `"P_Sample_Location_"`
sum_MC <- function(pubs, prefix) {
  i0 <- pubs |> select(starts_with(prefix), -contains("P_Sample_Location_Comment"))
  res <- list()
  for (j in 1:ncol(i0)) {
    res[[j]] <- sum(i0[, j], na.rm=TRUE)
    names(res[[j]]) <- str_remove(colnames(i0)[j], str_c("^", prefix))
  }
  return(unlist(res))
}
