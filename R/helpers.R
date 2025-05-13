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



# robust function to get the number of authors from an OpenAlex record
# FIXME: should move into OAmetrics

get_n_authors <- function(OA_object) {
  n_authors <- sapply(OA_object$author, nrow)

  # find NULL entries
  NULL_entries <- sapply(n_authors, is.null)
  if (any(NULL_entries)) {
    n_authors[NULL_entries] <- NA
  }

  return(n_authors |> unlist())
}
