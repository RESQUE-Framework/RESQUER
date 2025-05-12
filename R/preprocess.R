# This internal function counts the ratio of missing indicators.
# Can be used to check whether a publication is present that has no
# indicator values at all (e.g., if you do a bulk import via ORCID, and do not
# enter any indicators)
get_missing <- function(pub) {
  # select relevant columns (some are prepopulated with values by default, they never contain missings)
  pub_red <- pub |>
    select(contains("P_"), -contains("P_TypeMethod"), -P_Suitable, -contains("P_CRediT"), -P_TopPaper_Select)

  is_empty_perc <- function(x) sum(is.na(x) | x == "")/length(x)
  apply(pub_red, 1, is_empty_perc)
}


#' Preprocess and enrich the raw information from an applicant's JSON
#'
#' This function preprocesses the applicant data and enriches it with additional data.
#' Needs an internet connection to query the BIP! and OpenAlex APIs.
#'
#' @param applicant The applicant data to be preprocessed (as loaded with the `read_RESQUE` function).
#' @param verbose Show diagnostic information?
#'
#' @return Preprocessed applicant data.
#'
#' @importFrom dplyr filter mutate arrange
#' @importFrom stringr str_replace_all str_trim
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_fetch_memory
#' @importFrom openalexR oa_fetch
#' @importFrom utils URLencode
#' @importFrom lubridate year
#' @importFrom OAmetrics normalize_dois normalize_ORCIDs get_BIP FNCS get_network
#' @export

# For testing:
# applicant <- read_RESQUE(system.file("extdata/demo_profiles/resque_Schoenbrodt.json", package="RESQUER"))
# applicant <- read_RESQUE(system.file("extdata/demo_profiles/resque_Gaertner.json", package="RESQUER"))
# applicant <- read_RESQUE("/Users/felix/LMU/DGPs Kommission Open Science/RESQUE/Mainz Test 2/resque_schönbrodt_0.6.2.json")
# applicant <- read_RESQUE("/Users/felix/LMU/DGPs Kommission Open Science/RESQUE/Mainz Test 2/resque_röseler.json")

preprocess <- function(applicant, verbose=FALSE) {

  # create an empty vector that gets populated with relevant notes
  applicant$preprocessing_notes <- c()

  # create missing indicator variables
  ind_vars <- c("P_PreregisteredReplication", "P_MeritStatement")
  for (i in ind_vars) {
    if (!i %in% colnames(applicant$indicators)) applicant$indicators[, i] <- NA
  }

  # fix meta data
  if (!is.null(applicant$meta$YearPhD)) {
    applicant$meta$YearPhD <- as.numeric(applicant$meta$YearPhD)
  } else {
    applicant$meta$YearPhD <- NA
  }
  if (!is.null(applicant$meta$AcademicAgeBonus)) {
    applicant$meta$AcademicAgeBonus <- as.numeric(applicant$meta$AcademicAgeBonus)
    if (is.na(applicant$meta$AcademicAgeBonus)) applicant$meta$AcademicAgeBonus <- 0
  } else {
    applicant$meta$AcademicAgeBonus <- 0
  }
  applicant$meta$AcademicAge <- year(Sys.Date()) - applicant$meta$YearPhD - applicant$meta$AcademicAgeBonus

  # normalize some variables
  applicant$meta$LastName <- str_trim(applicant$meta$LastName)
  applicant$meta$ORCID <- normalize_ORCIDs(applicant$meta$ORCID)

  if (!is.na(applicant$meta$ORCID)) {
    # Retrieve OpenAlex Author ID
    author_info <- oa_fetch(entity="authors", orcid = applicant$meta$ORCID)
    if (is.null(author_info)) {
      applicant$meta$OA_author_id <- NA
    } else {
      applicant$meta$OA_author_id <- author_info$id
    }
  } else {
    applicant$meta$OA_author_id <- NA
    warning("No ORCID provided - some indexes cannot be computed.")
  }

  # assign new verbose factor levels
  applicant$indicators$P_Preregistration2 <- factor(applicant$indicators$P_Preregistration, levels=c("NotApplicable", "No", "Yes", "RegisteredReport"), labels=c("Not<br>Applicable", "Not<br>prereg", "Prereg", "Registered<br>Report"))

  applicant$indicators$replication <- factor(applicant$indicators$P_PreregisteredReplication, levels=c("NotApplicable", "No", "Yes"), labels=c("not<br>applicable", "No", "Yes"))

  # Check % of missing indicators (or even completely empty publications)
  # Later, we exclude publications with too many missings from the analysis
  # Flag all-empty applicants
  # ------------------------------------

  applicant$indicators$ind_missing <- get_missing(applicant$indicators)
  applicant$all_empty <- all(applicant$indicators$ind_missing > .95)

  # Fix some logical dependencies:
  # ------------------------------------

  # If there is no preregistration, there cannot be a preregistered replication:
  applicant$indicators$replication[is.na(applicant$indicators$replication) & applicant$indicators$P_Preregistration2 == "Not preregistered"] <- "No"

  # If the data set was "Reuse of someone else's existing data set" or P_Data == "No", we set
  # Open Data to "not applicable" and automatically provide a justification
  # (users cannot enter stuff in these fields, as they are hidden when
  # P_Data_Source_ReuseOther == "ReuseOther")

  applicant$indicators <- add_variables(applicant$indicators, c(
    "P_Data_Source_ReuseOther", "P_Data_Source_NewOwn", "P_Data_Source_ReuseOwn",
    "P_Data_Source_ReuseCompilation", "P_Data_Source_Simulated"), default=FALSE)


  pure_reuse_other <- applicant$indicators$P_Data_Source_ReuseOther == TRUE &
                      is.na(applicant$indicators$P_Data_Open) &
                      applicant$indicators$P_Data_Source_NewOwn == FALSE &
                      applicant$indicators$P_Data_Source_ReuseOwn == FALSE &
                      applicant$indicators$P_Data_Source_ReuseCompilation == FALSE &
                      applicant$indicators$P_Data_Source_Simulated == FALSE

  no_data <- applicant$indicators$P_Data == "No"

  if (any(pure_reuse_other, na.rm = TRUE) | any(no_data, na.rm = TRUE)) {
    change_index <- union(which(pure_reuse_other), which(no_data))

    applicant$indicators$P_Data_Open[change_index] <- "NotApplicable"

    if (is.null(applicant$indicators$P_Data_Open_NAExplanation)) applicant$indicators$P_Data_Open_NAExplanation <- ""
    applicant$indicators$P_Data_Open_NAExplanation[change_index] <- "(automatically added: For reused data sets of other researchers and for 'no data', we automatically set P_Data_Open to 'notApplicable')."

    note <- paste0("For ", length(change_index), " publication(s) with P_Data_Source = 'Reuse of someone else\'s existing data set' or P_Data = 'No', P_Data_Open has been set to 'notApplicable' and a justification has been added.)")
    warning(note)
    applicant$preprocessing_notes <- c(applicant$preprocessing_notes, note)
  }

  if (any(no_data, na.rm = TRUE)) {
    change_index <- which(no_data)

    applicant$indicators$P_ReproducibleScripts[change_index] <- "NotApplicable"

    if (is.null(applicant$indicators$P_ReproducibleScripts_NAExplanation )) applicant$indicators$P_ReproducibleScripts_NAExplanation  <- ""
    applicant$indicators$P_ReproducibleScripts_NAExplanation[change_index] <- "(automatically added: For 'no data', we automatically set P_ReproducibleScripts to 'notApplicable')."

    note <- paste0("For ", length(change_index), " publication(s) with P_Data = 'No', P_ReproducibleScripts has been set to 'notApplicable' and a justification has been added.)")
    warning(note)
    applicant$preprocessing_notes <- c(applicant$preprocessing_notes, note)
  }


  # Set all "notApplicable" claims to "notAvailable" when there is no justification
  # ------------------------------------

  # appl = applicant object
  check_justification <- function(ind, var, justi, notApp="NotApplicable", no="NotAvailable") {
    if (var %in% colnames(ind) &&
        justi %in% colnames(ind) &&
        any(ind[, var] == notApp, na.rm=TRUE)) {
      no_justi <- (ind[, var] == notApp) & (is.na(ind[, justi]) | ind[, justi] == "")
      no_justi[is.na(no_justi)] <- FALSE
      if (any(no_justi)) {
        ind[, var][no_justi] <- no
        note <- paste0("For ", sum(no_justi), " publication(s) without justification, ", var, " has been changed from '", notApp, "' to '", no, "'.")
        warning(note)
        applicant$preprocessing_notes <<- c(applicant$preprocessing_notes, note)
      }
    }

    return(ind)
  }


  applicant$indicators<- check_justification(applicant$indicators,
      var="P_Sample_RepresentativenessRelevance", justi="P_Sample_RepresentativenessRelevance_NAExplanation",
      notApp="NotApplicable", no="No")

  # Note: We don't do this correction for P_Sample_CrossCultural_NAExplanation,
  # because there is no clear "no" answer.

  applicant$indicators <- check_justification(applicant$indicators,
      var="P_Stimuli", justi="P_Stimuli_NAExplanation",
      notApp="NotApplicable", no="No")

  applicant$indicators <- check_justification(applicant$indicators,
      var="P_Stimuli_Representative", justi="P_Stimuli_Representative_NAExplanation",
      notApp="NotApplicable", no="NotConsidered")

  applicant$indicators <- check_justification(applicant$indicators,
      var="P_Data_Open", justi="P_Data_Open_NAExplanation",
      notApp="NotApplicable", no="NotAvailable")

  applicant$indicators <- check_justification(applicant$indicators,
      var="P_ReproducibleScripts", justi="P_ReproducibleScripts_NAExplanation",
      notApp="NotApplicable", no="NotAvailable")

  applicant$indicators <- check_justification(applicant$indicators,
      var="P_IndependentVerification", justi="P_IndependentVerification_NAExplanation",
      notApp="NotApplicable", no="NotAvailable")

  applicant$indicators <- check_justification(ind=applicant$indicators,
      var="P_OpenMaterials", justi="P_OpenMaterials_NAExplanation",
      notApp="NotApplicable", no="NotAvailable")

  applicant$indicators <- check_justification(applicant$indicators,
      var="P_Preregistration", justi="P_Preregistration_NAExplanation",
      notApp="NotApplicable", no="No")

  applicant$indicators <- check_justification(applicant$indicators,
      var="P_Preregistration_Replication", justi="P_Preregistration_Replication_NAExplanation",
      notApp="NotApplicable", no="No")

  # Note: We don't do this correction for P_Theorizing_NAExplanation,
  # because there is no clear "no" answer.



  # Check all provided URLs whether they are valid
  # (this does not check the content of the website, only if it exists, and whether a URL has been provided at all)
  # ------------------------------------

  # TODO: How to deal with false negatives? E.g., URLs with a space that can easily be fixed?

  # URL_fields <- applicant$indicators |> select(contains("_Identifier"))
  #
  # if ("P_Data_Open_Identifier" %in% colnames(URL_fields)) {
  #   for (i in 1:nrow(URL_fields)) {
  #     if (!is.na(URL_fields$P_Data_Open_Identifier[i])) {
  #       ch <- check_urls(URL_fields$P_Data_Open_Identifier[i])
  #       if (!any(ch$valid)) {
  #         applicant$indicators$P_Data_Open[i] <- "NotAvailable"
  #         note <- paste0("For 1 publication without valid URL, P_Data_Open has been set to 'NotAvailable")
  #         warning(note)
  #         applicant$preprocessing_notes <- c(applicant$preprocessing_notes, note)
  #       }
  #     }
  #   }
  # }

  # TODO: Add for other Identifiers

  # clean the dois:
  applicant$indicators$doi <- normalize_dois(applicant$indicators$DOI)
  applicant$indicators$doi_links_md <- paste0("[", applicant$indicators$doi, "](", applicant$indicators$doi, ")")

  applicant$indicators$title_links_html <- paste0("<a href='", applicant$indicators$doi, "'>", applicant$indicators$Title, "</a>")

  if (!is.null(applicant$indicators$P_TopPaper_Select)) {
    applicant$indicators$P_TopPaper_Select[is.na(applicant$indicators$P_TopPaper_Select)] <- FALSE
  } else {
    applicant$indicators$P_TopPaper_Select <- FALSE
  }


  # CRediT preprocessing
  #--------------------------------------------------------

  credit_tab <- table(applicant$credit$Role, applicant$credit$Degree)

  # arrange credit roles by weight (Lead > Support > Equal), summed across works
  applicant$credit_ordered <- as.data.frame.matrix(credit_tab) %>%
    mutate(
      LeadEqual = Lead + Equal,
      Sum = Lead + Equal + Support + NoRole,
      # normalized weight: All "Lead" (=max) would be 1
      weight = (Lead * 4 + Equal * 3 + Support * 1) / (Sum * 4),
      Role = rownames(.)
    ) %>%
    arrange(-LeadEqual, -Support)

  applicant$credit$Role_ordered <- factor(applicant$credit$Role, levels = rev(rownames(applicant$credit_ordered)))

  # for the compact display: group factor levels

  applicant$credit <- applicant$credit %>%
    mutate(Role_collapsed = fct_recode(Role,
       "Analysis" = "Data Curation",
       "Analysis" = "Formal Analysis",
       "Analysis" = "Investigation",
       "Analysis" = "Methodology",
       "Analysis" = "Software",
       "Conceptualization" = "Conceptualization",
       "Writing" = "Visualization",
       "Writing" = "Writing: Original draft",
       "Writing" = "Writing: Review & Editing",
       "Administration" = "Funding Acquisition",
       "Administration" = "Project Administration",
       "Administration" = "Resources",
       "Administration" = "Supervision",
       "Administration" = "Validation"
    ))

  # The "CRediT involvement" categories
  # ---------------------------------------------------------------------
  # TODO: Refactor into function

  credit_inv <- applicant$indicators %>% select(contains("CRediT"))
  roles <- colnames(credit_inv) |> str_replace("P_CRediT_", "") |> unCamel0()
  roles[roles == "Writing Review Editing"] <- "Writing: Review & Editing"
  roles[roles == "Writing Original Draft"]  <- "Writing: Original draft"

  main_roles <- rep("", nrow(credit_inv))
  for (i in 1:nrow(credit_inv)) {
    leads <- credit_inv[i, ] == "Lead"
    equals <- credit_inv[i, ] == "Equal"
    main_roles[i] <- paste0(
      ifelse(sum(leads)>0, paste0(
        "<b>Lead:</b> ",
        paste0(roles[leads], collapse=", ")), ""),
      ifelse(sum(equals)>0, paste0(
        "<br><b>Equal:</b> ",
        paste0(roles[equals], collapse=", ")), "")
    )
  }

  credit_inv$sum_lead <- apply(credit_inv[, 1:14], 1, function(x) sum(x=="Lead"))
  credit_inv$sum_equal <- apply(credit_inv[, 1:14], 1, function(x) sum(x=="Equal"))
  credit_inv$sum_leadequal <- apply(credit_inv[, 1:14], 1, function(x) sum(x %in% c("Lead", "Equal")))
  credit_inv$sum_support <- apply(credit_inv[, 1:14], 1, function(x) sum(x=="Support"))

  # define the categories
  credit_inv$CRediT_involvement <- factor(rep("Low", nrow(credit_inv)), levels=c("Low", "Medium", "High", "Very High"), ordered=TRUE)
  credit_inv$CRediT_involvement[credit_inv$sum_lead >= 3] <- "Very High"
  credit_inv$CRediT_involvement[credit_inv$sum_leadequal >= 5] <- "Very High"

  credit_inv$CRediT_involvement[credit_inv$sum_lead %in% c(1, 2)] <- "High"
  credit_inv$CRediT_involvement[credit_inv$sum_leadequal %in% c(3, 4) & credit_inv$CRediT_involvement != "Very High"] <- "High"

  credit_inv$CRediT_involvement[credit_inv$sum_equal %in% c(1, 2) & credit_inv$sum_lead == 0] <- "Medium"
  credit_inv$CRediT_involvement[credit_inv$sum_support >= 5 & credit_inv$CRediT_involvement <= "Medium"] <- "Medium"

  applicant$indicators$CRediT_involvement <- credit_inv$CRediT_involvement
  applicant$indicators$CRediT_involvement_roles <- main_roles

  rm(credit_tab, credit_inv, main_roles)


  #----------------------------------------------------------------
  # Select publications suitable for the RRS computation in object `rigor_pubs`.
  # Select publications suitable for the impact table in object `impact_pubs`.
  # All further preprocessing is restricted to these two objects.
  #----------------------------------------------------------------

  # Split the research outputs into types, reduce to suitable submissions
  applicant$indicators <- applicant$indicators |>
    mutate(
      # exclude fully empty publication records without indicator values (or only trivial information)
      rigor_pub = type == "Publication" & P_Suitable == "Yes" & ind_missing < .95,
      impact_pub = type == "Publication" & (P_Suitable == "Yes" & ind_missing < .95 | P_Suitable == "No")
    )

  # TODO: Semi-hotfix: As P_IndependentVerification might often be removed, set it to NA if it is missing
  # Should be done for all central variables?
  if (!"P_IndependentVerification" %in% colnames(applicant$indicators)) {
    applicant$indicators$P_IndependentVerification <- NA
  }

  applicant$rigor_pubs  <- applicant$indicators |> filter(rigor_pub == TRUE)
  applicant$impact_pubs <- applicant$indicators |> filter(impact_pub == TRUE)


  if (sum(!applicant$indicators$rigor_pub) > 0) {
    note <- paste0(sum(!applicant$indicators$rigor_pub), " publication(s) were removed from the rigor score computations because no indicators were provided.")
    warning(note)
    applicant$preprocessing_notes <- c(applicant$preprocessing_notes, note)
  }

  if (sum(!applicant$indicators$impact_pub) > 0) {
    note <- paste0(sum(!applicant$indicators$impact_pub), " publication(s) were removed from the impact table because no indicators were provided and no manual processing was requested.")
    warning(note)
    applicant$preprocessing_notes <- c(applicant$preprocessing_notes, note)
  }


  #----------------------------------------------------------------
  # Call BIP! API for impact measures
  #----------------------------------------------------------------

  if (nrow(applicant$impact_pubs) > 0) {
    applicant$BIP <- get_BIP(dois=applicant$impact_pubs$dois_normalized, verbose=verbose)
    applicant$BIP_n_papers <- sum(applicant$BIP$pop_class <= "C5", na.rm=TRUE)
    applicant$BIP_n_papers_top10 <- sum(applicant$BIP$pop_class <= "C4", na.rm=TRUE)
  } else {
    applicant$BIP <- NA
    applicant$BIP_n_papers <- NA
    applicant$BIP_n_papers_top10 <- NA
  }


  #----------------------------------------------------------------
  # Retrieve submitted works from OpenAlex (with error handling)
  #----------------------------------------------------------------

  if (nrow(applicant$impact_pubs) > 0) {
    # Safely fetch works; catch API errors or empty responses
    fetch_res <- tryCatch(
      oa_fetch(entity = "works", doi = normalize_dois(applicant$impact_pubs$doi)),
      error = function(e) {
        warning("OpenAlex API request failed: ", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(fetch_res) || nrow(fetch_res) == 0) {
      # No data fetched, skip OpenAlex metrics
      applicant$OAlex_papers <- NA
      applicant$preprocessing_notes <- c(
        applicant$preprocessing_notes,
        "OpenAlex fetch failed or returned no records; team size and impact metrics unavailable."
      )
    } else {
      OAlex_papers <- fetch_res

    #cat(paste0(nrow(OAlex_papers), " out of ", nrow(all_pubs), " submitted publications could be automatically retrieved with openAlex.\n"))

    if (nrow(OAlex_papers) < nrow(applicant$impact_pubs)) {
      note <- paste0(
        '## The following papers could *not* be retrieved by openAlex:\n\n',
        applicant$impact_pubs[!applicant$impact_pubs$doi %in% OAlex_papers$doi, ] %>%
          select(Title, Year, DOI, P_TypePublication)
      )
      warning(note)
      applicant$preprocessing_notes <- c(applicant$preprocessing_notes, note)
    }

    # Determine author list column (author or authorships) and compute number of authors
    if ("author" %in% names(OAlex_papers)) {
      author_list <- OAlex_papers$author
    } else if ("authorships" %in% names(OAlex_papers)) {
      author_list <- OAlex_papers$authorships
    } else {
      author_list <- rep(list(NA), nrow(OAlex_papers))
    }
    # Compute number of authors per paper; if structure unexpected, return NA
    n_authors <- vapply(author_list, function(x) {
      if (is.data.frame(x)) nrow(x) else NA_integer_
    }, integer(1))
    OAlex_papers$n_authors <- n_authors
    # Categorize team sizes
    OAlex_papers$team_category <- cut(n_authors,
                                      breaks = c(0, 1, 5, 15, Inf),
                                      labels = c("Single authored",
                                                 "Small team (<= 5 co-authors)",
                                                 "Large team (6-15 co-authors)",
                                                 "Big Team (> 15 co-authors)"))

      applicant$OAlex_papers <- OAlex_papers
      rm(OAlex_papers)
    }
  } else {
    applicant$OAlex_papers <- NA
  }

  #----------------------------------------------------------------
  # Get FNCS
  #----------------------------------------------------------------

  #----------------------------------------------------------------
  # Get Field- and age-normalized citation scores (FNCS)
  # Only if OAlex data is available
  if (nrow(applicant$impact_pubs) > 0 &&
      is.data.frame(applicant$OAlex_papers) &&
      "doi" %in% names(applicant$OAlex_papers) &&
      nrow(applicant$OAlex_papers) > 0) {
    c_counts_psy_2001_2023 <- readRDS(
      file = system.file("ref_set_psy/c_counts_psy_2001_2023.RDS", package="RESQUER")
    )
    fncs <- tryCatch(
      FNCS(
        dois = applicant$OAlex_papers$doi,
        ref_set = c_counts_psy_2001_2023,
        verbose = FALSE
      ),
      error = function(e) {
        warning("FNCS computation failed: ", conditionMessage(e))
        return(NA)
      }
    )
    applicant$FNCS <- fncs
  } else {
    applicant$FNCS <- NA
  }

  #----------------------------------------------------------------
  # Get TOP factor of the publication venues
  #----------------------------------------------------------------

  #----------------------------------------------------------------
  # Get TOP factor of the publication venues
  # Only if OAlex data is available
  if (nrow(applicant$impact_pubs) > 0 &&
      is.data.frame(applicant$OAlex_papers) &&
      "issn_l" %in% names(applicant$OAlex_papers)) {
    TOP <- read.csv(
      system.file("extdata", "top-factor.csv", package="RESQUER"),
      stringsAsFactors = FALSE
    )
    applicant$TOP_journals <- TOP %>%
      select(issn = Issn, Journal, Total) %>%
      filter(issn %in% applicant$OAlex_papers$issn_l)
    rm(TOP)
  } else {
    applicant$TOP_journals <- NA
  }


  #----------------------------------------------------------------
  # Compute Relative Rigor Score RRS
  #----------------------------------------------------------------

  if (nrow(applicant$impact_pubs) > 0) {
    applicant$RRS <- compute_RRS(applicant)

    # merge RRS scores into the other objects
    if (!is.na(applicant$RRS$overall_score)) {
      applicant$indicators <- left_join(applicant$indicators, applicant$RRS$paper_scores, by="doi")
      applicant$rigor_pubs <- left_join(applicant$rigor_pubs, applicant$RRS$paper_scores, by="doi")
      applicant$impact_pubs <- left_join(applicant$impact_pubs, applicant$RRS$paper_scores, by="doi")
    } else {
      applicant$RRS <- NA
      applicant$indicators$RRS_overall <- NA
      applicant$rigor_pubs$RRS_overall <- NA
      applicant$impact_pubs$RRS_overall <- NA
    }
  } else {
    applicant$RRS <- NA
  }

  #----------------------------------------------------------------
  # Get internationalization and interdisciplinarity scores
  #----------------------------------------------------------------

  #----------------------------------------------------------------
  # Get internationalization and interdisciplinarity scores (network)
  #----------------------------------------------------------------
  if (!is.null(applicant$meta$OA_author_id) && !is.na(applicant$meta$OA_author_id) && (nrow(applicant$impact_pubs) > 0)) {
    # Wrap network computation to avoid errors if expected columns missing
    nw <- tryCatch(
      get_network(works = applicant$OAlex_papers,
                  author.id = applicant$meta$OA_author_id,
                  min_coauthorships = 1,
                  verbose = FALSE),
      error = function(e) {
        warning("Could not compute network metrics: ", e$message)
        # Return NA for all expected fields
        list(
          international_evenness = NA,
          country_codes_repeated = NA,
          internationalization_string = NA,
          n_coauthors_international = NA,
          n_coauthors_same_country = NA,
          interdisc_evenness = NA,
          primary_fields_tab_reduced = NA,
          subfields_tab = NA,
          topics_tab = NA,
          interdisc_string = NA
        )
      }
    )
    # Populate internationalization
    applicant$internationalization <- list(
      international_evenness = nw$international_evenness,
      country_codes_repeated = nw$country_codes_repeated,
      internationalization_string = nw$internationalization_string,
      n_coauthors_international = nw$n_coauthors_international,
      n_coauthors_same_country = nw$n_coauthors_same_country,
      perc_international = round(nw$n_coauthors_international * 100 /
                                 (nw$n_coauthors_international + nw$n_coauthors_same_country)),
      perc_same_country = round(nw$n_coauthors_same_country * 100 /
                             (nw$n_coauthors_international + nw$n_coauthors_same_country))
    )
    # Populate interdisciplinarity
    applicant$interdisciplinarity <- list(
      interdisc_evenness = nw$interdisc_evenness,
      primary_fields_tab_reduced = nw$primary_fields_tab_reduced,
      subfields_tab = nw$subfields_tab,
      topics_tab = nw$topics_tab,
      interdisc_string = nw$interdisc_string
    )
  } else {
    # No OA author ID or no impact publications: fill with NA
    applicant$internationalization <- list(
      international_evenness = NA,
      country_codes_repeated = NA,
      internationalization_string = NA,
      n_coauthors_international = NA,
      n_coauthors_same_country = NA,
      perc_international = NA,
      perc_same_country = NA
    )
    applicant$interdisciplinarity <- list(
      interdisc_evenness = NA,
      primary_fields_tab_reduced = NA,
      subfields_tab = NA,
      topics_tab = NA,
      interdisc_string = NA
    )
  }



  #----------------------------------------------------------------
  # prepare all data for the open science sparkpie charts
  #----------------------------------------------------------------

  OpenDataPie <- data.frame(
    notApplicable = sum(applicant$rigor_pubs$P_Data_Open == "NotApplicable", na.rm=TRUE),
    No = sum(applicant$rigor_pubs$P_Data_Open == "NotAvailable", na.rm=TRUE),
    Partial = sum(applicant$rigor_pubs$P_Data_Open == "YesParts", na.rm=TRUE),
    Yes = sum(applicant$rigor_pubs$P_Data_Open == "YesEntire", na.rm=TRUE)
  )

  OpenMaterialPie <- data.frame(
    notApplicable = sum(applicant$rigor_pubs$P_OpenMaterials == "NotApplicable", na.rm=TRUE),
    No = sum(applicant$rigor_pubs$P_OpenMaterials == "NotAvailable", na.rm=TRUE),
    Partial = sum(applicant$rigor_pubs$P_OpenMaterials == "YesParts", na.rm=TRUE),
    Yes = sum(applicant$rigor_pubs$P_OpenMaterials == "YesEntire", na.rm=TRUE)
  )

  OpenCodePie <- data.frame(
    notApplicable = sum(applicant$rigor_pubs$P_ReproducibleScripts == "NotApplicable", na.rm=TRUE),
    No = sum(applicant$rigor_pubs$P_ReproducibleScripts == "NotAvailable", na.rm=TRUE),
    Partial = sum(applicant$rigor_pubs$P_ReproducibleScripts == "YesParts", na.rm=TRUE),
    Yes = sum(applicant$rigor_pubs$P_ReproducibleScripts == "YesEntire", na.rm=TRUE)
  )

  # Correction: If applicants claim an independent verification, but provide no link,
  # this is set to "No".
  # TODO: Needs to be done for OD and OM as well.
  applicant$rigor_pubs$P_IndependentVerification[applicant$rigor_pubs$P_IndependentVerification %in% c("WorkflowReproducible", "MainResultsReproducible", "AnalysisReplication") & applicant$rigor_pubs$P_IndependentVerification_Identifier == ""] <- "NotAvailable"

  ReproPie <-  data.frame(
    notApplicable = sum(applicant$rigor_pubs$P_IndependentVerification == "NotApplicable", na.rm=TRUE),
    No = sum(applicant$rigor_pubs$P_IndependentVerification == "No", na.rm=TRUE),
    Workflow = sum(applicant$rigor_pubs$P_IndependentVerification == "WorkflowReproducible", na.rm=TRUE),
    Results = sum(applicant$rigor_pubs$P_IndependentVerification %in% c("MainResultsReproducible", "AllResultsReproducible"), na.rm=TRUE),
    Replication = sum(applicant$rigor_pubs$P_IndependentVerification == "AnalysisReplication", na.rm=TRUE)
  )

  applicant$rigor_pubs$P_Preregistration2 <- factor(applicant$rigor_pubs$P_Preregistration, levels=c("NotApplicable", "No", "Yes", "RegisteredReport"), labels=c("Not Applicable", "Not preregistered", "Preregistration", "Registered Report"))

  PreregPie <- table(applicant$rigor_pubs$P_Preregistration2)

  applicant$OS_pie <- list(
    OpenData = OpenDataPie,
    OpenMaterial = OpenMaterialPie,
    OpenCode = OpenCodePie,
    Repro = ReproPie,
    Prereg = PreregPie
  )

  #----------------------------------------------------------------
  # Some meta-data
  #----------------------------------------------------------------

  applicant$meta$date_exported <- as.POSIXct(applicant$meta$date_exported / 1000, origin = "1970-01-01", tz = "UTC")
  applicant$meta$date_added <- as.POSIXct(applicant$meta$date_added / 1000, origin = "1970-01-01", tz = "UTC")
  applicant$meta$date_created <- as.POSIXct(applicant$meta$date_created / 1000, origin = "1970-01-01", tz = "UTC")
  applicant$meta$date_modified <- as.POSIXct(applicant$meta$date_modified / 1000, origin = "1970-01-01", tz = "UTC")
  applicant$meta$date_analysis <- format(Sys.time(), tz = "UTC", usetz = TRUE)

  return(applicant)
}
