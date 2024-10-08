% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractors.R
\name{extract_indicator_scores}
\alias{extract_indicator_scores}
\title{Extract scores on indicator level}
\usage{
extract_indicator_scores(applicant_list)
}
\arguments{
\item{applicant_list}{A list containing the RESQUE jsons from multiple applicants.}
}
\value{
A data frame containing the indicator scores for each applicant.
}
\description{
This function processes a list of applicants, extracting the Relative Rigor Score (RRS)
for each single indicator of each applicant and compiling them into a single long-format data frame.
}
