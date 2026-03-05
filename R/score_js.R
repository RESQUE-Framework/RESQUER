library(V8)
library(jsonlite)

ctx <- V8::v8()

ctx$source("./R/score2.js")

categories <- c()

score_all <- function(research_outputs) {
  scores <- ctx$call("scoreAll", research_outputs)
  
  # remove the first element: This is the meta-information which has no scores
  scores$scores <- scores$scores[-1, ]

  return(scores)
}

score_all_from_file <- function(research_outputs_file) {
    research_outputs <- jsonlite::fromJSON(
      research_outputs_file, simplifyVector = FALSE
    )
    
    score_all(research_outputs)
}
