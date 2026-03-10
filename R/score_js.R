library(V8)
library(jsonlite)

score_all <- function(research_outputs) {
  ctx <- V8::v8()
  ctx$source(system.file("js", "score2.js", package = "RESQUER"))
  
  scores <- ctx$call("scoreAll", research_outputs)
  scores$scores <- scores$scores[-1, ]
  return(scores)
}

score_all_from_file <- function(research_outputs_file) {
    research_outputs <- jsonlite::fromJSON(
      research_outputs_file, simplifyVector = FALSE
    )
    
    score_all(research_outputs)
}
