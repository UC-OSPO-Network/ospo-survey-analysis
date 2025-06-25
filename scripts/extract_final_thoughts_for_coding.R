#!/usr/bin/env Rscript

project_root <- here::here() # requires that you be somewhere in the
# project directory (not above it)
# packages
suppressMessages(source(file.path(project_root, "scripts/packages.R")))
# functions and objects used across scripts
suppressMessages(source(file.path(project_root, "scripts/utils.R")))


qual_data <- load_qualtrics_data("qual_responses.tsv")
responses <- data.frame(
  participantID = rownames(qual_data),
  final_thoughts = qual_data$final_thoughts
)

final_clean <- exclude_empty_rows(responses, strict = TRUE)

write.table(
  final_clean,
  file.path(Sys.getenv("DATA_PATH"), "final_thoughts.tsv"),
  quote = FALSE,
  row.names = FALSE,
  sep = "\t"
)
