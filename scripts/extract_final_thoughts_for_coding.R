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

duds <- c(194, 309, 337) # people who wrote things like "N/A" or "<blank>"
final_clean <- final_clean %>% filter(!(participantID %in% duds))


write.table(
  final_clean,
  file.path(Sys.getenv("DATA_PATH"), "final_thoughts.tsv"),
  quote = FALSE,
  row.names = FALSE,
  sep = "\t"
)

for (rownum in seq(nrow(final_clean))) {
  write.table(
    final_clean[rownum, "final_thoughts"],
    file.path(
      Sys.getenv("DATA_PATH"),
      "final_thoughts_responses",
      sprintf("%s.txt", final_clean[rownum, "participantID"])
    ),
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t"
  )
}
