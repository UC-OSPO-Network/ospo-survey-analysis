#!/usr/bin/env Rscript

project_root <- here::here() # requires that you be somewhere in the
# project directory (not above it)
# packages
suppressMessages(source(file.path(project_root, "scripts/packages.R")))
# functions and objects used across scripts
suppressMessages(source(file.path(project_root, "scripts/utils.R")))

destination_dir <- file.path(
  DATA_PATH,
  "motivations_responses"
)

qual_data <- load_qualtrics_data("qual_responses.tsv")
responses <- data.frame(
  participantID = rownames(qual_data),
  motivations = qual_data$motivations_11_TEXT
)

final_clean <- exclude_empty_rows(responses, strict = TRUE)

duds <- c(7) # people who wrote things like "N/A" or "<blank>"
final_clean <- final_clean %>% filter(!(participantID %in% duds))


write.table(
  final_clean,
  file.path(DATA_PATH, "motivations.tsv"),
  quote = FALSE,
  row.names = FALSE,
  sep = "\t"
)

if (!file.exists(destination_dir)) {
  dir.create(destination_dir)
}


for (rownum in seq(nrow(final_clean))) {
  write.table(
    final_clean[rownum, "motivations"],
    file.path(
      destination_dir,
      sprintf("%s.txt", final_clean[rownum, "participantID"])
    ),
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t"
  )
}
