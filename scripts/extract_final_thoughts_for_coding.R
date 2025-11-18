#!/usr/bin/env Rscript

# This script will not be useful to anyone reproducing this analysis.
# It ingests a data file that I did not make public, because it allows
# the reader to associate a participant's qualitative responses with all
# their other responses.

# The purpose of this script is to parse the qualitative responses
# from Q12 into many little files, so that I can upload those files to Taguette.

# Note that this script is fragile! "Dud" rows are hard-coded on line 21.
# These are people who wrote things like "N/A" or "<blank>" that we don't
# want in the final "cleaned" data file.

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

duds <- c(331, 188, 303)

# Sanity check
final_clean %>% filter(participantID %in% duds)

final_clean <- final_clean %>% filter(!(participantID %in% duds))

write.table(
  final_clean,
  file.path(DATA_PATH, "final_thoughts.tsv"),
  quote = FALSE,
  row.names = FALSE,
  sep = "\t"
)

dest_folder = file.path(DATA_PATH, "final_thoughts_responses")
if (dir.exists(dest_folder)) {
  # If it exists, delete it
  unlink(dest_folder, recursive = TRUE)
}

dir.create(dest_folder)

for (rownum in seq(nrow(final_clean))) {
  write.table(
    final_clean[rownum, "final_thoughts"],
    file.path(
      DATA_PATH,
      "final_thoughts_responses",
      sprintf("%s.txt", final_clean[rownum, "participantID"])
    ),
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t"
  )
}
