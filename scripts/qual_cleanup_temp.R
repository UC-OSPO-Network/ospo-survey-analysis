#A last-minute, late-stage script to shuffle all qualitative responses for deposition in Dryad.

# Load packages
project_root <- here::here() # requires that you be somewhere in the
# project directory (not above it)
suppressMessages(source(file.path(project_root, "scripts/packages.R")))
# functions and objects used across scripts
suppressMessages(source(file.path(project_root, "scripts/utils.R")))

set.seed(100)

# Load raw data
data <- load_qualtrics_data("qual_responses.tsv")

for (i in seq(8)) {
  tmp_vec <- data[[i]]
  cname <- names(data)[i]
  tmp_vec <- tmp_vec[nzchar(tmp_vec)]
  tmp_vec <- sample(tmp_vec)
  write(cname, file.path(DATA_PATH, "tmp_qual.txt"), append = TRUE)
  write.table(
    tmp_vec,
    file = file.path(DATA_PATH, "tmp_qual.txt"),
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE
  )
}
