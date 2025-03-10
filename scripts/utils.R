# Function to load or install packages
# If a package is already installed, it is loaded
# If a package is not installed, it is installed and then loaded
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

required_packages <- c("ggplot2", "dplyr", "readr", "tidyr", "patchwork", "scales")
load_packages(required_packages)


# In my ~/.Renviron file, I have DATA_PATH = "/Path/to/data/folder"
data_path <- Sys.getenv("DATA_PATH")

load_qualtrics_data <- function(filename) {
  read.csv(
    paste0(data_path, paste0("/", filename)),
    header = TRUE,
    sep = "\t",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# For all entries in the data frame, strip text after the colon
strip_descriptions <- function(df) {
  apply(df, MARGIN = c(1, 2), FUN = function(x) strsplit(x, ":")[[1]][1])
}

# Creates a new column name based on the entries in that column
# Takes a data frame and returns a vector of column names
# Only works if all entries in a column are the same (or NA)
rename_cols_based_on_entries <- function(df) {
  sapply(seq_len(ncol(df)), function(column_num) {
    unique_vals <- unique(df[, column_num])
    unique_vals <- unique_vals[!is.na(unique_vals)]
    stopifnot(length(unique_vals) == 1)
  })
}
