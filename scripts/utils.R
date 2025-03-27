# Utils for loading or installing packages

# If a package is already installed, it is loaded
# If a package is not installed, it is installed and then loaded
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE, quietly = TRUE)
    }
  }
}

required_packages <- c(
  "ggplot2",
  "dplyr",
  "readr",
  "tidyr",
  "patchwork",
  "scales",
  "gtools",
  "tools"
)
load_packages(required_packages)

# Utils for reading data

######### EDIT ME #########
subfolder <- "synthetic_data"
###########################

# In my ~/.Renviron file, I have DATA_PATH = "/Path/to/data/folder"
# To reuse this code, edit your .Renviron file or just write the path here
data_path <- Sys.getenv("DATA_PATH")
data_path <- paste0(data_path, subfolder, "/")

load_qualtrics_data <- function(filename) {
  read.csv(
    paste0(data_path, filename),
    header = TRUE,
    sep = "\t",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Utils for plotting

colors <- c(
  # modified from https://sronpersonalpages.nl/~pault/
  "#332288",
  "#88CCEE",
  "#44AA99",
  "#117733",
  "#999933",
  "#DDCC77",
  "#CC6677",
  "#882255",
  "#AA4499",
  "#353535"
)

# Save a plot
# Path is in my ~/.Renviron file
figure_path <- Sys.getenv("FIGURE_PATH")
save_plot <- function(fname, w, h) {
  ggsave(
    filename = fname,
    path = figure_path,
    width = w,
    height = h,
    device = "tiff",
    dpi = 700
  )
}




# Utils to clean data

# For all entries in the data frame, strip text after the colon
strip_descriptions <- function(df) {
  apply(df, MARGIN = c(1, 2), FUN = function(x) strsplit(x, ":")[[1]][1])
}

# Return a data frame with new column names, based on the entries in each column
rename_cols_based_on_entries <- function(df) {
  colnames(df) <- sapply(seq_len(ncol(df)), function(x) {
    get_unique_vals(df, x)
  })
  as.data.frame(df)
}
# Propose a column name based on the entries in that column.
# Only works if all entries in a column are the same.
# Ignores NA and empty strings.
get_unique_vals <- function(df, col_num) {
  unique_vals <- unique(df[, col_num])
  unique_vals <- unique_vals[!(is.na(unique_vals) | unique_vals == "")]
  stopifnot(length(unique_vals) == 1)
  unique_vals
}

# Example usage:
# r$> df <- data.frame(
#     col1 = c("A", "A", "", NA, "A"),
#     col2 = c("B", "", "B", NA, NA)
# )
# r$> get_unique_vals(df, 2)
# [1] "B"


# Replace values in a data frame with new values based on a list of codes.
recode_dataframe <- function(df, codes) {
  return(
    as.data.frame(lapply(df, recode_column, codes))
  )
}

recode_column <- function(column, codes) {
  return(
    names(codes)[match(column, codes)]
  )
}

rename_cols_based_on_codenames <- function(df, codes) {
  return(
    df %>%
      rename(any_of(codes))
  )
}



# Functions to calculate summary statistics

calculate_mode <- function(x) {
  x <- na.omit(x) # Remove NAs
  if (length(x) == 0) {
    return(NA)
  } # Return NA if no values
  uniq_vals <- unique(x)
  freq <- tabulate(match(x, uniq_vals))
  mode_val <- uniq_vals[which.max(freq)]
  return(mode_val)
}
