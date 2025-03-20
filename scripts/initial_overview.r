suppressWarnings(suppressMessages(source("utils.R")))

non_empty_rows <- function(df, col) {
  df %>%
    filter(.data[[col]] != "") %>%
    pull(.data[[col]]) # Return non-empty values in that column
}

data <- load_qualtrics_data("deidentified_no_qual.tsv")

# How many participants are in the dataset?
nrow(data)

# How many participants are from each campus?
data.frame(table(data$campus))

# How many participants are from each job category?
data.frame(table(data$job_category))

# How many participants are from each field of study?
field_data <- load_qualtrics_data("fields_of_study.tsv")
data.frame(table(
  non_empty_rows(field_data, "field_of_study_1")
))
data.frame(table(
  non_empty_rows(field_data, "subfield")
))

data.frame(table(data$staff_categories))

fc <- data %>% select(starts_with("future_contributors"))
fc_cleaned <- fc %>%
  filter(!(if_all(everything(), ~ is.na(.) | . == "")))

# How many people were future contributors only?
# Respondents only see this question if they say they haven't
# contributed to open source in the past but they would like to in the future.
nrow(fc_cleaned)

response_counts <- fc_cleaned %>%
  summarise(across(everything(), ~ sum(!is.na(.) & . != ""), .names = "count_{.col}")) %>%
  # count non-NA and non-empty responses in each column
  # Remove columns with zero counts
  select(where(~ . > 0))

response_counts
