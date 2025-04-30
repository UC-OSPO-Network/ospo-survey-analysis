# Run this script after data_cleanup.R to get an overview of participation rates.
# Doesn't write new data or figures, just prints out some info.

# Load packages and functions
suppressWarnings(suppressMessages(source("utils.R")))




data <- load_qualtrics_data("survey", "deidentified_no_qual.tsv")

# How many participants are in the dataset?
nrow(data)


# How many participants are experienced contributors?
# (Answered True to the first question, True or False to the second question)
status <- data %>% select(
  starts_with("contributor_status")
)
status %>%
  count(contributor_status_1, contributor_status_2)


fc <- data %>% select(starts_with("future_contributors"))
fc_cleaned <- fc %>% # Remove uninformative rows
  filter(!(if_all(everything(), ~ is.na(.) | . == "")))

# How many people were "future contributors" only?
nrow(fc_cleaned)
codenames <- c(
  "Accessible conferences" = "Conferences",
  "Access to free, feature-rich" = "Compute environments",
  "Educational materials" = "Education",
  "An open source discussion" = "Learning community",
  "Dedicated grants" = "Grants",
  "Networking opportunities" = "Industry connections",
  "Job/internship opportunities" = "Academic connections",
  "Other " = "Other",
  "Assistance identifying potential" = "Identifying funding",
  "Legal and licensing" = "Legal support",
  "A mentor/mentee" = "Mentorship"
)
fc_cleaned <- shorten_long_responses(fc_cleaned, codenames)
fc_cleaned <- rename_cols_based_on_entries(fc_cleaned)
fc_summary <- fc_cleaned %>% summarise(across(everything(), ~ sum(!is.na(.) & . != "")))
fc_long <- fc_summary %>%
  pivot_longer(
    everything(),
    names_to = "Response",
    values_to = "Count"
  )
fc_long <- reorder_factor_by_column(fc_long, Response, Count, descending = TRUE)
basic_bar_chart(fc_long, "Response", "Count", "Future contributors' needs")
save_plot("future_contributor_needs.tiff", 6, 4)

# How many participants are from each campus?
campus <- data.frame(table(data$campus))
campus <- reorder_factor_by_column(campus, Var1, Freq, descending = TRUE)
basic_bar_chart(campus, "Var1", "Freq", "Response rates by campus")
save_plot("responses_by_campus.tiff", 6, 4)

# How many participants are from each job category?
data.frame(table(data$job_category))
# The blank answers are participants who are neither past nor future contributors.

# How many participants are from each field of study?
data.frame(table(data$field_of_study))

# How many participants are from each staff category?
data.frame(table(data$staff_categories))

# How many participants have contributed to each type of project?
proj_types <- data %>% select(starts_with("project_types"))
proj_types <- strip_descriptions(proj_types)
# Shorten "Other (Please specify..." to "Other"
proj_types <- gsub("^Other.*", "Other", proj_types)
proj_types_cleaned <- rename_cols_based_on_entries(proj_types)
# Count non-NA and non-empty responses in each column
proj_types_cleaned %>% summarise(across(everything(), ~ sum(!is.na(.) & . != "")))
