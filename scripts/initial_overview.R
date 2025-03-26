# Run this script after data_cleanup.R to get an overview of participation rates.
# Doesn't write new data, just prints out some info.

# Load packages and functions
suppressWarnings(suppressMessages(source("utils.R")))


data <- load_qualtrics_data("deidentified_no_qual.tsv")

# How many participants are in the dataset?
nrow(data)

# How many participants are from each campus?
data.frame(table(data$campus))

# How many participants are from each job category?
data.frame(table(data$job_category))

# How many participants are from each field of study?
data.frame(table(data$field_of_study))

# How many participants are from each staff category?
data.frame(table(data$staff_categories))

fc <- data %>% select(starts_with("future_contributors"))
fc_cleaned <- fc %>% # Remove uninformative rows
  filter(!(if_all(everything(), ~ is.na(.) | . == "")))

# How many people were "future contributors"?
nrow(fc_cleaned)
codenames <- list(
  "Conferences" = "Accessible conferences or hackathons",
  "Compute environments" = "Access to free, feature-rich computing environments",
  "Education" = "Educational materials and workshops on programming languages, popular packages, etc.",
  "Learning community" = "An open source discussion group and learning community",
  "Grants" = "Dedicated grants for open-source project sustainability",
  "Industry connections" = "Networking opportunities with industry",
  "Academic connections" = "Job/internship opportunities at other academic institutions",
  "Other" = "Other (Please specify. Multiple answers should be comma-separated.)",
  "Identifying funding" = "Assistance identifying potential funding sources",
  "Legal support" = "Legal and licensing support",
  "Mentorship" = "A mentor/mentee program"
)
fc_cleaned <- recode_dataframe(fc_cleaned, codenames)
fc_cleaned <- rename_cols_based_on_entries(fc_cleaned)
fc_cleaned %>% summarise(across(everything(), ~ sum(!is.na(.) & . != "")))


# How many participants have contributed to each type of project?
proj_types <- data %>% select(starts_with("project_types"))
proj_types <- strip_descriptions(proj_types)
# Shorten "Other (Please specify..." to "Other"
proj_types <- gsub("^Other.*", "Other", proj_types)
proj_types_cleaned <- rename_cols_based_on_entries(proj_types)
# Count non-NA and non-empty responses in each column
proj_types_cleaned %>% summarise(across(everything(), ~ sum(!is.na(.) & . != "")))
