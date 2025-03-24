# This data-wrangling script reads in the raw data from Qualtrics
# and splits the data into multiple files. It assumes data
# were exported from Qualtrics using 'More Options' > 'Split multi-value fields into columns'.
# Check utils.R and make sure your data path is set correctly
#

suppressWarnings(suppressMessages(source("utils.R")))

write_subset_of_data <- function(df, filename) {
  write.table(df,
    paste0(data_path, filename),
    quote = FALSE,
    row.names = FALSE,
    sep = "\t"
  )
}



data <- read.csv(paste0(data_path, "OSPO_survey_generated_responses.tsv"),
  header = TRUE,
  sep = "\t",
  check.names = FALSE,
  fileEncoding = "utf-16", # N.B. Qualtrics exports in UTF-16
  stringsAsFactors = FALSE
)

# Remove rows where the "Finished" column is not "True".
# This has the added benefit of removing those first two rows
# that Qualtrics generated and that we don't care about.
data <- data %>% filter(Finished == "True")

# Qualtrics adds a bunch of columns at the beginning that we don't care about,
# e.g. StartDate, EndDate, Duration. Drop these.
data <- data %>% select(consent_form_2:last_col())
# ^Not sure why qualtrics names this column "consent_form_2"
# instead of just "consent_form" but whatever.

# It also arranges columns in a sort of arbitrary order; let's reorder them.
# This command sorts alphabetically so questions are no longer in survey order FYI
data <- data %>% select(mixedsort(names(.)))


# Write personally identifiable information (pii) to a separate file
pii_cols <- c(
  "usernames",
  "orb_followup_yes_1",
  "orb_followup_email",
  "stay_in_touch_boxes_1",
  "stay_in_touch_boxes_2",
  "stay_in_touch_email"
)

pii <- data %>% select(all_of(pii_cols))

write_subset_of_data(pii, "pii.tsv")

data <- data %>% select(-all_of(pii_cols))


# Put qualitative responses in a separate file
qual_cols <- c(
  "final_thoughts",
  "subfield"
)

qual <- data %>% select(ends_with("_TEXT"), all_of(qual_cols))
write_subset_of_data(qual, "qual_responses.tsv")

# qual2 <- data %>% select("field_of_study_1", "subfield")
# # Standardize capitalization
# qual2[] <- lapply(qual2, function(x) if (is.character(x)) toTitleCase(x) else x)
# write_subset_of_data(qual2, "fields_of_study.tsv")

# qual3 <- data %>% select("staff_categories_13_TEXT")
# write_subset_of_data(qual3, "staff_categories.tsv")

data <- data %>% select(-ends_with("_TEXT"), -all_of(qual_cols))


# Save deidentified quantitative data
write_subset_of_data(data, "deidentified_no_qual.tsv")
