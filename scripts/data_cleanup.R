# Assumes data were exported from Qualtrics using the 'More Options' > 'Split multi-value fields into columns' option.
library(dplyr)
library(gtools)

# In ~/.Renviron file: DATA_PATH = "/Path/to/data/folder"
data_path <- Sys.getenv("DATA_PATH")

data <- read.csv(paste0(data_path, "/dummy_data_split_cols.tsv"),
  header = TRUE,
  sep = "\t",
  check.names = F,
  fileEncoding = "utf-16", # N.B. Qualtrics exports in UTF-16
  stringsAsFactors = F
)

# Qualtrics adds a bunch of columns at the beginning that we don't care about,
# e.g. StartDate, EndDate, Duration. Drop these.
data <- data %>% select(consent_form_2:last_col())
# ^Not sure why qualtrics names this column "consent_form_2" instead of just "consent_form" but whatever.

# It also adds two rows we don't care about
data <- data %>% slice(-c(1, 2))

# It also arranges columns in a sort of arbitrary order; let's reorder them.
# This command sorts alphabetically so questions are no longer in survey order FYI
data <- data %>% select(mixedsort(names(.)))


# Write PII to a separate file
pii_cols <- c(
  "orb_repos_now",
  "orb_repos_later",
  "email_orb_info_yes_1",
  "email_orb_info_email",
  "stay_in_touch_email",
  "stay_in_touch_boxes_1",
  "stay_in_touch_boxes_2"
)

pii <- data %>% select(all_of(pii_cols))

write.table(pii,
  paste0(data_path, "/repos_and_emails.tsv"),
  quote = FALSE,
  row.names = FALSE,
  sep = "\t"
)

data <- data %>% select(-all_of(pii_cols))

# Put qualitative responses in a separate file
qual_cols <- c(
  "final_thoughts",
  "field_of_study_1",
  "subfield"
)

qual <- data %>% select(ends_with("_TEXT"), all_of(qual_cols))

write.table(qual,
  paste0(data_path, "/raw_qual_responses.tsv"),
  quote = FALSE,
  row.names = FALSE,
  sep = "\t"
)

data <- data %>% select(-ends_with("_TEXT"), -all_of(qual_cols))

# Save deidentified quantitative data
write.table(data,
  paste0(data_path, "/deidentified_no_qual.tsv"),
  quote = FALSE,
  row.names = FALSE,
  sep = "\t"
)
