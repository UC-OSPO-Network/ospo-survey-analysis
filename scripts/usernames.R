#!/usr/bin/env Rscript

# A script that takes in the usernames and related data from the survey
# and cleans it up for analysis. Does not work on the raw data file;
# works on an intermediate file that has bee somewhat cleaned up.

suppressWarnings(suppressMessages(source("utils.R")))

usr <- load_qualtrics_data("usernames_MANUALLY_CURATED.tsv")
usr <- make_df_binary(usr, cols = c("orb_followup_yes_1"))
usr <- exclude_empty_rows(usr)
usr$personID <- seq(1, nrow(usr))

usr <- usr %>%
  mutate(email = str_split(orb_followup_email, ",")) %>% # split on comma, since one person can have multiple emails
  unnest(email) %>% # unnest takes a list-column (a column where each cell contains a list), and expands it into rows
  mutate(email = str_trim(email)) %>% # remove leading and trailing whitespace
  mutate(email = str_to_lower(email)) %>% # make lowercase
  select(-"orb_followup_email") # this column is now redundant


usr_cleaned <- usr %>%
  mutate(usernames = str_split(usernames, ",")) %>%
  unnest(usernames) %>%
  mutate(usernames = str_trim(usernames)) %>%
  mutate(
    is_url = str_detect(usernames, "^https?://"),
    forge = if_else(is_url, "URL", str_extract(usernames, "^[^:]+")),
    username = if_else(
      is_url,
      usernames,
      str_trim(str_remove(usernames, "^[^:]+:"))
    ),
    forge = str_to_lower(forge)
  ) %>%
  mutate(username = replace_na(username, "")) %>% # replace NA with "", in cases where there was no username
  mutate(forge = replace_na(forge, "")) %>%
  select(personID, orb_followup_yes_1, email, forge, username)

usr_cleaned <- usr_cleaned %>%
  mutate(orb_followup_yes_1 = if_else(orb_followup_yes_1 == 1, "yes", "no")) %>%
  rename("would_like_to_be_contacted" = "orb_followup_yes_1")

write.table(usr_cleaned,
  file.path(Sys.getenv("DATA_PATH"), "usernames_FINAL.csv"),
  quote = FALSE,
  row.names = FALSE,
  sep = ","
)
