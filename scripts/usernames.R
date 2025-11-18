#!/usr/bin/env Rscript

# A script that takes in the usernames and related data from the survey
# and cleans it up for analysis. Does not work on the raw data file;
# works on an intermediate file that has been somewhat cleaned up.
# I am doing this so we can contact devs who were interested in being contacted
# about the ORB.

# Here are some notes on my manual curation process:
# For "bare" usernames with no forge mentioned, I added "unknown:" before the username.
# Where people clearly wrote in the name of their project or repository, I found it online and turned their comment into a URL.
# Made sure that input is of the format "GitHub: janedoe, GitLab: janedoe24", where the only commas are separating forge:username pairs, and the only other colons are in URLs.
# When parsing, I extracted the URLs first, so the only colons remaining are within forge:username pairs.
# I deleted one instance of "No longer available"--I guess this was someone who didn't finish the survey?
# I deleted one instance of "Firstname Lastname, GitHub"
# Removed the "@" if somebody put an "@" before their username
# Removed other miscellaneous little notes
# Some people put multiple usernames for the same forge. I'm guessing these are either: 1) a personal UN and a professional/organizational UN or 2) a personal UN and a repo name
# Some of the "usernames" look like repo names. I did not bother to investigate these. After we extract all the usernames we can find, we should try searching for repos with those names we couldn't find.
# At least one person entered "GitLSIT" as their forge. This is UCSB IT's version control system.
# One person put their email instead of their GitHub username. I left this as-is because I couldn't find them online.

project_root <- here::here() # requires that you be somewhere in the
# project directory (not above it)
# packages
suppressMessages(source(file.path(project_root, "scripts/packages.R")))
# functions and objects used across scripts
suppressMessages(source(file.path(project_root, "scripts/utils.R")))


usr <- load_qualtrics_data("orb_usernames/orb_usernames_MANUALLY_CURATED.tsv")
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
  select(personID, orb_followup_yes_1, email, forge, username, campus)

usr_cleaned <- usr_cleaned %>%
  mutate(orb_followup_yes_1 = if_else(orb_followup_yes_1 == 1, "yes", "no")) %>%
  rename("would_like_to_be_contacted" = "orb_followup_yes_1")

usr_cleaned <- usr_cleaned %>% filter(would_like_to_be_contacted == "yes")

campuses <- unique(usr_cleaned$campus)

for (camp in campuses) {
  t <- usr_cleaned %>% filter(camp == campus)
  write.table(
    t,
    file.path(
      DATA_PATH,
      sprintf("orb_usernames/orb_contacts_%s.tsv", camp)
    ),
    quote = FALSE,
    row.names = FALSE,
    sep = "\t"
  )
}


write.table(
  usr_cleaned,
  file.path(DATA_PATH, "usernames_all.csv"),
  quote = FALSE,
  row.names = FALSE,
  sep = ","
)
