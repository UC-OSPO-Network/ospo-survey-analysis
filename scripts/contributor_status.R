# Create a simple table showing how many participants are
# experienced contributors vs. future contributors

suppressWarnings(suppressMessages(source("utils.R")))


data <- load_qualtrics_data("deidentified_no_qual.tsv")


############## Exploring overall participation ##############
# I ended up making a table in MS Word to display these data.
# There's no good way in R to create a proportional Venn diagram.
# As for tables, the mmtable2 package looks pretty but is really annoying to use.

# How many participants are in the dataset?
nrow(data)

# How many participants are not affiliated with UC?
length(data$campus[data$campus == "I'm not affiliated with UC"])

# How many participants are experienced contributors?
# (Answered True to the first question, True or False to the second question)
status <- data %>% select(
  starts_with("contributor_status")
)
names(status) <- c(
  "past",
  "future"
)
status <- status %>%
  count(past, future)

# Drop rows where the 'past' and 'future' columns are both empty
# (These are non-UC respondents)
status <- status %>%
  filter(!(past == "" & future == "" | past == "False" & future == "False"))

only_past <- sum((status %>%
  filter(past == "True" & future == "False"))$n)
only_future <- sum((status %>%
  filter(past == "False" & future == "True"))$n)
both <- sum((status %>%
  filter(past == "True" & future == "True"))$n)

status_final <- data.frame(
  status = c("Only Past", "Only Future", "Past and Future"),
  n = c(only_past, only_future, both)
)
