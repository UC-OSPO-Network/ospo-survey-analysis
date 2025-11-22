# A script to clean up the qualitative discipline classification results
# for deposition in Dryad. Reshuffle row order and participant IDs.
# We can't remove participant IDs because we need them to see where the
# same person gave two answers, but we can make their value arbitrary.

# Load packages

project_root <- here::here() # requires that you be somewhere in the
# project directory (not above it)
suppressMessages(source(file.path(project_root, "scripts/packages.R")))
# functions and objects used across scripts
suppressMessages(source(file.path(project_root, "scripts/utils.R")))


set.seed(100)


# Load raw data
# columns: "participantID" "response"      "Level1"        "Level2"        "Level3"
data <- load_qualtrics_data(
  "curation_of_disciplines/qual_disciplines_final.tsv"
)

# participants can write multiple disciplines, so data$participantID
# may look like: 1, 2, 2, 3, 4, 5, 5, 6, 6, 7...170

# Next, create a "secret key": values from 1 to max
# (here max = 174, bc we had 174 academic participants)
# in a random order
cipher_key <- sample(1:max(data$participantID))
# length(cipher_key) = 174

# Use the original column as the index to pull values from cipher_key
NEWparticipantID <- cipher_key[data$participantID]
cleandata <- cbind(NEWparticipantID, data)


# Okay, now we have new participant IDs, but the rows are still in the
# order that people took the survey. Let's reshuffle the rows while keeping
# multiple answers from the same participant contiguous.

# Get all unique IDs in a random order
tmp <- sample(unique(cleandata$NEWparticipantID))
# length(tmp) = 170

data_shuffled <- cleandata %>%
  arrange(match(NEWparticipantID, tmp))


data_shuffled <- data_shuffled %>%
  select(-"participantID")


write.table(
  data_shuffled,
  file = file.path(
    DATA_PATH,
    "curation_of_disciplines/qual_disciplines_shuffled.tsv"
  ),
  quote = FALSE,
  row.names = FALSE,
  sep = "\t"
)
