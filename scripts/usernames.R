# Count how many people gave at least one username

suppressWarnings(suppressMessages(source("utils.R")))

data <- load_qualtrics_data("survey", "pii.tsv")

# Count how many entries in the vector are not empty
sum(data$usernames != "")
