suppressWarnings(suppressMessages(source("utils.R")))




shorten_long_response <- function(df, keyword, replacement) {
  df <- df %>%
    mutate(across(where(is.character), ~ gsub(paste0("^", keyword, ".*"), replacement, .)))
  return(df)
}

data <- load_qualtrics_data("deidentified_no_qual.tsv")

# Why do people contribute?
motivations <- data %>% select(
  starts_with("motivations")
)
codenames_values <- list(
  "Job" = "Developing open-source products is part of my job",
  "Improve Tools" = "To improve the tools in my field",
  "Customize" = "To customize existing tools for my specific needs",
  "Network" = "To build a network of peers",
  "Give back" = "To give back to the open source community",
  "Skills" = "To improve my skills",
  "Fun" = "Because it's fun",
  "Other" = "Other (Please specify. Multiple answers should be comma-separated.)"
)
motivations <- recode_dataframe(motivations, codenames)
motivations <- rename_cols_based_on_entries(motivations)
motivations %>% summarise(across(everything(), ~ sum(!is.na(.) & . != "")))




# How important is open source for each importance category?
# Need to do some data wrangling first.
importance <- data %>% select(
  starts_with("importance_opensrc")
)

codenames_columns <- c(
  "Research" = "importance_opensrc_1",
  "Teaching" = "importance_opensrc_2",
  "Learning" = "importance_opensrc_3",
  "Professional Development" = "importance_opensrc_4",
  "Job" = "importance_opensrc_5"
)

importance <- rename_cols_based_on_codenames(importance, codenames_columns)

# Convert "Non-applicable" to NA
importance <- importance %>%
  mutate(across(everything(), ~ ifelse(.x == "Non-applicable", NA, .x)))

recode_values <- c(
  "Not at all important" = 0,
  "Slightly important" = 1,
  "Moderately important" = 2,
  "Important" = 3,
  "Very important" = 4
)
importance <- importance %>%
  mutate(across(everything(), ~ recode_values[.x]))

# Create a summary dataframe (an alternative to summary(importance))
mode_row <- sapply(importance, calculate_mode)
mean_row <- colMeans(importance, na.rm = TRUE)
total_non_na <- colSums(!is.na(importance))
summary_df <- rbind(mode_row, mean_row, total_non_na)
rownames(summary_df) <- c("Mode", "Mean", "Total non-NA responses")
summary_df

hosting <- data %>% select(
  starts_with("hosting_services")
)
# Shorten the longer responses
hosting <- shorten_long_response(hosting, "Other", "Other")
hosting <- shorten_long_response(hosting, "OSF", "OSF")
hosting <- shorten_long_response(hosting, "A custom", "Custom Website")
hosting <- shorten_long_response(hosting, "In the", "Article supplement")

hosting <- rename_cols_based_on_entries(hosting)
hosting %>% summarise(across(everything(), ~ sum(!is.na(.) & . != "")))
hosting$Campus <- data$campus

long_hosting <- hosting %>%
  pivot_longer(cols = -Campus, names_to = "Service", values_to = "Usage") %>%
  filter(Usage != "" & !is.na(Usage)) %>%
  count(Campus, Service, name = "Count")

# Create a new dataframe with columns Campus, Number of Participants, Service

campus_totals <- data.frame(table(data$campus))
names(campus_totals) <- c("Campus", "Participants")


long_hosting <- long_hosting %>%
  left_join(campus_totals, by = "Campus") %>% # Merge with campus_totals
  mutate(Percent = round((Count / Participants) * 100)) %>% # Compute percentage
  select(Campus, Service, Count, Percent) # Keep relevant columns



ggplot(long_hosting, aes(x = Service, y = Percent, shape = Campus, color = Campus)) +
  geom_point(size = 4) + # Adjust dot size
  labs(x = "Hosting Service", y = "Count", title = "Usage of Hosting Services by Campus") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability



size <- data %>% select(starts_with("project_size"))

codenames_rev <- c(
  # Note that elsewhere in this repo, in the codenames variable,
  # the names to be replaced come first.
  "Small" = "project_size_1",
  "Medium" = "project_size_2",
  "Large" = "project_size_3"
)
size <- size %>%
  rename(any_of(codenames_rev))

size <- size %>%
  # Remove rows where any column is NA or empty
  filter(if_all(everything(), ~ !is.na(.) & . != ""))

recode_values <- c(
  "Never" = 0,
  "Relatively infrequently" = 1,
  "Occasionally" = 2,
  "Relatively frequently" = 3
)
size <- size %>%
  mutate(across(everything(), ~ recode_values[.x]))
