suppressWarnings(suppressMessages(source("utils.R")))


data <- load_qualtrics_data("deidentified_no_qual.tsv")

# Why do people contribute?
motivations <- data %>% select(
  starts_with("motivations")
)
motivations <- shorten_long_response(motivations, "Developing open-source", "Job")
motivations <- shorten_long_response(motivations, "To improve the tools", "Improve Tools")
motivations <- shorten_long_response(motivations, "To customize existing", "Customize")
motivations <- shorten_long_response(motivations, "To build a network", "Network")
motivations <- shorten_long_response(motivations, "To give back to", "Give back")
motivations <- shorten_long_response(motivations, "To improve my skills", "Skills")
motivations <- shorten_long_response(motivations, "Because it's fun", "Fun")
motivations <- shorten_long_response(motivations, "Other ", "Other")

motivations <- rename_cols_based_on_entries(motivations)
motivations <- make_df_binary(motivations)
custom_summary(motivations)
custom_summary(exclude_empty_rows(motivations))




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


recode_values <- c(
  "Non-applicable" = NA,
  "Not at all important" = 0,
  "Slightly important" = 1,
  "Moderately important" = 2,
  "Important" = 3,
  "Very important" = 4
)
importance <- recode_dataframe_likert(importance, recode_values)
custom_summary(importance)
# TODO: Edit this function to handle NAs



hosting <- data %>% select(
  starts_with("hosting_services")
)
# Shorten the longer responses
hosting <- shorten_long_response(hosting, "Other", "Other")
hosting <- shorten_long_response(hosting, "OSF", "OSF")
hosting <- shorten_long_response(hosting, "A custom", "Custom Website")
hosting <- shorten_long_response(hosting, "In the", "Article supplement")

hosting <- rename_cols_based_on_entries(hosting)
hosting <- make_df_binary(hosting)
custom_summary(hosting)


hosting$Campus <- data$campus

long_hosting <- hosting %>%
  pivot_longer(cols = -Campus, names_to = "Service", values_to = "Usage") %>%
  filter(Usage != "" & !is.na(Usage)) %>%
  count(Campus, Service, name = "Count")

# Create a new dataframe with columns Campus, Number of Participants, Service
# THIS PART IS BROKEN
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
