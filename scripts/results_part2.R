suppressWarnings(suppressMessages(source("utils.R")))


data <- load_qualtrics_data("survey", "deidentified_no_qual.tsv")

# Why do people contribute?
# This is just a summary. See motivations.R for a plot.
motivations <- data %>% select(
  starts_with("motivations")
)
codenames <- c(
  "Developing open-source" = "Job",
  "To improve the tools" = "Improve Tools",
  "To customize existing" = "Customize",
  "To build a network" = "Network",
  "To give back to" = "Give back",
  "To improve my skills" = "Skills",
  "Because it's fun" = "Fun",
  "Other " = "Other"
)
motivations <- shorten_long_responses(motivations, codenames)
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







hosting <- data %>% select(
  starts_with("hosting_services")
)

codenames <- c(
  "Other" = "Other",
  "OSF" = "OSF",
  "A custom" = "Custom Website",
  "In the" = "Article supplement"
)
hosting <- shorten_long_responses(hosting, codenames) # Note empty columns will be NAs
hosting <- hosting %>% select(where(~ !all(is.na(.x)))) # Remove empty columns
hosting <- rename_cols_based_on_entries(hosting)
hosting <- make_df_binary(hosting)
custom_summary(hosting)


hosting$Campus <- data$campus


campus_totals <- data.frame(table(data$campus))
names(campus_totals) <- c("Campus", "Participants")

hosting_at_least_one_response <- hosting[rowSums(hosting[, -ncol(hosting)]) > 0, ]
temp <- data.frame(table(hosting_at_least_one_response$Campus))
names(temp) <- c("Campus", "Responsive_Participants")

merged_campus_totals <- merge(campus_totals, temp, by = "Campus", all = TRUE)
merged_campus_totals[is.na(merged_campus_totals)] <- 0


hosting_long <- hosting %>%
  pivot_longer(cols = -Campus, names_to = "Service", values_to = "Count") %>%
  filter(Count > 0)

aggregated_data <- hosting_long %>%
  group_by(Campus, Service) %>%
  summarize(Count = sum(Count), .groups = "drop")

final_data <- aggregated_data %>%
  left_join(merged_campus_totals, by = "Campus") %>%
  mutate(Percent_of_Responsive_Participants = (Count / Responsive_Participants) * 100) %>%
  select(Campus, Service, Count, Percent_of_Responsive_Participants)

# Fill in missing Campus + Service combinations with a Count of 0
all_combinations <- expand_grid(
  Campus = unique(final_data$Campus),
  Service = colnames(hosting)[-ncol(hosting)] # Exclude the last column, Campus
)
final_data <- all_combinations %>%
  left_join(final_data, by = c("Campus", "Service")) %>%
  replace_na(list(Count = 0, Percent_of_Responsive_Participants = 0)) # Replace NAs with 0


ggplot(final_data, aes(x = Service, y = Percent_of_Responsive_Participants, shape = Campus, color = Campus)) +
  geom_point(size = 4) + # Adjust dot size
  labs(x = "Hosting Service", y = "Percent of Responsive Participants", title = "Usage of Hosting Services by Campus") +
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

size_long <- size %>%
  pivot_longer(cols = everything(), names_to = "Size", values_to = "Frequency") %>%
  count(Size, Frequency, name = "Count")

size_long$Frequency <- factor(
  size_long$Frequency,
  levels = c(
    "Never",
    "Relatively infrequently",
    "Occasionally",
    "Relatively frequently"
  )
)

ggplot(size_long, aes(x = Frequency, y = Count, shape = Size, color = Size)) +
  geom_point(size = 4) + # Adjust dot size
  geom_line(aes(group = Size), size = 1) + # Connect points with lines
  scale_color_manual(values = colors) +
  labs(
    x = "Frequency of contribution",
    y = "Number of Participants",
    title = "Contributions to Projects of a Certain Size"
  ) +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10),
    panel.background = element_blank(),
    panel.grid.major = element_line(linetype = "solid", color = "gray90"),
    panel.grid.minor = element_line(linetype = "solid", color = "gray90"),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
  )
