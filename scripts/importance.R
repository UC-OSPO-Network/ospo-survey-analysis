# A script to plot some bar charts showing the perceived importance
# of open source for different job categories and different tasks.

suppressWarnings(suppressMessages(source("utils.R")))

data <- load_qualtrics_data("deidentified_no_qual.tsv")

importance_and_job <- data %>%
  select(
    starts_with("importance_opensrc") | starts_with("job_category")
  )

# Reshape data to long format
long_data <- importance_and_job %>%
  pivot_longer(
    cols = starts_with("importance_opensrc"),
    names_to = "importance_area",
    values_to = "importance_level"
  )

long_data <- long_data %>%
  mutate(
    importance_area = recode(
      importance_area,
      "importance_opensrc_1" = "Research",
      "importance_opensrc_2" = "Teaching",
      "importance_opensrc_3" = "Learning",
      "importance_opensrc_4" = "Professional Development",
      "importance_opensrc_5" = "Job"
    )
  )

# # STOP! DO MANUALLY DOUBLE-CHECK THAT THE IMPORTANCE AREA LABELS ARE CORRECT!
# # Use emails to identify responses, and compare these to the display in Qualtrics.
# pii <- load_qualtrics_data("pii.tsv")
# emails <- pii %>%
#   select(starts_with("stay_in_touch_email"))

# t <- cbind(emails, importance_and_job)
# subset(t, startsWith(stay_in_touch_email, "PERSONNAMEHERE"))

# Remove all rows that contain an empty string in any column
# (This cuts out 190 data points)
long_data <- long_data %>%
  filter(!if_any(everything(), ~ . == ""))

# Shorten this one very long category
long_data$job_category <- gsub(
  "^Other.*",
  "Research Staff",
  long_data$job_category
)

long_data$importance_level <- factor(
  long_data$importance_level,
  levels = c(
    "Very important",
    "Important",
    "Moderately important",
    "Slightly important",
    "Not at all important",
    "Non-applicable"
  ),
  ordered = TRUE
)


############## (Grouped) bar plots for teachers, researchers, and non-research staff ##############

teaching <- long_data %>%
  filter(
    importance_area == "Teaching"
  ) %>%
  filter(
    importance_level != "Non-applicable"
  )
# I assume that if they answered "Teaching", they must be a teacher.

teaching <- teaching %>% select(-c(job_category, importance_area))

teaching <- teaching %>%
  count(importance_level, name = "Counts")

basic_bar_chart(
  teaching,
  x_var = "importance_level",
  y_var = "Counts",
  title = "Perceived Importance of Open Source for Teaching",
  ylabel = "Number of Respondents (Teachers Only)",
  show_bar_labels = TRUE
)

save_plot("importance_teachers.tiff", 8, 5)


research_learning_pd <- long_data %>%
  filter(
    importance_area == "Research" |
      # If they gave an answer for "research", I assume they must be a researcher.
      importance_area == "Learning" |
      importance_area == "Professional Development"
  ) %>%
  filter(importance_level != "Non-applicable")

grouped_bar_chart(
  research_learning_pd,
  "importance_level",
  "importance_area",
  "Perceived Importance of Open Source among Researchers"
)

save_plot("importance_researchers.tiff", 10, 5)


job_learning_pd <- long_data %>%
  filter(
    importance_area == "Job" |
      importance_area == "Learning" |
      importance_area == "Professional Development"
  ) %>%
  filter(job_category == "Non-research Staff") %>%
  filter(importance_level != "Non-applicable")

grouped_bar_chart(
  job_learning_pd,
  "importance_level",
  "importance_area",
  "Perceived Importance of Open Source among Non-research Staff"
)

save_plot("importance_nrstaff.tiff", 10, 5)


############## Stacked bar chart for just job, learning, and professional development ##############

recode_values <- c(
  "Non-applicable" = NA,
  "Not at all important" = 0,
  "Slightly important" = 1,
  "Moderately important" = 2,
  "Important" = 3,
  "Very important" = 4
)
long_data_quant <- recode_dataframe_likert(
  long_data,
  recode_values,
  "importance_level"
)


long_data_summed <- long_data_quant %>%
  count(job_category, importance_area, importance_level, name = "Counts")

# Remove rows that contain an NA
long_data_summed <- na.omit(long_data_summed)

weighted_counts <- long_data_summed %>%
  group_by(job_category, importance_area) %>%
  summarise(WeightedCounts = sum(importance_level * Counts), .groups = "drop")

weighted_counts_simple <- weighted_counts %>%
  filter(importance_area != "Teaching") %>%
  filter(importance_area != "Research")

weighted_counts_simple$job_category <- factor(
  weighted_counts_simple$job_category,
  levels = c(
    "Faculty",
    "Post-Doc",
    "Grad Student",
    "Undergraduate",
    "Research Staff",
    "Non-research Staff"
  ),
  ordered = TRUE
)


stacked_bar_chart(
  weighted_counts_simple,
  x_var = "job_category",
  y_var = "WeightedCounts",
  fill = "importance_area",
  title = "Perceived Importance of Open Source Software by Job Category",
  ylabel = "Relative Importance",
  proportional = TRUE
)
save_plot("importance_job_category.tiff", 10, 5)
