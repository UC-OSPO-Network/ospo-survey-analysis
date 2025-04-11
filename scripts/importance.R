# This script generates a stacked bar chart of the perceived importance
# of open source software among different campus groups.
# Input: deidentified_no_qual.tsv
# Output: importance.tiff

suppressWarnings(suppressMessages(source("utils.R")))

grouped_bar_chart <- function(df, x_var, fill_var, title, ylabel = NULL) {
  ylabel <- ifelse(is.null(ylabel), "Number of Respondents", ylabel)
  ggplot(df, aes(x = .data[[x_var]], fill = .data[[fill_var]])) +
    geom_bar(position = "dodge") +
    ggtitle(title) +
    labs(y = ylabel) +
    scale_fill_manual(values = colors) + # see utils.R
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(angle = 60, vjust = 0.6, size = 12),
      axis.ticks.x = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
    )
}











data <- load_qualtrics_data("deidentified_no_qual.tsv")

importance_and_job <- data %>% select(
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
  mutate(importance_area = recode(importance_area,
    "importance_opensrc_1" = "Research",
    "importance_opensrc_2" = "Teaching",
    "importance_opensrc_3" = "Learning",
    "importance_opensrc_4" = "Professional Development",
    "importance_opensrc_5" = "Job"
  ))

# STOP! DO MANUALLY DOUBLE-CHECK THAT THE IMPORTANCE AREA LABELS ARE CORRECT!

# Remove all rows that contain either an NA or an empty string
long_data <- long_data %>% filter(if_all(everything(), ~ . != "" & !is.na(.)))

# Shorten this one very long category
long_data$job_category <- gsub("^Other.*", "Research Staff", long_data$job_category)

long_data$importance_level <- factor(long_data$importance_level,
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

research_learning_pd <- long_data %>%
  filter(
    importance_area == "Research" |
      importance_area == "Learning" |
      importance_area == "Professional Development"
  ) %>%
  filter(importance_level != "Non-applicable")

grouped_bar_chart(
  research_learning_pd, "importance_level", "importance_area",
  "Perceived Importance of Open Source among Researchers"
)






job_learning_pd <- long_data %>%
  filter(
    importance_area == "Job" |
      importance_area == "Learning" |
      importance_area == "Professional Development"
  ) %>%
  filter(importance_level != "Non-applicable")

grouped_bar_chart(
  job_learning_pd, "importance_level", "importance_area",
  "Perceived Importance of Open Source among Non-research Staff"
)


teaching <- long_data %>%
  filter(
    importance_area == "Teaching"
  ) %>%
  filter(
    importance_level != "Non-applicable"
  )


ggplot(teaching, aes(
  x = importance_level,
  fill = importance_area
)) +
  geom_bar() +
  ggtitle("Perceived Importance of Open Source for Teaching") +
  labs(y = "Number of Respondents (Teachers Only)") +
  scale_fill_manual(values = colors) + # from https://sronpersonalpages.nl/~pault/
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 60, vjust = 0.6, size = 12),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
  )


# Let's just choose one job category


save_plot("importance.tiff", 10, 5)

faculty <- long_data %>%
  filter(job_category == "Faculty") %>%
  filter(importance_level != "Non-applicable")

grouped_bar_chart(
  faculty, "importance_level", "importance_area",
  "Perceived Importance of Open Source among Faculty",
  "Number of Faculty Respondents"
)
