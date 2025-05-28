# This script generates a stacked bar chart of the challenges
# of open source software reported by different job categories.
# Input: deidentified_no_qual.tsv
# Output: importance.tiff

suppressWarnings(suppressMessages(source("utils.R")))

data <- load_qualtrics_data("deidentified_no_qual.tsv")

challenges_and_job <- data %>%
  select(
    starts_with("challenges") | starts_with("job_category")
  )

long_data <- challenges_and_job %>%
  pivot_longer(
    cols = starts_with("challenges"),
    names_to = "challenge",
    values_to = "challenge_level"
  )

# Remove all rows that contain either an NA or an empty string
long_data <- long_data %>% filter(if_all(everything(), ~ . != "" & !is.na(.)))

# Shorten this one very long category
long_data$job_category <- gsub("^Other.*", "Other", long_data$job_category)


long_data <- long_data %>%
  mutate(
    challenge = recode(
      challenge,
      "challenges_1" = "Coding time",
      "challenges_2" = "Managing issues",
      "challenges_3" = "Attracting users",
      "challenges_4" = "Recognition",
      "challenges_5" = "Hiring",
      "challenges_6" = "Security",
      "challenges_7" = "Finding groups",
      "challenges_8" = "Finding mentors",
      "challenges_9" = "Education time",
      "challenges_10" = "Education resources",
      "challenges_11" = "Legal",
      "challenges_12" = "Finding funding",
      "challenges_13" = "Securing funding"
    )
  ) %>%
  mutate(
    challenge_level = recode(
      challenge_level,
      "Never" = 0,
      "Non-applicable" = 0,
      "Rarely" = 1,
      "Occasionally" = 2,
      "Frequently" = 3,
      "Always" = 4
    )
  )

# STOP! DO MANUALLY DOUBLE-CHECK THAT THE CHALLENGE LABELS ARE CORRECT!

# Arrange the challenges in descending order of total challenge level
long_data <- long_data %>%
  mutate(
    challenge = factor(
      challenge,
      levels = long_data %>%
        group_by(challenge) %>%
        summarize(total_level = sum(challenge_level)) %>%
        # ^Calculate total challenge level for each challenge
        arrange(desc(total_level)) %>%
        pull(challenge) # Extract ordered challenge names
    )
  )


ggplot(
  long_data,
  aes(
    x = challenge,
    y = challenge_level,
    fill = job_category
  )
) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Reported Challenges in Open Source, by Career") +
  labs(fill = "Career", y = "Frequency of Challenge (Coded)") +
  scale_fill_manual(values = colors) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 70, vjust = 0.9, hjust = 0.8, size = 12),
    axis.text.y = element_text(size = 12),
    axis.ticks.x = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
  )

save_plot("challenges.tiff", 8, 6)
