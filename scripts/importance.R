# This script generates a stacked bar chart of the perceived importance
# of open source software among different campus groups.
# Input: deidentified_no_qual.tsv
# Output: importance.tiff

source("utils.R")

data <- load_qualtrics_data("deidentified_no_qual.tsv")

importance_and_job <- data %>% select(starts_with("importance_opensrc") | starts_with("job_category"))

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
  )) %>%
  mutate(importance_level = recode(importance_level,
    "Not at all important" = 0,
    "Non-applicable" = 0,
    "Slightly important" = 1,
    "Moderately important" = 2,
    "Important" = 3,
    "Very important" = 4
  ))



# STOP! DO MANUALLY DOUBLE-CHECK THAT THE IMPORTANCE AREA LABELS ARE CORRECT!

# Remove all rows that contain either an NA or an empty string
long_data <- long_data %>% filter(if_all(everything(), ~ . != "" & !is.na(.)))

# Shorten this one very long category
long_data$job_category <- gsub("^Other.*", "Other", long_data$job_category)



ggplot(long_data, aes(x = job_category, y = importance_level, fill = importance_area)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Perceived Importance of Open Source among Campus Groups") +
  labs(fill = "Areas of Importance") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "#332288",
    "#88CCEE",
    "#44AA99",
    "#DDCC77",
    "#AA4499"
  )) + # from https://sronpersonalpages.nl/~pault/
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.6, size = 12),
    axis.ticks.x = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
  )


# Path is in my ~/.Renviron file
figure_path <- Sys.getenv("FIGURE_PATH")
ggsave(
  filename = "importance.tiff",
  path = figure_path,
  width = 10,
  height = 5,
  device = "tiff",
  dpi = 700
)
