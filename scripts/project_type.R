# Purpose: Create a bar plot of the number of participants who have contributed to each project type.
# Input: deidentified_no_qual.tsv
# Output: project_type.tiff

source("utils.R")

data <- load_qualtrics_data("deidentified_no_qual.tsv")

projs <- data %>% select(starts_with("project_types"))

# Make entries more readable
projs <- data.frame(strip_descriptions(projs))

# Make column names more readable
names(projs) <- rename_cols_based_on_entries(projs)

# How many participants selected each project type?
proj_counts <- projs %>%
  pivot_longer(
    cols = names(projs),
    values_to = "proj_type"
  ) %>%
  # convert to a 2-col tibble: column name and proj_type
  filter(proj_type != "") %>% # remove empty entries
  count(proj_type, name = "count")
# count occurrences of each proj from the 'projs' column,
# producing a new 2-col tibble: proj_type and count


proj_counts <- proj_counts %>%
  mutate(proj_type = recode(proj_type,
    "Libraries, packages, or frameworks" = "Libraries",
    "Other (Please specify)" = "Other",
    "Plug-ins or extensions" = "Extensions",
    "Website code" = "Web code"
  ))


# Sort rows by count
proj_counts <- proj_counts %>%
  arrange(count)

# Reorder factor levels so that plot bars are ordered
proj_counts$proj_type <- factor(
  proj_counts$proj_type,
  levels = proj_counts$proj_type
)


ggplot(proj_counts, aes(x = proj_type, y = count)) +
  geom_bar(stat = "identity", fill = "#88CCEE") +
  coord_flip() +
  ggtitle("Number of Participants Who Have Contributed to each Project Type") +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 10, 2),
    limits = c(0, 10)
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    panel.background = element_blank(),
    panel.grid.major = element_line(linetype = "solid", color = "gray90"),
    panel.grid.minor = element_line(linetype = "solid", color = "gray90"),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
  )

figure_path <- Sys.getenv("FIGURE_PATH")
ggsave(
  filename = "project_type.tiff",
  path = figure_path,
  width = 10,
  height = 7,
  device = "tiff",
  dpi = 700
)
