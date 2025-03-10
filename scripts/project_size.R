# Purpose: Create a line plot showing the frequency of contributions by project size
# Input: deidentified_no_qual.tsv
# Output: project_size.tiff

source("utils.R")

data <- load_qualtrics_data("deidentified_no_qual.tsv")

roles_and_size <- data %>% select(starts_with("project_size") | starts_with("contributor_role"))

# Make entries more readable
roles_and_size <- data.frame(strip_descriptions(roles_and_size))

# Remove the "Other (Please specify)" column, which is hard to interpret
roles_and_size <- roles_and_size %>% select(where(~ !any(. == "Other (Please specify)", na.rm = TRUE)))

# Reshape data to long format
long_data <- roles_and_size %>%
  pivot_longer(
    cols = starts_with("project_size"),
    names_to = "project_size",
    values_to = "frequency"
  ) %>%
  pivot_longer(
    cols = starts_with("contributor_role"),
    names_to = "role",
    values_to = "role_name"
  ) %>%
  filter(!is.na(role_name), !is.na(frequency)) %>%
  group_by(role_name, project_size) %>%
  summarise(frequent_count = sum(frequency == "Frequently", na.rm = TRUE), .groups = "drop")

long_data <- long_data %>%
  mutate(project_size = recode(project_size,
    "project_size_1" = "Very Small",
    "project_size_2" = "Small",
    "project_size_3" = "Medium",
    "project_size_4" = "Large",
    "project_size_5" = "Very Large"
  ))

# STOP! DO MANUALLY DOUBLE-CHECK THAT THE PROJECT SIZE LABELS ARE CORRECT!



long_data$project_size <- factor(long_data$project_size)
levels(long_data$project_size) <- c("Very Small", "Small", "Medium", "Large", "Very Large")
long_data$role_name <- factor(long_data$role_name)

# modified from https://sronpersonalpages.nl/~pault/
colors <- c(
  "#332288",
  "#88CCEE",
  "#44AA99",
  "#117733",
  "#999933",
  "#DDCC77",
  "#CC6677",
  "#882255",
  "#AA4499",
  "#353535"
)
labeled_colors <- setNames(as.list(colors), levels(long_data$role_name))


lineplot <- function(df, current_role) {
  x <- ggplot(
    subset(df, role_name == current_role),
    aes(x = project_size, y = frequent_count, group = role_name, color = role_name)
  ) +
    geom_line() +
    geom_point() +
    ylim(0, 6) +
    scale_x_discrete(expand = c(0.025, 0.025)) +
    ylab(current_role) +
    xlab("Project Size") +
    ggtitle("Frequent Contributions by Project Size") +
    scale_color_manual(values = c(labeled_colors[[current_role]])) +
    # Use different theme options depending on whether this is
    # the first plot, a middle plot, or the last plot in the stack
    # I know this code is painfully "wet" as opposed to "d.r.y" but it gets the job done
    {
      if (current_role == levels(long_data$role_name)[[1]]) {
        theme(
          axis.title.y = element_text(
            angle = 0,
            vjust = 0.5,
            color = labeled_colors[[current_role]],
            size = 12,
            face = "bold"
          ),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", color = "gray90"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", color = "gray90"),
          plot.margin = unit(c(0.3, 0.5, 0, 0), "cm"),
          plot.title = element_text(hjust = 0.5, size = 16),
          legend.position = "none"
        )
      }
    } +
    {
      if (
        current_role != levels(long_data$role_name)[[length(levels(long_data$role_name))]] &
          current_role != levels(long_data$role_name)[[1]]) {
        theme(
          axis.title.y = element_text(
            angle = 0,
            vjust = 0.5,
            color = labeled_colors[[current_role]],
            size = 12,
            face = "bold"
          ),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", color = "gray90"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", color = "gray90"),
          plot.margin = unit(c(0.3, 0.5, 0, 0), "cm"),
          plot.title = element_blank(),
          legend.position = "none"
        )
      }
    } +
    {
      if (current_role == levels(long_data$role_name)[[length(levels(long_data$role_name))]]) {
        theme(
          axis.title.y = element_text(
            angle = 0,
            vjust = 0.5,
            color = labeled_colors[[current_role]],
            size = 12,
            face = "bold"
          ),
          axis.title.x = element_text(size = 14, vjust = -0.5),
          axis.text.x = element_text(size = 12),
          panel.background = element_blank(),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", color = "gray90"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", color = "gray90"),
          plot.margin = unit(c(0.3, 0.5, 0.3, 0), "cm"),
          plot.title = element_blank(),
          legend.position = "none"
        )
      }
    }
}

plotlist <- lapply(
  levels(long_data$role_name),
  function(x) lineplot(long_data, x)
)

wrap_plots(plotlist, nrow = 10, ncol = 1)

figure_path <- Sys.getenv("FIGURE_PATH")
ggsave(
  filename = "project_size.tiff",
  path = figure_path,
  width = 8,
  height = 10,
  device = "tiff",
  dpi = 700
)
