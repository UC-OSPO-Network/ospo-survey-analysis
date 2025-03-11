# Purpose: create donut charts reflecting overall participation by group

suppressWarnings(suppressMessages(source("utils.R")))

data <- load_qualtrics_data("deidentified_no_qual.tsv")

# Select job column and remove empty strings
jobs <- data$job_category[nzchar(data$job_category)]
jobs <- table(jobs) # count occurrences of each job

long_data <- data.frame(jobs) %>%
  mutate(fraction = Freq / sum(Freq)) %>%
  arrange(desc(fraction))

# Clean up this one long job name
long_data$jobs <- gsub(
  "Other research staff \\(e.g., research scientist, research software engineer\\)",
  "Other research staff",
  long_data$jobs
)

# At this point we are essentially creating a single stacked bar with all our data.
# Then we will put it on a polar coordinate system to make it a donut chart.
# Method from https://r-graph-gallery.com/128-ring-or-donut-plot.html

# Compute the cumulative percentages (top of each rectangle)
long_data$ymax <- cumsum(long_data$fraction)

# Compute the bottom of each rectangle
long_data$ymin <- c(0, head(long_data$ymax, n = -1))

# Compute label position
long_data$labelPosition <- (long_data$ymax + long_data$ymin) / 2


# Create a donut chart

ggplot(long_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = jobs)) +
  geom_rect() +
  # geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 6) +
  scale_fill_manual(values = colors) +
  theme_void() +
  coord_polar(theta = "y") +
  xlim(c(2, 4))

# Create label column
long_data$label <- paste0(long_data$Freq, "\n(", round(long_data$fraction * 100, 1), "%)")

ggplot(long_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = jobs)) +
  geom_rect() +
  geom_text(x = 3.5, aes(y = labelPosition, label = label), size = 6, color = "white") + # Add labels
  scale_fill_manual(values = colors) +
  theme_void() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_blank()
  )


save_plot("donut.tiff", 8, 6)
