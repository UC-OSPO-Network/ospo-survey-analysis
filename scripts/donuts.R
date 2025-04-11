# Purpose: create donut charts reflecting response rates by group

suppressWarnings(suppressMessages(source("utils.R")))


create_df_for_plotting <- function(data, column) {
  if (!column %in% names(data)) {
    stop("Column not found in data frame")
  }
  # Extract specified column and remove empty strings
  values <- data[[column]][nzchar(data[[column]])]
  # Count occurrences of each unique value
  values_table <- table(values)

  # At this point we are essentially creating a single stacked bar with all our data.
  # Then we will put it on a polar coordinate system to make it a donut chart.
  # Method from https://r-graph-gallery.com/128-ring-or-donut-plot.html

  # Convert to data frame and compute fractions
  long_data <- as.data.frame(values_table) %>%
    mutate(fraction = Freq / sum(Freq)) %>%
    arrange(desc(fraction))

  # Compute the cumulative percentages (top of each rectangle)
  long_data$ymax <- cumsum(long_data$fraction)
  long_data$ymin <- c(0, head(long_data$ymax, n = -1))

  # Compute label position
  long_data$labelPosition <- (long_data$ymax + long_data$ymin) / 2

  # Create label column
  long_data$label <- paste0(long_data$Freq, "\n(", round(long_data$fraction * 100, 1), "%)")

  return(long_data)
}

donut_chart <- function(df) {
  ggplot(df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = values)) +
    geom_rect() +
    geom_text(x = 3.5, aes(y = labelPosition, label = label), size = 6, color = "white") + # Add labels
    scale_fill_manual(values = colors) +
    theme_void() +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 20),
      plot.background = element_rect(fill = "white", color = "white")
    )
}







data <- load_qualtrics_data("survey", "deidentified_no_qual.tsv")

job_data <- create_df_for_plotting(data, "job_category")

# Clean up this one long job name
job_data$values <- gsub(
  "^Other.*",
  "Other research staff",
  job_data$values
)

campus_data <- create_df_for_plotting(data, "campus")

p1 <- donut_chart(job_data) +
  labs(title = "Job Category of Respondents")

p2 <- donut_chart(campus_data) +
  labs(title = "Campus of Respondents")

# Combine the two plots side by side
p1 + p2


save_plot("donut.tiff", 16, 12)
