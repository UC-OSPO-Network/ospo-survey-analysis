#!/usr/bin/env Rscript

# A script to create donut charts reflecting response rates by various groups.
# If this script is run without arguments, it will create donut charts for all participants.
# If run with the command-line flag "-c", it will create donut charts for OS contributors only.

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
  long_data$label <- paste0(long_data$Freq)
  # long_data$label <- paste0(long_data$Freq, "\n(", round(long_data$fraction * 100, 1), "%)")

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






data <- load_qualtrics_data("deidentified_no_qual.tsv")



############## Argument handling ##############

args <- commandArgs(trailingOnly = TRUE) # only the user-supplied part

contributor_mode <- FALSE # default
if (length(args) > 0) {
  # treat the *first* argument as the flag
  flag <- tolower(args[1])
  if (flag %in% c("--contributors", "-c", "contributors")) {
    contributor_mode <- TRUE
  } else {
    stop(
      "Unknown argument: ", flag,
      "\nUsage: Rscript myscript.R [--contributors | -c | contributors]"
    )
  }
}

if (contributor_mode) {
  # Filter the data to include only OS contributors
  data <- data %>%
    filter(
      contributor_status_1 == "True"
    )
}

population <- ""
# For plot axis labels
if (contributor_mode) {
  population <- "Contributors"
} else {
  population <- "Respondents"
}





############## Donut charts of participation by groups ##############

# Donut chart 1
job_data <- create_df_for_plotting(data, "job_category")
# Clean up this one long job name
job_data$values <- gsub(
  "^Other.*",
  "Other research staff",
  job_data$values
)
job_data <- reorder_factor_by_column(job_data, values, Freq, descending = TRUE)

p1 <- donut_chart(job_data) +
  labs(title = sprintf("Job Category of %s", population))


# Donut chart 2
campus_data <- create_df_for_plotting(data, "campus")
campus_data <- reorder_factor_by_column(campus_data, values, Freq, descending = TRUE)
campus_data <- campus_data %>% filter(values != "I'm not affiliated with UC")

p2 <- donut_chart(campus_data) +
  labs(title = sprintf("Campus of %s", population))


# Donut chart 3
field_data <- create_df_for_plotting(data, "field_of_study")
field_data <- reorder_factor_by_column(field_data, values, Freq, descending = TRUE)
p3 <- donut_chart(field_data) +
  labs(title = sprintf("Academic %s' fields of study", tolower(population)))

# Donut chart 4
# I'm not using my function for this group because I want to combine the jobs that
# have only 1 or 2 responses into the existing "Other" category.

staff_data <- data[["staff_categories"]][nzchar(data[["staff_categories"]])]
# Count occurrences of each unique value
staff_data <- as.data.frame(table(staff_data))
names(staff_data) <- c("job", "count")
staff_data$job <- as.character(staff_data$job)
codenames <- c(
  "Academic and Research Support" = "Academic and Research Support",
  "Other" = "Other",
  "Finance" = "Finance"
)
staff_data <- shorten_long_responses(staff_data, codenames)

staff_data_clean <- as.data.frame(staff_data) %>% # your original two-column data frame
  mutate(job = if_else(count < 3, "Other", job)) %>% # relabel rare jobs as "Other"
  group_by(job) %>% # gather all “Other” rows together
  summarise(Freq = sum(count), .groups = "drop")

staff_data_clean <- reorder_factor_by_column(staff_data_clean, job, Freq, descending = TRUE)

staff_long_data <- as.data.frame(staff_data_clean) %>%
  mutate(fraction = Freq / sum(Freq)) %>%
  arrange(desc(fraction))

# Compute the cumulative percentages (top of each rectangle)
staff_long_data$ymax <- cumsum(staff_long_data$fraction)
staff_long_data$ymin <- c(0, head(staff_long_data$ymax, n = -1))

# Compute label position
staff_long_data$labelPosition <- (staff_long_data$ymax + staff_long_data$ymin) / 2

# Create label column
staff_long_data$label <- paste0(staff_long_data$Freq)

# Rename this one column to match the donut_chart function
names(staff_long_data)[names(staff_long_data) == "job"] <- "values"

p4 <- donut_chart(staff_long_data) +
  labs(title = sprintf("Staff %s' work areas", tolower(population)))


combined_donuts <- wrap_plots(p1, p2, p3, p4, ncol = 2)


save_plot(sprintf("combined_donuts_%s.tiff", tolower(population)), 18, 12)
