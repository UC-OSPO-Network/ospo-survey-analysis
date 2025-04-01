suppressWarnings(suppressMessages(source("utils.R")))

data <- load_qualtrics_data("deidentified_no_qual.tsv")

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

motivations_count <- motivations %>%
  pivot_longer(cols = everything(), names_to = "motivation", values_to = "response") %>%
  drop_na() %>%
  count(motivation, name = "count")


# Expects a two-column df with columns <some_variable> and "count", in that order.
reorder_factor_levels_by_count <- function(df, count_column_name = "count") {
  df <- df %>%
    arrange(desc(.data[[count_column_name]])) # Sort rows by count
  factor_column_name <- names(df)[1]
  df[[factor_column_name]] <- factor(df[[factor_column_name]], levels = df[[factor_column_name]])
  return(df) # Return the modified dataframe
}

motivations_count <- reorder_factor_levels_by_count(motivations_count)

ggplot(motivations_count, aes(
  x = motivation,
  y = count,
)) +
  geom_bar(stat = "identity", fill = colors[[1]]) +
  ggtitle("Reasons for Contributing to Open Source") +
  labs(y = "Number of Respondents (Teachers Only)") +
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
