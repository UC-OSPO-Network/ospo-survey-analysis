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

motivations <- make_df_binary(motivations)
motivations <- exclude_empty_rows(motivations)

sums <- data.frame(
  Motivation = names(motivations),
  Count = unname(apply(motivations, 2, function(x) round(sum(x, na.rm = TRUE))))
)

# Reorder factor levels based on count
sums <- sums %>%
  mutate(Motivation = fct_reorder(Motivation, Count, .desc = TRUE))


basic_bar_chart(sums, "Motivation", "Count", "Reasons for Contributing to Open Source")

basic_bar_chart(sums,
  x_var = "Motivation",
  y_var = "Count",
  title = "Reasons for Contributing to Open Source",
  show_axis_title_x = TRUE,
  show_axis_title_y = TRUE,
  axis_text_size_x = 12,
  axis_text_size_y = 12,
  color_index = 3,
  horizontal = TRUE,
  show_ticks_y = FALSE,
  axis_text_angle_x = 0
)
