suppressWarnings(suppressMessages(source("utils.R")))


prepare_df_for_plotting <- function(df) {
  df <- shorten_long_responses(df, codenames)
  # Remove any columns that are all NA or empty strings
  df <- df[, colSums(is.na(df) | df == "") < nrow(df)]
  df <- rename_cols_based_on_entries(df)

  df <- make_df_binary(df)
  df <- exclude_empty_rows(df)

  sums <- data.frame(
    Motivation = names(df),
    Count = unname(apply(df, 2, function(x) round(sum(x, na.rm = TRUE))))
  )

  return(sums)
}


basic_bar_chart_fixed_axis <- function(
    df,
    x_var,
    y_var,
    title,
    ylabel = "Number of Respondents",
    show_axis_title_x = TRUE,
    show_axis_title_y = TRUE,
    axis_title_size_x = 10,
    axis_title_size_y = 10,
    axis_text_size_x = 8,
    axis_text_size_y = 8,
    axis_text_angle_x = 60,
    title_size = 10,
    color_index = 3,
    horizontal = TRUE,
    show_ticks_x = FALSE,
    show_ticks_y = FALSE,
    show_bar_labels = TRUE) {
  # Axis title settings
  axis_title_x <- if (show_axis_title_x) element_text(size = axis_title_size_x) else element_blank()
  axis_title_y <- if (show_axis_title_y) element_text(size = axis_title_size_y) else element_blank()

  # Axis tick settings
  axis_ticks_x <- if (show_ticks_x) element_line() else element_blank()
  axis_ticks_y <- if (show_ticks_y) element_line() else element_blank()

  # Build the plot
  p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_bar(stat = "identity", fill = colors[[color_index]]) +
    scale_y_continuous(limits = c(0, 75)) + # FIX Y-AXIS MAXIMUM
    ggtitle(title) +
    labs(y = ifelse(is.null(ylabel), "Number of Respondents", ylabel)) +
    theme(
      axis.title.x = axis_title_x,
      axis.title.y = axis_title_y,
      axis.text.x = element_text(angle = axis_text_angle_x, vjust = 0.6, size = axis_text_size_x),
      axis.text.y = element_text(size = axis_text_size_y),
      axis.ticks.x = axis_ticks_x,
      axis.ticks.y = axis_ticks_y,
      legend.position = "none",
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5, size = title_size),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
    )

  # Add text labels inside bars (optional)
  if (show_bar_labels) {
    p <- p + geom_text(
      aes(label = .data[[y_var]]),
      color = "white",
      size = 5,
      vjust = if (horizontal) 0.5 else 1.2, # tweak based on orientation
      hjust = if (horizontal) 1.2 else 0.5
    )
  }

  # Flip coordinates if horizontal
  if (horizontal) {
    p <- p + coord_flip()
  }

  return(p)
}





data <- load_qualtrics_data("survey", "deidentified_no_qual.tsv")

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

# All contributors

motivations <- prepare_df_for_plotting(motivations)

# Reorder factor levels based on count
motivations <- motivations %>%
  mutate(Motivation = fct_reorder(Motivation, Count, .desc = FALSE))

overall_plot <- basic_bar_chart_fixed_axis(motivations,
  x_var = "Motivation",
  y_var = "Count",
  title = "Reasons for Contributing to Open Source: All Contributors"
)

save_plot("motivations_overall.tiff", 6, 4)


# Faculty

faculty <- data %>%
  filter(job_category == "Faculty") %>%
  select(
    starts_with("motivations")
  )

faculty <- prepare_df_for_plotting(faculty)

# Reorder factor levels based on order in overall motivations dataframe
faculty$Motivation <- factor(
  faculty$Motivation,
  levels = levels(motivations$Motivation)
)

faculty_plot <- basic_bar_chart_fixed_axis(faculty,
  x_var = "Motivation",
  y_var = "Count",
  title = "Faculty"
)

save_plot("motivations_faculty.tiff", 6, 4)




# Non-research staff

nrstaff <- data %>%
  filter(job_category == "Non-research Staff") %>%
  select(
    starts_with("motivations")
  )

nrstaff <- prepare_df_for_plotting(nrstaff)

# Reorder factor levels based on order in overall motivations dataframe
nrstaff$Motivation <- factor(
  nrstaff$Motivation,
  levels = levels(motivations$Motivation)
)

nrstaff_plot <- basic_bar_chart_fixed_axis(nrstaff,
  x_var = "Motivation",
  y_var = "Count",
  title = "Non-research Staff"
)


# Grad students

grads <- data %>%
  filter(job_category == "Grad Student") %>%
  select(
    starts_with("motivations")
  )
grads <- prepare_df_for_plotting(grads)
# Reorder factor levels based on order in overall motivations dataframe
grads$Motivation <- factor(
  grads$Motivation,
  levels = levels(motivations$Motivation)
)
grads_plot <- basic_bar_chart_fixed_axis(grads,
  x_var = "Motivation",
  y_var = "Count",
  title = "Grad Students"
)

# Other researchers
# (Postdocs and other research staff)

other_researchers <- data %>%
  filter(job_category == "Other research staff (e.g., research scientist, research software engineer)" |
    job_category == "Post-Doc") %>%
  select(
    starts_with("motivations")
  )
other_researchers <- prepare_df_for_plotting(other_researchers)
# Reorder factor levels based on order in overall motivations dataframe
other_researchers$Motivation <- factor(
  other_researchers$Motivation,
  levels = levels(motivations$Motivation)
)
other_researchers_plot <- basic_bar_chart_fixed_axis(other_researchers,
  x_var = "Motivation",
  y_var = "Count",
  title = "Postdocs and Staff Researchers"
)


# Undergrads

undergrads <- data %>%
  filter(job_category == "Undergraduate") %>%
  select(
    starts_with("motivations")
  )

undergrads <- prepare_df_for_plotting(undergrads)
# Reorder factor levels based on order in overall motivations dataframe
undergrads$Motivation <- factor(
  undergrads$Motivation,
  levels = levels(motivations$Motivation)
)
undergrads_plot <- basic_bar_chart_fixed_axis(undergrads,
  x_var = "Motivation",
  y_var = "Count",
  title = "Undergraduates"
)

patchwork::wrap_plots(
  faculty_plot,
  nrstaff_plot,
  grads_plot,
  other_researchers_plot,
  undergrads_plot,
  nrow = 2,
  ncol = 3
)

save_plot("motivations_composite.tiff", 8, 6)
