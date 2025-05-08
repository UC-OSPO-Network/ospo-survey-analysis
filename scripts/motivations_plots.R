# Plot data for Q6 of the suervey (motivations for contributing to open source)

suppressWarnings(suppressMessages(source("utils.R")))


get_df_for_job_category <- function(job) {
  df <- data %>%
    filter(job_category == job) %>%
    select(
      starts_with("motivations")
    )
  df <- shorten_long_responses(df, codenames)
  # Remove any columns that are all NA or empty strings
  df <- df[, colSums(is.na(df) | df == "") < nrow(df)]
  df <- rename_cols_based_on_entries(df)
  # Remove any rows where they didn't answer the question about motivations
  df <- make_df_binary(df)
  df <- data.frame(
    Motivation = names(df),
    Count = unname(apply(df, 2, function(x) round(sum(x, na.rm = TRUE))))
  )
  return(df)
}

stacked_bar_chart <- function(
    df,
    x_var,
    y_var,
    fill,
    title,
    ylabel = NULL,
    proportional = FALSE) {
  # Set position for geom_bar
  position_type <- if (proportional) "fill" else "stack"

  # Determine y-axis label if not provided
  ylabel_final <- if (!is.null(ylabel)) ylabel else if (proportional) "Proportion of Responses" else "Number of Responses"

  # Build the plot
  p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill]])) +
    geom_bar(stat = "identity", position = position_type) +
    ggtitle(title) +
    labs(y = ylabel_final) +
    scale_fill_manual(values = colors) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(angle = 60, vjust = 0.6, size = 10),
      axis.text.y = element_text(size = 10),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank(),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
    )
  return(p)
}



data <- load_qualtrics_data("deidentified_no_qual.tsv")

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






############## Basic bar plot of contributor motivations ##############

motivations <- data %>% select(
  starts_with("motivations")
)
motivations <- shorten_long_responses(motivations, codenames)
# Remove any columns that are all NA or empty strings
motivations <- motivations[, colSums(is.na(motivations) | motivations == "") < nrow(motivations)]
# Reomve any rows that are all NA or empty strings (this only works because we don't have a "Role" column yet)
motivations <- exclude_empty_rows(motivations)
motivations <- rename_cols_based_on_entries(motivations)
motivations <- make_df_binary(motivations)
motivations <- data.frame(
  Motivation = names(motivations),
  Count = unname(apply(motivations, 2, function(x) round(sum(x, na.rm = TRUE))))
)


# Reorder factor levels based on count
motivations <- motivations %>%
  mutate(Motivation = fct_reorder(Motivation, Count, .desc = FALSE))

overall_plot <- basic_bar_chart(motivations,
  x_var = "Motivation",
  y_var = "Count",
  title = "Reasons for Contributing to Open Source: All Contributors",
  horizontal = TRUE,
  show_bar_labels = TRUE,
  show_ticks_y = FALSE,
  color_index = 3,
  show_axis_title_y = FALSE
)

save_plot("motivations_overall.tiff", 8, 6)










############## Stacked bar plots of motivations by role ##############

faculty <- get_df_for_job_category("Faculty")
nrstaff <- get_df_for_job_category("Non-research Staff")
postdocs <- get_df_for_job_category("Post-Doc")
other_researchers <- get_df_for_job_category(
  "Other research staff (e.g., research scientist, research software engineer)"
)
grads <- get_df_for_job_category("Grad Student")
undergrads <- get_df_for_job_category("Undergraduate")


# Combine post-docs and other research staff into one category
postdocs_other <- bind_rows(postdocs, other_researchers) %>%
  group_by(Motivation) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")



faculty$Role <- "Faculty"
nrstaff$Role <- "Non-research Staff"
grads$Role <- "Grad Students"
postdocs_other$Role <- "Postdocs and Staff Researchers"
undergrads$Role <- "Undergraduates"
composite_df <- rbind(faculty, nrstaff, grads, postdocs_other, undergrads)


stacked_plot_raw <- stacked_bar_chart(composite_df,
  x_var = "Role",
  y_var = "Count",
  fill = "Motivation",
  title = "Reasons for Contributing to Open Source",
)

stacked_plot_proportional <- stacked_bar_chart(composite_df,
  x_var = "Role",
  y_var = "Count",
  fill = "Motivation",
  title = "Reasons for Contributing to Open Source",
  proportional = TRUE
)

stacked_plot_raw + stacked_plot_proportional

save_plot("motivations_stacks.tiff", 12, 8)






############## Line plot showing proportion motivated by skills, per role ##############

motivations_raw <- data %>% select(
  starts_with("motivations")
)
motivations_raw <- shorten_long_responses(motivations_raw, codenames)
motivations_raw <- rename_cols_based_on_entries(motivations_raw)
motivations_raw$Role <- data$job_category
motivations_raw <- shorten_long_responses(motivations_raw, c("Other research staff" = "Other research staff"))

# Remove any rows where they didn't answer the question about motivations
motivations_raw <- motivations_raw %>%
  filter(if_any(Job:Other, ~ .x != ""))

motivation_cols <- as.vector(codenames)
motivations_processed <- make_df_binary(motivations_raw, cols = motivation_cols)


skills_by_role <- motivations_processed %>%
  group_by(Role) %>%
  summarise(
    n_yes = sum(Skills == 1), # number of 1s
    n_tot = n(), # total rows
    Proportion = n_yes / n_tot
  )

skills_by_role_clean <- skills_by_role %>%
  # drop the staff categories
  filter(!Role %in% c("Non-research Staff", "Other research staff")) %>%
  # drop the unnecessary columns
  select(Role, Proportion) %>%
  # order the factor levels
  mutate(Role = factor(Role,
    levels = c(
      "Undergraduate",
      "Grad Student",
      "Post-Doc",
      "Faculty"
    ),
    ordered = TRUE
  )) %>%
  arrange(Role)


ggplot(skills_by_role_clean, aes(x = Role, y = Proportion)) +
  geom_point(size = 4) + # Adjust dot size
  labs(
    x = "Career stage",
    y = "Proportion of Participants Motivated by\nDesire to Learn New Skills",
    title = "Skills as a motivator, by career stage"
  ) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 60, vjust = 0.6, size = 12),
    axis.text.y = element_text(size = 12),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
    panel.grid = element_line(linetype = "solid", color = "gray90"),
    panel.background = element_blank()
  )

save_plot("motivations_skill_by_role.tiff", 8, 6)
