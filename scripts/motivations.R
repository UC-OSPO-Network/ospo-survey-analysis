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
  df <- df %>%
    filter(if_any(Job:Other, ~ .x != ""))
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



data <- load_qualtrics_data("survey", "deidentified_no_qual.tsv")

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





faculty <- get_df_for_job_category("Faculty")
nrstaff <- get_df_for_job_category("Non-research Staff")
grads <- get_df_for_job_category("Grad Student")
undergrads <- get_df_for_job_category("Undergraduate")
other_researchers <- get_df_for_job_category(
  "Other research staff (e.g., research scientist, research software engineer)"
)



faculty$Role <- "Faculty"
nrstaff$Role <- "Non-research Staff"
grads$Role <- "Grad Students"
other_researchers$Role <- "Postdocs and Staff Researchers"
undergrads$Role <- "Undergraduates"
composite_df <- rbind(faculty, nrstaff, grads, other_researchers, undergrads)


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



# Starting to think about statistical analysis....
# install.packages("simr")
# library(simr)
# library(lme4)
# Need to reshape data to these columns: RespondentID, Role, Motivation, Selected (where 1 = yes, 0 = no)

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

motivation_block <- motivations_raw %>%
  select(Job:Other)
motivation_block <- make_df_binary(motivation_block)

motivations_processed <- data.frame(motivation_block, motivations_raw$Role)
names(motivations_processed)[ncol(motivations_processed)] <- "Role"
motivations_processed$RespondentID <- as.factor(1:nrow(motivations_processed))
motivations_long <- pivot_longer(
  motivations_processed,
  cols = Job:Other,
  names_to = "Motivation",
  values_to = "Selected"
)

# Since each person gave multiple answers (one per motivation),
# these observations are not independent. We tell the model that
# RespondentID is a random effect.

mymodel <- glmer(
  Selected ~ Motivation * Role + (1 | RespondentID),
  data = motivations_long,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
# https://stats.stackexchange.com/questions/164457/r-glmer-warnings-model-fails-to-converge-model-is-nearly-unidentifiable
# Still isn't converging, need to troubleshoot
