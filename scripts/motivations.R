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

ggplot(sums, aes(
  x = Motivation,
  y = Count,
)) +
  geom_bar(stat = "identity", fill = colors[[1]]) +
  ggtitle("Reasons for Contributing to Open Source") +
  labs(y = "Number of Respondents") +
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
