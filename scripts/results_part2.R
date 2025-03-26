suppressWarnings(suppressMessages(source("utils.R")))


calculate_mode <- function(x) {
  x <- na.omit(x) # Remove NAs
  if (length(x) == 0) {
    return(NA)
  } # Return NA if no values
  uniq_vals <- unique(x)
  freq <- tabulate(match(x, uniq_vals))
  mode_val <- uniq_vals[which.max(freq)]
  return(mode_val)
}


data <- load_qualtrics_data("deidentified_no_qual.tsv")

# Why do people contribute?
motivations <- data %>% select(
  starts_with("motivations")
)
codenames <- list(
  "Job" = "Developing open-source products is part of my job",
  "Improve Tools" = "To improve the tools in my field",
  "Customize" = "To customize existing tools for my specific needs",
  "Network" = "To build a network of peers",
  "Give back" = "To give back to the open source community",
  "Skills" = "To improve my skills",
  "Fun" = "Because it's fun",
  "Other" = "Other (Please specify. Multiple answers should be comma-separated.)"
)
motivations <- recode_dataframe(motivations, codenames)
motivations <- rename_cols_based_on_entries(motivations)
motivations %>% summarise(across(everything(), ~ sum(!is.na(.) & . != "")))

# How important is open source for each importance category?
# Need to do some data wrangling first.
importance <- data %>% select(
  starts_with("importance_opensrc")
)
# Convert "Non-applicable" to NA
importance <- importance %>%
  mutate(across(everything(), ~ ifelse(.x == "Non-applicable", NA, .x)))
codenames_rev <- c(
  # Note that elsewhere in this repo, in the codenames variable,
  # the names to be replaced come first.
  "Research" = "importance_opensrc_1",
  "Teaching" = "importance_opensrc_2",
  "Learning" = "importance_opensrc_3",
  "Professional Development" = "importance_opensrc_4",
  "Job" = "importance_opensrc_5"
)
importance <- importance %>%
  rename(any_of(codenames_rev))

recode_values <- c(
  "Not at all important" = 0,
  "Slightly important" = 1,
  "Moderately important" = 2,
  "Important" = 3,
  "Very important" = 4
)
importance <- importance %>%
  mutate(across(everything(), ~ recode_values[.x]))

# Create a summary dataframe
mode_row <- sapply(importance, calculate_mode)
mean_row <- colMeans(importance, na.rm = TRUE)
total_non_na <- colSums(!is.na(importance))
summary_df <- rbind(mode_row, mean_row, total_non_na)
rownames(summary_df) <- c("Mode", "Mean", "Total non-NA responses")
summary_df
