#!/usr/bin/env Rscript

project_root <- here::here() # requires that you be somewhere in the
# project directory (not above it)
# packages
suppressMessages(source(file.path(project_root, "scripts/packages.R")))
# functions and objects used across scripts
suppressMessages(source(file.path(project_root, "scripts/utils.R")))


make_df_binary <- function(df, cols = NULL) {
  # Determine columns to modify
  if (is.null(cols)) {
    cols_to_modify <- names(df)
  } else if (is.numeric(cols)) {
    cols_to_modify <- names(df)[cols]
  } else if (is.character(cols)) {
    cols_to_modify <- cols
  } else {
    stop(
      "`cols` must be NULL, a character vector of column names, or numeric indices."
    )
  }

  df <- df %>%
    # Convert "Non-applicable" to NA
    mutate(across(
      all_of(cols_to_modify),
      ~ ifelse(.x == "Non-applicable", NA, .x)
    )) %>%
    # Turn empty strings into NAs, and turn non-empty strings into 1s
    mutate(across(all_of(cols_to_modify), ~ ifelse(.x == "", NA, 1))) %>%
    # Convert all NAs to 0s
    mutate(across(all_of(cols_to_modify), ~ ifelse(is.na(.x), 0, .x)))

  return(df)
}


pii_data <- load_qualtrics_data("pii.tsv")
quant_data <- load_qualtrics_data("all_quant.tsv")


pii_data <- pii_data %>%
  select(starts_with("stay_in_touch"))

data <- cbind(pii_data, quant_data$campus)
names(data)[ncol(data)] <- "campus"

data <- data %>% filter(stay_in_touch_email != "")

data <- make_df_binary(
  data,
  cols = c("stay_in_touch_boxes_1", "stay_in_touch_boxes_2")
)

# substitute 1s with "yes" and 0s with "no"
data <- data %>%
  mutate(across(everything(), ~ gsub("1", "yes", .))) %>%
  mutate(across(everything(), ~ gsub("0", "no", .)))

data <- data %>% arrange(campus)

write.table(
  data,
  file = file.path(DATA_PATH, "stay_in_touch.csv"),
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)
