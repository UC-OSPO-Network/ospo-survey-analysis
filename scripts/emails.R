#!/usr/bin/env Rscript

suppressWarnings(suppressMessages(source("utils.R")))

pii_data <- load_qualtrics_data("pii.tsv")
deid_data <- load_qualtrics_data("deidentified_no_qual.tsv")


pii_data <- pii_data %>%
  select(starts_with("stay_in_touch"))

data <- cbind(pii_data, deid_data$campus)
names(data)[ncol(data)] <- "campus"

data <- data %>% filter(stay_in_touch_email != "")

data <- make_df_binary(data, cols = c("stay_in_touch_boxes_1", "stay_in_touch_boxes_2"))

# substitute 1s with "yes" and 0s with "no"
data <- data %>%
  mutate(across(everything(), ~ gsub("1", "yes", .))) %>%
  mutate(across(everything(), ~ gsub("0", "no", .)))

data <- data %>% arrange(campus)

write.table(data,
  file = file.path(Sys.getenv("DATA_PATH"), "stay_in_touch.csv"),
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)
