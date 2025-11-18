# Here I am wrangling the export of the coded comments from Q12 or Q6.
# Input and output file names are hard-coded.
# I'm just quickly creating a new spreadsheet that will serve as a supplementary data table.

# Taguette lets me export the data as an .xlsx file.
# I then used MS Excel to convert that to a .tsv so I could read it into R.
# Now I will rearrange it into a format that I like better.

## Import packages and utilities

project_root <- here::here() # requires that you be somewhere in the
# project directory (not above it)
# packages
suppressMessages(source(file.path(project_root, "scripts/packages.R")))
# functions and objects used across scripts
suppressMessages(source(file.path(project_root, "scripts/utils.R")))

# This is just a wrapper for read.csv (see utils.R)
# For privacy reasons, I am not publishing the Taguette export,
# which shows which participants left which comments
data <- load_qualtrics_data("all_tags_q6.tsv")

data_wide <- data %>%
  mutate(mark = "X") %>%
  pivot_wider(
    id_cols = c(document, content), # keep these as rows
    names_from = tag, # tag columns
    values_from = mark, # fill with "X"
    values_fill = "" # fill in blanks
  ) %>%
  select(-document)


#For additional privacy, I'm rearranging the rows into a random order.
data_wide_shuffled <- data_wide[sample.int(nrow(data_wide)), , drop = FALSE]

# sort columns (codes) alphabetically,
# except for the "content" col, which is the survey comment.
final_data <- data_wide_shuffled %>%
  select(-content) %>%
  select(gtools::mixedsort(names(.)))
final_data <- cbind(data_wide_shuffled$content, final_data)
#rename this col
names(final_data)[1] <- "content"

# utils.R
write_df_to_file(
  final_data,
  #"supplementary_tables/q12_comments_all.tsv"
  "supplementary_tables/q6_comments_all.tsv"
)
