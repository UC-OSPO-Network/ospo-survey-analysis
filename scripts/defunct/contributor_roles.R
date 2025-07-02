# Purpose: Create a bar plot showing the number of participants
# who have held each role, and a text column plot showing the
# average number of roles per participant.
# Input: deidentified_no_qual.tsv
# Output: contributor_roles.tiff

suppressWarnings(suppressMessages(source("utils.R")))

data <- load_qualtrics_data("deidentified_no_qual.tsv")

roles <- data %>% select(starts_with("contributor_role"))


# Change e.g. "Technical support: assisting users with..."
# To just "Technical support"
roles <- strip_descriptions(roles)

# Change the column names to more useful labels
roles <- rename_cols_based_on_entries(roles)


# How many participants selected each role?
role_counts <- roles %>%
  pivot_longer(
    cols = names(roles),
    values_to = "role"
  ) %>% # convert to a 2-col tibble: column name and role
  filter(role != "") %>% # remove empty entries
  count(role, name = "count")
# ^Count occurrences of each role from the 'role' column,
# producing a new 2-col tibble: role and count



# Which group had the highest number of roles, on average?
# For each participant, count the number of roles they chose
roles2 <- cbind(roles, total_roles = rowSums(!is.na(roles)))

# For each role, calculate the average number of roles per participant
get_mean_num_roles <- function(df, column_num) {
  mean(df[!is.na(df[, column_num]), ]$total_roles)
}
num_roles <- data.frame(
  role = names(roles),
  avg_num_roles_per_participant = sapply(
    seq(ncol(roles)),
    function(x) get_mean_num_roles(roles2, x)
  )
)

# Create a df with both results: participants per role and average roles per participant
role_summary <- merge(role_counts, num_roles, by = "role")

# Sort rows by count
role_summary <- role_summary %>%
  arrange(count)

role_summary$role <- factor(role_summary$role, levels = role_summary$role)





# Bar plot
p1 <- ggplot(role_summary, aes(x = role, y = count)) +
  geom_bar(stat = "identity", fill = "#88CCEE") +
  coord_flip() +
  ggtitle("Number of Participants Who Have Held Each Role") +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid.major = element_line(linetype = "solid", color = "gray90"),
    panel.grid.minor = element_line(linetype = "solid", color = "gray90"),
    plot.title = element_text(vjust = 0.5)
  )


# Text column plot
p2 <- ggplot(role_summary) +
  geom_text(
    aes(
      x = role,
      y = 1,
      label = round(avg_num_roles_per_participant, 1)
    ),
    size = 4
  ) +
  coord_flip() +
  ggtitle("Average Number\nof Roles Per\nParticipant") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


# Combine the two plots side by side
p1 + p2 + plot_layout(widths = c(3, 1)) # Adjust widths for balance

save_plot("contributor_roles.tiff", 10, 5)
