# This script is not meant to be executed directly.
# It contains various functions and variables that are re-used
# by other scripts in this project.

################ EDIT ME, IF DESIRED ################
DATA_PATH <- Sys.getenv("DATA_PATH")
FIGURE_PATH <- Sys.getenv("FIGURE_PATH")
COLORS <- c(
  # modified from https://sronpersonalpages.nl/~pault/
  "#332288",
  "#88CCEE",
  "#44AA99",
  "#117733",
  "#999933",
  "#DDCC77",
  "#CC6677",
  "#882255",
  "#AA4499",
  "#BBBBBB"
)
####################################################
# I keep all my data in one folder, and all my figures
# in another. I am so paranoid about accidentally releasing
# sensitive data that I don't even keep the data in my git repo
# with a gitignore file. I keep them somewhere completely separate
# on my machine.
# I also keep my figures in a separate location, since I
# don't really benefit from checking those into version control.

################ Functions for reading and writing tabular data ################

load_qualtrics_data <- function(filename, fileEncoding = NULL) {
  # Base arguments to pass to read.csv
  args <- list(
    file = file.path(DATA_PATH, filename),
    header = TRUE,
    sep = "\t",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  # Conditionally add fileEncoding if provided
  if (!is.null(fileEncoding)) {
    args$fileEncoding <- fileEncoding
  }
  # Call read.csv with constructed argument list.
  # This way, read.csv will automatically choose
  # a fileEncoding if it is not specified.
  do.call(read.csv, args)
}

# This function is used by my data_cleanup scripts.
write_df_to_file <- function(df, filename) {
  write.table(
    df,
    file.path(DATA_PATH, filename),
    quote = FALSE,
    row.names = FALSE,
    sep = "\t"
  )
}


################ Functions for data wrangling ################

# Function to drop rows that are entirely NA or empty strings.
# Inputs:
#    df: a data frame
#    strict: optional; a boolean, FALSE by default. Indicates
#    whether you wish to discard all rows that are entirely NA or ""
#    (FALSE, default), or discard all rows that contain ANY NA or ""
#    (TRUE).
# Outputs:
#    A new data frame where incomplete rows have been removed.
#    Rows of all zeros will be retained.
# Example:
# t <- data.frame(
#   col1 = c("A", "", "", NA, "A", 0, "A"),
#   col2 = c("B", "", "B", NA, NA, 0, 0)
# )
#
# > t
#   col1 col2
# 1    A    B
# 2
# 3         B
# 4 <NA> <NA>
# 5    A <NA>
# 6    0    0
# 7    A    0
#
# > exclude_empty_rows(t)
#   col1 col2
# 1    A    B
# 3         B
# 5    A <NA>
# 6    0    0
# 7    A    0
#
# > exclude_empty_rows(t, strict = TRUE)
#   col1 col2
# 1    A    B
# 6    0    0
# 7    A    0

exclude_empty_rows <- function(df, strict = FALSE) {
  # Create a logical matrix indicating where values are NA or ""
  missing_mat <- is.na(df) | df == ""

  # Count per-row how many “missing” entries there are
  missing_per_row <- rowSums(missing_mat)
  n_cols <- ncol(df)

  if (strict) {
    # Keep only rows with zero missing entries
    keep <- missing_per_row == 0
  } else {
    # Keep rows that are not entirely missing
    keep <- missing_per_row < n_cols
  }

  # Subset, preserving data.frame structure even if one column
  return(df[keep, , drop = FALSE])
}


exclude_empty_columns <- function(df) {
  df[, colSums(is.na(df) | df == "") < nrow(df)]
} # Drop columns that are entirely NA or empty strings.
# Columns of all zeros will be retained.

################ Utils for plotting ################

# Re-order factor levels in one column by values in another.
# Returns the original data frame with factor_col re-ordered based on value_col.
# Leave column names unquoted.
reorder_factor_by_column <- function(
  df,
  factor_col,
  value_col,
  descending = TRUE
) {
  df <- df %>%
    mutate(
      {{ factor_col }} := fct_reorder(
        # double-curlys and := assign a col by name using tidy evaluation
        .f = {{ factor_col }},
        .x = {{ value_col }},
        .fun = if (descending) `-` else identity # If descending = TRUE, add a negative sign (-) to reverse the sort
      )
    )
  return(df)
}


basic_bar_chart <- function(
  df,
  x_var,
  y_var,
  title,
  ylabel = "Number of Respondents",
  show_axis_title_x = FALSE,
  show_axis_title_y = TRUE,
  axis_title_size_x = 24,
  axis_title_size_y = 24,
  axis_text_size_x = 22,
  axis_text_size_y = 22,
  axis_text_angle_x = 60,
  title_size = 24,
  color_index = 1,
  horizontal = FALSE,
  show_ticks_x = FALSE,
  show_ticks_y = TRUE,
  show_bar_labels = FALSE,
  label_position = c("inside", "above"),
  label_color = "white",
  show_grid = TRUE,
  percent = FALSE
) {
  label_position <- match.arg(label_position)

  # Axis title settings
  axis_title_x <- if (show_axis_title_x) {
    element_text(size = axis_title_size_x)
  } else {
    element_blank()
  }
  axis_title_y <- if (show_axis_title_y) {
    element_text(
      size = axis_title_size_y,
      margin = margin(r = 15)
    )
  } else {
    element_blank()
  }

  # Axis tick settings
  axis_ticks_x <- if (show_ticks_x) element_line() else element_blank()
  axis_ticks_y <- if (show_ticks_y) element_line() else element_blank()

  # Build the plot
  p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_bar(stat = "identity", fill = COLORS[[color_index]]) +
    ggtitle(title) +
    labs(y = ifelse(is.null(ylabel), "Number of Respondents", ylabel)) +
    theme(
      axis.title.x = axis_title_x,
      axis.title.y = axis_title_y,
      axis.text.x = element_text(
        angle = axis_text_angle_x,
        vjust = 0.6,
        size = axis_text_size_x
      ),
      axis.text.y = element_text(size = axis_text_size_y),
      axis.ticks.x = axis_ticks_x,
      axis.ticks.y = axis_ticks_y,
      legend.position = "none",
      panel.background = element_blank(),
      panel.grid = if (show_grid) {
        element_line(linetype = "solid", color = "gray90")
      } else {
        element_blank()
      },
      plot.title = element_text(
        hjust = 0.5,
        size = title_size,
        margin = margin(b = 15)
      ),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
    )

  # Convert y-axis values from proportion to percent (optional)
  if (percent) {
    p <- p +
      scale_y_continuous(labels = scales::percent)
  }

  # Add text labels (optional)
  if (show_bar_labels) {
    if (horizontal) {
      p <- p +
        geom_text(
          aes(
            # if percent=TRUE, format the y‐var as a percent; otherwise just show the raw value
            label = if (percent) {
              scales::percent(.data[[y_var]], accuracy = 1)
              # accuracy says use whole-number percentage, no decimal
            } else {
              .data[[y_var]]
            }
          ),
          color = label_color,
          size = 8,
          hjust = if (label_position == "inside") 1.2 else -0.1, # shift left of bar end
          vjust = 0.5
        )
    } else {
      # vertical bars
      p <- p +
        geom_text(
          aes(
            # if percent=TRUE, format the y‐var as a percent; otherwise just show the raw value
            label = if (percent) {
              scales::percent(.data[[y_var]], accuracy = 1)
              # accuracy says use whole-number percentage, no decimal
            } else {
              .data[[y_var]]
            }
          ),
          color = label_color,
          size = 8,
          vjust = if (label_position == "inside") 1.2 else -0.3, # above bar = negative vjust
          hjust = 0.5
        )
    }
  }

  # Flip coordinates if horizontal
  if (horizontal) {
    p <- p + coord_flip()
  }

  return(p)
}


stacked_bar_chart <- function(
  df,
  x_var,
  y_var,
  fill,
  title,
  ylabel = NULL,
  show_grid = TRUE,
  proportional = FALSE
) {
  # Set position for geom_bar
  position_type <- if (proportional) "fill" else "stack"

  # Determine y-axis label if not provided
  ylabel_final <- if (!is.null(ylabel)) {
    ylabel
  } else if (proportional) {
    "Proportion of Responses"
  } else {
    "Number of Responses"
  }

  # Build the plot
  p <- ggplot(
    df,
    aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill]])
  ) +
    geom_bar(stat = "identity", position = position_type) +
    ggtitle(title) +
    labs(y = ylabel_final) +
    scale_fill_manual(values = COLORS) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 24, margin = margin(r = 15)),
      axis.text.x = element_text(
        angle = 60,
        vjust = 0.9,
        hjust = 0.98,
        size = 24
      ),
      axis.text.y = element_text(size = 24),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank(),
      panel.grid = if (show_grid) {
        element_line(linetype = "solid", color = "gray90")
      } else {
        element_blank()
      },
      legend.title = element_blank(),
      legend.text = element_text(size = 24),
      plot.title = element_text(hjust = 0.5, size = 24),
      plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "cm")
    )
  return(p)
}


grouped_bar_chart <- function(
  df,
  x_var,
  fill_var,
  title,
  ylabel = NULL,
  color_palette = NULL,
  show_grid = TRUE
) {
  if (is.null(color_palette)) {
    color_palette <- COLORS
  }
  ylabel <- ifelse(is.null(ylabel), "Number of Respondents", ylabel)
  ggplot(df, aes(x = .data[[x_var]], fill = .data[[fill_var]])) +
    geom_bar(position = "dodge") +
    ggtitle(title) +
    labs(y = ylabel) +
    scale_fill_manual(values = color_palette) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 24),
      axis.text.x = element_text(angle = 60, vjust = 0.6, size = 18),
      axis.text.y = element_text(size = 18),
      axis.ticks.x = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 18),
      panel.background = element_blank(),
      panel.grid = if (show_grid) {
        element_line(linetype = "solid", color = "gray90")
      } else {
        element_blank()
      },
      plot.title = element_text(hjust = 0.5, size = 24),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
    )
}


# Save a plot
save_plot <- function(fname, w, h, p = NULL, ftype = "tiff", res = 700) {
  ggsave(
    filename = fname,
    width = w,
    height = h,
    plot = p, # if NULL, ggsave() will use the last‐drawn plot
    device = ftype,
    dpi = res,
    path = FIGURE_PATH
  )
}
