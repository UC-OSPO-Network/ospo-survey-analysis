# Utils for reading data

load_qualtrics_data <- function(filename, fileEncoding = NULL) {
  # In my ~/.Renviron file, I have DATA_PATH = "/Path/to/data/folder"
  data_path <- file.path(Sys.getenv("DATA_PATH"), filename)

  # Base arguments to pass to read.csv
  args <- list(
    file = data_path,
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


# Utils for plotting

colors <- c(
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
    geom_bar(stat = "identity", fill = colors[[color_index]]) +
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
    scale_fill_manual(values = colors) +
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
    color_palette <- colors
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
# Path is in my ~/.Renviron file
figure_path <- Sys.getenv("FIGURE_PATH")
save_plot <- function(fname, w, h, p = NULL, ftype = "tiff", res = 700) {
  ggsave(
    filename = fname,
    width = w,
    height = h,
    plot = p, # if NULL, ggsave() will use the last‐drawn plot
    device = ftype,
    dpi = res,
    path = figure_path
  )
}


# Utils to clean data

rename_cols_based_on_codenames <- function(df, codes) {
  return(
    df %>%
      rename(any_of(codes))
  )
}


# Replace values in a data frame with new values based on a list of codes.

recode_dataframe_likert <- function(df, codes, likert_cols) {
  df %>%
    mutate(
      across(
        all_of(likert_cols),
        ~ codes[as.character(.x)] # convert factor to character first
      )
    )
}


# Function to drop rows that are entirely NA or empty strings.
# Rows of all zeros will be retained.
# strict mode: drop rows containing even one NA or empty string.
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
  df[keep, , drop = FALSE]
}


exclude_empty_columns <- function(df) {
  df[, colSums(is.na(df) | df == "") < nrow(df)]
} # Drop columns that are entirely NA or empty strings.
# Columns of all zeros will be retained.

# Functions to calculate summary statistics

# Create a summary dataframe (an alternative to summary(importance))
custom_summary <- function(df) {
  mean_row <- round(colMeans(df, na.rm = TRUE), 2)
  median_row <- apply(df, 2, function(x) round(median(x, na.rm = TRUE), 2))
  mode_row <- sapply(df, function(x) round(calculate_mode(x), 2))
  total_value <- apply(df, 2, function(x) round(sum(x, na.rm = TRUE), 2))
  summary_df <- rbind(mean_row, median_row, mode_row, total_value)
  rownames(summary_df) <- c("Mean", "Median", "Mode", "Sum")
  return(summary_df)
}

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
