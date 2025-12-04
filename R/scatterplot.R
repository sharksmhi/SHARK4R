#' Scatterplot with optional horizontal threshold lines
#'
#' This function creates a scatterplot from a data frame, optionally coloring points
#' by a grouping column and adding horizontal threshold lines. Supports both static
#' `ggplot2` plots and interactive `plotly` plots with a linear/log toggle.
#'
#' @param data A data.frame or tibble containing at least the following columns:
#'   `"station_name"`, `"sample_date"`, `"value"`, `"parameter"`, `"unit"`.
#' @param x Character. The column to use for the x-axis. Either `"station_name"` or `"sample_date"`.
#' @param parameter Optional character. If provided, only data for this parameter will be plotted.
#'   If `NULL`, the function will plot the first parameter found in the dataset.
#' @param hline Numeric or data.frame. Horizontal line(s) to add. If numeric, a single line
#'   is drawn at that y-value. If a data.frame, must contain `hline_group_col` and `hline_value_col` columns.
#' @param hline_group_col Character. Column used for grouping when `hline` is a data.frame and/or for coloring points (optional).
#' @param hline_value_col Character. Column in `hline` used for the y-values of horizontal lines.
#' @param hline_style List. Appearance settings for horizontal lines. Should contain `linetype` and `size`.
#' @param max_hlines Integer. Maximum number of horizontal line groups to display per parameter when `hline` is a data.frame.
#' @param interactive Logical. If TRUE, returns an interactive `plotly` plot; if FALSE, returns a static `ggplot2` plot.
#' @param verbose Logical. If TRUE, messages will be displayed during execution. Defaults to TRUE.
#'
#' @return A `ggplot` object (if `interactive = FALSE`) or a `plotly` object (if `interactive = TRUE`).
#'
#' @details
#' - If `hline` is numeric, a single horizontal line is drawn across the plot.
#' - If `hline` is a data.frame, only the first `max_hlines` groups (sorted alphabetically) are displayed.
#' - Points can be colored by `hline_group_col` if provided.
#' - Interactive plots include buttons to switch between linear and log y-axis scales.
#'
#' @seealso
#' \code{\link{load_shark4r_stats}} for loading threshold or summary statistics that
#' can be used to define horizontal lines in the plot.
#'
#' @examples
#' \dontrun{
#' scatterplot(
#'   data = my_data,
#'   x = "station_name",
#'   parameter = "Chlorophyll-a",
#'   hline = c(10, 20)
#' )
#'
#' scatterplot(
#'   data = my_data,
#'   x = "sample_date",
#'   parameter = "Bacterial abundance",
#'   hline = thresholds_df,
#'   hline_group_col = "location_sea_basin",
#'   hline_value_col = "P99"
#' )
#' }
#'
#' @export
scatterplot <- function(data,
                        x = c("station_name", "sample_date"),
                        parameter = NULL,
                        hline = NULL,
                        hline_group_col = NULL,
                        hline_value_col = NULL,
                        hline_style = list(linetype = "dashed", size = 0.8),
                        max_hlines = 5,
                        interactive = TRUE,
                        verbose = TRUE) {
  x <- match.arg(x)

  required_cols <- c("station_name", "sample_date", "value", "parameter", "unit")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain the following columns: ",
         paste(required_cols, collapse = ", "))
  }

  # Filter by selected parameter (if specified)
  available_params <- unique(data$parameter)
  if (!is.null(parameter)) {
    if (!parameter %in% available_params) {
      stop("Parameter '", parameter, "' not found in data. Available parameters: ",
           paste(available_params, collapse = ", "))
    }
    data <- data[data$parameter == parameter, ]
  } else {
    if (length(available_params) > 1) {
      if (verbose) message("Multiple parameters found. Using the first: ", available_params[1])
      data <- data[data$parameter == available_params[1], ]
    }
  }

  parameter_name <- unique(data$parameter)
  unit_name <- unique(data$unit)
  y_label <- paste0(parameter_name[1], " (", unit_name[1], ")")

  # Use tidy evaluation instead of aes_string()
  if (!is.null(hline_group_col) && hline_group_col %in% names(data)) {
    aes_mapping <- ggplot2::aes(!!rlang::sym(x), value, color = !!rlang::sym(hline_group_col))
  } else {
    aes_mapping <- ggplot2::aes(!!rlang::sym(x), value)
  }

  p <- ggplot2::ggplot(data, aes_mapping) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::labs(
      x = "",
      y = y_label,
      color = hline_group_col
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_blank()
    )

  selected_groups <- NULL
  if (!is.null(hline)) {
    if (is.numeric(hline)) {
      p <- p + ggplot2::geom_hline(
        yintercept = hline,
        color = "red",
        linetype = hline_style$linetype,
        linewidth = hline_style$size
      )
    } else if (is.data.frame(hline)) {
      if (is.null(hline_group_col) || is.null(hline_value_col)) {
        stop("When hline is a dataframe, specify 'hline_group_col' and 'hline_value_col'.")
      }
      if (!all(c(hline_group_col, hline_value_col) %in% names(hline))) {
        stop("Columns '", hline_group_col, "' and '", hline_value_col,
             "' must exist in the hline dataframe.")
      }

      # Keep only groups that exist in data
      groups_in_data <- unique(data[[hline_group_col]])
      hline <- hline[hline[[hline_group_col]] %in% groups_in_data, ]
      all_groups <- sort(unique(hline[[hline_group_col]]))
      selected_groups <- utils::head(all_groups, max_hlines)
      hline_filtered <- hline[hline[[hline_group_col]] %in% selected_groups, , drop = FALSE]

      # Factorize for consistent colors
      hline_filtered[[hline_group_col]] <- factor(hline_filtered[[hline_group_col]], levels = selected_groups)
      data[[hline_group_col]] <- factor(data[[hline_group_col]], levels = selected_groups)

      p <- p + ggplot2::geom_hline(
        data = hline_filtered,
        ggplot2::aes(
          yintercept = !!rlang::sym(hline_value_col),
          color = !!rlang::sym(hline_group_col)
        ),
        linetype = hline_style$linetype,
        linewidth = hline_style$size
      )
    } else {
      stop("'hline' must be either numeric or a data.frame.")
    }
  }

  if (!is.null(hline_group_col) && !is.null(selected_groups)) {
    n_colors <- length(selected_groups)
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      palette <- RColorBrewer::brewer.pal(min(max(3, n_colors), 8), "Set1")
      p <- p + ggplot2::scale_color_manual(
        values = stats::setNames(palette[seq_len(n_colors)], selected_groups),
        drop = TRUE
      )
    } else {
      p <- p + ggplot2::scale_color_hue(drop = TRUE)
    }
  }

  if (!interactive) return(p)
  if (!requireNamespace("plotly", quietly = TRUE))
    stop("Package 'plotly' is required for interactive plots.")

  plt <- plotly::ggplotly(p)
  y_buttons <- list(
    list(method = "relayout", args = list(list(yaxis = list(type = "linear"))), label = "Linear scale"),
    list(method = "relayout", args = list(list(yaxis = list(type = "log"))), label = "Log scale")
  )

  plt <- plt %>%
    plotly::layout(
      updatemenus = list(
        list(
          type = "buttons",
          direction = "right",
          buttons = y_buttons,
          x = 0,
          y = 1.15,
          xanchor = "left",
          yanchor = "top",
          pad = list(r = 10, t = 0),
          showactive = TRUE
        )
      )
    )

  return(plt)
}
