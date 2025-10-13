#' Scatterplot for SHARK data with optional horizontal threshold lines
#'
#' This function creates a scatterplot of SHARK data, optionally coloring points
#' by a grouping column and adding horizontal threshold lines. Supports both static
#' `ggplot2` plots and interactive `Plotly` plots with a log/linear toggle.
#'
#' @param data A data.frame or tibble containing at least the following columns:
#'   `"station_name"`, `"sample_date"`, `"value"`, `"parameter"`, `"unit"`.
#' @param x Character. The column to use for the x-axis. Choices are `"station_name"` or `"sample_date"`.
#' @param hline Numeric or data.frame. Horizontal line(s) to add. If numeric, a single line
#'   is added. If a data.frame, must contain `parameter`, a grouping column (`hline_group_col`)
#'   and a value column (`hline_value_col`).
#' @param hline_group_col Character. Column in `data` and `hline` used for grouping (optional).
#' @param hline_value_col Character. Column in `hline` used for the y-values of horizontal lines.
#' @param hline_style List. A list with `linetype` and `size` controlling the appearance of hlines.
#' @param interactive Logical. If TRUE, returns a Plotly interactive plot. If FALSE, returns ggplot2.
#' @param max_hlines Integer. Maximum number of horizontal lines to display per parameter when showing all groups.
#'
#' @return A `ggplot` object (if `interactive = FALSE`) or a `plotly` object (if `interactive = TRUE`).
#'
#' @examples
#' \dontrun{
#' scatterplot(
#'   data = my_ifcb_data,
#'   x = "station_name",
#'   hline = thresholds_df,
#'   hline_group_col = "location_sea_basin",
#'   hline_value_col = "P99"
#' )
#' }
#'
#' @export
scatterplot <- function(data,
                        x = c("station_name", "sample_date"),
                        hline = NULL,
                        hline_group_col = NULL,
                        hline_value_col = NULL,
                        hline_style = list(linetype = "dashed", size = 0.8),
                        interactive = TRUE,
                        max_hlines = 5) {

  x <- match.arg(x)

  required_cols <- c("station_name", "sample_date", "value", "parameter", "unit")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain the following columns: ",
         paste(required_cols, collapse = ", "))
  }

  parameter_name <- unique(data$parameter)
  unit_name <- unique(data$unit)
  if (length(parameter_name) > 1)
    warning("Multiple parameters found; only the first will be used for labeling.")
  y_label <- paste0(parameter_name[1], " (", unit_name[1], ")")

  aes_mapping <- if (!is.null(hline_group_col) && hline_group_col %in% names(data)) {
    ggplot2::aes_string(x = x, y = "value", color = hline_group_col)
  } else {
    ggplot2::aes_string(x = x, y = "value")
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
      selected_groups <- head(all_groups, max_hlines)
      hline_filtered <- hline[hline[[hline_group_col]] %in% selected_groups, , drop = FALSE]

      # Factorize for consistent colors
      hline_filtered[[hline_group_col]] <- factor(hline_filtered[[hline_group_col]], levels = selected_groups)
      data[[hline_group_col]] <- factor(data[[hline_group_col]], levels = selected_groups)

      p <- p + ggplot2::geom_hline(
        data = hline_filtered,
        ggplot2::aes_string(yintercept = hline_value_col, color = hline_group_col),
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
    } else {
      palette <- ggplot2::hue_pal()(n_colors)
    }
    p <- p + ggplot2::scale_color_manual(
      values = setNames(palette[seq_len(n_colors)], selected_groups),
      drop = TRUE
    )
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

