#' Interactive scatterplot with parameter selection and flexible axes
#'
#' Creates an interactive scatterplot using `plotly`, where the y-axis
#' represents measured values (`value`) for different parameters, and the
#' x-axis can be switched interactively between available options.
#'
#' @param data A data frame or tibble containing at least the following columns:
#'   - `station_name`: Name of the station (default x-axis)
#'   - `sample_date`: Sample date (optional alternative x-axis)
#'   - `value`: Numeric variable to plot on the y-axis
#'   - `parameter`: Parameter name (used for y-axis labeling and selection)
#'   - `unit`: Measurement unit corresponding to the parameter
#' @param x Optional. Character string specifying a column in `data` to use as
#'   the x-axis instead of `station_name`. Defaults to `NULL`.
#' @param hline Optional. Numeric value or named list of values giving a
#'   horizontal reference line (e.g., from `get_shark_statistics`).
#'   If a named list, names should match `parameter` values.
#' @param hline_style List of styling options for the line
#'   (default: red, dashed, width 2).
#' @param ... Additional arguments passed to [plotly::plot_ly()], such as
#'   marker styling.
#'
#' @return A `plotly` object representing the interactive scatterplot.
#'
#' @examples
#' \dontrun{
#'
#' df <- dplyr::tibble(
#'   station_name = c("A", "B", "C", "A", "B", "C"),
#'   sample_date = rep(as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")), 2),
#'   expedition_id = rep(1:3, 2),
#'   value = c(10, 15, 8, 5, 7, 6),
#'   parameter = rep(c("Chlorophyll-a", "POC"), each = 3),
#'   unit = rep(c("µg/L", "mg/L"), each = 3)
#' )
#'
#' # Default: station_name on x-axis
#' scatterplot(df)
#'
#' # Use expedition_id on x-axis
#' scatterplot(df, x = "expedition_id")
#'
#' # Add custom marker styling
#' scatterplot(df, marker = list(size = 12, color = "red"))
#'
#' # Add a fixed horizontal reference line
#' scatterplot(df, hline = 10)
#'
#' # Supply different reference lines per parameter
#' scatterplot(
#'   df,
#'   hline = list("Chlorophyll-a" = 7, "POC" = 12)
#' )
#'
#' # Custom styled horizontal line
#' scatterplot(
#'   df,
#'   hline = 5,
#'   hline_style = list(color = "blue", dash = "dot", width = 3)
#' )
#' }
#'
#' @export
scatterplot <- function(data, x = NULL, hline = NULL,
                        hline_style = list(color = "red", dash = "dash", width = 2),
                        ...) {

  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = TRUE,
    showgrid = TRUE
  )

  # Determine x column
  x_col <- if (!is.null(x) && x %in% names(data)) x else "station_name"

  # Ensure parameter + unit columns exist
  if (!all(c("parameter", "unit") %in% names(data))) {
    stop("`data` must contain both `parameter` and `unit` columns.")
  }

  parameters <- unique(data$parameter)

  # Map parameters to their unit (assume 1 unit per parameter)
  param_units <- stats::setNames(
    sapply(parameters, function(p) unique(data$unit[data$parameter == p])[1]),
    parameters
  )

  # Initial y-axis label
  y_label <- paste(parameters[1], "(", param_units[parameters[1]], ")")

  # Create first trace (first parameter visible)
  p <- plot_ly(
    data = subset(data, parameter == parameters[1]),
    x = stats::as.formula(paste0("~`", x_col, "`")),
    y = ~value,
    type = "scatter", mode = "markers", visible = TRUE, ...
  )

  # Add traces for the other parameters (hidden initially)
  for (par in parameters[-1]) {
    p <- add_trace(
      p,
      data = subset(data, parameter == par),
      x = stats::as.formula(paste0("~`", x_col, "`")),
      y = ~value,
      type = "scatter", mode = "markers", visible = FALSE
    )
  }

  # Build dropdown menus
  menus <- list(
    # Parameter selector (also updates y-axis title + hline if named)
    list(
      buttons = lapply(seq_along(parameters), function(i) {
        par <- parameters[i]
        y_title <- paste(par, "(", param_units[par], ")")

        # If hline is a named list, match parameter
        hline_val <- if (is.list(hline) && !is.null(hline[[par]])) hline[[par]] else hline
        shapes <- if (!is.null(hline_val)) list(
          list(type = "line",
               xref = "paper", x0 = 0, x1 = 1,
               yref = "y", y0 = hline_val, y1 = hline_val,
               line = hline_style)
        ) else list()

        list(
          method = "update",
          args = list(
            list(visible = seq_along(parameters) == i),
            list(yaxis = list(title = y_title),
                 shapes = shapes)
          ),
          label = par
        )
      }),
      direction = "down",
      x = 0, y = 1.15,
      xanchor = "left", yanchor = "bottom"
    ),
    # X-axis selector
    list(
      buttons = list(
        list(
          method = "restyle",
          args = list("x", list(data$station_name)),
          label = "X = STATION_NAME"
        ),
        list(
          method = "restyle",
          args = list("x", list(data$sample_date)),
          label = "X = SAMPLE_DATE"
        )
      ),
      direction = "down",
      x = 0.35, y = 1.15,
      xanchor = "left", yanchor = "bottom"
    ),
    # Y-axis scale selector
    list(
      buttons = list(
        list(
          method = "relayout",
          args = list(list(yaxis = list(type = "linear", title = y_label))),
          label = "Y Linear"
        ),
        list(
          method = "relayout",
          args = list(list(yaxis = list(type = "log", title = y_label))),
          label = "Y Log"
        )
      ),
      direction = "down",
      x = 0.7, y = 1.15,
      xanchor = "left", yanchor = "bottom"
    )
  )

  # Add initial hline if present
  shapes_init <- list()
  if (!is.null(hline)) {
    hline_val <- if (is.list(hline) && !is.null(hline[[parameters[1]]])) hline[[parameters[1]]] else hline
    if (!is.null(hline_val)) {
      shapes_init <- list(
        list(type = "line",
             xref = "paper", x0 = 0, x1 = 1,
             yref = "y", y0 = hline_val, y1 = hline_val,
             line = hline_style)
      )
    }
  }

  p %>%
    layout(
      xaxis = ax,
      yaxis = utils::modifyList(ax, list(title = y_label)),
      updatemenus = menus,
      shapes = shapes_init,
      margin = list(t = 100) # Extra space for menus
    )
}
