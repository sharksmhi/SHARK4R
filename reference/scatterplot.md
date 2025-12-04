# Scatterplot with optional horizontal threshold lines

This function creates a scatterplot from a data frame, optionally
coloring points by a grouping column and adding horizontal threshold
lines. Supports both static `ggplot2` plots and interactive `plotly`
plots with a linear/log toggle.

## Usage

``` r
scatterplot(
  data,
  x = c("station_name", "sample_date"),
  parameter = NULL,
  hline = NULL,
  hline_group_col = NULL,
  hline_value_col = NULL,
  hline_style = list(linetype = "dashed", size = 0.8),
  max_hlines = 5,
  interactive = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble containing at least the following columns:
  `"station_name"`, `"sample_date"`, `"value"`, `"parameter"`, `"unit"`.

- x:

  Character. The column to use for the x-axis. Either `"station_name"`
  or `"sample_date"`.

- parameter:

  Optional character. If provided, only data for this parameter will be
  plotted. If `NULL`, the function will plot the first parameter found
  in the dataset.

- hline:

  Numeric or data.frame. Horizontal line(s) to add. If numeric, a single
  line is drawn at that y-value. If a data.frame, must contain
  `hline_group_col` and `hline_value_col` columns.

- hline_group_col:

  Character. Column used for grouping when `hline` is a data.frame
  and/or for coloring points (optional).

- hline_value_col:

  Character. Column in `hline` used for the y-values of horizontal
  lines.

- hline_style:

  List. Appearance settings for horizontal lines. Should contain
  `linetype` and `size`.

- max_hlines:

  Integer. Maximum number of horizontal line groups to display per
  parameter when `hline` is a data.frame.

- interactive:

  Logical. If TRUE, returns an interactive `plotly` plot; if FALSE,
  returns a static `ggplot2` plot.

- verbose:

  Logical. If TRUE, messages will be displayed during execution.
  Defaults to TRUE.

## Value

A `ggplot` object (if `interactive = FALSE`) or a `plotly` object (if
`interactive = TRUE`).

## Details

- If `hline` is numeric, a single horizontal line is drawn across the
  plot.

- If `hline` is a data.frame, only the first `max_hlines` groups (sorted
  alphabetically) are displayed.

- Points can be colored by `hline_group_col` if provided.

- Interactive plots include buttons to switch between linear and log
  y-axis scales.

## See also

[`load_shark4r_stats`](https://sharksmhi.github.io/SHARK4R/reference/load_shark4r_stats.md)
for loading threshold or summary statistics that can be used to define
horizontal lines in the plot.

## Examples

``` r
if (FALSE) { # \dontrun{
scatterplot(
  data = my_data,
  x = "station_name",
  parameter = "Chlorophyll-a",
  hline = c(10, 20)
)

scatterplot(
  data = my_data,
  x = "sample_date",
  parameter = "Bacterial abundance",
  hline = thresholds_df,
  hline_group_col = "location_sea_basin",
  hline_value_col = "P99"
)
} # }
```
