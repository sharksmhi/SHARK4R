# General outlier check function for SHARK data

This function checks whether values for a specified parameter exceed a
predefined threshold. Thresholds are provided in a dataframe (default
`.threshold_values`), which should contain columns for `parameter`,
`datatype`, and at least one numeric threshold column (e.g.,
`extreme_upper`). Only rows in `data` matching both the `parameter` and
`delivery_datatype` (`datatype`) are considered. Optionally, data can be
grouped by a custom column (e.g., `location_sea_basin`) when thresholds
vary by group.

## Usage

``` r
check_outliers(
  data,
  parameter,
  datatype,
  threshold_col = "extreme_upper",
  thresholds = .threshold_values,
  custom_group = NULL,
  direction = c("above", "below"),
  return_df = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  A tibble containing data in SHARK format. Must include columns:
  `parameter`, `value`, `delivery_datatype`, `station_name`,
  `sample_date`, `sample_id`, `shark_sample_id_md5`,
  `sample_min_depth_m`, `sample_max_depth_m`, and any custom grouping
  column used in `custom_group`.

- parameter:

  Character. Name of the parameter to check. Must exist in both
  `data$parameter` and `thresholds$parameter`.

- datatype:

  Character. Data type to match against `delivery_datatype` in `data`
  and `datatype` in `thresholds`.

- threshold_col:

  Character. Name of the threshold column in `thresholds` to use for
  comparison. Defaults to `"extreme_upper"`. Other columns (e.g.,
  `"min"`, `"Q1"`, `"median"`, `"max"`, `"mild_upper"`, etc.) can also
  be used if present.

- thresholds:

  A tibble/data frame of thresholds. Must include columns `parameter`,
  `datatype`, and at least one numeric threshold column. Defaults to
  `.threshold_values`.

- custom_group:

  Character or NULL. Optional column name in `data` and `thresholds` for
  grouping (e.g., `"location_sea_basin"`). If specified, thresholds are
  matched by group as well as `parameter` and `datatype`.

- direction:

  Character. Either `"above"` (flag values above threshold) or `"below"`
  (flag values below threshold). Default is `"above"`.

- return_df:

  Logical. If TRUE, returns a plain data.frame of flagged rows instead
  of a DT datatable. Default = FALSE.

- verbose:

  Logical. If TRUE, messages will be displayed during execution.
  Defaults to TRUE.

## Value

If outliers are found, returns a
[`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) or a
data.frame (if `return_df = TRUE`) containing: `datatype`,
`station_name`, `sample_date`, `sample_id`, `parameter`, `value`,
`threshold`, and `custom_group` if specified. Otherwise, prints a
message indicating that values are within the threshold range (if
`verbose = TRUE`) and returns `invisible(NULL)`.

## Details

- Only rows in `data` matching both `parameter` and `delivery_datatype`
  are checked.

- If `custom_group` is specified, thresholds are applied per group.

- If `threshold_col` does not exist in `thresholds`, the function stops
  with a warning.

- Values exceeding (or below) the threshold are flagged as outliers.

- Intended for interactive use in Shiny apps where `threshold_col` can
  be selected dynamically.

## See also

[`get_shark_statistics()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_statistics.md)
for preparing updated threshold data.

## Examples

``` r
# Minimal example dataset
example_data <- dplyr::tibble(
  station_name = c("S1", "S2"),
  sample_date = as.Date(c("2025-01-01", "2025-01-02")),
  sample_id = 1:2,
  shark_sample_id_md5 = letters[1:2],
  sample_min_depth_m = c(0, 5),
  sample_max_depth_m = c(1, 6),
  parameter = c("Param1", "Param1"),
  value = c(5, 12),
  delivery_datatype = c("TypeA", "TypeA")
)

example_thresholds <- dplyr::tibble(
  parameter = "Param1",
  datatype = "TypeA",
  extreme_upper = 10,
  mild_upper = 8
)

# Check for values above "extreme_upper"
check_outliers(
  data = example_data,
  parameter = "Param1",
  datatype = "TypeA",
  threshold_col = "extreme_upper",
  thresholds = example_thresholds,
  return_df = TRUE
)
#> WARNING: Param1 ( TypeA ) exceeds extreme_upper in dataset
#> # A tibble: 1 × 7
#>   delivery_datatype station_name sample_date sample_id parameter value threshold
#>   <chr>             <chr>        <date>          <int> <chr>     <dbl>     <dbl>
#> 1 TypeA             S2           2025-01-02          2 Param1       12        10

# Check for values above "mild_upper"
check_outliers(
  data = example_data,
  parameter = "Param1",
  datatype = "TypeA",
  threshold_col = "mild_upper",
  thresholds = example_thresholds,
  return_df = TRUE
)
#> WARNING: Param1 ( TypeA ) exceeds mild_upper in dataset
#> # A tibble: 1 × 7
#>   delivery_datatype station_name sample_date sample_id parameter value threshold
#>   <chr>             <chr>        <date>          <int> <chr>     <dbl>     <dbl>
#> 1 TypeA             S2           2025-01-02          2 Param1       12         8
```
