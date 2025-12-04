# Summarize numeric SHARK parameters with ranges and outlier thresholds

Downloads SHARK data for a given time period, filters to numeric
parameters, and calculates descriptive statistics and Tukey outlier
thresholds.

## Usage

``` r
get_shark_statistics(
  fromYear = NULL,
  toYear = NULL,
  datatype = NULL,
  group_col = NULL,
  min_obs = 3,
  max_non_numeric_frac = 0.05,
  cache_result = FALSE,
  prod = TRUE,
  utv = FALSE,
  verbose = TRUE
)
```

## Arguments

- fromYear:

  Start year for download (numeric). Defaults to 5 years before the last
  complete year.

- toYear:

  End year for download (numeric). Defaults to the last complete year.

- datatype:

  Optional, one or more datatypes to filter on (e.g.
  `"Bacterioplankton"`). If `NULL`, all datatypes are included.

- group_col:

  Optional column name in the SHARK data to group by (e.g.
  `"station_name"`). If provided, statistics will be computed separately
  for each group. Default is `NULL`.

- min_obs:

  Minimum number of numeric observations required for a parameter to be
  included (default: 3).

- max_non_numeric_frac:

  Maximum allowed fraction of non-numeric values for a parameter to be
  kept (default: 0.05).

- cache_result:

  Logical, whether to save the result in a persistent cache
  (`statistics.rds`) for use by other functions. Default is `FALSE`.

- prod:

  Logical, whether to download from the production (`TRUE`, default) or
  test (`FALSE`) SHARK server. Ignored if `utv` is `TRUE`.

- utv:

  Logical. Select UTV server when `TRUE`.

- verbose:

  Logical, whether to show download progress messages. Default is
  `TRUE`.

## Value

A tibble with one row per parameter (and optionally per group) and the
following columns:

- parameter:

  Parameter name (character).

- datatype:

  SHARK datatype (character).

- min, Q1, median, Q3, max:

  Observed quantiles.

- P01, P05, P95, P99:

  1st, 5th, 95th and 99th percentiles.

- IQR:

  Interquartile range.

- mean:

  Arithmetic mean of numeric values.

- sd:

  Standard deviation of numeric values.

- var:

  Variance of numeric values.

- cv:

  Coefficient of variation (sd / mean).

- mad:

  Median absolute deviation.

- mild_lower, mild_upper:

  Lower/upper bounds for mild outliers (1.5 × IQR).

- extreme_lower, extreme_upper:

  Lower/upper bounds for extreme outliers (3 × IQR).

- n:

  Number of numeric observations used.

- fromYear:

  First year included in the SHARK data download (numeric).

- toYear:

  Last year included in the SHARK data download (numeric).

- \<group_col\>:

  Optional grouping column if provided.

## Details

By default, the function uses the *previous five complete years*. For
example, if called in 2025 it will use data from 2020–2024.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Uses previous 5 years automatically
  res <- get_shark_statistics()

  # Explicitly set years and datatype
  res <- get_shark_statistics(2018, 2022, datatype = "Chlorophyll")

  # Group by station name and save result in persistent cache
  res <- get_shark_statistics(group_col = "station_name", cache_result = TRUE)
} # }
```
