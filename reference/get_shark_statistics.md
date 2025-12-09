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
# \donttest{
# Uses previous 5 years automatically, Chlorophyll data only
res <- get_shark_statistics(datatype = "Chlorophyll", verbose = FALSE)
print(res)
#> # A tibble: 1 × 24
#>   parameter  datatype fromYear toYear     n   min    Q1 median    Q3   max   P01
#>   <chr>      <chr>       <dbl>  <dbl> <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1 Chlorophy… Chlorop…     2020   2024  1374  0.19  1.39    2.2   3.3  22.2 0.307
#> # ℹ 13 more variables: P05 <dbl>, P95 <dbl>, P99 <dbl>, IQR <dbl>, mean <dbl>,
#> #   sd <dbl>, var <dbl>, cv <dbl>, mad <dbl>, mild_lower <dbl>,
#> #   mild_upper <dbl>, extreme_lower <dbl>, extreme_upper <dbl>

# Group by station name and save result in persistent cache
res_station <- get_shark_statistics(datatype = "Chlorophyll",
                                    group_col = "station_name",
                                    cache_result = TRUE,
                                    verbose = FALSE)
print(res_station)
#> # A tibble: 78 × 25
#>    parameter     datatype  station_name fromYear toYear   min    Q1 median    Q3
#>    <chr>         <chr>     <chr>           <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>
#>  1 Chlorophyll-a Chloroph… A 01B            2020   2023  1.85 2.21    2.98  3.96
#>  2 Chlorophyll-a Chloroph… ANHOLT E         2020   2022  0.5  0.9     1.2   1.7 
#>  3 Chlorophyll-a Chloroph… B7               2020   2023  0.37 0.985   2.12  3.52
#>  4 Chlorophyll-a Chloroph… BCS III-10       2020   2022  0.2  1.12    2.1   3.05
#>  5 Chlorophyll-a Chloroph… BERGÖFJÄRDE…     2021   2022  1.1  1.32    1.6   1.8 
#>  6 Chlorophyll-a Chloroph… BODÖFJÄRDEN…     2021   2022  1.1  1.4     1.85  2.22
#>  7 Chlorophyll-a Chloroph… BURSFJÄRDEN…     2021   2022  2.6  2.8     3     3.2 
#>  8 Chlorophyll-a Chloroph… BY10             2020   2022  1    1.5     2.4   5.1 
#>  9 Chlorophyll-a Chloroph… BY15 GOTLAN…     2020   2022  0.4  1.52    2.5   3.08
#> 10 Chlorophyll-a Chloroph… BY2 ARKONA       2020   2022  0.6  1.5     2     2.55
#> # ℹ 68 more rows
#> # ℹ 16 more variables: max <dbl>, P01 <dbl>, P05 <dbl>, P95 <dbl>, P99 <dbl>,
#> #   IQR <dbl>, mean <dbl>, sd <dbl>, var <dbl>, cv <dbl>, mad <dbl>,
#> #   mild_lower <dbl>, mild_upper <dbl>, extreme_lower <dbl>,
#> #   extreme_upper <dbl>, n <int>
# }
```
