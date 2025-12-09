# General checker for parameter-specific logical rules

This function checks for logical rule violations in benthos/epibenthos
data by applying a user-defined condition to values for a given
parameter. It is intended to replace the old family of
`check_*_*_logical()` functions.

## Usage

``` r
check_logical_parameter(
  data,
  param_name,
  condition,
  return_df = FALSE,
  return_logical = FALSE
)
```

## Arguments

- data:

  A data frame. Must contain columns `parameter` and `value`.

- param_name:

  Character; the name of the parameter to check.

- condition:

  A function that takes a numeric vector of values and returns a logical
  vector (TRUE for rows considered problematic).

- return_df:

  Logical. If TRUE, return a plain data.frame of problematic rows.

- return_logical:

  Logical. If TRUE, return a logical vector of length nrow(data).
  Overrides return_df.

## Value

A DT datatable, a data.frame, a logical vector, or NULL if no problems
found.

## Examples

``` r
# Example dataset
df <- dplyr::tibble(
  station_name = c("A1", "A2", "A3", "A4"),
  sample_date = as.Date("2023-05-01") + 0:3,
  sample_id = 101:104,
  parameter = c("Biomass", "Biomass", "Abundance", "Biomass"),
  value = c(5, -2, 10, 0)
)

# 1. Check that Biomass is never negative
check_logical_parameter(df, "Biomass", function(x) x < 0,  return_df = TRUE)
#> # A tibble: 1 × 5
#>   station_name sample_date sample_id parameter value
#>   <chr>        <date>          <int> <chr>     <dbl>
#> 1 A2           2023-05-02        102 Biomass      -2

# 2. Same check, but return problematic rows as a data frame
check_logical_parameter(df, "Biomass", function(x) x < 0, return_df = TRUE)
#> # A tibble: 1 × 5
#>   station_name sample_date sample_id parameter value
#>   <chr>        <date>          <int> <chr>     <dbl>
#> 1 A2           2023-05-02        102 Biomass      -2

# 3. Return logical vector marking problematic rows
check_logical_parameter(df, "Biomass", function(x) x < 0, return_logical = TRUE)
#> [1] FALSE  TRUE FALSE FALSE

# 4. Check that Abundance is not zero (no problems found -> returns NULL)
abundance_check <- check_logical_parameter(df, "Abundance", function(x) x == 0)
print(abundance_check)
#> NULL
```
