# Identify non-numeric or non-logical values in measurement data

This function checks whether entries in the `value` column of a dataset
are valid numeric or logical values. It is particularly useful for
identifying common data entry errors such as inequality symbols (`<`,
`>`) or unintended text strings (e.g., "NA", "below detection"). The
function reports any invalid entries in an interactive
[`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) for easy
inspection.

## Usage

``` r
check_value_logical(data, return_df = FALSE)
```

## Arguments

- data:

  A data frame. Must contain a column named `value`.

- return_df:

  Logical. If TRUE, return a plain data.frame of problematic rows
  instead of a DT datatable. Default = FALSE.

## Value

A [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) or data
frame listing unique invalid entries, or `NULL` (invisibly) if all
values are correctly formatted as numeric or logical.

## Examples

``` r
# Example dataset with mixed valid and invalid values
df <- data.frame(
  station_name = c("A", "B", "C", "D", "E"),
  value = c("3.4", "<0.2", "TRUE", "NA", "5e-3")
)

# Check for invalid (non-numeric / non-logical) entries
check_value_logical(df, return_df = TRUE)
#> ERROR: Expected numerical/logical value but found invalid characters.
#> Common problems are e.g. '<', '>' signs, text labels, or malformed numbers.
#>   value
#> 1  <0.2
#> 2    NA

# Example with all valid numeric and logical values
df_valid <- data.frame(value = c(1.2, 0, TRUE, FALSE, 3.5))
check_value_logical(df_valid)
#> All values are correctly formatted as numeric or logical.
```
