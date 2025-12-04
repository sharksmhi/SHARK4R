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
check_value_logical(data)
```

## Arguments

- data:

  A data frame. Must contain a column named `value`.

## Value

A [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) listing
unique invalid entries, or `NULL` (invisibly) if all values are
correctly formatted as numeric or logical.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example dataset with mixed valid and invalid values
df <- data.frame(
  station_name = c("A", "B", "C", "D", "E"),
  value = c("3.4", "<0.2", "TRUE", "NA", "5e-3")
)

# Check for invalid (non-numeric / non-logical) entries
check_value_logical(df)

# Example with all valid numeric and logical values
df_valid <- data.frame(value = c(1.2, 0, TRUE, FALSE, 3.5))
check_value_logical(df_valid)
} # }
```
