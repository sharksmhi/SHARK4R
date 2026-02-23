# Identify records with zero-valued measurement data

This function scans a dataset for cases where the measurement column
(`value`) contains zero (0) values, which may indicate missing,
censored, or erroneous data. It returns either a
[`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) for easy
inspection or a plain `data.frame` of the affected rows. This function
is useful for quality control and validation prior to data aggregation,
reporting, or database submission.

## Usage

``` r
check_zero_value(data, return_df = FALSE)
```

## Arguments

- data:

  A data frame. Must contain a column named `value`.

- return_df:

  Logical. If TRUE, return a plain data.frame of problematic rows
  instead of a DT datatable. Default = FALSE.

## Value

A DT datatable or a data.frame of zero-value records, or `NULL`
(invisibly) if no zero values are found.

## Examples

``` r
# Example dataset
df <- data.frame(
  station_name = c("A", "B", "C", "D"),
  sample_date = as.Date(c("2023-06-01", "2023-06-02", "2023-06-03", "2023-06-04")),
  value = c(3.2, 0, 1.5, 0)
)

# Return a plain data.frame of zero-value records
check_zero_value(df, return_df = TRUE)
#> Warning: Value column contains zeroes (0). Please check zero values!
#>   station_name sample_date value
#> 1            B  2023-06-02     0
#> 2            D  2023-06-04     0
```
