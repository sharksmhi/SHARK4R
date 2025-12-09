# Identify samples with zero-valued station coordinates

This function inspects a dataset containing sample coordinates to
identify potential issues where longitude or latitude values are zero
(0), which typically indicate missing or erroneous station positions.
The function can return a summary table, a filtered data frame, or a
logical vector highlighting problematic rows. It is useful as a data
quality control step before spatial analyses or database imports.

## Usage

``` r
check_zero_positions(
  data,
  coord = "longitude",
  return_df = FALSE,
  return_logical = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame. Must contain `sample_longitude_dd` and/or
  `sample_latitude_dd`.

- coord:

  Character. Which coordinate(s) to check: "longitude", "latitude", or
  "both". Default = "longitude".

- return_df:

  Logical. If TRUE, return a plain data.frame of problematic rows
  instead of a DT datatable. Default = FALSE.

- return_logical:

  Logical. If TRUE, return a logical vector of length nrow(data)
  indicating which rows have zero in the selected coordinate(s).
  Overrides return_df. Default = FALSE.

- verbose:

  Logical. If TRUE, messages will be displayed during execution.
  Defaults to TRUE.

## Value

A DT datatable, a data.frame, a logical vector, or NULL (if no problems
found and return_logical = FALSE).

## Examples

``` r
# Example data
df <- data.frame(
  station_name = c("A", "B", "C"),
  sample_longitude_dd = c(15.2, 0, 18.7),
  sample_latitude_dd = c(56.3, 58.1, 0)
)

# Check for zeroes in both coordinates and return as data.frame
check_zero_positions(df, coord = "both", return_df = TRUE)
#> ERROR: Positions contain zeroes (0). Please check station coordinates with zero values!
#>   station_name sample_longitude_dd sample_latitude_dd
#> 1            B                 0.0               58.1
#> 2            C                18.7                0.0

# Return a logical vector instead of a table
check_zero_positions(df, coord = "both", return_logical = TRUE)
#> [1] FALSE  TRUE  TRUE
```
