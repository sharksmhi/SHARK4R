# Check if stations are reported as nominal positions

This function attempts to determine whether stations in a dataset are
reported using nominal positions (i.e., generic or repeated coordinates
across events), rather than actual measured coordinates.

## Usage

``` r
check_nominal_station(data, verbose = TRUE)
```

## Arguments

- data:

  A data frame containing at least the columns: `sample_date`,
  `station_name`, `sample_longitude_dd`, and `sample_latitude_dd`.

- verbose:

  Logical. If TRUE, messages will be displayed during execution.
  Defaults to TRUE.

## Value

A data frame with distinct station names and their corresponding
latitude/longitude positions, if nominal positions are suspected.
Otherwise, returns `NULL`.

## Details

The function compares the number of unique sampling dates with the
number of unique station coordinates.

If the number of unique sampling dates is larger than the number of
unique station coordinates, the function suspects nominal station
positions and issues a warning.

## Examples

``` r
df <- data.frame(
  sample_date = rep(seq.Date(Sys.Date(), by = "day", length.out = 3), each = 2),
  station_name = rep(c("ST1", "ST2"), 3),
  sample_longitude_dd = rep(c(15.0, 16.0), 3),
  sample_latitude_dd = rep(c(58.5, 58.6), 3)
)
check_nominal_station(df)
#> WARNING: Suspected nominal positions reported! Is this correct?
#>   STATION LON  LAT
#> 1     ST1  15 58.5
#> 2     ST2  16 58.6
```
