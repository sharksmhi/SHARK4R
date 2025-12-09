# Check if stations are reported as nominal positions

**\[deprecated\]** This function is deprecated and has been replaced by
[`check_nominal_station()`](https://sharksmhi.github.io/SHARK4R/reference/check_nominal_station.md).

## Usage

``` r
nominal_station(data)
```

## Arguments

- data:

  A data frame containing at least the columns: `sample_date`,
  `station_name`, `sample_longitude_dd`, and `sample_latitude_dd`.

## Value

A data frame with distinct station names and their corresponding
latitude/longitude positions, if nominal positions are suspected.
Otherwise, returns `NULL`.

## Details

This function attempts to determine whether stations in a dataset are
reported using nominal positions (i.e., generic or repeated coordinates
across events), rather than actual measured coordinates. It compares the
number of unique sampling dates with the number of unique station
coordinates.

If the number of unique sampling dates is larger than the number of
unique station coordinates, the function suspects nominal station
positions and issues a warning.

## Examples

``` r
# \donttest{
df <- data.frame(
  sample_date = rep(seq.Date(Sys.Date(), by = "day", length.out = 3), each = 2),
  station_name = rep(c("ST1", "ST2"), 3),
  sample_longitude_dd = rep(c(15.0, 16.0), 3),
  sample_latitude_dd = rep(c(58.5, 58.6), 3)
)
nominal_station(df)
#> Warning: `nominal_station()` was deprecated in SHARK4R 1.0.0.
#> ℹ Please use `check_nominal_station()` instead.
#> WARNING: Suspected nominal positions reported! Is this correct?
#>   STATION LON  LAT
#> 1     ST1  15 58.5
#> 2     ST2  16 58.6
# }
```
