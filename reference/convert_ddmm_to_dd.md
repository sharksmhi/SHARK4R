# Convert coordinates from DDMM format to decimal degrees

This function converts geographic coordinates provided in the DDMM
format (degrees and minutes) to decimal degrees. It can handle:

- DDMM (e.g., 5733 to 57°33' to 57.55°)

- DDMMss or DDMMss… (extra digits after minutes are interpreted as
  fractional minutes, e.g., 573345 to 57°33.45' to 57.5575°)

## Usage

``` r
convert_ddmm_to_dd(coord)
```

## Arguments

- coord:

  A numeric or character vector of coordinates in DDMM format.

## Value

A numeric vector of decimal degrees corresponding to the input
coordinates. Names from the input vector are removed.

## Details

Non-numeric characters are removed before conversion. Coordinates
shorter than 4 digits are returned as `NA`.

## Examples

``` r
# Basic DDMM input
convert_ddmm_to_dd(c(5733, 6045))
#> [1] 57.55 60.75
# Input with fractional minutes
convert_ddmm_to_dd(c("573345", "604523"))
#> [1] 57.55750 60.75383
# Input with non-numeric characters
convert_ddmm_to_dd(c("57°33'", "60°45'23\""))
#> [1] 57.55000 60.75383
```
