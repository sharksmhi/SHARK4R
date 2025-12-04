# Lookup spatial data for a set of points

Retrieves spatial information (e.g., distance to shore, environmental
grids, and area data) for a set of geographic coordinates. The function
handles invalid or duplicate coordinates automatically and supports
returning results as either a data frame or a list.

## Usage

``` r
lookup_xy(
  data,
  shoredistance = TRUE,
  grids = TRUE,
  areas = FALSE,
  as_data_frame = TRUE
)
```

## Arguments

- data:

  A data frame containing geographic coordinates. The required columns
  are `sample_longitude_dd` and `sample_latitude_dd`. These must be
  numeric and within valid ranges (-180 to 180 for longitude, -90 to 90
  for latitude).

- shoredistance:

  Logical; if `TRUE` (default), the distance to the nearest shore is
  returned.

- grids:

  Logical; if `TRUE` (default), environmental grid values (e.g.,
  temperature, bathymetry) are returned.

- areas:

  Logical or numeric; if `TRUE`, area values are returned for points at
  a 0 m radius. If a positive integer is provided, all areas within that
  radius (in meters) are returned. Default is `FALSE`.

- as_data_frame:

  Logical; if `TRUE` (default), results are returned as a data frame
  with one row per input coordinate. If `FALSE`, results are returned as
  a list.

## Value

Either a data frame or a list with the requested spatial data:

- For data frame output, each row corresponds to the input coordinates.
  Columns include `shoredistance`, environmental grids, and `areas` (if
  requested). Invalid coordinates are filled with `NA`.

- For list output, each element corresponds to one input coordinate.
  Invalid coordinates are `NULL`.

## Details

- The function first cleans the coordinates, removing invalid or missing
  values and identifying unique points to avoid redundant lookups.

- Coordinates are queried in chunks of 25,000 to avoid overloading the
  OBIS web service.

- When `areas` is a positive integer, all area values within that radius
  are returned. A value of `TRUE` is equivalent to 0 m, while `FALSE`
  disables area retrieval.

- Results are mapped back to the original input order, and duplicates in
  the input are correctly handled.

- The function has been modified from the `obistools` package (Provoost
  and Bosch, 2024).

## References

Provoost P, Bosch S (2024). “obistools: Tools for data enhancement and
quality control” Ocean Biodiversity Information System.
Intergovernmental Oceanographic Commission of UNESCO. R package version
0.1.0, <https://iobis.github.io/obistools/>.

## See also

[`check_onland`](https://sharksmhi.github.io/SHARK4R/reference/check_onland.md),
[`check_depth`](https://sharksmhi.github.io/SHARK4R/reference/check_depth.md),
<https://iobis.github.io/xylookup/> – OBIS xylookup web service

## Examples

``` r
if (FALSE) { # \dontrun{
# Example data frame
data <- data.frame(sample_longitude_dd = c(10.983229, 18.265451),
                   sample_latitude_dd = c(58.121034, 58.331616))

# Retrieve shore distances and environmental grids for a dataset
xy_data <- lookup_xy(data, shoredistance = TRUE, grids = TRUE, areas = FALSE)

# Retrieve area data within a 500-meter radius
xy_areas <- lookup_xy(data, shoredistance = FALSE, grids = FALSE, areas = 500)

# Get results as a list instead of a data frame
xy_list <- lookup_xy(data, shoredistance = TRUE, grids = TRUE, areas = FALSE, as_data_frame = FALSE)
} # }

```
