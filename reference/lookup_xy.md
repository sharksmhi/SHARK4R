# Lookup spatial information for geographic points

Retrieves shore distance, environmental grids, and area values for given
coordinates. Coordinates may be supplied either through a data frame or
as separate numeric vectors.

## Usage

``` r
lookup_xy(
  data = NULL,
  lon = NULL,
  lat = NULL,
  shoredistance = TRUE,
  grids = TRUE,
  areas = FALSE,
  as_data_frame = TRUE
)
```

## Arguments

- data:

  Optional data frame containing coordinate columns. The expected names
  are `sample_longitude_dd` and `sample_latitude_dd`. These must be
  numeric and fall within valid geographic ranges.

- lon:

  Optional numeric vector of longitudes. Must be supplied together with
  `lat` when used. Ignored when a data frame is provided unless both
  `lon` and `lat` are set.

- lat:

  Optional numeric vector of latitudes. Must be supplied together with
  `lon` when used.

- shoredistance:

  Logical; if `TRUE`, distance to the nearest shore is included.

- grids:

  Logical; if `TRUE`, environmental grid values are included.

- areas:

  Logical or numeric. When logical, `TRUE` requests area values at zero
  radius, and `FALSE` disables area retrieval. A positive integer
  specifies the search radius in meters for area values.

- as_data_frame:

  Logical; if `TRUE`, the result is returned as a data frame. When
  `FALSE`, the result is returned as a list.

## Value

A data frame or list, depending on `as_data_frame`. Invalid coordinates
produce `NA` entries (data frame) or `NULL` elements (list). Duplicate
input coordinates return repeated results.

## Details

- When both vector inputs and a data frame are provided, the vector
  inputs take precedence.

- Coordinates are validated and cleaned before lookup, and only unique
  values are queried.

- Queries are processed in batches to avoid overloading the remote
  service.

- Area retrieval accepts either a logical flag or a radius. A radius of
  zero corresponds to requesting a single area value.

- Final results are reordered to match the original input positions.

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
# \donttest{
# Using a data frame
df <- data.frame(sample_longitude_dd = c(10.9, 18.3),
                 sample_latitude_dd = c(58.1, 58.3))
lookup_xy(df)
#>   shoredistance sssalinity sstemperature bathymetry
#> 1         25043    29.2312       10.3866      185.8
#> 2         48074     6.4531        8.9640      132.8

# Area search within a radius
lookup_xy(df, areas = 500)
#>   shoredistance sssalinity sstemperature bathymetry
#> 1         25043    29.2312       10.3866      185.8
#> 2         48074     6.4531        8.9640      132.8
#>                                        obis               lme               iho
#> 1  233, 235, Sweden: all, Sweden: North Sea  40022, North Sea  32379, Skagerrak
#> 2 233, 234, Sweden: all, Sweden: Baltic Sea 40023, Baltic Sea 32401, Baltic Sea

# Using separate coordinate vectors
lookup_xy(lon = c(10.9, 18.3), lat = c(58.1, 58.3))
#>   shoredistance sssalinity sstemperature bathymetry
#> 1         25043    29.2312       10.3866      185.8
#> 2         48074     6.4531        8.9640      132.8
# }
```
