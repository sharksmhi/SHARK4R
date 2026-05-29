# Determine if positions are near land

This function is a **wrapper/re-export** of
[`iRfcb::ifcb_is_near_land()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_is_near_land.html).
The `iRfcb` package is only required if you want to actually call this
function.

Determines whether given positions are near land based on a land polygon
shape file.

## Usage

``` r
positions_are_near_land(
  latitudes,
  longitudes,
  distance = 500,
  shape = NULL,
  source = "obis",
  crs = 4326,
  remove_small_islands = TRUE,
  small_island_threshold = 2e+06,
  plot = FALSE,
  verbose = TRUE
)
```

## Arguments

- latitudes:

  Numeric vector of latitudes for positions.

- longitudes:

  Numeric vector of longitudes for positions. Must be the same length as
  `latitudes`.

- distance:

  Buffer distance (in meters) from the coastline to consider "near
  land." Default is 500 meters.

- shape:

  Optional path to a shapefile (`.shp`) containing coastline data. If
  provided, this file will be used instead of the default OBIS land
  vectors. A high-resolution shapefile can improve the accuracy of
  buffer distance calculations. You can retrieve a more detailed
  European coastline by setting the `source` argument to `"eea"`.
  Downloaded shape files are cached across R sessions in a user-specific
  cache directory.

- source:

  Character string indicating which default coastline source to use when
  `shape = NULL`. Options are `"obis"` (Ocean Biodiversity Information
  System, default), `"ne"` (Natural Earth 1:10 vectors) and `"eea"`
  (European Environment Agency). Ignored if `shape` is provided.

- crs:

  Coordinate reference system (CRS) to use for input and output. Default
  is EPSG code 4326 (WGS84).

- remove_small_islands:

  Logical indicating whether to remove small islands from the coastline.
  Useful in archipelagos. Default is `TRUE`.

- small_island_threshold:

  Area threshold in square meters below which islands will be considered
  small and removed, if remove_small_islands is set to `TRUE`. Default
  is 2 square km.

- plot:

  A boolean indicating whether to plot the points, land polygon and
  buffer. Default is `FALSE`.

- verbose:

  A logical indicating whether to print progress messages. Default is
  TRUE.

## Value

If `plot = FALSE` (default), a logical vector is returned indicating
whether each position is near land or not, with `NA` for positions where
coordinates are missing. If `plot = TRUE`, a `ggplot` object is returned
showing the land polygon, buffer area, and position points colored by
their proximity to land.

## Details

This function calculates a buffered area around the coastline using a
polygon shapefile and determines if each input position intersects with
this buffer or the landmass itself. By default, it uses the OBIS land
vector dataset.

Coastline sources used when `shape = NULL`:

- `"obis"` - the land polygon distributed by the Ocean Biodiversity
  Information System, downloaded from
  <https://obis-resources.s3.amazonaws.com/land.gpkg>. The first call
  downloads and caches the file under
  [`clean_shark4r_cache()`](https://sharksmhi.github.io/SHARK4R/reference/clean_shark4r_cache.md).

- `"ne"` - Natural Earth 1:10m coastline / land vectors, provided via
  the `rnaturalearth` package; see <https://www.naturalearthdata.com>.

- `"eea"` - high-resolution European coastline from the European
  Environment Agency (EEA Coastline 2017). Downloaded chunked from the
  EEA arcgis service the first time and cached locally. Dataset
  metadata:
  <https://sdi.eea.europa.eu/catalogue/datahub/api/records/9faa6ea1-372a-4826-a3c7-fb5b05e31c52/formatters/xsl-view?output=pdf&language=eng&approved=true>.

## See also

[`clean_shark4r_cache()`](https://sharksmhi.github.io/SHARK4R/reference/clean_shark4r_cache.md)
to manually clear cached shape files.

[`iRfcb::ifcb_is_near_land`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_is_near_land.html)
for the original function.

## Examples

``` r
# \donttest{
# Define coordinates
latitudes <- c(62.500353, 58.964498, 57.638725, 56.575338)
longitudes <- c(17.845993, 20.394418, 18.284523, 16.227174)

# Call the function
try(near_land <- positions_are_near_land(latitudes, longitudes, distance = 300, crs = 4326))
#> Downloading OBIS coastline data...

# Print the result
if (exists("near_land")) print(near_land)
#> [1]  TRUE FALSE  TRUE  TRUE
# }
```
