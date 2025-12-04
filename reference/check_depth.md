# Validate depth values against bathymetry and logical constraints

`check_depth()` inspects one or two depth columns in a dataset and
reports potential problems such as missing values, non-numeric entries,
or values that conflict with bathymetry and shoreline information. It
can also validate depths against bathymetry data retrieved from a
[terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
object or, if `bathymetry = NULL`, via the
[`lookup_xy()`](https://sharksmhi.github.io/SHARK4R/reference/lookup_xy.md)
function, which calls the OBIS XY lookup API to obtain bathymetry (using
EMODnet Bathymetry) and shore distance.

## Usage

``` r
check_depth(
  data,
  depth_cols = c("sample_min_depth_m", "sample_max_depth_m"),
  lat_col = "sample_latitude_dd",
  lon_col = "sample_longitude_dd",
  report = TRUE,
  depthmargin = 0,
  shoremargin = NA,
  bathymetry = NULL
)
```

## Arguments

- data:

  A data frame containing sample metadata, including longitude,
  latitude, and one or two depth columns.

- depth_cols:

  Character vector naming the depth column(s). Can be one column (e.g.,
  `"water_depth_m"`) or two columns (minimum and maximum depth, e.g.,
  `c("sample_min_depth_m", "sample_max_depth_m")`).

- lat_col:

  Name of the column containing latitude values. Default:
  `"sample_latitude_dd"`.

- lon_col:

  Name of the column containing longitude values. Default:
  `"sample_longitude_dd"`.

- report:

  Logical. If `TRUE` (default), returns a tibble of detected problems.
  If `FALSE`, returns the subset of input rows that failed validation.

- depthmargin:

  Numeric. Allowed deviation (in meters) above bathymetry before a depth
  is flagged as an error. Default = `0`.

- shoremargin:

  Numeric. Minimum offshore distance (in meters) required for negative
  depths to be considered valid. If `NA` (default), this check is
  skipped.

- bathymetry:

  Optional
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object with one layer giving bathymetry values. If `NULL` (default),
  bathymetry and shore distance are retrieved using
  [`lookup_xy()`](https://sharksmhi.github.io/SHARK4R/reference/lookup_xy.md),
  which calls the OBIS XY lookup API.

## Value

A tibble with one row per detected problem, containing:

- level:

  Severity of the issue ("warning" or "error").

- row:

  Row index in the input data where the issue occurred.

- field:

  Name of the column(s) involved.

- message:

  Human-readable description of the problem.

If `report = FALSE`, returns the subset of input rows that failed any
check.

## Details

The following checks are performed:

1.  **Missing depth column** → warning

2.  **Empty depth column** (all values missing) → warning

3.  **Non-numeric depth values** → error

4.  **Depth exceeds bathymetry + margin** (`depthmargin`) → error

5.  **Negative depth at offshore locations** (beyond `shoremargin`) →
    error

6.  **Minimum depth greater than maximum depth** (if two columns
    supplied) → error

7.  **Longitude/latitude outside raster bounds** → warning

8.  **Missing bathymetry value** at coordinate → warning

The function has been modified from the `obistools` package (Provoost
and Bosch, 2024).

## References

Provoost P, Bosch S (2024). “obistools: Tools for data enhancement and
quality control” Ocean Biodiversity Information System.
Intergovernmental Oceanographic Commission of UNESCO. R package version
0.1.0, <https://iobis.github.io/obistools/>.

## See also

[`lookup_xy`](https://sharksmhi.github.io/SHARK4R/reference/lookup_xy.md),
[`check_onland`](https://sharksmhi.github.io/SHARK4R/reference/check_onland.md)

## Examples

``` r
# Example dataset with one depth column
example_data <- data.frame(
  sample_latitude_dd = c(59.3, 58.1, 57.5),
  sample_longitude_dd = c(18.0, 17.5, 16.2),
  sample_depth_m = c(10, -5, NA)
)

# Validate depths using OBIS XY lookup (bathymetry = NULL)
if (FALSE) { # \dontrun{
check_depth(example_data, depth_cols = "sample_depth_m")
} # }

# Example dataset with min/max depth columns
example_data2 <- data.frame(
  sample_latitude_dd = c(59.0, 58.5),
  sample_longitude_dd = c(18.0, 17.5),
  sample_min_depth_m = c(5, 15),
  sample_max_depth_m = c(3, 20)
)

if (FALSE) { # \dontrun{
check_depth(example_data2, depth_cols = c("sample_min_depth_m", "sample_max_depth_m"))
} # }

# Return only failing rows
if (FALSE) { # \dontrun{
check_depth(example_data, depth_cols = "sample_depth_m", report = FALSE)
} # }
```
