# Check whether points are located on land

Identifies records whose coordinates fall on land, optionally applying a
buffer to allow points near the coast. The function supports both
offline and online modes:

## Usage

``` r
check_onland(
  data,
  land = NULL,
  report = FALSE,
  buffer = 0,
  offline = FALSE,
  plot_leaflet = FALSE,
  only_bad = FALSE
)
```

## Arguments

- data:

  A data frame containing at least `sample_longitude_dd` and
  `sample_latitude_dd`. These columns must be numeric and within valid
  ranges (-180 to 180 for longitude, -90 to 90 for latitude).

- land:

  Optional `sf` object containing land polygons. Used only in offline
  mode.

- report:

  Logical; if `TRUE`, returns a tibble listing rows on land and
  warnings. If `FALSE` (default), returns a subset of `data` containing
  only records on land.

- buffer:

  Numeric; distance in meters inland for which points are still
  considered valid. Only used in online mode. Default is 0.

- offline:

  Logical; if `TRUE`, the function uses the local cached shoreline. If
  `FALSE` (default), the OBIS web service is queried.

- plot_leaflet:

  Logical; if `TRUE`, returns a leaflet map showing points colored by
  whether they are on land (red) or in water (green). Default is
  `FALSE`.

- only_bad:

  Logical; if `TRUE` and `plot_leaflet = TRUE`, only points on land
  (red) are plotted. Default is `FALSE`, meaning all points are plotted.

## Value

If `report = TRUE`, a tibble with columns:

- `field`: always `NA` (placeholder for future extension)

- `level`: `"warning"` for all flagged rows

- `row`: row numbers in `data` flagged as located on land

- `message`: description of the issue

If `report = FALSE` and `plot_leaflet = FALSE`, returns a subset of
`data` with only the flagged rows. If `plot_leaflet = TRUE`, returns a
leaflet map showing points on land (red) and in water (green), unless
`only_bad = TRUE`, in which case only red points are plotted.

## Details

- **Offline mode (`offline = TRUE`)**: uses a local simplified shoreline
  from a cached geopackage (`land.gpkg`). If the file does not exist, it
  is downloaded automatically and cached across R sessions.

- **Online mode (`offline = FALSE`)**: uses the OBIS web service to
  determine distance to the shore.

Optionally, a leaflet map can be plotted. Points on land are displayed
as red markers, while points in water are green. If `only_bad = TRUE`,
only the red points (on land) are plotted.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example data frame with coordinates
example_data <- data.frame(
  sample_latitude_dd = c(59.3, 58.1, 57.5),
  sample_longitude_dd = c(18.6, 17.5, 16.7)
)

# Report points on land with a 100 m buffer
report <- check_onland(example_data, report = TRUE, buffer = 100)
print(report)

# Plot all points colored by land/water
m <- check_onland(example_data, plot_leaflet = TRUE)
m

# Plot only bad points on land
m_bad <- check_onland(example_data, plot_leaflet = TRUE, only_bad = TRUE)
m_bad

# Remove points on land by adding a buffer of 2000 m
ok <- check_onland(example_data, report = FALSE, buffer = 2000)
print(nrow(ok))
} # }
```
