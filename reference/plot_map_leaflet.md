# Create an interactive Leaflet map of sampling stations

Generates an interactive map using the `leaflet` package, plotting
sampling stations from a data frame. The function automatically detects
column names for station, longitude, and latitude, supporting both
standard and delivery-style datasets.

## Usage

``` r
plot_map_leaflet(data, provider = "CartoDB.Positron")
```

## Arguments

- data:

  A data frame containing station coordinates and names. The function
  accepts either:

  - Standard format: `station_name`, `sample_longitude_dd`,
    `sample_latitude_dd`

  - Delivery format: `STATN`, `LONGI`, `LATIT`

- provider:

  Character. The tile provider to use for the map background. See
  available providers at
  <https://leaflet-extras.github.io/leaflet-providers/preview/>.
  Defaults to `"CartoDB.Positron"`.

## Value

An HTML widget object (`leaflet` map) that can be printed or displayed
in R Markdown or Shiny applications.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  station_name = c("Station A", "Station B"),
  sample_longitude_dd = c(10.0, 10.5),
  sample_latitude_dd = c(59.0, 59.5)
)
plot_map_leaflet(df)

df_deliv <- data.frame(
  STATN = c("Station A", "Station B"),
  LONGI = c(10.0, 10.5),
  LATIT = c(59.0, 59.5)
)
plot_map_leaflet(df_deliv)
} # }
```
