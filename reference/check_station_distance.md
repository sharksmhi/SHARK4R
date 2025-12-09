# Check station distances against SMHI station list

Matches reported station names against the SMHI curated station list
(`"station.txt"`) and checks whether matched stations fall within
pre-defined distance limits. This helps ensure that station assignments
are spatially consistent.

## Usage

``` r
check_station_distance(
  data,
  station_file = NULL,
  plot_leaflet = FALSE,
  try_synonyms = TRUE,
  fallback_crs = 4326,
  only_bad = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame containing at least the columns: `station_name`,
  `sample_longitude_dd`, `sample_latitude_dd`.

- station_file:

  Optional path to a custom station file (tab-delimited). If `NULL`
  (default), the function will first attempt to use the `NODC_CONFIG`
  environment variable, and if that fails, will use the bundled
  `"station.zip"` from the `SHARK4R` package.

- plot_leaflet:

  Logical; if `TRUE`, displays a leaflet map with SMHI stations (blue
  circles with radius in popup) and reported stations (green/red/gray
  markers). Default is `FALSE`.

- try_synonyms:

  Logical; if `TRUE` (default), unmatched station names are also
  compared against the `SYNONYM_NAMES` column in the station database.

- fallback_crs:

  Integer; CRS (EPSG code) to use when creating spatial points if no CRS
  is available. Defaults to `4326` (WGS84). Change this if your
  coordinates are reported in another CRS (e.g., `3006` for SWEREF99
  TM).

- only_bad:

  Logical; if `TRUE`, the leaflet map will only display stations that
  are outside the allowed radius (red markers). Default is `FALSE`.

- verbose:

  Logical. If TRUE, messages will be displayed during execution.
  Defaults to TRUE.

## Value

If `plot_leaflet = FALSE`, returns a data frame with columns:

- station_name:

  Reported station name.

- match_type:

  `TRUE` if station matched in SMHI list, `FALSE` otherwise.

- distance_m:

  Distance in meters from reported station to matched SMHI station.

- within_limit:

  `TRUE` if distance \<= allowed radius, `FALSE` if outside, `NA` if
  unmatched.

If `plot_leaflet = TRUE`, the function produces a leaflet map showing:

- Blue circles for SMHI stations with radius in the popup.

- Reported stations colored by status: green (within radius), red
  (outside radius), gray (unmatched).

- If `only_bad = TRUE`, only the red stations (outside radius) are
  displayed.

## Details

Optionally, a leaflet map of stations can be plotted. SMHI stations that
match the reported data are shown as blue circles, with their allowed
radius visualized and displayed in the popup (e.g., "ST1 (Radius: 1000
m)"). Reported stations are shown as markers colored by whether they
fall within the radius (green), outside the radius (red), or unmatched
(gray).

If `try_synonyms = TRUE`, the function will attempt a second match using
the `SYNONYM_NAMES` column in the station database, splitting multiple
synonyms separated by `<or>`.

The function first checks if a station file path is provided via the
`station_file` argument. If not, it looks for the `NODC_CONFIG`
environment variable. This variable can point to a folder where the NODC
(Swedish National Oceanographic Data Center) configuration and station
file are stored, typically including:

- `<NODC_CONFIG>/config/station.txt`

If `NODC_CONFIG` is set and the folder exists, the function will use
`station.txt` from that location. Otherwise, it falls back to the
bundled `station.zip` included in the `SHARK4R` package.

## Examples

``` r
# Example data
df <- data.frame(
  station_name = c("ANHOLT E", "BY5 BORNHOLMSDJ", "NEW STATION"),
  sample_longitude_dd = c(12.1, 15.97, 17.5),
  sample_latitude_dd  = c(56.7, 55.25, 58.7)
)

# Check station distance
check_station_distance(df, try_synonyms = TRUE, verbose = FALSE)
#>      station_name sample_longitude_dd sample_latitude_dd distance_m
#> 1        ANHOLT E               12.10              56.70  3551.0004
#> 2 BY5 BORNHOLMSDJ               15.97              55.25   898.1066
#> 3     NEW STATION               17.50              58.70         NA
#>   within_limit
#> 1        FALSE
#> 2         TRUE
#> 3           NA

# Plot bad points in leaflet map
map <- check_station_distance(df,
                              plot_leaflet = TRUE,
                              only_bad = TRUE,
                              verbose = FALSE)
```
