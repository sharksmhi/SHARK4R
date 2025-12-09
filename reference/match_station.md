# Match station names against SMHI station list

Matches reported station names in your dataset against a curated station
list (`"station.txt"`), which is synced with "Stationsregistret":
<https://stationsregister.miljodatasamverkan.se/>.

## Usage

``` r
match_station(names, station_file = NULL, try_synonyms = TRUE, verbose = TRUE)
```

## Arguments

- names:

  Character vector of station names to match.

- station_file:

  Optional path to a custom station file (tab-delimited). If `NULL`
  (default), the function will first attempt to use the `NODC_CONFIG`
  environment variable, and if that fails, will use the bundled
  `"station.zip"` from the `SHARK4R` package.

- try_synonyms:

  Logical; if `TRUE` (default), unmatched names are also compared
  against the `SYNONYM_NAMES` column in the database.

- verbose:

  Logical. If TRUE, messages will be displayed during execution.
  Defaults to TRUE.

## Value

A data frame with two columns:

- reported_station_name:

  The input station names.

- match_type:

  Logical; `TRUE` if the station was found in the SMHI station list
  (including synonyms if enabled), otherwise `FALSE`.

## Details

This function is useful for validating station names and identifying any
unmatched or misspelled entries.

If `try_synonyms = TRUE`, unmatched station names are also compared
against the `SYNONYM_NAMES` column in the station database, splitting
multiple synonyms separated by `<or>`.

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
# Example stations
stations <- c("ANHOLT E", "BY5 BORNHOLMSDJ", "STX999")

# Check if stations names are in stations.txt (including synonyms)
match_station(stations, try_synonyms = TRUE, verbose = FALSE)
#>   reported_station_name match_type
#> 1              ANHOLT E       TRUE
#> 2       BY5 BORNHOLMSDJ       TRUE
#> 3                STX999      FALSE
```
