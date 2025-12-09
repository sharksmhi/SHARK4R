# Retrieve tabular data from SHARK

The `get_shark_data()` function retrieves tabular data from the SHARK
database hosted by SMHI. The function sends a POST request to the SHARK
API with customizable filters, including year, month, taxon name, water
category, and more, and returns the retrieved data as a structured
`tibble`. To view available filter options, see
[`get_shark_options`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_options.md).

## Usage

``` r
get_shark_data(
  tableView = "sharkweb_overview",
  headerLang = "internal_key",
  save_data = FALSE,
  file_path = NULL,
  delimiters = "point-tab",
  lineEnd = "win",
  encoding = "utf_8",
  dataTypes = c(),
  bounds = c(),
  fromYear = NULL,
  toYear = NULL,
  months = c(),
  parameters = c(),
  checkStatus = "",
  qualityFlags = c(),
  deliverers = c(),
  orderers = c(),
  projects = c(),
  datasets = c(),
  minSamplingDepth = "",
  maxSamplingDepth = "",
  redListedCategory = c(),
  taxonName = c(),
  stationName = c(),
  vattenDistrikt = c(),
  seaBasins = c(),
  counties = c(),
  municipalities = c(),
  waterCategories = c(),
  typOmraden = c(),
  helcomOspar = c(),
  seaAreas = c(),
  hideEmptyColumns = FALSE,
  row_limit = 10^7,
  prod = TRUE,
  utv = FALSE,
  verbose = TRUE
)
```

## Arguments

- tableView:

  Character. Specifies the columns of the table to retrieve. Options
  include:

  - `"sharkweb_overview"`: Overview table

  - `"sharkweb_all"`: All available columns

  - `"sharkdata_bacterioplankton"`: Bacterioplankton table

  - `"sharkdata_chlorophyll"`: Chlorophyll table

  - `"sharkdata_epibenthos"`: Epibenthos table

  - `"sharkdata_greyseal"`: Greyseal table

  - `"sharkdata_harbourporpoise"`: Harbour porpoise table

  - `"sharkdata_harbourseal"`: Harbour seal table

  - `"sharkdata_jellyfish"`: Jellyfish table

  - `"sharkdata_physicalchemical"`: Physical chemical table

  - `"sharkdata_physicalchemical_columns"`: Physical chemical table:
    column view

  - `"sharkdata_phytoplankton"`: Phytoplankton table

  - `"sharkdata_picoplankton"`: Picoplankton table

  - `"sharkdata_planktonbarcoding"`: Plankton barcoding table

  - `"sharkdata_primaryproduction"`: Primary production table

  - `"sharkdata_ringedseal"`: Ringed seal table

  - `"sharkdata_sealpathology"`: Seal pathology table

  - `"sharkdata_sedimentation"`: Sedimentation table

  - `"sharkdata_zoobenthos"`: Zoobenthos table

  - `"sharkdata_zooplankton"`: Zooplankton table

  - `"report_sum_year_param"`: Report sum per year and parameter

  - `"report_sum_year_param_taxon"`: Report sum per year, parameter and
    taxon

  - `"report_sampling_per_station"`: Report sampling per station

  - `"report_obs_taxon"`: Report observed taxa

  - `"report_stations"`: Report stations

  - `"report_taxon"`: Report taxa

  Default is `"sharkweb_overview"`.

- headerLang:

  Character. Language option for column headers. Possible values:

  - `"sv"`: Swedish.

  - `"en"`: English.

  - `"short"`: Shortened version.

  - `"internal_key"`: Internal key (default).

- save_data:

  Logical. If `TRUE`, the downloaded data is written to `file_path` on
  disk. If `FALSE` (default), data is temporarily written to a file and
  then read into memory as a `data.frame`, after which the temporary
  file is deleted.

- file_path:

  Character. The file path where the data should be saved. Required if
  `save_data` is TRUE. Ignored if `save_data` is FALSE.

- delimiters:

  Character. Specifies the delimiter used to separate values in the
  file, if `save_data` is TRUE. Options are `"point-tab"`
  (tab-separated) or `"point-semi"` (semicolon-separated). Default is
  `"point-tab"`.

- lineEnd:

  Character. Defines the type of line endings in the file, if
  `save_data` is TRUE. Options are `"win"` (Windows-style, `\r\n`) or
  `"unix"` (Unix-style, `\n`). Default is `"win"`.

- encoding:

  Character. Sets the file's text encoding, if `save_data` is TRUE.
  Options are `"cp1252"`, `"utf_8"`, `"utf_16"`, or `"latin_1"`. Default
  is `"utf_8"`.

- dataTypes:

  Character vector. Specifies data types to filter. Possible values
  include:

  - "Bacterioplankton"

  - "Chlorophyll"

  - "Epibenthos"

  - "Grey seal"

  - "Harbour Porpoise"

  - "Harbour seal"

  - "Jellyfish"

  - "Physical and Chemical"

  - "Phytoplankton"

  - "Picoplankton"

  - "PlanktonBarcoding"

  - "Primary production"

  - "Profile"

  - "Ringed seal"

  - "Seal pathology"

  - "Sedimentation"

  - "Zoobenthos"

  - "Zooplankton"

- bounds:

  A numeric vector of length 4 specifying the geographical search
  boundaries in decimal degrees, formatted as
  `c(lon_min, lat_min, lon_max, lat_max)`, e.g., `c(11, 58, 12, 59)`.
  Default is [`c()`](https://rdrr.io/r/base/c.html) to include all data.

- fromYear:

  Integer (optional). The starting year for data retrieval. If set to
  `NULL` (default), the function will use the earliest available year in
  SHARK.

- toYear:

  Integer (optional). The ending year for data retrieval. If set to
  `NULL` (default), the function will use the latest available year in
  SHARK.

- months:

  Integer vector. The months to retrieve data for, e.g., `c(4, 5, 6)`
  for April to June.

- parameters:

  Character vector. Optional parameters to filter the results by, such
  as `"Chlorophyll-a"`.

- checkStatus:

  Character string. Optional status check to filter results.

- qualityFlags:

  Character vector. Specifies the quality flags to filter the data. By
  default, all data are included, including those with the "B" flag
  (Bad).

- deliverers:

  Character vector. Specifies the data deliverers to filter by.

- orderers:

  Character vector. Orderers to filter by specific organizations or
  individuals.

- projects:

  Character vector. Projects to filter data by specific research or
  monitoring projects.

- datasets:

  Character vector. Datasets to filter data by specific datasets.

- minSamplingDepth:

  Numeric. Minimum sampling depth (in meters) to filter the data.

- maxSamplingDepth:

  Numeric. Maximum sampling depth (in meters) to filter the data.

- redListedCategory:

  Character vector. Red-listed taxa for conservation filtering.

- taxonName:

  Character vector. Optional vector of taxa names to filter by.

- stationName:

  Character vector. Station names to filter data by specific stations.

- vattenDistrikt:

  Character vector. Water district names to filter by Swedish water
  districts.

- seaBasins:

  Character vector. Sea basins to filter by.

- counties:

  Character vector. Counties to filter by specific administrative
  regions.

- municipalities:

  Character vector. Municipalities to filter by.

- waterCategories:

  Character vector. Water categories to filter by.

- typOmraden:

  Character vector. Type areas to filter by.

- helcomOspar:

  Character vector. HELCOM or OSPAR areas for regional filtering.

- seaAreas:

  Character vector. Sea area codes to filter by specific sea areas.

- hideEmptyColumns:

  Logical. Whether to hide empty columns. Default is FALSE.

- row_limit:

  Numeric. Specifies the maximum number of rows that can be retrieved in
  a single request. If the requested data exceeds this limit, the
  function automatically downloads the data in yearly chunks (ignored
  when `tableView = "report_*"`). The default value is 10 million rows.

- prod:

  Logical, whether to download from the production (`TRUE`, default) or
  test (`FALSE`) SHARK server. Ignored if `utv` is `TRUE`.

- utv:

  Logical. Select UTV server when `TRUE`.

- verbose:

  Logical. Whether to display progress information. Default is TRUE.

## Value

A `tibble` containing the retrieved SHARK data, parsed from the API's
delimited text response. Column types are inferred automatically.

## Details

This function sends a POST request to the SHARK API with the specified
filters. The API returns a delimited text file (e.g., tab- or
semicolon-separated), which is downloaded and read into R as a `tibble`.
If the `row_limit` parameter is exceeded, the data is retrieved in
yearly chunks and combined into a single table. Adjusting the
`row_limit` parameter may be necessary when retrieving large datasets or
detailed reports. Note that making very large requests (e.g., retrieving
the entire SHARK database) can be extremely time- and memory-intensive.

## Note

For large queries spanning multiple years or including several data
types, retrieval can be time-consuming and memory-intensive. Consider
filtering by year, data type, or region for improved performance.

## See also

- <https://shark.smhi.se/en> – SHARK database portal

- [`get_shark_options()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_options.md)
  – Retrieve available filters

- [`get_shark_table_counts()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_table_counts.md)
  – Check table row counts before download

- [`get_shark_datasets()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_datasets.md)
  – To download datasets as zip-archives

## Examples

``` r
# \donttest{
  # Retrieve chlorophyll data from 2019 to 2020 for April to June
  shark_data <- get_shark_data(fromYear = 2019, toYear = 2020,
                               months = c(4, 5, 6), dataTypes = "Chlorophyll",
                               verbose = FALSE)
  print(shark_data)
#> # A tibble: 179 × 72
#>    delivery_datatype check_status_sv data_checked_by_sv visit_year visit_month
#>    <chr>             <chr>           <chr>                   <dbl>       <dbl>
#>  1 Chlorophyll       Klar            Leverantör               2020           6
#>  2 Chlorophyll       Klar            Leverantör               2020           6
#>  3 Chlorophyll       Klar            Leverantör               2020           6
#>  4 Chlorophyll       Klar            Leverantör               2020           6
#>  5 Chlorophyll       Klar            Leverantör               2020           6
#>  6 Chlorophyll       Klar            Leverantör               2020           6
#>  7 Chlorophyll       Klar            Leverantör               2020           6
#>  8 Chlorophyll       Klar            Leverantör               2020           6
#>  9 Chlorophyll       Klar            Leverantör               2020           6
#> 10 Chlorophyll       Klar            Leverantör               2020           6
#> # ℹ 169 more rows
#> # ℹ 67 more variables: station_name <chr>, reported_station_name <chr>,
#> #   sample_location_id <dbl>, station_id <dbl>, sample_project_name_sv <lgl>,
#> #   sample_orderer_name_sv <lgl>, visit_id <dbl>, visit_date <lgl>,
#> #   shark_sample_id_md5 <chr>, sample_date <date>, sample_time <time>,
#> #   sample_enddate <lgl>, sample_endtime <lgl>, sample_latitude_dm <chr>,
#> #   sample_longitude_dm <chr>, sample_latitude_dd <dbl>, …
# }
```
