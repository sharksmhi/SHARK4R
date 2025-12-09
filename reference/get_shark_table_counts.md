# Retrieve SHARK data table row counts

The `get_shark_table_counts()` function retrieves the number of records
(row counts) from various SHARK data tables based on specified filters
such as year, months, data type, stations, and taxa. To view available
filter options, see
[`get_shark_options`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_options.md).

## Usage

``` r
get_shark_table_counts(
  tableView = "sharkweb_overview",
  fromYear = 2019,
  toYear = 2020,
  months = c(),
  dataTypes = c(),
  parameters = c(),
  orderers = c(),
  qualityFlags = c(),
  deliverers = c(),
  projects = c(),
  datasets = c(),
  minSamplingDepth = "",
  maxSamplingDepth = "",
  checkStatus = "",
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
  prod = TRUE,
  utv = FALSE
)
```

## Arguments

- tableView:

  Character. Specifies the view of the table to retrieve. Options
  include:

  - `"sharkweb_overview"`: Overview table

  - `"sharkweb_all"`: All available columns

  - `"sharkdata_bacterioplankton"`: Bacterioplankton table

  - `"sharkdata_chlorophyll"`: Chlorophyll table

  - `"sharkdata_epibenthos"`: Epibenthos table

  - `"sharkdata_greyseal"`: Greyseal table

  - `"sharkdata_harbourporpoise"`: Harbour porpoise table

  - `"sharkdata_harbourseal`: Harbour seal table

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

- fromYear:

  Integer. The starting year for the data to retrieve. Default is
  `2019`.

- toYear:

  Integer. The ending year for the data to retrieve. Default is `2020`.

- months:

  Integer vector. The months to retrieve data for (e.g., `c(4, 5, 6)`
  for April to June).

- dataTypes:

  Character vector. Specifies data types to filter, such as
  `"Chlorophyll"` or `"Epibenthos"`.

- parameters:

  Character vector. Optional. Parameters to filter results, such as
  `"Chlorophyll-a"`.

- orderers:

  Character vector. Optional. Orderers to filter data by specific
  organizations.

- qualityFlags:

  Character vector. Optional. Quality flags to filter data.

- deliverers:

  Character vector. Optional. Deliverers to filter data by data
  providers.

- projects:

  Character vector. Optional. Projects to filter data by specific
  research or monitoring projects.

- datasets:

  Character vector. Optional. Datasets to filter data by specific
  dataset names.

- minSamplingDepth:

  Numeric. Optional. Minimum depth (in meters) for sampling data.

- maxSamplingDepth:

  Numeric. Optional. Maximum depth (in meters) for sampling data.

- checkStatus:

  Character string. Optional. Status check to filter results.

- redListedCategory:

  Character vector. Optional. Red-listed taxa for conservation
  filtering.

- taxonName:

  Character vector. Optional. Taxa names for filtering specific species
  or taxa.

- stationName:

  Character vector. Optional. Station names to retrieve data from
  specific stations.

- vattenDistrikt:

  Character vector. Optional. Water district names to filter data by
  Swedish water districts.

- seaBasins:

  Character vector. Optional. Sea basin names to filter data by
  different sea areas.

- counties:

  Character vector. Optional. Counties to filter data within specific
  administrative regions in Sweden.

- municipalities:

  Character vector. Optional. Municipalities to filter data within
  specific local regions.

- waterCategories:

  Character vector. Optional. Water categories to filter data by.

- typOmraden:

  Character vector. Optional. Type areas to filter data by specific
  areas.

- helcomOspar:

  Character vector. Optional. HELCOM or OSPAR areas for regional
  filtering.

- seaAreas:

  Character vector. Optional. Sea area codes for filtering by specific
  sea areas.

- prod:

  Logical. Select production server when `TRUE` (default). Ignored if
  `utv` is `TRUE`.

- utv:

  Logical. Select UTV server when `TRUE`.

## Value

An integer representing the total number of rows in the requested SHARK
table after applying the specified filters.

## See also

<https://shark.smhi.se/en> for SHARK database.

[`get_shark_options`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_options.md)
to see filter options

[`get_shark_data`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_data.md)
to download SHARK data

## Examples

``` r
# \donttest{
  # Retrieve chlorophyll data for April to June from 2019 to 2020
  shark_data_counts <- get_shark_table_counts(fromYear = 2019, toYear = 2020,
                                              months = c(4, 5, 6), dataTypes = c("Chlorophyll"))
  print(shark_data_counts)
#> [1] 179
# }
```
