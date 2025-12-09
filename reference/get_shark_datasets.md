# Download SHARK dataset zip archives

Downloads one or more datasets (zip-archives) from the SHARK database
(Swedish national marine environmental data archive) and optionally
unzips them. The function matches provided dataset names against all
available SHARK datasets.

## Usage

``` r
get_shark_datasets(
  dataset_name,
  save_dir = NULL,
  prod = TRUE,
  utv = FALSE,
  unzip_file = FALSE,
  return_df = FALSE,
  encoding = "latin_1",
  guess_encoding = TRUE,
  verbose = TRUE
)
```

## Arguments

- dataset_name:

  Character vector with one or more dataset names (or partial names).
  Each entry will be matched against available SHARK dataset identifiers
  (e.g., `"SHARK_Phytoplankton_2023_SMHI_BVVF"` for a specific dataset,
  or `"SHARK_Phytoplankton"` for all Phytoplankton datasets).

- save_dir:

  Directory where zip files (and optionally their extracted contents)
  should be stored. Defaults to `NULL`. If `NULL` or `""`, a temporary
  directory is used.

- prod:

  Logical, whether to download from the production (`TRUE`, default) or
  test (`FALSE`) SHARK server. Ignored if `utv` is `TRUE`.

- utv:

  Logical. Select UTV server when `TRUE`.

- unzip_file:

  Logical, whether to extract downloaded zip archives (`TRUE`) or only
  save them (`FALSE`, default).

- return_df:

  Logical, whether to return a combined data frame with the contents of
  all downloaded datasets (`TRUE`) instead of a list of file paths
  (`FALSE`, default).

- encoding:

  Character. File encoding of `shark_data.txt`. Options: `"cp1252"`,
  `"utf_8"`, `"utf_16"`, `"latin_1"`. Default is `"latin_1"`. If
  `guess_encoding = TRUE`, detected encoding overrides this value.
  Ignored if `return_df` is `FALSE`.

- guess_encoding:

  Logical. If `TRUE` (default), automatically detect file encoding. If
  `FALSE`, the function uses only the user-specified encoding. Ignored
  if `return_df` is `FALSE`.

- verbose:

  Logical, whether to show download and extraction progress messages.
  Default is `TRUE`.

## Value

If `return_df = FALSE`, a named list of character vectors. Each element
corresponds to one matched dataset and contains either the path to the
downloaded zip file (if `unzip_file = FALSE`) or the path to the
extraction directory (if `unzip_file = TRUE`). If `return_df = TRUE`, a
single combined data frame with all dataset contents, including a
`source` column indicating the dataset.

## See also

<https://shark.smhi.se/en> for SHARK database.

[`get_shark_options()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_options.md)
for listing available datasets.

[`get_shark_data()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_data.md)
for downloading tabular data.

## Examples

``` r
# \donttest{
# Get a specific dataset
get_shark_datasets("SHARK_Phytoplankton_2023_SMHI_BVVF", verbose = FALSE)
#> $`SHARK_Phytoplankton_2023_SMHI_BVVF_version_2025-03-09.zip`
#> [1] "/tmp/RtmpWnWKWd/SHARK_Phytoplankton_2023_SMHI_BVVF_version_2025-03-09.zip"
#> 

# Get all Zooplankton datasets from 2022 and unzip them
get_shark_datasets(
  dataset_name = "Zooplankton_2022",
  unzip_file = TRUE,
  verbose = FALSE
)
#> $`SHARK_Zooplankton_2022_DEEP_version_2024-12-17.zip`
#> [1] "/tmp/RtmpWnWKWd/SHARK_Zooplankton_2022_DEEP_version_2024-12-17"
#> 
#> $`SHARK_Zooplankton_2022_SMHI_version_2024-12-17.zip`
#> [1] "/tmp/RtmpWnWKWd/SHARK_Zooplankton_2022_SMHI_version_2024-12-17"
#> 
#> $`SHARK_Zooplankton_2022_UMSC_version_2024-03-27.zip`
#> [1] "/tmp/RtmpWnWKWd/SHARK_Zooplankton_2022_UMSC_version_2024-03-27"
#> 

# Get all Chlorophyll datasets and return as a combined data frame
combined_df <- get_shark_datasets(
  dataset_name = "Chlorophyll",
  return_df = TRUE,
  verbose = FALSE
)
#> Detected encoding 'windows-1252' differs from specified 'latin_1'. Using detected encoding.
head(combined_df)
#> # A tibble: 6 × 73
#>   source delivery_datatype check_status_sv data_checked_by_sv visit_year
#>    <dbl> <chr>             <chr>           <chr>                   <dbl>
#> 1      1 Chlorophyll       Klar            Leverantör               1989
#> 2      1 Chlorophyll       Klar            Leverantör               1989
#> 3      1 Chlorophyll       Klar            Leverantör               1985
#> 4      1 Chlorophyll       Klar            Leverantör               1989
#> 5      1 Chlorophyll       Klar            Leverantör               1989
#> 6      1 Chlorophyll       Klar            Leverantör               1989
#> # ℹ 68 more variables: visit_month <dbl>, station_name <chr>,
#> #   reported_station_name <chr>, sample_location_id <dbl>, station_id <dbl>,
#> #   sample_project_name_en <chr>, sample_orderer_name_en <chr>,
#> #   platform_code <chr>, visit_id <chr>, expedition_id <chr>,
#> #   shark_sample_id_md5 <chr>, sample_date <date>, sample_time <time>,
#> #   sample_enddate <date>, sample_endtime <lgl>, sample_latitude_dm <chr>,
#> #   sample_longitude_dm <chr>, sample_latitude_dd <dbl>, …
# }
```
