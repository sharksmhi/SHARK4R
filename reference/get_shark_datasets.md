# Download SHARK dataset zip archives

Downloads one or more datasets (zip-archives) from the SHARK database
(Swedish national marine environmental data archive) and optionally
unzips them. The function matches provided dataset names against all
available SHARK datasets.

## Usage

``` r
get_shark_datasets(
  dataset_name,
  save_dir = "",
  prod = TRUE,
  utv = FALSE,
  unzip_file = FALSE,
  return_df = FALSE,
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
  should be stored. Defaults to `""`. If `NULL` or `""`, the current
  working directory is used.

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

<https://shark.smhi.se> for SHARK database.

[`get_shark_options()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_options.md)
for listing available datasets.

[`get_shark_data()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_data.md)
for downloading tabular data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a specific dataset
get_shark_datasets("SHARK_Phytoplankton_2023_SMHI_BVVF")

# Get all Zooplankton datasets from 2022 and unzip them
get_shark_datasets(
  dataset_name = c("Zooplankton_2022"),
  save_dir = "data",
  unzip_file = TRUE
)

# Get all Phytoplankton datasets and return as a combined data frame
combined_df <- get_shark_datasets(
  dataset_name = "Phytoplankton_2023",
  return_df = TRUE
)
} # }
```
