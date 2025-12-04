# Load SHARK4R statistics from GitHub

This function downloads and loads precomputed SHARK4R statistical data
(e.g., threshold or summary statistics) directly from the
[SHARK4R-statistics](https://github.com/nodc-sweden/SHARK4R-statistics)
GitHub repository. The data are stored as `.rds` files and read into R
as objects.

## Usage

``` r
load_shark4r_stats(file_name = "sea_basin.rds", verbose = TRUE)
```

## Arguments

- file_name:

  Character string specifying the name of the `.rds` file to download.
  Defaults to `"sea_basin.rds"`.

- verbose:

  Logical; if `TRUE` (default), prints progress messages during download
  and loading.

## Value

An R object (typically a `tibble` or `data.frame`) read from the
specified `.rds` file.

## Details

The function retrieves the file from the GitHub repository’s `data/`
folder. It temporarily downloads the file to the local system and then
reads it into R using
[`readRDS()`](https://rdrr.io/r/base/readRDS.html).

If the download fails (e.g., due to a network issue or invalid
filename), the function throws an error with a descriptive message.

## See also

[`check_outliers`](https://sharksmhi.github.io/SHARK4R/reference/check_outliers.md)
for detecting threshold exceedances using the loaded statistics,
[`get_shark_statistics`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_statistics.md)
for generating and caching statistical summaries used in SHARK4R.
[`scatterplot`](https://sharksmhi.github.io/SHARK4R/reference/scatterplot.md)
for generating interactive plots with threshold values.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the default SHARK4R statistics file
stats <- load_shark4r_stats()

# Load a specific file
thresholds <- load_shark4r_stats("scientific_name.rds")
} # }
```
