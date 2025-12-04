# Download and read Darwin Core Archive files from Dyntaxa

This function downloads a complete Darwin Core Archive (DwCA) of Dyntaxa
from the SLU Artdatabanken API, extracts the archive, and reads the
specified CSV file into R.

## Usage

``` r
get_dyntaxa_dwca(
  subscription_key = Sys.getenv("DYNTAXA_KEY"),
  file_to_read = "Taxon.csv",
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- subscription_key:

  A Dyntaxa API subscription key. By default, the key is read from the
  environment variable `DYNTAXA_KEY`.

  You can provide the key in three ways:

  - **Directly as a parameter**:
    `get_dyntaxa_dwca(subscription_key = "your_key_here")`.

  - **Temporarily for the session**:
    `Sys.setenv(DYNTAXA_KEY = "your_key_here")`. After this, you do not
    need to pass `subscription_key` to the function.

  - **Permanently across sessions** by adding it to your `~/.Renviron`
    file. Use `usethis::edit_r_environ()` to open the file, then add:
    `DYNTAXA_KEY=your_key_here`. After this, you do not need to pass
    `subscription_key` to the function.

- file_to_read:

  A string specifying the name of the CSV file to read from the
  extracted archive. Allowed options are: `"Reference.csv"`,
  `"SpeciesDistribution.csv"`, `"Taxon.csv"`, or `"VernacularName.csv"`.
  Defaults to `"Taxon.csv"`.

- force:

  A logical value indicating whether to force a fresh download of the
  archive, even if a cached copy is available. Defaults to `FALSE`.

- verbose:

  A logical value indicating whether to show download progress. Defaults
  to `TRUE`.

## Value

A tibble containing the data from the specified CSV file.

## Details

By default, the archive is downloaded and cached across R sessions. On
subsequent calls, the function reuses the cached copy of the extracted
files to avoid repeated downloads. Use the `force` parameter to
re-download the archive if needed. The cache is cleared automatically
after 24 hours, but you can also manually clear it using
[`clean_shark4r_cache`](https://sharksmhi.github.io/SHARK4R/reference/clean_shark4r_cache.md).

A valid Dyntaxa API subscription key is required. You can request a free
key for the "Taxonomy" service from the ArtDatabanken API portal:
<https://api-portal.artdatabanken.se/>

**Note**: Please review the [API
conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
and [register for access](https://api-portal.artdatabanken.se/) before
using the API. Data collected through the API is stored at SLU
Artdatabanken. Please also note that the authors of `SHARK4R` are not
affiliated with SLU Artdatabanken.

## See also

[`clean_shark4r_cache()`](https://sharksmhi.github.io/SHARK4R/reference/clean_shark4r_cache.md)
to manually clear cached files.

## Examples

``` r
if (FALSE) { # \dontrun{
# Provide your Dyntaxa API subscription key
subscription_key <- "your_subscription_key"

# Download and read the Taxon.csv file
taxon_data <- get_dyntaxa_dwca(subscription_key, file_to_read = "Taxon.csv")
} # }
```
