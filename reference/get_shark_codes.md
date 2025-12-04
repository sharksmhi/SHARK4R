# Get SHARK codelist from SMHI

This function downloads the SHARK codes Excel file from SMHI (if not
already cached) and reads it into R. The file is stored in a persistent
cache directory so it does not need to be downloaded again in subsequent
sessions.

## Usage

``` r
get_shark_codes(
  url =
    "https://smhi.se/oceanografi/oce_info_data/shark_web/downloads/codelist_SMHI.xlsx",
  sheet = 1,
  skip = 1,
  force = FALSE,
  clean_cache_days = 30
)
```

## Arguments

- url:

  Character string with the URL to the SHARK codes Excel file. Defaults
  to the official SMHI codelist.

- sheet:

  Sheet to read. Can be either the sheet name or its index (default is
  `1`).

- skip:

  Number of rows to skip before reading data (default is `1`, to skip
  the header row).

- force:

  Logical; if `TRUE`, forces re-download of the Excel file even if a
  cached copy is available. Default is `FALSE`.

- clean_cache_days:

  Numeric; if not `NULL`, cached SHARK code Excel files older than this
  number of days will be automatically deleted. Defaults to 30. Set to
  `NULL` to disable automatic cleanup.

## Value

A tibble containing the contents of the requested sheet.

## See also

[`clean_shark4r_cache()`](https://sharksmhi.github.io/SHARK4R/reference/clean_shark4r_cache.md)
to manually clear cached files.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Read the first sheet, skipping the first row
  codes <- get_shark_codes()

  # Read second sheet without skipping rows
  codes2 <- get_shark_codes(sheet = 2, skip = 0)

  # Force re-download of the Excel file
  codes3 <- get_shark_codes(force = TRUE)
} # }
```
