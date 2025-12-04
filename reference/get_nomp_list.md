# Get the latest NOMP biovolume Excel list

This function downloads the latest available Nordic Marine Phytoplankton
Group (NOMP) biovolume zip archive from SMHI (using `cache_nomp_zip()`),
unzips it, and reads the first Excel file by default. You can also
specify which file in the archive to read.

## Usage

``` r
get_nomp_list(
  year = as.numeric(format(Sys.Date(), "%Y")),
  file = NULL,
  sheet = NULL,
  force = FALSE,
  base_url = NULL,
  clean_cache_days = 30,
  verbose = TRUE
)
```

## Arguments

- year:

  Numeric year to download. Default is current year; if not available,
  previous years are automatically tried.

- file:

  Character string specifying which file in the zip archive to read.
  Defaults to the first Excel file in the archive.

- sheet:

  Character or numeric; the name or index of the sheet to read from the
  Excel file. If neither argument specifies the sheet, defaults to the
  first sheet.

- force:

  Logical; if `TRUE`, forces re-download of the zip file even if cached
  copy exists.

- base_url:

  Base URL (without "/nomp_taxa_biovolumes_and_carbon_YYYY.zip") for the
  NOMP biovolume files. Defaults to the SMHI directory.

- clean_cache_days:

  Numeric; if not `NULL`, cached NOMP zip files older than this number
  of days will be automatically deleted and replaced by a new download.
  Defaults to 30. Set to `NULL` to disable automatic cleanup.

- verbose:

  A logical indicating whether to print progress messages. Default is
  TRUE.

## Value

A tibble with the contents of the requested Excel file.

## See also

[`clean_shark4r_cache()`](https://sharksmhi.github.io/SHARK4R/reference/clean_shark4r_cache.md)
to manually clear cached files.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Get the latest available list
  nomp_list <- get_nomp_list()

  # Get the 2023 list and clean old cache files older than 60 days
  nomp_list_2023 <- get_nomp_list(2023, clean_cache_days = 60)
} # }
```
