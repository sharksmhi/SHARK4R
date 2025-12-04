# Get the latest EG-Phyto/PEG biovolume Excel list

This function downloads the EG-Phyto (previously PEG) biovolume zip
archive from ICES (using `cache_peg_zip()`), unzips it, and reads the
first Excel file by default. You can also specify which file in the
archive to read.

## Usage

``` r
get_peg_list(
  file = NULL,
  sheet = NULL,
  force = FALSE,
  url = "https://www.ices.dk/data/Documents/ENV/PEG_BVOL.zip",
  clean_cache_days = 30,
  verbose = TRUE
)
```

## Arguments

- file:

  Character string specifying which file in the zip archive to read.
  Defaults to the first Excel file in the archive.

- sheet:

  Character or numeric; the name or index of the sheet to read from the
  Excel file. If neither argument specifies the sheet, defaults to the
  first sheet.

- force:

  Logical; if `TRUE`, forces re-download of the zip file even if a
  cached copy exists.

- url:

  Character string with the URL of the PEG zip file. Defaults to the
  official ICES link.

- clean_cache_days:

  Numeric; if not `NULL`, cached PEG zip files older than this number of
  days will be automatically deleted and replaced by a new download.
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
  # Read the first Excel file from the PEG zip
  peg_list <- get_peg_list()

  # Read the latest list and clean old cache files older than 60 days
  peg_list2 <- get_peg_list(clean_cache_days = 60)
} # }
```
