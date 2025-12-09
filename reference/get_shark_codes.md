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

A `tibble` containing the contents of the requested sheet.

## See also

[`clean_shark4r_cache()`](https://sharksmhi.github.io/SHARK4R/reference/clean_shark4r_cache.md)
to manually clear cached files.

## Examples

``` r
# \donttest{
  # Read the first sheet, skipping the first row
  codes <- get_shark_codes()
  head(codes)
#> # A tibble: 6 × 6
#>   Data_field Code  Beskrivning/Svensk ö…¹ Description/English …² Note  Reference
#>   <chr>      <chr> <chr>                  <chr>                  <chr> <chr>    
#> 1 CLMET      F     Flowmeter reading      Flowmeter reading      NA    NA       
#> 2 CLMET      H     Haul length            Haul length            NA    NA       
#> 3 CLOUD      0     Inga moln              No clouds              NA    NA       
#> 4 CLOUD      1     1/8 eller mindre, men… 1/8 or less, but not … NA    NA       
#> 5 CLOUD      2     2/8                    2/8                    NA    NA       
#> 6 CLOUD      3     3/8                    3/8                    NA    NA       
#> # ℹ abbreviated names: ¹​`Beskrivning/Svensk översättning`,
#> #   ²​`Description/English translate`

  # Force re-download of the Excel file
  codes <- get_shark_codes(force = TRUE)
# }
```
