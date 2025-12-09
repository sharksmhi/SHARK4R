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
# \donttest{
  # Read the first Excel file from the PEG zip
  peg_list <- get_peg_list()
#> Reading PEG biovolume Excel file for year: 2025
  head(peg_list)
#> # A tibble: 6 × 36
#>   Division     Class Order Genus Species SFLAG STAGE Author AphiaID AphiaID_link
#>   <chr>        <chr> <chr> <chr> <chr>   <chr> <chr> <chr>    <dbl> <chr>       
#> 1 CYANOBACTER… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
#> 2 CYANOBACTER… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
#> 3 CYANOBACTER… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
#> 4 CYANOBACTER… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
#> 5 CYANOBACTER… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
#> 6 CYANOBACTER… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
#> # ℹ 26 more variables: Trophy <chr>, Geometric_shape <chr>, FORMULA <chr>,
#> #   SizeClassNo <dbl>, Nonvalid_SIZCL <lgl>, Not_accepted_name <lgl>,
#> #   Unit <chr>, SizeRange <chr>, `Length(l1)µm` <dbl>, `Length(l2)µm` <dbl>,
#> #   `Width(w)µm` <dbl>, `Height(h)µm` <dbl>, `Diameter(d1)µm` <dbl>,
#> #   `Diameter(d2)µm` <dbl>, `No_of_cells/counting_unit` <dbl>,
#> #   `Calculated_volume_µm3 (with formula) - NOT IMPORTED, NOT handled by ICES` <dbl>,
#> #   `Calculated_volume_µm3/counting_unit` <dbl>, Comment <chr>, …
# }
```
