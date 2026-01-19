# Clean SHARK4R cache by file age and session

Deletes cached files in the SHARK4R cache directory that are older than
a specified number of days.

## Usage

``` r
clean_shark4r_cache(
  days = 1,
  cache_dir = NULL,
  clear_perm_cache = FALSE,
  search_pattern = NULL,
  verbose = TRUE
)
```

## Arguments

- days:

  Numeric; remove files older than this number of days. Default is 1.

- cache_dir:

  Character; path to the cache directory to clean. Defaults to the
  package cache directory in the user-specific R folder (via the
  internal `cache_dir()` helper). You can override this parameter to
  specify a custom cache location.

- clear_perm_cache:

  Logical. If `TRUE`, filed that are cached across R sessions are
  cleared, i.e. geographical shape files. Defaults to `FALSE`.

- search_pattern:

  Character; optional regex pattern to filter which files to consider
  for deletion.

- verbose:

  Logical. If `TRUE`, displays messages of cache cleaning progress.
  Defaults to `TRUE`.

## Value

Invisible `NULL`. Messages are printed about what was deleted and
whether the in-memory session cache was cleared.

## Details

The cache is automatically cleared for files older than 24 hours. Files
in the `perm` subdirectory are not removed automatically and must be
cleared explicitly using `clear_perm_cache = TRUE`.

## See also

[`get_peg_list()`](https://sharksmhi.github.io/SHARK4R/reference/get_peg_list.md),
[`get_nomp_list()`](https://sharksmhi.github.io/SHARK4R/reference/get_nomp_list.md),
[`get_shark_codes()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_codes.md),
[`get_dyntaxa_dwca()`](https://sharksmhi.github.io/SHARK4R/reference/get_dyntaxa_dwca.md),
[`get_shark_statistics()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_statistics.md)
for functions that populate the cache.

## Examples

``` r
# \donttest{
  # Remove files older than 60 days and clear session cache
  clean_shark4r_cache(days = 60)
#> No files older than 60 days to remove.
# }
```
