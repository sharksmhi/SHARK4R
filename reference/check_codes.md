# Check matches of reported codes in SMHI's SHARK codelist

This function checks whether the codes reported in a specified column of
a dataset (e.g., project codes, ship codes, etc.) are present in the
official SHARK codelist provided by SMHI. If a cell contains multiple
codes separated by commas, each code is checked individually. The
function downloads and caches the codelist if necessary, compares the
reported values against the valid codes, and returns a tibble showing
which codes matched. Informative messages are printed if unmatched codes
are found.

## Usage

``` r
check_codes(
  data,
  field = "sample_project_name_en",
  code_type = "PROJ",
  match_column = "Description/English translate",
  clean_cache_days = 30,
  verbose = TRUE
)
```

## Arguments

- data:

  A tibble (or data.frame) containing the codes to check.

- field:

  Character; name of the column in `data` that contains the codes to be
  validated against the SHARK codelist. If a cell contains multiple
  codes separated by commas, each code is validated separately. Default
  is `"sample_project_name_en"`.

- code_type:

  Character; the type of code to check (e.g., `"PROJ"`). Defaults to
  `"PROJ"`.

- match_column:

  Character; the column in the SHARK codelist to match against. Must be
  one of `"Code"` or `"Description/English translate"`. Defaults to
  `"Description/English translate"`.

- clean_cache_days:

  Numeric; if not `NULL`, cached SHARK code Excel files older than this
  number of days will be automatically deleted and replaced by a new
  download. Defaults to 30. Set to `NULL` to disable automatic cleanup.

- verbose:

  Logical. If TRUE, messages will be displayed during execution.
  Defaults to TRUE.

## Value

A tibble with unique reported codes (after splitting comma-separated
entries) and a logical column `match_type` indicating if they exist in
the SHARK codelist.

## See also

[`get_shark_codes()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_codes.md)
to get the current code list.

[`clean_shark4r_cache()`](https://sharksmhi.github.io/SHARK4R/reference/clean_shark4r_cache.md)
to manually clear cached files.
