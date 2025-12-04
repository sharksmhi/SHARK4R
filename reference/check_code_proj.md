# Check matches of reported codes in SMHI's SHARK codelist

**\[deprecated\]** This function is deprecated and has been replaced by
[`check_codes()`](https://sharksmhi.github.io/SHARK4R/reference/check_codes.md).

## Usage

``` r
check_code_proj(data, field = "sample_project_name_sv", clean_cache_days = 30)
```

## Arguments

- data:

  for tibble be be checked

- field:

  Character; name of the column in `data` that contains the codes to be
  validated against the SHARK codelist. If a cell contains multiple
  codes separated by commas, each code is validated separately. Default
  is `"sample_project_name_en"`.

- clean_cache_days:

  Numeric; if not NULL, cached SHARK code Excel files older than this
  number of days will be automatically deleted and be replaced by a new
  download. Defaults to 30. Set to NULL to disable automatic cleanup.

## Value

unmatched codes with true or false results

## See also

[`get_shark_codes()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_codes.md)
to get the current code list.
