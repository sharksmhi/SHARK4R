# Check if the required and recommended datatype-specific SHARK system fields are present

**\[deprecated\]** This function is deprecated and has been replaced by
[`check_fields()`](https://sharksmhi.github.io/SHARK4R/reference/check_fields.md).

## Usage

``` r
check_deliv_Chlorophyll(data, level = "error")
```

## Arguments

- data:

  The data frame.

- level:

  The level of error reporting, i.e. "error" or "warning". Recommended
  fields are only checked in case of "warning".

## Value

Any warnings or errors.
