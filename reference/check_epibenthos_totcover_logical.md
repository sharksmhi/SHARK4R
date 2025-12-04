# Check if Epibenthos total cover exceeds 100%

**\[deprecated\]** This function is deprecated and has been replaced by
[`check_logical_parameter()`](https://sharksmhi.github.io/SHARK4R/reference/check_logical_parameter.md).
Alternatively, you can use
[`check_parameter_rules()`](https://sharksmhi.github.io/SHARK4R/reference/check_parameter_rules.md).

## Usage

``` r
check_epibenthos_totcover_logical(
  data,
  return_df = FALSE,
  return_logical = FALSE
)
```

## Arguments

- data:

  A data frame. Must contain columns `parameter` and `value`.

- return_df:

  Logical. If TRUE, return a plain data.frame of problematic rows.

- return_logical:

  Logical. If TRUE, return a logical vector of length nrow(data)
  indicating which rows exceed 100% for Total cover. Overrides
  return_df.

## Value

A DT datatable, a data.frame, a logical vector, or NULL if no problems
found.
