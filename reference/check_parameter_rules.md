# Check parameter values against logical rules

Applies parameter-specific and row-wise logical rules to
benthos/epibenthos data, flagging measurements that violate defined
conditions. This function replaces multiple deprecated
`check_*_logical()` functions with a general, flexible implementation.

## Usage

``` r
check_parameter_rules(
  data,
  param_conditions = get(".param_conditions", envir = asNamespace("SHARK4R")),
  rowwise_conditions = get(".rowwise_conditions", envir = asNamespace("SHARK4R")),
  return_df = FALSE,
  return_logical = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame containing at least the columns `parameter` and `value`.

- param_conditions:

  A named list of parameter-specific rules. Each element should be a
  list with:

  condition

  :   Function taking a numeric vector and returning a logical vector
      (TRUE = violation).

  range_msg

  :   Character string describing the expected range.

  Defaults to `SHARK4R:::.param_conditions` defined in the package
  namespace.

- rowwise_conditions:

  A named list of row-wise rules applied across multiple parameters.
  Each element should be a function taking the full data frame and
  returning a logical vector. Defaults to
  `SHARK4R:::.rowwise_conditions` defined in the package namespace.

- return_df:

  Logical. If TRUE, problematic rows are returned as plain
  `data.frame`s.

- return_logical:

  Logical. If TRUE, problematic rows are returned as logical vectors.
  Overrides `return_df`.

- verbose:

  Logical. If TRUE, messages will be displayed during execution.
  Defaults to TRUE.

## Value

A named list of results for each parameter:

- Logical vector:

  If `return_logical = TRUE`.

- Data frame:

  If `return_df = TRUE` and violations exist.

- DT datatable:

  If violations exist and `return_df = FALSE`.

- NULL:

  If no violations exist for the parameter.

Invisible return.

## Details

This function evaluates each parameter in `param_conditions` and
`rowwise_conditions`. Only parameters present in the dataset are
checked. Messages are printed indicating whether values are within
expected ranges or which rows violate rules.

## Examples

``` r
df <- data.frame(
  station_name = c("A1", "A2", "A3", "A4"),
  sample_date = as.Date("2023-05-01") + 0:3,
  sample_id = 101:104,
  parameter = c("Wet weight", "Wet weight", "Abundance", "BQIm"),
  value = c(0, 5, 0, 3)
)

# Check against default package rules
check_parameter_rules(df)
#> Parameter Wet weight, measurement(s) outside expected range: > 0
#> Parameter BQIm, row-wise logical check failed

# Return problematic rows as data.frame
check_parameter_rules(df, return_df = TRUE)
#> Parameter Wet weight, measurement(s) outside expected range: > 0
#> Parameter BQIm, row-wise logical check failed

# Return logical vectors for each parameter
rule_check <- check_parameter_rules(df, return_logical = TRUE)
print(rule_check)
#> $`Wet weight`
#> [1]  TRUE FALSE FALSE FALSE
#> 
#> $BQIm
#> [1] FALSE FALSE  TRUE  TRUE
#> 
```
