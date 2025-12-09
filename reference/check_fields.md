# Validate SHARK data fields for a given datatype

This function checks a SHARK data frame against the required and
recommended fields defined for a specific datatype. It verifies that all
required fields are present and contain non-empty values. If
`level = "warning"`, it also checks for recommended fields and empty
values within them.

## Usage

``` r
check_fields(
  data,
  datatype,
  level = "error",
  stars = 1,
  bacterioplankton_subtype = "abundance",
  field_definitions = .field_definitions
)
```

## Arguments

- data:

  A data frame containing SHARK data to be validated.

- datatype:

  A string giving the SHARK datatype to validate against. Must exist as
  a name in the provided `field_definitions`.

- level:

  Character string, either `"error"` or `"warning"`. If `"error"`, only
  required fields are validated. If `"warning"`, recommended fields are
  also checked and reported as warnings.

- stars:

  Integer. Maximum number of "*" levels to include. Default = 1 (only
  single "*"). For example, `stars = 2` includes "*" and "\*\*",
  `stars = 3` includes "*", "**", and "**\*".

- bacterioplankton_subtype:

  Character. For "Bacterioplankton" only: either "abundance" (default)
  or "production". Ignored for other datatypes.

- field_definitions:

  A named list of field definitions. Each element should contain two
  character vectors: `required` and `recommended`. Defaults to the
  package's built-in `SHARK4R:::.field_definitions`. Alternatively, the
  latest definitions can be loaded directly from the official SHARK4R
  GitHub repository using
  [`load_shark4r_fields()`](https://sharksmhi.github.io/SHARK4R/reference/load_shark4r_fields.md).

## Value

A tibble with the following columns:

- level:

  Either `"error"` or `"warning"`.

- field:

  The name of the field that triggered the check.

- row:

  Row number(s) in `data` where the issue occurred, or `NA` if the whole
  field is missing.

- message:

  A descriptive message explaining the problem.

The tibble will be empty if no problems are found.

## Details

Note: A single "\*" marks required fields in the standard SHARK
template. A double "\*\*" is often used to specify columns required for
**national monitoring only**. For more information, see:
https://www.smhi.se/data/hav-och-havsmiljo/datavardskap-oceanografi-och-marinbiologi/leverera-data

Field definitions for SHARK data can be loaded in two ways:

1.  **From the SHARK4R package bundle (default):** The package contains
    a built-in object, `.field_definitions`, which stores required and
    recommended fields for each datatype.

2.  **From GitHub (latest official version):** To use the most
    up-to-date field definitions, you can load them directly from the
    [SHARK4R-statistics](https://github.com/nodc-sweden/SHARK4R-statistics)
    repository:

            defs <- load_shark4r_fields()
            check_fields(my_data, "Phytoplankton", field_definitions = defs)
            

**Delivery-format (all-caps) data:** If the column names in `data` are
all uppercase (e.g. SDATE), `check_fields()` assumes the dataset follows
the official SHARK delivery template. In this case:

- Required fields are determined from the delivery template using
  [`get_delivery_template()`](https://sharksmhi.github.io/SHARK4R/reference/get_delivery_template.md)
  and
  [`find_required_fields()`](https://sharksmhi.github.io/SHARK4R/reference/find_required_fields.md).

- Recommended fields are ignored because the delivery templates do not
  define them.

- The function validates that all required columns exist and contain
  non-empty values.

This ensures that both internal `SHARK4R` datasets (with camelCase or
snake_case columns) and official delivery files (ALL_CAPS columns) are
validated correctly using the appropriate rules.

Stars in the template

Leading asterisks in the delivery template indicate required levels:

- *\** = standard required column

- \* = required for national monitoring

- Other symbols = additional requirement level

The `stars` parameter in `check_fields()` controls how many levels of
required columns to include.

## See also

[`load_shark4r_fields`](https://sharksmhi.github.io/SHARK4R/reference/load_shark4r_fields.md)
for fetching the latest field definitions from GitHub,
[`get_delivery_template`](https://sharksmhi.github.io/SHARK4R/reference/get_delivery_template.md)
for downloading delivery templates from SMHI's website.

## Examples

``` r
# Example 1: Using built-in field definitions for "Phytoplankton"
df_phyto <- data.frame(
  visit_date = "2023-06-01",
  sample_id = "S1",
  scientific_name = "Skeletonema marinoi",
  value = 123
)

# Check fields
check_fields(df_phyto, "Phytoplankton", level = "warning")
#> # A tibble: 42 × 4
#>    level field                  row   message                                   
#>    <chr> <chr>                  <lgl> <chr>                                     
#>  1 error visit_year             NA    Required field visit_year is missing      
#>  2 error station_name           NA    Required field station_name is missing    
#>  3 error reported_station_name  NA    Required field reported_station_name is m…
#>  4 error sample_project_name_sv NA    Required field sample_project_name_sv is …
#>  5 error sample_orderer_name_sv NA    Required field sample_orderer_name_sv is …
#>  6 error platform_code          NA    Required field platform_code is missing   
#>  7 error sample_date            NA    Required field sample_date is missing     
#>  8 error sample_time            NA    Required field sample_time is missing     
#>  9 error sample_latitude_dd     NA    Required field sample_latitude_dd is miss…
#> 10 error sample_longitude_dd    NA    Required field sample_longitude_dd is mis…
#> # ℹ 32 more rows

# \donttest{
# Example 2: Load latest definitions from GitHub and use them
defs <- load_shark4r_fields(verbose = FALSE)

# Check fields using loaded field definitions
check_fields(df_phyto, "Phytoplankton", field_definitions = defs)
#> # A tibble: 40 × 4
#>    level field                  row   message                                   
#>    <chr> <chr>                  <lgl> <chr>                                     
#>  1 error visit_year             NA    Required field visit_year is missing      
#>  2 error station_name           NA    Required field station_name is missing    
#>  3 error reported_station_name  NA    Required field reported_station_name is m…
#>  4 error sample_project_name_sv NA    Required field sample_project_name_sv is …
#>  5 error sample_orderer_name_sv NA    Required field sample_orderer_name_sv is …
#>  6 error platform_code          NA    Required field platform_code is missing   
#>  7 error sample_date            NA    Required field sample_date is missing     
#>  8 error sample_time            NA    Required field sample_time is missing     
#>  9 error sample_latitude_dd     NA    Required field sample_latitude_dd is miss…
#> 10 error sample_longitude_dd    NA    Required field sample_longitude_dd is mis…
#> # ℹ 30 more rows
# }

# Example 3: Custom datatype with required + recommended fields
defs <- list(
  ExampleType = list(
    required = c("id", "value"),
    recommended = "comment"
  )
)

# Example data
df_ok <- data.frame(id = 1, value = "x", comment = "ok")

# Check fields using custom field definitions
check_fields(df_ok, "ExampleType", level = "warning", field_definitions = defs)
#> # A tibble: 0 × 0
```
