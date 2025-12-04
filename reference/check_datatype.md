# Validate SHARK system fields in a data frame

This function checks whether the required and recommended global and
datatype-specific SHARK system fields are present in a data frame.

## Usage

``` r
check_datatype(data, level = "error")
```

## Arguments

- data:

  A `data.frame` or `tibble` containing SHARK data to validate.

- level:

  Character. The level of validation:

  - `"error"` (default) — checks only required fields.

  - `"warning"` — checks both required and recommended fields.

## Value

A `tibble` summarizing missing or empty fields, with columns:

- `level`: `"error"` or `"warning"`.

- `field`: Name of the missing or empty field.

- `row`: Row number where the value is missing (NA) or `NA` if the whole
  column is missing.

- `message`: Description of the issue.

## Details

- **Required fields**: Missing or empty required fields are reported as
  **errors**.

- **Recommended fields**: Missing or empty recommended fields are
  reported as **warnings**, but only if `level = "warning"` is
  specified.

## Examples

``` r
# Example with required fields missing
df <- data.frame(
  visit_year = 2024,
  station_name = NA
)
check_datatype(df, level = "error")
#> # A tibble: 9 × 4
#>   level field                     row message                                   
#>   <chr> <chr>                   <int> <chr>                                     
#> 1 error sample_project_name_en     NA Required field sample_project_name_en is …
#> 2 error sample_orderer_name_en     NA Required field sample_orderer_name_en is …
#> 3 error platform_code              NA Required field platform_code is missing   
#> 4 error sample_date                NA Required field sample_date is missing     
#> 5 error sample_latitude_dd         NA Required field sample_latitude_dd is miss…
#> 6 error sample_longitude_dd        NA Required field sample_longitude_dd is mis…
#> 7 error positioning_system_code    NA Required field positioning_system_code is…
#> 8 error water_depth_m              NA Required field water_depth_m is missing   
#> 9 error station_name                1 Empty value for required field station_na…

# Example checking recommended fields as warnings
check_datatype(df, level = "warning")
#> # A tibble: 14 × 4
#>    level   field                           row message                          
#>    <chr>   <chr>                         <int> <chr>                            
#>  1 error   sample_project_name_en           NA Required field sample_project_na…
#>  2 error   sample_orderer_name_en           NA Required field sample_orderer_na…
#>  3 error   platform_code                    NA Required field platform_code is …
#>  4 error   sample_date                      NA Required field sample_date is mi…
#>  5 error   sample_latitude_dd               NA Required field sample_latitude_d…
#>  6 error   sample_longitude_dd              NA Required field sample_longitude_…
#>  7 error   positioning_system_code          NA Required field positioning_syste…
#>  8 error   water_depth_m                    NA Required field water_depth_m is …
#>  9 error   station_name                      1 Empty value for required field s…
#> 10 warning monitoring_station_type_code     NA Recommended field monitoring_sta…
#> 11 warning monitoring_purpose_code          NA Recommended field monitoring_pur…
#> 12 warning monitoring_program_code          NA Recommended field monitoring_pro…
#> 13 warning reporting_institute_name_en      NA Recommended field reporting_inst…
#> 14 warning analytical_laboratory_name_en    NA Recommended field analytical_lab…
```
