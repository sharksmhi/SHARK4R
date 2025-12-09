# Load SHARK4R fields from GitHub

This function downloads and sources the SHARK4R required and recommended
field definitions directly from the
[SHARK4R-statistics](https://github.com/nodc-sweden/SHARK4R-statistics)
GitHub repository.

## Usage

``` r
load_shark4r_fields(verbose = TRUE)
```

## Arguments

- verbose:

  Logical; if `TRUE` (default), prints progress messages during download
  and loading.

## Value

Invisibly returns a list with two elements:

- required_fields:

  Object containing required SHARK fields.

- recommended_fields:

  Object containing recommended SHARK fields.

## Details

The definitions are stored in an R script (`fields.R`) located in the
`fields/` folder of the repository. The function sources this file
directly from GitHub into the current R session.

The sourced script defines two main objects:

- `required_fields` — vector or data frame of required SHARK fields.

- `recommended_fields` — vector or data frame of recommended SHARK
  fields.

The output of this function can be directly supplied to the
[`check_fields`](https://sharksmhi.github.io/SHARK4R/reference/check_fields.md)
function through its `field_definitions` argument for validating SHARK4R
data consistency.

If sourcing fails (e.g., due to a network issue or repository changes),
the function throws an error with a descriptive message.

## See also

[`check_fields`](https://sharksmhi.github.io/SHARK4R/reference/check_fields.md)
for validating datasets using the loaded field definitions (as
`field_definitions`).
[`load_shark4r_stats`](https://sharksmhi.github.io/SHARK4R/reference/load_shark4r_stats.md)
for loading precomputed SHARK4R statistics,

## Examples

``` r
# \donttest{
# Load SHARK4R field definitions from GitHub
fields <- load_shark4r_fields(verbose = FALSE)

# Access required or recommended fields for the first entry
fields[[1]]$required
#>  [1] "visit_year"                         "station_name"                      
#>  [3] "sample_project_name_sv"             "sample_orderer_name_sv"            
#>  [5] "platform_code"                      "sample_date"                       
#>  [7] "sample_time"                        "sample_latitude_dd"                
#>  [9] "sample_longitude_dd"                "positioning_system_code"           
#> [11] "water_depth_m"                      "sample_min_depth_m"                
#> [13] "sample_max_depth_m"                 "sampling_laboratory_name_sv"       
#> [15] "sampling_laboratory_accreditated"   "sampler_type_code"                 
#> [17] "sampled_volume_l"                   "scientific_name"                   
#> [19] "value"                              "quality_flag"                      
#> [21] "analysis_method_code"               "method_reference_code"             
#> [23] "analytical_laboratory_name_sv"      "analytical_laboratory_accreditated"
#> [25] "analysed_volume_cm3"                "preservation_method_code"          
#> [27] "counted_portions"                   "reporting_institute_name_sv"       
fields[[1]]$recommended
#> [1] "monitoring_program_code"
# }
if (FALSE) { # \dontrun{
# Use the loaded definitions in check_fields()
check_fields(my_data, field_definitions = fields)
} # }
```
