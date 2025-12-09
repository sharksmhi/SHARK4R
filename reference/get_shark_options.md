# Retrieve available search options from SHARK

The `get_shark_options()` function retrieves available search options
from the SHARK database. It sends a GET request to the SHARK API and
returns the results as a structured named list.

## Usage

``` r
get_shark_options(prod = TRUE, utv = FALSE, unparsed = FALSE)
```

## Arguments

- prod:

  Logical value that selects the production server when `TRUE` and the
  test server when `FALSE`, unless `utv` is `TRUE`.

- utv:

  Logical value that selects the UTV server when `TRUE`.

- unparsed:

  Logical. If `TRUE`, returns the complete JSON response as a nested
  list without parsing. Defaults to `FALSE`.

## Value

A named `list` of available search options from the SHARK API. If
`unparsed = TRUE`, returns the raw JSON structure as a list.

## Details

This function sends a GET request to the `/api/options` endpoint of the
SHARK API to retrieve available search filters and options that can be
used in SHARK data queries.

## See also

[`get_shark_data()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_data.md)
for retrieving actual data from the SHARK API.

<https://shark.smhi.se/en> for the SHARK database portal.

## Examples

``` r
# \donttest{
  # Retrieve available search options (simplified)
  shark_options <- get_shark_options()
  names(shark_options)
#>  [1] "datasets"              "dataTypes"             "parameters"           
#>  [4] "minYear"               "maxYear"               "months"               
#>  [7] "checkStatuses"         "qualityFlags"          "deliverers"           
#> [10] "orderers"              "projects"              "redListedCategories"  
#> [13] "taxa"                  "stations"              "seaAreas"             
#> [16] "vattenDistrikt"        "typOmraden"            "seaBasins"            
#> [19] "counties"              "municipalities"        "waterCategories"      
#> [22] "helcomOspar"           "parametersForDatatype" "headerlangers"        
#> [25] "minYearPerDatatype"   

  # Retrieve full unparsed JSON response
  raw_options <- get_shark_options(unparsed = TRUE)

  # View available datatypes
  print(shark_options$dataTypes)
#>  [1] "Bacterioplankton"      "Chlorophyll"           "Epibenthos"           
#>  [4] "Grey seal"             "Harbour Porpoise"      "Harbour seal"         
#>  [7] "Physical and Chemical" "Phytoplankton"         "Picoplankton"         
#> [10] "Plankton Barcoding"    "Plankton Imaging"      "Primary production"   
#> [13] "Profile"               "Ringed seal"           "Seal pathology"       
#> [16] "Sedimentation"         "Zoobenthos"            "Zooplankton"          
# }
```
