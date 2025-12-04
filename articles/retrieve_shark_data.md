# Retrieve Data From SHARK

## SHARK

SHARK is the Swedish Ocean Archive’s platform for data downloads. It
stores biological, physical, and chemical marine environmental
monitoring data. On behalf of the Swedish Agency for Marine and Water
Management, SMHI serves as the national data host for oceanography and
marine biology and is designated by UNESCO as a National Oceanographic
Data Center (NODC). The data can be accessed via a [web
interface](https://shark.smhi.se/) or through the
[API](https://shark.smhi.se/api/docs/), as demonstrated in this tutorial
using `SHARK4R`.

## Getting Started

#### Installation

You can install the latest version of the package from GitHub using the
`remotes` package:

``` r
# install.packages("remotes")
remotes::install_github("sharksmhi/SHARK4R",
                        ref = remotes::github_release(),
                        dependencies = TRUE)
```

Load the `SHARK4R` library:

``` r
library(SHARK4R)
```

## Retrieve Data Table

Data can be retrieved with the same filtering options available in
[SHARK](https://shark.smhi.se/). To see the available filtering options,
please refer to
[`get_shark_options()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_options.md)
and the information below.

``` r
# Retrieve chlorophyll data for April to June from 2019 to 2020
shark_data <- get_shark_data(fromYear = 2019, 
                             toYear = 2020,
                             months = c(4, 5, 6), 
                             dataTypes = c("Chlorophyll"),
                             verbose = FALSE)

# Print data
print(shark_data)
```

    ## # A tibble: 179 × 72
    ##    delivery_datatype check_status_sv data_checked_by_sv visit_year visit_month
    ##    <chr>             <chr>           <chr>                   <dbl>       <dbl>
    ##  1 Chlorophyll       Klar            Leverantör               2020           6
    ##  2 Chlorophyll       Klar            Leverantör               2020           6
    ##  3 Chlorophyll       Klar            Leverantör               2020           6
    ##  4 Chlorophyll       Klar            Leverantör               2020           6
    ##  5 Chlorophyll       Klar            Leverantör               2020           6
    ##  6 Chlorophyll       Klar            Leverantör               2020           6
    ##  7 Chlorophyll       Klar            Leverantör               2020           6
    ##  8 Chlorophyll       Klar            Leverantör               2020           6
    ##  9 Chlorophyll       Klar            Leverantör               2020           6
    ## 10 Chlorophyll       Klar            Leverantör               2020           6
    ## # ℹ 169 more rows
    ## # ℹ 67 more variables: station_name <chr>, reported_station_name <chr>,
    ## #   sample_location_id <dbl>, station_id <dbl>, sample_project_name_sv <lgl>,
    ## #   sample_orderer_name_sv <lgl>, visit_id <dbl>, visit_date <lgl>,
    ## #   shark_sample_id_md5 <chr>, sample_date <date>, sample_time <time>,
    ## #   sample_enddate <lgl>, sample_endtime <lgl>, sample_latitude_dm <chr>,
    ## #   sample_longitude_dm <chr>, sample_latitude_dd <dbl>, …

## Get SHARK API Options

Filtering options, including data types, dataset names, stations, taxa,
and more, can be retrieved using the
[`get_shark_options()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_options.md)
function.

``` r
# Retrieve available search options
shark_options <- get_shark_options()

# List the names of the available options
names(shark_options)
```

    ##  [1] "datasets"              "dataTypes"             "parameters"           
    ##  [4] "minYear"               "maxYear"               "months"               
    ##  [7] "checkStatuses"         "qualityFlags"          "deliverers"           
    ## [10] "orderers"              "projects"              "redListedCategories"  
    ## [13] "taxa"                  "stations"              "seaAreas"             
    ## [16] "vattenDistrikt"        "typOmraden"            "seaBasins"            
    ## [19] "counties"              "municipalities"        "waterCategories"      
    ## [22] "helcomOspar"           "parametersForDatatype" "headerlangers"        
    ## [25] "minYearPerDatatype"

``` r
# View available datatypes
dataTypes <- shark_options$dataTypes
print(dataTypes)
```

    ##  [1] "Bacterioplankton"      "Chlorophyll"           "Epibenthos"           
    ##  [4] "Grey seal"             "Harbour Porpoise"      "Harbour seal"         
    ##  [7] "Physical and Chemical" "Phytoplankton"         "Picoplankton"         
    ## [10] "Plankton Barcoding"    "Plankton Imaging"      "Primary production"   
    ## [13] "Profile"               "Ringed seal"           "Seal pathology"       
    ## [16] "Sedimentation"         "Zoobenthos"            "Zooplankton"

``` r
# View available dataset names
datasetNames <- shark_options$datasets
head(datasetNames) # Print first few dataset names
```

    ## [1] "SHARK_Bacterioplankton_ABUND_2023_UMSC_version_2024-06-04.zip"        
    ## [2] "SHARK_Bacterioplankton_ABU_2006_UMSC_Bactabund_version_2024-09-27.zip"
    ## [3] "SHARK_Bacterioplankton_ABU_2007_UMSC_Bactabund_version_2024-09-27.zip"
    ## [4] "SHARK_Bacterioplankton_ABU_2008_UMSC_Bactabund_version_2024-09-27.zip"
    ## [5] "SHARK_Bacterioplankton_ABU_2009_UMSC_Bactabund_version_2024-09-27.zip"
    ## [6] "SHARK_Bacterioplankton_ABU_2010_UMSC_Bactabund_version_2024-09-27.zip"

## Retrieve Datasets (Zip-archives)

In addition to accessing data in tabular form, you can also download
complete datasets packaged as zip archives. This is useful if you want
to store complete datasets locally for further analysis.

To explore all available dataset names, use the
[`get_shark_options()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_options.md).
Once you know which datasets you need, you can pass their names (or
partial names) to the
[`get_shark_datasets()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_datasets.md)
function.

``` r
# Select a dataset name (e.g., the first two in the list)
dataset_name <- datasetNames[1:2]

# Download the dataset as a zip-archive to a temporary directory
shark_data_zip <- get_shark_datasets(dataset_name,
                                     save_dir = tempdir(),
                                     verbose = FALSE) # Quiet output

# Print the paths to the downloaded files
print(shark_data_zip)
```

    ## $`SHARK_Bacterioplankton_ABUND_2023_UMSC_version_2024-06-04.zip`
    ## [1] "/tmp/RtmpwD1sLG/SHARK_Bacterioplankton_ABUND_2023_UMSC_version_2024-06-04.zip"
    ## 
    ## $`SHARK_Bacterioplankton_ABU_2006_UMSC_Bactabund_version_2024-09-27.zip`
    ## [1] "/tmp/RtmpwD1sLG/SHARK_Bacterioplankton_ABU_2006_UMSC_Bactabund_version_2024-09-27.zip"

Please note that `SHARK4R` also includes useful functions for reading
local SHARK data files, such as
[`read_shark()`](https://sharksmhi.github.io/SHARK4R/reference/read_shark.md)
and
[`read_shark_deliv()`](https://sharksmhi.github.io/SHARK4R/reference/read_shark_deliv.md).

------------------------------------------------------------------------

## Citation

    ## To cite package 'SHARK4R' in publications use:
    ## 
    ##   Lindh, M. and Torstensson, A. (2025). SHARK4R: Accessing and
    ##   Validating Marine Environmental Data from SHARK and Related
    ##   Databases. R package version 1.0.0.
    ##   https://CRAN.R-project.org/package=SHARK4R
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {SHARK4R: Accessing and Validating Marine Environmental Data from SHARK and Related Databases},
    ##     author = {Markus Lindh and Anders Torstensson},
    ##     year = {2025},
    ##     note = {R package version 1.0.0},
    ##     url = {https://CRAN.R-project.org/package=SHARK4R},
    ##   }
