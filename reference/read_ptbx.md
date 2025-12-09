# Read a Plankton Toolbox export file

This function reads a sample file exported as an Excel (.xlsx) file from
Plankton Toolbox and extracts data from a specified sheet. The default
sheet is "sample_data.txt", which contains count data.

## Usage

``` r
read_ptbx(
  file_path,
  sheet = c("sample_data.txt", "sample_info.txt", "counting_method.txt",
    "Sample summary", "README")
)
```

## Arguments

- file_path:

  Character. Path to the Excel file.

- sheet:

  Character. The name of the sheet to read. Must be one of:
  "sample_data.txt", "Sample summary", "sample_info.txt",
  "counting_method.txt", or "README". Default is "sample_data.txt".

## Value

A `tibble` containing the contents of the selected sheet.

## See also

<https://nordicmicroalgae.org/plankton-toolbox/> for downloading
Plankton Toolbox.

<https://github.com/planktontoolbox/plankton-toolbox/> for Plankton
Toolbox source code.

## Examples

``` r
# Read the default data sheet
sample_data <- read_ptbx(system.file("extdata/Anholt_E_2024-09-15_0-10m.xlsx",
                                     package = "SHARK4R"))

# Print output
sample_data
#> # A tibble: 89 × 23
#>    scientific_full_name       taxon_class scientific_name size_class method_step
#>    <chr>                      <chr>       <chr>           <chr>      <chr>      
#>  1 Alexandrium pseudogonyaul… Dinophyceae Alexandrium ps… 2          A1 - Uterm…
#>  2 Cerataulina pelagica       Bacillario… Cerataulina pe… 2          A2 - Uterm…
#>  3 Chaetoceros affinis        Bacillario… Chaetoceros af… 2          A1 - Uterm…
#>  4 Chaetoceros cf. affinis    Bacillario… Chaetoceros af… 4          A1 - Uterm…
#>  5 Chaetoceros cf. peruvianus Bacillario… Chaetoceros pe… 1          A1 - Uterm…
#>  6 Chaetoceros debilis        Bacillario… Chaetoceros de… 3          A2 - Uterm…
#>  7 Chaetoceros similis        Bacillario… Chaetoceros si… 5          A1 - Uterm…
#>  8 Chaetoceros socialis       Bacillario… Chaetoceros so… 1          A4 - Uterm…
#>  9 Chaetoceros tenuissimus    Bacillario… Chaetoceros te… 2          A4 - Uterm…
#> 10 Chaetoceros                Bacillario… Chaetoceros     2          A4 - Uterm…
#> # ℹ 79 more rows
#> # ℹ 18 more variables: count_area_number <chr>, locked_at_area <chr>,
#> #   counted_units <chr>, counted_units_list <chr>, abundance_class <lgl>,
#> #   coefficient <chr>, abundance_units_l <chr>, volume_mm3_l <chr>,
#> #   carbon_ugc_l <chr>, volume_um3_unit <chr>, carbon_pgc_unit <chr>,
#> #   variable_comment <lgl>, trophic_type <chr>, unit_type <chr>,
#> #   species_flag_code <chr>, cf <chr>, bvol_list <chr>, bvol_list_calc <chr>


# Read a specific sheet
sample_info <- read_ptbx(system.file("extdata/Anholt_E_2024-09-15_0-10m.xlsx",
                                     package = "SHARK4R"),
                         sheet = "sample_info.txt")
# Print output
sample_info
#> # A tibble: 41 × 2
#>    key                      value                    
#>    <chr>                    <chr>                    
#>  1 plankton_toolbox_version 1.4.2                    
#>  2 sample_name              Anholt E_2024-09-15_0-10m
#>  3 sample_id                Anholt E_2024-09-15_0-10m
#>  4 sample_date              2024-09-15               
#>  5 sample_time              09:57                    
#>  6 visit_year               2024                     
#>  7 country_code             77                       
#>  8 platform_code            77SE                     
#>  9 sampling_series          0853                     
#> 10 sampling_laboratory      SMHI                     
#> # ℹ 31 more rows
```
