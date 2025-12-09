# Retrieve marine biotoxin data from IOC-UNESCO Toxins Database

This function collects data from the [IOC-UNESCO Toxins
Database](https://toxins.hais.ioc-unesco.org/) and returns information
about toxins.

## Usage

``` r
get_toxin_list(return_count = FALSE)
```

## Arguments

- return_count:

  Logical. If `TRUE`, the function returns the count of toxins available
  in the database. If `FALSE` (default), it returns detailed toxin data.

## Value

If `return_count = TRUE`, the function returns a numeric value
representing the number of toxins in the database. Otherwise, it returns
a `tibble` of toxins with detailed information.

## See also

<https://toxins.hais.ioc-unesco.org/> for IOC-UNESCO Toxins Database.

## Examples

``` r
# \donttest{
# Retrieve the full list of toxins
toxin_list <- get_toxin_list()
head(toxin_list)
#> # A tibble: 6 × 55
#>      id toxin_group recommended_name       synonyms recommended_acronym acronyms
#>   <int> <chr>       <chr>                  <list>   <chr>               <list>  
#> 1   606 Anatoxin    10-OH-homoanatoxin-a   <NULL>   NA                  <NULL>  
#> 2   605 Anatoxin    10-OH-anatoxin-a       <NULL>   NA                  <NULL>  
#> 3   604 Anatoxin    10-OH-H2-homoanatoxin… <NULL>   NA                  <NULL>  
#> 4   603 Anatoxin    10-OH-H2-anatoxin-a    <NULL>   NA                  <NULL>  
#> 5   602 Anatoxin    CH3SH-anatoxin-a       <NULL>   NA                  <NULL>  
#> 6   601 Anatoxin    CH3O-homoanatoxin-a    <NULL>   NA                  <NULL>  
#> # ℹ 49 more variables: cas_number <chr>, alternative_cas_numbers <list>,
#> #   formula <chr>, exact_mono_isotopic_mass <dbl>, molfile <chr>,
#> #   alternative_molfiles <lgl>, smiles <chr>, alternative_smiles <list>,
#> #   inchi_key <chr>, alternative_inchi_keys <list>, inchi <chr>,
#> #   alternative_inchies <lgl>, spectra_available <lgl>, certified <lgl>,
#> #   non_certified_reference_material <lgl>, chemical_analysis_research <lgl>,
#> #   chemical_analysis_standardized <lgl>, chemical_analysis_validated <lgl>, …

# Retrieve only the count of toxins
toxin_count <- get_toxin_list(return_count = TRUE)
print(toxin_count)
#> [1] 521
# }
```
