# Retrieve HAB Data From IOC-UNESCO Databases

## IOC-UNESCO Databases

The Intergovernmental Oceanographic Commission (IOC) of UNESCO is the
United Nations body dedicated to supporting global ocean science,
services, and governance. The [IOC-UNESCO Taxonomic Reference List of
Harmful Microalgae](https://www.marinespecies.org/hab/) focuses on
species known to produce toxins or exhibit toxic effects, along with a
few species suspected of toxin production. All toxic species in the list
are verified with [WoRMS](https://www.marinespecies.org) to ensure
accurate taxonomy.

The [IOC-UNESCO Toxins database](https://toxins.hais.ioc-unesco.org/)
complements this list by providing detailed reference information about
toxins, some of which are associated with harmful algal events.

The information from these databases can be access through APIs, as
demonstrated in this tutorial using `SHARK4R`.

## Getting Started

#### Installation

You can install the latest version of `SHARK4R` from CRAN using:

``` r
install.packages("SHARK4R")
```

Load the `SHARK4R` and `dplyr` libraries:

``` r
library(SHARK4R)
library(dplyr)
```

## Retrieve HAB List From IOC Taxonomic List

The complete HAB list, including scientific names and AphiaIDs, can be
downloaded from the [IOC-UNESCO Taxonomic Reference List of Harmful
Microalgae](https://www.marinespecies.org/hab/). The output fields are
customizable through function parameters—for example, setting
`classification = FALSE` excludes higher taxonomic information from the
results.

``` r
# Retrieve complete HAB list
hab_list <- get_hab_list()
```

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

``` r
# Print result as tibble
tibble(hab_list)
```

    ## # A tibble: 708 × 28
    ##    AphiaID ScientificName               AphiaID_accepted ScientificName_accepted
    ##    <chr>   <chr>                        <chr>            <chr>                  
    ##  1 1653558 Aerosakkonemataceae          1653558          Aerosakkonemataceae    
    ##  2 836651  Aetokthonos                  836651           Aetokthonos            
    ##  3 841664  Aetokthonos hydrillicola     841664           Aetokthonos hydrillico…
    ##  4 231787  Akashiwo                     231787           Akashiwo               
    ##  5 232546  Akashiwo sanguinea           232546           Akashiwo sanguinea     
    ##  6 109470  Alexandrium                  109470           Alexandrium            
    ##  7 109707  Alexandrium affine           109707           Alexandrium affine     
    ##  8 231872  Alexandrium andersoni        246835           Alexandrium andersonii 
    ##  9 246835  Alexandrium andersonii       246835           Alexandrium andersonii 
    ## 10 233452  Alexandrium angustitabulatum 109711           Alexandrium minutum    
    ## # ℹ 698 more rows
    ## # ℹ 24 more variables: Authority_accepted <chr>, Fossil <chr>, Kingdom <chr>,
    ## #   Phylum <chr>, Class <chr>, Order <chr>, Family <chr>, taxonRank <chr>,
    ## #   Genus <chr>, Subgenus <chr>, Species <chr>, Subspecies <chr>, Marine <dbl>,
    ## #   Brackish <dbl>, Fresh <dbl>, Terrestrial <dbl>, taxonomicStatus <chr>,
    ## #   Qualitystatus <chr>, Unacceptreason <chr>, DateLastModified <chr>,
    ## #   LSID <chr>, `Parent AphiaID` <chr>, Storedpath <chr>, Citation <chr>

## Retrieve HAB Toxins From IOC Toxins Database

The complete Toxin list can be downloaded from the [IOC-UNESCO Toxins
database](https://toxins.hais.ioc-unesco.org/) using the
[`get_toxin_list()`](https://sharksmhi.github.io/SHARK4R/reference/get_toxin_list.md)
function.

``` r
# Retrieve complete toxin list
toxin_list <- get_toxin_list()

# Print result as tibble
tibble(toxin_list)
```

    ## # A tibble: 521 × 55
    ##       id toxin_group recommended_name      synonyms recommended_acronym acronyms
    ##    <int> <chr>       <chr>                 <list>   <chr>               <list>  
    ##  1   606 Anatoxin    10-OH-homoanatoxin-a  <NULL>   NA                  <NULL>  
    ##  2   605 Anatoxin    10-OH-anatoxin-a      <NULL>   NA                  <NULL>  
    ##  3   604 Anatoxin    10-OH-H2-homoanatoxi… <NULL>   NA                  <NULL>  
    ##  4   603 Anatoxin    10-OH-H2-anatoxin-a   <NULL>   NA                  <NULL>  
    ##  5   602 Anatoxin    CH3SH-anatoxin-a      <NULL>   NA                  <NULL>  
    ##  6   601 Anatoxin    CH3O-homoanatoxin-a   <NULL>   NA                  <NULL>  
    ##  7   600 Anatoxin    CH3O-anatoxin-a       <NULL>   NA                  <NULL>  
    ##  8   599 Anatoxin    3-OH-homoanatoxin-a   <NULL>   NA                  <NULL>  
    ##  9   598 Anatoxin    3-OH-anatoxin-a       <NULL>   NA                  <NULL>  
    ## 10   597 Anatoxin    carboxyhomoanatoxin-a <NULL>   NA                  <NULL>  
    ## # ℹ 511 more rows
    ## # ℹ 49 more variables: cas_number <chr>, alternative_cas_numbers <list>,
    ## #   formula <chr>, exact_mono_isotopic_mass <dbl>, molfile <chr>,
    ## #   alternative_molfiles <lgl>, smiles <chr>, alternative_smiles <list>,
    ## #   inchi_key <chr>, alternative_inchi_keys <list>, inchi <chr>,
    ## #   alternative_inchies <lgl>, spectra_available <lgl>, certified <lgl>,
    ## #   non_certified_reference_material <lgl>, chemical_analysis_research <lgl>, …

------------------------------------------------------------------------

## Citation

    ## To cite package 'SHARK4R' in publications use:
    ## 
    ##   Lindh, M. and Torstensson, A. (2025). SHARK4R: Accessing and
    ##   Validating Marine Environmental Data from 'SHARK' and Related
    ##   Databases. R package version 1.0.1.
    ##   https://CRAN.R-project.org/package=SHARK4R
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {SHARK4R: Accessing and Validating Marine Environmental Data from 'SHARK' and Related Databases},
    ##     author = {Markus Lindh and Anders Torstensson},
    ##     year = {2025},
    ##     note = {R package version 1.0.1},
    ##     url = {https://CRAN.R-project.org/package=SHARK4R},
    ##   }
