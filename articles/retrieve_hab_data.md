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

# Print result as tibble
tibble(hab_list)
```

    ## # A tibble: 695 × 29
    ##    AphiaID ScientificName      Authority AphiaID_accepted ScientificName_accep…¹
    ##      <dbl> <chr>               <chr>                <dbl> <chr>                 
    ##  1 1653558 Aerosakkonemataceae Struneck…          1653558 Aerosakkonemataceae   
    ##  2  836651 Aetokthonos         S.B.Wild…           836651 Aetokthonos           
    ##  3  841664 Aetokthonos hydril… S.B.Wild…           841664 Aetokthonos hydrillic…
    ##  4  231787 Akashiwo            G.Hansen…           231787 Akashiwo              
    ##  5  232546 Akashiwo sanguinea  (K.Hiras…           232546 Akashiwo sanguinea    
    ##  6  109470 Alexandrium         Halim, 1…           109470 Alexandrium           
    ##  7  109707 Alexandrium affine  (H.Inoue…           109707 Alexandrium affine    
    ##  8  231872 Alexandrium anders… Balech, …           246835 Alexandrium andersonii
    ##  9  246835 Alexandrium anders… Balech, …           246835 Alexandrium andersonii
    ## 10  233452 Alexandrium angust… F.J.R.Ta…           109711 Alexandrium minutum   
    ## # ℹ 685 more rows
    ## # ℹ abbreviated name: ¹​ScientificName_accepted
    ## # ℹ 24 more variables: Authority_accepted <chr>, Fossil <dbl>, Kingdom <chr>,
    ## #   Phylum <chr>, Class <chr>, Order <chr>, Family <chr>, taxonRank <chr>,
    ## #   Genus <chr>, Subgenus <lgl>, Species <chr>, Subspecies <lgl>, Marine <dbl>,
    ## #   Brackish <dbl>, Fresh <dbl>, Terrestrial <dbl>, taxonomicStatus <chr>,
    ## #   Qualitystatus <chr>, Unacceptreason <chr>, DateLastModified <date>, …

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
    ##   Databases. R package version 1.0.2.
    ##   https://CRAN.R-project.org/package=SHARK4R
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {SHARK4R: Accessing and Validating Marine Environmental Data from 'SHARK' and Related Databases},
    ##     author = {Markus Lindh and Anders Torstensson},
    ##     year = {2025},
    ##     note = {R package version 1.0.2},
    ##     url = {https://CRAN.R-project.org/package=SHARK4R},
    ##   }
