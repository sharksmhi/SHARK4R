# Retrieve HAB Data From IOC-UNESCO Databases

## IOC-UNESCO Databases

The Intergovernmental Oceanographic Commission (IOC) of UNESCO is the
United Nations body dedicated to supporting global ocean science,
services, and governance. The [IOC-UNESCO Taxonomic Reference List of
Harmful Microalgae](https://www.marinespecies.org/hab/) (Lundholm et
al., 2009) focuses on species known to produce toxins or exhibit toxic
effects, along with a few species suspected of toxin production. All
toxic species in the list are verified with
[WoRMS](https://www.marinespecies.org) to ensure accurate taxonomy.

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

# Print result
print(hab_list)
```

    ## # A tibble: 378 × 29
    ##    AphiaID ScientificName      Authority AphiaID_accepted ScientificName_accep…¹
    ##      <dbl> <chr>               <chr>                <dbl> <chr>                 
    ##  1  841664 Aetokthonos hydril… S.B.Wild…           841664 Aetokthonos hydrillic…
    ##  2  232546 Akashiwo sanguinea  (K.Hiras…           232546 Akashiwo sanguinea    
    ##  3  109707 Alexandrium affine  (H.Inoue…           109707 Alexandrium affine    
    ##  4  231872 Alexandrium anders… Balech, …           246835 Alexandrium andersonii
    ##  5  246835 Alexandrium anders… Balech, …           246835 Alexandrium andersonii
    ##  6  233452 Alexandrium angust… F.J.R.Ta…           109711 Alexandrium minutum   
    ##  7  833063 Alexandrium austra… Sh.Murra…           833063 Alexandrium australie…
    ##  8  231873 Alexandrium catene… (Whedon …           231873 Alexandrium catenella 
    ##  9  233472 Alexandrium excava… (Braarud…           231873 Alexandrium catenella 
    ## 10 1469211 Alexandrium fragae  S.Branco…          1469211 Alexandrium fragae    
    ## # ℹ 368 more rows
    ## # ℹ abbreviated name: ¹​ScientificName_accepted
    ## # ℹ 24 more variables: Authority_accepted <chr>, Fossil <dbl>, Kingdom <chr>,
    ## #   Phylum <chr>, Class <chr>, Order <chr>, Family <chr>, taxonRank <chr>,
    ## #   Genus <chr>, Subgenus <lgl>, Species <chr>, Subspecies <lgl>, Marine <dbl>,
    ## #   Brackish <dbl>, Fresh <dbl>, Terrestrial <dbl>, taxonomicStatus <chr>,
    ## #   Qualitystatus <chr>, Unacceptreason <chr>, DateLastModified <date>, …

A separate list of harmful non-toxic species is also available (Zingone
& Escalera, 2025). This compilation focuses on taxa for which no toxin
production is known, yet which have been linked to negative impacts on
marine organisms, including mortality and ecosystem disturbance. The
list is designed to be used alongside the IOC-UNESCO Taxonomic Reference
List of Harmful Microalgae and is mutually exclusive with it. As a
result, any species known to produce toxins is omitted, even when
reported harmful effects are attributed to non-toxic processes such as
oxygen depletion.

``` r
# Retrieve complete Harmful non-toxic list
hab_non_toxic_list <- get_hab_list(harmful_non_toxic_only = TRUE,
                                   verbose = FALSE)

# Print result
print(hab_non_toxic_list)
```

    ## # A tibble: 67 × 24
    ##    AphiaID ScientificName      Authority AphiaID_accepted ScientificName_accep…¹
    ##      <int> <chr>               <chr>                <int> <chr>                 
    ##  1  109708 Alexandrium balech… (Steidin…           109708 Alexandrium balechii  
    ##  2  375699 Aureococcus anopha… Hargrave…           375699 Aureococcus anophagef…
    ##  3  375701 Aureoumbra lagunen… D.A.Stoc…           375701 Aureoumbra lagunensis 
    ##  4 1360848 Blixaea quinquecor… (T.H.Abé…          1360848 Blixaea quinquecornis 
    ##  5  149619 Cerataulina pelagi… (Cleve) …           149619 Cerataulina pelagica  
    ##  6  163013 Chaetoceros calcit… (Paulsen…           163015 Chaetoceros simplex v…
    ##  7  178180 Chaetoceros coarct… Lauder, …           178180 Chaetoceros coarctatus
    ##  8  156607 Chaetoceros concav… Mangin, …           156607 Chaetoceros concavico…
    ##  9  156611 Chaetoceros convol… Castraca…           156611 Chaetoceros convolutus
    ## 10  465389 Chaetoceros crioph… Castraca…           465389 Chaetoceros criophilus
    ## # ℹ 57 more rows
    ## # ℹ abbreviated name: ¹​ScientificName_accepted
    ## # ℹ 19 more variables: Authority_accepted <chr>, Fossil <int>, Kingdom <chr>,
    ## #   Phylum <chr>, Class <chr>, Order <chr>, Family <chr>, taxonRank <chr>,
    ## #   Genus <chr>, Marine <int>, Brackish <int>, Fresh <int>, Terrestrial <int>,
    ## #   taxonomicStatus <chr>, Unacceptreason <chr>, DateLastModified <dttm>,
    ## #   LSID <chr>, `Parent AphiaID` <int>, Citation <chr>

## Retrieve HAB Toxins From IOC Toxins Database

The complete Toxin list can be downloaded from the [IOC-UNESCO Toxins
database](https://toxins.hais.ioc-unesco.org/) using the
[`get_toxin_list()`](https://sharksmhi.github.io/SHARK4R/reference/get_toxin_list.md)
function.

``` r
# Retrieve complete toxin list
toxin_list <- get_toxin_list()

# Print result
print(toxin_list)
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
    ##   Lindh, M. and Torstensson, A. (2026). SHARK4R: Accessing and
    ##   Validating Marine Environmental Data from 'SHARK' and Related
    ##   Databases. R package version 1.0.3.
    ##   https://CRAN.R-project.org/package=SHARK4R
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {SHARK4R: Accessing and Validating Marine Environmental Data from 'SHARK' and Related Databases},
    ##     author = {Markus Lindh and Anders Torstensson},
    ##     year = {2026},
    ##     note = {R package version 1.0.3},
    ##     url = {https://CRAN.R-project.org/package=SHARK4R},
    ##   }

------------------------------------------------------------------------

## References

- Lundholm, N.; Bernard, C.; Churro, C.; Escalera, L.; Hoppenrath, M.;
  Iwataki, M.; Larsen, J.; Mertens, K.; Murray, S.; Probert, I.; Salas,
  R.; Tillmann, U.; Zingone, A. (Eds) (2009 onwards). IOC-UNESCO
  Taxonomic Reference List of Harmful Microalgae. Accessed at
  <https://www.marinespecies.org/hab/> on 2026-01-19.
  [doi:10.14284/362](https://doi.org/10.14284/362)
- Zingone A. and L. Escalera (2025) Non toxigenic animal-killing
  microalgal species. In: Lundholm, N.; Bernard, C.; Churro, C.;
  Escalera, L.; Hoppenrath, M.; Iwataki, M.; Larsen, J.; Mertens, K.;
  Murray, S.; Probert, I.; Salas, R.; Tillmann, U.; Zingone, A. (Eds)
  (2009 onwards). IOC-UNESCO Taxonomic Reference List of Harmful
  Microalgae. Accessed at <https://www.marinespecies.org/hab/> on
  2026-01-19. [doi:10.14284/362](https://doi.org/10.14284/362)
