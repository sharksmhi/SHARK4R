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

The information from these databases can be accessed through APIs, as
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

    ## # A tibble: 536 × 55
    ##       id toxin_group recommended_name synonyms recommended_acronym acronyms
    ##    <int> <chr>       <chr>            <list>   <chr>               <list>  
    ##  1   624 Azaspiracid Azaspiracid-68   <NULL>   AZA-68              <NULL>  
    ##  2   623 Azaspiracid Azaspiracid-67   <NULL>   AZA-67              <NULL>  
    ##  3   622 Azaspiracid Azaspiracid-66   <NULL>   AZA-66              <NULL>  
    ##  4   621 Azaspiracid Azaspiracid-65   <NULL>   AZA-65              <NULL>  
    ##  5   620 Azaspiracid Azaspiracid-61   <NULL>   AZA-61              <NULL>  
    ##  6   619 Azaspiracid Azaspiracid-60   <NULL>   AZA-60              <NULL>  
    ##  7   618 Azaspiracid Azaspiracid-49   <NULL>   AZA-49              <NULL>  
    ##  8   617 Azaspiracid Azaspiracid-48   <NULL>   AZA-48              <NULL>  
    ##  9   616 Azaspiracid Azaspiracid-47   <NULL>   AZA-47              <NULL>  
    ## 10   615 Azaspiracid Azaspiracid-46   <NULL>   AZA-46              <NULL>  
    ## # ℹ 526 more rows
    ## # ℹ 49 more variables: cas_number <chr>, alternative_cas_numbers <list>,
    ## #   formula <chr>, exact_mono_isotopic_mass <dbl>, molfile <chr>,
    ## #   alternative_molfiles <lgl>, smiles <chr>, alternative_smiles <list>,
    ## #   inchi_key <chr>, alternative_inchi_keys <list>, inchi <chr>,
    ## #   alternative_inchies <lgl>, spectra_available <lgl>, certified <lgl>,
    ## #   non_certified_reference_material <lgl>, chemical_analysis_research <lgl>, …

On occasion the Toxins database TLS certificate may temporarily lapse.
Should
[`get_toxin_list()`](https://sharksmhi.github.io/SHARK4R/reference/get_toxin_list.md)
fail with a certificate error, the verification step can be bypassed by
running `get_toxin_list(insecure = TRUE)`. Only do this when the
certificate issue is known and trusted, as it disables protection
against tampering.

------------------------------------------------------------------------

## Citation

    ## To cite package 'SHARK4R' in publications use:
    ## 
    ##   Lindh, M. and Torstensson, A. (2026). SHARK4R: Accessing and
    ##   Validating Marine Environmental Data from 'SHARK' and Related
    ##   Databases. R package version 1.2.1.
    ##   https://CRAN.R-project.org/package=SHARK4R
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {SHARK4R: Accessing and Validating Marine Environmental Data from 'SHARK' and Related Databases},
    ##     author = {Markus Lindh and Anders Torstensson},
    ##     year = {2026},
    ##     note = {R package version 1.2.1},
    ##     url = {https://CRAN.R-project.org/package=SHARK4R},
    ##   }

------------------------------------------------------------------------

## References

- Lundholm, N.; Bernard, C.; Churro, C.; Escalera, L.; Hoppenrath, M.;
  Iwataki, M.; Larsen, J.; Mertens, K.; Murray, S.; Probert, I.; Salas,
  R.; Tillmann, U.; Zingone, A. (Eds) (2009 onwards). IOC-UNESCO
  Taxonomic Reference List of Harmful Microalgae. Accessed at
  <https://www.marinespecies.org/hab/> on 2026-09-08.
  [doi:10.14284/362](https://doi.org/10.14284/362)
- Zingone A. and L. Escalera (2025) Non toxigenic animal-killing
  microalgal species. In: Lundholm, N.; Bernard, C.; Churro, C.;
  Escalera, L.; Hoppenrath, M.; Iwataki, M.; Larsen, J.; Mertens, K.;
  Murray, S.; Probert, I.; Salas, R.; Tillmann, U.; Zingone, A. (Eds)
  (2009 onwards). IOC-UNESCO Taxonomic Reference List of Harmful
  Microalgae. Accessed at <https://www.marinespecies.org/hab/> on
  2026-09-08. [doi:10.14284/362](https://doi.org/10.14284/362)
