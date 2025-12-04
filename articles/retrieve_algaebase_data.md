# Retrieve Taxonomic Data From AlgaeBase

## AlgaeBase

AlgaeBase is a comprehensive database containing information on a wide
variety of algae species, including terrestrial, marine, and freshwater
organisms, with an emphasis on marine botany. AlgaeBase is continually
updated and funded by various phycological societies, with contributions
from researchers and institutions worldwide. It can be accessed via a
[web interface](https://www.algaebase.org/) or through the
[API](https://www.algaebase.org/api/), as demonstrated in this tutorial
using `SHARK4R`. Please note that the authors of `SHARK4R` are not
affiliated with AlgaeBase.

## Getting Started

### Installation

You can install the latest version of the package from GitHub using the
`remotes` package:

``` r
# install.packages("remotes")
remotes::install_github("sharksmhi/SHARK4R", 
                        ref = remotes::github_release(), 
                        dependencies = TRUE)
```

Load the `SHARK4R` and `dplyr` libraries:

``` r
library(SHARK4R)
library(dplyr)
```

### AlgaeBase API Key

AlgaeBase requires a subscription key to access its API. To obtain your
own key, please visit the [API
documentation](https://www.algaebase.org/api/). In the example below,
the key is retrieved from an environment variable.

``` r
# Retrieve the API key
algaebase_key <- Sys.getenv("ALGAEBASE_KEY")
```

## Match Genus Name

Taxonomic records can be retrieved for individual genera names using the
[`match_algaebase_genus()`](https://sharksmhi.github.io/SHARK4R/reference/match_algaebase_genus.md)
function.

``` r
# Match a genus name with AlgaeBase API
genus_records <- match_algaebase_genus(genus = "Gymnodinium",
                                       subscription_key = algaebase_key)

# Print the result
print(genus_records)
```

    ##     kingdom         phylum       class         order         family    id
    ## 1 Chromista Dinoflagellata Dinophyceae Gymnodiniales Gymnodiniaceae 43632
    ##         genus species infrasp                 taxonomic_status
    ## 1 Gymnodinium      NA      NA currently accepted taxonomically
    ##   nomenclatural_status currently_accepted accepted_name genus_only  input_name
    ## 1                   NA                  1            NA          1 Gymnodinium
    ##   input_match taxon_rank   mod_date                 long_name authorship
    ## 1           1      genus 2024-06-13 Gymnodinium F.Stein, 1878    F.Stein

## Match Species Name

Taxonomic records can be retrieved for individual species names using
the
[`match_algaebase_species()`](https://sharksmhi.github.io/SHARK4R/reference/match_algaebase_species.md)
function.

``` r
# Match a species with AlgaeBase API
species_records <- match_algaebase_species(genus = "Tripos",
                                           species = "muelleri",
                                           subscription_key = algaebase_key)

# Print the result
print(species_records)
```

    ##      id   accepted_name      input_name input_match currently_accepted
    ## 1 65254 Tripos muelleri Tripos muelleri           1                  1
    ##   genus_only   kingdom         phylum       class         order      family
    ## 1          0 Chromista Dinoflagellata Dinophyceae Gonyaulacales Ceratiaceae
    ##    genus  species infrasp            long_name                 taxonomic_status
    ## 1 Tripos muelleri      NA Tripos muelleri Bory currently accepted taxonomically
    ##   nomenclatural_status taxon_rank   mod_date authorship
    ## 1                 <NA>    species 2024-05-28       Bory

## Match Multiple Scientific Names

Multiple names can be matched with the
[`match_algaebase_taxa()`](https://sharksmhi.github.io/SHARK4R/reference/match_algaebase_taxa.md)
function. The scientific names need to be parsed into `genus` and
`species` names before being passed to the API, which can be achieved by
the
[`parse_scientific_names()`](https://sharksmhi.github.io/SHARK4R/reference/parse_scientific_names.md)
function.

``` r
# Retrieve all phytoplankton data from April 2015
shark_data <- get_shark_data(fromYear = 2015, 
                             toYear = 2015,
                             months = 4, 
                             dataTypes = c("Phytoplankton"),
                             verbose = FALSE)

# Randomly select 10 rows from the shark_data dataframe
random_rows <- shark_data[sample(nrow(shark_data), 10), ]

# Parse scientific names into genus and species names
parsed_taxa <- parse_scientific_names(random_rows$scientific_name)

# Print the parsed data
print(parsed_taxa)
```

    ##              genus    species
    ## 1        Guinardia delicatula
    ## 2    Monoraphidium  contortum
    ## 3      Flagellates           
    ## 4    Thalassiosira    baltica
    ## 5           Amylax triacantha
    ## 6      Flagellates           
    ## 7      Karlodinium     micrum
    ## 8  Cryptomonadales           
    ## 9         Octactis   speculum
    ## 10      Mesodinium     rubrum

``` r
# Match the taxa with AlgaeBase
algaebase_match <- match_algaebase_taxa(genera = parsed_taxa$genus,
                                        species = parsed_taxa$species,
                                        subscription_key = algaebase_key,
                                        verbose = FALSE)

# Print the result
tibble(algaebase_match)
```

    ## # A tibble: 9 × 20
    ##   genus   species     id accepted_name input_name input_match currently_accepted
    ##   <chr>   <chr>    <int> <chr>         <chr>            <dbl>              <dbl>
    ## 1 Amylax  triaca…  52120 Amylax triac… Amylax tr…           1                  1
    ## 2 Crypto… NA          NA NA            NA                  NA                 NA
    ## 3 Flagel… NA          NA NA            NA                  NA                 NA
    ## 4 Guinar… delica…  43911 Guinardia de… Guinardia…           1                  1
    ## 5 Karlod… micrum   44335 Karlodinium … Karlodini…           1                  0
    ## 6 Mesodi… rubrum   56539 Mesodinium r… Mesodiniu…           1                  1
    ## 7 Monora… contor…  27719 Monoraphidiu… Monoraphi…           1                  1
    ## 8 Octact… specul… 164546 Octactis spe… Octactis …           1                  1
    ## 9 Thalas… baltica  39921 Thalassiosir… Thalassio…           1                  1
    ## # ℹ 13 more variables: genus_only <dbl>, kingdom <chr>, phylum <chr>,
    ## #   class <chr>, order <chr>, family <chr>, infrasp <lgl>, long_name <chr>,
    ## #   taxonomic_status <chr>, nomenclatural_status <lgl>, taxon_rank <chr>,
    ## #   mod_date <date>, authorship <chr>

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
