# Retrieve Taxonomic Data From Dyntaxa

## Dyntaxa

Dyntaxa is a taxonomic database of Swedish organisms hosted at [SLU
Artdatabanken](https://www.slu.se/artdatabanken/), providing information
on their names and relationships. The database includes details such as
the current classification, recommended names, and commonly used
synonymous or misapplied names. Dyntaxa is continuously updated with new
species for Sweden, new Swedish names, synonymous scientific names, and
new data on relationships. The data in Dyntaxa serves as the foundation
and framework for taxonomic information in
[SHARK](https://shark.smhi.se/en/). It can be accessed via a [web
interface](https://artfakta.se/) or through the
[API](https://api-portal.artdatabanken.se/), as demonstrated in this
tutorial using `SHARK4R`. Please note that the authors of `SHARK4R` are
not affiliated with Dyntaxa.

## Getting Started

### Installation

You can install the latest version of `SHARK4R` from CRAN using:

``` r
install.packages("SHARK4R")
```

Load the `SHARK4R` and `dplyr` libraries:

``` r
library(SHARK4R)
```

### Retrieve Taxonomy Table from SHARK

Taxon and data tables can be retrieved with the same filtering options
available in [SHARK](https://shark.smhi.se/en/). To see the available
filtering options, please refer to
[`get_shark_options()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_options.md)
and the [Retrieve Data From
SHARK](https://sharksmhi.github.io/SHARK4R/articles/retrieve_shark_data.html)
tutorial.

``` r
# Retrieve taxonomy reports for phytoplankton between 2019 and 2020
shark_taxon <- get_shark_data(tableView = "report_taxon",
                              fromYear = 2019,
                              toYear = 2020,
                              dataTypes = "Phytoplankton",
                              verbose = FALSE)

# Print data
print(shark_taxon)
```

    ## # A tibble: 663 × 6
    ##    reported_scientific_name  scientific_name dyntaxa_id aphia_id taxon_hierarchy
    ##    <chr>                     <chr>                <dbl>    <dbl> <chr>          
    ##  1 Acanthoceras zachariasii  Acanthoceras z…     264148   178990 Chromista - Sa…
    ##  2 Acanthoica quattrospina   Acanthoica qua…     236952   235802 Chromista - Ha…
    ##  3 Acanthostomella           Acanthostomella    1010638       NA Chromista - Sa…
    ##  4 Acanthostomella norvegica Acanthostomell…     238502   183556 Chromista - Sa…
    ##  5 Achnanthes                Achnanthes         1010466   149191 Chromista - Sa…
    ##  6 Actinastrum hantzschii    Actinastrum ha…     238839   160543 Plantae - Viri…
    ##  7 Actinocyclus              Actinocyclus       1010407   148944 Chromista - Sa…
    ##  8 Actinocyclus normanii     Actinocyclus n…     237433   148945 Chromista - Sa…
    ##  9 Actinocyclus octonarius   Actinocyclus o…     237434   149164 Chromista - Sa…
    ## 10 Actinocyclus octonarius … Actinocyclus o…     248668   162770 Chromista - Sa…
    ## # ℹ 653 more rows
    ## # ℹ 1 more variable: counted_rows <dbl>

``` r
# Retrieve all phytoplankton data from July 2015
shark_data <- get_shark_data(tableView = "sharkdata_phytoplankton",
                             fromYear = 2015, 
                             toYear = 2015,
                             months = 7,
                             dataTypes = "Phytoplankton",
                             verbose = FALSE)

# Print data
print(shark_data)
```

    ## # A tibble: 1,627 × 114
    ##    delivery_datatype check_status_sv data_checked_by_sv visit_year visit_month
    ##    <chr>             <chr>           <chr>                   <dbl>       <dbl>
    ##  1 Phytoplankton     Klar            Leverantör               2015           7
    ##  2 Phytoplankton     Klar            Leverantör               2015           7
    ##  3 Phytoplankton     Klar            Leverantör               2015           7
    ##  4 Phytoplankton     Klar            Leverantör               2015           7
    ##  5 Phytoplankton     Klar            Leverantör               2015           7
    ##  6 Phytoplankton     Klar            Leverantör               2015           7
    ##  7 Phytoplankton     Klar            Leverantör               2015           7
    ##  8 Phytoplankton     Klar            Leverantör               2015           7
    ##  9 Phytoplankton     Klar            Leverantör               2015           7
    ## 10 Phytoplankton     Klar            Leverantör               2015           7
    ## # ℹ 1,617 more rows
    ## # ℹ 109 more variables: station_name <chr>, reported_station_name <chr>,
    ## #   sample_location_id <dbl>, station_id <dbl>, sample_project_name_sv <chr>,
    ## #   sample_orderer_name_sv <chr>, platform_code <chr>, expedition_id <dbl>,
    ## #   shark_sample_id_md5 <chr>, sample_date <date>, sample_time <time>,
    ## #   sample_latitude_dm <chr>, sample_longitude_dm <chr>,
    ## #   sample_latitude_dd <dbl>, sample_longitude_dd <dbl>, …

### Dyntaxa API Key

Dyntaxa requires a subscription key to access its API. To obtain your
own key, sign up for the taxonomy product at the [SLU Swedish Species
Information Centre’s Developer
Portal](https://api-portal.artdatabanken.se/). In the example below, the
key is retrieved from an environment variable.

``` r
# Retrieve the API key
dyntaxa_key <- Sys.getenv("DYNTAXA_KEY")
```

## Update SHARK Taxonomy Data

If the taxonomic data downloaded from SHARK are outdated, they can be
updated to the latest Dyntaxa information using `SHARK4R`.
Alternatively, data can be retrieved from WoRMS. For details, see the
[WoRMS
Tutorial](https://sharksmhi.github.io/SHARK4R/articles/retrieve_worms_data.html).

``` r
# Update taxonomy information for the retrieved phytoplankton data
updated_taxonomy <- update_dyntaxa_taxonomy(
  dyntaxa_ids = shark_data$dyntaxa_id,
  subscription_key = dyntaxa_key,
  verbose = FALSE)

# Print the updated taxonomy data
print(updated_taxonomy)
```

    ## # A tibble: 1,627 × 10
    ##    dyntaxa_id scientific_name taxon_kingdom taxon_phylum taxon_class taxon_order
    ##         <dbl> <chr>           <chr>         <chr>        <chr>       <chr>      
    ##  1     238225 Alexandrium ps… Chromista     Miozoa       Dinophyceae Gonyaulaca…
    ##  2     238302 Tripos fusus    Chromista     Miozoa       Dinophyceae Gonyaulaca…
    ##  3     238302 Tripos fusus    Chromista     Miozoa       Dinophyceae Gonyaulaca…
    ##  4     238307 Tripos lineatus Chromista     Miozoa       Dinophyceae Gonyaulaca…
    ##  5     238308 Tripos longipes Chromista     Miozoa       Dinophyceae Gonyaulaca…
    ##  6     238319 Tripos muelleri Chromista     Miozoa       Dinophyceae Gonyaulaca…
    ##  7    6018294 Chaetoceros so… Chromista     Gyrista      Mediophyce… Chaetocero…
    ##  8    5000054 Ciliophora      Chromista     Ciliophora   NA          NA         
    ##  9    5000054 Ciliophora      Chromista     Ciliophora   NA          NA         
    ## 10    4000164 Coscinodiscoph… Chromista     Gyrista      Coscinodis… NA         
    ## # ℹ 1,617 more rows
    ## # ℹ 4 more variables: taxon_family <chr>, taxon_genus <chr>,
    ## #   taxon_species <chr>, taxon_hierarchy <chr>

## Match Taxon Names

``` r
# Randomly select 10 phytoplankton taxa from shark_taxon
taxon_names <- sample(shark_taxon$scientific_name, size = 10)

# Match taxon_names with Dyntaxa API
matches <- match_dyntaxa_taxa(taxon_names = taxon_names, 
                              subscription_key = dyntaxa_key, 
                              multiple_options = FALSE,
                              verbose = FALSE)

# Print the result
print(matches)
```

    ## # A tibble: 10 × 5
    ##    search_pattern             taxon_id best_match              author valid_name
    ##    <chr>                         <int> <chr>                   <chr>  <chr>     
    ##  1 Cuspidothrix issatschenkoi   263645 Cuspidothrix issatsche… (Usač… Cuspidoth…
    ##  2 Oblea                       1010594 Oblea                   Balec… Oblea     
    ##  3 Chrysotila carterae          236939 Chrysotila carterae     (Braa… Chrysotil…
    ##  4 Bacillaria paxillifera       237763 Bacillaria paxillifera  (O.F.… Bacillari…
    ##  5 Protoperidinium conicoides   238248 Protoperidinium conico… (Paul… Protoperi…
    ##  6 Scrippsiella                1010578 Scrippsiella            Balec… Scrippsie…
    ##  7 Binuclearia lauterbornii     238927 Binuclearia lauterborn… (Schm… Binuclear…
    ##  8 Dictyochales                3000573 Dictyochales            Haeck… Dictyocha…
    ##  9 Diatoma vulgaris             238027 Diatoma vulgaris        Bory   Diatoma v…
    ## 10 Pseudopediastrum boryanum    257418 Pseudopediastrum borya… (Turp… Pseudoped…

## Retrieve Taxonomic information

Taxonomic records can be retrieved for individual taxa using the
[`get_dyntaxa_records()`](https://sharksmhi.github.io/SHARK4R/reference/get_dyntaxa_records.md)
function.

``` r
# Get all Dyntaxa IDs
dyntaxa_id <- unique(matches$taxon_id)

# Remove potential NAs
dyntaxa_id <- dyntaxa_id[!is.na(dyntaxa_id)]

# Get Dyntaxa records
dyntaxa_records <- get_dyntaxa_records(taxon_ids = dyntaxa_id,
                                       subscription_key = dyntaxa_key)

# Print records
print(dyntaxa_records)
```

    ## # A tibble: 10 × 24
    ##    taxonId parentId secondaryParents sortOrder isMicrospecies externalComment   
    ##      <int>    <int> <list>               <int> <lgl>          <chr>             
    ##  1  236939  6018284 <list [0]>           68975 FALSE           NA               
    ##  2  237763  1010465 <list [0]>           71463 FALSE          "Illustration (LM…
    ##  3  238027  1010523 <list [0]>           73370 FALSE          "Illustration (LM…
    ##  4  238248  1010596 <list [0]>           69898 FALSE           NA               
    ##  5  238927  1016130 <list [0]>          113664 FALSE           NA               
    ##  6  257418  6001140 <list [0]>          112799 FALSE           NA               
    ##  7  263645  1016291 <list [0]>           67675 FALSE          "2005. Published …
    ##  8 1010578  6332738 <list [0]>           70020 FALSE          "Scrippsiella han…
    ##  9 1010594  2003235 <list [0]>           69882 FALSE           NA               
    ## 10 3000573  4000161 <list [0]>           71223 FALSE           NA               
    ## # ℹ 18 more variables: redlistCategory <lgl>, excludeFromReportingSystem <lgl>,
    ## #   nrOfChilds <int>, names <list>, typedRelations.parentRelations <list>,
    ## #   typedRelations.childRelations <list>, status.id <int>, status.value <chr>,
    ## #   status.name <chr>, statusReason.id <int>, statusReason.value <chr>,
    ## #   statusReason.name <chr>, category.id <int>, category.value <chr>,
    ## #   category.name <chr>, type.id <int>, type.value <chr>, type.name <chr>

## Retrieve Parent IDs

All parent taxa above the Dyntaxa ID can be retrieved using the
[`get_dyntaxa_parent_ids()`](https://sharksmhi.github.io/SHARK4R/reference/get_dyntaxa_parent_ids.md)
function.

``` r
# Get all parents
parents_id <- get_dyntaxa_parent_ids(taxon_ids = dyntaxa_id, 
                                     subscription_key = dyntaxa_key,
                                     verbose = FALSE)

# List the IDs
print(parents_id)
```

    ## [[1]]
    ## [1] 5000052 5000053 4000147 6020337 3000511 6012635 1016291  263645
    ## 
    ## [[2]]
    ##  [1] 5000055 6011755 6011756 6011759 6011678 5000062 6011725 6011726 4000169
    ## [10] 3000850 2003235 1010594
    ## 
    ## [[3]]
    ## [1] 5000055 6011754 5000056 4000153 6012363 3000558 2003122 6018284  236939
    ## 
    ## [[4]]
    ##  [1] 5000055 6011755 6011756 6011758 6322929 5000104 6323134 6323136 4000165
    ## [10] 3000830 3000599 2003196 1010465  237763
    ## 
    ## [[5]]
    ##  [1] 5000055 6011755 6011756 6011759 6011678 5000062 6011725 6011726 4000169
    ## [10] 3000850 2003235 1010596  238248
    ## 
    ## [[6]]
    ##  [1] 5000055 6011755 6011756 6011759 6011678 5000062 6011725 6011726 4000169
    ## [10] 3000681 6332738 1010578
    ## 
    ## [[7]]
    ## [1] 5000045 6000581 6000582 5000046 4000178 3000405 2003796 1016130  238927
    ## 
    ## [[8]]
    ##  [1] 5000055 6011755 6011756 6011758 6322929 5000104 6323134 6323135 4000161
    ## [10] 3000573
    ## 
    ## [[9]]
    ##  [1] 5000055 6011755 6011756 6011758 6322929 5000104 6323134 6323136 4000165
    ## [10] 4000166 3000610 2003217 1010523  238027
    ## 
    ## [[10]]
    ## [1] 5000045 6000581 6000582 5000046 4000128 3000412 2003286 6001140  257418

## Construct Complete Taxonomic Table

A comprehensive taxonomic table, including related taxa, can be created
with the
[`construct_dyntaxa_table()`](https://sharksmhi.github.io/SHARK4R/reference/construct_dyntaxa_table.md)
function. Use the `add_synonyms` parameter to include synonyms, and the
`add_parents` and `add_descendants` parameters to include parent and
descendant taxa, respectively. If Taxon IDs are missing from the DwC-A
export downloaded through
[`get_dyntaxa_dwca()`](https://sharksmhi.github.io/SHARK4R/reference/get_dyntaxa_dwca.md)
(e.g. species complex and pseudotaxon), they can be matched using the
`add_missing_taxa` argument. Additionally, complete hierarchy
information can be added as a string of parent taxa separated by “-”
using the `add_hierarchy` argument.

``` r
# Retrieve complete taxonomic table (including parents and descendants)
taxonomy_table <- construct_dyntaxa_table(taxon_ids = dyntaxa_id, 
                                          subscription_key = dyntaxa_key, 
                                          shark_output = FALSE, 
                                          add_parents = TRUE,
                                          add_synonyms = TRUE, 
                                          add_descendants = TRUE,
                                          add_descendants_rank = "genus",
                                          add_missing_taxa = FALSE,
                                          add_hierarchy = FALSE,
                                          verbose = FALSE)

# Print the taxonomy table
print(taxonomy_table)
```

    ## # A tibble: 296 × 16
    ##    taxonId        acceptedNameUsageID parentNameUsageID scientificName taxonRank
    ##    <chr>          <chr>               <chr>             <chr>          <chr>    
    ##  1 urn:lsid:dynt… urn:lsid:dyntaxa.s… urn:lsid:dyntaxa… Scrippsiella   genus    
    ##  2 urn:lsid:dynt… urn:lsid:dyntaxa.s… urn:lsid:dyntaxa… Oblea          genus    
    ##  3 urn:lsid:dynt… urn:lsid:dyntaxa.s… urn:lsid:dyntaxa… Diatoma vulga… species  
    ##  4 urn:lsid:dynt… urn:lsid:dyntaxa.s… urn:lsid:dyntaxa… Pseudopediast… species  
    ##  5 urn:lsid:dynt… urn:lsid:dyntaxa.s… urn:lsid:dyntaxa… Dictyochales   order    
    ##  6 urn:lsid:dynt… urn:lsid:dyntaxa.s… urn:lsid:dyntaxa… Binuclearia l… species  
    ##  7 urn:lsid:dynt… urn:lsid:dyntaxa.s… urn:lsid:dyntaxa… Chrysotila ca… species  
    ##  8 urn:lsid:dynt… urn:lsid:dyntaxa.s… urn:lsid:dyntaxa… Bacillaria pa… species  
    ##  9 urn:lsid:dynt… urn:lsid:dyntaxa.s… urn:lsid:dyntaxa… Cuspidothrix … species  
    ## 10 urn:lsid:dynt… urn:lsid:dyntaxa.s… urn:lsid:dyntaxa… Protoperidini… species  
    ## # ℹ 286 more rows
    ## # ℹ 11 more variables: scientificNameAuthorship <chr>, taxonomicStatus <chr>,
    ## #   nomenclaturalStatus <chr>, taxonRemarks <chr>, kingdom <chr>, phylum <chr>,
    ## #   class <chr>, order <chr>, family <chr>, genus <chr>, species <chr>

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
