# Retrieve Data From Nordic Microalgae

## Nordic Microalgae

[Nordic Microalgae](https://nordicmicroalgae.org/) is an online platform
providing high-quality images, illustrations, and detailed information
about aquatic microalgae and related organisms in the Nordic region,
including phytoplankton, microzooplankton, and benthic microalgae. It
features a comprehensive species checklist, up-to-date taxonomic data
linked to WoRMS and AlgaeBase, and supplementary resources such as
biovolume lists and occurrence maps. Established in 1996 and supported
by long-term funding from the Swedish Research Council through the
Swedish Biodiversity Data Infrastructure (SBDI).

All data on Nordic Microalgae is accessible through our
[API](https://nordicmicroalgae.org/api/). Below are examples
demonstrating how to extract data using the SHARK4R package.

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

## Retrieve Complete Nordic Microalgae Taxon Table

A complete Nordic Microalgae taxa list can be retrieved through the API.

``` r
# Get taxa information
taxa <- get_nua_taxa(unparsed = FALSE)

# Print data
tibble(taxa)
```

    ## # A tibble: 6,860 × 5
    ##    scientific_name           authority                       rank  slug  nua_url
    ##    <chr>                     <chr>                           <chr> <chr> <chr>  
    ##  1 Abollifer                 Vørs, 1992                      Genus abol… https:…
    ##  2 Abollifer prolabens       Vørs, 1992                      Spec… abol… https:…
    ##  3 Acanthoceras              Honigm., 1910                   Genus acan… https:…
    ##  4 Acanthoceras zachariasii  (Brun) Simonsen, 1979           Spec… acan… https:…
    ##  5 Acanthocerataceae         Round, Crawford & Mann, 1990    Fami… acan… https:…
    ##  6 Acanthocorbis             S.Hara & E.Takahashi, 1984      Genus acan… https:…
    ##  7 Acanthocorbis apoda       (Leadbeater) Hara & Takahashi,… Spec… acan… https:…
    ##  8 Acanthocorbis asymmetrica (Thomsen) Hara & Takahashi, 19… Spec… acan… https:…
    ##  9 Acanthocorbis campanula   (Espeland) Thomsen, 1991        Spec… acan… https:…
    ## 10 Acanthocorbis haurakiana  Thomsen, 1991                   Spec… acan… https:…
    ## # ℹ 6,850 more rows

The full taxonomic information can be accessed as an unparsed list by
enabling the `unparsed` parameter.

## Get Nordic Microalgae External Links or Facts

Each taxon sheet on Nordic Microalgae contains facts, such as links to
external webpages (e.g. AlgaeBase, WoRMS and Dyntaxa). These links can
be retrieved through the API.

``` r
# Randomly select 10 taxa from shark_taxon$scientific_name
slugs <- sample(taxa$slug, size = 10)

# Get external links
external_links <- get_nua_external_links(slugs, 
                                         verbose = FALSE, 
                                         unparsed = FALSE)

# Print list
tibble(external_links)
```

    ## # A tibble: 52 × 6
    ##    slug                     provider  label  external_id external_url collection
    ##    <chr>                    <chr>     <chr>  <chr>       <chr>        <chr>     
    ##  1 pterosperma-vanhoeffenii AlgaeBase Algae… 56416       https://www… External …
    ##  2 pterosperma-vanhoeffenii Dyntaxa   Dynta… 238959      https://art… External …
    ##  3 pterosperma-vanhoeffenii GBIF      GBIF   2688130     https://www… External …
    ##  4 pterosperma-vanhoeffenii WoRMS     WoRMS  345881      https://www… External …
    ##  5 rosenvingiellopsis       AlgaeBase Algae… 52010       https://www… External …
    ##  6 rosenvingiellopsis       Dyntaxa   Dynta… 6046635     https://art… External …
    ##  7 rosenvingiellopsis       ENA       ENA    1936980     https://www… External …
    ##  8 rosenvingiellopsis       GBIF      GBIF   9916441     https://www… External …
    ##  9 rosenvingiellopsis       NCBI      NCBI   1936980     https://www… External …
    ## 10 rosenvingiellopsis       WoRMS     WoRMS  1312598     https://www… External …
    ## # ℹ 42 more rows

The full list of facts can be accessed as an unparsed list by setting
the `unparsed` parameter to `TRUE`.

## Get Nordic Microalgae Harmfulness Information

Taxa listed in the [IOC-UNESCO Taxonomic Reference List of Harmful Micro
Algae](https://www.marinespecies.org/hab/) contain information about
harmfulness. This information can be retrieved through the API.

``` r
# Get external links
harmfulness <- get_nua_harmfulness(c("dinophysis-acuta", 
                                     "alexandrium-ostenfeldii"), 
                                   verbose = FALSE)

# Print list
tibble(harmfulness)
```

    ## # A tibble: 11 × 6
    ##    slug                    provider   label  external_id external_url collection
    ##    <chr>                   <chr>      <chr>  <chr>       <chr>        <chr>     
    ##  1 dinophysis-acuta        IOC        IOC H… 109604      https://www… Harmful a…
    ##  2 dinophysis-acuta        IOC-UNESCO IOC-U… 6           https://tox… Harmful a…
    ##  3 dinophysis-acuta        IOC-UNESCO IOC-U… 1           https://tox… Harmful a…
    ##  4 dinophysis-acuta        IOC-UNESCO IOC-U… 5           https://tox… Harmful a…
    ##  5 alexandrium-ostenfeldii IOC        IOC H… 109712      https://www… Harmful a…
    ##  6 alexandrium-ostenfeldii IOC-UNESCO IOC-U… 464         https://tox… Harmful a…
    ##  7 alexandrium-ostenfeldii IOC-UNESCO IOC-U… 463         https://tox… Harmful a…
    ##  8 alexandrium-ostenfeldii IOC-UNESCO IOC-U… 462         https://tox… Harmful a…
    ##  9 alexandrium-ostenfeldii IOC-UNESCO IOC-U… 12          https://tox… Harmful a…
    ## 10 alexandrium-ostenfeldii IOC-UNESCO IOC-U… 15          https://tox… Harmful a…
    ## 11 alexandrium-ostenfeldii IOC-UNESCO IOC-U… 17          https://tox… Harmful a…

## Get Nordic Microalgae Media Links

Links to all images present on Nordic Microalgae can be retrieved
through the API. The images are available in four sizes: original (o),
small (s), medium (m), and large (l).

``` r
# Get all media links
media <- get_nua_media_links(unparsed = FALSE)

# Print list
tibble(media)
```

    ## # A tibble: 2,420 × 10
    ##    slug              image_l_url image_o_url image_s_url image_m_url contributor
    ##    <chr>             <chr>       <chr>       <chr>       <chr>       <chr>      
    ##  1 lennoxia-faveola… https://no… https://no… https://no… https://no… Maria Karl…
    ##  2 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ##  3 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ##  4 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ##  5 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ##  6 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ##  7 pleurosigma       https://no… https://no… https://no… https://no… Maria Karl…
    ##  8 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ##  9 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ## 10 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ## # ℹ 2,410 more rows
    ## # ℹ 4 more variables: photographer_artist <chr>, copyright_holder <chr>,
    ## #   license <chr>, galleries <chr>

Complete media information can be retrieved as an unparsed list by
setting the `unparsed` parameter to `TRUE`.

## Get NOMP and EG Phyto Biovolume lists

To standardize phytoplankton monitoring efforts in the Baltic Sea,
Skagerrak, and the North Sea, various lists containing size class
information for each taxon have been established. These lists can be
retrieved directly in R using `SHARK4R`:

``` r
# Get EG Phyto Biovolume list
peg_list <- get_peg_list()
```

    ## Reading PEG biovolume Excel file for year: 2025

``` r
# Print list
tibble(peg_list)
```

    ## # A tibble: 3,527 × 36
    ##    Division    Class Order Genus Species SFLAG STAGE Author AphiaID AphiaID_link
    ##    <chr>       <chr> <chr> <chr> <chr>   <chr> <chr> <chr>    <dbl> <chr>       
    ##  1 CYANOBACTE… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
    ##  2 CYANOBACTE… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
    ##  3 CYANOBACTE… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
    ##  4 CYANOBACTE… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
    ##  5 CYANOBACTE… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
    ##  6 CYANOBACTE… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
    ##  7 CYANOBACTE… Cyan… CHRO… Apha… Aphano… NA    NA    West …  146563 http://www.…
    ##  8 CYANOBACTE… Cyan… CHRO… Apha… Aphano… NA    NA    W. & …  146564 http://www.…
    ##  9 CYANOBACTE… Cyan… CHRO… Apha… Aphano… NA    NA    W. & …  146564 http://www.…
    ## 10 CYANOBACTE… Cyan… CHRO… Apha… Aphano… NA    NA    W. & …  146564 http://www.…
    ## # ℹ 3,517 more rows
    ## # ℹ 26 more variables: Trophy <chr>, Geometric_shape <chr>, FORMULA <chr>,
    ## #   SizeClassNo <dbl>, Nonvalid_SIZCL <lgl>, Not_accepted_name <lgl>,
    ## #   Unit <chr>, SizeRange <chr>, `Length(l1)µm` <dbl>, `Length(l2)µm` <dbl>,
    ## #   `Width(w)µm` <dbl>, `Height(h)µm` <dbl>, `Diameter(d1)µm` <dbl>,
    ## #   `Diameter(d2)µm` <dbl>, `No_of_cells/counting_unit` <dbl>,
    ## #   `Calculated_volume_µm3 (with formula) - NOT IMPORTED, NOT handled by ICES` <dbl>, …

``` r
# Get NOMP Biovolume list
nomp_list <- get_nomp_list()
```

    ## File for year 2025 not available. Trying previous year...

    ## File for year 2024 downloaded and cached.

``` r
# Print list
tibble(nomp_list)
```

    ## # A tibble: 3,846 × 39
    ##    List      `HELCOM area` `OSPAR area` Division Class Order Genus Species SFLAG
    ##    <chr>     <chr>         <chr>        <chr>    <chr> <chr> <chr> <chr>   <chr>
    ##  1 PEG_BVOL… x             NA           CYANOBA… Cyan… CHRO… Apha… Aphano… NA   
    ##  2 PEG_BVOL… x             NA           CYANOBA… Cyan… CHRO… Apha… Aphano… NA   
    ##  3 PEG_BVOL… x             NA           CYANOBA… Cyan… CHRO… Apha… Aphano… NA   
    ##  4 PEG_BVOL… x             NA           CYANOBA… Cyan… CHRO… Apha… Aphano… NA   
    ##  5 PEG_BVOL… x             NA           CYANOBA… Cyan… CHRO… Apha… Aphano… NA   
    ##  6 PEG_BVOL… x             NA           CYANOBA… Cyan… CHRO… Apha… Aphano… NA   
    ##  7 PEG_BVOL… x             NA           CYANOBA… Cyan… CHRO… Apha… Aphano… NA   
    ##  8 PEG_BVOL… x             NA           CYANOBA… Cyan… CHRO… Apha… Aphano… NA   
    ##  9 PEG_BVOL… x             NA           CYANOBA… Cyan… CHRO… Apha… Aphano… NA   
    ## 10 PEG_BVOL… x             NA           CYANOBA… Cyan… CHRO… Apha… Aphano… NA   
    ## # ℹ 3,836 more rows
    ## # ℹ 30 more variables: STAGE <chr>, Author <chr>, AphiaID <dbl>,
    ## #   AphiaID_link <chr>, Trophy <chr>, Geometric_shape <chr>, FORMULA <chr>,
    ## #   SizeClassNo <dbl>, Nonvalid_SIZCL <lgl>, Not_accepted_name <chr>,
    ## #   Unit <chr>, SizeRange <chr>, `Length(l1)µm` <dbl>, `Length(l2)µm` <dbl>,
    ## #   `Width(w)µm` <dbl>, `Height(h)µm` <dbl>, `Diameter(d1)µm` <dbl>,
    ## #   `Diameter(d2)µm` <dbl>, `No_of_cells/counting_unit` <dbl>, …

Please note that `SHARK4R` also includes a useful function for reading
exported [Plankton
Toolbox](https://nordicmicroalgae.org/plankton-toolbox/) data files:
[`read_ptbx()`](https://sharksmhi.github.io/SHARK4R/reference/read_ptbx.md).

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
