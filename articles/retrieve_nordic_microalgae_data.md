# Retrieve Data From Nordic Microalgae

## Nordic Microalgae

[Nordic Microalgae](https://nordicmicroalgae.org/) is an online platform
providing high-quality images, illustrations, and detailed information
about aquatic microalgae and related organisms in the Nordic region,
including phytoplankton, microzooplankton, and benthic microalgae
(Torstensson et al. 2024). It features a comprehensive species
checklist, up-to-date taxonomic data linked to WoRMS and AlgaeBase, and
supplementary resources such as biovolume lists and occurrence maps.
Established in 1996 and supported by long-term funding from the Swedish
Research Council through the Swedish Biodiversity Data Infrastructure
(SBDI).

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
```

## Retrieve Complete Nordic Microalgae Taxon Table

A complete Nordic Microalgae taxa list can be retrieved through the API.

``` r
# Get taxa information
taxa <- get_nua_taxa(unparsed = FALSE)

# Print data
print(taxa)
```

    ## # A tibble: 6,867 × 5
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
    ## # ℹ 6,857 more rows

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
print(external_links)
```

    ## # A tibble: 59 × 6
    ##    slug                  provider  label     external_id external_url collection
    ##    <chr>                 <chr>     <chr>     <chr>       <chr>        <chr>     
    ##  1 pterosperma-polygonum AlgaeBase AlgaeBase 56414       https://www… External …
    ##  2 pterosperma-polygonum Dyntaxa   Dyntaxa   238955      https://art… External …
    ##  3 pterosperma-polygonum GBIF      GBIF      2688134     https://www… External …
    ##  4 pterosperma-polygonum ITIS      ITIS      9590        http://www.… External …
    ##  5 pterosperma-polygonum WoRMS     WoRMS     341610      https://www… External …
    ##  6 romeria-gracilis      AlgaeBase AlgaeBase 65685       https://www… External …
    ##  7 romeria-gracilis      Dyntaxa   Dyntaxa   263585      https://art… External …
    ##  8 romeria-gracilis      ENA       ENA       1744976     https://www… External …
    ##  9 romeria-gracilis      GBIF      GBIF      7644679     https://www… External …
    ## 10 romeria-gracilis      NCBI      NCBI      1744976     https://www… External …
    ## # ℹ 49 more rows

The full list of facts can be accessed as an unparsed list by setting
the `unparsed` parameter to `TRUE`.

## Get Nordic Microalgae Harmfulness Information

Taxa listed in the [IOC-UNESCO Taxonomic Reference List of Harmful
Microalgae](https://www.marinespecies.org/hab/) contain information
about harmfulness. This information can be retrieved through the API.

``` r
# Get external links
harmfulness <- get_nua_harmfulness(c("dinophysis-acuta", 
                                     "alexandrium-ostenfeldii"), 
                                   verbose = FALSE)

# Print list
print(harmfulness)
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
print(media)
```

    ## # A tibble: 2,421 × 10
    ##    slug              image_l_url image_o_url image_s_url image_m_url contributor
    ##    <chr>             <chr>       <chr>       <chr>       <chr>       <chr>      
    ##  1 octactis-speculum https://no… https://no… https://no… https://no… SMHI       
    ##  2 lennoxia-faveola… https://no… https://no… https://no… https://no… Maria Karl…
    ##  3 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ##  4 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ##  5 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ##  6 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ##  7 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ##  8 pleurosigma       https://no… https://no… https://no… https://no… Maria Karl…
    ##  9 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ## 10 NA                https://no… https://no… https://no… https://no… Per Wilhel…
    ## # ℹ 2,411 more rows
    ## # ℹ 4 more variables: photographer_artist <chr>, copyright_holder <chr>,
    ## #   license <chr>, galleries <chr>

Complete media information can be retrieved as an unparsed list by
setting the `unparsed` parameter to `TRUE`.

## Get Nordic Microalgae Media Metadata

Detailed metadata for all media items on Nordic Microalgae can be
retrieved through the API. This includes information such as location,
sampling date, geographic coordinates, imaging technique, and
contributor details.

``` r
# Get all media metadata
media_metadata <- get_nua_media_metadata(unparsed = FALSE)

# Print list
print(media_metadata)
```

    ## # A tibble: 2,421 × 25
    ##    slug    taxon_slug scientific_name file  type  title caption license location
    ##    <chr>   <chr>      <chr>           <chr> <chr> <chr> <chr>   <chr>   <chr>   
    ##  1 octact… octactis-… Octactis specu… octa… imag… Octa… "Two c… Creati… Danafjo…
    ##  2 lennox… lennoxia-… Lennoxia faveo… lenn… imag… Lenn… ""      Creati… Danafjo…
    ##  3 bolmen… NA         NA              bolm… imag… Bolm… "Taken… Creati… Bolmen,…
    ##  4 bolmen… NA         NA              bolm… imag… Bolm… "Taken… Creati… Bolmen,…
    ##  5 bolmen… NA         NA              bolm… imag… Bolm… "Taken… Creati… Bolmen,…
    ##  6 hassle… NA         NA              hass… imag… Hass… "Taken… Creati… Hasslem…
    ##  7 hassle… NA         NA              hass… imag… Hass… "Taken… Creati… Hasslem…
    ##  8 pleuro… pleurosig… Pleurosigma     pleu… imag… Pleu… ""      Creati… Byttelo…
    ##  9 vombsj… NA         NA              vomb… imag… Vomb… "Taken… Creati… Vombsjö…
    ## 10 vombsj… NA         NA              vomb… imag… Vomb… "Taken… Creati… Vombsjö…
    ## # ℹ 2,411 more rows
    ## # ℹ 16 more variables: contributor <chr>, photographer_artist <chr>,
    ## #   copyright_holder <chr>, galleries <chr>, technique <list>,
    ## #   contrast_enhancement <list>, preservation <list>, stain <list>,
    ## #   sampling_date <chr>, geographic_area <chr>, latitude_degree <chr>,
    ## #   longitude_degree <chr>, institute <chr>, contributing_organisation <chr>,
    ## #   created_at <chr>, updated_at <chr>

## Get Nordic Microalgae Image Labeling Links

Nordic Microalgae hosts images from automated imaging instruments (e.g.,
IFCB) used for image labeling. Links to these images can be retrieved
through the API, similar to
[`get_nua_media_links()`](https://sharksmhi.github.io/SHARK4R/reference/get_nua_media_links.md).

``` r
# Get all image labeling media links
il_links <- get_nua_image_labeling_links(unparsed = FALSE)

# Print list
print(il_links)
```

    ## # A tibble: 796 × 10
    ##    slug            image_l_url   image_o_url image_s_url image_m_url contributor
    ##    <chr>           <chr>         <chr>       <chr>       <chr>       <chr>      
    ##  1 tripos-muelleri https://nord… https://no… https://no… https://no… Anders Tor…
    ##  2 tripos-muelleri https://nord… https://no… https://no… https://no… Anders Tor…
    ##  3 tripos-muelleri https://nord… https://no… https://no… https://no… Anders Tor…
    ##  4 tripos-muelleri https://nord… https://no… https://no… https://no… Anders Tor…
    ##  5 tripos-muelleri https://nord… https://no… https://no… https://no… Anders Tor…
    ##  6 tripos-lineatus https://nord… https://no… https://no… https://no… Anders Tor…
    ##  7 tripos-lineatus https://nord… https://no… https://no… https://no… Anders Tor…
    ##  8 tripos-lineatus https://nord… https://no… https://no… https://no… Anders Tor…
    ##  9 tripos-lineatus https://nord… https://no… https://no… https://no… Anders Tor…
    ## 10 tripos-lineatus https://nord… https://no… https://no… https://no… Anders Tor…
    ## # ℹ 786 more rows
    ## # ℹ 4 more variables: copyright_holder <chr>, license <chr>,
    ## #   imaging_instrument <chr>, priority <int>

## Get Nordic Microalgae Image Labeling Metadata

Detailed metadata for automated imaging images can be retrieved through
the API. This includes information about the imaging instrument,
training dataset, location, and taxonomic details.

``` r
# Get all image labeling metadata
il_metadata <- get_nua_image_labeling_metadata(unparsed = FALSE)

# Print list
print(il_metadata)
```

    ## # A tibble: 796 × 22
    ##    slug    taxon_slug scientific_name file  type  title caption license location
    ##    <chr>   <chr>      <chr>           <chr> <chr> <chr> <chr>   <chr>   <chr>   
    ##  1 tripos… tripos-mu… Tripos muelleri trip… imag… Trip… ""      Creati… Tångesu…
    ##  2 tripos… tripos-mu… Tripos muelleri trip… imag… Trip… ""      Creati… Tångesu…
    ##  3 tripos… tripos-mu… Tripos muelleri trip… imag… Trip… ""      Creati… Tångesu…
    ##  4 tripos… tripos-mu… Tripos muelleri trip… imag… Trip… ""      Creati… Tångesu…
    ##  5 tripos… tripos-mu… Tripos muelleri trip… imag… Trip… ""      Creati… Tångesu…
    ##  6 tripos… tripos-li… Tripos lineatus trip… imag… Trip… ""      Creati… Tångesu…
    ##  7 tripos… tripos-li… Tripos lineatus trip… imag… Trip… ""      Creati… Tångesu…
    ##  8 tripos… tripos-li… Tripos lineatus trip… imag… Trip… ""      Creati… Tångesu…
    ##  9 tripos… tripos-li… Tripos lineatus trip… imag… Trip… ""      Creati… Tångesu…
    ## 10 tripos… tripos-li… Tripos lineatus trip… imag… Trip… ""      Creati… Tångesu…
    ## # ℹ 786 more rows
    ## # ℹ 13 more variables: contributor <chr>, copyright_holder <chr>,
    ## #   imaging_instrument <chr>, training_dataset <chr>, sampling_date <lgl>,
    ## #   geographic_area <chr>, latitude_degree <chr>, longitude_degree <chr>,
    ## #   institute <chr>, contributing_organisation <chr>, priority <int>,
    ## #   created_at <chr>, updated_at <chr>

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
print(peg_list)
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

    ## File for year 2026 not available. Trying previous year...

    ## File for year 2025 downloaded and cached.

``` r
# Print list
print(nomp_list)
```

    ## # A tibble: 3,852 × 39
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
    ## # ℹ 3,842 more rows
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

- Torstensson, A., Loo, A., Sundqvist, L., Skjevik, A.-T., Karlberg, M.,
  Johansen, M., Andreasson, A., and Karlson, B. (2024). Nordic
  Microalgae 2.0, Accessed at www.nordicmicroalgae.org on 2026-02-23.
