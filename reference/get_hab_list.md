# Download the IOC-UNESCO Taxonomic Reference List of Harmful Micro Algae

This function retrieves the IOC-UNESCO Taxonomic Reference List of
Harmful Micro Algae (Lundholm et al. 2009) from the World Register of
Marine Species (WoRMS). The data is returned as a dataframe, with
options to customize the fields included in the download.

## Usage

``` r
get_hab_list(
  species_only = TRUE,
  harmful_non_toxic_only = FALSE,
  aphia_id = TRUE,
  scientific_name = TRUE,
  authority = TRUE,
  fossil = TRUE,
  rank_name = TRUE,
  status_name = TRUE,
  qualitystatus_name = TRUE,
  modified = TRUE,
  lsid = TRUE,
  parent_id = TRUE,
  stored_path = TRUE,
  citation = TRUE,
  classification = TRUE,
  environment = TRUE,
  accepted_taxon = TRUE,
  verbose = TRUE
)
```

## Arguments

- species_only:

  Logical. If `TRUE`, only species-level records are returned (i.e.,
  rows where the `Species` column is not `NA`). Note that this filter is
  only applied when `harmful_non_toxic_only = FALSE`; it is ignored when
  `harmful_non_toxic_only = TRUE`.

- harmful_non_toxic_only:

  Logical. If `TRUE`, retrieves only non-toxigenic marine microalgal
  species flagged with harmful effects. Defaults to `FALSE`.
  **\[experimental\]**

- aphia_id:

  Logical. Include the AphiaID field. Defaults to `TRUE`.

- scientific_name:

  Logical. Include the scientific name field. Defaults to `TRUE`.

- authority:

  Logical. Include the authority field. Defaults to `TRUE`.

- fossil:

  Logical. Include information about fossil status. Defaults to `TRUE`.

- rank_name:

  Logical. Include the taxonomic rank (e.g., species, variety, forma).
  Defaults to `TRUE`.

- status_name:

  Logical. Include the taxonomic status field. Defaults to `TRUE`.

- qualitystatus_name:

  Logical. Include the quality status field. Defaults to `TRUE`.

- modified:

  Logical. Include the date of last modification field. Defaults to
  `TRUE`.

- lsid:

  Logical. Include the Life Science Identifier (LSID) field. Defaults to
  `TRUE`.

- parent_id:

  Logical. Include the parent AphiaID field. Defaults to `TRUE`.

- stored_path:

  Logical. Include the stored path field. Defaults to `TRUE`.

- citation:

  Logical. Include citation information. Defaults to `TRUE`.

- classification:

  Logical. Include the full taxonomic classification (e.g., kingdom,
  phylum, class). Defaults to `TRUE`.

- environment:

  Logical. Include environmental data (e.g., marine, brackish,
  freshwater, terrestrial). Defaults to `TRUE`.

- accepted_taxon:

  Logical. Include information about the accepted taxon (e.g.,
  scientific name and authority). Defaults to `TRUE`.

- verbose:

  Logical. Whether to display progress information. Default is
  \`TRUE“\`.

## Value

A `tibble` containing the HABs taxonomic list, with columns based on the
selected parameters.

## Details

This function submits a POST request to the WoRMS database to retrieve
the IOC-UNESCO Taxonomic Reference List of Harmful Micro Algae. The
downloaded data can include various fields, which are controlled by the
input parameters. If a field is not required, set the corresponding
parameter to `FALSE` to exclude it from the output.

## References

Lundholm, N.; Bernard, C.; Churro, C.; Escalera, L.; Hoppenrath, M.;
Iwataki, M.; Larsen, J.; Mertens, K.; Murray, S.; Probert, I.; Salas,
R.; Tillmann, U.; Zingone, A. (Eds) (2009 onwards). IOC-UNESCO Taxonomic
Reference List of Harmful Microalgae.
https://www.marinespecies.org/hab/. doi:10.14284/362

## See also

<https://www.marinespecies.org/hab/> for IOC-UNESCO Taxonomic Reference
List of Harmful Micro Algae

## Examples

``` r
# \donttest{
# Download the default HABs taxonomic list
habs_taxlist_df <- get_hab_list()
head(habs_taxlist_df)
#> # A tibble: 6 × 29
#>   AphiaID ScientificName       Authority AphiaID_accepted ScientificName_accep…¹
#>     <dbl> <chr>                <chr>                <dbl> <chr>                 
#> 1  841664 Aetokthonos hydrill… S.B.Wild…           841664 Aetokthonos hydrillic…
#> 2  232546 Akashiwo sanguinea   (K.Hiras…           232546 Akashiwo sanguinea    
#> 3  109707 Alexandrium affine   (H.Inoue…           109707 Alexandrium affine    
#> 4  231872 Alexandrium anderso… Balech, …           246835 Alexandrium andersonii
#> 5  246835 Alexandrium anderso… Balech, …           246835 Alexandrium andersonii
#> 6  233452 Alexandrium angusti… F.J.R.Ta…           109711 Alexandrium minutum   
#> # ℹ abbreviated name: ¹​ScientificName_accepted
#> # ℹ 24 more variables: Authority_accepted <chr>, Fossil <dbl>, Kingdom <chr>,
#> #   Phylum <chr>, Class <chr>, Order <chr>, Family <chr>, taxonRank <chr>,
#> #   Genus <chr>, Subgenus <lgl>, Species <chr>, Subspecies <lgl>, Marine <dbl>,
#> #   Brackish <dbl>, Fresh <dbl>, Terrestrial <dbl>, taxonomicStatus <chr>,
#> #   Qualitystatus <chr>, Unacceptreason <chr>, DateLastModified <date>,
#> #   LSID <chr>, `Parent AphiaID` <dbl>, Storedpath <chr>, Citation <chr>

# Include higer taxa records
habs_taxlist_df <- get_hab_list(species_only = FALSE)
head(habs_taxlist_df)
#> # A tibble: 6 × 29
#>   AphiaID ScientificName       Authority AphiaID_accepted ScientificName_accep…¹
#>     <dbl> <chr>                <chr>                <dbl> <chr>                 
#> 1 1653558 Aerosakkonemataceae  Struneck…          1653558 Aerosakkonemataceae   
#> 2  836651 Aetokthonos          S.B.Wild…           836651 Aetokthonos           
#> 3  841664 Aetokthonos hydrill… S.B.Wild…           841664 Aetokthonos hydrillic…
#> 4  231787 Akashiwo             G.Hansen…           231787 Akashiwo              
#> 5  232546 Akashiwo sanguinea   (K.Hiras…           232546 Akashiwo sanguinea    
#> 6  109470 Alexandrium          Halim, 1…           109470 Alexandrium           
#> # ℹ abbreviated name: ¹​ScientificName_accepted
#> # ℹ 24 more variables: Authority_accepted <chr>, Fossil <dbl>, Kingdom <chr>,
#> #   Phylum <chr>, Class <chr>, Order <chr>, Family <chr>, taxonRank <chr>,
#> #   Genus <chr>, Subgenus <lgl>, Species <chr>, Subspecies <lgl>, Marine <dbl>,
#> #   Brackish <dbl>, Fresh <dbl>, Terrestrial <dbl>, taxonomicStatus <chr>,
#> #   Qualitystatus <chr>, Unacceptreason <chr>, DateLastModified <date>,
#> #   LSID <chr>, `Parent AphiaID` <dbl>, Storedpath <chr>, Citation <chr>

# Retrieve only non-toxigenic harmful species (experimental stage)
habs_taxlist_df <- get_hab_list(harmful_non_toxic_only = TRUE, verbose = FALSE)
head(habs_taxlist_df)
#> # A tibble: 6 × 24
#>   AphiaID ScientificName       Authority AphiaID_accepted ScientificName_accep…¹
#>     <int> <chr>                <chr>                <int> <chr>                 
#> 1  109708 Alexandrium balechii (Steidin…           109708 Alexandrium balechii  
#> 2  375699 Aureococcus anophag… Hargrave…           375699 Aureococcus anophagef…
#> 3  375701 Aureoumbra lagunens… D.A.Stoc…           375701 Aureoumbra lagunensis 
#> 4 1360848 Blixaea quinquecorn… (T.H.Abé…          1360848 Blixaea quinquecornis 
#> 5  149619 Cerataulina pelagica (Cleve) …           149619 Cerataulina pelagica  
#> 6  163013 Chaetoceros calcitr… (Paulsen…           163015 Chaetoceros simplex v…
#> # ℹ abbreviated name: ¹​ScientificName_accepted
#> # ℹ 19 more variables: Authority_accepted <chr>, Fossil <int>, Kingdom <chr>,
#> #   Phylum <chr>, Class <chr>, Order <chr>, Family <chr>, taxonRank <chr>,
#> #   Genus <chr>, Marine <int>, Brackish <int>, Fresh <int>, Terrestrial <int>,
#> #   taxonomicStatus <chr>, Unacceptreason <chr>, DateLastModified <dttm>,
#> #   LSID <chr>, `Parent AphiaID` <int>, Citation <chr>

# Include only specific fields in the output
habs_taxlist_df <- get_hab_list(aphia_id = TRUE, scientific_name = TRUE, authority = FALSE)
head(habs_taxlist_df)
#> # A tibble: 6 × 28
#>   AphiaID ScientificName               AphiaID_accepted ScientificName_accepted 
#>     <dbl> <chr>                                   <dbl> <chr>                   
#> 1  841664 Aetokthonos hydrillicola               841664 Aetokthonos hydrillicola
#> 2  232546 Akashiwo sanguinea                     232546 Akashiwo sanguinea      
#> 3  109707 Alexandrium affine                     109707 Alexandrium affine      
#> 4  231872 Alexandrium andersoni                  246835 Alexandrium andersonii  
#> 5  246835 Alexandrium andersonii                 246835 Alexandrium andersonii  
#> 6  233452 Alexandrium angustitabulatum           109711 Alexandrium minutum     
#> # ℹ 24 more variables: Authority_accepted <chr>, Fossil <dbl>, Kingdom <chr>,
#> #   Phylum <chr>, Class <chr>, Order <chr>, Family <chr>, taxonRank <chr>,
#> #   Genus <chr>, Subgenus <lgl>, Species <chr>, Subspecies <lgl>, Marine <dbl>,
#> #   Brackish <dbl>, Fresh <dbl>, Terrestrial <dbl>, taxonomicStatus <chr>,
#> #   Qualitystatus <chr>, Unacceptreason <chr>, DateLastModified <date>,
#> #   LSID <chr>, `Parent AphiaID` <dbl>, Storedpath <chr>, Citation <chr>
# }
```
