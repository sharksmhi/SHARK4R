# Download the IOC-UNESCO Taxonomic Reference List of Harmful Micro Algae

This function retrieves the IOC-UNESCO Taxonomic Reference List of
Harmful Micro Algae from the World Register of Marine Species (WoRMS).
The data is returned as a dataframe, with options to customize the
fields included in the download.

## Usage

``` r
get_hab_list(
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
  accepted_taxon = TRUE
)
```

## Arguments

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

## Value

A `tibble` containing the HABs taxonomic list, with columns based on the
selected parameters.

## Details

This function submits a POST request to the WoRMS database to retrieve
the IOC-UNESCO Taxonomic Reference List of Harmful Micro Algae. The
downloaded data can include various fields, which are controlled by the
input parameters. If a field is not required, set the corresponding
parameter to `FALSE` to exclude it from the output.

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

# Include only specific fields in the output
habs_taxlist_df <- get_hab_list(aphia_id = TRUE, scientific_name = TRUE, authority = FALSE)
head(habs_taxlist_df)
#> # A tibble: 6 × 28
#>   AphiaID ScientificName           AphiaID_accepted ScientificName_accepted 
#>     <dbl> <chr>                               <dbl> <chr>                   
#> 1 1653558 Aerosakkonemataceae               1653558 Aerosakkonemataceae     
#> 2  836651 Aetokthonos                        836651 Aetokthonos             
#> 3  841664 Aetokthonos hydrillicola           841664 Aetokthonos hydrillicola
#> 4  231787 Akashiwo                           231787 Akashiwo                
#> 5  232546 Akashiwo sanguinea                 232546 Akashiwo sanguinea      
#> 6  109470 Alexandrium                        109470 Alexandrium             
#> # ℹ 24 more variables: Authority_accepted <chr>, Fossil <dbl>, Kingdom <chr>,
#> #   Phylum <chr>, Class <chr>, Order <chr>, Family <chr>, taxonRank <chr>,
#> #   Genus <chr>, Subgenus <lgl>, Species <chr>, Subspecies <lgl>, Marine <dbl>,
#> #   Brackish <dbl>, Fresh <dbl>, Terrestrial <dbl>, taxonomicStatus <chr>,
#> #   Qualitystatus <chr>, Unacceptreason <chr>, DateLastModified <date>,
#> #   LSID <chr>, `Parent AphiaID` <dbl>, Storedpath <chr>, Citation <chr>
# }
```
