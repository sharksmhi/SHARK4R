# Download the IOC-UNESCO Taxonomic Reference List of Harmful Microalgae

This function retrieves the IOC-UNESCO Taxonomic Reference List of
Harmful Microalgae (Lundholm et al. 2009) from the World Register of
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

  Logical. Whether to display progress information. Default is `TRUE`.

## Value

A `tibble` containing the HABs taxonomic list, with columns based on the
selected parameters.

## Details

This function submits a POST request to the WoRMS database to retrieve
the IOC-UNESCO Taxonomic Reference List of Harmful Microalgae. The
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
List of Harmful Microalgae

## Examples

``` r
# \donttest{
# Download the default HABs taxonomic list
try(habs_taxlist_df <- get_hab_list())
#> Error in curl::curl_fetch_memory(url, handle = handle) : 
#>   Timeout was reached [www.marinespecies.org]:
#> Failed to connect to www.marinespecies.org port 443 after 10001 ms: Timeout was reached
if (exists("habs_taxlist_df")) head(habs_taxlist_df)

# Include higher taxa records
try(habs_taxlist_df <- get_hab_list(species_only = FALSE))
#> Error in curl::curl_fetch_memory(url, handle = handle) : 
#>   Timeout was reached [www.marinespecies.org]:
#> Failed to connect to www.marinespecies.org port 443 after 10002 ms: Timeout was reached
if (exists("habs_taxlist_df")) head(habs_taxlist_df)

# Retrieve only non-toxigenic harmful species (experimental stage)
try(habs_taxlist_df <- get_hab_list(harmful_non_toxic_only = TRUE, verbose = FALSE))
#> Error in curl::curl_fetch_memory(url, handle = handle) : 
#>   Timeout was reached [www.marinespecies.org]:
#> Failed to connect to www.marinespecies.org port 443 after 10002 ms: Timeout was reached
if (exists("habs_taxlist_df")) head(habs_taxlist_df)

# Include only specific fields in the output
try(habs_taxlist_df <- get_hab_list(aphia_id = TRUE, scientific_name = TRUE, authority = FALSE))
#> Error in curl::curl_fetch_memory(url, handle = handle) : 
#>   Timeout was reached [www.marinespecies.org]:
#> Failed to connect to www.marinespecies.org port 443 after 10002 ms: Timeout was reached
if (exists("habs_taxlist_df")) head(habs_taxlist_df)
# }
```
