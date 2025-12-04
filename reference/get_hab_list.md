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

A dataframe containing the HABs taxonomic list, with columns based on
the selected parameters.

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
if (FALSE) { # \dontrun{
# Download the default HABs taxonomic list
habs_taxlist_df <- get_hab_list()
head(habs_taxlist_df)

# Include only specific fields in the output
habs_taxlist_df <- get_hab_list(aphia_id = TRUE, scientific_name = TRUE, authority = FALSE)
} # }
```
