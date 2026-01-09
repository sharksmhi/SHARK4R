# Get taxonomic information from Dyntaxa for specified taxon IDs

This function queries the SLU Artdatabanken API (Dyntaxa) to retrieve
taxonomic information for the specified taxon IDs. It constructs a
request with the provided taxon IDs, sends the request to the SLU
Artdatabanken API, and processes the response to return taxonomic
information in a data frame.

## Usage

``` r
get_dyntaxa_records(taxon_ids, subscription_key = Sys.getenv("DYNTAXA_KEY"))
```

## Arguments

- taxon_ids:

  A vector of numeric taxon IDs (Dyntaxa ID) for which taxonomic
  information is requested.

- subscription_key:

  A Dyntaxa API subscription key. By default, the key is read from the
  environment variable `DYNTAXA_KEY`.

  You can provide the key in three ways:

  - **Directly as a parameter**:
    `get_dyntaxa_records(238366, subscription_key = "your_key_here")`.

  - **Temporarily for the session**:
    `Sys.setenv(DYNTAXA_KEY = "your_key_here")`. After this, you do not
    need to pass `subscription_key` to the function.

  - **Permanently across sessions** by adding it to your `~/.Renviron`
    file. Use `usethis::edit_r_environ()` to open the file, then add:
    `DYNTAXA_KEY=your_key_here`. After this, you do not need to pass
    `subscription_key` to the function.

## Value

A `tibble` containing taxonomic information for the specified taxon IDs.
Columns include `taxonId`, `names`, `category`, `rank`, `isRecommended`,
and `parentTaxonId.`

## Details

A valid Dyntaxa API subscription key is required. You can request a free
key for the "Taxonomy" service from the ArtDatabanken API portal:
<https://api-portal.artdatabanken.se/>

**Note**: Please review the [API
conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
and [register for access](https://api-portal.artdatabanken.se/) before
using the API. Data collected through the API is stored at SLU
Artdatabanken. Please also note that the authors of `SHARK4R` are not
affiliated with SLU Artdatabanken.

## See also

[SLU Artdatabanken API
Documentation](https://api-portal.artdatabanken.se/)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get taxonomic information for taxon IDs 238366 and 1010380
taxon_info <- get_dyntaxa_records(c(238366, 1010380), "your_subscription_key")
print(taxon_info)
} # }

```
