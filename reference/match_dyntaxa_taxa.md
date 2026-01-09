# Match Dyntaxa taxon names

This function matches a list of taxon names against the SLU
Artdatabanken API (Dyntaxa) and retrieves the best matches along with
their taxon IDs.

## Usage

``` r
match_dyntaxa_taxa(
  taxon_names,
  subscription_key = Sys.getenv("DYNTAXA_KEY"),
  multiple_options = FALSE,
  searchFields = "Both",
  isRecommended = "NotSet",
  isOkForObservationSystems = "NotSet",
  culture = "sv_SE",
  page = 1,
  pageSize = 100,
  verbose = TRUE
)
```

## Arguments

- taxon_names:

  A vector of taxon names to match.

- subscription_key:

  A Dyntaxa API subscription key. By default, the key is read from the
  environment variable `DYNTAXA_KEY`.

  You can provide the key in three ways:

  - **Directly as a parameter**:
    `match_dyntaxa_taxa("Skeletonema marinoi", subscription_key = "your_key_here")`.

  - **Temporarily for the session**:
    `Sys.setenv(DYNTAXA_KEY = "your_key_here")`. After this, you do not
    need to pass `subscription_key` to the function.

  - **Permanently across sessions** by adding it to your `~/.Renviron`
    file. Use `usethis::edit_r_environ()` to open the file, then add:
    `DYNTAXA_KEY=your_key_here`. After this, you do not need to pass
    `subscription_key` to the function.

- multiple_options:

  Logical. If TRUE, the function will return multiple matching names.
  Default is FALSE, selecting the first match.

- searchFields:

  A character string indicating the search fields. Defaults to 'Both'.

- isRecommended:

  A character string indicating whether the taxon is recommended.
  Defaults to 'NotSet'.

- isOkForObservationSystems:

  A character string indicating whether the taxon is suitable for
  observation systems. Defaults to 'NotSet'.

- culture:

  A character string indicating the culture. Defaults to 'sv_SE'.

- page:

  An integer specifying the page number for pagination. Defaults to 1.

- pageSize:

  An integer specifying the page size for pagination. Defaults to 100.

- verbose:

  Logical. Print progress bar. Default is TRUE.

## Value

A `tibble` containing the search pattern, taxon ID, and best match for
each taxon name.

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
# Match taxon names against SLU Artdatabanken API
matched_taxa <- match_dyntaxa_taxa(c("Homo sapiens", "Canis lupus"), "your_subscription_key")
print(matched_taxa)
} # }
```
