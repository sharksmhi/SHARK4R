# Retrieve WoRMS records by taxonomic names with retry logic

This function retrieves records from the WoRMS database using the
`worrms` R package for a vector of taxonomic names. It includes retry
logic to handle temporary failures and ensures all names are processed.
The function can query all names at once using a bulk API call or
iterate over names individually.

## Usage

``` r
match_worms_taxa(
  taxa_names,
  fuzzy = TRUE,
  best_match_only = TRUE,
  max_retries = 3,
  sleep_time = 10,
  marine_only = TRUE,
  bulk = FALSE,
  chunk_size = 500,
  verbose = TRUE
)
```

## Arguments

- taxa_names:

  A character vector of taxonomic names for which to retrieve records.

- fuzzy:

  A logical value indicating whether to perform a fuzzy search. Default
  is TRUE. **Note:** Fuzzy search is only applied in iterative mode
  (`bulk = FALSE`) and is ignored in bulk mode.

- best_match_only:

  A logical value indicating whether to automatically select the first
  match and return a single match. Default is TRUE.

- max_retries:

  Integer specifying the maximum number of retries for the request in
  case of failure. Default is 3.

- sleep_time:

  Numeric specifying the number of seconds to wait before retrying a
  failed request. Default is 10.

- marine_only:

  Logical indicating whether to restrict results to marine taxa only.
  Default is TRUE.

- bulk:

  Logical indicating whether to perform a bulk API call for all unique
  names at once. Default is FALSE.

- chunk_size:

  Integer specifying the maximum number of taxa per bulk API request.
  Default is 500. Only used when `bulk = TRUE`. WoRMS API may reject
  very large requests, so chunking prevents overload.

- verbose:

  Logical indicating whether to print progress messages. Default is
  TRUE.

## Value

A data frame containing the retrieved WoRMS records. Each row
corresponds to a record for a taxonomic name. Repeated taxa in the input
are preserved in the output.

## Details

- If `bulk = TRUE`, all unique names are sent to the API in a single
  request. Fuzzy matching is ignored.

- If `bulk = FALSE`, the function iterates over names individually,
  optionally using fuzzy matching.

- The function retries failed requests up to `max_retries` times,
  pausing for `sleep_time` seconds between attempts.

- Names for which no records are found will have `status = "no content"`
  and `AphiaID = NA`.

- Names are cleaned before being passed to the API call by converting
  them to UTF-8, replacing problematic symbols with spaces, removing
  trailing periods, collapsing extra spaces and by trimming whitespace.

## See also

`match_worms_taxa()` to match taxa names interactively.

<https://marinespecies.org/> for WoRMS website.

<https://CRAN.R-project.org/package=worrms>

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve WoRMS records iteratively for two taxonomic names
records <- match_worms_taxa(c("Amphidinium", "Karenia"),
                            max_retries = 3, sleep_time = 5, marine_only = TRUE)

# Retrieve WoRMS records in bulk mode (faster for many names)
records_bulk <- match_worms_taxa(c("Amphidinium", "Karenia", "Navicula"),
                                 bulk = TRUE, marine_only = TRUE)
} # }
```
