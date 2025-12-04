# Retrieve WoRMS records by taxonomic names with retry logic

**\[deprecated\]**

This function has been deprecated. Users are encouraged to use
[`match_worms_taxa`](https://sharksmhi.github.io/SHARK4R/reference/match_worms_taxa.md)
instead.

This function retrieves records from the WoRMS database using the
`worrms` R package for a vector of taxonomic names. It includes retry
logic to handle temporary failures and ensures all names are processed.

## Usage

``` r
get_worms_records_name(
  taxa_names,
  fuzzy = TRUE,
  best_match_only = TRUE,
  max_retries = 3,
  sleep_time = 10,
  marine_only = TRUE,
  verbose = TRUE
)
```

## Arguments

- taxa_names:

  A vector of taxonomic names for which to retrieve records.

- fuzzy:

  A logical value indicating whether to search using a fuzzy search
  pattern. Default is TRUE.

- best_match_only:

  A logical value indicating whether to automatically select the first
  match and return a single match. Default is TRUE.

- max_retries:

  An integer specifying the maximum number of retries for the request in
  case of failure. Default is 3.

- sleep_time:

  A numeric value specifying the number of seconds to wait before
  retrying a failed request. Default is 10.

- marine_only:

  A logical value indicating whether to restrict the results to marine
  taxa only. Default is `FALSE`.

- verbose:

  A logical indicating whether to print progress messages. Default is
  TRUE.

## Value

A data frame containing the retrieved WoRMS records. Each row
corresponds to a record for a taxonomic name.

## Details

The function attempts to retrieve records for the input taxonomic names
using the `wm_records_names` function from the WoRMS API. If a request
fails, it retries up to `max_retries` times, pausing for `sleep_time`
seconds between attempts. If all attempts fail, the function stops and
throws an error.

## See also

<https://marinespecies.org/> for WoRMS website.

<https://CRAN.R-project.org/package=worrms>

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve WoRMS records for the taxonomic names "Amphidinium" and "Karenia"
records <- get_worms_records_name(c("Amphidinium", "Karenia"),
                                  max_retries = 3, sleep_time = 5, marine_only = TRUE)
} # }
```
