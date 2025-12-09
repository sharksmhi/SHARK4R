# Retrieve WoRMS records

This function retrieves records from the WoRMS (World Register of Marine
Species) database using the `worrms` R package for a given list of Aphia
IDs. If the retrieval fails, it retries a specified number of times
before stopping.

## Usage

``` r
get_worms_records(
  aphia_ids,
  max_retries = 3,
  sleep_time = 10,
  verbose = TRUE,
  aphia_id = deprecated()
)
```

## Arguments

- aphia_ids:

  A vector of Aphia IDs for which records should be retrieved.

- max_retries:

  An integer specifying the maximum number of retry attempts for each
  Aphia ID in case of failure. Default is 3.

- sleep_time:

  A numeric value specifying the time (in seconds) to wait between retry
  attempts. Default is 10 seconds.

- verbose:

  A logical indicating whether to print progress messages. Default is
  TRUE.

- aphia_id:

  **\[deprecated\]** Use `aphia_ids` instead.

## Value

A `tibble` containing the retrieved WoRMS records for the provided Aphia
IDs. Each row corresponds to one Aphia ID.

## Details

The function attempts to fetch records for each Aphia ID in the provided
vector. If a retrieval fails, it retries up to the specified
`max_retries`, with a pause of `sleep_time` seconds between attempts. If
all retries fail for an Aphia ID, the function stops with an error
message.

## See also

<https://marinespecies.org/> for WoRMS website.

<https://CRAN.R-project.org/package=worrms>

## Examples

``` r
# \donttest{
# Example usage with a vector of Aphia IDs
aphia_ids <- c(12345, 67890, 112233)
worms_records <- get_worms_records(aphia_ids, verbose = FALSE)

print(worms_records)
#> # A tibble: 3 × 28
#>   AphiaID status url   scientificname authority unacceptreason taxonRankID rank 
#>     <dbl> <chr>  <chr> <chr>          <chr>     <lgl>                <int> <chr>
#> 1   12345 no co… NA    NA             NA        NA                      NA NA   
#> 2   67890 no co… NA    NA             NA        NA                      NA NA   
#> 3  112233 accep… http… Nonionella     Cushman,… NA                     180 Genus
#> # ℹ 20 more variables: valid_AphiaID <int>, valid_name <chr>,
#> #   valid_authority <chr>, parentNameUsageID <int>, originalNameUsageID <int>,
#> #   kingdom <chr>, phylum <chr>, class <chr>, order <chr>, family <chr>,
#> #   genus <chr>, citation <chr>, lsid <chr>, isMarine <int>, isBrackish <lgl>,
#> #   isFreshwater <int>, isTerrestrial <int>, isExtinct <int>, match_type <chr>,
#> #   modified <chr>
# }
```
