# Retrieve and organize WoRMS taxonomy for SHARK Aphia IDs

**\[deprecated\]**

This function was deprecated and replaced by a function with more
accurate name. Use
[`add_worms_taxonomy()`](https://sharksmhi.github.io/SHARK4R/reference/add_worms_taxonomy.md)
instead.

This function collects WoRMS (World Register of Marine Species) taxonomy
information for a given set of Aphia IDs. The data is organized into a
full taxonomic table that can be joined with data downloaded from
[SHARK](https://shark.smhi.se/en/).

## Usage

``` r
update_worms_taxonomy(aphia_id, aphiaid = deprecated())
```

## Arguments

- aphia_id:

  A numeric vector containing Aphia IDs for which WoRMS taxonomy needs
  to be updated.

- aphiaid:

  **\[deprecated\]** Use `aphia_id` instead.

## Value

A `tibble` containing updated WoRMS taxonomy information.

## See also

<https://marinespecies.org/> for WoRMS website.

[`get_shark_data`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_data.md),
[`update_dyntaxa_taxonomy`](https://sharksmhi.github.io/SHARK4R/reference/update_dyntaxa_taxonomy.md),
[WoRMS API Documentation](https://www.marinespecies.org/rest/),
<https://CRAN.R-project.org/package=worrms>

## Examples

``` r
# \donttest{
# Update WoRMS taxonomy for a set of Aphia IDs
try(updated_taxonomy <- update_worms_taxonomy(c(149619, 149122, 11)))
#> Warning: `update_worms_taxonomy()` was deprecated in SHARK4R 0.1.3.
#> ℹ Please use `add_worms_taxonomy()` instead.
#> Retrieving WoRMS classification for 3 AphiaIDs.
#> Warning: Failed to retrieve WoRMS classification for AphiaID 149619: Timeout was reached
#> [www.marinespecies.org]: SSL connection timeout
#> Retrieving WoRMS classification ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  5s
#> Warning: Failed to retrieve WoRMS classification for AphiaID 149122: Timeout was reached
#> [www.marinespecies.org]: Failed to connect to www.marinespecies.org port 443
#> after 10002 ms: Timeout was reached
#> Retrieving WoRMS classification ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> Warning: Failed to retrieve WoRMS classification for AphiaID 11: Timeout was reached
#> [www.marinespecies.org]: Failed to connect to www.marinespecies.org port 443
#> after 10002 ms: Timeout was reached
#> Error in dplyr::relocate(df_all, worms_hierarchy, .after = dplyr::last_col()) : 
#>   Can't select columns that don't exist.
#> ✖ Column `worms_hierarchy` doesn't exist.
if (exists("updated_taxonomy")) print(updated_taxonomy)
# }
```
