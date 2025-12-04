# Retrieve and organize WoRMS taxonomy for SHARK Aphia IDs

**\[deprecated\]**

This function was deprecated and replaced by a function with more
accurate name. Use
[`add_worms_taxonomy()`](https://sharksmhi.github.io/SHARK4R/reference/add_worms_taxonomy.md)
instead.

This function collects WoRMS (World Register of Marine Species) taxonomy
information for a given set of Aphia IDs. The data is organized into a
full taxonomic table that can be joined with data downloaded from
[SHARK](https://shark.smhi.se/).

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

A data frame containing updated WoRMS taxonomy information.

## See also

<https://marinespecies.org/> for WoRMS website.

[`get_shark_data`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_data.md),
[`update_dyntaxa_taxonomy`](https://sharksmhi.github.io/SHARK4R/reference/update_dyntaxa_taxonomy.md),
[WoRMS API Documentation](https://www.marinespecies.org/rest/),
<https://CRAN.R-project.org/package=worrms>

## Examples

``` r
if (FALSE) { # \dontrun{
# Update WoRMS taxonomy for a set of Aphia IDs
updated_taxonomy <- update_worms_taxonomy(c(149619, 149122, 11))
print(updated_taxonomy)
} # }
```
