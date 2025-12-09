# Retrieve hierarchical classification from WoRMS

Retrieves the hierarchical taxonomy for one or more AphiaIDs from the
World Register of Marine Species (WoRMS) and returns it in a wide
format. Optionally, a hierarchy string column can be added that
concatenates ranks.

## Usage

``` r
get_worms_classification(
  aphia_ids,
  add_rank_to_hierarchy = FALSE,
  verbose = TRUE
)
```

## Arguments

- aphia_ids:

  Numeric vector of AphiaIDs to retrieve classification for. Must not be
  NULL or empty. Duplicates are allowed and will be preserved in the
  output.

- add_rank_to_hierarchy:

  Logical (default FALSE). If TRUE, the hierarchy string prepends rank
  names (e.g., `[Kingdom] Animalia - [Phylum] Chordata`) to each taxon
  name in the `worms_hierarchy` column. Only applies if
  `worms_hierarchy` is present.

- verbose:

  Logical (default TRUE). If TRUE, prints progress messages and a
  progress bar during data retrieval.

## Value

A `tibble` where each row corresponds to an input AphiaID. Typical
columns include:

- aphia_id:

  The AphiaID of the taxon (matches input).

- scientific_name:

  The last scientific name in the hierarchy for this AphiaID.

- taxonomic ranks:

  Columns for each rank present in the WoRMS hierarchy (e.g., Kingdom,
  Phylum, Class, Order, Family, Genus, Species). Missing ranks are NA.

- worms_hierarchy:

  A concatenated string of all ranks for this AphiaID. Added for every
  row if `wm_classification()` returned hierarchy data. Format depends
  on `add_rank_to_hierarchy`.

## Details

The function performs the following steps:

1.  Validates input AphiaIDs and removes NA values.

2.  Retrieves the hierarchical classification for each AphiaID using
    [`worrms::wm_classification()`](https://docs.ropensci.org/worrms/reference/wm_classification.html).

3.  Converts the hierarchy to a wide format with one column per rank.

4.  Adds a `worms_hierarchy` string concatenating all ranks.

5.  Preserves input order and duplicates.

## See also

[`wm_classification`](https://docs.ropensci.org/worrms/reference/wm_classification.html),
<https://marinespecies.org/>

## Examples

``` r
# \donttest{
# Single AphiaID
single_taxa <- get_worms_classification(109604, verbose = FALSE)
print(single_taxa)
#> # A tibble: 1 × 15
#>   aphia_id scientific_name  parent_name Kingdom   Subkingdom Infrakingdom Phylum
#>      <dbl> <chr>            <chr>       <chr>     <chr>      <chr>        <chr> 
#> 1   109604 Dinophysis acuta Dinophysis  Chromista Harosa     Alveolata    Myzoz…
#> # ℹ 8 more variables: Subphylum <chr>, Infraphylum <chr>, Class <chr>,
#> #   Order <chr>, Family <chr>, Genus <chr>, Species <chr>,
#> #   worms_hierarchy <chr>

# Multiple AphiaIDs
multiple_taxa <- get_worms_classification(c(109604, 376667), verbose = FALSE)
print(multiple_taxa)
#> # A tibble: 2 × 17
#>   aphia_id scientific_name    parent_name Kingdom Subkingdom Infrakingdom Phylum
#>      <dbl> <chr>              <chr>       <chr>   <chr>      <chr>        <chr> 
#> 1   109604 Dinophysis acuta   Dinophysis  Chromi… Harosa     Alveolata    Myzoz…
#> 2   376667 Skeletonema marin… Skeletonema Chromi… NA         NA           Heter…
#> # ℹ 10 more variables: Subphylum <chr>, Infraphylum <chr>, Class <chr>,
#> #   Order <chr>, Family <chr>, Genus <chr>, Species <chr>, Subclass <chr>,
#> #   Superorder <chr>, worms_hierarchy <chr>

# Hierarchy with ranks in the string
with_rank <- get_worms_classification(c(109604, 376667),
                                      add_rank_to_hierarchy = TRUE,
                                      verbose = FALSE)

# Print hierarchy columns with ranks
print(with_rank$worms_hierarchy[1])
#> [1] "[Kingdom] Chromista - [Subkingdom] Harosa - [Infrakingdom] Alveolata - [Phylum] Myzozoa - [Subphylum] Dinozoa - [Infraphylum] Dinoflagellata - [Class] Dinophyceae - [Order] Dinophysales - [Family] Dinophysaceae - [Genus] Dinophysis - [Species] Dinophysis acuta"

# Compare with result when add_rank_to_hierarchy = FALSE
print(multiple_taxa$worms_hierarchy[1])
#> [1] "Chromista - Harosa - Alveolata - Myzozoa - Dinozoa - Dinoflagellata - Dinophyceae - Dinophysales - Dinophysaceae - Dinophysis - Dinophysis acuta"
# }
```
