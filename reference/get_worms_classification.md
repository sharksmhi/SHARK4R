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

A `data.frame` where each row corresponds to an input AphiaID. Typical
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
if (FALSE) { # \dontrun{
# Single AphiaID
get_worms_classification(109604)

# Multiple AphiaIDs
get_worms_classification(c(109604, 376667))

# Hierarchy with ranks in the string
get_worms_classification(c(109604, 376667), add_rank_to_hierarchy = TRUE)
} # }
```
