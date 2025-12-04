# Retrieve hierarchical taxonomy data from WoRMS

Retrieves the hierarchical taxonomy for one or more AphiaIDs from the
World Register of Marine Species (WoRMS). Optionally, the function can
include all descendants of taxa at a specified rank and/or synonyms for
all retrieved taxa.

## Usage

``` r
get_worms_taxonomy_tree(
  aphia_ids,
  add_descendants = FALSE,
  add_descendants_rank = "Species",
  add_synonyms = FALSE,
  add_hierarchy = FALSE,
  add_rank_to_hierarchy = FALSE,
  verbose = TRUE
)
```

## Arguments

- aphia_ids:

  Numeric vector of AphiaIDs to retrieve taxonomy for. Must not be
  missing or all NA.

- add_descendants:

  Logical (default FALSE). If TRUE, retrieves all child taxa for each
  taxon at the rank specified by `add_descendants_rank`.

- add_descendants_rank:

  Character (default `"Species"`). The taxonomic rank of descendants to
  retrieve. For example, if set to `"Species"`, the function will
  collect all species belonging to each genus present in the initial
  dataset.

- add_synonyms:

  Logical (default FALSE). If TRUE, retrieves synonym records for all
  retrieved taxa and appends them to the dataset.

- add_hierarchy:

  Logical (default FALSE). If TRUE, adds a `hierarchy` column that
  contains the concatenated hierarchy of each taxon (e.g. Kingdom -
  Phylum - Class).

- add_rank_to_hierarchy:

  Logical (default FALSE). If TRUE, the hierarchy string prepends rank
  names (e.g., `[Kingdom] Animalia - [Phylum] Chordata`) to each taxon
  name in the `hierarchy` column. Only used if `add_hierarchy = TRUE`.

- verbose:

  Logical (default TRUE). If TRUE, prints progress messages and progress
  bars during data retrieval.

## Value

A `data.frame` containing detailed WoRMS records for all requested
AphiaIDs, including optional descendants and synonyms. Typical columns
include:

- AphiaID:

  The AphiaID of the taxon.

- parentNameUsageID:

  The AphiaID of the parent taxon.

- scientificname:

  Scientific name of the taxon.

- rank:

  Taxonomic rank (e.g., Kingdom, Phylum, Genus, Species).

- status:

  Taxonomic status (e.g., accepted, unaccepted).

- valid_AphiaID:

  AphiaID of the accepted taxon, if the record is a synonym.

- species:

  Added only if a `Species` rank exists in the retrieved data and if
  `add_hierarchy = TRUE`; otherwise not present.

- parentName:

  Added only if a `parentName` rank exists in the retrieved data and if
  `add_hierarchy = TRUE`; otherwise not present.

- hierarchy:

  Added only if `add_hierarchy = TRUE` and hierarchy data are available.
  Contains a concatenated string of the taxonomic path.

- ...:

  Additional columns returned by WoRMS, including authorship and source
  information.

## Details

The function performs the following steps:

1.  Validates input AphiaIDs and removes NA values.

2.  Retrieves the hierarchical classification for each AphiaID using
    [`worrms::wm_classification()`](https://docs.ropensci.org/worrms/reference/wm_classification.html).

3.  Optionally retrieves all descendants at the rank specified by
    `add_descendants_rank` if `add_descendants = TRUE`.

4.  Optionally retrieves synonyms for all retrieved taxa if
    `add_synonyms = TRUE`.

5.  Optionally adds a `hierarchy` column if `add_hierarchy = TRUE`.

6.  Returns a combined, distinct dataset of all records.

## See also

[`add_worms_taxonomy`](https://sharksmhi.github.io/SHARK4R/reference/add_worms_taxonomy.md),
[`construct_dyntaxa_table`](https://sharksmhi.github.io/SHARK4R/reference/construct_dyntaxa_table.md)

[`wm_classification`](https://docs.ropensci.org/worrms/reference/wm_classification.html),
[`wm_children`](https://docs.ropensci.org/worrms/reference/wm_children.html),
[`wm_synonyms`](https://docs.ropensci.org/worrms/reference/wm_synonyms.html)

<https://marinespecies.org/> for the WoRMS website.

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve hierarchy for a single AphiaID
get_worms_taxonomy_tree(aphia_ids = 109604)

# Retrieve hierarchy including species-level descendants
get_worms_taxonomy_tree(
  aphia_ids = c(109604, 376667),
  add_descendants = TRUE
)

# Retrieve hierarchy including hierarchy column
get_worms_taxonomy_tree(
  aphia_ids = c(109604, 376667),
  add_hierarchy = TRUE
)
} # }
```
