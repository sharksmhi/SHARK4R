# Construct a hierarchical taxonomy table from Dyntaxa

This function constructs a taxonomy table based on Dyntaxa taxon IDs. It
queries the SLU Artdatabanken API (Dyntaxa) to fetch taxonomy
information and organizes the data into a hierarchical table.

## Usage

``` r
construct_dyntaxa_table(
  taxon_ids,
  subscription_key = Sys.getenv("DYNTAXA_KEY"),
  shark_output = TRUE,
  add_parents = TRUE,
  add_descendants = FALSE,
  add_descendants_rank = "genus",
  add_synonyms = TRUE,
  add_missing_taxa = FALSE,
  add_hierarchy = FALSE,
  verbose = TRUE,
  add_genus_children = deprecated(),
  recommended_only = deprecated(),
  parent_ids = deprecated()
)
```

## Arguments

- taxon_ids:

  An integer vector containing taxon IDs for which taxonomy information
  is requested. These IDs should correspond to specific taxonomic
  entities within the Dyntaxa database.

- subscription_key:

  A Dyntaxa API subscription key. By default, the key is read from the
  environment variable `DYNTAXA_KEY`.

  You can provide the key in three ways:

  - **Directly as a parameter**:
    `construct_dyntaxa_table(238366, subscription_key = "your_key_here")`.

  - **Temporarily for the session**:
    `Sys.setenv(DYNTAXA_KEY = "your_key_here")`. After this, you do not
    need to pass `subscription_key` to the function.

  - **Permanently across sessions** by adding it to your `~/.Renviron`
    file. Use `usethis::edit_r_environ()` to open the file, then add:
    `DYNTAXA_KEY=your_key_here`. After this, you do not need to pass
    `subscription_key` to the function.

- shark_output:

  Logical. If TRUE, the function will return a table formatted with
  SHARK-compatible columns. If FALSE, all available columns are
  returned. Default is TRUE.

- add_parents:

  Logical. If TRUE, the function will include parent taxa (higher ranks)
  for the specified taxon IDs in the output. Default is TRUE.

- add_descendants:

  Logical. If TRUE, the output will include descendant taxa (lower
  ranks) for the specified taxon IDs and the rank specified in
  `add_descendants_rank`. Default is FALSE.

- add_descendants_rank:

  Character string specifying the rank of descendant taxa to include.
  Allowed values are "kingdom", "phylum", "class", "order", "family",
  "genus", and "species". Default is "genus".

- add_synonyms:

  Logical. If TRUE, the function will include synonyms for the accepted
  taxa in the output. Default is TRUE.

- add_missing_taxa:

  Logical. If TRUE, the function will attempt to fetch missing taxa
  (i.e., taxa not found in the initial Dyntaxa DwC-A query, such as
  species complexes). Default is FALSE.

- add_hierarchy:

  Logical. If TRUE, the function will add a hierarchy column indicating
  the taxonomic relationships (e.g., parent-child) among the taxa.
  Default is FALSE.

- verbose:

  Logical. If TRUE, the function will print additional messages to
  provide feedback on its progress. Default is TRUE.

- add_genus_children:

  **\[deprecated\]** Use `add_descendants` instead.

- recommended_only:

  **\[deprecated\]** Use `add_synonyms` instead.

- parent_ids:

  **\[deprecated\]** Use `taxon_ids` instead. `construct_dyntaxa_table`
  now handles taxon IDs.

## Value

A data frame representing the constructed taxonomy table.

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

[`get_worms_taxonomy_tree`](https://sharksmhi.github.io/SHARK4R/reference/get_worms_taxonomy_tree.md)
for an equivalent WoRMS function

[SLU Artdatabanken API
Documentation](https://api-portal.artdatabanken.se/)

## Examples

``` r
if (FALSE) { # \dontrun{
# Construct Dyntaxa taxonomy table for taxon IDs 238366 and 1010380
taxon_ids <- c(238366, 1010380)
taxonomy_table <- construct_dyntaxa_table(taxon_ids, "your_subscription_key")
print(taxonomy_table)
} # }
```
