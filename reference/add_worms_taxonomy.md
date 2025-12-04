# Add WoRMS taxonomy hierarchy to AphiaIDs or scientific names

This function enhances a dataset of AphiaIDs (and optionally scientific
names) with their complete hierarchical taxonomy from the World Register
of Marine Species (WoRMS). Missing AphiaIDs can be resolved from
scientific names automatically.

## Usage

``` r
add_worms_taxonomy(
  aphia_ids,
  scientific_names = NULL,
  add_rank_to_hierarchy = FALSE,
  verbose = TRUE,
  aphia_id = deprecated(),
  scientific_name = deprecated()
)
```

## Arguments

- aphia_ids:

  Numeric vector of AphiaIDs.

- scientific_names:

  Optional character vector of scientific names (same length as
  `aphia_id`).

- add_rank_to_hierarchy:

  Logical (default FALSE). If TRUE, includes rank labels in the
  concatenated hierarchy string.

- verbose:

  Logical (default TRUE). If TRUE, prints progress updates.

- aphia_id:

  **\[deprecated\]** Use `aphia_ids` instead.

- scientific_name:

  **\[deprecated\]** Use `scientific_names` instead.

## Value

A tibble with taxonomy columns added, including:

- `aphia_id`, `scientific_name`

- `worms_kingdom`, `worms_phylum`, `worms_class`, `worms_order`,
  `worms_family`, `worms_genus`, `worms_species`

- `worms_scientific_name`, `worms_hierarchy`

## Examples

``` r
if (FALSE) { # \dontrun{
add_worms_taxonomy(c(1080, 109604))

add_worms_taxonomy(
  aphia_ids = c(NA, 109604),
  scientific_names = c("Calanus finmarchicus", "Oithona similis")
)
} # }
```
