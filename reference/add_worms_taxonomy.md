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

A `tibble` with taxonomy columns added, including:

- `aphia_id`, `scientific_name`

- `worms_kingdom`, `worms_phylum`, `worms_class`, `worms_order`,
  `worms_family`, `worms_genus`, `worms_species`

- `worms_scientific_name`, `worms_hierarchy`

## Examples

``` r
# \donttest{
# Using AphiaID only
add_worms_taxonomy(c(1080, 109604), verbose = FALSE)
#> # A tibble: 2 × 10
#>   aphia_id worms_scientific_name worms_kingdom worms_phylum worms_class
#>      <dbl> <chr>                 <chr>         <chr>        <chr>      
#> 1     1080 Copepoda              Animalia      Arthropoda   Copepoda   
#> 2   109604 Dinophysis acuta      Chromista     Myzozoa      Dinophyceae
#> # ℹ 5 more variables: worms_order <chr>, worms_family <chr>, worms_genus <chr>,
#> #   worms_species <chr>, worms_hierarchy <chr>

# Using a combination of AphiaID and scientific name
add_worms_taxonomy(
  aphia_ids = c(NA, 109604),
  scientific_names = c("Calanus finmarchicus", "Oithona similis"),
  verbose = FALSE
)
#> # A tibble: 2 × 11
#>   aphia_id scientific_name      worms_scientific_name worms_kingdom worms_phylum
#>      <dbl> <chr>                <chr>                 <chr>         <chr>       
#> 1   104464 Calanus finmarchicus Calanus finmarchicus  Animalia      Arthropoda  
#> 2   109604 Oithona similis      Dinophysis acuta      Chromista     Myzozoa     
#> # ℹ 6 more variables: worms_class <chr>, worms_order <chr>, worms_family <chr>,
#> #   worms_genus <chr>, worms_species <chr>, worms_hierarchy <chr>
# }
```
