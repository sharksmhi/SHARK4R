# Parse scientific names into genus and species components.

This function processes a character vector of scientific names,
splitting them into genus and species components. It handles binomial
names (e.g., "Homo sapiens"), removes undesired descriptors (e.g.,
'Cfr.', 'cf.', 'sp.', 'spp.'), and manages cases involving varieties,
subspecies, or invalid species names. Special characters and whitespace
are handled appropriately.

## Usage

``` r
parse_scientific_names(
  scientific_names,
  remove_undesired_descriptors = TRUE,
  remove_subspecies = TRUE,
  remove_invalid_species = TRUE,
  encoding = "UTF-8",
  scientific_name = deprecated()
)
```

## Arguments

- scientific_names:

  A character vector containing scientific names, which may include
  binomials, additional descriptors, or varieties.

- remove_undesired_descriptors:

  Logical, if TRUE, undesired descriptors (e.g., 'Cfr.', 'cf.',
  'colony', 'cells', etc.) are removed. Default is TRUE.

- remove_subspecies:

  Logical, if TRUE, subspecies/variety descriptors (e.g., 'var.',
  'subsp.', 'f.', etc.) are removed. Default is TRUE.

- remove_invalid_species:

  Logical, if TRUE, invalid species names (e.g., 'sp.', 'spp.') are
  removed. Default is TRUE.

- encoding:

  A string specifying the encoding to be used for the input names (e.g.,
  'UTF-8'). Default is 'UTF-8'.

- scientific_name:

  **\[deprecated\]** Use `scientific_names` instead.

## Value

A `tibble` with two columns:

- `genus` — Genus names.

- `species` — Species names (empty if unavailable or invalid). Invalid
  descriptors such as `"sp."`, `"spp."`, and numeric entries are
  excluded from this column.

## See also

<https://www.algaebase.org/> for AlgaeBase website.

## Examples

``` r
# Example with a vector of scientific names
scientific_names <- c("Skeletonema marinoi", "Cf. Azadinium perforatum", "Gymnodinium sp.",
                      "Melosira varians", "Aulacoseira islandica var. subarctica")

# Parse names
result <- parse_scientific_names(scientific_names)

# Check the resulting data
print(result)
#> # A tibble: 5 × 2
#>   genus       species               
#>   <chr>       <chr>                 
#> 1 Skeletonema "marinoi"             
#> 2 Azadinium   "perforatum"          
#> 3 Gymnodinium ""                    
#> 4 Melosira    "varians"             
#> 5 Aulacoseira "islandica subarctica"
```
