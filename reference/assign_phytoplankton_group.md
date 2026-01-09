# Assign phytoplankton group to scientific names

This function assigns default phytoplankton groups (Diatoms,
Dinoflagellates, Cyanobacteria, or Other) to a list of scientific names
or Aphia IDs by retrieving species information from the World Register
of Marine Species (WoRMS). The function checks both Aphia IDs and
scientific names, handles missing records, and assigns the appropriate
plankton group based on taxonomic classification in WoRMS. Additionally,
custom plankton groups can be specified using the `custom_groups`
parameter, allowing users to define additional classifications based on
specific taxonomic criteria.

## Usage

``` r
assign_phytoplankton_group(
  scientific_names,
  aphia_ids = NULL,
  diatom_class = c("Bacillariophyceae", "Coscinodiscophyceae", "Mediophyceae",
    "Diatomophyceae"),
  dinoflagellate_class = "Dinophyceae",
  cyanobacteria_class = "Cyanophyceae",
  cyanobacteria_phylum = "Cyanobacteria",
  match_first_word = TRUE,
  marine_only = FALSE,
  return_class = FALSE,
  custom_groups = list(),
  verbose = TRUE
)
```

## Arguments

- scientific_names:

  A character vector of scientific names of marine species.

- aphia_ids:

  A numeric vector of Aphia IDs corresponding to the scientific names.
  If provided, it improves the accuracy and speed of the matching
  process. The length of `aphia_ids` must match the length of
  `scientific_names`. Defaults to `NULL`, in which case the function
  will attempt to assign plankton groups based only on the scientific
  names.

- diatom_class:

  A character string or vector representing the diatom class. Default is
  "Bacillariophyceae", "Coscinodiscophyceae", "Mediophyceae" and
  "Diatomophyceae".

- dinoflagellate_class:

  A character string or vector representing the dinoflagellate class.
  Default is "Dinophyceae".

- cyanobacteria_class:

  A character string or vector representing the cyanobacteria class.
  Default is "Cyanophyceae".

- cyanobacteria_phylum:

  A character string or vector representing the cyanobacteria phylum.
  Default is "Cyanobacteria".

- match_first_word:

  A logical value indicating whether to match the first word of the
  scientific name if the Aphia ID is missing. Default is TRUE.

- marine_only:

  A logical value indicating whether to restrict the results to marine
  taxa only. Default is `FALSE`.

- return_class:

  A logical value indicating whether to include class information in the
  result. Default is `FALSE`.

- custom_groups:

  A named list of additional custom plankton groups (optional). The
  names of the list correspond to the custom group names (e.g.,
  "Cryptophytes"), and the values should be character vectors specifying
  one or more of the following taxonomic levels: `phylum`, `class`,
  `order`, `family`, `genus`, or `scientific_name`. For example:
  `list("Green Algae" = list(class = c("Chlorophyceae", "Ulvophyceae")))`.
  This allows users to extend the default classifications (e.g.,
  Cyanobacteria, Diatoms, Dinoflagellates) with their own groups.

- verbose:

  A logical value indicating whether to print progress messages. Default
  is TRUE.

## Value

A `tibble` with two columns: `scientific_name` and `plankton_group`,
where the plankton group is assigned based on taxonomic classification.

## Details

The `aphia_ids` parameter is not necessary but, if provided, will
improve the certainty of the matching process. If `aphia_ids` are
available, they will be used directly to retrieve more accurate WoRMS
records. If missing, the function will attempt to match the scientific
names to Aphia IDs by querying WoRMS using the scientific name(s), with
an additional fallback mechanism to match based on the first word of the
scientific name.

To skip one of the default plankton groups, you can set the class or
phylum of the respective group to an empty string (`""`). For example,
to skip the "Cyanobacteria" group, you can set
`cyanobacteria_class = ""` or `cyanobacteria_phylum = ""`. These taxa
will then be placed in `Others`.

Custom groups are processed in the order they appear in the
`custom_groups` list. If a taxon matches multiple custom groups, it will
be assigned to the group that appears last in the list, as later matches
overwrite earlier ones. For example, if `Teleaulax amphioxeia` matches
both `Cryptophytes` (class-based) and a specific group `Teleaulax`
(name-based), it will be assigned to `Teleaulax` if `Teleaulax` is
listed after `Cryptophytes` in the `custom_groups` list.

## See also

<https://marinespecies.org/> for WoRMS website.

<https://CRAN.R-project.org/package=worrms>

## Examples

``` r
# \donttest{
# Assign plankton groups to a list of species names
result <- assign_phytoplankton_group(
  scientific_names = c("Tripos fusus", "Diatoma", "Nodularia spumigena", "Octactis speculum"),
  verbose = FALSE)

print(result)
#> # A tibble: 4 × 2
#>   scientific_name     plankton_group 
#>   <chr>               <chr>          
#> 1 Tripos fusus        Dinoflagellates
#> 2 Diatoma             Diatoms        
#> 3 Nodularia spumigena Cyanobacteria  
#> 4 Octactis speculum   Other          

# Improve classification by explicitly providing Aphia IDs for ambiguous taxa
# Actinocyclus and Navicula are names shared by both diatoms and animals,
# which can lead to incorrect group assignment without an Aphia ID
result <- assign_phytoplankton_group(
  scientific_names = c("Actinocyclus", "Navicula", "Nodularia spumigena", "Tripos fusus"),
  aphia_ids = c(148944, 149142, NA, NA),
  verbose = FALSE)

print(result)
#> # A tibble: 4 × 2
#>   scientific_name     plankton_group 
#>   <chr>               <chr>          
#> 1 Actinocyclus        Diatoms        
#> 2 Navicula            Diatoms        
#> 3 Nodularia spumigena Cyanobacteria  
#> 4 Tripos fusus        Dinoflagellates

# Assign plankton groups using additional custom grouping
custom_groups <- list(
    Cryptophytes = list(class = "Cryptophyceae"),
    Ciliates = list(phylum = "Ciliophora")
)

# Assign with custom groups
result_custom <- assign_phytoplankton_group(
  scientific_names = c("Teleaulax amphioxeia", "Mesodinium rubrum", "Dinophysis acuta"),
  aphia_ids = c(106306, 232069, 109604),
  custom_groups = custom_groups,         # Adding custom groups
  verbose = FALSE
)

print(result_custom)
#> # A tibble: 3 × 2
#>   scientific_name      plankton_group 
#>   <chr>                <chr>          
#> 1 Teleaulax amphioxeia Cryptophytes   
#> 2 Mesodinium rubrum    Ciliates       
#> 3 Dinophysis acuta     Dinoflagellates
# }
```
