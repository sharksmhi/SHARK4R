# Check if taxon names exist in Dyntaxa

Checks whether the supplied scientific names exist in the Swedish
taxonomic database Dyntaxa. Optionally, returns a data frame with taxon
names, taxon IDs, and match status.

## Usage

``` r
is_in_dyntaxa(
  taxon_names,
  subscription_key = Sys.getenv("DYNTAXA_KEY"),
  use_dwca = FALSE,
  return_df = FALSE,
  verbose = FALSE
)
```

## Arguments

- taxon_names:

  Character vector of taxon names to check.

- subscription_key:

  A Dyntaxa API subscription key. By default, the key is read from the
  environment variable `DYNTAXA_KEY`.

  You can provide the key in three ways:

  - **Directly as a parameter**:
    `is_in_dyntaxa("Skeletonema marinoi", subscription_key = "your_key_here")`.

  - **Temporarily for the session**:
    `Sys.setenv(DYNTAXA_KEY = "your_key_here")`. After this, you do not
    need to pass `subscription_key` to the function.

  - **Permanently across sessions** by adding it to your `~/.Renviron`
    file. Use `usethis::edit_r_environ()` to open the file, then add:
    `DYNTAXA_KEY=your_key_here`. After this, you do not need to pass
    `subscription_key` to the function.

- use_dwca:

  Logical; if TRUE, uses the DwCA version of Dyntaxa instead of querying
  the API.

- return_df:

  Logical; if TRUE, returns a data frame with columns `taxon_name`,
  `taxon_id`, and `match`. Default is FALSE (returns a logical vector).

- verbose:

  Logical; if TRUE, prints messages about unmatched taxa.

## Value

If `return_df = FALSE` (default), a logical vector indicating whether
each input name was found in Dyntaxa. Returned invisibly if
`verbose = TRUE`. If `return_df = TRUE`, a data frame with columns:

- `taxon_name`: original input names

- `taxon_id`: corresponding Dyntaxa taxon IDs (NA if not found)

- `match`: logical indicating presence in Dyntaxa

## Details

A valid Dyntaxa API subscription key is required. You can request a free
key for the "Taxonomy" service from the ArtDatabanken API portal:
<https://api-portal.artdatabanken.se/>

## Examples

``` r
if (FALSE) { # \dontrun{
# Using an environment variable (recommended for convenience)
Sys.setenv(DYNTAXA_KEY = "your_key_here")
is_in_dyntaxa(c("Skeletonema marinoi", "Nonexistent species"))

# Return a data frame instead of logical vector
is_in_dyntaxa(c("Skeletonema marinoi", "Nonexistent species"), return_df = TRUE)

# Or pass the key directly
is_in_dyntaxa("Skeletonema marinoi", subscription_key = "your_key_here")

# Suppress messages
is_in_dyntaxa("Skeletonema marinoi", verbose = FALSE)
} # }
```
