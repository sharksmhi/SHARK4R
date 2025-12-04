# Search AlgaeBase for information about a species of algae

This function searches the AlgaeBase API for species based on genus and
species names. It allows for flexible search parameters such as
filtering by exact matches, returning the most recent results, and
including higher taxonomy details.

## Usage

``` r
match_algaebase_species(
  genus,
  species,
  subscription_key = Sys.getenv("ALGAEBASE_KEY"),
  higher = TRUE,
  unparsed = FALSE,
  newest_only = TRUE,
  exact_matches_only = TRUE,
  apikey = deprecated()
)
```

## Arguments

- genus:

  A character string specifying the genus name.

- species:

  A character string specifying the species or specific epithet.

- subscription_key:

  A character string containing the API key for accessing the AlgaeBase
  API. By default, the key is read from the environment variable
  `ALGAEBASE_KEY`.

  You can provide the key in three ways:

  - **Directly as a parameter**:
    `match_algaebase_species("Skeletonema", "marinoi", subscription_key = "your_key_here")`.

  - **Temporarily for the session**:
    `Sys.setenv(ALGAEBASE_KEY = "your_key_here")`. After this, you do
    not need to pass `subscription_key` to the function.

  - **Permanently across sessions** by adding it to your `~/.Renviron`
    file. Use `usethis::edit_r_environ()` to open the file, then add:
    `ALGAEBASE_KEY=your_key_here`. After this, you do not need to pass
    `subscription_key` to the function.

- higher:

  A logical value indicating whether to include higher taxonomy details
  (default is `TRUE`).

- unparsed:

  A logical value indicating whether to print the full JSON response
  from the API (default is `FALSE`).

- newest_only:

  A logical value indicating whether to return only the most recent
  entries (default is `TRUE`).

- exact_matches_only:

  A logical value indicating whether to return only exact matches
  (default is `TRUE`).

- apikey:

  **\[deprecated\]** Use `subscription_key` instead.

## Value

A data frame with details about the species, including:

- `taxonomic_status` — The current status of the taxon (e.g., accepted,
  synonym, unverified).

- `taxon_rank` — The rank of the taxon (e.g., species, genus).

- `accepted_name` — The currently accepted scientific name, if
  applicable.

- `authorship` — Author information for the scientific name (if
  available).

- `mod_date` — Date when the taxonomic record was last modified.

- `...` — Other relevant information returned by the data source.

## Details

A valid API key is requested from the AlgaeBase team.

This function queries the AlgaeBase API for species based on the genus
and species names, and filters the results based on various parameters.
The function handles different taxonomic ranks and formats the output
for easy use. It can merge higher taxonomy data if requested.

## See also

<https://www.algaebase.org/> for AlgaeBase website.

## Examples

``` r
if (FALSE) { # \dontrun{
# Search for a species with exact matches only, return the most recent results
result <- match_algaebase_species(
  genus = "Skeletonema", species = "marinoi", subscription_key = "your_api_key"
)

# Print result
print(result)
} # }
```
