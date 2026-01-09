# Search AlgaeBase for taxonomic information

**\[deprecated\]**

This function has been deprecated. Users are encouraged to use
[`match_algaebase_taxa`](https://sharksmhi.github.io/SHARK4R/reference/match_algaebase_taxa.md)
instead.

This function queries the AlgaeBase API to retrieve taxonomic
information for a list of algae names based on genus and (optionally)
species. It supports exact matching, genus-only searches, and retrieval
of higher taxonomic ranks.

## Usage

``` r
match_algaebase(
  genus,
  species,
  subscription_key = Sys.getenv("ALGAEBASE_KEY"),
  genus_only = FALSE,
  higher = TRUE,
  unparsed = FALSE,
  exact_matches_only = TRUE,
  sleep_time = 1,
  newest_only = TRUE,
  verbose = TRUE,
  apikey = deprecated()
)
```

## Arguments

- genus:

  A character vector of genus names.

- species:

  A character vector of species names corresponding to the `genus`
  vector. Must be the same length as `genus`.

- subscription_key:

  A character string containing the API key for accessing the AlgaeBase
  API. By default, the key is read from the environment variable
  `ALGAEBASE_KEY`.

  You can provide the key in three ways:

  - **Directly as a parameter**:
    `match_algaebase("Skeletonema", "marinoi", subscription_key = "your_key_here")`.

  - **Temporarily for the session**:
    `Sys.setenv(ALGAEBASE_KEY = "your_key_here")`. After this, you do
    not need to pass `subscription_key` to the function.

  - **Permanently across sessions** by adding it to your `~/.Renviron`
    file. Use `usethis::edit_r_environ()` to open the file, then add:
    `ALGAEBASE_KEY=your_key_here`. After this, you do not need to pass
    `subscription_key` to the function.

- genus_only:

  Logical. If `TRUE`, searches are based solely on the genus name,
  ignoring species. Defaults to `FALSE`.

- higher:

  Logical. If `TRUE`, includes higher taxonomy (e.g., kingdom, phylum)
  in the output. Defaults to `TRUE`.

- unparsed:

  Logical. If `TRUE`, returns raw JSON output instead of a `tibble`.
  Defaults to `FALSE`.

- exact_matches_only:

  Logical. If `TRUE`, restricts results to exact matches. Defaults to
  `TRUE`.

- sleep_time:

  Numeric. The delay (in seconds) between consecutive AlgaeBase API
  queries. Defaults to `1`. A delay is recommended to avoid overwhelming
  the API for large queries.

- newest_only:

  A logical value indicating whether to return only the most recent
  entries (default is `TRUE`).

- verbose:

  Logical. If `TRUE`, displays a progress bar to indicate query status.
  Defaults to `TRUE`.

- apikey:

  **\[deprecated\]** Use `subscription_key` instead.

## Value

A `tibble` containing taxonomic information for each input genus–species
combination. The following columns may be included:

- `id` — AlgaeBase ID (if available).

- `kingdom`, `phylum`, `class`, `order`, `family` — Higher taxonomy
  (returned if `higher = TRUE`).

- `genus`, `species`, `infrasp` — Genus, species, and infraspecies names
  (if applicable).

- `taxonomic_status` — Status of the name (e.g., accepted, synonym,
  unverified).

- `currently_accepted` — Logical indicator whether the name is currently
  accepted (`TRUE`/`FALSE`).

- `accepted_name` — Currently accepted name if different from the input
  name.

- `input_name` — The name supplied by the user.

- `input_match` — Indicator of exact match (`1` = exact, `0` = not
  exact).

- `taxon_rank` — Taxonomic rank of the accepted name (e.g., genus,
  species).

- `mod_date` — Date when the entry was last modified in AlgaeBase.

- `long_name` — Full species name with authorship and date.

- `authorship` — Author(s) associated with the species name.

## Details

A valid API key is requested from the AlgaeBase team.

Scientific names can be parsed using the
[`parse_scientific_names()`](https://sharksmhi.github.io/SHARK4R/reference/parse_scientific_names.md)
function before being processed by `match_algaebase()`.

Duplicate genus-species combinations are handled efficiently by querying
each unique combination only once. Genus-only searches are performed
when `genus_only = TRUE` or when the species name is missing or invalid.
Errors during API queries are gracefully handled by returning rows with
`NA` values for missing or unavailable data.

The function allows for integration with data analysis workflows that
require resolving or verifying taxonomic names against AlgaeBase.

## See also

<https://www.algaebase.org/> for AlgaeBase website.

[`parse_scientific_names`](https://sharksmhi.github.io/SHARK4R/reference/parse_scientific_names.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with genus and species vectors
genus_vec <- c("Thalassiosira", "Skeletonema", "Tripos")
species_vec <- c("pseudonana", "costatum", "furca")

algaebase_results <- match_algaebase(
  genus = genus_vec,
  species = species_vec,
  subscription_key = "your_api_key",
  exact_matches_only = TRUE,
  verbose = TRUE
)
head(algaebase_results)
} # }
```
