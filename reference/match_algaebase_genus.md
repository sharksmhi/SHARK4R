# Search AlgaeBase for information about a genus of algae

This function searches the AlgaeBase API for genus information and
returns detailed taxonomic data, including higher taxonomy, taxonomic
status, scientific names, and other related metadata.

## Usage

``` r
match_algaebase_genus(
  genus,
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

  The genus name to search for (character string). This parameter is
  required.

- subscription_key:

  A character string containing the API key for accessing the AlgaeBase
  API. By default, the key is read from the environment variable
  `ALGAEBASE_KEY`.

  You can provide the key in three ways:

  - **Directly as a parameter**:
    `match_algaebase_genus("Skeletonema", subscription_key = "your_key_here")`.

  - **Temporarily for the session**:
    `Sys.setenv(ALGAEBASE_KEY = "your_key_here")`. After this, you do
    not need to pass `subscription_key` to the function.

  - **Permanently across sessions** by adding it to your `~/.Renviron`
    file. Use `usethis::edit_r_environ()` to open the file, then add:
    `ALGAEBASE_KEY=your_key_here`. After this, you do not need to pass
    `subscription_key` to the function.

- higher:

  A boolean flag indicating whether to include higher taxonomy in the
  output (default is TRUE).

- unparsed:

  A boolean flag indicating whether to return the raw JSON output from
  the API (default is FALSE).

- newest_only:

  A boolean flag to return only the most recent entry (default is TRUE).

- exact_matches_only:

  A boolean flag to limit results to exact matches (default is TRUE).

- apikey:

  **\[deprecated\]** Use `subscription_key` instead.

## Value

A `tibble` with the following columns:

- `id` — AlgaeBase identifier.

- `accepted_name` — Accepted scientific name (if different from the
  input).

- `input_name` — The genus name supplied by the user.

- `input_match` — Indicator of exact match (`1` = exact, `0` = not
  exact).

- `currently_accepted` — Indicator if the taxon is currently accepted
  (`1` = TRUE, `0` = FALSE).

- `genus_only` — Indicator if the search was for a genus only (`1` =
  genus, `0` = genus + species).

- `kingdom`, `phylum`, `class`, `order`, `family` — Higher taxonomy
  (returned if `higher = TRUE`).

- `taxonomic_status` — Status of the taxon (e.g., currently accepted,
  synonym, unverified).

- `taxon_rank` — Taxonomic rank of the accepted name (e.g., genus,
  species).

- `mod_date` — Date when the entry was last modified.

- `long_name` — Full scientific name including author and date (if
  available).

- `authorship` — Author information (if available).

## Details

A valid API key is requested from the AlgaeBase team.

## See also

<https://www.algaebase.org/> for AlgaeBase website.

## Examples

``` r
if (FALSE) { # \dontrun{
  match_algaebase_genus("Anabaena", subscription_key = "your_api_key")
} # }
```
