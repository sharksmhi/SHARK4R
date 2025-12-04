# Retrieve taxa information from Nordic Microalgae

This function retrieves all taxonomic information for algae taxa from
the Nordic Microalgae API. It fetches details including scientific
names, authorities, ranks, and image URLs (in different sizes: large,
medium, original, and small).

## Usage

``` r
get_nua_taxa(unparsed = FALSE)
```

## Arguments

- unparsed:

  Logical. If `TRUE`, complete API response is returned as an unparsed
  list. Default is `FALSE`.

## Value

When unparsed = `FALSE`: a data frame containing the following columns:

- slug:

  A unique identifier for the taxon.

- scientific_name:

  The scientific name of the taxon.

- authority:

  The authority associated with the scientific name.

- rank:

  The taxonomic rank of the taxon.

## See also

<https://nordicmicroalgae.org/> for Nordic Microalgae website.

<https://nordicmicroalgae.org/api/> for Nordic Microalgae API
documentation.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Retrieve and display taxa data
  taxa_data <- get_nua_taxa(unparsed = FALSE)
  head(taxa_data)
} # }
```
