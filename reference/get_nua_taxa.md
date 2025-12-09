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

When unparsed = `FALSE`: a `tibble` containing the following columns:

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
# \donttest{
  # Retrieve and display taxa data
  taxa_data <- get_nua_taxa(unparsed = FALSE)
  head(taxa_data)
#> # A tibble: 6 × 5
#>   scientific_name          authority                    rank    slug     nua_url
#>   <chr>                    <chr>                        <chr>   <chr>    <chr>  
#> 1 Abollifer                Vørs, 1992                   Genus   abollif… https:…
#> 2 Abollifer prolabens      Vørs, 1992                   Species abollif… https:…
#> 3 Acanthoceras             Honigm., 1910                Genus   acantho… https:…
#> 4 Acanthoceras zachariasii (Brun) Simonsen, 1979        Species acantho… https:…
#> 5 Acanthocerataceae        Round, Crawford & Mann, 1990 Family  acantho… https:…
#> 6 Acanthocorbis            S.Hara & E.Takahashi, 1984   Genus   acantho… https:…
# }
```
