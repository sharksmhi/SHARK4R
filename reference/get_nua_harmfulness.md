# Retrieve harmfulness for taxa from Nordic Microalgae

This function retrieves harmfulness information related to algae taxa
from the Nordic Microalgae API. It takes a vector of slugs (taxon
identifiers) and returns a data frame containing the harmfulness
information associated with each taxon. The data includes the provider,
label, external ID, and the URL of the external link.

## Usage

``` r
get_nua_harmfulness(slug, verbose = TRUE)
```

## Arguments

- slug:

  A vector of taxon slugs (identifiers) for which to retrieve external
  links.

- verbose:

  A logical flag indicating whether to display a progress bar. Default
  is `TRUE`.

## Value

A data frame containing the following columns:

- slug:

  The slug (identifier) of the taxon.

- provider:

  The provider of the external link.

- label:

  The label of the external link.

- external_id:

  The external ID associated with the external link.

- external_url:

  The URL of the external link.

- collection:

  The collection category, which is "Harmful algae blooms" for all rows.

## Details

The slugs (taxon identifiers) used in this function can be retrieved
using the
[`get_nua_taxa()`](https://sharksmhi.github.io/SHARK4R/reference/get_nua_taxa.md)
function, which returns a data frame with a column for taxon slugs,
along with other relevant metadata for each taxon.

## See also

<https://nordicmicroalgae.org/> for Nordic Microalgae website.

<https://nordicmicroalgae.org/api/> for Nordic Microalgae API
documentation.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Retrieve external links for a vector of slugs
  harmfulness <- get_nua_harmfulness(slug = c("dinophysis-acuta",
                                              "alexandrium-ostenfeldii"))
  print(harmfulness)
} # }
```
