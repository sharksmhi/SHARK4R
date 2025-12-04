# Retrieve external links or facts for taxa from Nordic Microalgae

This function retrieves external links related to algae taxa from the
Nordic Microalgae API. It takes a vector of slugs (taxon identifiers)
and returns a data frame containing the external links associated with
each taxon. The data includes the provider, label, external ID, and the
URL of the external link.

## Usage

``` r
get_nua_external_links(slug, verbose = TRUE, unparsed = FALSE)
```

## Arguments

- slug:

  A vector of taxon slugs (identifiers) for which to retrieve external
  links.

- verbose:

  A logical flag indicating whether to display a progress bar. Default
  is `TRUE`.

- unparsed:

  Logical. If `TRUE`, the API response with all facts is returned as an
  unparsed list. Default is `FALSE`.

## Value

When unparsed = `FALSE`: a data frame containing the following columns:

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

  The collection category, which is "External Links" for all rows.

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
  external_links <- get_nua_external_links(slug = c("chaetoceros-debilis", "alexandrium-tamarense"))
  head(external_links)
} # }
```
