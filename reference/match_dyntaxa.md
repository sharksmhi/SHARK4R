# Taxon matching using Dyntaxa (https://www.dyntaxa.se/)

**\[deprecated\]**

This function is deprecated and has been replaced by
[`is_in_dyntaxa()`](https://sharksmhi.github.io/SHARK4R/reference/is_in_dyntaxa.md).

## Usage

``` r
match_dyntaxa(names, subscription_key = Sys.getenv("DYNTAXA_KEY"))
```

## Arguments

- names:

  Character vector of scientific names to check in Dyntaxa.

- subscription_key:

  A Dyntaxa API subscription key. By default, the key is read from the
  environment variable `DYNTAXA_KEY`.

## Value

A logical vector indicating whether each input name was found in
Dyntaxa, same as
[`is_in_dyntaxa()`](https://sharksmhi.github.io/SHARK4R/reference/is_in_dyntaxa.md).
Messages about unmatched taxa are printed.

## Details

This function is retained for backward compatibility but may be removed
in future versions. Use the newer function
[`is_in_dyntaxa()`](https://sharksmhi.github.io/SHARK4R/reference/is_in_dyntaxa.md)
instead.

A valid Dyntaxa API subscription key is required. You can request a free
key for the "Taxonomy" service from the ArtDatabanken API portal:
<https://api-portal.artdatabanken.se/>

## Examples

``` r
if (FALSE) { # \dontrun{
# Deprecated function usage
match_dyntaxa(c("Skeletonema marinoi", "Nonexistent species"),
              subscription_key = "your_key_here")
} # }
```
