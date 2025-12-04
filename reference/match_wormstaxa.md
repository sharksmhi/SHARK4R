# Taxon matching using WoRMS (http://www.marinespecies.org/)

**\[deprecated\]**

This function has been deprecated. Users are encouraged to use
[`match_worms_taxa`](https://sharksmhi.github.io/SHARK4R/reference/match_worms_taxa.md)
instead.

matches latin name in data with WoRMS taxon list

## Usage

``` r
match_wormstaxa(names, ask = TRUE)
```

## Arguments

- names:

  Vector of scientific names.

- ask:

  Ask user in case of multiple matches.

## Value

Data frame with scientific name, scientific name ID and match type.

## References

Provoost P, Bosch S (2025). obistools: Tools for data enhancement and
quality control. Ocean Biodiversity Information System.
Intergovernmental Oceanographic Commission of UNESCO. R package version
0.1.0, <https://iobis.github.io/obistools/>.
