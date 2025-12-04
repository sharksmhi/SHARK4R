# Update SHARK taxonomy records using Dyntaxa

This function updates Dyntaxa taxonomy records based on a list of
Dyntaxa taxon IDs. It collects parent IDs from SLU Artdatabanken API
(Dyntaxa), retrieves full taxonomy records, and organizes the data into
a full taxonomic table that can be joined with data downloaded from
[SHARK](https://shark.smhi.se/)

## Usage

``` r
update_dyntaxa_taxonomy(
  dyntaxa_ids,
  subscription_key = Sys.getenv("DYNTAXA_KEY"),
  add_missing_taxa = FALSE,
  verbose = TRUE
)
```

## Arguments

- dyntaxa_ids:

  A vector of Dyntaxa taxon IDs to update.

- subscription_key:

  A Dyntaxa API subscription key. By default, the key is read from the
  environment variable `DYNTAXA_KEY`.

  You can provide the key in three ways:

  - **Directly as a parameter**:
    `update_dyntaxa_taxonomy(238366, subscription_key = "your_key_here")`.

  - **Temporarily for the session**:
    `Sys.setenv(DYNTAXA_KEY = "your_key_here")`. After this, you do not
    need to pass `subscription_key` to the function.

  - **Permanently across sessions** by adding it to your `~/.Renviron`
    file. Use `usethis::edit_r_environ()` to open the file, then add:
    `DYNTAXA_KEY=your_key_here`. After this, you do not need to pass
    `subscription_key` to the function.

- add_missing_taxa:

  Logical. If TRUE, the function will attempt to fetch missing taxa
  (i.e., taxon_ids not found in the initial Dyntaxa DwC-A query).
  Default is FALSE.

- verbose:

  Logical. Print progress messages. Default is TRUE.

## Value

A data frame representing the updated Dyntaxa taxonomy table.

## Details

A valid Dyntaxa API subscription key is required. You can request a free
key for the "Taxonomy" service from the ArtDatabanken API portal:
<https://api-portal.artdatabanken.se/>

**Note**: Please review the [API
conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
and [register for access](https://api-portal.artdatabanken.se/) before
using the API. Data collected through the API is stored at SLU
Artdatabanken. Please also note that the authors of `SHARK4R` are not
affiliated with SLU Artdatabanken.

## See also

[`get_shark_data`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_data.md),
[`update_worms_taxonomy`](https://sharksmhi.github.io/SHARK4R/reference/update_worms_taxonomy.md),
[SLU Artdatabanken API
Documentation](https://api-portal.artdatabanken.se/)

## Examples

``` r
if (FALSE) { # \dontrun{
# Update Dyntaxa taxonomy for taxon IDs 238366 and 1010380
updated_taxonomy <- update_dyntaxa_taxonomy(c(238366, 1010380), "your_subscription_key")
print(updated_taxonomy)
} # }

```
