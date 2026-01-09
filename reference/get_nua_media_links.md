# Retrieve and extract media URLs from Nordic Microalgae

This function retrieves media information from the Nordic Microalgae API
and extracts slugs and URLs for different renditions (large, original,
small, medium) for each media item.

## Usage

``` r
get_nua_media_links(unparsed = FALSE)
```

## Arguments

- unparsed:

  Logical. If `TRUE`, complete API response is returned as an unparsed
  list. Default is `FALSE`.

## Value

When unparsed = `FALSE`: a `tibble` with the following columns:

- `slug`: The slug of the related taxon.

- `l_url`: The URL for the "large" rendition.

- `o_url`: The URL for the "original" rendition.

- `s_url`: The URL for the "small" rendition.

- `m_url`: The URL for the "medium" rendition.

## See also

<https://nordicmicroalgae.org/> for Nordic Microalgae website.

<https://nordicmicroalgae.org/api/> for Nordic Microalgae API
documentation.

## Examples

``` r
# \donttest{
# Retrieve media information
media_info <- get_nua_media_links(unparsed = FALSE)

# Preview the extracted data
head(media_info)
#> # A tibble: 6 × 10
#>   slug               image_l_url image_o_url image_s_url image_m_url contributor
#>   <chr>              <chr>       <chr>       <chr>       <chr>       <chr>      
#> 1 octactis-speculum  https://no… https://no… https://no… https://no… SMHI       
#> 2 lennoxia-faveolata https://no… https://no… https://no… https://no… Maria Karl…
#> 3 NA                 https://no… https://no… https://no… https://no… Per Wilhel…
#> 4 NA                 https://no… https://no… https://no… https://no… Per Wilhel…
#> 5 NA                 https://no… https://no… https://no… https://no… Per Wilhel…
#> 6 NA                 https://no… https://no… https://no… https://no… Per Wilhel…
#> # ℹ 4 more variables: photographer_artist <chr>, copyright_holder <chr>,
#> #   license <chr>, galleries <chr>
# }
```
