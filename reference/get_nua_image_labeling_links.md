# Retrieve image labeling media links from Nordic Microalgae

This function retrieves media URLs for automated imaging images from the
Nordic Microalgae API. These are images from automated imaging
instruments (e.g., IFCB) used for image labeling purposes. It returns
URLs for different renditions (large, medium, original, small) along
with basic attribution.

## Usage

``` r
get_nua_image_labeling_links(unparsed = FALSE)
```

## Arguments

- unparsed:

  Logical. If `TRUE`, complete API response is returned as an unparsed
  list. Default is `FALSE`.

## Value

When unparsed = `FALSE`: a `tibble` with the following columns:

- `slug`: The slug of the related taxon.

- `image_l_url`: The URL for the "large" rendition.

- `image_o_url`: The URL for the "original" rendition.

- `image_s_url`: The URL for the "small" rendition.

- `image_m_url`: The URL for the "medium" rendition.

- `contributor`: The contributor of the media item.

- `copyright_holder`: The copyright holder.

- `license`: The license of the media item.

- `imaging_instrument`: Comma-separated list of imaging instruments.

- `priority`: The priority of the image.

## See also

<https://nordicmicroalgae.org/> for Nordic Microalgae website.

<https://nordicmicroalgae.org/api/> for Nordic Microalgae API
documentation.

[`get_nua_image_labeling_metadata`](https://sharksmhi.github.io/SHARK4R/reference/get_nua_image_labeling_metadata.md)
for retrieving full metadata for image labeling images.

[`get_nua_media_links`](https://sharksmhi.github.io/SHARK4R/reference/get_nua_media_links.md)
for retrieving regular media image URLs.

## Examples

``` r
# \donttest{
# Retrieve image labeling media links
il_links <- get_nua_image_labeling_links(unparsed = FALSE)

# Preview the extracted data
head(il_links)
#> # A tibble: 6 × 10
#>   slug            image_l_url    image_o_url image_s_url image_m_url contributor
#>   <chr>           <chr>          <chr>       <chr>       <chr>       <chr>      
#> 1 tripos-muelleri https://nordi… https://no… https://no… https://no… Anders Tor…
#> 2 tripos-muelleri https://nordi… https://no… https://no… https://no… Anders Tor…
#> 3 tripos-muelleri https://nordi… https://no… https://no… https://no… Anders Tor…
#> 4 tripos-muelleri https://nordi… https://no… https://no… https://no… Anders Tor…
#> 5 tripos-muelleri https://nordi… https://no… https://no… https://no… Anders Tor…
#> 6 tripos-lineatus https://nordi… https://no… https://no… https://no… Anders Tor…
#> # ℹ 4 more variables: copyright_holder <chr>, license <chr>,
#> #   imaging_instrument <chr>, priority <int>
# }
```
