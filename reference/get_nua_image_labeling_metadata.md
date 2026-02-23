# Retrieve image labeling metadata from Nordic Microalgae

This function retrieves detailed metadata for automated imaging images
from the Nordic Microalgae API. These are images from automated imaging
instruments (e.g., IFCB) used for image labeling purposes. It returns
comprehensive metadata including location, instrument, dataset, and
taxonomic information.

## Usage

``` r
get_nua_image_labeling_metadata(unparsed = FALSE)
```

## Arguments

- unparsed:

  Logical. If `TRUE`, complete API response is returned as an unparsed
  list. Default is `FALSE`.

## Value

When unparsed = `FALSE`: a `tibble` with the following columns:

- `slug`: The slug of the media item.

- `taxon_slug`: The slug of the related taxon.

- `scientific_name`: The scientific name of the related taxon.

- `file`: The filename of the media item.

- `type`: The MIME type of the media item.

- `title`: The title of the media item.

- `caption`: The caption of the media item.

- `license`: The license of the media item.

- `location`: The location where the media was collected.

- `contributor`: The contributor of the media item.

- `copyright_holder`: The copyright holder.

- `imaging_instrument`: Comma-separated list of imaging instruments.

- `training_dataset`: DOI or URL of the training dataset.

- `sampling_date`: The date the sample was collected.

- `geographic_area`: The geographic area of collection.

- `latitude_degree`: The latitude in degrees.

- `longitude_degree`: The longitude in degrees.

- `institute`: Comma-separated list of institutes.

- `contributing_organisation`: The contributing organisation.

- `priority`: The priority of the image.

- `created_at`: The creation timestamp.

- `updated_at`: The last update timestamp.

## See also

<https://nordicmicroalgae.org/> for Nordic Microalgae website.

<https://nordicmicroalgae.org/api/> for Nordic Microalgae API
documentation.

[`get_nua_image_labeling_links`](https://sharksmhi.github.io/SHARK4R/reference/get_nua_image_labeling_links.md)
for retrieving image labeling media URLs.

[`get_nua_media_metadata`](https://sharksmhi.github.io/SHARK4R/reference/get_nua_media_metadata.md)
for retrieving regular media metadata.

## Examples

``` r
# \donttest{
# Retrieve image labeling metadata
il_metadata <- get_nua_image_labeling_metadata(unparsed = FALSE)

# Preview the extracted data
head(il_metadata)
#> # A tibble: 6 × 22
#>   slug     taxon_slug scientific_name file  type  title caption license location
#>   <chr>    <chr>      <chr>           <chr> <chr> <chr> <chr>   <chr>   <chr>   
#> 1 tripos-… tripos-mu… Tripos muelleri trip… imag… Trip… ""      Creati… Tångesu…
#> 2 tripos-… tripos-mu… Tripos muelleri trip… imag… Trip… ""      Creati… Tångesu…
#> 3 tripos-… tripos-mu… Tripos muelleri trip… imag… Trip… ""      Creati… Tångesu…
#> 4 tripos-… tripos-mu… Tripos muelleri trip… imag… Trip… ""      Creati… Tångesu…
#> 5 tripos-… tripos-mu… Tripos muelleri trip… imag… Trip… ""      Creati… Tångesu…
#> 6 tripos-… tripos-li… Tripos lineatus trip… imag… Trip… ""      Creati… Tångesu…
#> # ℹ 13 more variables: contributor <chr>, copyright_holder <chr>,
#> #   imaging_instrument <chr>, training_dataset <chr>, sampling_date <lgl>,
#> #   geographic_area <chr>, latitude_degree <chr>, longitude_degree <chr>,
#> #   institute <chr>, contributing_organisation <chr>, priority <int>,
#> #   created_at <chr>, updated_at <chr>
# }
```
