# Retrieve media metadata from Nordic Microalgae

This function retrieves metadata for media items from the Nordic
Microalgae API. It returns detailed attributes such as title, caption,
location, sampling date, geographic coordinates, and contributor
information.

## Usage

``` r
get_nua_media_metadata(unparsed = FALSE)
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

- `photographer_artist`: The photographer or artist.

- `copyright_holder`: The copyright holder.

- `copyright_stamp`: The copyright stamp.

- `galleries`: Comma-separated list of galleries.

- `technique`: The imaging technique used.

- `contrast_enhancement`: The contrast enhancement method used.

- `preservation`: The preservation method used.

- `stain`: The stain used.

- `sampling_date`: The date the sample was collected.

- `geographic_area`: The geographic area of collection.

- `latitude_degree`: The latitude in degrees.

- `longitude_degree`: The longitude in degrees.

- `institute`: Comma-separated list of institutes.

- `contributing_organisation`: The contributing organisation.

- `created_at`: The creation timestamp.

- `updated_at`: The last update timestamp.

## See also

<https://nordicmicroalgae.org/> for Nordic Microalgae website.

<https://nordicmicroalgae.org/api/> for Nordic Microalgae API
documentation.

[`get_nua_media_links`](https://sharksmhi.github.io/SHARK4R/reference/get_nua_media_links.md)
for retrieving media image URLs.

## Examples

``` r
# \donttest{
# Retrieve media metadata
media_metadata <- get_nua_media_metadata(unparsed = FALSE)

# Preview the extracted data
head(media_metadata)
#> # A tibble: 6 × 26
#>   slug     taxon_slug scientific_name file  type  title caption license location
#>   <chr>    <chr>      <chr>           <chr> <chr> <chr> <chr>   <chr>   <chr>   
#> 1 odontel… odontella… Odontella sine… odon… imag… Odon… "O. si… Creati… Kosterf…
#> 2 odontel… odontella… Odontella auri… odon… imag… Odon… "Live … Creati… Havsten…
#> 3 octacti… octactis-… Octactis specu… octa… imag… Octa… "Two c… Creati… Danafjo…
#> 4 lennoxi… lennoxia-… Lennoxia faveo… lenn… imag… Lenn… ""      Creati… Danafjo…
#> 5 bolmen-… NA         NA              bolm… imag… Bolm… "Taken… Creati… Bolmen,…
#> 6 bolmen-… NA         NA              bolm… imag… Bolm… "Taken… Creati… Bolmen,…
#> # ℹ 17 more variables: contributor <chr>, photographer_artist <chr>,
#> #   copyright_holder <chr>, copyright_stamp <chr>, galleries <chr>,
#> #   technique <list>, contrast_enhancement <list>, preservation <list>,
#> #   stain <list>, sampling_date <chr>, geographic_area <chr>,
#> #   latitude_degree <chr>, longitude_degree <chr>, institute <chr>,
#> #   contributing_organisation <chr>, created_at <chr>, updated_at <chr>
# }
```
