# Load SHARK4R statistics from GitHub

This function downloads and loads precomputed SHARK4R statistical data
(e.g., threshold or summary statistics) directly from the
[SHARK4R-statistics](https://github.com/nodc-sweden/SHARK4R-statistics)
GitHub repository. The data are stored as `.rds` files and read into R
as objects.

## Usage

``` r
load_shark4r_stats(file_name = "sea_basin.rds", verbose = TRUE)
```

## Arguments

- file_name:

  Character string specifying the name of the `.rds` file to download.
  Defaults to `"sea_basin.rds"`.

- verbose:

  Logical; if `TRUE` (default), prints progress messages during download
  and loading.

## Value

An R object (typically a `tibble` or `data.frame`) read from the
specified `.rds` file.

## Details

The function retrieves the file from the GitHub repository’s `data/`
folder. It temporarily downloads the file to the local system and then
reads it into R using
[`readRDS()`](https://rdrr.io/r/base/readRDS.html).

If the download fails (e.g., due to a network issue or invalid
filename), the function throws an error with a descriptive message.

## See also

[`check_outliers`](https://sharksmhi.github.io/SHARK4R/reference/check_outliers.md)
for detecting threshold exceedances using the loaded statistics,
[`get_shark_statistics`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_statistics.md)
for generating and caching statistical summaries used in SHARK4R.
[`scatterplot`](https://sharksmhi.github.io/SHARK4R/reference/scatterplot.md)
for generating interactive plots with threshold values.

## Examples

``` r
# \donttest{
# Load the default SHARK4R statistics file
stats <- load_shark4r_stats(verbose = FALSE)
print(stats)
#> # A tibble: 746 × 25
#>    parameter datatype      location_sea_basin fromYear toYear     n   min     Q1
#>    <chr>     <chr>         <chr>                 <dbl>  <dbl> <int> <dbl>  <dbl>
#>  1 # counted Bacterioplan… 1 - Bottenviken        2023   2023    47   229 1110. 
#>  2 # counted Bacterioplan… 3 - Bottenhavet        2023   2023    63   352 1156  
#>  3 # counted Grey seal     2 - Norra Kvarken      2020   2020     4     4  240. 
#>  4 # counted Grey seal     3 - Bottenhavet        2020   2020    19     1   34.5
#>  5 # counted Grey seal     7 - Norra Gotland…     2020   2020    21     2   12  
#>  6 # counted Grey seal     8 - Västra Gotlan…     2020   2020    51     1   18  
#>  7 # counted Grey seal     9 - Östra Gotland…     2020   2020    17     1   29  
#>  8 # counted Grey seal     NA                     2020   2020   100     1   45.8
#>  9 # counted Harbour seal  16 - Kattegatt         2020   2020    93     1   23  
#> 10 # counted Harbour seal  17 - Skagerrak         2020   2020   125     1   15  
#> # ℹ 736 more rows
#> # ℹ 17 more variables: median <dbl>, Q3 <dbl>, max <dbl>, P01 <dbl>, P05 <dbl>,
#> #   P95 <dbl>, P99 <dbl>, IQR <dbl>, mean <dbl>, sd <dbl>, var <dbl>, cv <dbl>,
#> #   mad <dbl>, mild_lower <dbl>, mild_upper <dbl>, extreme_lower <dbl>,
#> #   extreme_upper <dbl>

# Load a specific file
thresholds <- load_shark4r_stats("scientific_name.rds", verbose = FALSE)
print(thresholds)
#> # A tibble: 6,209 × 25
#>    parameter datatype  scientific_name fromYear toYear     n   min     Q1 median
#>    <chr>     <chr>     <chr>              <dbl>  <dbl> <int> <dbl>  <dbl>  <dbl>
#>  1 # counted Bacterio… Bacteria            2023   2023   110   229 1152.  1328. 
#>  2 # counted Grey seal Halichoerus gr…     2020   2020   214     1   27.2   88  
#>  3 # counted Harbour … Phoca vitulina      2020   2020   517     1   17     37  
#>  4 # counted Phytopla… Acanthoceras z…     2021   2023    24     1    1      3.5
#>  5 # counted Phytopla… Acanthoica qua…     2020   2024    30     1    1      1  
#>  6 # counted Phytopla… Acanthostomell…     2021   2024     4     1    1      1  
#>  7 # counted Phytopla… Achnanthes          2021   2024    32     1    3      6  
#>  8 # counted Phytopla… Actinocyclus        2020   2024   602     1    1      2  
#>  9 # counted Phytopla… Actinocyclus o…     2020   2024   148     1    1      1  
#> 10 # counted Phytopla… Actinocyclus o…     2020   2021    22     1    1      1  
#> # ℹ 6,199 more rows
#> # ℹ 16 more variables: Q3 <dbl>, max <dbl>, P01 <dbl>, P05 <dbl>, P95 <dbl>,
#> #   P99 <dbl>, IQR <dbl>, mean <dbl>, sd <dbl>, var <dbl>, cv <dbl>, mad <dbl>,
#> #   mild_lower <dbl>, mild_upper <dbl>, extreme_lower <dbl>,
#> #   extreme_upper <dbl>
# }
```
