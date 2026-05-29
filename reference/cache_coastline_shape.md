# Cache a high-resolution coastline shapefile

Internal helper: download (if not already cached) the OBIS or EEA
coastline polygon dataset to the package cache and return its on-disk
path. Used by both
[`positions_are_near_land()`](https://sharksmhi.github.io/SHARK4R/reference/positions_are_near_land.md)
and the EEA/OBIS basemap option of
[`create_pie_map()`](https://sharksmhi.github.io/SHARK4R/reference/create_pie_map.md).

## Usage

``` r
cache_coastline_shape(source, verbose = TRUE)
```

## Arguments

- source:

  One of `"obis"` or `"eea"`.

- verbose:

  Logical. If `TRUE` (default) print download progress.

## Value

Absolute path to the cached `.gpkg` file.
