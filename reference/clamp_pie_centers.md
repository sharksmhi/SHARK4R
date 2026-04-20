# Clamp pie chart centres to lie inside the map panel

Clamp pie chart centres to lie inside the map panel

## Usage

``` r
clamp_pie_centers(wide, map_xlim = c(10.5, 21.5), map_ylim = c(54.2, 59.8))
```

## Arguments

- wide:

  Data frame with columns `lon`, `lat` and optional `r_pie`.

- map_xlim, map_ylim:

  Numeric length-2 vectors bounding the pie centre.

## Value

`wide` with clamped `lon`/`lat`.
