# Displace pie chart centres away from each other

Sequential, asymmetric placement so that pie charts never overlap and
any pie that *does* get displaced is pushed far enough that its anchor
(true station location) lands outside the pie boundary, so the leader
line and anchor dot remain visible. Geometry is computed in an isotropic
coordinate system \\(lon \cdot \cos(\bar{lat}), lat)\\ so that visual
distance on a `coord_sf` map corresponds approximately to Euclidean
distance here.

## Usage

``` r
repel_pie_centers(
  wide,
  map_xlim = c(10.5, 21.5),
  map_ylim = c(54.2, 59.8),
  min_sep = 2.4,
  min_disp = 1.6
)
```

## Arguments

- wide:

  Data frame with columns `lon`, `lat` and `r_pie` (the per-station pie
  radius in latitude degrees). True (anchor) coordinates are read from
  `lon`/`lat`.

- map_xlim, map_ylim:

  Numeric length-2 vectors bounding the pie centre.

- min_sep:

  Minimum centre-to-centre separation between two pies, expressed as a
  multiple of the larger of the two radii. Default `2.40`.

- min_disp:

  Minimum displacement for a pie that is moved at all, expressed as a
  multiple of its radius. Default `1.60`.

## Value

`wide` with added columns `anchor_lon`, `anchor_lat` (the true station
coordinates) and updated `lon`/`lat` holding the displaced pie centres.
