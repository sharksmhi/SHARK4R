# Find label positions that avoid all pie charts and each other

Uses a greedy sequential algorithm: stations are processed
most-constrained first, and each label is placed in the direction that
maximises clearance from all pie circles *and* from already-placed label
bounding boxes.

## Usage

``` r
place_pie_labels(
  wide,
  map_xlim = c(10.8, 21.2),
  map_ylim = c(54.4, 59.6),
  n_angles = 48,
  char_w = 0.055,
  char_h = 0.055,
  radius_mults = c(1.08, 1.25, 1.55, 1.9, 2.4, 3, 3.8, 4.8)
)
```

## Arguments

- wide:

  Data frame with one row per station, columns `lon`, `lat`, `r_pie`,
  `r_lon`, and `label`.

- map_xlim, map_ylim:

  Numeric length-2 vectors giving the allowed label anchor range.

- n_angles:

  Number of candidate directions to test per station.

- char_w:

  Estimated label width per character in longitude degrees.

- char_h:

  Estimated label half-height in latitude degrees.

- radius_mults:

  Candidate radial distances in units of pie radius.

## Value

`wide` with columns `label_x`, `label_y`, `hjust`, `vjust` appended.
