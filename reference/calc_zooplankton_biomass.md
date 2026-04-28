# Calculate zooplankton biomass from dry weight and abundance

Calculates zooplankton biomass by combining per-individual dry weight
(`"Dry weight (mean)"`, in `ug`) with abundance measurements in the same
observation. Two biomass parameters are produced:

## Usage

``` r
calc_zooplankton_biomass(
  data,
  abundance_parameter = "Abundance",
  integrated_abundance_parameter = "Integrated abundance",
  dry_weight_parameter = "Dry weight (mean)",
  biomass_concentration_parameter = "Biomass concentration",
  integrated_biomass_parameter = "Integrated biomass",
  append = TRUE,
  drop_na_values = TRUE,
  keep_reference = FALSE
)
```

## Arguments

- data:

  A data frame or tibble in SHARK zooplankton format. Must contain the
  observation-key columns listed in the *Observation key* section, plus
  `parameter` and `value`.

- abundance_parameter:

  Character string giving the parameter name for abundance in `ind/m3`.
  Defaults to `"Abundance"`.

- integrated_abundance_parameter:

  Character string giving the parameter name for integrated abundance in
  `ind/m2`. Defaults to `"Integrated abundance"`.

- dry_weight_parameter:

  Character string giving the parameter name for per-individual dry
  weight in `ug`. Defaults to `"Dry weight (mean)"`.

- biomass_concentration_parameter:

  Character string used for calculated biomass concentration rows.
  Defaults to `"Biomass concentration"`.

- integrated_biomass_parameter:

  Character string used for calculated integrated biomass rows. Defaults
  to `"Integrated biomass"`.

- append:

  Logical. If `TRUE` (default), append calculated biomass rows to
  `data`. If `FALSE`, return only the calculated biomass rows.

- drop_na_values:

  Logical. If `TRUE` (default), drop calculated rows where biomass could
  not be calculated and `value` is `NA`.

- keep_reference:

  Logical. If `TRUE`, keep the dry-weight coefficient and reference
  columns used to compute biomass in the output.

## Value

A tibble. By default, the original data are returned with calculated
biomass rows appended. If `append = FALSE`, only the calculated rows are
returned.

## Details

- **Biomass concentration** (`mg/m3`) from `"Abundance"` rows
  (`ind/m3`).

- **Integrated biomass** (`mg/m2`) from `"Integrated abundance"` rows
  (`ind/m2`).

The conversion is `biomass = dry_weight_ug * abundance / 1000` so that
the result is expressed in `mg/m3` or `mg/m2`.

## Observation key

Dry-weight and abundance rows are matched one-to-one using the following
columns, which together identify a single zooplankton observation in
SHARK:

- `platform_code`

- `station_name`

- `sample_date`

- `sample_time`

- `sample_min_depth_m`

- `sample_max_depth_m`

- `aphia_id`

- `sex_code`

- `dev_stage_code`

- `size_class`

One biomass row is produced per matching abundance row (no aggregation
across size classes or development stages). Abundance rows without a
matching dry-weight value yield `NA` biomass and are dropped by default.

If no `"Dry weight (mean)"` rows are present in `data`, the function
calls
[`calc_zooplankton_dry_weight()`](https://sharksmhi.github.io/SHARK4R/reference/calc_zooplankton_dry_weight.md)
internally to compute them from `"Length (mean)"` before calculating
biomass.

## See also

[`calc_zooplankton_dry_weight()`](https://sharksmhi.github.io/SHARK4R/reference/calc_zooplankton_dry_weight.md)

## Examples

``` r
zoo <- dplyr::tibble(
  platform_code = "77SE",
  station_name = "ANHOLT E",
  sample_date = as.Date("2023-06-01"),
  sample_time = "10:00",
  sample_min_depth_m = 0,
  sample_max_depth_m = 30,
  aphia_id = 104251,
  sex_code = NA_character_,
  dev_stage_code = "AD",
  size_class = NA_character_,
  parameter = c("Length (mean)", "Abundance", "Integrated abundance"),
  value = c(800, 120, 3600),
  unit = c("um", "ind/m3", "ind/m2")
)

calc_zooplankton_biomass(zoo, append = FALSE)
#> # A tibble: 2 × 13
#>   platform_code station_name sample_date sample_time sample_min_depth_m
#>   <chr>         <chr>        <date>      <chr>                    <dbl>
#> 1 77SE          ANHOLT E     2023-06-01  10:00                        0
#> 2 77SE          ANHOLT E     2023-06-01  10:00                        0
#> # ℹ 8 more variables: sample_max_depth_m <dbl>, aphia_id <dbl>, sex_code <chr>,
#> #   dev_stage_code <chr>, size_class <chr>, parameter <chr>, value <dbl>,
#> #   unit <chr>
```
