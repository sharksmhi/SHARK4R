# Calculate zooplankton dry weight from mean length

Calculates zooplankton dry weight from rows where `parameter` equals
`"Length (mean)"` using taxa-specific coefficients from
`inst/extdata/Mesozooplankton_Kattegat_Skagerrak_taxa_and_biomass_calculations.xlsx`.

## Usage

``` r
calc_zooplankton_dry_weight(
  data,
  length_parameter = "Length (mean)",
  dry_weight_parameter = "Dry weight",
  append = TRUE,
  keep_reference = FALSE
)
```

## Arguments

- data:

  A data frame or tibble in SHARK zooplankton format. Must contain the
  columns `parameter`, `value`, `aphia_id`, and `dev_stage_code`.

- length_parameter:

  Character string giving the parameter name used for mean length.
  Defaults to `"Length (mean)"`.

- dry_weight_parameter:

  Character string used for the calculated dry weight rows. Defaults to
  `"Dry weight"`.

- append:

  Logical. If `TRUE` (default), append calculated dry-weight rows to
  `data`. If `FALSE`, return only the calculated dry-weight rows.

- keep_reference:

  Logical. If `TRUE`, keep additional columns with the selected
  coefficients and reference metadata in the output.

## Value

A tibble. By default, the original data are returned with calculated
`"Dry weight"` rows appended. If `append = FALSE`, only the calculated
rows are returned.

## Details

The dry weight calculation follows:

`DW = 10^((B * log10(length)) - A)`

For nauplii (`dev_stage_code == "NP"`), taxon-specific nauplii
coefficients are used when available. Otherwise, the general
coefficients for `"copepod nauplii *all copepod species"` are used. All
other development stages use the adult coefficients, defined in the
reference list as rows with missing `Stadium` (`NA` in the workbook).

Matching is performed using `aphia_id`, as preferred for SHARK data.
Taxa with no matching coefficient keep `NA` dry-weight values.

## Reference coefficient table

|                                       |         |         |      |      |                                              |
|---------------------------------------|---------|---------|------|------|----------------------------------------------|
| Reference taxon                       | AphiaID | Stadium | B    | A    | Reference                                    |
| Acartia bifilosa                      | 345919  | adult   | 2.96 | 7.71 | Hay 1991 (same as for A.clausi)              |
| Acartia clausi                        | 149755  | adult   | 2.96 | 7.71 | Hay 1991                                     |
| Acartia longiremis                    | 346037  | adult   | 2.96 | 7.71 | Hay 1991 (same as for A.clausi)              |
| Acartia                               | 104108  | adult   | 2.96 | 7.71 | Hay 1991 (same as for A.clausi)              |
| Calanus finmarchicus                  | 104464  | adult   | 2.69 | 6.88 | Hay 1991 (same as for C. helgolandicus)      |
| Calanus finmarchicus                  | 104464  | NP      | 2.03 | 5.38 | Hygum et al 2000                             |
| Centropages hamatus                   | 104496  | adult   | 2.45 | 6.09 | Hay 1991                                     |
| Centropages                           | 104159  | adult   | 2.45 | 6.10 | Hay 1991                                     |
| Centropages typicus                   | 104499  | adult   | 2.45 | 6.10 | Hay 1991                                     |
| Clausocalanus                         | 104161  | adult   | 3.35 | 8.90 | Hay et al 1988                               |
| Corycaeus                             | 128634  | adult   | 2.63 | 6.07 | Satapoomin 1999                              |
| Cyclopoida                            | 106415  | adult   | 2.71 | 6.72 | Uye 1982 (Oithona)                           |
| Evadne nordmanni                      | 106273  | adult   | 2.80 | 5.79 | Hernroth 1985                                |
| Fritillaria                           | 103358  | adult   | 2.66 | 4.51 | Paffenhofer 1976 (same as for Oikopleura)    |
| Harpacticoid copepod                  | 1102    | adult   | 2.89 | 7.24 | Uye (equation "for all copepods")            |
| copepod nauplii \*all copepod species | 1080    | NP      | 2.23 | 5.48 | Hay 1991 (same as for Pseudocalanus)         |
| Metridia                              | 104190  | adult   | 2.68 | 7.12 | Hirche & Mumm 1992                           |
| Microcalanus                          | 104164  | adult   | 2.91 | 7.86 | Hay 1991 (same as for Temora, se Koski 2012) |
| Microsetella                          | 115341  | adult   | 2.88 | 7.66 | Satapoomin 1999                              |
| Oikopleura dioica                     | 103407  | adult   | 2.66 | 4.51 | Paffenhofer 1976                             |
| Oithona                               | 106485  | NP      | 2.14 | 2.68 | Almeda et al 2010                            |
| Oithona similis                       | 106656  | adult   | 2.71 | 6.72 | Uye 1982                                     |
| Oncaea                                | 128690  | adult   | 2.63 | 6.28 | Satapoomin 1999                              |
| Paracalanus parvus                    | 104685  | adult   | 2.45 | 6.16 | Hay 1991                                     |
| Penilia avirostris                    | 106272  | adult   | 2.38 | 4.95 | Atienza et al 2006                           |
| Podon leukarti                        | 106277  | adult   | 3.02 | 7.52 | Uye 1982                                     |
| Podon polyphemoides                   | 159919  | adult   | 2.75 | 6.60 | Uye 1982                                     |
| Pseudocalanus                         | 104165  | adult   | 3.00 | 8.37 | Hay et al 1988                               |
| Temora longicornis                    | 104878  | adult   | 3.00 | 8.37 | Hay et al 1988                               |

## Examples

``` r
zoo <- dplyr::tibble(
  scientific_name = c("Acartia clausi", "Calanus finmarchicus", "Unknown taxon"),
  parameter = c("Length (mean)", "Length (mean)", "Length (mean)"),
  value = c(200, 250, 160),
  aphia_id = c(149755, 104464, 999999),
  dev_stage_code = c("AD", "NP", "NP")
)

calc_zooplankton_dry_weight(zoo, append = FALSE)
#> # A tibble: 3 × 5
#>   scientific_name      parameter  value aphia_id dev_stage_code
#>   <chr>                <chr>      <dbl>    <dbl> <chr>         
#> 1 Acartia clausi       Dry weight 0.129   149755 AD            
#> 2 Calanus finmarchicus Dry weight 0.307   104464 NP            
#> 3 Unknown taxon        Dry weight 0.272   999999 NP            
```
