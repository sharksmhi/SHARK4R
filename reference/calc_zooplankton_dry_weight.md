# Calculate zooplankton dry weight from mean length

Calculates zooplankton dry weight from rows where `parameter` equals
`"Length (mean)"` using bundled taxa-specific coefficients for
mesozooplankton from Kattegat and Skagerrak.

## Usage

``` r
calc_zooplankton_dry_weight(
  data,
  length_parameter = "Length (mean)",
  dry_weight_parameter = "Dry weight (mean)",
  append = TRUE,
  drop_na_values = TRUE,
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
  `"Dry weight (mean)"`.

- append:

  Logical. If `TRUE` (default), append calculated dry-weight rows to
  `data`. If `FALSE`, return only the calculated dry-weight rows.

- drop_na_values:

  Logical. If `TRUE` (default), drop calculated rows where dry weight
  could not be calculated and `value` is `NA`.

- keep_reference:

  Logical. If `TRUE`, keep additional columns with the selected
  coefficients and reference metadata in the output.

## Value

A tibble. By default, the original data are returned with calculated
`"Dry weight (mean)"` rows appended. If `append = FALSE`, only the
calculated rows are returned.

## Details

The dry weight calculation follows:

`DW = 10^((B * log10(length)) - A)`

For nauplii (`dev_stage_code == "NP"`), taxon-specific nauplii
coefficients are used when available. Otherwise, the general
coefficients for `"copepod nauplii *all copepod species"` are used. All
other development stages use the non-nauplii coefficients. In practice,
these coefficients are used for adults as well as copepodite stages and
other non-`NP` stages.

The calculation assumes that SHARK `"Length (mean)"` values are reported
in `um`. Calculated dry-weight rows are assigned the unit `ug`.

The bundled coefficient workbook used by this function can be accessed
with:
`system.file("extdata", "Mesozooplankton_Kattegat_Skagerrak_taxa_and_biomass_calculations.xlsx", package = "SHARK4R")`.

Matching is performed using `aphia_id`, as preferred for SHARK data.
Taxa with no matching coefficient keep `NA` dry-weight values.

## Reference coefficient table

|                                       |             |                           |       |       |                       |
|---------------------------------------|-------------|---------------------------|-------|-------|-----------------------|
| **Reference taxon**                   | **AphiaID** | **Development stage**     | **A** | **B** | **Reference**         |
| Acartia bifilosa                      | 345919      | all stages except nauplii | 7.71  | 2.96  | Hay 1991¹             |
| Acartia clausi                        | 149755      | all stages except nauplii | 7.71  | 2.96  | Hay 1991¹             |
| Acartia longiremis                    | 346037      | all stages except nauplii | 7.71  | 2.96  | Hay 1991¹             |
| Acartia                               | 104108      | all stages except nauplii | 7.71  | 2.96  | Hay 1991¹             |
| Calanus finmarchicus                  | 104464      | all stages except nauplii | 6.88  | 2.69  | Hay 1991¹             |
| Calanus finmarchicus                  | 104464      | nauplii                   | 5.38  | 2.03  | Hygum et al. 2000²    |
| Centropages hamatus                   | 104496      | all stages except nauplii | 6.09  | 2.45  | Hay 1991¹             |
| Centropages                           | 104159      | all stages except nauplii | 6.10  | 2.45  | Hay 1991¹             |
| Centropages typicus                   | 104499      | all stages except nauplii | 6.10  | 2.45  | Hay 1991¹             |
| Clausocalanus                         | 104161      | all stages except nauplii | 8.90  | 3.35  | Hay et al. 1988³      |
| Corycaeus                             | 128634      | all stages except nauplii | 6.07  | 2.63  | Satapoomin 1999⁴      |
| Cyclopoida                            | 106415      | all stages except nauplii | 6.72  | 2.71  | Uye 1982⁵             |
| Evadne nordmanni                      | 106273      | all stages except nauplii | 5.79  | 2.80  | Hernroth 1985⁶        |
| Fritillaria                           | 103358      | all stages except nauplii | 4.51  | 2.66  | Paffenhofer 1976¹⁰    |
| Harpacticoid copepod                  | 1102        | all stages except nauplii | 7.24  | 2.89  | Uye 1982⁵             |
| copepod nauplii \*all copepod species | 1080        | nauplii                   | 5.48  | 2.23  | Hay 1991¹             |
| Metridia                              | 104190      | all stages except nauplii | 7.12  | 2.68  | Hirche and Mumm 1992⁹ |
| Microcalanus                          | 104164      | all stages except nauplii | 7.86  | 2.91  | Hay 1991¹             |
| Microsetella                          | 115341      | all stages except nauplii | 7.66  | 2.88  | Satapoomin 1999⁴      |
| Oikopleura dioica                     | 103407      | all stages except nauplii | 4.51  | 2.66  | Paffenhofer 1976¹⁰    |
| Oithona                               | 106485      | nauplii                   | 2.68  | 2.14  | Almeda et al. 2010⁸   |
| Oithona similis                       | 106656      | all stages except nauplii | 6.72  | 2.71  | Uye 1982⁵             |
| Oncaea                                | 128690      | all stages except nauplii | 6.28  | 2.63  | Satapoomin 1999⁴      |
| Paracalanus parvus                    | 104685      | all stages except nauplii | 6.16  | 2.45  | Hay 1991¹             |
| Penilia avirostris                    | 106272      | all stages except nauplii | 4.95  | 2.38  | Atienza et al. 2006⁷  |
| Podon leukarti                        | 106277      | all stages except nauplii | 7.52  | 3.02  | Uye 1982⁵             |
| Podon polyphemoides                   | 159919      | all stages except nauplii | 6.60  | 2.75  | Uye 1982⁵             |
| Pseudocalanus                         | 104165      | all stages except nauplii | 8.37  | 3.00  | Hay et al. 1988³      |
| Temora longicornis                    | 104878      | all stages except nauplii | 8.37  | 3.00  | Hay et al. 1988³      |

## References

1.  Hay SJ, Kiørboe T, Matthews A (1991) Zooplankton biomass and
    production in the North Sea during the Autumn Circulation
    experiment, October 1987-March 1988. *Continental Shelf Research*
    11(12):1453-1476. <https://doi.org/10.1016/0278-4343(91)90021-W>

2.  Hygum BH, Rey C, Hansen BW (2000) Growth and development rates of
    *Calanus finmarchicus* nauplii during a diatom spring bloom. *Marine
    Biology* 136:1075-1085. <https://doi.org/10.1007/s002270000313>

3.  Hay SJ, Evans GT, Gamble JC (1988) Birth, growth and death rates for
    enclosed populations of calanoid copepods. *Journal of Plankton
    Research* 10(3):431-454. <https://doi.org/10.1093/plankt/10.3.431>

4.  Satapoomin S (1999) Carbon content of some common tropical Andaman
    Sea copepods. *Journal of Plankton Research* 21(11):2117-2123.
    <https://doi.org/10.1093/plankt/21.11.2117>

5.  Uye SI (1982) Length-weight relationships of important zooplankton
    from the Inland Sea of Japan. *Journal of the Oceanographical
    Society of Japan* 38:149-158. <https://doi.org/10.1007/BF02110286>

6.  Hernroth L, ed. (1985) *Recommendations on methods for marine
    biological studies in the Baltic Sea: mesozooplankton biomass
    assessment / individual volume technique*. Publication / The Baltic
    Marine Biologists - BMB, 10. Lysekil: Institute of Marine Research.

7.  Atienza D, Saiz E, Calbet A (2006) Feeding ecology of the marine
    cladoceran *Penilia avirostris*: natural diet, prey selectivity and
    daily ration. *Marine Ecology Progress Series* 315:211-220.
    <https://doi.org/10.3354/meps315211>

8.  Almeda R, Calbet A, Alcaraz M, Yebra L, Saiz E (2010) Effects of
    temperature and food concentration on the survival, development and
    growth rates of naupliar stages of *Oithona davisae* (Copepoda,
    Cyclopoida). *Marine Ecology Progress Series* 410:97-109.
    <https://doi.org/10.3354/meps08625>

9.  Hirche HJ, Mumm N (1992) Distribution of dominant copepods in the
    Nansen Basin, Arctic Ocean, in summer. *Deep-Sea Research Part A.
    Oceanographic Research Papers* 39(Suppl. 2):S485-S505.
    <https://doi.org/10.1016/S0198-0149(06)80017-8>

10. Paffenhöfer GA (1976) On the biology of appendicularia of the
    southeastern North Sea. In: 10th European Symposium on Marine
    Biology, Ostend, Belgium, 17-23 September 1975, Vol. 2, pp. 437-455.

## Examples

``` r
# Minimal example with a few rows
zoo <- dplyr::tibble(
  scientific_name = c("Acartia clausi", "Calanus finmarchicus", "Unknown taxon"),
  parameter = c("Length (mean)", "Length (mean)", "Length (mean)"),
  value = c(200, 250, 160),
  aphia_id = c(104251, 104464, 999999),
  dev_stage_code = c("AD", "NP", "NP")
)

# Calculate dry weight rows only
calc_zooplankton_dry_weight(zoo, append = FALSE)
#> # A tibble: 3 × 5
#>   scientific_name      parameter         value aphia_id dev_stage_code
#>   <chr>                <chr>             <dbl>    <dbl> <chr>         
#> 1 Acartia clausi       Dry weight (mean) 0.129   104251 AD            
#> 2 Calanus finmarchicus Dry weight (mean) 0.307   104464 NP            
#> 3 Unknown taxon        Dry weight (mean) 0.272   999999 NP            

# \donttest{
# Download zooplankton data from SHARK
zoo_shark <- get_shark_data(
  "sharkdata_zooplankton",
  dataTypes = "Zooplankton",
  fromYear = 2023,
  toYear = 2023,
  stationName = "ANHOLT E",
  verbose = FALSE,
)

# Calculate dry weight from "Length (mean)" and return only the new rows
calc_zooplankton_dry_weight(
  zoo_shark,
  append = FALSE
)
#> # A tibble: 463 × 109
#>    delivery_datatype check_status_sv data_checked_by_sv visit_year visit_month
#>    <chr>             <chr>           <chr>                   <dbl>       <dbl>
#>  1 Zooplankton       Klar            Leverantör               2023          12
#>  2 Zooplankton       Klar            Leverantör               2023          12
#>  3 Zooplankton       Klar            Leverantör               2023          12
#>  4 Zooplankton       Klar            Leverantör               2023          12
#>  5 Zooplankton       Klar            Leverantör               2023          12
#>  6 Zooplankton       Klar            Leverantör               2023          12
#>  7 Zooplankton       Klar            Leverantör               2023          12
#>  8 Zooplankton       Klar            Leverantör               2023          12
#>  9 Zooplankton       Klar            Leverantör               2023          12
#> 10 Zooplankton       Klar            Leverantör               2023          12
#> # ℹ 453 more rows
#> # ℹ 104 more variables: station_name <chr>, reported_station_name <chr>,
#> #   sample_location_id <dbl>, station_id <dbl>, sample_project_name_sv <chr>,
#> #   sample_orderer_name_sv <chr>, platform_code <chr>, visit_id <dbl>,
#> #   expedition_id <dbl>, shark_sample_id_md5 <chr>, sample_date <date>,
#> #   sample_time <time>, sample_latitude_dm <chr>, sample_longitude_dm <chr>,
#> #   sample_latitude_dd <dbl>, sample_longitude_dd <dbl>, …
# }
```
