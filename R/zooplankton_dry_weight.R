#' Calculate zooplankton dry weight from mean length
#'
#' Calculates zooplankton dry weight from rows where `parameter` equals
#' `"Length (mean)"` using bundled taxa-specific coefficients for
#' mesozooplankton from Kattegat and Skagerrak.
#'
#' The dry weight calculation follows:
#'
#' `DW = 10^((B * log10(length)) - A)`
#'
#' For nauplii (`dev_stage_code == "NP"`), taxon-specific nauplii coefficients are
#' used when available. Otherwise, the general coefficients for
#' `"copepod nauplii *all copepod species"` are used. All other development
#' stages use the non-nauplii coefficients. In practice, these coefficients are
#' used for adults as well as copepodite stages and other non-`NP` stages.
#'
#' The calculation assumes that SHARK `"Length (mean)"` values are reported in
#' `um`. Calculated dry-weight rows are assigned the unit `ug`.
#'
#' The bundled coefficient workbook used by this function can be accessed with:
#' `system.file("extdata",
#' "Mesozooplankton_Kattegat_Skagerrak_taxa_and_biomass_calculations.xlsx",
#' package = "SHARK4R")`.
#'
#' Matching is performed using `aphia_id`, as preferred for SHARK data. Taxa with
#' no matching coefficient keep `NA` dry-weight values.
#'
#' @param data A data frame or tibble in SHARK zooplankton format. Must contain
#'   the columns `parameter`, `value`, `aphia_id`, and `dev_stage_code`.
#' @param length_parameter Character string giving the parameter name used for
#'   mean length. Defaults to `"Length (mean)"`.
#' @param dry_weight_parameter Character string used for the calculated dry
#'   weight rows. Defaults to `"Dry weight (mean)"`.
#' @param append Logical. If `TRUE` (default), append calculated dry-weight rows
#'   to `data`. If `FALSE`, return only the calculated dry-weight rows.
#' @param drop_na_values Logical. If `TRUE` (default), drop calculated rows where
#'   dry weight could not be calculated and `value` is `NA`.
#' @param keep_reference Logical. If `TRUE`, keep additional columns with the
#'   selected coefficients and reference metadata in the output.
#'
#' @return A tibble. By default, the original data are returned with calculated
#'   `"Dry weight (mean)"` rows appended. If `append = FALSE`, only the calculated rows
#'   are returned.
#'
#' @section Reference coefficient table:
#' \tabular{llllll}{
#' \strong{Reference taxon} \tab \strong{AphiaID} \tab \strong{Development stage} \tab \strong{A} \tab \strong{B} \tab \strong{Reference}\cr
#' Acartia bifilosa \tab 345919 \tab all stages except nauplii \tab 7.71 \tab 2.96 \tab Hay 1991¹\cr
#' Acartia clausi \tab 149755 \tab all stages except nauplii \tab 7.71 \tab 2.96 \tab Hay 1991¹\cr
#' Acartia longiremis \tab 346037 \tab all stages except nauplii \tab 7.71 \tab 2.96 \tab Hay 1991¹\cr
#' Acartia \tab 104108 \tab all stages except nauplii \tab 7.71 \tab 2.96 \tab Hay 1991¹\cr
#' Calanus finmarchicus \tab 104464 \tab all stages except nauplii \tab 6.88 \tab 2.69 \tab Hay 1991¹\cr
#' Calanus finmarchicus \tab 104464 \tab nauplii \tab 5.38 \tab 2.03 \tab Hygum et al. 2000²\cr
#' Centropages hamatus \tab 104496 \tab all stages except nauplii \tab 6.09 \tab 2.45 \tab Hay 1991¹\cr
#' Centropages \tab 104159 \tab all stages except nauplii \tab 6.10 \tab 2.45 \tab Hay 1991¹\cr
#' Centropages typicus \tab 104499 \tab all stages except nauplii \tab 6.10 \tab 2.45 \tab Hay 1991¹\cr
#' Clausocalanus \tab 104161 \tab all stages except nauplii \tab 8.90 \tab 3.35 \tab Hay et al. 1988³\cr
#' Corycaeus \tab 128634 \tab all stages except nauplii \tab 6.07 \tab 2.63 \tab Satapoomin 1999⁴\cr
#' Cyclopoida \tab 106415 \tab all stages except nauplii \tab 6.72 \tab 2.71 \tab Uye 1982⁵\cr
#' Evadne nordmanni \tab 106273 \tab all stages except nauplii \tab 5.79 \tab 2.80 \tab Hernroth 1985⁶\cr
#' Fritillaria \tab 103358 \tab all stages except nauplii \tab 4.51 \tab 2.66 \tab Paffenhofer 1976¹⁰\cr
#' Harpacticoid copepod \tab 1102 \tab all stages except nauplii \tab 7.24 \tab 2.89 \tab Uye 1982⁵\cr
#' copepod nauplii *all copepod species \tab 1080 \tab nauplii \tab 5.48 \tab 2.23 \tab Hay 1991¹\cr
#' Metridia \tab 104190 \tab all stages except nauplii \tab 7.12 \tab 2.68 \tab Hirche and Mumm 1992⁹\cr
#' Microcalanus \tab 104164 \tab all stages except nauplii \tab 7.86 \tab 2.91 \tab Hay 1991¹\cr
#' Microsetella \tab 115341 \tab all stages except nauplii \tab 7.66 \tab 2.88 \tab Satapoomin 1999⁴\cr
#' Oikopleura dioica \tab 103407 \tab all stages except nauplii \tab 4.51 \tab 2.66 \tab Paffenhofer 1976¹⁰\cr
#' Oithona \tab 106485 \tab nauplii \tab 2.68 \tab 2.14 \tab Almeda et al. 2010⁸\cr
#' Oithona similis \tab 106656 \tab all stages except nauplii \tab 6.72 \tab 2.71 \tab Uye 1982⁵\cr
#' Oncaea \tab 128690 \tab all stages except nauplii \tab 6.28 \tab 2.63 \tab Satapoomin 1999⁴\cr
#' Paracalanus parvus \tab 104685 \tab all stages except nauplii \tab 6.16 \tab 2.45 \tab Hay 1991¹\cr
#' Penilia avirostris \tab 106272 \tab all stages except nauplii \tab 4.95 \tab 2.38 \tab Atienza et al. 2006⁷\cr
#' Podon leukarti \tab 106277 \tab all stages except nauplii \tab 7.52 \tab 3.02 \tab Uye 1982⁵\cr
#' Podon polyphemoides \tab 159919 \tab all stages except nauplii \tab 6.60 \tab 2.75 \tab Uye 1982⁵\cr
#' Pseudocalanus \tab 104165 \tab all stages except nauplii \tab 8.37 \tab 3.00 \tab Hay et al. 1988³\cr
#' Temora longicornis \tab 104878 \tab all stages except nauplii \tab 8.37 \tab 3.00 \tab Hay et al. 1988³\cr
#' }
#'
#' @references
#' 1. Hay SJ, Kiørboe T, Matthews A (1991) Zooplankton biomass and production
#' in the North Sea during the Autumn Circulation experiment, October
#' 1987-March 1988. *Continental Shelf Research* 11(12):1453-1476.
#' <https://doi.org/10.1016/0278-4343(91)90021-W>
#'
#' 2. Hygum BH, Rey C, Hansen BW (2000) Growth and development rates of
#' *Calanus finmarchicus* nauplii during a diatom spring bloom. *Marine
#' Biology* 136:1075-1085. <https://doi.org/10.1007/s002270000313>
#'
#' 3. Hay SJ, Evans GT, Gamble JC (1988) Birth, growth and death rates for
#' enclosed populations of calanoid copepods. *Journal of Plankton Research*
#' 10(3):431-454. <https://doi.org/10.1093/plankt/10.3.431>
#'
#' 4. Satapoomin S (1999) Carbon content of some common tropical Andaman Sea
#' copepods. *Journal of Plankton Research* 21(11):2117-2123.
#' <https://doi.org/10.1093/plankt/21.11.2117>
#'
#' 5. Uye SI (1982) Length-weight relationships of important zooplankton from
#' the Inland Sea of Japan. *Journal of the Oceanographical Society of Japan*
#' 38:149-158. <https://doi.org/10.1007/BF02110286>
#'
#' 6. Hernroth L, ed. (1985) *Recommendations on methods for marine biological
#' studies in the Baltic Sea: mesozooplankton biomass assessment / individual
#' volume technique*. Publication / The Baltic Marine Biologists - BMB, 10.
#' Lysekil: Institute of Marine Research.
#'
#' 7. Atienza D, Saiz E, Calbet A (2006) Feeding ecology of the marine
#' cladoceran *Penilia avirostris*: natural diet, prey selectivity and daily
#' ration. *Marine Ecology Progress Series* 315:211-220.
#' <https://doi.org/10.3354/meps315211>
#'
#' 8. Almeda R, Calbet A, Alcaraz M, Yebra L, Saiz E (2010) Effects of
#' temperature and food concentration on the survival, development and growth
#' rates of naupliar stages of *Oithona davisae* (Copepoda, Cyclopoida).
#' *Marine Ecology Progress Series* 410:97-109.
#' <https://doi.org/10.3354/meps08625>
#'
#' 9. Hirche HJ, Mumm N (1992) Distribution of dominant copepods in the Nansen
#' Basin, Arctic Ocean, in summer. *Deep-Sea Research Part A. Oceanographic
#' Research Papers* 39(Suppl. 2):S485-S505.
#' <https://doi.org/10.1016/S0198-0149(06)80017-8>
#'
#' 10. Paffenhöfer GA (1976) On the biology of appendicularia of the
#' southeastern North Sea. In: 10th European Symposium on Marine Biology,
#' Ostend, Belgium, 17-23 September 1975, Vol. 2, pp. 437-455.
#'
#' @examples
#' # Minimal example with a few rows
#' zoo <- dplyr::tibble(
#'   scientific_name = c("Acartia clausi", "Calanus finmarchicus", "Unknown taxon"),
#'   parameter = c("Length (mean)", "Length (mean)", "Length (mean)"),
#'   value = c(200, 250, 160),
#'   aphia_id = c(104251, 104464, 999999),
#'   dev_stage_code = c("AD", "NP", "NP")
#' )
#'
#' # Calculate dry weight rows only
#' calc_zooplankton_dry_weight(zoo, append = FALSE)
#'
#' \donttest{
#' # Download zooplankton data from SHARK
#' zoo_shark <- get_shark_data(
#'   "sharkdata_zooplankton",
#'   dataTypes = "Zooplankton",
#'   fromYear = 2023,
#'   toYear = 2023,
#'   stationName = "ANHOLT E",
#'   verbose = FALSE,
#' )
#'
#' # Calculate dry weight from "Length (mean)" and return only the new rows
#' calc_zooplankton_dry_weight(
#'   zoo_shark,
#'   append = FALSE
#' )
#' }
#'
#' @importFrom rlang .data
#' @export
calc_zooplankton_dry_weight <- function(data,
                                        length_parameter = "Length (mean)",
                                        dry_weight_parameter = "Dry weight (mean)",
                                        append = TRUE,
                                        drop_na_values = TRUE,
                                        keep_reference = FALSE) {
  required_cols <- c("parameter", "value", "aphia_id", "dev_stage_code")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  coeffs <- load_zooplankton_dry_weight_coefficients()

  adult_coeffs <- coeffs %>%
    dplyr::filter(is.na(.data$Stadium)) %>%
    dplyr::transmute(
      aphia_id = .data$AphiaID,
      dw_B_adult = .data$B,
      dw_A_adult = .data$A,
      dw_ref_adult = .data$Ref,
      dw_taxon_adult = .data$reference_taxon
    )

  nauplii_coeffs <- coeffs %>%
    dplyr::filter(.data$Stadium == "NP") %>%
    dplyr::transmute(
      aphia_id = .data$AphiaID,
      dw_B_np = .data$B,
      dw_A_np = .data$A,
      dw_ref_np = .data$Ref,
      dw_taxon_np = .data$reference_taxon
    )

  general_nauplii <- nauplii_coeffs %>%
    dplyr::filter(.data$aphia_id == 1080) %>%
    dplyr::slice(1)

  if (nrow(general_nauplii) != 1) {
    stop("General copepod nauplii coefficients could not be loaded.", call. = FALSE)
  }

  dry_weight_rows <- data %>%
    dplyr::filter(.data$parameter == length_parameter) %>%
    dplyr::mutate(
      value = suppressWarnings(as.numeric(.data$value)),
      aphia_id = suppressWarnings(as.numeric(.data$aphia_id))
    ) %>%
    dplyr::left_join(adult_coeffs, by = "aphia_id") %>%
    dplyr::left_join(
      dplyr::filter(nauplii_coeffs, .data$aphia_id != 1080),
      by = "aphia_id"
    ) %>%
    dplyr::mutate(
      dw_match_type = dplyr::case_when(
        .data$dev_stage_code == "NP" & !is.na(.data$dw_B_np) ~ "nauplii_taxon_specific",
        .data$dev_stage_code == "NP" ~ "nauplii_general",
        !is.na(.data$dw_B_adult) ~ "adult",
        TRUE ~ NA_character_
      ),
      dry_weight_coeff_b = dplyr::case_when(
        .data$dev_stage_code == "NP" & !is.na(.data$dw_B_np) ~ .data$dw_B_np,
        .data$dev_stage_code == "NP" ~ general_nauplii$dw_B_np,
        TRUE ~ .data$dw_B_adult
      ),
      dry_weight_coeff_a = dplyr::case_when(
        .data$dev_stage_code == "NP" & !is.na(.data$dw_A_np) ~ .data$dw_A_np,
        .data$dev_stage_code == "NP" ~ general_nauplii$dw_A_np,
        TRUE ~ .data$dw_A_adult
      ),
      dry_weight_reference = dplyr::case_when(
        .data$dev_stage_code == "NP" & !is.na(.data$dw_ref_np) ~ .data$dw_ref_np,
        .data$dev_stage_code == "NP" ~ general_nauplii$dw_ref_np,
        TRUE ~ .data$dw_ref_adult
      ),
      dry_weight_reference_taxon = dplyr::case_when(
        .data$dev_stage_code == "NP" & !is.na(.data$dw_taxon_np) ~ .data$dw_taxon_np,
        .data$dev_stage_code == "NP" ~ general_nauplii$dw_taxon_np,
        TRUE ~ .data$dw_taxon_adult
      ),
      value = dplyr::case_when(
        !is.na(.data$dry_weight_coeff_a) &
          !is.na(.data$dry_weight_coeff_b) &
          !is.na(.data$value) &
          .data$value > 0 ~ 10^((.data$dry_weight_coeff_b * log10(.data$value)) - .data$dry_weight_coeff_a),
        TRUE ~ NA_real_
      ),
      parameter = dry_weight_parameter
    )

  if ("unit" %in% names(dry_weight_rows)) {
    dry_weight_rows <- dry_weight_rows %>%
      dplyr::mutate(unit = "ug")
  }

  if ("calculation_method" %in% names(dry_weight_rows)) {
    dry_weight_rows <- dry_weight_rows %>%
      dplyr::mutate(
        calculation_method = paste0(
          "Calculated from ",
          length_parameter,
          " using Log DW = (B x Log(length)) - A with AphiaID-based coefficients; assumes length in um and returns dry weight in ug"
        )
      )
  }

  if ("reported_value" %in% names(dry_weight_rows)) {
    dry_weight_rows <- dry_weight_rows %>%
      dplyr::mutate(reported_value = NA)
  }

  if (isTRUE(drop_na_values)) {
    dry_weight_rows <- dry_weight_rows %>%
      dplyr::filter(!is.na(.data$value))
  }

  helper_cols <- c(
    "dw_B_adult", "dw_A_adult", "dw_ref_adult", "dw_taxon_adult",
    "dw_B_np", "dw_A_np", "dw_ref_np", "dw_taxon_np"
  )

  if (!keep_reference) {
    dry_weight_rows <- dry_weight_rows %>%
      dplyr::select(-dplyr::any_of(c(
        helper_cols,
        "dry_weight_coeff_a",
        "dry_weight_coeff_b",
        "dry_weight_reference",
        "dry_weight_reference_taxon",
        "dw_match_type"
      )))
  } else {
    dry_weight_rows <- dry_weight_rows %>%
      dplyr::select(-dplyr::any_of(helper_cols))
  }

  if (append) {
    return(dplyr::bind_rows(data, dry_weight_rows))
  }

  dry_weight_rows
}

load_zooplankton_dry_weight_coefficients <- function() {
  coeff_path <- system.file(
    "extdata",
    "Mesozooplankton_Kattegat_Skagerrak_taxa_and_biomass_calculations.xlsx",
    package = "SHARK4R"
  )

  if (!nzchar(coeff_path)) {
    stop("Could not locate zooplankton dry-weight coefficient workbook.", call. = FALSE)
  }

  readxl::read_excel(coeff_path, progress = FALSE) %>%
    dplyr::mutate(
      reference_taxon = .data[["Scientific name"]]
    )
}
