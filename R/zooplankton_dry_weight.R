#' Calculate zooplankton dry weight from mean length
#'
#' Calculates zooplankton dry weight from rows where `parameter` equals
#' `"Length (mean)"` using taxa-specific coefficients from
#' `inst/extdata/Mesozooplankton_Kattegat_Skagerrak_taxa_and_biomass_calculations.xlsx`.
#'
#' The dry weight calculation follows:
#'
#' `DW = 10^((B * log10(length)) - A)`
#'
#' For nauplii (`dev_stage_code == "NP"`), taxon-specific nauplii coefficients are
#' used when available. Otherwise, the general coefficients for
#' `"copepod nauplii *all copepod species"` are used. All other development
#' stages use the adult coefficients, defined in the reference list as rows with
#' missing `Stadium` (`NA` in the workbook).
#'
#' Matching is performed using `aphia_id`, as preferred for SHARK data. Taxa with
#' no matching coefficient keep `NA` dry-weight values.
#'
#' @param data A data frame or tibble in SHARK zooplankton format. Must contain
#'   the columns `parameter`, `value`, `aphia_id`, and `dev_stage_code`.
#' @param length_parameter Character string giving the parameter name used for
#'   mean length. Defaults to `"Length (mean)"`.
#' @param dry_weight_parameter Character string used for the calculated dry
#'   weight rows. Defaults to `"Dry weight"`.
#' @param append Logical. If `TRUE` (default), append calculated dry-weight rows
#'   to `data`. If `FALSE`, return only the calculated dry-weight rows.
#' @param keep_reference Logical. If `TRUE`, keep additional columns with the
#'   selected coefficients and reference metadata in the output.
#'
#' @return A tibble. By default, the original data are returned with calculated
#'   `"Dry weight"` rows appended. If `append = FALSE`, only the calculated rows
#'   are returned.
#'
#' @section Reference coefficient table:
#' \tabular{llllll}{
#' Reference taxon \tab AphiaID \tab Stadium \tab B \tab A \tab Reference\cr
#' Acartia bifilosa \tab 345919 \tab adult \tab 2.96 \tab 7.71 \tab Hay 1991 (same as for A.clausi)\cr
#' Acartia clausi \tab 149755 \tab adult \tab 2.96 \tab 7.71 \tab Hay 1991\cr
#' Acartia longiremis \tab 346037 \tab adult \tab 2.96 \tab 7.71 \tab Hay 1991 (same as for A.clausi)\cr
#' Acartia \tab 104108 \tab adult \tab 2.96 \tab 7.71 \tab Hay 1991 (same as for A.clausi)\cr
#' Calanus finmarchicus \tab 104464 \tab adult \tab 2.69 \tab 6.88 \tab Hay 1991 (same as for C. helgolandicus)\cr
#' Calanus finmarchicus \tab 104464 \tab NP \tab 2.03 \tab 5.38 \tab Hygum et al 2000\cr
#' Centropages hamatus \tab 104496 \tab adult \tab 2.45 \tab 6.09 \tab Hay 1991\cr
#' Centropages \tab 104159 \tab adult \tab 2.45 \tab 6.10 \tab Hay 1991\cr
#' Centropages typicus \tab 104499 \tab adult \tab 2.45 \tab 6.10 \tab Hay 1991\cr
#' Clausocalanus \tab 104161 \tab adult \tab 3.35 \tab 8.90 \tab Hay et al 1988\cr
#' Corycaeus \tab 128634 \tab adult \tab 2.63 \tab 6.07 \tab Satapoomin 1999\cr
#' Cyclopoida \tab 106415 \tab adult \tab 2.71 \tab 6.72 \tab Uye 1982 (Oithona)\cr
#' Evadne nordmanni \tab 106273 \tab adult \tab 2.80 \tab 5.79 \tab Hernroth 1985\cr
#' Fritillaria \tab 103358 \tab adult \tab 2.66 \tab 4.51 \tab Paffenhofer 1976 (same as for Oikopleura)\cr
#' Harpacticoid copepod \tab 1102 \tab adult \tab 2.89 \tab 7.24 \tab Uye (equation "for all copepods")\cr
#' copepod nauplii *all copepod species \tab 1080 \tab NP \tab 2.23 \tab 5.48 \tab Hay 1991 (same as for Pseudocalanus)\cr
#' Metridia \tab 104190 \tab adult \tab 2.68 \tab 7.12 \tab Hirche & Mumm 1992\cr
#' Microcalanus \tab 104164 \tab adult \tab 2.91 \tab 7.86 \tab Hay 1991 (same as for Temora, se Koski 2012)\cr
#' Microsetella \tab 115341 \tab adult \tab 2.88 \tab 7.66 \tab Satapoomin 1999\cr
#' Oikopleura dioica \tab 103407 \tab adult \tab 2.66 \tab 4.51 \tab Paffenhofer 1976\cr
#' Oithona \tab 106485 \tab NP \tab 2.14 \tab 2.68 \tab Almeda et al 2010\cr
#' Oithona similis \tab 106656 \tab adult \tab 2.71 \tab 6.72 \tab Uye 1982\cr
#' Oncaea \tab 128690 \tab adult \tab 2.63 \tab 6.28 \tab Satapoomin 1999\cr
#' Paracalanus parvus \tab 104685 \tab adult \tab 2.45 \tab 6.16 \tab Hay 1991\cr
#' Penilia avirostris \tab 106272 \tab adult \tab 2.38 \tab 4.95 \tab Atienza et al 2006\cr
#' Podon leukarti \tab 106277 \tab adult \tab 3.02 \tab 7.52 \tab Uye 1982\cr
#' Podon polyphemoides \tab 159919 \tab adult \tab 2.75 \tab 6.60 \tab Uye 1982\cr
#' Pseudocalanus \tab 104165 \tab adult \tab 3.00 \tab 8.37 \tab Hay et al 1988\cr
#' Temora longicornis \tab 104878 \tab adult \tab 3.00 \tab 8.37 \tab Hay et al 1988\cr
#' }
#'
#' @examples
#' zoo <- dplyr::tibble(
#'   scientific_name = c("Acartia clausi", "Calanus finmarchicus", "Unknown taxon"),
#'   parameter = c("Length (mean)", "Length (mean)", "Length (mean)"),
#'   value = c(200, 250, 160),
#'   aphia_id = c(149755, 104464, 999999),
#'   dev_stage_code = c("AD", "NP", "NP")
#' )
#'
#' calc_zooplankton_dry_weight(zoo, append = FALSE)
#'
#' @importFrom rlang .data
#' @export
calc_zooplankton_dry_weight <- function(data,
                                        length_parameter = "Length (mean)",
                                        dry_weight_parameter = "Dry weight",
                                        append = TRUE,
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

  if ("calculation_method" %in% names(dry_weight_rows)) {
    dry_weight_rows <- dry_weight_rows %>%
      dplyr::mutate(
        calculation_method = paste0(
          "Calculated from ",
          length_parameter,
          " using Log DW = (B x Log(length)) - A with AphiaID-based coefficients"
        )
      )
  }

  if ("reported_value" %in% names(dry_weight_rows)) {
    dry_weight_rows <- dry_weight_rows %>%
      dplyr::mutate(reported_value = NA)
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
