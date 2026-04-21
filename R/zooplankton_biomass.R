#' Calculate zooplankton biomass from dry weight and abundance
#'
#' Calculates zooplankton biomass by combining per-individual dry weight
#' (`"Dry weight (mean)"`, in `ug`) with abundance measurements in the same
#' observation. Two biomass parameters are produced:
#'
#' \itemize{
#'   \item **Biomass concentration** (`mg/m3`) from `"Abundance"` rows
#'     (`ind/m3`).
#'   \item **Integrated biomass** (`mg/m2`) from `"Integrated abundance"` rows
#'     (`ind/m2`).
#' }
#'
#' The conversion is `biomass = dry_weight_ug * abundance / 1000` so that the
#' result is expressed in `mg/m3` or `mg/m2`.
#'
#' @section Observation key:
#' Dry-weight and abundance rows are matched one-to-one using the following
#' columns, which together identify a single zooplankton observation in SHARK:
#' \itemize{
#'   \item `platform_code`
#'   \item `station_name`
#'   \item `sample_date`
#'   \item `sample_time`
#'   \item `sample_min_depth_m`
#'   \item `sample_max_depth_m`
#'   \item `aphia_id`
#'   \item `sex_code`
#'   \item `dev_stage_code`
#'   \item `size_class`
#' }
#'
#' One biomass row is produced per matching abundance row (no aggregation
#' across size classes or development stages). Abundance rows without a
#' matching dry-weight value yield `NA` biomass and are dropped by default.
#'
#' If no `"Dry weight (mean)"` rows are present in `data`, the function calls
#' [calc_zooplankton_dry_weight()] internally to compute them from
#' `"Length (mean)"` before calculating biomass.
#'
#' @param data A data frame or tibble in SHARK zooplankton format. Must contain
#'   the observation-key columns listed in the *Observation key* section, plus
#'   `parameter` and `value`.
#' @param abundance_parameter Character string giving the parameter name for
#'   abundance in `ind/m3`. Defaults to `"Abundance"`.
#' @param integrated_abundance_parameter Character string giving the parameter
#'   name for integrated abundance in `ind/m2`. Defaults to
#'   `"Integrated abundance"`.
#' @param dry_weight_parameter Character string giving the parameter name for
#'   per-individual dry weight in `ug`. Defaults to `"Dry weight (mean)"`.
#' @param biomass_concentration_parameter Character string used for calculated
#'   biomass concentration rows. Defaults to `"Biomass concentration"`.
#' @param integrated_biomass_parameter Character string used for calculated
#'   integrated biomass rows. Defaults to `"Integrated biomass"`.
#' @param append Logical. If `TRUE` (default), append calculated biomass rows
#'   to `data`. If `FALSE`, return only the calculated biomass rows.
#' @param drop_na_values Logical. If `TRUE` (default), drop calculated rows
#'   where biomass could not be calculated and `value` is `NA`.
#' @param keep_reference Logical. If `TRUE`, keep the dry-weight coefficient
#'   and reference columns used to compute biomass in the output.
#'
#' @return A tibble. By default, the original data are returned with calculated
#'   biomass rows appended. If `append = FALSE`, only the calculated rows are
#'   returned.
#'
#' @examples
#' zoo <- dplyr::tibble(
#'   platform_code = "77SE",
#'   station_name = "ANHOLT E",
#'   sample_date = as.Date("2023-06-01"),
#'   sample_time = "10:00",
#'   sample_min_depth_m = 0,
#'   sample_max_depth_m = 30,
#'   aphia_id = 104251,
#'   sex_code = NA_character_,
#'   dev_stage_code = "AD",
#'   size_class = NA_character_,
#'   parameter = c("Length (mean)", "Abundance", "Integrated abundance"),
#'   value = c(800, 120, 3600),
#'   unit = c("um", "ind/m3", "ind/m2")
#' )
#'
#' calc_zooplankton_biomass(zoo, append = FALSE)
#'
#' @seealso [calc_zooplankton_dry_weight()]
#'
#' @importFrom rlang .data
#' @export
calc_zooplankton_biomass <- function(data,
                                     abundance_parameter = "Abundance",
                                     integrated_abundance_parameter = "Integrated abundance",
                                     dry_weight_parameter = "Dry weight (mean)",
                                     biomass_concentration_parameter = "Biomass concentration",
                                     integrated_biomass_parameter = "Integrated biomass",
                                     append = TRUE,
                                     drop_na_values = TRUE,
                                     keep_reference = FALSE) {
  key_cols <- c(
    "platform_code", "station_name", "sample_date", "sample_time",
    "sample_min_depth_m", "sample_max_depth_m",
    "aphia_id", "sex_code", "dev_stage_code", "size_class"
  )

  required_cols <- c(key_cols, "parameter", "value")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  has_dry_weight <- any(data$parameter == dry_weight_parameter, na.rm = TRUE)

  working <- data

  if (!has_dry_weight) {
    working <- calc_zooplankton_dry_weight(
      working,
      dry_weight_parameter = dry_weight_parameter,
      append = TRUE,
      drop_na_values = FALSE,
      keep_reference = keep_reference
    )
  }

  dry_weight_lookup <- working %>%
    dplyr::filter(.data$parameter == dry_weight_parameter) %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(.data$value))) %>%
    dplyr::select(dplyr::all_of(key_cols), dry_weight_ug = "value")

  dry_weight_lookup <- dry_weight_lookup %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(key_cols)), .keep_all = TRUE)

  biomass_concentration_rows <- build_biomass_rows(
    working,
    abundance_parameter,
    biomass_concentration_parameter,
    "mg/m3",
    dry_weight_lookup,
    key_cols
  )

  integrated_biomass_rows <- build_biomass_rows(
    working,
    integrated_abundance_parameter,
    integrated_biomass_parameter,
    "mg/m2",
    dry_weight_lookup,
    key_cols
  )

  biomass_rows <- dplyr::bind_rows(
    biomass_concentration_rows,
    integrated_biomass_rows
  )

  if (isTRUE(drop_na_values)) {
    biomass_rows <- biomass_rows %>%
      dplyr::filter(!is.na(.data$value))
  }

  if (append) {
    return(dplyr::bind_rows(data, biomass_rows))
  }

  biomass_rows
}

build_biomass_rows <- function(data,
                               source_parameter,
                               target_parameter,
                               target_unit,
                               dry_weight_lookup,
                               key_cols) {
  abundance_rows <- data %>%
    dplyr::filter(.data$parameter == source_parameter)

  if (nrow(abundance_rows) == 0) {
    return(abundance_rows[0, , drop = FALSE])
  }

  abundance_rows <- abundance_rows %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(.data$value))) %>%
    dplyr::left_join(dry_weight_lookup, by = key_cols) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        !is.na(.data$dry_weight_ug) & !is.na(.data$value) ~
          .data$dry_weight_ug * .data$value / 1000,
        TRUE ~ NA_real_
      ),
      parameter = target_parameter
    )

  if ("unit" %in% names(abundance_rows)) {
    abundance_rows <- abundance_rows %>%
      dplyr::mutate(unit = target_unit)
  }

  if ("calculation_method" %in% names(abundance_rows)) {
    abundance_rows <- abundance_rows %>%
      dplyr::mutate(
        calculation_method = paste0(
          "Calculated as dry weight (ug) x ",
          source_parameter,
          " / 1000; returns biomass in ",
          target_unit
        )
      )
  }

  if ("reported_value" %in% names(abundance_rows)) {
    abundance_rows <- abundance_rows %>%
      dplyr::mutate(reported_value = NA)
  }

  abundance_rows %>%
    dplyr::select(-dplyr::any_of("dry_weight_ug"))
}
