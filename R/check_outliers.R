#' General outlier check function for SHARK data
#'
#' This function checks whether values for a specified parameter exceed a predefined
#' threshold. Thresholds are provided in a dataframe (default `.threshold_values`),
#' which should contain columns for `parameter`, `datatype`, and at least one numeric
#' threshold column (e.g., `extreme_upper`). Only rows in `data` matching both the
#' `parameter` and `delivery_datatype` (`datatype`) are considered. Optionally, data
#' can be grouped by a custom column (e.g., `location_sea_basin`) when thresholds vary by group.
#'
#' @param data A tibble containing data in SHARK format. Must include columns:
#'   `parameter`, `value`, `delivery_datatype`, `station_name`, `sample_date`,
#'   `sample_id`, `shark_sample_id_md5`, `sample_min_depth_m`, `sample_max_depth_m`,
#'   and any custom grouping column used in `custom_group`.
#' @param parameter Character. Name of the parameter to check. Must exist in both
#'   `data$parameter` and `thresholds$parameter`.
#' @param datatype Character. Data type to match against `delivery_datatype` in `data`
#'   and `datatype` in `thresholds`.
#' @param threshold_col Character. Name of the threshold column in `thresholds`
#'   to use for comparison. Defaults to `"extreme_upper"`. Other columns (e.g., `"min"`,
#'   `"Q1"`, `"median"`, `"max"`, `"mild_upper"`, etc.) can also be used if present.
#' @param thresholds A tibble/data frame of thresholds. Must include columns `parameter`,
#'   `datatype`, and at least one numeric threshold column. Defaults to `.threshold_values`.
#' @param custom_group Character or NULL. Optional column name in `data` and `thresholds`
#'   for grouping (e.g., `"location_sea_basin"`). If specified, thresholds are matched by
#'   group as well as `parameter` and `datatype`.
#' @param direction Character. Either `"above"` (flag values above threshold) or `"below"`
#'   (flag values below threshold). Default is `"above"`.
#' @param return_df Logical. If TRUE, returns a plain data.frame of flagged rows instead of
#'   a DT datatable. Default = FALSE.
#' @param verbose Logical. If TRUE, messages will be displayed during execution. Defaults to TRUE.
#'
#' @return If outliers are found, returns a `DT::datatable` or a data.frame (if `return_df = TRUE`)
#'   containing:
#'   `datatype`, `station_name`, `sample_date`, `sample_id`, `parameter`, `value`, `threshold`,
#'   and `custom_group` if specified. Otherwise, prints a message indicating that values
#'   are within the threshold range (if `verbose = TRUE`) and returns `invisible(NULL)`.
#'
#' @details
#' - Only rows in `data` matching both `parameter` and `delivery_datatype` are checked.
#' - If `custom_group` is specified, thresholds are applied per group.
#' - If `threshold_col` does not exist in `thresholds`, the function stops with a warning.
#' - Values exceeding (or below) the threshold are flagged as outliers.
#' - Intended for interactive use in Shiny apps where `threshold_col` can be selected dynamically.
#'
#' @seealso [get_shark_statistics()] for preparing updated threshold data.
#'
#' @examples
#' # Minimal example dataset
#' example_data <- dplyr::tibble(
#'   station_name = c("S1", "S2"),
#'   sample_date = as.Date(c("2025-01-01", "2025-01-02")),
#'   sample_id = 1:2,
#'   shark_sample_id_md5 = letters[1:2],
#'   sample_min_depth_m = c(0, 5),
#'   sample_max_depth_m = c(1, 6),
#'   parameter = c("Param1", "Param1"),
#'   value = c(5, 12),
#'   delivery_datatype = c("TypeA", "TypeA")
#' )
#'
#' example_thresholds <- dplyr::tibble(
#'   parameter = "Param1",
#'   datatype = "TypeA",
#'   extreme_upper = 10,
#'   mild_upper = 8
#' )
#'
#' # Check for values above "extreme_upper"
#' check_outliers(
#'   data = example_data,
#'   parameter = "Param1",
#'   datatype = "TypeA",
#'   threshold_col = "extreme_upper",
#'   thresholds = example_thresholds,
#'   return_df = TRUE
#' )
#'
#' # Check for values above "mild_upper"
#' check_outliers(
#'   data = example_data,
#'   parameter = "Param1",
#'   datatype = "TypeA",
#'   threshold_col = "mild_upper",
#'   thresholds = example_thresholds,
#'   return_df = TRUE
#' )
#'
#' @export
check_outliers <- function(data,
                           parameter,
                           datatype,
                           threshold_col = "extreme_upper",
                           thresholds = .threshold_values,
                           custom_group = NULL,
                           direction = c("above", "below"),
                           return_df = FALSE,
                           verbose = TRUE) {

  direction <- match.arg(direction)  # ensures only "above" or "below"

  # --- Validate inputs ---
  if (!threshold_col %in% names(thresholds)) {
    warning(paste("Column", threshold_col, "not found in thresholds dataframe"))
    return(invisible(NULL))
  }

  if (!all(c("parameter", "datatype") %in% names(thresholds))) {
    stop("Thresholds must contain columns 'parameter' and 'datatype'")
  }

  if (!is.null(custom_group) && !custom_group %in% names(data)) {
    stop(paste("Grouping column", custom_group, "not found in data"))
  }

  # --- Filter relevant thresholds ---
  thr_df <- thresholds %>%
    dplyr::filter(parameter == !!parameter, datatype == !!datatype)

  if (nrow(thr_df) == 0) {
    warning(paste("No thresholds found for", parameter, "and", datatype))
    return(invisible(NULL))
  }

  # --- Merge thresholds into data (optionally by group) ---
  data_param <- data %>%
    dplyr::filter(parameter == !!parameter) %>%
    dplyr::filter(delivery_datatype == !!datatype)

  if (!is.null(custom_group) && custom_group %in% names(thr_df)) {
    if (custom_group == "parameter") {
      by_cols <- c("parameter", "delivery_datatype" = "datatype")
    } else {
      by_cols <- c("parameter", "delivery_datatype" = "datatype", custom_group)
    }

    data_joined <- dplyr::left_join(
      data_param,
      thr_df %>%
        dplyr::select(parameter, datatype, !!custom_group, !!threshold_col),
      by = by_cols
    )
  } else {
    data_joined <- dplyr::mutate(
      data_param,
      !!threshold_col := thr_df[[threshold_col]][1]
    )
  }

  # --- Flag outliers ---
  outliers <- data_joined %>%
    dplyr::filter(
      if (direction == "above") value > .data[[threshold_col]] else value < .data[[threshold_col]]
    ) %>%
    dplyr::mutate(threshold = .data[[threshold_col]]) %>%
    dplyr::select(any_of(c(
      "datatype", "delivery_datatype", "station_name", "sample_date", "sample_id",
      "scientific_name", "parameter", "value", "threshold", custom_group
    )))

  # --- Output ---
  if (nrow(outliers) > 0) {
    if (verbose) {
      msg <- paste(
        "WARNING:", parameter, "(", datatype, ")",
        if (direction == "above") "exceeds" else "is below",
        threshold_col, "in", if (!is.null(custom_group)) custom_group else "dataset"
      )
      message(msg)
    }

    if (return_df) {
      return(outliers)
    } else {
      return(DT::datatable(outliers))
    }

  } else {
    if (verbose) message(parameter, " is within the ", threshold_col, " range.")
    return(invisible(outliers))
  }
}

#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_bacterial_production <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_bacterial_production()", "check_outliers()")
  check_outliers(
    data = data,
    parameter = "Bacterial carbon production",
    datatype = "Bacterioplankton",
    threshold_col = "extreme_upper"
  )
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_bacterial_concentration <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_bacterial_concentration()", "check_outliers()")
  check_outliers(data, parameter = "Bacterial abundance", datatype = "Bacterioplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_bacterial_carbon <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_bacterial_carbon()", "check_outliers()")
  check_outliers(data, parameter = "Bacterial cell carbon content", datatype = "Bacterioplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_chlorophyll_conc <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_chlorophyll_conc()", "check_outliers()")
  check_outliers(data, parameter = "Chlorophyll-a", datatype = "Chlorophyll", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_picoplankton_abundance <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_picoplankton_abundance()", "check_outliers()")
  check_outliers(data, parameter = "Abundance", datatype = "Picoplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_picoplankton_biovol <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_picoplankton_biovol()", "check_outliers()")
  check_outliers(data, parameter = "Biovolume concentration", datatype = "Picoplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_picoplankton_carbon <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_picoplankton_carbon()", "check_outliers()")
  check_outliers(data, parameter = "Carbon concentration", datatype = "Picoplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_picoplankton_counted <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_picoplankton_counted()", "check_outliers()")
  check_outliers(data, parameter = "# counted", datatype = "Picoplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_zooplankton_abund <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_zooplankton_abund()", "check_outliers()")
  check_outliers(data, parameter = "Abundance", datatype = "Zooplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_zooplankton_counted <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_zooplankton_counted()", "check_outliers()")
  check_outliers(data, parameter = "# counted", datatype = "Zooplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_zooplankton_length_mean <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_zooplankton_length_mean()", "check_outliers()")
  check_outliers(data, parameter = "Length (mean)", datatype = "Zooplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_zooplankton_length_median <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_zooplankton_length_median()", "check_outliers()")
  check_outliers(data, parameter = "Length (median)", datatype = "Zooplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_zooplankton_wetweight <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_zooplankton_wetweight()", "check_outliers()")
  check_outliers(data, parameter = "Wet weight", datatype = "Zooplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_zooplankton_carbon <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_zooplankton_carbon()", "check_outliers()")
  check_outliers(data, parameter = "Carbon content", datatype = "Zooplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_zooplankton_wetweight_volume <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_zooplankton_wetweight_volume()", "check_outliers()")
  check_outliers(data, parameter = "Wet weight/volume", datatype = "Zooplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_zooplankton_wetweight_area <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_zooplankton_wetweight_area()", "check_outliers()")
  check_outliers(data, parameter = "Wet weight/area", datatype = "Zooplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_phytoplankton_abund <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_phytoplankton_abund()", "check_outliers()")
  check_outliers(data, parameter = "Abundance", datatype = "Phytoplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_phytoplankton_biovol <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_phytoplankton_biovol()", "check_outliers()")
  check_outliers(data, parameter = "Biovolume concentration", datatype = "Phytoplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_phytoplankton_carbon <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_phytoplankton_carbon()", "check_outliers()")
  check_outliers(data, parameter = "Carbon concentration", datatype = "Phytoplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_phytoplankton_counted <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_phytoplankton_counted()", "check_outliers()")
  check_outliers(data, parameter = "# counted", datatype = "Phytoplankton", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_primaryproduction_carbonprod <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_primaryproduction_carbonprod()", "check_outliers()")
  check_outliers(data, parameter = "Carbon production", datatype = "Primary production", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_primaryproduction_carbonprodlight <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_primaryproduction_carbonprodlight()", "check_outliers()")
  check_outliers(data, parameter = "Carbon prod in light", datatype = "Primary production", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_primaryproduction_carbonprod_hour <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_primaryproduction_carbonprod_hour()", "check_outliers()")
  check_outliers(data, parameter = "Carbon production/hour", datatype = "Primary production", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_epibenthos_counted <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_epibenthos_counted()", "check_outliers()")
  check_outliers(data, parameter = "# counted", datatype = "Epibenthos", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_epibenthos_dryweight <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_epibenthos_dryweight()", "check_outliers()")
  check_outliers(data, parameter = "Dry weight", datatype = "Epibenthos", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_epibenthos_specdistr_maxdepth <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_epibenthos_specdistr_maxdepth()", "check_outliers()")
  check_outliers(data, parameter = "Species distribution max depth", datatype = "Epibenthos", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_epibenthos_specdistr_mindepth <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_epibenthos_specdistr_mindepth()", "check_outliers()")
  check_outliers(data, parameter = "Species distribution min depth", datatype = "Epibenthos", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_harbourseal_counted <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_harbourseal_counted()", "check_outliers()")
  check_outliers(data, parameter = "# counted", datatype = "Harbour seal", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_greyseal_counted <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_greyseal_counted()", "check_outliers()")
  check_outliers(data, parameter = "# counted", datatype = "Grey seal", threshold_col = "extreme_upper")
}
#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_zoobenthos_BQIm <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_zoobenthos_BQIm()", "check_outliers()")
  check_outliers(data, parameter = "BQIm", datatype = "Zoobenthos", threshold_col = "extreme_upper")
}

#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_zoobenthos_abund <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_zoobenthos_abund()", "check_outliers()")
  check_outliers(data = data, parameter = "Abundance", datatype = "Zoobenthos", threshold_col = "extreme_upper")
}

#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_zoobenthos_counted <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_zoobenthos_counted()", "check_outliers()")
  check_outliers(data = data, parameter = "# counted", datatype = "Zoobenthos", threshold_col = "extreme_upper")
}

#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_zoobenthos_wetweight <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_zoobenthos_wetweight()", "check_outliers()")
  check_outliers(data = data, parameter = "Wet weight", datatype = "Zoobenthos", threshold_col = "extreme_upper")
}

#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_ringedseal_calccounted <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_ringedseal_calccounted()", "check_outliers()")
  check_outliers(data = data, parameter = "Calculated # counted", datatype = "Ringed seal", threshold_col = "extreme_upper")
}

#' Uses data from national marine monitoring for the last 5 years to identify outliers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_outliers()].
#'
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return tibble of data with outliers
#' @keywords internal
#' @export
check_harbporp_positivemin <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "check_harbporp_positivemin()", "check_outliers()")
  check_outliers(data = data, parameter = "Porpoise positive minutes", datatype = "Harbour Porpoise", threshold_col = "extreme_upper")
}
