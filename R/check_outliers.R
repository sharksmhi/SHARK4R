#' General outlier check function for SHARK data
#'
#' This function checks whether values for a specified parameter exceed a predefined
#' threshold. Thresholds are provided in a dataframe (default `.threshold_values`),
#' which should contain columns for `parameter`, `datatype`, and at least one threshold
#' column (e.g., `extreme_upper`). Only rows in `data` matching both the `parameter`
#' and `delivery_datatype` (`datatype`) are considered.
#'
#' @param data A tibble containing data in SHARK format. Must include columns:
#'   `parameter`, `value`, `delivery_datatype`, `station_name`, `sample_date`,
#'   `sample_id`, `shark_sample_id_md5`, `sample_min_depth_m`, `sample_max_depth_m`.
#' @param parameter Character. Name of the parameter to check. Must exist in both
#'   `data$parameter` and `thresholds$parameter`.
#' @param datatype Character. Data type to match against `delivery_datatype` in `data`
#'   and `datatype` in `thresholds`.
#' @param threshold_col Character. Name of the threshold column in `thresholds`
#'   to use for comparison. Default is `"extreme_upper"`, with the default option `"mild_upper"`.
#' @param thresholds A tibble/data frame of thresholds. Must include columns `parameter`,
#'   `datatype`, and at least one numeric threshold column. Defaults to `.threshold_values`.
#'
#' @return If outliers are found, returns a `DT::datatable` containing:
#'   `delivery_datatype`, `station_name`, `sample_date`, `sample_id`,
#'   `shark_sample_id_md5`, `sample_min_depth_m`, `sample_max_depth_m`,
#'   `parameter`, `value`, and `threshold`. Otherwise, prints a message indicating
#'   that values are within the threshold range.
#'
#' @details
#' - Only rows in `data` matching both `parameter` and `delivery_datatype` are checked.
#' - If `threshold_col` does not exist in `thresholds`, the function stops with an error.
#' - If no threshold is found for the specified `parameter` and `datatype`, the function
#'   stops with an error.
#' - Values exceeding the threshold are flagged as outliers and displayed in an interactive table.
#'
#' @seealso [get_shark_statistics()] for preparing an updated threshold data frame.
#'
#' @examples
#' # Create a minimal example dataset
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
#'   extreme_upper = 10
#' )
#'
#' check_outliers(
#'   data = example_data,
#'   parameter = "Param1",
#'   datatype = "TypeA",
#'   threshold_col = "extreme_upper",
#'   thresholds = example_thresholds
#' )
#'
#' @export
check_outliers <- function(data,
                           parameter,
                           datatype,
                           threshold_col = "extreme_upper",
                           thresholds = .threshold_values) {

  # Check if requested threshold exists
  if (!threshold_col %in% names(thresholds)) {
    stop(paste("Column", threshold_col, "not found in thresholds dataframe"))
  }

  # Get threshold for this parameter
  thr <- thresholds[thresholds$parameter == parameter & thresholds$datatype == datatype,
                    threshold_col, drop = TRUE]

  if (length(thr) == 0) {
    stop(paste("Parameter", parameter, "not found in thresholds dataframe"))
  }

  if (length(thr) > 1) {
    stop(paste("Multiple thresholds found for parameter", parameter, "and datatype", datatype))
  }

  # Filter data for the parameter
  data_param <- data %>%
    dplyr::filter(parameter == !!parameter) %>%
    dplyr::filter(delivery_datatype == !!datatype)

  # Flag outliers
  outliers <- data_param %>%
    dplyr::filter(value > thr) %>%
    dplyr::mutate(threshold = thr) %>%
    dplyr::select(delivery_datatype, station_name, sample_date, sample_id,
                  shark_sample_id_md5, sample_min_depth_m,
                  sample_max_depth_m, parameter, value, threshold)

  if (nrow(outliers) > 0) {
    message(paste("WARNING:", parameter, "exceeds", threshold_col,
                  "- please check outliers!"))
    return(DT::datatable(outliers))
  } else {
    message(paste(parameter, "is within the", threshold_col, "range."))
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_bacterial_production()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_bacterial_concentration()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_bacterial_carbon()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_chlorophyll_conc()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_picoplankton_abundance()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_picoplankton_biovol()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_picoplankton_carbon()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_picoplankton_counted()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_zooplankton_abund()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_zooplankton_counted()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_zooplankton_length_mean()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_zooplankton_length_median()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_zooplankton_wetweight()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_zooplankton_carbon()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_zooplankton_wetweight_volume()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_zooplankton_wetweight_area()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_phytoplankton_abund()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_phytoplankton_biovol()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_phytoplankton_carbon()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_phytoplankton_counted()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_primaryproduction_carbonprod()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_primaryproduction_carbonprodlight()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_primaryproduction_carbonprod_hour()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_epibenthos_counted()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_epibenthos_dryweight()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_epibenthos_specdistr_maxdepth()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_epibenthos_specdistr_mindepth()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_harbourseal_counted()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_greyseal_counted()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_zoobenthos_BQIm()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_zoobenthos_abund()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_zoobenthos_counted()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_zoobenthos_wetweight()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_ringedseal_calccounted()", "check_outliers()")
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
  lifecycle::deprecate_warn("0.1.7.9000", "check_harbporp_positivemin()", "check_outliers()")
  check_outliers(data = data, parameter = "Porpoise positive minutes", datatype = "Harbour Porpoise", threshold_col = "extreme_upper")
}
