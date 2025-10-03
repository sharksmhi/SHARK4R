#' Check logical assumptions of data for specific variables and parameters
#' @param data A data frame. Must contain a column named `value`.
#' @return A DT datatable with problematic entries, or NULL (invisibly) if all values are valid.
#' @export
check_value_logical <- function(data) {
  if (!"value" %in% names(data)) {
    stop("data must contain a 'value' column")
  }

  vals <- data$value
  vals_chr <- as.character(vals)

  # strict numeric/logical pattern
  #  - numeric (int/float, with optional scientific notation)
  #  - TRUE or FALSE (case-insensitive)
  valid_pattern <- "^(TRUE|FALSE|[+-]?(?:[0-9]*\\.?[0-9]+)([eE][+-]?[0-9]+)?)$"

  valid_ok <- grepl(valid_pattern, vals_chr, ignore.case = TRUE)
  valid_ok[is.na(valid_ok)] <- FALSE

  non_valid_idx <- !valid_ok

  if (any(non_valid_idx)) {
    message("ERROR: Expected numerical/logical value but found invalid characters")
    message("Common problems are e.g. '<', '>' signs or letters not matching TRUE/FALSE.")
    matches <- unique(vals_chr[non_valid_idx])
    matches_df <- data.frame(value = matches, stringsAsFactors = FALSE)
    return(DT::datatable(matches_df))
  } else {
    message("Expected values are correctly formatted (numeric or logical).")
    invisible(NULL)
  }
}

#' Check logical assumptions of data for specific variables and parameters
#' @param data A data frame. Must contain a column named `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows
#'        instead of a DT datatable. Default = FALSE.
#' @return A DT datatable with zero-value records, or NULL (invisibly) if none found.
#' @export
check_zero_value <- function(data, return_df = FALSE) {
  if (!"value" %in% names(data)) {
    stop("data must contain a 'value' column")
  }

  # Coerce to numeric safely (for factors/characters that are numeric-like)
  vals <- suppressWarnings(as.numeric(as.character(data$value)))

  if (any(vals == 0, na.rm = TRUE)) {
    message("ERROR: Value contain zeroes (0). Please check zero values!")
    zero_values <- data %>%
      dplyr::filter(vals == 0) %>%
      dplyr::select(
        dplyr::any_of(c(
          "station_name", "sample_date", "sample_id",
          "shark_sample_id_md5", "sample_min_depth_m",
          "sample_max_depth_m", "value"
        ))
      )

    if (return_df) {
      return(zero_values)
    } else {
      return(DT::datatable(zero_values))
    }
  } else {
    message("No zero values were found")
    invisible(NULL)
  }
}

#' Check logical assumptions of data for station positions
#' @param data A data frame. Must contain `sample_longitude_dd` and/or `sample_latitude_dd`.
#' @param coord Character. Which coordinate(s) to check: "longitude", "latitude", or "both". Default = "longitude".
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows
#'        instead of a DT datatable. Default = FALSE.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows have zero in the selected coordinate(s). Overrides return_df. Default = FALSE.
#' @return A DT datatable, a data.frame, a logical vector, or NULL (if no problems found
#'         and return_logical = FALSE).
#' @export
check_zero_positions <- function(data, coord = "longitude", return_df = FALSE, return_logical = FALSE) {
  if(return_df & return_logical) {
    warning("Both return_df and return_logical are TRUE. Ignoring return_df and returning logical vector.")
    return_df <- FALSE
  }

  coord <- match.arg(coord, c("longitude", "latitude", "both"))

  # Check required columns
  required_cols <- c()
  if (coord %in% c("longitude", "both")) required_cols <- c(required_cols, "sample_longitude_dd")
  if (coord %in% c("latitude", "both")) required_cols <- c(required_cols, "sample_latitude_dd")

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data must contain column(s): ", paste(missing_cols, collapse = ", "))
  }

  # Coerce to numeric safely
  zero_vec <- rep(FALSE, nrow(data))
  if (coord %in% c("longitude", "both")) {
    lon_vals <- suppressWarnings(as.numeric(as.character(data$sample_longitude_dd)))
    zero_vec <- zero_vec | (lon_vals == 0)
  }
  if (coord %in% c("latitude", "both")) {
    lat_vals <- suppressWarnings(as.numeric(as.character(data$sample_latitude_dd)))
    zero_vec <- zero_vec | (lat_vals == 0)
  }
  zero_vec[is.na(zero_vec)] <- FALSE

  if (return_logical) return(zero_vec)

  if (any(zero_vec)) {
    message("ERROR: Positions contain zeroes (0). Please check station coordinates with zero values!")

    zero_positions <- data %>%
      dplyr::filter(zero_vec) %>%
      dplyr::select(
        dplyr::any_of(c(
          "station_name", "sample_date", "sample_id",
          "shark_sample_id_md5", "sample_min_depth_m",
          "sample_max_depth_m", "sample_longitude_dd", "sample_latitude_dd"
        ))
      )

    if (return_df) {
      return(zero_positions)
    } else {
      return(DT::datatable(zero_positions))
    }
  } else {
    message("No zero positions were found")
    invisible(NULL)
  }
}

#' General checker for parameter-specific logical rules
#'
#' This function checks for logical rule violations in benthos/epibenthos data
#' by applying a user-defined condition to values for a given parameter.
#' It is intended to replace the old family of `check_*_*_logical()` functions.
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param param_name Character; the name of the parameter to check.
#' @param condition A function that takes a numeric vector of values and
#'   returns a logical vector (TRUE for rows considered problematic).
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data).
#'        Overrides return_df.
#'
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @export
#' @examples
#' # Example dataset
#' df <- dplyr::tibble(
#'   station_name = c("A1", "A2", "A3", "A4"),
#'   sample_date = as.Date("2023-05-01") + 0:3,
#'   sample_id = 101:104,
#'   parameter = c("Biomass", "Biomass", "Abundance", "Biomass"),
#'   value = c(5, -2, 10, 0)
#' )
#'
#' # 1. Check that Biomass is never negative (returns DT datatable by default)
#' check_logical_parameter(df, "Biomass", function(x) x < 0)
#'
#' # 2. Same check, but return problematic rows as a data frame
#' check_logical_parameter(df, "Biomass", function(x) x < 0, return_df = TRUE)
#'
#' # 3. Return logical vector marking problematic rows
#' check_logical_parameter(df, "Biomass", function(x) x < 0, return_logical = TRUE)
#'
#' # 4. Check that Abundance is not zero (no problems found -> returns NULL)
#' check_logical_parameter(df, "Abundance", function(x) x == 0)
#'
check_logical_parameter <- function(data, param_name, condition,
                                    return_df = FALSE, return_logical = FALSE) {
  required_cols <- c("parameter", "value")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data must contain column(s): ", paste(missing_cols, collapse = ", "))
  }

  if (return_df & return_logical) {
    warning("Both return_df and return_logical are TRUE. Ignoring return_df and returning logical vector.")
    return_df <- FALSE
  }

  value_num <- suppressWarnings(as.numeric(as.character(data$value)))
  is_param <- data$parameter == param_name
  error_vec <- is_param & (!is.na(value_num) & condition(value_num))

  if (return_logical) return(error_vec)

  if (any(error_vec)) {
    logical_error <- data %>%
      dplyr::filter(error_vec) %>%
      dplyr::select(dplyr::any_of(c(
        "station_name", "sample_date", "sample_id",
        "shark_sample_id_md5", "sample_min_depth_m",
        "sample_max_depth_m", "parameter", "value"
      )))

    if (return_df) {
      return(logical_error)
    } else {
      return(DT::datatable(logical_error))
    }
  } else {
    invisible(NULL)
  }
}

#' Check if Epibenthos total cover exceeds 100%
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_logical_parameter()]. Alternatively, you can use [check_parameter_rules()].
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 100% for Total cover. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @keywords internal
#' @export
check_epibenthos_totcover_logical <- function(data, return_df = FALSE, return_logical = FALSE) {
  lifecycle::deprecate_warn("0.1.7.9000", "check_epibenthos_totcover_logical()", "check_logical_parameter()")

  res <- check_logical_parameter(
    data,
    param_name = "Total cover of all species (%)",
    condition = function(x) x > 100,
    return_df = return_df,
    return_logical = return_logical
  )

  if (is.logical(res)) return(res)

  if (!is.null(res)) {
    message("Parameter Total cover of all species (%), measurement(s) > 100%")
    return(res)
  } else {
    message("Parameter Total cover of all species (%), measurement(s) are within 0-100%")
    return(invisible(NULL))
  }
}

#' Check if Epibenthos cover (%) exceeds 100%
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_logical_parameter()]. Alternatively, you can use [check_parameter_rules()].
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 100% for Total cover. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @keywords internal
#' @export
check_epibenthos_coverpercent_logical <- function(data, return_df = FALSE, return_logical = FALSE) {
  lifecycle::deprecate_warn("0.1.7.9000", "check_epibenthos_coverpercent_logical()", "check_logical_parameter()")

  res <- check_logical_parameter(
    data,
    param_name = "Cover (%)",
    condition = function(x) x > 100,
    return_df = return_df,
    return_logical = return_logical
  )

  if (is.logical(res)) return(res)

  if (!is.null(res)) {
    message("Parameter Cover (%), measurement(s) > 100%")
    return(res)
  } else {
    message("Parameter Cover (%), measurement(s) are within 0-100%")
    return(invisible(NULL))
  }
}

#' Check if Epibenthos cover exceeds 100%
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_logical_parameter()]. Alternatively, you can use [check_parameter_rules()].
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 100% for Total cover. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @keywords internal
#' @export
check_epibenthos_cover_logical <- function(data, return_df = FALSE, return_logical = FALSE) {
  lifecycle::deprecate_warn("0.1.7.9000", "check_epibenthos_cover_logical()", "check_logical_parameter()")

  res <- check_logical_parameter(
    data,
    param_name = "Cover",
    condition = function(x) x > 100,
    return_df = return_df,
    return_logical = return_logical
  )

  if (is.logical(res)) return(res)

  if (!is.null(res)) {
    message("Parameter Cover, measurement(s) > 100%")
    return(res)
  } else {
    message("Parameter Cover, measurement(s) are within 0-100%")
    return(invisible(NULL))
  }
}

#' Check if Epibenthos cover class exceeds 10
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_logical_parameter()]. Alternatively, you can use [check_parameter_rules()].
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 100% for Total cover. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @keywords internal
#' @export
check_epibenthos_coverclass_logical <- function(data, return_df = FALSE, return_logical = FALSE) {
  lifecycle::deprecate_warn("0.1.7.9000", "check_epibenthos_coverclass_logical()", "check_logical_parameter()")

  res <- check_logical_parameter(
    data,
    param_name = "Cover class",
    condition = function(x) x > 10,
    return_df = return_df,
    return_logical = return_logical
  )

  if (is.logical(res)) return(res)

  if (!is.null(res)) {
    message("Parameter Cover class, measurement(s) > 10")
    return(res)
  } else {
    message("Parameter Cover class, measurement(s) are within 0-10")
    return(invisible(NULL))
  }
}

#' Check if Sediment deposition cover (%) exceeds 100%
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_logical_parameter()]. Alternatively, you can use [check_parameter_rules()].
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 100% for Total cover. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @keywords internal
#' @export
check_epibenthos_sedimentdepos_logical <- function(data, return_df = FALSE, return_logical = FALSE) {
  lifecycle::deprecate_warn("0.1.7.9000", "check_epibenthos_sedimentdepos_logical()", "check_logical_parameter()")

  res <- check_logical_parameter(
    data,
    param_name = "Sediment deposition cover (%)",
    condition = function(x) x > 100,
    return_df = return_df,
    return_logical = return_logical
  )

  if (is.logical(res)) return(res)

  if (!is.null(res)) {
    message("Parameter Sediment deposition cover (%), measurement(s) > 100%")
    return(res)
  } else {
    message("Parameter Sediment deposition cover (%), measurement(s) are within 0-100%")
    return(invisible(NULL))
  }
}

#' Check if Abundance class exceeds 10
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_logical_parameter()]. Alternatively, you can use [check_parameter_rules()].
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 100% for Total cover. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @keywords internal
#' @export
check_epibenthos_abundclass_logical <- function(data, return_df = FALSE, return_logical = FALSE) {
  lifecycle::deprecate_warn("0.1.7.9000", "check_epibenthos_abundclass_logical()", "check_logical_parameter()")

  res <- check_logical_parameter(
    data,
    param_name = "Abundance class",
    condition = function(x) x > 10,
    return_df = return_df,
    return_logical = return_logical
  )

  if (is.logical(res)) return(res)

  if (!is.null(res)) {
    message("Parameter Abundance class, measurement(s) > 10")
    return(res)
  } else {
    message("Parameter Abundance class, measurement(s) are within 0-10")
    return(invisible(NULL))
  }
}

#' Check logical relationship between Abundance and BQIm
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_logical_parameter()]. Alternatively, you can use [check_parameter_rules()].
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 100% for Total cover. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @keywords internal
#' @export
check_zoobenthos_BQIm_logical <- function(data, return_df = FALSE, return_logical = FALSE) {
  lifecycle::deprecate_warn("0.1.7.9000", "check_zoobenthos_BQIm_logical()", "check_logical_parameter()")

  # Required columns
  required_cols <- c("parameter", "value")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data must contain column(s): ", paste(missing_cols, collapse = ", "))
  }

  # Resolve conflicting return options
  if (return_df & return_logical) {
    warning("Both return_df and return_logical are TRUE. Ignoring return_df and returning logical vector.")
    return_df <- FALSE
  }

  # Coerce value to numeric safely
  value_num <- suppressWarnings(as.numeric(as.character(data$value)))

  # Row-wise logic (keeps the original behavior expected by your tests):
  # - Flag Abundance rows where value == 0
  # - Flag BQIm rows where value > 0
  is_abundance <- data$parameter == "Abundance"
  is_BQIm <- data$parameter == "BQIm"

  error_vec <- (!is.na(value_num) & ((is_abundance & value_num == 0) | (is_BQIm & value_num > 0)))

  # If user asked for logical vector, return it immediately
  if (return_logical) return(error_vec)

  # If any violation, prepare and return data.frame or DT::datatable
  if (any(error_vec)) {
    message("Parameter BQIm, measurement(s) > 0 when Abundance = 0")
    logical_error <- data %>%
      dplyr::filter(error_vec) %>%
      dplyr::select(dplyr::any_of(c(
        "station_name", "sample_date", "sample_id",
        "shark_sample_id_md5", "sample_min_depth_m",
        "sample_max_depth_m", "parameter", "value"
      )))

    if (return_df) {
      return(logical_error)
    } else {
      return(DT::datatable(logical_error))
    }

  } else {
    message("Parameter BQIm, measurement(s) follow logical assumption")
    invisible(NULL)
  }
}

#' Check if wet weight measurements are zero
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_logical_parameter()]. Alternatively, you can use [check_parameter_rules()].
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 100% for Total cover. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @keywords internal
#' @export
check_zoobenthos_wetweight_logical <- function(data, return_df = FALSE, return_logical = FALSE) {
  lifecycle::deprecate_warn("0.1.7.9000", "check_zoobenthos_wetweight_logical()", "check_logical_parameter()")

  res <- check_logical_parameter(
    data,
    param_name = "Wet weight",
    condition = function(x) x == 0,
    return_df = return_df,
    return_logical = return_logical
  )

  if (is.logical(res)) return(res)

  if (!is.null(res)) {
    message("Parameter Wet weight, measurement(s) violate logical assumption: should not be 0")
    return(res)
  } else {
    message("Parameter Wet weight, measurement(s) follow logical assumption: > 0")
    return(invisible(NULL))
  }
}

#' Check parameter values against logical rules
#'
#' Applies parameter-specific and row-wise logical rules to benthos/epibenthos data,
#' flagging measurements that violate defined conditions. This function replaces
#' multiple deprecated `check_*_logical()` functions with a general, flexible implementation.
#'
#' @param data A data frame containing at least the columns `parameter` and `value`.
#' @param param_conditions A named list of parameter-specific rules.
#'        Each element should be a list with:
#'        \describe{
#'          \item{condition}{Function taking a numeric vector and returning a logical vector (TRUE = violation).}
#'          \item{range_msg}{Character string describing the expected range.}
#'        }
#'        Defaults to `SHARK4R:::.param_conditions` defined in the package namespace.
#' @param rowwise_conditions A named list of row-wise rules applied across multiple parameters.
#'        Each element should be a function taking the full data frame and returning a logical vector.
#'        Defaults to `SHARK4R:::.rowwise_conditions` defined in the package namespace.
#' @param return_df Logical. If TRUE, problematic rows are returned as plain `data.frame`s.
#' @param return_logical Logical. If TRUE, problematic rows are returned as logical vectors.
#'        Overrides `return_df`.
#'
#' @return A named list of results for each parameter:
#'         \describe{
#'           \item{Logical vector}{If `return_logical = TRUE`.}
#'           \item{Data frame}{If `return_df = TRUE` and violations exist.}
#'           \item{DT datatable}{If violations exist and `return_df = FALSE`.}
#'           \item{NULL}{If no violations exist for the parameter.}
#'         }
#'         Invisible return.
#'
#' @details
#' This function evaluates each parameter in `param_conditions` and `rowwise_conditions`.
#' Only parameters present in the dataset are checked. Messages are printed
#' indicating whether values are within expected ranges or which rows violate rules.
#'
#' @examples
#' df <- data.frame(
#'   station_name = c("A1", "A2", "A3", "A4"),
#'   sample_date = as.Date("2023-05-01") + 0:3,
#'   sample_id = 101:104,
#'   parameter = c("Wet weight", "Wet weight", "Abundance", "BQIm"),
#'   value = c(0, 5, 0, 3)
#' )
#'
#' # Check against default package rules
#' check_parameter_rules(df)
#'
#' # Return problematic rows as data.frame
#' check_parameter_rules(df, return_df = TRUE)
#'
#' # Return logical vectors for each parameter
#' check_parameter_rules(df, return_logical = TRUE)
#'
#' @export
check_parameter_rules <- function(
    data,
    param_conditions = get(".param_conditions", envir = asNamespace("SHARK4R")),
    rowwise_conditions = get(".rowwise_conditions", envir = asNamespace("SHARK4R")),
    return_df = FALSE,
    return_logical = FALSE) {

  # --- Validate input ---
  required_cols <- c("parameter", "value")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data must contain column(s): ", paste(missing_cols, collapse = ", "))
  }

  if (return_df & return_logical) {
    warning("Both return_df and return_logical are TRUE. Returning logical vectors.")
    return_df <- FALSE
  }

  # --- Check if dataset contains any relevant parameters ---
  all_params <- c(names(param_conditions), names(rowwise_conditions))
  params_in_data <- intersect(all_params, unique(data$parameter))

  if (length(params_in_data) == 0) {
    message(
      "No parameters from the logical rules are present in the dataset. ",
      "Available parameters are: ", paste(names(param_conditions), collapse = ", ")
    )
    return(invisible(NULL))
  }

  results <- list()

  # --- Simple threshold-based rules ---
  for (param in names(param_conditions)) {
    if (!param %in% data$parameter) {
      results[[param]] <- NULL
      next
    }
    cond <- param_conditions[[param]]$condition
    range_msg <- param_conditions[[param]]$range_msg
    value_num <- suppressWarnings(as.numeric(as.character(data$value)))
    is_param <- data$parameter == param
    error_vec <- is_param & (!is.na(value_num) & cond(value_num))

    if (return_logical) {
      results[[param]] <- error_vec
    } else if (any(error_vec)) {
      res_df <- data[error_vec, ]
      message("Parameter ", param, ", measurement(s) outside expected range: ", range_msg)
      results[[param]] <- if (return_df) res_df else DT::datatable(res_df)
    } else {
      results[[param]] <- NULL
      message("Parameter ", param, ", all measurements within expected range: ", range_msg)
    }
  }

  # --- Row-wise dependent rules ---
  for (param in names(rowwise_conditions)) {
    relevant_params <- c(param, "Abundance") # adjust if needed
    if (!any(relevant_params %in% data$parameter)) {
      results[[param]] <- NULL
      next
    }
    cond_fun <- rowwise_conditions[[param]]
    error_vec <- cond_fun(data)

    if (return_logical) {
      results[[param]] <- error_vec
    } else if (any(error_vec)) {
      res_df <- data[error_vec, ]
      message("Parameter ", param, ", row-wise logical check failed")
      results[[param]] <- if (return_df) res_df else DT::datatable(res_df)
    } else {
      results[[param]] <- NULL
      message("Parameter ", param, ", row-wise logical check passed")
    }
  }

  invisible(results)
}
