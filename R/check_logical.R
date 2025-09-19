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

#' Check if Epibenthos total cover exceeds 100%
#'
#' @usage check_epibenthos_totcover_logical(data, return_df = FALSE, return_logical = FALSE)
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 100% for Total cover. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @export
check_epibenthos_totcover_logical <- function(data, return_df = FALSE, return_logical = FALSE) {

  if(return_df & return_logical) {
    warning("Both return_df and return_logical are TRUE. Ignoring return_df and returning logical vector.")
    return_df <- FALSE
  }

  # Required columns
  required_cols <- c("parameter", "value")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data must contain column(s): ", paste(missing_cols, collapse = ", "))
  }

  # Coerce value to numeric safely
  value_num <- suppressWarnings(as.numeric(as.character(data$value)))

  # Identify rows of the parameter exceeding 100%
  is_totcover <- data$parameter == "Total cover of all species (%)"
  error_vec <- is_totcover & (!is.na(value_num) & value_num > 100)

  if (return_logical) return(error_vec)

  if (any(error_vec)) {
    message("Parameter Total cover of all species (%), measurement(s) exceed 100%")
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
    message("Parameter Total cover of all species (%), measurement(s) are within 0-100%")
    invisible(NULL)
  }
}
#' Check if Epibenthos cover (%) exceeds 100%
#'
#' @usage check_epibenthos_coverpercent_logical(data, return_df = FALSE, return_logical = FALSE)
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 100% for Cover (%). Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @export
check_epibenthos_coverpercent_logical <- function(data, return_df = FALSE, return_logical = FALSE) {

  if(return_df & return_logical) {
    warning("Both return_df and return_logical are TRUE. Ignoring return_df and returning logical vector.")
    return_df <- FALSE
  }

  # Required columns
  required_cols <- c("parameter", "value")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data must contain column(s): ", paste(missing_cols, collapse = ", "))
  }

  # Coerce value to numeric safely
  value_num <- suppressWarnings(as.numeric(as.character(data$value)))

  # Identify rows with parameter "Cover (%)" exceeding 100%
  is_cover <- data$parameter == "Cover (%)"
  error_vec <- is_cover & (!is.na(value_num) & value_num > 100)

  if (return_logical) return(error_vec)

  if (any(error_vec)) {
    message("Parameter Cover (%), measurement(s) exceed 100%")
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
    message("Parameter Cover (%), measurement(s) are within 0-100%")
    invisible(NULL)
  }
}
#' Check if Epibenthos cover exceeds 100%
#'
#' @usage check_epibenthos_cover_logical(data, return_df = FALSE, return_logical = FALSE)
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 100% for Cover. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @export
check_epibenthos_cover_logical <- function(data, return_df = FALSE, return_logical = FALSE) {

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

  # Identify rows with parameter "Cover" exceeding 100%
  is_cover <- data$parameter == "Cover"
  error_vec <- is_cover & (!is.na(value_num) & value_num > 100)

  if (return_logical) return(error_vec)

  if (any(error_vec)) {
    message("Parameter Cover, measurement(s) exceed 100%")
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
    message("Parameter Cover, measurement(s) are within 0-100%")
    invisible(NULL)
  }
}
#' Check if Epibenthos cover class exceeds 10
#'
#' @usage check_epibenthos_coverclass_logical(data, return_df = FALSE, return_logical = FALSE)
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 10 for Cover class. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @export
check_epibenthos_coverclass_logical <- function(data, return_df = FALSE, return_logical = FALSE) {

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

  # Identify rows with parameter "Cover class" exceeding 10
  is_coverclass <- data$parameter == "Cover class"
  error_vec <- is_coverclass & (!is.na(value_num) & value_num > 10)

  if (return_logical) return(error_vec)

  if (any(error_vec)) {
    message("Parameter Cover class, measurement(s) exceed 10")
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
    message("Parameter Cover class, measurement(s) are within 0-10")
    invisible(NULL)
  }
}
#' Check if Sediment deposition cover (%) exceeds 100%
#'
#' @usage check_epibenthos_sedimentdepos_logical(data, return_df = FALSE, return_logical = FALSE)
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 100% for Sediment deposition cover (%). Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @export
check_epibenthos_sedimentdepos_logical <- function(data, return_df = FALSE, return_logical = FALSE) {

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

  # Identify rows with parameter "Sediment deposition cover (%)" exceeding 100%
  is_sediment <- data$parameter == "Sediment deposition cover (%)"
  error_vec <- is_sediment & (!is.na(value_num) & value_num > 100)

  if (return_logical) return(error_vec)

  if (any(error_vec)) {
    message("Parameter Sediment deposition cover (%), measurement(s) exceed 100%")
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
    message("Parameter Sediment deposition cover (%), measurement(s) are within 0-100%")
    invisible(NULL)
  }
}

#' Check if Abundance class exceeds 10
#'
#' @usage check_epibenthos_abundclass_logical(data, return_df = FALSE, return_logical = FALSE)
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows exceed 10 for Abundance class. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @export
check_epibenthos_abundclass_logical <- function(data, return_df = FALSE, return_logical = FALSE) {

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

  # Identify rows with parameter "Abundance class" exceeding 10
  is_abundclass <- data$parameter == "Abundance class"
  error_vec <- is_abundclass & (!is.na(value_num) & value_num > 10)

  if (return_logical) return(error_vec)

  if (any(error_vec)) {
    message("Parameter Abundance class, measurement(s) exceed 10")
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
    message("Parameter Abundance class, measurement(s) are within 0-10")
    invisible(NULL)
  }
}

#' Check logical relationship between Abundance and BQIm
#'
#' @usage check_zoobenthos_BQIm_logical(data, return_df = FALSE, return_logical = FALSE)
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows violate the logical assumption. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @export
check_zoobenthos_BQIm_logical <- function(data, return_df = FALSE, return_logical = FALSE) {

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

  # Create logical vectors for parameters
  is_abundance <- data$parameter == "Abundance"
  is_BQIm <- data$parameter == "BQIm"

  # Identify rows violating the logical assumption: Abundance == 0 & BQIm > 0
  error_vec <- (!is.na(value_num) & ((is_abundance & value_num == 0) | (is_BQIm & value_num > 0)))

  # More strict: only flag BQIm > 0 when Abundance == 0 in same row (if structure allows it)
  # For simplicity, we keep row-wise logical vector
  if (return_logical) return(error_vec)

  if (any(error_vec)) {
    message("Parameter BQIm, measurement(s) violate logical assumption: Abundance == 0 then BQIm should <= 0")

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
#' @usage check_zoobenthos_wetweight_logical(data, return_df = FALSE, return_logical = FALSE)
#'
#' @param data A data frame. Must contain columns `parameter` and `value`.
#' @param return_df Logical. If TRUE, return a plain data.frame of problematic rows.
#' @param return_logical Logical. If TRUE, return a logical vector of length nrow(data)
#'        indicating which rows have Wet weight == 0. Overrides return_df.
#' @return A DT datatable, a data.frame, a logical vector, or NULL if no problems found.
#' @export
check_zoobenthos_wetweight_logical <- function(data, return_df = FALSE, return_logical = FALSE) {

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

  # Identify rows where parameter "Wet weight" is zero
  is_wetweight <- data$parameter == "Wet weight"
  error_vec <- is_wetweight & (!is.na(value_num) & value_num == 0)

  if (return_logical) return(error_vec)

  if (any(error_vec)) {
    message("Parameter Wet weight, measurement(s) violate logical assumption: should not be 0")

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
    message("Parameter Wet weight, measurement(s) follow logical assumption: > 0")
    invisible(NULL)
  }
}
