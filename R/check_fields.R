#' Validate SHARK system fields in a data frame
#'
#' This function checks whether the required and recommended global and
#' datatype-specific SHARK system fields are present in a data frame.
#'
#' - **Required fields**: Missing or empty required fields are reported as **errors**.
#' - **Recommended fields**: Missing or empty recommended fields are reported as **warnings**,
#'   but only if `level = "warning"` is specified.
#'
#' @param data A `data.frame` or `tibble` containing SHARK data to validate.
#' @param level Character. The level of validation:
#'   - `"error"` (default) — checks only required fields.
#'   - `"warning"` — checks both required and recommended fields.
#'
#' @return A `tibble` summarizing missing or empty fields, with columns:
#'   - `level`: `"error"` or `"warning"`.
#'   - `field`: Name of the missing or empty field.
#'   - `row`: Row number where the value is missing (NA) or `NA` if the whole column is missing.
#'   - `message`: Description of the issue.
#'
#' @examples
#' # Example with required fields missing
#' df <- data.frame(
#'   visit_year = 2024,
#'   station_name = NA
#' )
#' check_datatype(df, level = "error")
#'
#' # Example checking recommended fields as warnings
#' check_datatype(df, level = "warning")
#'
#' @export
check_datatype <- function(data, level = "error") {

  errors <- tibble()
  required <- c("visit_year", "station_name", "sample_project_name_en", "sample_orderer_name_en", "platform_code", "sample_date", "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code", "water_depth_m")
  recommended <- c("monitoring_station_type_code", "monitoring_purpose_code", "monitoring_program_code", "reporting_institute_name_en", "analytical_laboratory_name_en")

  # find missing required fields
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, tibble(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  } else {
    message("All required fields present")
  }

  # find empty values for required fields
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[[field]])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, tibble(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }

  # recommended fields
  if (level == "warning") {

    # find missing recommended fields
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, tibble(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    else {
      message("All recommended fields present")
    }

    # find empty values for recommended fields
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[[field]])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, tibble(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
  }

  return(errors)
}

#' Validate SHARK data fields for a given datatype
#'
#' This function checks a SHARK data frame against the required and recommended
#' fields defined for a specific datatype. It verifies that all required fields
#' are present and contain non-empty values. If \code{level = "warning"}, it
#' also checks for recommended fields and empty values within them.
#'
#' Note: A single "*" marks required fields in the standard SHARK template.
#' A double "**" is often used to specify columns required for **national monitoring only**.
#' For more information, see:
#' https://www.smhi.se/data/hav-och-havsmiljo/datavardskap-oceanografi-och-marinbiologi/leverera-data
#'
#' @param data A data frame containing SHARK data to be validated.
#' @param datatype A string giving the SHARK datatype to validate against.
#'   Must exist as a name in the provided \code{field_definitions}.
#' @param level Character string, either \code{"error"} or \code{"warning"}.
#'   If \code{"error"}, only required fields are validated. If \code{"warning"},
#'   recommended fields are also checked and reported as warnings.
#' @param field_definitions A named list of field definitions. Each element
#'   should contain two character vectors: \code{required} and \code{recommended}.
#'   Defaults to the package's built-in \code{SHARK4R:::.field_definitions}.
#'   Alternatively, the latest definitions can be loaded directly from the
#'   official SHARK4R GitHub repository using
#'   \code{\link{load_shark4r_fields}()}.
#' @param stars Integer. Maximum number of "*" levels to include.
#'   Default = 1 (only single "*").
#'   For example, `stars = 2` includes "*" and "**",
#'   `stars = 3` includes "*", "**", and "***".
#' @param bacterioplankton_subtype Character. For "Bacterioplankton" only: either
#'   "abundance" (default) or "production". Ignored for other datatypes.
#'
#' @details
#' Field definitions for SHARK data can be loaded in two ways:
#' \enumerate{
#'   \item **From the SHARK4R package bundle (default):**
#'     The package contains a built-in object, \code{.field_definitions},
#'     which stores required and recommended fields for each datatype.
#'
#'   \item **From GitHub (latest official version):**
#'     To use the most up-to-date field definitions, you can load them directly from the
#'     \href{https://github.com/nodc-sweden/SHARK4R-statistics}{SHARK4R-statistics} repository:
#'     \preformatted{
#'     defs <- load_shark4r_fields()
#'     check_fields(my_data, "Phytoplankton", field_definitions = defs)
#'     }
#' }
#'
#' **Delivery-format (all-caps) data:**
#' If the column names in \code{data} are all uppercase (e.g. SDATE), \code{check_fields()} assumes
#' the dataset follows the official SHARK delivery template. In this case:
#' \itemize{
#'   \item Required fields are determined from the delivery template using
#'         \code{\link{get_delivery_template}()} and \code{\link{find_required_fields}()}.
#'   \item Recommended fields are ignored because the delivery templates do not define them.
#'   \item The function validates that all required columns exist and contain non-empty values.
#' }
#' This ensures that both internal `SHARK4R` datasets (with camelCase or snake_case columns)
#' and official delivery files (ALL_CAPS columns) are validated correctly using the appropriate rules.
#'
#' Stars in the template
#'
#' Leading asterisks in the delivery template indicate required levels:
#'
#' \itemize{
#'   \item \emph{*} = standard required column
#'   \item * = required for national monitoring
#'   \item Other symbols = additional requirement level
#' }
#'
#' The \code{stars} parameter in \code{check_fields()} controls how many levels of required
#' columns to include.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{level}{Either \code{"error"} or \code{"warning"}.}
#'   \item{field}{The name of the field that triggered the check.}
#'   \item{row}{Row number(s) in \code{data} where the issue occurred, or \code{NA}
#'     if the whole field is missing.}
#'   \item{message}{A descriptive message explaining the problem.}
#' }
#' The tibble will be empty if no problems are found.
#'
#' @seealso
#' \code{\link{load_shark4r_fields}} for fetching the latest field definitions from GitHub,
#' \code{\link{get_delivery_template}} for downloading delivery templates from SMHI's website.
#'
#' @examples
#' # Example 1: Using built-in field definitions for "Phytoplankton"
#' df_phyto <- data.frame(
#'   visit_date = "2023-06-01",
#'   sample_id = "S1",
#'   scientific_name = "Skeletonema marinoi",
#'   value = 123
#' )
#'
#' # Check fields
#' check_fields(df_phyto, "Phytoplankton", level = "warning")
#'
#' \donttest{
#' # Example 2: Load latest definitions from GitHub and use them
#' defs <- load_shark4r_fields(verbose = FALSE)
#'
#' # Check fields using loaded field definitions
#' check_fields(df_phyto, "Phytoplankton", field_definitions = defs)
#' }
#'
#' # Example 3: Custom datatype with required + recommended fields
#' defs <- list(
#'   ExampleType = list(
#'     required = c("id", "value"),
#'     recommended = "comment"
#'   )
#' )
#'
#' # Example data
#' df_ok <- data.frame(id = 1, value = "x", comment = "ok")
#'
#' # Check fields using custom field definitions
#' check_fields(df_ok, "ExampleType", level = "warning", field_definitions = defs)
#'
#' @export
check_fields <- function(data, datatype, level = "error", stars = 1,
                         bacterioplankton_subtype = "abundance", field_definitions = .field_definitions) {

  if (!datatype %in% names(field_definitions) && !grepl("^deliv_", datatype)) {
    stop("Unknown datatype: ", datatype)
  }

  all_caps <- all(names(data) == toupper(names(data)))

  if (all_caps) {
    required <- find_required_fields(datatype,
                                     stars = stars,
                                     bacterioplankton_subtype = bacterioplankton_subtype)

    defs <- list(
      required = required,
      recommended = c() # No recommended fields defined for all-caps datasets
    )
  } else {
    defs <- field_definitions[[datatype]]
  }

  required <- defs$required
  recommended <- defs$recommended

  errors <- tibble()

  # ---- Required fields ----
  missing <- missing_fields(data, required)
  if (length(missing)) {
    errors <- bind_rows(errors,
                        tibble(level = "error",
                               field = missing,
                               row = NA,
                               message = paste0("Required field ", missing, " is missing")))
  }

  for (field in intersect(required, names(data))) {
    empty <- missing_values(data[[field]])
    if (any(empty)) {
      errors <- bind_rows(errors,
                          tibble(level = "error",
                                 field = field,
                                 row = which(empty),
                                 message = paste0("Empty value for required field ", field)))
    }
  }

  # ---- Recommended fields ----
  if (level == "warning") {
    missing <- missing_fields(data, recommended)
    if (length(missing)) {
      errors <- bind_rows(errors,
                          tibble(level = "warning",
                                 field = missing,
                                 row = NA,
                                 message = paste0("Recommended field ", missing, " is missing")))
    }

    for (field in intersect(recommended, names(data))) {
      empty <- missing_values(data[[field]])
      if (any(empty)) {
        errors <- bind_rows(errors,
                            tibble(level = "warning",
                                   field = field,
                                   row = which(empty),
                                   message = paste0("Empty value for recommended field ", field)))
      }
    }
  }

  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_Bacterioplankton <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_Bacterioplankton()", "check_fields()")
  errors <- check_fields(data, "Bacterioplankton", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_Chlorophyll <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_Chlorophyll()", "check_fields()")
  errors <- check_fields(data, "Chlorophyll", level = level)
  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_Epibenthos <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_Epibenthos()", "check_fields()")
  errors <- check_fields(data, "Epibenthos", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_EpibenthosDropvideo <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_EpibenthosDropvideo()", "check_fields()")
  errors <- check_fields(data, "EpibenthosDropvideo", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_GreySeal <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_GreySeal()", "check_fields()")
  errors <- check_fields(data, "GreySeal", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_HarbourPorpoise <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_HarbourPorpoise()", "check_fields()")
  errors <- check_fields(data, "HarbourPorpoise", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_HarbourSeal <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_HarbourSeal()", "check_fields()")
  errors <- check_fields(data, "HarbourSeal", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_PhysicalChemical <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_PhysicalChemical()", "check_fields()")
  errors <- check_fields(data, "PhysicalChemical", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_Phytoplankton <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_Phytoplankton()", "check_fields()")
  errors <- check_fields(data, "Phytoplankton", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_Picoplankton <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_Picoplankton()", "check_fields()")
  errors <- check_fields(data, "Picoplankton", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_PrimaryProduction <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_PrimaryProduction()", "check_fields()")
  errors <- check_fields(data, "PrimaryProduction", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_RingedSeal <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_RingedSeal()", "check_fields()")
  errors <- check_fields(data, "RingedSeal", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_SealPathology <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_SealPathology()", "check_fields()")
  errors <- check_fields(data, "SealPathology", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_Sedimentation <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_Sedimentation()", "check_fields()")
  errors <- check_fields(data, "Sedimentation", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_Zoobenthos <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_Zoobenthos()", "check_fields()")
  errors <- check_fields(data, "Zoobenthos", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_Zooplankton <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_Zooplankton()", "check_fields()")
  errors <- check_fields(data, "Zooplankton", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_Bacterioplankton <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_Bacterioplankton()", "check_fields()")
  errors <- check_fields(data, "deliv_Bacterioplankton", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_Chlorophyll <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_Chlorophyll()", "check_fields()")
  errors <- check_fields(data, "deliv_Chlorophyll", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_Epibenthos <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_Epibenthos()", "check_fields()")
  errors <- check_fields(data, "deliv_Epibenthos", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_EpibenthosDropvideo <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_EpibenthosDropvideo()", "check_fields()")
  errors <- check_fields(data, "deliv_EpibenthosDropvideo", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_GreySeal <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_GreySeal()", "check_fields()")
  errors <- check_fields(data, "deliv_GreySeal", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_HarbourPorpoise <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_HarbourPorpoise()", "check_fields()")
  errors <- check_fields(data, "deliv_HarbourPorpoise", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_HarbourSeal <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_HarbourSeal()", "check_fields()")
  errors <- check_fields(data, "deliv_HarbourSeal", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_PhysicalChemical <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_PhysicalChemical()", "check_fields()")
  errors <- check_fields(data, "deliv_PhysicalChemical", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_Phytoplankton <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_Phytoplankton()", "check_fields()")
  errors <- check_fields(data, "deliv_Phytoplankton", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_Picoplankton <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_Picoplankton()", "check_fields()")
  errors <- check_fields(data, "deliv_Picoplankton", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_PrimaryProduction <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_PrimaryProduction()", "check_fields()")
  errors <- check_fields(data, "deliv_PrimaryProduction", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_RingedSeal <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_RingedSeal()", "check_fields()")
  errors <- check_fields(data, "deliv_RingedSeal", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_SealPathology <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_SealPathology()", "check_fields()")
  errors <- check_fields(data, "deliv_SealPathology", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_Sedimentation <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_Sedimentation()", "check_fields()")
  errors <- check_fields(data, "deliv_Sedimentation", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_Zoobenthos <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_Zoobenthos()", "check_fields()")
  errors <- check_fields(data, "deliv_Zoobenthos", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
#' Check if the required and recommended datatype-specific SHARK system fields are present
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_fields()].
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' @keywords internal
check_deliv_Zooplankton <- function(data, level = "error") {
  lifecycle::deprecate_warn("1.0.0", "check_deliv_Zooplankton()", "check_fields()")
  errors <- check_fields(data, "deliv_Zooplankton", level = level)

  if (nrow(errors) == 0) {
    message("All required fields present")
    if (level == "warning") {
      message("All recommended fields present")
    }
  }

  return(errors)
}
