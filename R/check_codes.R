#' Get SHARK codelist from SMHI
#'
#' This function downloads the SHARK codes Excel file from SMHI (if not already cached)
#' and reads it into R. The file is stored in a persistent cache directory
#' so it does not need to be downloaded again in subsequent sessions.
#'
#' @param url Character string with the URL to the SHARK codes Excel file.
#'   Defaults to the official SMHI codelist.
#' @param sheet Sheet to read. Can be either the sheet name or its index
#'   (default is `1`).
#' @param skip Number of rows to skip before reading data
#'   (default is `1`, to skip the header row).
#' @param force Logical; if `TRUE`, forces re-download of the Excel file
#'   even if a cached copy is available. Default is `FALSE`.
#' @param clean_cache_days Numeric; if not `NULL`, cached SHARK code Excel files older than
#'   this number of days will be automatically deleted. Defaults to 30. Set to `NULL`
#'   to disable automatic cleanup.
#'
#' @return A `tibble` containing the contents of the requested sheet.
#' @export
#'
#' @seealso [clean_shark4r_cache()] to manually clear cached files.
#'
#' @examples
#' \donttest{
#'   # Read the first sheet, skipping the first row
#'   codes <- get_shark_codes()
#'   head(codes)
#'
#'   # Force re-download of the Excel file
#'   codes <- get_shark_codes(force = TRUE)
#' }
get_shark_codes <- function(url = "https://smhi.se/oceanografi/oce_info_data/shark_web/downloads/codelist_SMHI.xlsx",
                            sheet = 1,
                            skip = 1,
                            force = FALSE,
                            clean_cache_days = 30) {
  # Optional: remove old cached SHARK code files
  if (!is.null(clean_cache_days) && clean_cache_days > 0) {
    cache_dir <- file.path(tools::R_user_dir("SHARK4R", "cache"), "perm")
    if (dir.exists(cache_dir)) {
      shark_files <- list.files(cache_dir, pattern = "codelist_SMHI\\.xlsx$", full.names = TRUE)
      old_files <- shark_files[file.info(shark_files)$mtime < Sys.time() - clean_cache_days*24*60*60]
      if (length(old_files) > 0) unlink(old_files, force = TRUE)
    }
  }

  cache_file <- cache_excel_download(url = url, force = force)
  readxl::read_excel(cache_file, sheet = sheet, skip = skip, guess_max = 2000, progress = FALSE)
}

#' Check matches of reported codes in SMHI's SHARK codelist
#'
#' This function checks whether the codes reported in a specified column of a
#' dataset (e.g., project codes, ship codes, etc.) are present in the
#' official SHARK codelist provided by SMHI. If a cell contains multiple codes
#' separated by commas, each code is checked individually. The function downloads
#' and caches the codelist if necessary, compares the reported values against
#' the valid codes, and returns a `tibble` showing which codes matched.
#' Informative messages are printed if unmatched codes are found.
#'
#' @param data A tibble (or data.frame) containing the codes to check.
#' @param field Character; name of the column in `data` that contains the codes
#'   to be validated against the SHARK codelist. If a cell contains multiple
#'   codes separated by commas, each code is validated separately.
#'   Default is `"sample_project_name_en"`.
#' @param code_type Character; the type of code to check (e.g., `"PROJ"`).
#'   Defaults to `"PROJ"`.
#' @param match_column Character; the column in the SHARK codelist to match
#'   against. Must be one of `"Code"` or `"Description/English translate"`. Defaults to
#'   `"Description/English translate"`.
#' @param clean_cache_days Numeric; if not `NULL`, cached SHARK code Excel files
#'   older than this number of days will be automatically deleted and replaced
#'   by a new download. Defaults to 30. Set to `NULL` to disable automatic cleanup.
#' @param verbose Logical. If TRUE, messages will be displayed during execution. Defaults to TRUE.
#'
#' @seealso [get_shark_codes()] to get the current code list.
#' @seealso [clean_shark4r_cache()] to manually clear cached files.
#'
#' @return A `tibble` with unique reported codes (after splitting comma-separated
#'   entries) and a logical column `match_type` indicating if they exist in the
#'   SHARK codelist.
#' @export
check_codes <- function(data,
                        field = "sample_project_name_en",
                        code_type = "PROJ",
                        match_column = "Description/English translate",
                        clean_cache_days = 30,
                        verbose = TRUE) {
  # validate field in data
  if (!field %in% names(data)) {
    stop(sprintf("Field '%s' not found in data.", field))
  }

  # validate match_column
  valid_columns <- c("Code", "Description/English translate")
  if (!match_column %in% valid_columns) {
    stop(sprintf("Invalid match_column '%s'. Must be one of: %s",
                 match_column, paste(valid_columns, collapse = ", ")))
  }

  # split codes by comma and trim whitespace
  to_match <- data[[field]] %>%
    strsplit(",") %>%
    unlist() %>%
    trimws() %>%
    unique()

  # fetch codes
  shark_codes <- get_shark_codes(clean_cache_days = clean_cache_days)
  valid_codes <- shark_codes %>%
    dplyr::filter(Data_field == code_type) %>%
    dplyr::pull(match_column) %>%
    unique()

  # match check
  match_type <- to_match %in% valid_codes
  matches <- dplyr::tibble(
    reported_code = to_match,
    match_type = match_type
  )

  # message
  if (verbose) {
    if (any(!match_type)) {
      message(sprintf("ERROR: Unmatched %s code(s) found", code_type))
      print(dplyr::filter(matches, !match_type))
    } else {
      message(sprintf("All %s codes found", code_type))
    }
  }

  return(matches)
}

#' Check matches of reported codes in SMHI's SHARK codelist
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_codes()].
#'
#' @param data for tibble be be checked
#' @param field Character; name of the column in `data` that contains the codes
#'   to be validated against the SHARK codelist. If a cell contains multiple
#'   codes separated by commas, each code is validated separately.
#'   Default is `"sample_project_name_en"`.
#' @param clean_cache_days Numeric; if not NULL, cached SHARK code Excel files older than
#' this number of days will be automatically deleted and be replaced by a new download.
#' Defaults to 30. Set to NULL to disable automatic cleanup.
#' @seealso [get_shark_codes()] to get the current code list.
#' @return unmatched codes with true or false results
#' @export
#'
#' @keywords internal
#' @export
check_code_proj <- function(data, field = "sample_project_name_sv", clean_cache_days = 30) {
  lifecycle::deprecate_warn("1.0.0", "check_code_proj()", "check_codes()")
  check_codes(data = data,
              field = field,
              code_type = "PROJ",
              clean_cache_days = clean_cache_days)
}
