#' Read SHARK Excel delivery files (.xls or .xlsx)
#'
#' Reads Excel files delivered to SHARK in a standardized format.
#' The function automatically detects whether the file is `.xls` or `.xlsx`
#' and reads the specified sheet, skipping a configurable number of rows.
#' Column types are automatically converted, and if a column `"SDATE"` exists,
#' it is converted to `Date`.
#'
#' @param filename Path to the Excel file to be read.
#' @param skip Minimum number of rows to skip before reading anything (column names or data).
#'   Leading empty rows are automatically skipped, so this is a lower bound.
#'   Ignored if `sheet` or `range` specifies a starting row. Default is 2.
#' @param sheet Sheet to read. Either a string (sheet name) or integer (sheet index).
#'   If neither is specified, defaults to the second sheet.
#'
#' @return A data frame containing the parsed contents of the Excel file, or `NULL` if the file
#'   does not exist, is empty, or cannot be read.
#'
#' @seealso [read_shark()] for reading SHARK tab- or semicolon-delimited export files or zip-archives.
#'
#' @examples
#' \dontrun{
#' # Read the second sheet of a .xlsx file (default)
#' df_xlsx <- read_shark_deliv("shark_delivery.xlsx")
#'
#' # Read the first sheet of a .xls file, skipping 3 rows
#' df_xls <- read_shark_deliv("shark_delivery.xls", skip = 3, sheet = 1)
#' }
#'
#' @export
read_shark_deliv <- function(filename, skip = 2, sheet = 2) {

  if (!file.exists(filename)) {
    message("ERROR: File does not exist: ", filename)
    return(NULL)
  }

  ext <- tolower(tools::file_ext(filename))

  # Choose reader based on file extension
  reader <- switch(ext,
                   xlsx = readxl::read_xlsx,
                   xls  = readxl::read_xls,
                   {
                     message("ERROR: Unsupported file extension: ", ext)
                     return(NULL)
                   }
  )

  i <- tryCatch(
    reader(filename, skip = skip, sheet = sheet,
           guess_max = 2000, col_names = TRUE, progress = FALSE),
    error = function(e) {
      message("ERROR: Could not read file: ", e$message)
      return(NULL)
    }
  )

  if (is.null(i) || ncol(i) == 0 || nrow(i) == 0) {
    message("ERROR: File is empty or not in Excel format")
    return(NULL)
  }

  # Convert columns to appropriate types
  i <- as.data.frame(lapply(i, utils::type.convert, as.is = TRUE))

  # Convert SDATE if present
  if ("SDATE" %in% names(i)) {
    i$SDATE <- as.Date(i$SDATE)
  } else {
    warning("Column 'SDATE' not found. Skipping date conversion.")
  }

  return(i)
}

#' Read SHARK export files (tab- or semicolon-delimited, plain text or zipped)
#'
#' Reads tab- or semicolon-delimited SHARK export files with standardized format.
#' The function can handle plain text files (`.txt`) or zip archives (`.zip`) containing
#' a file named `shark_data.txt`. It automatically detects and converts column types
#' and can optionally coerce the `"value"` column to numeric. The `"sample_date"` column
#' is converted to `Date` if it exists.
#'
#' This function is robust to file encoding issues. By default (`guess_encoding = TRUE`),
#' it attempts to automatically detect the file encoding and will use it if it differs
#' from the user-specified `encoding`. Automatic detection can be disabled.
#'
#' @param filename Path to the SHARK export file. Can be a `.txt` or `.zip` file.
#'   If a zip file is provided, it should contain a file named `shark_data.txt`.
#' @param delimiters Character. Specifies the delimiter used in the file. Options:
#'   `"point-tab"` (tab-separated, default) or `"point-semi"` (semicolon-separated).
#' @param encoding Character. File encoding. Options: `"cp1252"`, `"utf_8"`, `"utf_16"`, `"latin_1"`.
#'   Default is `"utf_8"`. If `guess_encoding = TRUE`, detected encoding overrides this value.
#' @param guess_encoding Logical. If `TRUE` (default), automatically detect file encoding.
#'   If `FALSE`, the function uses only the user-specified encoding.
#' @param value_numeric Logical. If `TRUE` (default), attempts to convert the `"value"` column
#'   to numeric. If `FALSE`, leaves `"value"` as-is.
#'
#' @return A data frame containing the parsed contents of the SHARK export file,
#'   or `NULL` if the file is empty or could not be read.
#'
#' @seealso [read_shark_deliv()] for reading SHARK Excel delivery files (`.xls`/`.xlsx`).
#'
#' @examples
#' \dontrun{
#' # Read a plain text SHARK export
#' df_txt <- read_shark("sharkweb_data.txt")
#'
#' # Read a SHARK export from a zip archive
#' df_zip <- read_shark("shark_data.zip")
#'
#' # Read with explicit encoding and do not convert value
#' df_custom <- read_shark("shark_data.txt",
#'                         encoding = "latin_1",
#'                         guess_encoding = FALSE,
#'                         value_numeric = FALSE)
#' }
#'
#' @export
read_shark <- function(filename,
                       delimiters = "point-tab",
                       encoding = "utf_8",
                       guess_encoding = TRUE,
                       value_numeric = TRUE) {

  # Map encodings
  encoding_map <- c(
    cp1252  = "WINDOWS-1252",
    utf_8   = "UTF-8",
    utf_16  = "UTF-16",
    latin_1 = "ISO-8859-1"
  )

  if (!encoding %in% names(encoding_map)) {
    warning("'encoding' must be one of 'cp1252', 'utf_8', 'utf_16', or 'latin_1'. Defaulting to 'utf_8'.")
    encoding <- "utf_8"
  }

  sep_char <- switch(
    delimiters,
    "point-tab"  = "\t",
    "point-semi" = ";",
    { warning("Invalid 'delimiters'. Defaulting to 'point-tab'."); "\t" }
  )

  # Decide if file is zip or not
  is_zip <- grepl("\\.zip$", filename, ignore.case = TRUE)

  # Guess encoding only if enabled
  if (guess_encoding) {
    if (is_zip) {
      con <- unz(description = filename, filename = "shark_data.txt", open = "rb")
      raw_sample <- readBin(con, what = "raw", n = 5000)
      close(con)
    } else {
      raw_sample <- readBin(filename, what = "raw", n = 5000)
    }

    detected <- stringi::stri_enc_detect(raw_sample)[[1]]
    best_guess <- normalize_encoding(detected$Encoding[1])

    if (!is.null(best_guess) && best_guess != encoding) {
      message(sprintf("Detected encoding '%s' differs from specified '%s'. Using detected encoding.",
                      detected$Encoding[1], encoding))
      encoding <- best_guess
    }
  }

  content_encoding <- encoding_map[[encoding]]

  # Actual read
  if (is_zip) {
    input <- unz(description = filename, filename = "shark_data.txt")
  } else {
    input <- filename
  }

  i <- readr::read_delim(
    input,
    delim = sep_char,
    guess_max = 2000,
    col_names = TRUE,
    locale = readr::locale(encoding = content_encoding),
    progress = FALSE,
    col_types = readr::cols()
  )

  # Type conversions
  i <- i %>%
    dplyr::mutate(dplyr::across(everything(), ~ utils::type.convert(., as.is = TRUE)))

  if ("sample_date" %in% names(i)) {
    i <- i %>% dplyr::mutate(dplyr::across("sample_date", as.Date))
  }

  if (value_numeric && "value" %in% names(i)) {
    i <- i %>% dplyr::mutate(suppressWarnings(dplyr::across("value", as.numeric)))
  }

  if (nrow(i) > 0) {
    return(i)
  } else {
    msg <- if (is_zip) "Zip archive is empty or invalid" else "File is empty or invalid"
    message("ERROR: ", msg)
    return(NULL)
  }
}

#' Read .xlsx files delivered to SHARK
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [read_shark_deliv()].
#'
#' Uses readxl to read excel files with standardized delivery format
#' @param filename path to file to be read
#' @param skip Minimum number of rows to skip before reading anything, be it column names or data. Leading empty rows are automatically skipped, so this is a lower bound. Ignored if range is given. Default is 2.
#' @param sheet Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). Ignored if the sheet is specified via range. If neither argument specifies the sheet, defaults to the second sheet.
#' @return Data frame of file
#' @keywords internal
#' @export
shark_read_deliv <- function(filename, skip = 2, sheet = 2) {
  lifecycle::deprecate_warn("1.0.0", "shark_read_deliv()", "read_shark_deliv()")
  read_shark_deliv(filename = filename, skip = skip, sheet = sheet)
}

#' Read .xls files delivered to SHARK
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated and has been replaced by [read_shark_deliv()].
#'
#' Uses readxl to read excel files with standardized delivery format
#' @param filename path to file to be read
#' @param skip Minimum number of rows to skip before reading anything, be it column names or data. Leading empty rows are automatically skipped, so this is a lower bound. Ignored if range is given. Default is 2.
#' @param sheet Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). Ignored if the sheet is specified via range. If neither argument specifies the sheet, defaults to the second sheet.
#' @return Data frame of file
#' @keywords internal
#' @export
shark_read_deliv_xls <- function(filename, skip = 2, sheet = 2) {
  lifecycle::deprecate_warn("1.0.0", "shark_read_deliv_xls()", "read_shark_deliv()")
  read_shark_deliv(filename = filename, skip = skip, sheet = sheet)
}

#' Read tab delimited files downloaded from SHARK
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [read_shark()].
#'
#' Uses `read_delim()` to read tab-delimited or semicolon-delimited files
#' with standardized export format from SHARK.
#'
#' This function is robust to encoding issues:
#' it accepts a user-specified encoding (`cp1252`, `utf_8`, `utf_16`, or `latin_1`)
#' but also attempts to automatically detect the file encoding.
#' If the detected encoding differs from the specified one,
#' the detected encoding will be used instead.
#' This helps in cases where the file encoding has been wrongly specified,
#' mislabeled, or varies between SHARK exports.
#'
#' @param filename Path to file to be read.
#' @param delimiters Character. Specifies the delimiter used to separate values in `filename`.
#'   Options are `"point-tab"` (tab-separated) or `"point-semi"` (semicolon-separated).
#'   Default is `"point-tab"`.
#' @param encoding Character. Specifies the text encoding of `filename`.
#'   Options are `"cp1252"`, `"utf_8"`, `"utf_16"`, or `"latin_1"`.
#'   Default is `"utf_8"`. If an encoding mismatch is detected, the detected encoding is used.
#' @return A data frame containing the parsed contents of the SHARK export file.
#' @keywords internal
#' @export
shark_read <- function(filename, delimiters = "point-tab", encoding = "latin_1") {
  lifecycle::deprecate_warn("1.0.0", "shark_read_zip()", "read_shark()")
  read_shark(filename = filename, delimiters = delimiters, encoding = encoding)
}

#' Read zip archive and unzip tab delimited files downloaded from SHARK
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [read_shark()].
#'
#' Uses `unz()` and `read_delim()` to extract and read tab-delimited or
#' semicolon-delimited files with standardized export format from SHARK.
#'
#' Like [shark_read()], this function is tolerant to encoding issues.
#' It allows a user-specified encoding (`cp1252`, `utf_8`, `utf_16`, or `latin_1`),
#' but also automatically detects the encoding from the file content.
#' If the detected encoding does not match the specified one,
#' the detected encoding is preferred.
#' This ensures files with wrongly labeled or inconsistent encodings are still read correctly.
#'
#' @param zipname Path to the zip archive containing SHARK data (expects a file named `shark_data.txt` inside).
#' @param delimiters Character. Specifies the delimiter used to separate values in the file.
#'   Options are `"point-tab"` (tab-separated) or `"point-semi"` (semicolon-separated).
#'   Default is `"point-tab"`.
#' @param encoding Character. Specifies the text encoding of the file.
#'   Options are `"cp1252"`, `"utf_8"`, `"utf_16"`, or `"latin_1"`.
#'   Default is `"utf_8"`. If encoding mismatch is detected, the detected encoding is used.
#' @return A data frame containing the parsed contents of the SHARK export file.
#' @keywords internal
#' @export
shark_read_zip <- function(zipname, delimiters = "point-tab", encoding = "latin_1") {
  lifecycle::deprecate_warn("1.0.0", "shark_read_zip()", "read_shark()")
  read_shark(filename = zipname, delimiters = delimiters, encoding = encoding)
}
