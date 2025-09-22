#' Read .xlsx files delivered to SHARK
#'
#' Uses readxl to read excel files with standardized delivery format
#' @param filename path to file to be read
#' @param skip Minimum number of rows to skip before reading anything, be it column names or data. Leading empty rows are automatically skipped, so this is a lower bound. Ignored if range is given. Default is 2.
#' @param sheet Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). Ignored if the sheet is specified via range. If neither argument specifies the sheet, defaults to the second sheet.
#' @return Data frame of file
#' @export

shark_read_deliv <- function(filename, skip = 2, sheet = 2) {

  i <- read_xlsx(filename, skip = skip, sheet = sheet, guess_max = 2000, col_names = T, progress = FALSE)

  i <- i %>%
    mutate(across(everything(), ~ type.convert(., as.is = TRUE))) %>%
    #mutate_if(is.factor, as.character) %>%
    mutate(SDATE = as.Date(SDATE))

  if (length(i) > 0) {
    return(i)
  }
  else {
    message("ERROR: File is empty or not in excel format")
  }
}

#' Read .xls files delivered to SHARK
#'
#' Uses readxl to read excel files with standardized delivery format
#' @param filename path to file to be read
#' @param skip Minimum number of rows to skip before reading anything, be it column names or data. Leading empty rows are automatically skipped, so this is a lower bound. Ignored if range is given. Default is 2.
#' @param sheet Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). Ignored if the sheet is specified via range. If neither argument specifies the sheet, defaults to the second sheet.
#' @return Data frame of file
#' @export

shark_read_deliv_xls <- function(filename, skip = 2, sheet = 2) {

  i <- read_xls(filename, skip = skip, sheet = sheet, guess_max = 2000, col_names = T, progress = FALSE)

  i <- i %>%
    mutate(across(everything(), ~ type.convert(., as.is = TRUE))) %>%
    #mutate_if(is.factor, as.character) %>%
    mutate(SDATE = as.Date(SDATE))

  if (length(i) > 0) {
    return(i)
  }
  else {
    message("ERROR: File is empty or not in excel format")
  }
}

#' Read tab delimited files downloaded from SHARK
#'
#' Uses read_delim to read tab delimited files with standardized export format from SHARK
#' @param filename Path to file to be read
#' @param delimiters Character. Specifies the delimiter used to separate values in `filename`.
#'   Options are `"point-tab"` (tab-separated) or `"point-semi"` (semicolon-separated).
#'   Default is `"point-tab"`.
#' @param encoding Character. Specifies the text encoding of `filename`.
#'   Options are `"cp1252"`, `"utf_8"`, `"utf_16"`, or `"latin_1"`.
#'   Default is `"utf_8"`.
#' @return Data frame of file
#' @export

shark_read <- function(filename, delimiters = "point-tab", encoding = "latin_1") {

  # Encoding translation
  encoding_map <- c("cp1252" = "windows-1252",
                    "utf_8" = "UTF-8",
                    "utf_16" = "UTF-16",
                    "latin_1" = "ISO-8859-1")

  if (!encoding %in% c("cp1252", "utf_8", "utf_16", "latin_1")) {
    warning("'encoding' must be one of 'cp1252', 'utf_8', 'utf_16', or 'latin_1'. Defaulting to 'utf_8'.")

    encoding<-"utf_8"
  }

  # Check if the provided encoding is valid
  content_encoding <- encoding_map[[encoding]]

  if (!delimiters %in% c("point-tab", "point-semi")) {
    warning("'delimiters' must be one of 'point-tab' or 'point-semi'. Defaulting to 'point-tab'.")

    delimiters<-"point-tab"
  }

  # Map 'delimiters' input to actual separator
  sep_char <- switch(
    delimiters,
    "point-tab" = "\t",   # Tab-separated
    "point-semi" = ";",   # Semicolon-separated
    stop("Invalid 'delimiters' value. Use 'point-tab' or 'point-semi'.")
  )

  i <- read_delim(filename,
                  delim = sep_char,
                  guess_max = 2000,
                  col_names = T,
                  locale = readr::locale(encoding = content_encoding),
                  progress = FALSE,
                  col_types = cols())

  i <- i %>%
    mutate(across(everything(), ~ type.convert(., as.is = TRUE))) %>%
    #mutate_if(is.factor, as.character)%>%
    mutate(across("sample_date", as.Date)) %>%
    mutate(across("value", as.numeric))

  if (length(i) > 0) {
    return(i)
  }
  else {
    message("ERROR: File is empty or not in tab delimited format")
  }
}

#' Read zip archive and unzip tab delimited files downloaded from SHARK
#'
#' Uses unzip (unz) and read_delim to unzip archive and read tab delimited files with standardized export format from SHARK
#' @param zipname path to file to be read
#' @return Data frame of file
#' @export
shark_read_zip <- function(zipname) {

  i <- read_delim(unz(description = zipname,
                      filename = "shark_data.txt"),
                  delim ="\t",
                  guess_max = 2000,
                  col_names = T,
                  locale = readr::locale(encoding = "latin1"),
                  col_types = cols(),
                  progress = FALSE)

  i <- i %>%
    mutate(across(everything(), ~ type.convert(., as.is = TRUE))) %>%
    #mutate_if(is.factor, as.character)%>%
    mutate(across("sample_date", as.Date)) %>%
    mutate(across("value", as.numeric))

  if (length(i) > 0) {
    return(i)
  }
  else {
    message("ERROR: Zip archive or File is empty or not in tab delimited format")
  }
}
