#' Read .xlsx files delivered to SHARK
#' Uses readxl to read excel files with standardized delivery format
#' @param filename path to file to be read
#' @param skip Minimum number of rows to skip before reading anything, be it column names or data. Leading empty rows are automatically skipped, so this is a lower bound. Ignored if range is given. Default is 2.
#' @param sheet Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). Ignored if the sheet is specified via range. If neither argument specifies the sheet, defaults to the second sheet.
#' @return Data frame of file
#' @export

shark_read_deliv <- function(filename, skip = 2, sheet = 2) {

  i <- read_xlsx(filename, skip = skip, sheet = sheet, guess_max = 2000, col_names = T)

  i <- i %>%
    mutate_all(type.convert) %>%
    #mutate_if(is.factor, as.character) %>%
    mutate_at("SDATE", ymd)

  if (length(i) > 0) {
    return(i)
    }
  else {
    message("ERROR: File is empty or not in excel format")
  }
}

#' Read .xls files delivered to SHARK
#' Uses readxl to read excel files with standardized delivery format
#' @param filename path to file to be read
#' @param skip Minimum number of rows to skip before reading anything, be it column names or data. Leading empty rows are automatically skipped, so this is a lower bound. Ignored if range is given. Default is 2.
#' @param sheet Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). Ignored if the sheet is specified via range. If neither argument specifies the sheet, defaults to the second sheet.
#' @return Data frame of file
#' @export

shark_read_deliv_xls <- function(filename, skip = 2, sheet = 2) {

  i <- read_xls(filename, skip = skip, sheet = sheet, guess_max = 2000, col_names = T)

  i <- i %>%
    mutate_all(type.convert) %>%
    #mutate_if(is.factor, as.character) %>%
    mutate_at("SDATE", ymd)

  if (length(i) > 0) {
    return(i)
  }
  else {
    message("ERROR: File is empty or not in excel format")
  }
}

#' Read tab delimited files downloaded from SHARK
#' Uses read_delim to read tab delimited files with standardized export format from SHARK
#' @param filename Path to file to be read
#' @param type File extension. Default is `.txt`.
#' @return Data frame of file
#' @export

shark_read <- function(filename, type = "txt") {

  i <- read_delim(filename, delim = "\t", guess_max = 2000, col_names = T, locale = readr::locale(encoding = "latin1"))

  i <- i %>%
    mutate_all(type.convert) %>%
    #mutate_if(is.factor, as.character)%>%
    mutate_at("sample_date", ymd) %>%
    mutate_at("value", as.numeric)


  if (length(i) > 0) {
    return(i)
  }
  else {
    message("ERROR: File is empty or not in tab delimited format")
  }
}

#' Read zip archive and unzip tab delimited files downloaded from SHARK
#' Uses unzip (unz) and read_delim to unzip archive and read tab delimited files with standardized export format from SHARK
#' @param zipname path to file to be read
#' @param type File extension. Default is `.zip`.
#' @return Data frame of file
#' @export

shark_read_zip <- function(zipname, type = "zip") {

  i <- read_delim(unz(description = zipname, filename = "shark_data.txt"), delim ="\t", guess_max = 2000, col_names = T, locale = readr::locale(encoding = "latin1"))

  i <- i %>%
    mutate_all(type.convert) %>%
    #mutate_if(is.factor, as.character)%>%
    mutate_at("sample_date", ymd) %>%
    mutate_at("value", as.numeric)

  if (length(i) > 0) {
    return(i)
  }
  else {
    message("ERROR: Zip archive or File is empty or not in tab delimited format")
  }
}
