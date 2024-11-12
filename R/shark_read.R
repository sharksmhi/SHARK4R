#' Read .xlsx files delivered to SHARK
#' Uses readxl to read excel files with standardized delivery format 
#' @param filename path to file to be read
#' @return Data frame of file
#' @importFrom dplyr mutate_all mutate_at
#' @importFrom utils type.convert
#' @importFrom lubridate ymd
#' @importFrom readxl read_xlsx
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
#' @return Data frame of file
#' @importFrom dplyr mutate_all mutate_at
#' @importFrom utils type.convert
#' @importFrom lubridate ymd
#' @importFrom readxl read_xls
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
#' @param filename path to file to be read
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
#' @param filename path to file to be read
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