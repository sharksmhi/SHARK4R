#' Check if stations are reported as nominal position or not
#' Function makes an estimated guess whether stations are nominal
#' @param names Vector of latitude and longitude positions.
#' @return Data frame with station name and latitude and longitude positions.
#' @export

nominal_station <- function(data) {
    eventdate = data %>% 
      select(sample_date) %>% 
      rename(DATE = sample_date) %>% 
      distinct()
      
    coord = data %>% 
      select(station_name, sample_longitude_dd, sample_latitude_dd) %>% 
      rename(STATION = station_name, LON = sample_longitude_dd, LAT = sample_latitude_dd) %>% 
      distinct()

    if (length(eventdate)>length(coord)) {
    message("WARNING: Suspected nominal positions reported! Is this correct?")
    return(coord)
  }
  else {
    message("Positions are not suspected to be nominal")
  }
}
  
#' Station matching using SMHI station list "stations.txt" (the list is synced with "Stationsregistret": https://stationsregister.miljodatasamverkan.se/stationsregister/composer/)
#' matches reported station name in data with curated station list
#' @param names Vector of station names.
#' @return Data frame with station name and match type.
#' @export

match_station <- function(names) {
  
  station_db <- read_delim(system.file("extdata", "stations.txt", package = "SHARK4R"), delim ="\t", guess_max = 2000, col_names = T, locale = readr::locale(encoding = "latin1"))
  
  match_index <- match(names, station_db$STATION_NAME)
  
  match_type <- names %in% station_db$STATION_NAME
  
  matches <- data.frame(reported_station_name = names, match_type = match_type)
  
  if (length(which(match_type == FALSE))>0) {
    message("WARNING: Unmatched stations found, check synonyms")
    print(matches[!match_type,])
  }
  else {
    message("All stations found")
  }
}

#' Station distance check using SMHI station list "stations.txt" (the list is synced with "Stationsregistret": https://stationsregister.miljodatasamverkan.se/stationsregister/composer/)
#' matches reported station name in data with curated station list and checks if it is within preset distance
#' @param names Vector of station names.
#' @param latitude and longitude
#' @return Data frame with station name and logical value within/outside preset distance limits.
#' @export

check_station_distance<- function(names, lat, lon) {
  
  station_db <- read_delim(system.file("extdata", "stations.txt", package = "SHARK4R"), delim ="\t", guess_max = 2000, col_names = T, locale = readr::locale(encoding = "latin1"))
  
  match_index <- match(names, station_db$STATION_NAME)
  
  match_type <- names %in% station_db$STATION_NAME
  
  matches <- data.frame(reported_station_name = names, match_type = match_type)
  
  if (length(which(match_type == FALSE))>0) {
    message("WARNING: Unmatched stations found, check synonyms")
    print(matches[!match_type,])
  }
  else if (length(which(match_type == TRUE))>0) {
    message("WARNING: Matched stations found but they are outside distance limit")
  }
  else {
    message("All stations found within distance limits")
  }
}