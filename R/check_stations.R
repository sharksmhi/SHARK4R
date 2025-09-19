#' Check if stations are reported as nominal positions
#'
#' This function attempts to determine whether stations in a dataset are reported
#' using nominal positions (i.e., generic or repeated coordinates across events),
#' rather than actual measured coordinates. It compares the number of unique
#' sampling dates with the number of unique station coordinates.
#'
#' If the number of unique sampling dates is larger than the number of unique
#' station coordinates, the function suspects nominal station positions and
#' issues a warning.
#'
#' @param data A data frame containing at least the columns:
#'   \code{sample_date}, \code{station_name},
#'   \code{sample_longitude_dd}, and \code{sample_latitude_dd}.
#'
#' @return A data frame with distinct station names and their corresponding
#'   latitude/longitude positions, if nominal positions are suspected.
#'   Otherwise, returns \code{NULL}.
#'
#' @examples
#' df <- data.frame(
#'   sample_date = rep(seq.Date(Sys.Date(), by = "day", length.out = 3), each = 2),
#'   station_name = rep(c("ST1", "ST2"), 3),
#'   sample_longitude_dd = rep(c(15.0, 16.0), 3),
#'   sample_latitude_dd = rep(c(58.5, 58.6), 3)
#' )
#' nominal_station(df)
#'
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
    return(NULL)
  }
}

#' Station matching using SMHI station list
#'
#' Matches reported station names in your data with a curated station list
#' ("station.txt"), which is synced with "Stationsregistret":
#' <https://stationsregister.miljodatasamverkan.se/>.
#'
#' @param names Character vector of station names to match.
#' @param station_file Optional path to a custom station file (tab-delimited).
#'   If \code{NULL} (default), the function will extract and use the bundled
#'   "station.zip" from the SHARK4R package.
#'
#' @return A data frame with two columns:
#'   \itemize{
#'     \item \code{reported_station_name} – the input station names
#'     \item \code{match_type} – logical indicating if a match was found
#'   }
#'
#' @examples
#' stations <- c("BY31", "NonexistentStation")
#' match_station(stations)
#'
#' @export
match_station <- function(names, station_file = NULL) {

  if (!is.null(station_file)) {
    station_db <- read_delim(station_file,
                             delim ="\t",
                             guess_max = 2000,
                             col_names = T,
                             locale = readr::locale(encoding = "latin1"),
                             col_types = cols(),
                             progress = FALSE)
  } else {
    # Path to the zip file inside your package
    zip_path <- system.file("extdata", "station.zip", package = "SHARK4R")

    # Create a temporary directory for extraction
    tmp_dir <- tempdir()

    # Extract the zip contents
    utils::unzip(zip_path, exdir = tmp_dir)

    # Build path to the extracted file
    station_file <- file.path(tmp_dir, "station.txt")

    station_db <- read_delim(station_file,
                             delim ="\t",
                             guess_max = 2000,
                             col_names = T,
                             locale = readr::locale(encoding = "latin1"),
                             col_types = cols(),
                             progress = FALSE)
  }

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

#' Station distance check using SMHI station list
#'
#' Matches reported station names in your data with a curated station list
#' ("station.txt"), synced with "Stationsregistret":
#' <https://stationsregister.miljodatasamverkan.se/>, and checks if the stations
#' are within preset distance limits.
#'
#' @param names Character vector of station names to check.
#' @param station_file Optional path to a custom station file (tab-delimited).
#'   If NULL (default), the function will extract and use the bundled
#'   "station.zip" from the SHARK4R package.
#' @export
check_station_distance<- function(names, station_file = NULL) {

  if (!is.null(station_file)) {
    station_db <- read_delim(station_file,
                             delim ="\t",
                             guess_max = 2000,
                             col_names = T,
                             locale = readr::locale(encoding = "latin1"),
                             col_types = cols(),
                             progress = FALSE)
  } else {
    # Path to the zip file inside your package
    zip_path <- system.file("extdata", "station.zip", package = "SHARK4R")

    # Create a temporary directory for extraction
    tmp_dir <- tempdir()

    # Extract the zip contents
    utils::unzip(zip_path, exdir = tmp_dir)

    # Build path to the extracted file
    station_file <- file.path(tmp_dir, "station.txt")

    station_db <- read_delim(station_file,
                             delim ="\t",
                             guess_max = 2000,
                             col_names = T,
                             locale = readr::locale(encoding = "latin1"),
                             col_types = cols(),
                             progress = FALSE)
  }

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
