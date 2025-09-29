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
#' check_nominal_station(df)
#'
#' @export
check_nominal_station <- function(data) {
  eventdate = data %>%
    select(sample_date) %>%
    rename(DATE = sample_date) %>%
    distinct()

  coord = data %>%
    select(station_name, sample_longitude_dd, sample_latitude_dd) %>%
    rename(STATION = station_name, LON = sample_longitude_dd, LAT = sample_latitude_dd) %>%
    distinct()

  if (nrow(eventdate) > nrow(coord)) {
    message("WARNING: Suspected nominal positions reported! Is this correct?")
    return(coord)
  } else {
    message("Positions are not suspected to be nominal")
    return(invisible(NULL))
  }
}

#' Check if stations are reported as nominal positions
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [check_nominal_station()].
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
#' @keywords internal
#' @export
nominal_station <- function(data) {
  lifecycle::deprecate_warn("0.1.7.9000", "nominal_station()", "check_nominal_station()")

  check_nominal_station(data)
}

#' Match station names against SMHI station list
#'
#' Matches reported station names in your dataset against a curated station list
#' (\code{"station.txt"}), which is synced with "Stationsregistret":
#' <https://stationsregister.miljodatasamverkan.se/>.
#'
#' This is useful for validating station names and identifying any unmatched
#' or misspelled entries.
#'
#' If \code{try_synonyms = TRUE}, unmatched station names are also compared
#' against the \code{SYNONYM_NAMES} column in the station database, splitting
#' multiple synonyms separated by \code{<or>}.
#'
#' @param names Character vector of station names to match.
#' @param station_file Optional path to a custom station file (tab-delimited).
#'   If \code{NULL} (default), the function will extract and use the bundled
#'   \code{"station.zip"} from the \code{SHARK4R} package.
#' @param try_synonyms Logical; if \code{TRUE} (default), unmatched names
#'   are also compared against the \code{SYNONYM_NAMES} column in the database.
#'
#' @return A data frame with two columns:
#'   \describe{
#'     \item{reported_station_name}{The input station names.}
#'     \item{match_type}{Logical; \code{TRUE} if the station was found in the SMHI station list (including synonyms if enabled), otherwise \code{FALSE}.}
#'   }
#'
#' @examples
#' \dontrun{
#' stations <- c("ANHOLT E", "BY5 BORNHOLMSDJ", "STX999")
#' match_station(stations, try_synonyms = TRUE)
#' }
#'
#' @export
match_station <- function(names, station_file = NULL, try_synonyms = TRUE) {

  if (!is.null(station_file)) {
    station_db <- readr::read_delim(station_file,
                                    delim ="\t",
                                    guess_max = 2000,
                                    col_names = TRUE,
                                    locale = readr::locale(encoding = "latin1"),
                                    col_types = readr::cols(),
                                    progress = FALSE)
  } else {
    zip_path <- system.file("extdata", "station.zip", package = "SHARK4R")
    tmp_dir <- tempdir()
    utils::unzip(zip_path, exdir = tmp_dir)
    station_file <- file.path(tmp_dir, "station.txt")
    station_db <- readr::read_delim(station_file,
                                    delim ="\t",
                                    guess_max = 2000,
                                    col_names = TRUE,
                                    locale = readr::locale(encoding = "latin1"),
                                    col_types = readr::cols(),
                                    progress = FALSE)
  }

  # ---- Initial match against STATION_NAME ----
  match_type <- names %in% station_db$STATION_NAME

  # ---- Try matching against synonyms if enabled ----
  if (try_synonyms && any(!match_type) && "SYNONYM_NAMES" %in% names(station_db)) {
    unmatched_idx <- which(!match_type)
    for (i in unmatched_idx) {
      s_name <- names[i]
      syn_match <- sapply(station_db$SYNONYM_NAMES, function(x) {
        any(trimws(unlist(strsplit(x, "<or>"))) == s_name)
      })
      if (any(syn_match, na.rm = TRUE)) {
        match_type[i] <- TRUE
      }
    }
  }

  matches <- data.frame(
    reported_station_name = names,
    match_type = match_type
  )

  if (any(!match_type)) {
    message("WARNING: Unmatched stations found, check synonyms")
  } else {
    message("All stations found")
  }

  return(matches)
}

#' Check station distances against SMHI station list
#'
#' Matches reported station names against the SMHI curated station list
#' (\code{"station.txt"}) and checks whether matched stations fall within
#' pre-defined distance limits. This helps ensure that station assignments
#' are spatially consistent.
#'
#' Optionally, a leaflet map of stations can be plotted. SMHI stations that
#' match the reported data are shown as blue circles, with their allowed
#' radius visualized and displayed in the popup (e.g., "ST1 (Radius: 1000 m)").
#' Reported stations are shown as markers colored by whether they fall within
#' the radius (green), outside the radius (red), or unmatched (gray).
#'
#' If \code{try_synonyms = TRUE}, the function will attempt a second match
#' using the \code{SYNONYM_NAMES} column in the station database, splitting
#' multiple synonyms separated by \code{<or>}.
#'
#' @param data A data frame containing at least the columns:
#'   \code{station_name}, \code{sample_longitude_dd}, \code{sample_latitude_dd}.
#' @param station_file Optional path to a custom station file (tab-delimited).
#'   If \code{NULL} (default), the function will extract and use the bundled
#'   \code{"station.zip"} from the \code{SHARK4R} package.
#' @param plot_leaflet Logical; if \code{TRUE}, displays a leaflet map with
#'   SMHI stations (blue circles with radius in popup) and reported stations
#'   (green/red/gray markers). Default is \code{FALSE}.
#' @param try_synonyms Logical; if \code{TRUE} (default), unmatched station
#'   names are also compared against the \code{SYNONYM_NAMES} column in
#'   the station database.
#' @param fallback_crs Integer; CRS (EPSG code) to use when creating spatial
#'   points if no CRS is available. Defaults to \code{4326} (WGS84). Change this
#'   if your coordinates are reported in another CRS (e.g., \code{3006} for
#'   SWEREF99 TM).
#'
#' @return If \code{plot_leaflet = FALSE}, returns a data frame with columns:
#'   \describe{
#'     \item{station_name}{Reported station name.}
#'     \item{match_type}{\code{TRUE} if station matched in SMHI list, \code{FALSE} otherwise.}
#'     \item{distance_m}{Distance in meters from reported station to matched SMHI station.}
#'     \item{within_limit}{\code{TRUE} if distance <= allowed radius, \code{FALSE} if outside, \code{NA} if unmatched.}
#'   }
#'
#' If \code{plot_leaflet = TRUE}, the function also produces a leaflet map
#' (returned invisibly). SMHI station popups include both the station name
#' and allowed radius.
#'
#' @examples
#' df <- data.frame(
#'   station_name = c("ANHOLT E", "BY5 BORNHOLMSDJ", "NEW STATION"),
#'   sample_longitude_dd = c(12.1, 15.97, 17.5),
#'   sample_latitude_dd  = c(56.7, 55.25, 58.7)
#' )
#' check_station_distance(df, plot_leaflet = FALSE, try_synonyms = TRUE)
#'
#' @export
check_station_distance <- function(data, station_file = NULL,
                                   plot_leaflet = FALSE,
                                   try_synonyms = TRUE,
                                   fallback_crs = 4326) {
  # ---- Required columns in reported data ----
  required_data_cols <- c("station_name", "sample_longitude_dd", "sample_latitude_dd")
  missing_data_cols <- setdiff(required_data_cols, names(data))
  if (length(missing_data_cols) > 0) {
    stop("Missing required column(s) in input data: ",
         paste(missing_data_cols, collapse = ", "))
  }

  # ---- Load station database ----
  if (!is.null(station_file)) {
    station_db <- readr::read_delim(station_file,
                                    delim ="\t",
                                    guess_max = 2000,
                                    col_names = TRUE,
                                    locale = readr::locale(encoding = "latin1"),
                                    col_types = readr::cols(),
                                    progress = FALSE)
  } else {
    zip_path <- system.file("extdata", "station.zip", package = "SHARK4R")
    tmp_dir <- tempdir()
    utils::unzip(zip_path, exdir = tmp_dir)
    station_file <- file.path(tmp_dir, "station.txt")
    station_db <- readr::read_delim(station_file,
                                    delim ="\t",
                                    guess_max = 2000,
                                    col_names = TRUE,
                                    locale = readr::locale(encoding = "latin1"),
                                    col_types = readr::cols(),
                                    progress = FALSE)
  }

  # ---- Required columns in station_db ----
  required_db_cols <- c("STATION_NAME",
                        "LATITUDE_WGS84_SWEREF99_DD",
                        "LONGITUDE_WGS84_SWEREF99_DD",
                        "OUT_OF_BOUNDS_RADIUS")
  missing_db_cols <- setdiff(required_db_cols, names(station_db))
  if (length(missing_db_cols) > 0) {
    stop("Missing required column(s) in station database: ",
         paste(missing_db_cols, collapse = ", "))
  }

  # ---- Initial join with station names ----
  merged <- dplyr::left_join(
    data,
    station_db %>%
      dplyr::select(STATION_NAME,
                    LAT_REF = LATITUDE_WGS84_SWEREF99_DD,
                    LON_REF = LONGITUDE_WGS84_SWEREF99_DD,
                    OUT_OF_BOUNDS_RADIUS,
                    SYNONYM_NAMES),
    by = c("station_name" = "STATION_NAME")
  )

  # ---- Try matching using synonyms if enabled ----
  if (try_synonyms) {
    unmatched_idx <- which(is.na(merged$LAT_REF))
    if (length(unmatched_idx) > 0) {
      for (i in unmatched_idx) {
        s_name <- merged$station_name[i]
        syn_idx <- which(sapply(station_db$SYNONYM_NAMES, function(x) {
          any(trimws(unlist(strsplit(x, "<or>"))) == s_name)
        }))
        if (length(syn_idx) == 1) {
          merged$LAT_REF[i] <- station_db$LATITUDE_WGS84_SWEREF99_DD[syn_idx]
          merged$LON_REF[i] <- station_db$LONGITUDE_WGS84_SWEREF99_DD[syn_idx]
          merged$OUT_OF_BOUNDS_RADIUS[i] <- station_db$OUT_OF_BOUNDS_RADIUS[syn_idx]
        }
      }
    }
  }

  # ---- Calculate distances (sf, with configurable fallback CRS) ----
  merged <- merged %>%
    dplyr::mutate(
      match_type = !is.na(LAT_REF) & !is.na(LON_REF),
      distance_m = purrr::pmap_dbl(list(sample_longitude_dd, sample_latitude_dd, LON_REF, LAT_REF, match_type),
                                   function(lon1, lat1, lon2, lat2, matched) {
                                     if (!matched) return(NA_real_)
                                     p1 <- sf::st_sfc(sf::st_point(c(lon1, lat1)), crs = fallback_crs)
                                     p2 <- sf::st_sfc(sf::st_point(c(lon2, lat2)), crs = fallback_crs)
                                     as.numeric(sf::st_distance(p1, p2, by_element = TRUE))
                                   }),
      within_limit = ifelse(match_type & !is.na(OUT_OF_BOUNDS_RADIUS),
                            distance_m <= OUT_OF_BOUNDS_RADIUS,
                            NA)
    )

  # ---- Messages ----
  if (any(!merged$match_type, na.rm = TRUE)) {
    message("WARNING: Unmatched stations found, check synonyms")
    print(merged[!merged$match_type, c("station_name")])
  }
  if (any(merged$match_type & !merged$within_limit, na.rm = TRUE)) {
    message("WARNING: Some stations are outside the allowed distance limit")
    print(merged[merged$match_type & !merged$within_limit,
                 c("station_name", "distance_m", "OUT_OF_BOUNDS_RADIUS")])
  }

  # ---- Leaflet map ----
  if (plot_leaflet) {
    matched_station_names <- merged %>%
      dplyr::filter(match_type) %>%
      dplyr::pull(station_name) %>%
      unique()

    station_points <- station_db %>%
      dplyr::filter(STATION_NAME %in% matched_station_names |
                      (try_synonyms & sapply(SYNONYM_NAMES, function(x) any(trimws(unlist(strsplit(x, "<or>"))) %in% matched_station_names)))) %>%
      dplyr::rename(LAT = LATITUDE_WGS84_SWEREF99_DD,
                    LON = LONGITUDE_WGS84_SWEREF99_DD,
                    RADIUS = OUT_OF_BOUNDS_RADIUS,
                    STATION = STATION_NAME) %>%
      dplyr::distinct() %>%
      dplyr::mutate(popup_text = paste0(STATION, " (Radius: ", RADIUS, " m)"))

    reported_points <- merged %>%
      dplyr::mutate(color = dplyr::case_when(
        match_type & within_limit ~ "green",
        match_type & !within_limit ~ "red",
        !match_type ~ "gray"
      )) %>%
      dplyr::rename(LON = sample_longitude_dd,
                    LAT = sample_latitude_dd,
                    STATION = station_name)

    icons_rep <- leaflet::awesomeIcons(
      icon = "map-marker",
      iconColor = "white",
      markerColor = reported_points$color,
      library = "fa"
    )

    m <- leaflet::leaflet() %>%
      leaflet::addProviderTiles("Esri.OceanBasemap", options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
      leaflet::addCircleMarkers(data = station_points, lng = ~LON, lat = ~LAT,
                                color = "blue", radius = 5, fill = TRUE, fillOpacity = 0.7,
                                popup = ~popup_text) %>%
      leaflet::addCircles(data = station_points, lng = ~LON, lat = ~LAT,
                          radius = ~RADIUS, color = "blue", fill = FALSE) %>%
      leaflet::addAwesomeMarkers(data = reported_points, lng = ~LON, lat = ~LAT,
                                 icon = icons_rep, popup = ~STATION)

    return(m)
  }

  return(merged %>%
           dplyr::select(station_name,
                         match_type,
                         distance_m,
                         within_limit))
}
