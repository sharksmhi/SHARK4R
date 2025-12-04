#' Check if stations are reported as nominal positions
#'
#' This function attempts to determine whether stations in a dataset are reported
#' using nominal positions (i.e., generic or repeated coordinates across events),
#' rather than actual measured coordinates.
#'
#' @details
#' The function compares the number of unique sampling dates with the number of
#' unique station coordinates.
#'
#' If the number of unique sampling dates is larger than the number of unique
#' station coordinates, the function suspects nominal station positions and
#' issues a warning.
#'
#' @param data A data frame containing at least the columns:
#'   \code{sample_date}, \code{station_name},
#'   \code{sample_longitude_dd}, and \code{sample_latitude_dd}.
#' @param verbose Logical. If TRUE, messages will be displayed during execution. Defaults to TRUE.
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
check_nominal_station <- function(data, verbose = TRUE) {
  eventdate = data %>%
    select(sample_date) %>%
    rename(DATE = sample_date) %>%
    distinct()

  coord = data %>%
    select(station_name, sample_longitude_dd, sample_latitude_dd) %>%
    rename(STATION = station_name, LON = sample_longitude_dd, LAT = sample_latitude_dd) %>%
    distinct()

  if (nrow(eventdate) > nrow(coord)) {
    if (verbose) message("WARNING: Suspected nominal positions reported! Is this correct?")
    return(coord)
  } else {
    if (verbose) message("Positions are not suspected to be nominal")
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
#' \donttest{
#' df <- data.frame(
#'   sample_date = rep(seq.Date(Sys.Date(), by = "day", length.out = 3), each = 2),
#'   station_name = rep(c("ST1", "ST2"), 3),
#'   sample_longitude_dd = rep(c(15.0, 16.0), 3),
#'   sample_latitude_dd = rep(c(58.5, 58.6), 3)
#' )
#' nominal_station(df)
#' }
#'
#' @keywords internal
#' @export
nominal_station <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "nominal_station()", "check_nominal_station()")

  check_nominal_station(data)
}

#' Match station names against SMHI station list
#'
#' Matches reported station names in your dataset against a curated station list
#' (\code{"station.txt"}), which is synced with "Stationsregistret":
#' <https://stationsregister.miljodatasamverkan.se/>.
#'
#' @details
#' This function is useful for validating station names and identifying any unmatched
#' or misspelled entries.
#'
#' If \code{try_synonyms = TRUE}, unmatched station names are also compared
#' against the \code{SYNONYM_NAMES} column in the station database, splitting
#' multiple synonyms separated by \code{<or>}.
#'
#' The function first checks if a station file path is provided via the
#' \code{station_file} argument. If not, it looks for the
#' \code{NODC_CONFIG} environment variable. This variable can point to a folder
#' where the NODC (Swedish National Oceanographic Data Center) configuration and station file
#' are stored, typically including:
#' \itemize{
#'   \item \code{<NODC_CONFIG>/config/station.txt}
#' }
#' If \code{NODC_CONFIG} is set and the folder exists, the function will use
#' \code{station.txt} from that location. Otherwise, it falls back to the
#' bundled \code{station.zip} included in the \code{SHARK4R} package.
#'
#' @param names Character vector of station names to match.
#' @param station_file Optional path to a custom station file (tab-delimited).
#'   If \code{NULL} (default), the function will first attempt to use the
#'   \code{NODC_CONFIG} environment variable, and if that fails, will use the
#'   bundled \code{"station.zip"} from the \code{SHARK4R} package.
#' @param try_synonyms Logical; if \code{TRUE} (default), unmatched names
#'   are also compared against the \code{SYNONYM_NAMES} column in the database.
#' @param verbose Logical. If TRUE, messages will be displayed during execution. Defaults to TRUE.
#'
#' @return A data frame with two columns:
#'   \describe{
#'     \item{reported_station_name}{The input station names.}
#'     \item{match_type}{Logical; \code{TRUE} if the station was found in the SMHI station list (including synonyms if enabled), otherwise \code{FALSE}.}
#'   }
#'
#' @examples
#' # Example stations
#' stations <- c("ANHOLT E", "BY5 BORNHOLMSDJ", "STX999")
#'
#' # Check if stations names are in stations.txt (including synonyms)
#' match_station(stations, try_synonyms = TRUE, verbose = FALSE)
#'
#' @export
match_station <- function(names, station_file = NULL, try_synonyms = TRUE, verbose = TRUE) {

  # Load station database via helper
  station_db <- load_station_bundle(station_file = station_file, verbose = verbose)

  # ---- Match unique names first ----
  unique_names <- unique(names)
  match_type_unique <- unique_names %in% station_db$STATION_NAME

  # ---- Try matching against synonyms if enabled ----
  if (try_synonyms && any(!match_type_unique) && "SYNONYM_NAMES" %in% names(station_db)) {
    unmatched_unique <- unique_names[!match_type_unique]

    # Preprocess synonyms once
    syn_list <- strsplit(station_db$SYNONYM_NAMES, "<or>")
    syn_list <- lapply(syn_list, trimws)

    syn_matches <- sapply(unmatched_unique, function(s_name) {
      any(sapply(syn_list, function(x) s_name %in% x), na.rm = TRUE)
    })

    match_type_unique[unique_names %in% unmatched_unique] <- syn_matches
  }

  # ---- Map results back to original names ----
  match_type <- match_type_unique[match(names, unique_names)]

  matches <- data.frame(
    reported_station_name = names,
    match_type = match_type
  )

  if (any(!match_type)) {
    if (verbose) message("WARNING: Unmatched stations found, check synonyms")
  } else {
    if (verbose) message("All stations found")
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
#' @details
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
#' The function first checks if a station file path is provided via the
#' \code{station_file} argument. If not, it looks for the
#' \code{NODC_CONFIG} environment variable. This variable can point to a folder
#' where the NODC (Swedish National Oceanographic Data Center) configuration and station file
#' are stored, typically including:
#' \itemize{
#'   \item \code{<NODC_CONFIG>/config/station.txt}
#' }
#' If \code{NODC_CONFIG} is set and the folder exists, the function will use
#' \code{station.txt} from that location. Otherwise, it falls back to the
#' bundled \code{station.zip} included in the \code{SHARK4R} package.
#'
#' @param data A data frame containing at least the columns:
#'   \code{station_name}, \code{sample_longitude_dd}, \code{sample_latitude_dd}.
#' @param station_file Optional path to a custom station file (tab-delimited).
#'   If \code{NULL} (default), the function will first attempt to use the
#'   \code{NODC_CONFIG} environment variable, and if that fails, will use the
#'   bundled \code{"station.zip"} from the \code{SHARK4R} package.
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
#' @param only_bad Logical; if \code{TRUE}, the leaflet map will only display
#'   stations that are outside the allowed radius (red markers). Default is \code{FALSE}.
#' @param verbose Logical. If TRUE, messages will be displayed during execution. Defaults to TRUE.
#'
#' @return If \code{plot_leaflet = FALSE}, returns a data frame with columns:
#'   \describe{
#'     \item{station_name}{Reported station name.}
#'     \item{match_type}{\code{TRUE} if station matched in SMHI list, \code{FALSE} otherwise.}
#'     \item{distance_m}{Distance in meters from reported station to matched SMHI station.}
#'     \item{within_limit}{\code{TRUE} if distance <= allowed radius, \code{FALSE} if outside, \code{NA} if unmatched.}
#'   }
#'
#' If \code{plot_leaflet = TRUE}, the function produces a leaflet map showing:
#' \itemize{
#'   \item Blue circles for SMHI stations with radius in the popup.
#'   \item Reported stations colored by status: green (within radius), red (outside radius), gray (unmatched).
#'   \item If \code{only_bad = TRUE}, only the red stations (outside radius) are displayed.
#' }
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   station_name = c("ANHOLT E", "BY5 BORNHOLMSDJ", "NEW STATION"),
#'   sample_longitude_dd = c(12.1, 15.97, 17.5),
#'   sample_latitude_dd  = c(56.7, 55.25, 58.7)
#' )
#'
#' # Check station distance
#' check_station_distance(df, try_synonyms = TRUE, verbose = FALSE)
#'
#' # Plot bad points in leaflet map
#' map <- check_station_distance(df,
#'                               plot_leaflet = TRUE,
#'                               only_bad = TRUE,
#'                               verbose = FALSE)
#'
#' @export
check_station_distance <- function(data, station_file = NULL,
                                   plot_leaflet = FALSE,
                                   try_synonyms = TRUE,
                                   fallback_crs = 4326,
                                   only_bad = FALSE,
                                   verbose = TRUE) {
  # ---- Required columns in reported data ----
  required_data_cols <- c("station_name", "sample_longitude_dd", "sample_latitude_dd")
  missing_data_cols <- setdiff(required_data_cols, names(data))
  if (length(missing_data_cols) > 0) {
    stop("Missing required column(s) in input data: ",
         paste(missing_data_cols, collapse = ", "))
  }

  # ---- Load station database (replaces duplicated logic) ----
  station_db <- load_station_bundle(station_file = station_file, verbose = verbose)

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
    unmatched_names <- unique(merged$station_name[is.na(merged$LAT_REF)])
    if (length(unmatched_names) > 0) {
      syn_list <- strsplit(station_db$SYNONYM_NAMES, "<or>")
      syn_list <- lapply(syn_list, trimws)
      syn_match_idx <- sapply(unmatched_names, function(s_name) {
        idx <- which(sapply(syn_list, function(x) s_name %in% x))
        if (length(idx) == 1) return(idx) else return(NA_integer_)
      })
      for (i in seq_along(unmatched_names)) {
        idx_in_merged <- which(merged$station_name == unmatched_names[i] & is.na(merged$LAT_REF))
        if (!is.na(syn_match_idx[i])) {
          merged$LAT_REF[idx_in_merged] <- station_db$LATITUDE_WGS84_SWEREF99_DD[syn_match_idx[i]]
          merged$LON_REF[idx_in_merged] <- station_db$LONGITUDE_WGS84_SWEREF99_DD[syn_match_idx[i]]
          merged$OUT_OF_BOUNDS_RADIUS[idx_in_merged] <- station_db$OUT_OF_BOUNDS_RADIUS[syn_match_idx[i]]
        }
      }
    }
  }

  # ---- Distance calculation (optimized) ----
  merged <- merged %>%
    dplyr::mutate(match_type = !is.na(LAT_REF) & !is.na(LON_REF))

  matched_rows <- merged %>%
    dplyr::filter(match_type) %>%
    dplyr::distinct(sample_longitude_dd, sample_latitude_dd, LON_REF, LAT_REF, OUT_OF_BOUNDS_RADIUS)

  if (nrow(matched_rows) > 0) {
    p1 <- sf::st_as_sf(matched_rows, coords = c("sample_longitude_dd", "sample_latitude_dd"), crs = fallback_crs)
    p2 <- sf::st_as_sf(matched_rows, coords = c("LON_REF", "LAT_REF"), crs = fallback_crs)
    matched_rows$distance_m <- as.numeric(sf::st_distance(p1, p2, by_element = TRUE))
    matched_rows$within_limit <- matched_rows$distance_m <= matched_rows$OUT_OF_BOUNDS_RADIUS
    merged <- merged %>%
      dplyr::left_join(
        matched_rows %>%
          dplyr::select(sample_longitude_dd, sample_latitude_dd, LON_REF, LAT_REF, distance_m, within_limit),
        by = c("sample_longitude_dd", "sample_latitude_dd", "LON_REF", "LAT_REF")
      )
  } else {
    merged$distance_m <- NA_real_
    merged$within_limit <- NA
  }

  # ---- Messages ----
  if (any(!merged$match_type, na.rm = TRUE)) {
    if (verbose) message("WARNING: Unmatched stations found, check synonyms")
    if (verbose) print(merged[!merged$match_type, c("station_name")])
  }
  if (any(merged$match_type & !merged$within_limit, na.rm = TRUE)) {
    if (verbose) message("WARNING: Some stations are outside the allowed distance limit")
    if (verbose) print(merged[merged$match_type & !merged$within_limit,
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
                    STATION = station_name) %>%
      distinct()

    if (only_bad) {
      # Only show bad reported points
      reported_points_bad <- reported_points %>% dplyr::filter(!within_limit)

      # Filter station_points to only those matching the bad reported points
      bad_station_names <- reported_points_bad$STATION
      station_points_bad <- station_points %>%
        dplyr::filter(
          STATION %in% bad_station_names |
            (try_synonyms & sapply(SYNONYM_NAMES, function(x) any(trimws(unlist(strsplit(x, "<or>"))) %in% bad_station_names)))
        )

      icons_bad <- leaflet::awesomeIcons(icon = "map-marker", iconColor = "white",
                                         markerColor = "red", library = "fa")

      m <- leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron", options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
        # Add only matched station points
        leaflet::addCircleMarkers(data = station_points_bad, lng = ~LON, lat = ~LAT,
                                  color = "blue", radius = 5, fill = TRUE, fillOpacity = 0.7,
                                  popup = ~popup_text) %>%
        leaflet::addCircles(data = station_points_bad, lng = ~LON, lat = ~LAT,
                            radius = ~RADIUS, color = "blue", fill = FALSE) %>%
        # Add only bad reported points
        leaflet::addAwesomeMarkers(data = reported_points_bad, lng = ~LON, lat = ~LAT,
                                   icon = icons_bad, popup = ~STATION)
    } else {
      # Split reported points
      reported_points_ok <- reported_points %>% dplyr::filter(within_limit == TRUE)
      reported_points_bad <- reported_points %>% dplyr::filter(within_limit == FALSE)

      icons_ok <- leaflet::awesomeIcons(icon = "map-marker", iconColor = "white",
                                        markerColor = "green", library = "fa")
      icons_bad <- leaflet::awesomeIcons(icon = "map-marker", iconColor = "white",
                                         markerColor = "red", library = "fa")

      m <- leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron", options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
        leaflet::addCircleMarkers(data = station_points, lng = ~LON, lat = ~LAT,
                                  color = "blue", radius = 5, fill = TRUE, fillOpacity = 0.7,
                                  popup = ~popup_text) %>%
        leaflet::addCircles(data = station_points, lng = ~LON, lat = ~LAT,
                            radius = ~RADIUS, color = "blue", fill = FALSE) %>%
        leaflet::addAwesomeMarkers(data = reported_points_ok, lng = ~LON, lat = ~LAT,
                                   icon = icons_ok, popup = ~STATION,
                                   clusterOptions = leaflet::markerClusterOptions()) %>%
        leaflet::addAwesomeMarkers(data = reported_points_bad, lng = ~LON, lat = ~LAT,
                                   icon = icons_bad, popup = ~STATION)
    }

    m <- m %>%
      addTiles(
        urlTemplate = "https://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
        attribution = "&copy; OpenSeaMap contributors, OpenStreetMap contributors",
        options = providerTileOptions(noWrap = TRUE)
      )

    return(m)
  }

  return(merged %>%
           dplyr::select(station_name,
                         sample_longitude_dd,
                         sample_latitude_dd,
                         distance_m,
                         within_limit))
}

#' Load station database (station.txt) from path, NODC_CONFIG, or package bundle
#'
#' @param station_file Optional path to a station.txt file.
#' @param verbose Logical; if TRUE, prints messaging about which source is used.
#'
#' @return A data frame containing the station database.
#' @keywords internal
load_station_bundle <- function(station_file = NULL, verbose = TRUE) {

  # ---- Determine station file path ----
  if (is.null(station_file)) {
    env_path <- Sys.getenv("NODC_CONFIG", unset = NA)
    if (!is.na(env_path) && dir.exists(env_path)) {
      files <- list.files(file.path(env_path),
                          pattern = "^station\\.txt$", recursive = TRUE, full.names = TRUE)
      if (length(files) > 0) {
        station_file <- files[1]
        if (verbose) message("Using station.txt from NODC_CONFIG: ", station_file)
      }
    }
    if (is.null(station_file)) {
      zip_path <- system.file("extdata", "station.zip", package = "SHARK4R")
      tmp_dir <- tempdir()
      utils::unzip(zip_path, exdir = tmp_dir)
      files <- list.files(tmp_dir, pattern = "^station\\.txt$", recursive = TRUE, full.names = TRUE)
      if (length(files) > 0) {
        station_file <- files[1]
        if (verbose) message("Using station.txt from SHARK4R bundle: ", station_file)
      }
    }
  }

  if (is.null(station_file)) {
    stop("No station.txt file found via station_file, NODC_CONFIG, or package bundle.")
  }

  # ---- Read station database ----
  station_db <- readr::read_delim(
    station_file,
    delim = "\t",
    guess_max = 2000,
    col_names = TRUE,
    locale = readr::locale(encoding = "latin1"),
    col_types = readr::cols(),
    progress = FALSE
  )

  return(station_db)
}
