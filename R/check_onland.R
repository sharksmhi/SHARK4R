#' Check whether points are located on land
#'
#' Identifies records whose coordinates fall on land, optionally applying a buffer to allow
#' points near the coast.
#'
#' @param data A data frame containing at least `sample_longitude_dd` and `sample_latitude_dd`.
#'   Both columns must be numeric, within valid ranges (longitude: -180 to 180, latitude: -90 to 90),
#'   and use WGS84 coordinates (EPSG:4326).
#' @param land Optional `sf` object containing land polygons. Used only in offline mode.
#' @param report Logical; if `TRUE`, returns a tibble listing rows on land and warnings.
#'   If `FALSE` (default), returns a subset of `data` containing only records on land.
#' @param buffer Numeric; distance in meters inland for which points are still considered valid.
#'   Only used in online mode. Default is 0.
#' @param offline Logical; if `TRUE`, the function uses the local cached shoreline (if available). If `FALSE` (default),
#'   the OBIS web service is queried.
#' @param plot_leaflet Logical; if `TRUE`, returns a leaflet map showing points colored by
#'   whether they are on land (red) or in water (green). Default is `FALSE`.
#' @param only_bad Logical; if `TRUE` and `plot_leaflet = TRUE`, only points on land (red) are plotted.
#'   Default is `FALSE`, meaning all points are plotted.
#'
#' @return If `report = TRUE`, a tibble with columns:
#'   \itemize{
#'     \item `field`: always `NA` (placeholder for future extension)
#'     \item `level`: `"warning"` for all flagged rows
#'     \item `row`: row numbers in `data` flagged as located on land
#'     \item `message`: description of the issue
#'   }
#'   If `report = FALSE` and `plot_leaflet = FALSE`, returns a subset of `data` with only the flagged rows.
#'   If `plot_leaflet = TRUE`, returns a leaflet map showing points on land (red) and in water (green),
#'   unless `only_bad = TRUE`, in which case only red points are plotted.
#'
#' @details
#' The function supports both offline and online modes:
#'
#' * **Offline mode (`offline = TRUE`)**: uses a local simplified shoreline from a cached
#'   geopackage (`land.gpkg`). If the file does not exist, it is downloaded automatically and cached across R sessions.
#' * **Online mode (`offline = FALSE`)**: uses the OBIS web service to determine distance to the shore.
#'
#' The function assumes all coordinates are in WGS84 (EPSG:4326). Supplying coordinates
#' in a different CRS will result in incorrect intersection tests.
#'
#' Optionally, a leaflet map can be plotted. Points on land are displayed as red markers,
#' while points in water are green. If \code{only_bad = TRUE}, only the red points (on land) are plotted.
#'
#' @examples
#' # Example data frame with coordinates
#' example_data <- data.frame(
#'   sample_latitude_dd = c(59.3, 58.1, 57.5),
#'   sample_longitude_dd = c(18.6, 17.5, 16.7)
#' )
#'
#' # Report points on land with a 100 m buffer
#' report <- check_onland(example_data, report = TRUE, buffer = 100)
#' print(report)
#'
#' # Plot all points colored by land/water
#' map <- check_onland(example_data, plot_leaflet = TRUE)
#'
#' # Plot only bad points on land
#' map_bad <- check_onland(example_data, plot_leaflet = TRUE, only_bad = TRUE)
#'
#' # Remove points on land by adding a buffer of 2000 m
#' ok <- check_onland(example_data, report = FALSE, buffer = 2000)
#' print(nrow(ok))
#'
#' @export
check_onland <- function(data, land = NULL, report = FALSE, buffer = 0, offline = FALSE,
                         plot_leaflet = FALSE, only_bad = FALSE) {
  errors <- check_lonlat(data, report)
  if (NROW(errors) > 0 && report) return(errors)

  if(!is.null(land) && !offline) warning("The land parameter is not supported when offline = FALSE")
  if (buffer !=0 && offline) warning("The buffer parameter is not supported when offline = TRUE")

  if (offline && is.null(land)) {
    cache_dir <- file.path(tools::R_user_dir("SHARK4R", which = "cache"), "perm")
    landpath <- file.path(cache_dir, 'land.gpkg')
    if(!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    if (!file.exists(landpath)) {
      utils::download.file("https://obis-resources.s3.amazonaws.com/land.gpkg", landpath, mode = "wb")
    }
    land <- tryCatch({
      sf::read_sf(landpath) %>% terra::vect()
    }, error = function(e) {
      stop(
        "Failed to read land shapefile at: ", landpath, "\n", "Error: ", e$message, "\n\n",
        "Try clearing the cache:\n", "  clean_shark4r_cache(days = 0, clear_perm_cache = TRUE)",
        call. = FALSE
      )
    })
  }

  if (offline) {
    pts_sf <- sf::st_as_sf(
      data,
      coords = c("sample_longitude_dd", "sample_latitude_dd"),
      crs = 4326
    )
    # convert to terra without setting CRS
    data_vect <- terra::vect(pts_sf)
    i <- which(colSums(terra::relate(land, data_vect, "intersects")) > 0)
  } else {
    shoredistances <- lookup_xy(data, shoredistance = TRUE, grids = FALSE, areas = FALSE, as_data_frame = TRUE)
    i <- which(as.vector(shoredistances$shoredistance) < (-1*buffer))
  }

  if (plot_leaflet) {
    data_plot <- data %>%
      dplyr::mutate(
        bad_point = seq_len(nrow(data)) %in% i,
        color = ifelse(bad_point, "red", "green"),
        popup_text = paste0("Row: ", seq_len(nrow(data)),
                            "<br>Lat: ", sample_latitude_dd,
                            "<br>Lon: ", sample_longitude_dd)
      )

    # Optionally keep only bad points
    if (only_bad) {
      data_plot <- dplyr::filter(data_plot, bad_point)
      data_good <- NULL
      data_bad <- data_plot
    } else {
      data_good <- dplyr::filter(data_plot, !bad_point)
      data_bad  <- dplyr::filter(data_plot, bad_point)
    }

    # Icons
    icons_good <- leaflet::awesomeIcons(
      icon = "map-marker",
      iconColor = "white",
      markerColor = "green",
      library = "fa"
    )

    icons_bad <- leaflet::awesomeIcons(
      icon = "map-marker",
      iconColor = "white",
      markerColor = "red",
      library = "fa"
    )

    # Build map
    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addTiles(
        urlTemplate = "https://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
        attribution = "&copy; OpenSeaMap contributors, OpenStreetMap contributors",
        options = providerTileOptions(noWrap = TRUE)
      )

    if (!only_bad && nrow(data_good) > 0) {
      m <- m %>%
        addAwesomeMarkers(data = data_good,
                          lng = ~sample_longitude_dd, lat = ~sample_latitude_dd,
                          icon = icons_good,
                          popup = ~popup_text,
                          clusterOptions = leaflet::markerClusterOptions())
    }

    if (nrow(data_bad) > 0) {
      m <- m %>%
        addAwesomeMarkers(data = data_bad,
                          lng = ~sample_longitude_dd, lat = ~sample_latitude_dd,
                          icon = icons_bad,
                          popup = ~popup_text)
    }

    return(m)
  }

  if (report) {
    if (length(i) > 0) {
      return(tibble(
        field = NA,
        level = "warning",
        row = i,
        message = "Coordinates are located on land"
      ))
    } else {
      return(tibble())
    }
  } else {
    return(data[i, ])
  }
}
