#' Create an interactive Leaflet map of sampling stations
#'
#' Generates an interactive map using the `leaflet` package, plotting sampling
#' stations from a data frame. The function automatically detects column names
#' for station, longitude, and latitude, supporting both standard and
#' delivery-style datasets.
#'
#' @param data A data frame containing station coordinates and names. The function
#'   accepts either:
#'   - Standard format: `station_name`, `sample_longitude_dd`, `sample_latitude_dd`
#'   - Delivery format: `STATN`, `LONGI`, `LATIT`
#' @param provider Character. The tile provider to use for the map background.
#'   See available providers at
#'   \url{https://leaflet-extras.github.io/leaflet-providers/preview/}.
#'   Defaults to `"CartoDB.Positron"`.
#'
#' @return An HTML widget object (`leaflet` map) that can be printed or displayed
#'   in R Markdown or Shiny applications.
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   station_name = c("Station A", "Station B"),
#'   sample_longitude_dd = c(10.0, 10.5),
#'   sample_latitude_dd = c(59.0, 59.5)
#' )
#'
#' # Plot points on map
#' map <- plot_map_leaflet(df)
#'
#' # Example data in SHARK delivery format
#' df_deliv <- data.frame(
#'   STATN = c("Station A", "Station B"),
#'   LONGI = c(10.0, 10.5),
#'   LATIT = c(59.0, 59.5)
#' )
#'
#' # Plot points on map
#' map_deliv <- plot_map_leaflet(df_deliv)
#'
#' @export
plot_map_leaflet <- function(data, provider = "CartoDB.Positron") {
  # Determine which column naming style is present
  if (all(c("station_name", "sample_longitude_dd", "sample_latitude_dd") %in% names(data))) {
    coord <- data %>%
      select(station_name, sample_longitude_dd, sample_latitude_dd) %>%
      rename(STATION = station_name, LON = sample_longitude_dd, LAT = sample_latitude_dd)
  } else if (all(c("STATN", "LONGI", "LATIT") %in% names(data))) {
    coord <- data %>%
      select(STATN, LONGI, LATIT) %>%
      rename(STATION = STATN, LON = LONGI, LAT = LATIT)
  } else {
    stop("Data frame must contain either standard columns (station_name, sample_longitude_dd, sample_latitude_dd) ",
         "or delivery-style columns (STATN, LONGI, LATIT).")
  }

  # Remove duplicates
  coord <- coord %>% distinct()

  # Create the Leaflet map
  m <- leaflet() %>%
    addProviderTiles(provider, options = providerTileOptions(noWrap = TRUE)) %>%
    addMarkers(data = coord, lng = ~LON, lat = ~LAT, popup = ~STATION) %>%
    addTiles(
      urlTemplate = "https://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
      attribution = "&copy; OpenSeaMap contributors, OpenStreetMap contributors",
      options = providerTileOptions(noWrap = TRUE)
    )

  return(m)
}
#' Create a Leaflet map.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and has been replaced by [plot_map_leaflet()].
#'
#' @param data The data frame.
#' @param provider Tile provider, see
#'   https://leaflet-extras.github.io/leaflet-providers/preview/. Default is `Esri.OceanBasemap`.
#' @return HTML widget object.
#' @keywords internal
#' @export
plot_map_leaflet_deliv <- function(data, provider = "Esri.OceanBasemap") {
  lifecycle::deprecate_warn("1.0.0", "plot_map_leaflet_deliv()", "plot_map_leaflet()")
  plot_map_leaflet(data = data, provider = provider)
}
