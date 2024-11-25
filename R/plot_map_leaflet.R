#' Create a Leaflet map.
#'
#' @param data The data frame.
#' @param provider Tile provider, see
#'   https://leaflet-extras.github.io/leaflet-providers/preview/. Default is `Esri.OceanBasemap`.
#' @return HTML widget object.
#' @export
plot_map_leaflet <- function(data, provider = "Esri.OceanBasemap") {
  coord = data %>%
    select(station_name, sample_longitude_dd, sample_latitude_dd) %>%
    rename(STATION = station_name, LON = sample_longitude_dd, LAT = sample_latitude_dd) %>%
    distinct()

  m = leaflet() %>%
    addProviderTiles(provider,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addMarkers(data = coord, popup = ~STATION)
  return(m)
}
#' Create a Leaflet map.
#'
#' @param data The data frame.
#' @param provider Tile provider, see
#'   https://leaflet-extras.github.io/leaflet-providers/preview/. Default is `Esri.OceanBasemap`.
#' @return HTML widget object.
#' @export
plot_map_leaflet_deliv <- function(data, provider = "Esri.OceanBasemap") {
  coord <- data %>%
    select(STATN, LONGI, LATIT) %>%
    rename(STATION = STATN, LON = LONGI, LAT = LATIT) %>%
    distinct()

  m = leaflet() %>%
    addProviderTiles(provider,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addMarkers(data = coord, popup = ~STATION)
  return(m)
}
