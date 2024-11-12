#' Create a Leaflet map.
#'
#' @param data The data frame.
#' @param provider Tile provider, see
#'   https://leaflet-extras.github.io/leaflet-providers/preview/.
#' @param popup The field to display as a popup or a character vector with as
#'   many elements as there are rows, by default the row names are shown.
#' @return HTML widget object.
#' @export
#' @importFrom leaflet leaflet addProviderTiles providerTileOptions addMarkers
plot_map_leaflet <- function(data) {
  coord = data %>% 
    select(station_name, sample_longitude_dd, sample_latitude_dd) %>% 
    rename(STATION = station_name, LON = sample_longitude_dd, LAT = sample_latitude_dd) %>% 
    distinct()
  
  m = leaflet() %>%
    addProviderTiles("Esri.OceanBasemap",
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addMarkers(data = coord, popup = ~STATION)
  return(m)
}
#' Create a Leaflet map.
#'
#' @param data The data frame.
#' @param provider Tile provider, see
#'   https://leaflet-extras.github.io/leaflet-providers/preview/.
#' @param popup The field to display as a popup or a character vector with as
#'   many elements as there are rows, by default the row names are shown.
#' @return HTML widget object.
#' @export
plot_map_leaflet_deliv <- function(data) {
  coord <- data %>% 
    select(STATN, LONGI, LATIT) %>% 
    rename(STATION = STATN, LON = LONGI, LAT = LATIT) %>% 
    distinct()
  
  m = leaflet() %>%
    addProviderTiles("Esri.OceanBasemap",
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addMarkers(data = coord, popup = ~STATION)
  return(m)
}
