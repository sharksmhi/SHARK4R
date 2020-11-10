#' Create a Leaflet map.
#'
#' @param data The data frame.
#' @param provider Tile provider, see
#'   https://leaflet-extras.github.io/leaflet-providers/preview/.
#' @param popup The field to display as a popup or a character vector with as
#'   many elements as there are rows, by default the row names are shown.
#' @return HTML widget object.
#' @export
plot_map_leaflet <- function(data) {
  coord <- data %>% 
    select(station_name, sample_longitude_dd, sample_latitude_dd) %>% 
    rename(STATION = station_name, LONG = sample_longitude_dd, LAT = sample_latitude_dd)
  m <- leaflet(coord) %>%
    addProviderTiles("Esri.OceanBasemap") %>%
    addCircleMarkers(~LONG, ~LAT, popup = ~STATION, radius = 3, weight = 1, fillColor = "#FF368B", color = "#FF368B", opacity = 1, fillOpacity = 0.1, clusterOptions = markerClusterOptions()) 
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
    rename(STATION = STATN, LONG = LONGI, LAT = LATIT)
  m <- leaflet(coord) %>%
    addProviderTiles("Esri.OceanBasemap") %>%
    addCircleMarkers(~LONG, ~LAT, popup = ~STATION, radius = 3, weight = 1, fillColor = "#FF368B", color = "#FF368B", opacity = 1, fillOpacity = 0.1, clusterOptions = markerClusterOptions()) 
  return(m)
}
