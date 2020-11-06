#' Create a Leaflet map.
#'
#' @param data The data frame.
#' @param provider Tile provider, see
#'   https://leaflet-extras.github.io/leaflet-providers/preview/.
#' @param popup The field to display as a popup or a character vector with as
#'   many elements as there are rows, by default the row names are shown.
#' @return HTML widget object.
#' @examples
#' plot_map_leaflet(abra)
#' plot_map_leaflet(abra, popup = "datasetID")
#' plot_map_leaflet(abra, popup = head(colnames(abra)))
#' @export
plot_map_leaflet <- function(data, provider = "Esri.OceanBasemap", popup = NULL) {
  check_lonlat(data, FALSE)
  if (!is.null(popup) && all(popup %in% names(data))) {
    if(length(popup) == 1) {
      popupdata <- as.character(data[,popup])
    } else {
      popupdata <- apply(data[,popup], 1, knitr::kable, format="html")
      popupdata <- gsub("<thead>.*</thead>", "", popupdata) # remove column header
    }
  } else if (length(popup) == NROW(data)) {
    popupdata <- as.character(popup)
  } else {
    popupdata <- as.character(rownames(data))
  }

  m <- leaflet(data) %>%
    addProviderTiles(provider) %>%
    addCircleMarkers(~sample_longitude_dd, ~sample_latitude_dd, popup = popupdata, radius = 3, weight = 1, fillColor = "#FF368B", color = "#FF368B", opacity = 1, fillOpacity = 0.1)
  return(m)
}
