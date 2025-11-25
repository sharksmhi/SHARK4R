#' Determine if positions are near land
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated. Users are encouraged to use \code{\link{positions_are_near_land}} instead.
#'
#' Determines whether given positions are near land based on a coastline shape file.
#' The Natural Earth 1:50m land vectors are included as default shapefile in `SHARK4R`.
#'
#' @param latitudes Numeric vector of latitudes for positions.
#' @param longitudes Numeric vector of longitudes for positions.
#' @param distance Buffer distance in meters around the coastline. Default is 500 m.
#' @param shape Optional path to a shapefile containing coastline data. If provided,
#'   the function will use this shapefile instead of the default Natural Earth 1:50m land vectors.
#'   Using a more detailed shapefile allows for a smaller buffer distance.
#'   For detailed European coastlines, download polygons from the EEA at
#'   \url{https://www.eea.europa.eu/data-and-maps/data/eea-coastline-for-analysis-2/gis-data/eea-coastline-polygon}.
#'   For more detailed world maps, download from Natural Earth at
#'   \url{https://www.naturalearthdata.com/downloads/10m-physical-vectors/}.
#' @param crs Coordinate reference system (CRS) to use for positions and output.
#'   Default is EPSG code 4326 (WGS84).
#' @param utm_zone UTM zone for buffering the coastline. Default is 33 (between 12°E and 18°E, northern hemisphere).
#' @param remove_small_islands Logical indicating whether to remove small islands from
#'   the coastline if a custom shapefile is provided. Default is TRUE.
#' @param small_island_threshold Area threshold in square meters below which islands
#'   will be considered small and removed, if remove_small_islands is set to TRUE. Default is 2 sqkm.
#'
#' @return Logical vector indicating whether each position is near land.
#'
#' @details
#' This function calculates a buffered area around the coastline and checks if
#' given positions (specified by longitudes and latitudes) are within this buffer
#' or intersect with land.
#'
#' This function is re-exported from the `iRfcb` package available at \url{https://github.com/EuropeanIFCBGroup/iRfcb}
#'
#' @examples
#' \donttest{
#' # Define coordinates
#' latitudes <- c(62.500353, 58.964498, 57.638725, 56.575338)
#' longitudes <- c(17.845993, 20.394418, 18.284523, 16.227174)
#'
#' # Call the function
#' near_land <- ifcb_is_near_land(latitudes, longitudes, distance = 300, crs = 4326)
#'
#' # Print the result
#' print(near_land)
#' }
#'
#' @keywords internal
#' @export
ifcb_is_near_land <- function(latitudes,
                              longitudes,
                              distance = 500,
                              shape = NULL,
                              crs = 4326,
                              utm_zone = 33,
                              remove_small_islands = TRUE,
                              small_island_threshold = 2000000) {

  lifecycle::deprecate_warn("1.0.0", "ifcb_is_near_land()", "positions_are_near_land()")

  positions_are_near_land(latitudes = latitudes,
                          longitudes = longitudes,
                          distance = distance,
                          shape = shape,
                          source = "ne",
                          crs = crs,
                          remove_small_islands = remove_small_islands,
                          small_island_threshold = small_island_threshold,
                          plot = FALSE)
}
