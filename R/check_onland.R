#' Check whether points are located on land
#'
#' Identifies records whose coordinates fall on land, optionally applying a buffer to allow
#' points near the coast. The function supports both offline and online modes:
#'
#' * **Offline mode (`offline = TRUE`)**: uses a local simplified shoreline from a cached
#'   geopackage (`land.gpkg`). If the file does not exist, it is downloaded automatically and cached across R sessions.
#' * **Online mode (`offline = FALSE`)**: uses the OBIS web service to determine distance to the shore.
#'
#' @param data A data frame containing at least `sample_longitude_dd` and `sample_latitude_dd`.
#'   These columns must be numeric and within valid ranges (-180 to 180 for longitude, -90 to 90 for latitude).
#' @param land Optional `sf` object containing land polygons. Used only in offline mode.
#' @param report Logical; if `TRUE`, returns a tibble listing rows on land and warnings.
#'   If `FALSE` (default), returns a subset of `data` containing only records on land.
#' @param buffer Numeric; distance in meters inland for which points are still considered valid.
#'   Only used in online mode. Default is 0.
#' @param offline Logical; if `TRUE`, the function uses the local cached shoreline. If `FALSE` (default),
#'   the OBIS web service is queried.
#'
#' @return If `report = TRUE`, a tibble with columns:
#'   \itemize{
#'     \item `field`: always `NA` (placeholder for future extension)
#'     \item `level`: `"warning"` for all flagged rows
#'     \item `row`: row numbers in `data` flagged as located on land
#'     \item `message`: description of the issue
#'   }
#'   If `report = FALSE`, returns a subset of `data` with only the flagged rows.
#'
#' @details
#' - The function first validates longitude and latitude columns using.
#' - In offline mode, `land` is converted to a `terra::SpatVector` and points are tested using
#'   `terra::relate()` to detect intersections with land polygons. The `buffer` parameter
#'   is ignored in this mode.
#' - In online mode, `lookup_xy()` is used to fetch distances to the nearest shoreline.
#'   Points with negative distances (inland) beyond the `buffer` are flagged.
#' - Warnings are issued if `land` is provided in online mode or `buffer` is used in offline mode.
#'
#' @examples
#' \dontrun{
#' # Report points on land with a 100 m buffer
#' report <- check_onland(abra, report = TRUE, buffer = 100)
#' print(report)
#'
#' # Plot flagged points on a map
#' plot_map_leaflet(abra[report$row, ], popup = "id")
#'
#' # Remove points on land
#' ok <- abra[-report$row, ]
#' ok <- check_onland(abra, report = FALSE, buffer = 100)
#' print(nrow(ok))
#' }
#'
#' @seealso \code{\link{check_depth}}, \code{\link{lookup_xy}}, \code{\link{clean_shark4r_cache}}
#' @export
check_onland <- function(data, land = NULL, report = FALSE, buffer = 0, offline = FALSE) {
  errors <- check_lonlat(data, report)
  if (NROW(errors) > 0 && report) {
    return(errors)
  }
  if(!is.null(land) && !offline) warning("The land parameter is not supported when offline = FALSE")
  if (buffer !=0 && offline) warning("The buffer parameter is not supported when offline = TRUE")

  if (offline && is.null(land)) {
    cache_dir <- file.path(tools::R_user_dir("SHARK4R", which = "cache"), "perm")
    landpath <- file.path(cache_dir, 'land.gpkg')
    if(!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    if (!file.exists(landpath)) {
      utils::download.file("https://obis-resources.s3.amazonaws.com/land.gpkg", landpath, mode = "wb")
    }
    land <- sf::read_sf(landpath) %>% terra::vect()
  }

  if (offline) {
    data_vect <- data %>% terra::vect(geom = c("sample_longitude_dd", "sample_latitude_dd"), crs = "EPSG:4326")
    i <- which(colSums(terra::relate(land, data_vect, "intersects")) > 0)
  } else {
    shoredistances <- lookup_xy(data, shoredistance = TRUE, grids = FALSE, areas = FALSE, as_data_frame = TRUE)
    i <- which(as.vector(shoredistances$shoredistance) < (-1*buffer))
  }
  if (report) {
    if (length(i) > 0) {
      return(tibble(
        field = NA,
        level = "warning",
        row = i,
        message = paste0("Coordinates are located on land")
      ))
    } else {
      return(tibble())
    }
  } else {
    return(data[i,])
  }
}
