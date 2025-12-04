#' Determine if positions are near land
#'
#' This function is a **wrapper/re-export** of
#' `iRfcb::ifcb_is_near_land()`. The `iRfcb` package is only required
#' if you want to actually call this function.
#'
#' Determines whether given positions are near land based on a land polygon shape file.
#'
#' @param latitudes Numeric vector of latitudes for positions.
#' @param longitudes Numeric vector of longitudes for positions. Must be the same length as `latitudes`.
#' @param distance Buffer distance (in meters) from the coastline to consider "near land." Default is 500 meters.
#' @param shape Optional path to a shapefile (`.shp`) containing coastline data. If provided,
#'   this file will be used instead of the default OBIS land vectors.
#'   A high-resolution shapefile can improve the accuracy of buffer distance calculations.
#'   You can retrieve a more detailed European coastline by setting the `source` argument to `"eea"`.
#'   Downloaded shape files are cached across R sessions in a user-specific cache directory.
#' @param source Character string indicating which default coastline source to use when `shape = NULL`.
#'   Options are `"obis"` (Ocean Biodiversity Information System, default),
#'   `"ne"` (Natural Earth 1:10 vectors) and `"eea"` (European Environment Agency).
#'   Ignored if `shape` is provided.
#' @param crs Coordinate reference system (CRS) to use for input and output.
#'   Default is EPSG code 4326 (WGS84).
#' @param remove_small_islands Logical indicating whether to remove small islands from
#'   the coastline. Useful in archipelagos. Default is `TRUE`.
#' @param small_island_threshold Area threshold in square meters below which islands
#'   will be considered small and removed, if remove_small_islands is set to `TRUE`. Default is 2 square km.
#' @param plot A boolean indicating whether to plot the points, land polygon and buffer. Default is `FALSE`.
#'
#' @return
#' If `plot = FALSE` (default), a logical vector is returned indicating whether each position
#' is near land or not, with `NA` for positions where coordinates are missing.
#' If `plot = TRUE`, a `ggplot` object is returned showing the land polygon, buffer area,
#' and position points colored by their proximity to land.
#'
#' @details
#' This function calculates a buffered area around the coastline using a polygon shapefile and
#' determines if each input position intersects with this buffer or the landmass itself.
#' By default, it uses the OBIS land vector dataset.
#'
#' The EEA shapefile is downloaded from \url{https://www.eea.europa.eu/data-and-maps/data/eea-coastline-for-analysis-2/gis-data/eea-coastline-polygon}
#' when `source = "eea"`.
#'
#' @seealso [clean_shark4r_cache()] to manually clear cached shape files.
#' @seealso [`iRfcb::ifcb_is_near_land`] for the original function.
#'
#' @examples
#' \donttest{
#' # Define coordinates
#' latitudes <- c(62.500353, 58.964498, 57.638725, 56.575338)
#' longitudes <- c(17.845993, 20.394418, 18.284523, 16.227174)
#'
#' # Call the function
#' near_land <- positions_are_near_land(latitudes, longitudes, distance = 300, crs = 4326)
#'
#' # Print the result
#' print(near_land)
#' }
#'
#' @export
positions_are_near_land <- function(latitudes,
                                    longitudes,
                                    distance = 500,
                                    shape = NULL,
                                    source = "obis",
                                    crs = 4326,
                                    remove_small_islands = TRUE,
                                    small_island_threshold = 2000000,
                                    plot = FALSE) {
  if (!requireNamespace("iRfcb", quietly = TRUE)) {
    stop("The `iRfcb` package is required for `positions_are_near_land()`.")
  }

  # Cache OBIS shapefile across sessions if source is "eea" and shape is NULL
  if (is.null(shape) && source == "obis") {
    cache_dir <- file.path(tools::R_user_dir("SHARK4R", which = "cache"), "perm")

    url <- "https://obis-resources.s3.amazonaws.com/land.gpkg"
    shape <- file.path(cache_dir, "land.gpkg")

    if (!file.exists(shape)) {
      tryCatch({
        utils::download.file(url, shape, mode = "wb")
      }, error = function(e) {
        stop("Could not download OBIS land data. Please manually download it from:\n",
             url, "\nThen provide the path to the `.gpkg` or `.shp` file using the `shape` argument. Or set `source = 'ne'` or `source = 'eea'` to use alternative vectors")
      })
    }
  }

  # Cache EEA shapefile across sessions if source is "eea" and shape is NULL
  if (is.null(shape) && source == "eea") {
    cache_dir <- file.path(tools::R_user_dir("SHARK4R", which = "cache"), "perm")

    url <- "https://www.eea.europa.eu/data-and-maps/data/eea-coastline-for-analysis-2/gis-data/eea-coastline-polygon/at_download/file"

    exdir <- file.path(tempdir(), paste0("positions_are_near_land", source))
    if (!dir.exists(exdir)) {
      dir.create(exdir, recursive = TRUE)
    }

    temp_zip <- file.path(exdir, "eea_coastline_polygon.zip")

    if (!file.exists(temp_zip)) {
      tryCatch({
        utils::download.file(url, temp_zip, mode = "wb")
      }, error = function(e) {
        stop("Could not download OBIS land data. Please manually download it from:\n",
             url, "\nThen provide the path to the `.gpkg` or `.shp` file using the `shape` argument. Or set `source = 'ne'` or `source = 'obis'` to use alternative vectors")
      })
    }

    utils::unzip(temp_zip, exdir = cache_dir)
    shape <- list.files(cache_dir, pattern = "\\.shp$", full.names = TRUE)[1]
  }

  iRfcb::ifcb_is_near_land(latitudes = latitudes,
                           longitudes = longitudes,
                           distance = distance,
                           shape = shape,
                           source = source,
                           crs = crs,
                           remove_small_islands = remove_small_islands,
                           small_island_threshold = small_island_threshold,
                           plot = plot)
}

#' Determine if points are in a specified sea basin
#'
#' This function is a **wrapper/re-export** of
#' `iRfcb::ifcb_which_basin()`. The `iRfcb` package is only required
#' if you want to actually call this function.
#'
#' This function identifies which sub-basin a set of latitude and longitude points belong to, using a user-specified or default shapefile.
#' The default shapefile includes the Baltic Sea, Kattegat, and Skagerrak basins and is included in the `iRfcb` package.
#'
#' @param latitudes A numeric vector of latitude points.
#' @param longitudes A numeric vector of longitude points.
#' @param plot A boolean indicating whether to plot the points along with the sea basins. Default is FALSE.
#' @param shape_file The absolute path to a custom polygon shapefile in WGS84 (EPSG:4326) that represents the sea basin.
#'                   Defaults to the Baltic Sea, Kattegat, and Skagerrak basins included in the `iRfcb` package.
#'
#' @return A vector indicating the basin each point belongs to, or a ggplot object if `plot = TRUE`.
#'
#' @details This function reads a pre-packaged shapefile of the Baltic Sea, Kattegat, and Skagerrak basins from the `iRfcb` package by default, or a user-supplied
#'          shapefile if provided. The shapefiles originate from SHARK (https://shark.smhi.se/en/). It sets the CRS, transforms the CRS to WGS84 (EPSG:4326) if necessary, and checks if the given points
#'          fall within the specified sea basin. Optionally, it plots the points and the sea basin polygons together.
#'
#' @seealso [`iRfcb::ifcb_which_basin`] for the original function.
#'
#' @examples
#' # Define example latitude and longitude vectors
#' latitudes <- c(55.337, 54.729, 56.311, 57.975)
#' longitudes <- c(12.674, 14.643, 12.237, 10.637)
#'
#' # Check in which Baltic sea basin the points are in
#' points_in_the_baltic <- which_basin(latitudes, longitudes)
#' print(points_in_the_baltic)
#'
#' # Plot the points and the basins
#' map <- which_basin(latitudes, longitudes, plot = TRUE)
#'
#' @export
which_basin <- function(latitudes, longitudes, plot = FALSE, shape_file = NULL) {
  if (!requireNamespace("iRfcb", quietly = TRUE)) {
    stop("The `iRfcb` package is required for `which_basin()`.")
  }
  iRfcb::ifcb_which_basin(latitudes = latitudes,
                          longitudes = longitudes,
                          plot = plot,
                          shape_file = shape_file)
}
