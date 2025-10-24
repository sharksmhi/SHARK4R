#' Lookup spatial data for a set of points
#'
#' Retrieves spatial information (e.g., distance to shore, environmental grids, and area data)
#' for a set of geographic coordinates. The function handles invalid or duplicate coordinates
#' automatically and supports returning results as either a data frame or a list.
#'
#' @param data A data frame containing geographic coordinates. The required columns are
#'   \code{sample_longitude_dd} and \code{sample_latitude_dd}. These must be numeric and
#'   within valid ranges (-180 to 180 for longitude, -90 to 90 for latitude).
#' @param shoredistance Logical; if \code{TRUE} (default), the distance to the nearest shore is returned.
#' @param grids Logical; if \code{TRUE} (default), environmental grid values (e.g., temperature, bathymetry)
#'   are returned.
#' @param areas Logical or numeric; if \code{TRUE}, area values are returned for points at a 0 m radius.
#'   If a positive integer is provided, all areas within that radius (in meters) are returned. Default is \code{FALSE}.
#' @param as_data_frame Logical; if \code{TRUE} (default), results are returned as a data frame with one row per input coordinate.
#'   If \code{FALSE}, results are returned as a list.
#'
#' @return Either a data frame or a list with the requested spatial data:
#' \itemize{
#'   \item For data frame output, each row corresponds to the input coordinates. Columns include \code{shoredistance},
#'     environmental grids, and \code{areas} (if requested). Invalid coordinates are filled with \code{NA}.
#'   \item For list output, each element corresponds to one input coordinate. Invalid coordinates are \code{NULL}.
#' }
#'
#' @details
#' - The function first cleans the coordinates, removing invalid or missing values
#'   and identifying unique points to avoid redundant lookups.
#' - Coordinates are queried in chunks of 25,000 to avoid overloading the OBIS web service.
#' - When \code{areas} is a positive integer, all area values within that radius are returned. A value of \code{TRUE}
#'   is equivalent to 0 m, while \code{FALSE} disables area retrieval.
#' - Results are mapped back to the original input order, and duplicates in the input are correctly handled.
#' - The function has been modified from the `obistools` package (Provoost and Bosch, 2024).
#'
#' @examples
#' \dontrun{
#' # Example data frame
#' data <- data.frame(sample_longitude_dd = c(10.983229, 18.265451),
#'                    sample_latitude_dd = c(58.121034, 58.331616))
#'
#' # Retrieve shore distances and environmental grids for a dataset
#' xy_data <- lookup_xy(data, shoredistance = TRUE, grids = TRUE, areas = FALSE)
#'
#' # Retrieve area data within a 500-meter radius
#' xy_areas <- lookup_xy(data, shoredistance = FALSE, grids = FALSE, areas = 500)
#'
#' # Get results as a list instead of a data frame
#' xy_list <- lookup_xy(data, shoredistance = TRUE, grids = TRUE, areas = FALSE, as_data_frame = FALSE)
#' }
#'
#'
#' @references Provoost P, Bosch S (2024). “obistools: Tools for data enhancement and quality control” Ocean Biodiversity Information System. Intergovernmental Oceanographic Commission of UNESCO. R package version 0.1.0, <https://iobis.github.io/obistools/>.
#'
#' @seealso
#' \code{\link{check_onland}}, \code{\link{check_depth}}, \url{https://iobis.github.io/xylookup/} – OBIS xylookup web service
#'
#' @export
lookup_xy <- function(data, shoredistance=TRUE, grids=TRUE, areas=FALSE, as_data_frame=TRUE) {
  xy <- get_xy_clean_duplicates(data)
  if(NROW(xy$uniquesp) == 0) {
    output <- data.frame(row.names=seq_len(NROW(data)))
    if(!as_data_frame) {
      # Create a list with only NULL values
      output <- list()
      output[[NROW(data)+1]] <- NA
      output[[NROW(data)+1]] <- NULL
    }
    return(output)
  } else {
    areasdistancewithin = 0
    if(is.numeric(areas) && as.numeric(as.integer(areas)) == areas) {
      if(areas < 0) {
        warning("Areas parameter should be TRUE/FALSE or a positive integer")
      } else {
        areasdistancewithin = areas
        areas = TRUE
      }
    } else if(!is.logical(areas)) {
      warning("Areas parameter should be TRUE/FALSE or a positive integer")
      areas = FALSE
    }

    # Prepare message
    splists <- unname(split(as.matrix(xy$uniquesp), seq(nrow(xy$uniquesp))))
    # Divide in chunks of 25000 coordinates
    chunks <- split(splists, ceiling(seq_along(splists)/25000))

    content_chunks <- lapply(chunks, function(chunk) {
      msg <- jsonlite::toJSON(list(points=chunk, shoredistance=shoredistance, grids=grids, areas=areas, areasdistancewithin=areasdistancewithin), auto_unbox=TRUE)
      raw_content <- lookup_xy_chunk(msg)
      jsonlite::fromJSON(rawToChar(raw_content), simplifyVector = FALSE)
    })
    content <- unlist(content_chunks, recursive = FALSE, use.names = FALSE)
    if(as_data_frame) {
      # Convert to dataframe while ensuring that:
      # 1. area is a nested list in the data.frame
      # 2. grids and shoredistance results are columns
      # 3. NA values are written for coordinates that were not OK (!isclean)
      # 4. results for the non-unique coordinates are duplicated
      content <- jsonlite::fromJSON(jsonlite::toJSON(content, auto_unbox = TRUE), simplifyVector = TRUE)
      content <- as.data.frame(content)
      df <- data.frame(row.names = seq_len(NROW(content)))
      if (shoredistance) {
        df <- cbind(df, shoredistance=content[,"shoredistance", drop=TRUE])
      }
      if (grids) {
        df <- merge(df, content[,"grids", drop=TRUE], by=0, sort = FALSE)[, -1, drop=FALSE]
      }
      if (areas) {
        df <- merge(df, content[,"areas", drop=TRUE], by=0, sort = FALSE)[, -1, drop=FALSE]
      }
      output <- stats::setNames(data.frame(matrix(ncol=NCOL(df), nrow=NROW(data))), colnames(df))
      output[xy$isclean,] <- df[xy$duplicated_lookup, , drop=FALSE]
    } else {
      # Convert to list, keep into account invalid coordinates and duplicate coordinates
      output <- list()
      output[xy$isclean] <- content[xy$duplicated_lookup]
    }
    return(output)
  }
}

lookup_xy_chunk <- function(msg) {
  # Call service
  url <- getOption("obistools_xylookup_url", "https://api.obis.org/xylookup")
  service_call(url, msg)
}
