#' Lookup spatial information for geographic points
#'
#' Retrieves shore distance, environmental grids, and area values for given coordinates.
#' Coordinates may be supplied either through a data frame or as separate numeric vectors.
#'
#' @param data Optional data frame containing coordinate columns. The expected names are
#' \code{sample_longitude_dd} and \code{sample_latitude_dd}. These must be numeric and fall
#' within valid geographic ranges.
#' @param lon Optional numeric vector of longitudes. Must be supplied together with \code{lat}
#' when used. Ignored when a data frame is provided unless both \code{lon} and \code{lat} are set.
#' @param lat Optional numeric vector of latitudes. Must be supplied together with \code{lon}
#' when used.
#' @param shoredistance Logical; if \code{TRUE}, distance to the nearest shore is included.
#' @param grids Logical; if \code{TRUE}, environmental grid values are included.
#' @param areas Logical or numeric. When logical, \code{TRUE} requests area values at zero radius,
#' and \code{FALSE} disables area retrieval. A positive integer specifies the search radius
#' in meters for area values.
#' @param as_data_frame Logical; if \code{TRUE}, the result is returned as a data frame.
#' When \code{FALSE}, the result is returned as a list.
#'
#' @return A data frame or list, depending on \code{as_data_frame}. Invalid coordinates produce
#' \code{NA} entries (data frame) or \code{NULL} elements (list). Duplicate input coordinates
#' return repeated results.
#'
#' @details
#' - When both vector inputs and a data frame are provided, the vector inputs take precedence.
#' - Coordinates are validated and cleaned before lookup, and only unique values are queried.
#' - Queries are processed in batches to avoid overloading the remote service.
#' - Area retrieval accepts either a logical flag or a radius. A radius of zero corresponds to
#' requesting a single area value.
#' - Final results are reordered to match the original input positions.
#' - The function has been modified from the `obistools` package (Provoost and Bosch, 2024).
#'
#' @examples
#' \donttest{
#' # Using a data frame
#' df <- data.frame(sample_longitude_dd = c(10.9, 18.3),
#'                  sample_latitude_dd = c(58.1, 58.3))
#' lookup_xy(df)
#'
#' # Area search within a radius
#' lookup_xy(df, areas = 500)
#'
#' # Using separate coordinate vectors
#' lookup_xy(lon = c(10.9, 18.3), lat = c(58.1, 58.3))
#' }
#'
#' @references Provoost P, Bosch S (2024). “obistools: Tools for data enhancement and quality control” Ocean Biodiversity Information System. Intergovernmental Oceanographic Commission of UNESCO. R package version 0.1.0, <https://iobis.github.io/obistools/>.
#'
#' @seealso
#' \code{\link{check_onland}}, \code{\link{check_depth}}, \url{https://iobis.github.io/xylookup/} – OBIS xylookup web service
#'
#' @export
lookup_xy <- function(data = NULL, lon = NULL, lat = NULL,
                      shoredistance=TRUE, grids=TRUE,  areas=FALSE,  as_data_frame=TRUE) {

  # Handle vector input
  if (!is.null(lon) || !is.null(lat)) {
    if (is.null(lon) || is.null(lat)) {
      stop("Both lon and lat must be provided")
    }
    if (!is.null(data)) {
      warning("Data frame ignored because lon and lat were supplied")
    }
    data <- data.frame(
      sample_longitude_dd = lon,
      sample_latitude_dd = lat,
      stringsAsFactors = FALSE
    )
  }

  # Require that 'data' exists at this point
  if (is.null(data)) {
    stop("Provide either a data frame or lon and lat vectors")
  }

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
