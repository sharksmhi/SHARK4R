#' Validate depth values against bathymetry and logical constraints
#'
#' `check_depth()` inspects one or two depth columns in a dataset and reports
#' potential problems such as missing values, non-numeric entries, or values
#' that conflict with bathymetry and shoreline information. It can also
#' validate depths against bathymetry data retrieved from a [terra::SpatRaster]
#' object or, if `bathymetry = NULL`, via the `lookup_xy()` function, which calls
#' the OBIS XY lookup API to obtain bathymetry and shore distance.
#'
#' @param data A data frame containing sample metadata, including longitude,
#'   latitude, and one or two depth columns.
#' @param depth_cols Character vector naming the depth column(s). Can be one
#'   column (e.g., `"water_depth_m"`) or two columns (minimum and maximum
#'   depth, e.g., `c("sample_min_depth_m", "sample_max_depth_m")`).
#' @param lat_col Name of the column containing latitude values. Default:
#'   `"sample_latitude_dd"`.
#' @param lon_col Name of the column containing longitude values. Default:
#'   `"sample_longitude_dd"`.
#' @param report Logical. If `TRUE` (default), returns a tibble of detected
#'   problems. If `FALSE`, returns the subset of input rows that failed
#'   validation.
#' @param depthmargin Numeric. Allowed deviation (in meters) above bathymetry
#'   before a depth is flagged as an error. Default = `0`.
#' @param shoremargin Numeric. Minimum offshore distance (in meters) required
#'   for negative depths to be considered valid. If `NA` (default), this check
#'   is skipped.
#' @param bathymetry Optional [terra::SpatRaster] object with one layer giving
#'   bathymetry values. If `NULL` (default), bathymetry and shore distance are
#'   retrieved using [lookup_xy()], which calls the OBIS XY lookup API.
#'
#' @details
#' The following checks are performed:
#' \enumerate{
#'   \item **Missing depth column** → warning
#'   \item **Empty depth column** (all values missing) → warning
#'   \item **Non-numeric depth values** → error
#'   \item **Depth exceeds bathymetry + margin** (`depthmargin`) → error
#'   \item **Negative depth at offshore locations** (beyond `shoremargin`) → error
#'   \item **Minimum depth greater than maximum depth** (if two columns supplied) → error
#'   \item **Longitude/latitude outside raster bounds** → warning
#'   \item **Missing bathymetry value** at coordinate → warning
#' }
#'
#' The function has been modified from the `obistools` package (Provoost and Bosch, 2024).
#'
#' @return
#' A tibble with one row per detected problem, containing:
#' \describe{
#'   \item{level}{Severity of the issue ("warning" or "error").}
#'   \item{row}{Row index in the input data where the issue occurred.}
#'   \item{field}{Name of the column(s) involved.}
#'   \item{message}{Human-readable description of the problem.}
#' }
#'
#' If `report = FALSE`, returns the subset of input rows that failed any check.
#'
#' @examples
#' # Example dataset with one depth column
#' test_data <- data.frame(
#'   sample_latitude_dd = c(59.3, 58.1, 57.5),
#'   sample_longitude_dd = c(18.0, 17.5, 16.2),
#'   sample_depth_m = c(10, -5, NA)
#' )
#'
#' # Validate depths using OBIS XY lookup (bathymetry = NULL)
#' \dontrun{
#' check_depth(test_data, depth_cols = "sample_depth_m")
#' }
#'
#' # Example dataset with min/max depth columns
#' test_data2 <- data.frame(
#'   sample_latitude_dd = c(59.0, 58.5),
#'   sample_longitude_dd = c(18.0, 17.5),
#'   sample_min_depth_m = c(5, 15),
#'   sample_max_depth_m = c(3, 20)
#' )
#'
#' \dontrun{
#' check_depth(test_data2, depth_cols = c("sample_min_depth_m", "sample_max_depth_m"))
#' }
#'
#' # Return only failing rows
#' \dontrun{
#' check_depth(test_data, depth_cols = "sample_depth_m", report = FALSE)
#' }
#'
#' @seealso \code{\link{lookup_xy}}, \code{\link{check_onland}}
#' @export
check_depth <- function(data,
                        depth_cols = c("sample_min_depth_m", "sample_max_depth_m"),
                        lat_col = "sample_latitude_dd",
                        lon_col = "sample_longitude_dd",
                        report = TRUE,
                        depthmargin = 0,
                        shoremargin = NA,
                        bathymetry = NULL) {
  # Check lon/lat using generalized helper
  errors <- check_lonlat(data, report = report, latcol = lat_col, loncol = lon_col)
  if (!is.null(errors) && report) return(errors)

  result <- tibble(level = character(), row = integer(), field = character(), message = character())
  original_data <- data
  data <- as.data.frame(data)

  xmin <- -180; ymin <- -90; xmax <- 180; ymax <- 90

  # Lookup bathymetry / shore distance
  if (is.null(bathymetry)) {
    lookupvalues <- lookup_xy(data, shoredistance = !is.na(shoremargin), grids = TRUE, areas = FALSE)
  } else if (inherits(bathymetry, "SpatRaster")) {
    stopifnot(terra::nlyr(bathymetry) == 1)
    lookupvalues <- if (!is.na(shoremargin)) lookup_xy(data, shoredistance = TRUE, grids = FALSE, areas = FALSE) else data.frame(row.names = seq_len(nrow(data)))
    xy <- get_xy_clean_duplicates(data, latcol = lat_col, loncol = lon_col)
    cells <- terra::cellFromXY(bathymetry, xy$uniquesp)
    values <- terra::extract(bathymetry, cells)
    lookupvalues[xy$isclean, "bathymetry"] <- values[xy$duplicated_lookup, 1]
    xmin <- terra::xmin(bathymetry); ymin <- terra::ymin(bathymetry)
    xmax <- terra::xmax(bathymetry); ymax <- terra::ymax(bathymetry)
  } else stop("bathymetry should be a SpatRaster")

  # Handle min/max depth comparison if two columns
  if (length(depth_cols) == 2 && all(depth_cols %in% colnames(data))) {
    mind <- as.numeric(as.character(data[, depth_cols[1]]))
    maxd <- as.numeric(as.character(data[, depth_cols[2]]))
    minGTmax <- !is.na(maxd) & !is.na(mind) & mind > maxd
    i <- which(minGTmax)
    if (length(i) > 0) {
      msg <- sprintf("Minimum depth [%s] is greater than maximum depth [%s]", mind[i], maxd[i])
      result <- rbind(result, tibble(level = "error", row = i, field = paste(depth_cols[1], depth_cols[2], sep = ","), message = msg))
    }
  }

  # Check each depth column
  for (column in depth_cols) {
    result <- check_depth_column(result, data, column, lookupvalues, depthmargin, shoremargin)
  }

  # Latitude/Longitude bounds check using chosen columns
  wrong_x <- is.na(data[[lon_col]]) | data[[lon_col]] < xmin | data[[lon_col]] > xmax
  wrong_y <- is.na(data[[lat_col]]) | data[[lat_col]] < ymin | data[[lat_col]] > ymax
  result <- add_depth_message(result, data, lon_col, wrong_x,
                              "Longitude [%s] is outside raster bounds (%s)", rep(paste(xmin, xmax), nrow(data)), level = "warning")
  result <- add_depth_message(result, data, lat_col, wrong_y,
                              "Latitude [%s] is outside raster bounds (%s)", rep(paste(ymin, ymax), nrow(data)), level = "warning")

  # Missing bathymetry
  result <- add_depth_message(result, data, lon_col,
                              is.na(lookupvalues$bathymetry),
                              "No bathymetry value found for coordinate (%s, %s)",
                              level = "warning", extra_data = data[[lat_col]])

  if (!report) {
    result <- original_data[sort(unique(stats::na.omit(result$row))), ]
  }
  return(result)
}

add_depth_message <- function(result, data, columns, i, message, extra_data=NULL, level='warning') {
  # If `i` is a logical vector, convert it to row indices
  if(is.logical(i)) {
    i <- which(i)
  }

  # Only proceed if there are any rows to report
  if (length(i) > 0) {
    # Prepare a list of arguments for sprintf()
    args <- list('fmt' = message)

    # Add the column values for the rows being reported to the args list
    for(column in columns) {
      args[[column]] <- data[i,column]
    }

    # Add extra data (e.g., latitude or shore distance) if provided
    if(length(extra_data) > 0) {
      args[['extra_data']] <- extra_data[i]
    }

    # Construct the formatted message using sprintf with the collected arguments
    message <- do.call(sprintf, args)

    # Append a new tibble row for each detected issue
    result <- rbind(
      result,
      tibble(
        level = level,                    # Severity of the issue
        row = i,                          # Row indices in the original data
        field = rep(columns, length(i)),  # Columns involved (recycled for each row)
        message = message                  # Human-readable message
      )
    )
  }

  return(result)
}

check_depth_column <- function(result, data, column, lookupvalues, depthmargin, shoremargin) {
  # Check if the specified depth column exists in the data
  if (column %in% colnames(data)) {
    # Convert column values to numeric (handles factors/characters)
    depths <- as.numeric(as.character(data[,column]))

    # Warn if the entire column is empty
    if(all(is.na(data[[column]]) | data[[column]] == '')) {
      result <- rbind(result, tibble(level = 'warning', row = NA, field=column,
                                     message = paste('Column',column,'empty')))
    }

    # Flag values that are non-numeric but not empty
    invalid <- is.na(depths) & data[,column] != ''
    result <- add_depth_message(result, data, column, invalid,
                                'Depth value (%s) is not numeric and not empty')

    # Check for depth values exceeding bathymetry + margin
    gridwrong <- !is.na(depths) & depths > 0 & !is.na(lookupvalues$bathymetry) &
      depths > (lookupvalues$bathymetry + rep(depthmargin, nrow(lookupvalues)))
    result <- add_depth_message(result, data, column, gridwrong,
                                paste0('Depth value (%s) is greater than the value found in the bathymetry raster (depth=%0.1f, margin=',depthmargin,')'),
                                lookupvalues$bathymetry)

    # Check for negative depths that are offshore beyond the allowed margin
    if(!is.na(shoremargin)) {
      negativewrong <- !is.na(depths) & depths < 0 &
        ((lookupvalues$shoredistance - rep(shoremargin, nrow(lookupvalues))) > 0)
      result <- add_depth_message(result, data, column, negativewrong,
                                  paste0('Depth value (%s) is negative for offshore points (shoredistance=%s, margin=', shoremargin,')'),
                                  lookupvalues$shoredistance)
    }

  } else {
    # Warn if the specified column does not exist
    result <- rbind(result, tibble(level = 'warning', row = NA, field = column,
                                   message = paste('Column', column, 'missing')))
  }

  return(result)
}
