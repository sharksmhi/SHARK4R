#' Validate depth values against bathymetry and logical constraints
#'
#' `check_depth()` inspects one or two depth columns in a dataset and reports
#' potential problems such as missing values, non-numeric entries, or values
#' that conflict with bathymetry and shoreline information. It can also
#' validate depths against bathymetry data retrieved from a [terra::SpatRaster]
#' object or, if `bathymetry = NULL`, via the `lookup_xy()` function, which calls
#' the OBIS XY lookup API to obtain bathymetry (using EMODnet Bathymetry) and shore distance.
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
#'   \item **Non-numeric depth values** → warning
#'   \item **Depth exceeds bathymetry + margin** (`depthmargin`) → warning
#'   \item **Negative depth at offshore locations** (beyond `shoremargin`) → warning
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
#' \donttest{
#' # Example dataset with one depth column
#' example_data <- data.frame(
#'   sample_latitude_dd = c(59.3, 58.1, 57.5),
#'   sample_longitude_dd = c(18.0, 17.5, 16.2),
#'   sample_depth_m = c(10, -5, NA)
#' )
#'
#' # Validate depths using OBIS XY lookup (bathymetry = NULL)
#' check_depth(example_data, depth_cols = "sample_depth_m")
#'
#' # Example dataset with min/max depth columns
#' example_data2 <- data.frame(
#'   sample_latitude_dd = c(59.0, 58.5),
#'   sample_longitude_dd = c(18.0, 17.5),
#'   sample_min_depth_m = c(5, 15),
#'   sample_max_depth_m = c(3, 20)
#' )
#'
#' check_depth(example_data2, depth_cols = c("sample_min_depth_m", "sample_max_depth_m"))
#'
#' # Return only failing rows
#' check_depth(example_data, depth_cols = "sample_depth_m", report = FALSE)
#' }
#'
#' @references Provoost P, Bosch S (2024). “obistools: Tools for data enhancement and quality control” Ocean Biodiversity Information System. Intergovernmental Oceanographic Commission of UNESCO. R package version 0.1.0, <https://iobis.github.io/obistools/>.
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
  # --- Input validation ---
  stopifnot(is.data.frame(data))
  missing_cols <- setdiff(c(lat_col, lon_col), colnames(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # --- Check lon/lat first ---
  errors <- check_lonlat(data, report = report, latcol = lat_col, loncol = lon_col)
  if (!is.null(errors) && report) return(errors)

  original_data <- data
  data <- as.data.frame(data) # ensure base df for indexing
  issues <- list()            # collect problems here

  # --- Coordinate extent defaults ---
  xmin <- -180; xmax <- 180; ymin <- -90; ymax <- 90

  # --- Bathymetry / shore distance lookup ---
  if (is.null(bathymetry)) {
    lookupvalues <- lookup_xy(data,
                              shoredistance = !is.na(shoremargin),
                              grids = TRUE, areas = FALSE)
  } else if (inherits(bathymetry, "SpatRaster")) {
    stopifnot(terra::nlyr(bathymetry) == 1)
    lookupvalues <- if (!is.na(shoremargin)) {
      lookup_xy(data, shoredistance = TRUE, grids = FALSE, areas = FALSE)
    } else {
      data.frame(row.names = seq_len(nrow(data)))
    }
    xy <- get_xy_clean_duplicates(data, latcol = lat_col, loncol = lon_col)
    cells <- terra::cellFromXY(bathymetry, xy$uniquesp)
    values <- terra::extract(bathymetry, cells)
    lookupvalues[xy$isclean, "bathymetry"] <- values[xy$duplicated_lookup, 1]

    xmin <- terra::xmin(bathymetry); xmax <- terra::xmax(bathymetry)
    ymin <- terra::ymin(bathymetry); ymax <- terra::ymax(bathymetry)
  } else {
    stop("bathymetry should be a SpatRaster or NULL")
  }

  # --- Min/max depth consistency ---
  if (length(depth_cols) == 2 && all(depth_cols %in% colnames(data))) {
    min_depth <- suppressWarnings(as.numeric(as.character(data[[depth_cols[1]]])))
    max_depth <- suppressWarnings(as.numeric(as.character(data[[depth_cols[2]]])))
    bad_rows <- which(!is.na(min_depth) & !is.na(max_depth) & min_depth > max_depth)
    if (length(bad_rows) > 0) {
      msg <- paste0("Minimum depth [", min_depth[bad_rows],
                    "] is greater than maximum depth [", max_depth[bad_rows], "]")
      issues[[length(issues) + 1]] <- dplyr::tibble(
        level = "error",
        row = bad_rows,
        field = paste(depth_cols, collapse = ","),
        message = msg
      )
    }
  }

  # --- Per-depth column checks ---
  for (col in depth_cols) {
    issues[[length(issues) + 1]] <- check_depth_column(
      dplyr::tibble(level = character(), row = integer(),
                     field = character(), message = character()),
      data, col, lookupvalues, depthmargin, shoremargin
    )
  }

  # --- Coordinate bounds checks ---
  wrong_x <- is.na(data[[lon_col]]) | data[[lon_col]] < xmin | data[[lon_col]] > xmax
  wrong_y <- is.na(data[[lat_col]]) | data[[lat_col]] < ymin | data[[lat_col]] > ymax
  issues[[length(issues) + 1]] <- add_depth_message(
    dplyr::tibble(), data, lon_col, wrong_x,
    "Longitude [%s] is outside raster bounds (%s)",
    rep(paste(xmin, xmax), nrow(data)), level = "warning"
  )
  issues[[length(issues) + 1]] <- add_depth_message(
    dplyr::tibble(), data, lat_col, wrong_y,
    "Latitude [%s] is outside raster bounds (%s)",
    rep(paste(ymin, ymax), nrow(data)), level = "warning"
  )

  # --- Missing bathymetry ---
  issues[[length(issues) + 1]] <- add_depth_message(
    dplyr::tibble(), data, lon_col,
    is.na(lookupvalues$bathymetry),
    "No bathymetry value found for coordinate (%s, %s)",
    level = "warning", extra_data = data[[lat_col]]
  )

  # --- Combine results ---
  result <- dplyr::bind_rows(issues)

  if (!report) {
    failing_rows <- sort(unique(stats::na.omit(result$row)))
    result <- original_data[failing_rows, , drop = FALSE]
  }

  dplyr::as_tibble(result)
}

add_depth_message <- function(result, data, column, rows, message,
                              extra_data = NULL, level = "warning") {
  if (is.logical(rows)) rows <- which(rows)
  if (length(rows) == 0) return(result)

  args <- list(fmt = message, data[rows, column, drop = TRUE])
  if (!is.null(extra_data)) {
    args[[length(args) + 1]] <- extra_data[rows]
  }

  new_rows <- dplyr::tibble(
    level   = level,
    row     = rows,
    field   = rep(column, length(rows)),
    message = do.call(sprintf, args)
  )

  dplyr::bind_rows(result, new_rows)
}

check_depth_column <- function(result, data, column, lookupvalues, depthmargin, shoremargin) {
  if (!(column %in% colnames(data))) {
    return(dplyr::bind_rows(
      result,
      dplyr::tibble(
        level = "warning",
        row   = NA_integer_,
        field = column,
        message = paste("Column", column, "missing")
      )
    ))
  }

  raw_vals <- data[[column]]
  depths   <- suppressWarnings(as.numeric(as.character(raw_vals)))

  # empty column
  if (all(is.na(raw_vals) | raw_vals == "")) {
    result <- dplyr::bind_rows(
      result,
      dplyr::tibble(
        level = "warning", row = NA_integer_, field = column,
        message = paste("Column", column, "empty")
      )
    )
  }

  # non-numeric but not empty
  invalid <- is.na(depths) & raw_vals != ""
  result <- add_depth_message(result, data, column, invalid,
                              "Depth value (%s) is not numeric and not empty")

  # too deep
  too_deep <- !is.na(depths) & depths > 0 & !is.na(lookupvalues$bathymetry) &
    depths > (lookupvalues$bathymetry + depthmargin)
  result <- add_depth_message(result, data, column, too_deep,
                              paste0("Depth value (%s) is greater than the value found in the bathymetry raster ",
                                     "(depth=%0.1f, margin=", depthmargin, ")"),
                              lookupvalues$bathymetry)

  # negative offshore
  if (!is.na(shoremargin)) {
    neg_offshore <- !is.na(depths) & depths < 0 &
      ((lookupvalues$shoredistance - shoremargin) > 0)
    result <- add_depth_message(result, data, column, neg_offshore,
                                paste0("Depth value (%s) is negative for offshore points ",
                                       "(shoredistance=%s, margin=", shoremargin, ")"),
                                lookupvalues$shoredistance)
  }

  result
}
