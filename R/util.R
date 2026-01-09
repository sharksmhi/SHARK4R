#' Clean SHARK4R cache by file age and session
#'
#' Deletes cached files in the SHARK4R cache directory that are older than
#' a specified number of days.
#'
#' @param days Numeric; remove files older than this number of days. Default is 1.
#' @param cache_dir Character; path to the cache directory to clean.
#'   Defaults to the SHARK4R cache directory in the user-specific R folder
#'   (via `tools::R_user_dir("SHARK4R", "cache")`). You can override this
#'   parameter for custom cache locations.
#' @param clear_perm_cache Logical. If `TRUE`, filed that are cached across R sessions are cleared, i.e. geographical shape files.
#'        Defaults to `FALSE`.
#' @param search_pattern Character; optional regex pattern to filter which files to consider for deletion.
#' @param verbose Logical. If `TRUE`, displays messages of cache cleaning progress. Defaults to `TRUE`.
#'
#' @details
#' The cache is automatically cleared after 24h.
#'
#' @export
#'
#' @seealso [get_peg_list()], [get_nomp_list()], [get_shark_codes()], [get_dyntaxa_dwca()], [get_shark_statistics()]
#'   for functions that populate the cache.
#'
#' @return Invisible `NULL`. Messages are printed about what was deleted
#'   and whether the in-memory session cache was cleared.
#'
#' @examples
#' \donttest{
#'   # Remove files older than 60 days and clear session cache
#'   clean_shark4r_cache(days = 60)
#' }
clean_shark4r_cache <- function(days = 1,
                                cache_dir = tools::R_user_dir("SHARK4R", "cache"),
                                clear_perm_cache = FALSE,
                                search_pattern = NULL,
                                verbose = TRUE) {

  if (!dir.exists(cache_dir)) {
    if (verbose) message("No SHARK4R cache directory found.")
    return(invisible(NULL))
  }

  files <- list.files(cache_dir, full.names = TRUE)
  if (clear_perm_cache) {
    perm_dir <- file.path(cache_dir, "perm")
    if (dir.exists(perm_dir)) {
      perm_files <- list.files(perm_dir, full.names = TRUE)
      files <- c(files, perm_files)
    }
  }

  if (search_pattern == "" || is.null(search_pattern)) {
    # No filtering
  } else {
    files <- files[grepl(search_pattern, basename(files))]
  }

  if (length(files) == 0) {
    if (verbose) message("SHARK4R cache is already empty.")
    return(invisible(NULL))
  }

  old_files <- files[file.info(files)$mtime < Sys.time() - days * 24*60*60]

  if (length(old_files) == 0) {
    if (verbose) message("No files older than ", days, " days to remove.")
    return(invisible(NULL))
  }

  unlink(old_files, recursive = TRUE, force = TRUE)
  if (verbose) message("Removed ", length(old_files), " file(s) older than ", days, " days from SHARK4R cache.")
  invisible(NULL)
}

#' Download and set up SHARK4R support files
#'
#' This function downloads the \code{products} folder from
#' the SHARK4R GitHub repository and places them in a user-specified directory.
#' These folders contain Shiny applications and R Markdown documents used for
#' quality control (QC) of SHARK data.
#'
#' If the `path` folders already exist, the download will be skipped unless
#' \code{force = TRUE} is specified. Optionally, the function can launch the
#' QC Shiny app directly after setup.
#'
#' @param path Character string giving the directory where the products folder
#'   should be created. Must be provided by the user.
#' @param run_app Logical, if \code{TRUE} runs the QC Shiny app located in the
#'   \code{products} folder after setup. Default is \code{FALSE}.
#' @param force Logical, if \code{TRUE} forces a re-download and overwrites
#'   existing folder. Default is \code{FALSE}.
#' @param verbose Logical, if \code{TRUE} prints progress messages.
#'   Default is \code{TRUE}.
#'
#' @return An (invisible) list with the path to the local \code{products} folder:
#'
#' @examples
#' \donttest{
#' # Download support files into a temporary directory
#' check_setup(path = tempdir())
#'
#' # Force re-download if already present
#' check_setup(path = tempdir(), force = TRUE)
#'
#' # Download and run the QC Shiny app
#' if(interactive()){
#'  check_setup(path = tempdir(), run_app = TRUE)
#' }
#' }
#'
#' @export
check_setup <- function(path, run_app = FALSE, force = FALSE, verbose = TRUE) {
  # Target folders
  products_dir <- file.path(path, "products")

  # If already present
  if (!force && dir.exists(products_dir)) {
    if (verbose) message("Products and scripts already exist in: ", normalizePath(path))
  } else {
    if (verbose) message("Downloading setup files for SHARK4R...")

    # GitHub master url
    zip_url <- "https://github.com/sharksmhi/SHARK4R/archive/refs/heads/master.zip"

    # Temporary download location
    tmp <- tempfile(fileext = ".zip")
    utils::download.file(zip_url, tmp, quiet = !verbose)

    # Unpack into temp folder
    unpack_dir <- tempfile()
    utils::unzip(tmp, exdir = unpack_dir)

    # Source root inside the zip
    src_root <- file.path(unpack_dir, "SHARK4R-master")

    dir.create(path, showWarnings = FALSE, recursive = TRUE)

    # Copy the wanted folders
    file.copy(file.path(src_root, "products"), path, recursive = TRUE, overwrite = force)
    # file.copy(file.path(src_root, "inst", "shiny", "shark-qc", "report.Rmd"), path, recursive = TRUE, overwrite = force)

    # Clean up
    unlink(tmp)
    unlink(unpack_dir, recursive = TRUE)

    if (verbose) message("Setup complete. Files are available in ", normalizePath(path))
  }

  # Run Shiny app if requested
  if (run_app) {

    message("Launching SHARK4R Bio-QC Shiny app...")
    run_qc_app()
  }

  invisible(list(products = products_dir))
}

#' Translate SHARK4R datatype names
#'
#' Converts user-facing datatype names (e.g., "Grey seal") to internal SHARK4R names
#' (e.g., "GreySeal") based on `SHARK4R:::.type_lookup`. See available user-facing
#' datatypes in `get_shark_options()$dataTypes`.
#'
#' @param x Character vector of datatype names to translate
#' @return Character vector of translated datatype names
#'
#' @examples
#' # Example strings
#' datatypes <- c("Grey seal", "Primary production", "Physical and Chemical")
#'
#' # Basic translation
#' translate_shark_datatype(datatypes)
#'
#' @export
translate_shark_datatype <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.character(x)) x <- as.character(x)

  translated <- .type_lookup[x]

  # Warn if some names are unknown
  unknown <- x[is.na(translated)]
  if (length(unknown) > 0) {
    warning("Unknown datatype(s) not in .type_lookup: ", paste(unknown, collapse = ", "))
    translated[is.na(translated)] <- unknown  # keep original
  }

  unname(translated)
}

#' Load SHARK4R statistics from GitHub
#'
#' This function downloads and loads precomputed SHARK4R statistical data
#' (e.g., threshold or summary statistics) directly from the
#' [SHARK4R-statistics](https://github.com/nodc-sweden/SHARK4R-statistics) GitHub repository.
#' The data are stored as `.rds` files and read into R as objects.
#'
#' @param file_name Character string specifying the name of the `.rds` file to download.
#'   Defaults to `"sea_basin.rds"`.
#' @param verbose Logical; if `TRUE` (default), prints progress messages during download and loading.
#'
#' @details
#' The function retrieves the file from the GitHub repository’s `data/` folder.
#' It temporarily downloads the file to the local system and then reads it into R using `readRDS()`.
#'
#' If the download fails (e.g., due to a network issue or invalid filename), the function
#' throws an error with a descriptive message.
#'
#' @return
#' An R object (typically a `tibble` or `data.frame`) read from the specified `.rds` file.
#'
#' @seealso
#' \code{\link{check_outliers}} for detecting threshold exceedances using the loaded statistics,
#' \code{\link{get_shark_statistics}} for generating and caching statistical summaries used in SHARK4R.
#' \code{\link{scatterplot}} for generating interactive plots with threshold values.
#'
#' @examples
#' \donttest{
#' # Load the default SHARK4R statistics file
#' stats <- load_shark4r_stats(verbose = FALSE)
#' print(stats)
#'
#' # Load a specific file
#' thresholds <- load_shark4r_stats("scientific_name.rds", verbose = FALSE)
#' print(thresholds)
#' }
#'
#' @export
load_shark4r_stats <- function(file_name = "sea_basin.rds",
                               verbose = TRUE) {

  base_url <- "https://raw.githubusercontent.com/nodc-sweden/SHARK4R-statistics/main/statistics/"

  url <- paste0(base_url, file_name)

  # Create a temporary file to store the download
  tmp <- tempfile(fileext = ".rds")

  # Try downloading the file
  tryCatch({
    if (verbose) message("Downloading stats data from GitHub...")
    utils::download.file(url, destfile = tmp, mode = "wb", quiet = !verbose)

    if (verbose) message("Reading ", file_name, " file into R...")
    data <- readRDS(tmp)

    if (verbose) message("Data successfully loaded.")
    return(data)
  },
  error = function(e) {
    stop("Failed to load data from GitHub: ", e$message)
  })
}

#' Load SHARK4R fields from GitHub
#'
#' This function downloads and sources the SHARK4R required and recommended field definitions
#' directly from the
#' [SHARK4R-statistics](https://github.com/nodc-sweden/SHARK4R-statistics) GitHub repository.
#'
#' The definitions are stored in an R script (`fields.R`) located in the `fields/` folder of the repository.
#' The function sources this file directly from GitHub into the current R session.
#'
#' @param verbose Logical; if `TRUE` (default), prints progress messages during download and loading.
#'
#' @details
#' The sourced script defines two main objects:
#' \itemize{
#'   \item `required_fields` — vector or data frame of required SHARK fields.
#'   \item `recommended_fields` — vector or data frame of recommended SHARK fields.
#' }
#'
#' The output of this function can be directly supplied to the
#' \code{\link{check_fields}} function through its `field_definitions` argument
#' for validating SHARK4R data consistency.
#'
#' If sourcing fails (e.g., due to a network issue or repository changes), the function throws
#' an error with a descriptive message.
#'
#' @return
#' Invisibly returns a list with two elements:
#' \describe{
#'   \item{required_fields}{Object containing required SHARK fields.}
#'   \item{recommended_fields}{Object containing recommended SHARK fields.}
#' }
#'
#' @seealso
#' \code{\link{check_fields}} for validating datasets using the loaded field definitions (as `field_definitions`).
#' \code{\link{load_shark4r_stats}} for loading precomputed SHARK4R statistics,
#'
#' @examples
#' \donttest{
#' # Load SHARK4R field definitions from GitHub
#' fields <- load_shark4r_fields(verbose = FALSE)
#'
#' # Access required or recommended fields for the first entry
#' fields[[1]]$required
#' fields[[1]]$recommended
#' }
#' \dontrun{
#' # Use the loaded definitions in check_fields()
#' check_fields(my_data, field_definitions = fields)
#' }
#'
#' @export
load_shark4r_fields <- function(verbose = TRUE) {
  base_url <- "https://raw.githubusercontent.com/nodc-sweden/SHARK4R-statistics/main/fields/fields.R"
  tmp <- tempfile(fileext = ".R")

  tryCatch({
    if (verbose) message("Downloading SHARK4R field definitions from GitHub...")
    utils::download.file(base_url, destfile = tmp, quiet = !verbose)

    if (verbose) message("Sourcing field definitions into R...")
    env <- new.env()
    sys.source(tmp, envir = env)

    if (!exists(".field_definitions", envir = env)) {
      stop("The sourced file does not contain an object named '.field_definitions'.")
    }

    defs <- get(".field_definitions", envir = env)

    if (verbose) message("Field definitions successfully loaded.")
    return(defs)
  },
  error = function(e) {
    stop("Failed to load SHARK4R field definitions from GitHub: ", e$message)
  })
}

#' Convert coordinates from DDMM format to decimal degrees
#'
#' This function converts geographic coordinates provided in the DDMM
#' format (degrees and minutes) to decimal degrees. It can handle:
#' - DDMM (e.g., 5733 to 57°33' to 57.55°)
#' - DDMMss or DDMMss… (extra digits after minutes are interpreted
#'   as fractional minutes, e.g., 573345 to 57°33.45' to 57.5575°)
#'
#' Non-numeric characters are removed before conversion. Coordinates
#' shorter than 4 digits are returned as `NA`.
#'
#' @param coord A numeric or character vector of coordinates in DDMM format.
#'
#' @return A numeric vector of decimal degrees corresponding to the input coordinates.
#'   Names from the input vector are removed.
#'
#' @examples
#' # Basic DDMM input
#' convert_ddmm_to_dd(c(5733, 6045))
#' # Input with fractional minutes
#' convert_ddmm_to_dd(c("573345", "604523"))
#' # Input with non-numeric characters
#' convert_ddmm_to_dd(c("57°33'", "60°45'23\""))
#'
#' @export
convert_ddmm_to_dd <- function(coord) {
  coord <- as.character(coord)
  coord <- gsub("[^0-9]", "", coord)  # Remove non-numeric characters

  # Handle too short input
  coord[nchar(coord) < 4] <- NA

  decimal_degrees <- sapply(coord, function(c) {
    if (is.na(c)) return(NA_real_)

    len <- nchar(c)
    deg <- as.numeric(substr(c, 1, 2))
    min <- as.numeric(substr(c, 3, 4))

    # Extra digits beyond DDMM -> fractional minutes
    if (len > 4) {
      min_decimals <- as.numeric(substr(c, 5, len))
      min <- min + min_decimals / (10^(len - 4))
    }

    return(deg + min / 60)
  })

  return(unname(decimal_degrees))  # remove the names
}

## Helpers

missing_fields <- function(data, fields) {
  missing <- !(fields %in% names(data))
  return(fields[missing])
}

missing_values <- function(data) {
  return(data %in% c(NA, ""))
}

check_lonlat <- function(data, report = TRUE, latcol = "sample_latitude_dd", loncol = "sample_longitude_dd") {
  errors <- c()
  if (!loncol %in% names(data)) {
    errors <- c(errors, sprintf("Column %s missing", loncol))
  } else if (!is.numeric(data[[loncol]])) {
    errors <- c(errors, sprintf("Column %s is not numeric", loncol))
  }
  if (!latcol %in% names(data)) {
    errors <- c(errors, sprintf("Column %s missing", latcol))
  } else if (!is.numeric(data[[latcol]])) {
    errors <- c(errors, sprintf("Column %s is not numeric", latcol))
  }
  if(length(errors) > 0) {
    if(report) {
      return(tibble(level = "error", message = errors))
    } else {
      stop(paste(errors, collapse = ", "))
    }
  }
  return(NULL)
}

get_xy_clean <- function(data, returnisclean = FALSE, latcol = "sample_latitude_dd", loncol = "sample_longitude_dd") {
  check_lonlat(data, report = FALSE, latcol = latcol, loncol = loncol)
  sp <- data.frame(lon = numeric(0), lat = numeric(0))
  isclean <- NULL

  if(NROW(data) > 0) {
    sp <- data %>% dplyr::select(all_of(c(loncol, latcol)))
    names(sp) <- c("lon", "lat")
    isclean <- stats::complete.cases(sp) &
      sapply(sp$lon, is.numeric) &
      sapply(sp$lat, is.numeric) &
      !is.na(sp$lon) & !is.na(sp$lat) &
      sp$lon >= -180 & sp$lon <= 180 &
      sp$lat >= -90 & sp$lat <= 90
  }

  cleansp <- sp[isclean,,drop=FALSE]
  if(returnisclean) {
    return(list(cleansp = cleansp, isclean = isclean))
  } else {
    return(cleansp)
  }
}

get_xy_clean_duplicates <- function(data, latcol = "sample_latitude_dd", loncol = "sample_longitude_dd") {
  clean <- get_xy_clean(data, returnisclean = TRUE, latcol = latcol, loncol = loncol)
  if(NROW(clean$cleansp) > 0) {
    key <- paste(clean$cleansp$lon, clean$cleansp$lat, sep = "\r")
    notdup <- !duplicated(key)
    uniquesp <- clean$cleansp[notdup, , drop=FALSE]
    duplicated_lookup <- match(key, key[notdup])
    list(uniquesp = uniquesp, isclean = clean$isclean, duplicated_lookup = duplicated_lookup)
  } else {
    list(uniquesp = data.frame(lon = numeric(0), lat = numeric(0)),
         isclean = NULL,
         duplicated_lookup = NULL)
  }
}

list_cache <- function(include_perm = FALSE) {
  list.files(tools::R_user_dir("SHARK4R", which = "cache"), full.names = TRUE)
  if(include_perm) {
    perm_dir <- file.path(tools::R_user_dir("SHARK4R", which = "cache"), "perm")
    if(dir.exists(perm_dir)) {
      c(list.files(tools::R_user_dir("SHARK4R", which = "cache"), full.names = TRUE),
        list.files(perm_dir, full.names = TRUE))
    } else {
      list.files(tools::R_user_dir("SHARK4R", which = "cache"), full.names = TRUE)
    }
  } else {
    list.files(tools::R_user_dir("SHARK4R", which = "cache"), full.names = TRUE)
  }
}

cache_call <- function(key, expr, env = NULL) {
  stopifnot(is.expression(expr))
  if(is.null(env)) {
    env = parent.frame()
  }
  cache_dir <- tools::R_user_dir("SHARK4R", which = "cache")

  tmpfile <- tempfile()
  saveRDS(list(key = key, expr = expr), tmpfile)
  hash <- tools::md5sum(tmpfile)
  unlink(tmpfile)

  cachefile <- file.path(cache_dir, paste0("call_", hash, ".rds"))

  cachefile <- file.path(cache_dir, paste0("call_", hash, ".rds"))
  if(file.exists(cachefile) && difftime(Sys.time(), file.info(cachefile)[,"mtime"], units = "hours") < 10) {
    return(readRDS(cachefile))
  } else {
    result <- eval(expr, envir = NULL, enclos = env)
    if(!dir.exists(cache_dir)) {
      dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    }
    saveRDS(result, cachefile)
    return(result)
  }
}

service_call <- function(url, msg) {
  cache_call(key = paste(url, msg),
             expression({
               response <- httr::POST(url,
                                      httr::content_type("application/json"),
                                      httr::user_agent("SHARK4R - https://github.com/sharksmhi/SHARK4R"),
                                      body=msg)

               # Parse result
               raw_content <- httr::content(response, as="raw")
               if(response$status_code != 200) {
                 if(is.list(raw_content) && all(c("title", "description") %in% names(raw_content))) {
                   stop(paste0(raw_content$title, ": ", raw_content$description))
                 }
                 stop(rawToChar(raw_content))
               }
               raw_content
             }))
}

# Function to download and cache an Excel file
cache_excel_download <- function(url = "https://smhi.se/oceanografi/oce_info_data/shark_web/downloads/codelist_SMHI.xlsx",
                                 filename = basename(url),
                                 force = FALSE) {
  cache_dir <- tools::R_user_dir("SHARK4R", which = "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  destfile <- file.path(cache_dir, filename)

  # Download only if file is missing or force = TRUE
  if (!file.exists(destfile) || force) {
    ok <- tryCatch({
      utils::download.file(url, destfile, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      FALSE
    }, warning = function(w) {
      FALSE
    })

    if (!ok) {
      if (file.exists(destfile)) {
        warning("Download failed, using cached copy at: ", destfile)
      } else {
        stop("Download failed and no cached file available. ",
             "Check your internet connection or URL: ", url, call. = FALSE)
      }
    }
  }

  destfile
}

cache_nomp_zip <- function(base_url = "https://www.smhi.se/oceanografi/oce_info_data/shark_web/downloads/sbdi/NOMP/biovolume",
                           year = as.numeric(format(Sys.Date(), "%Y")),
                           force = FALSE,
                           verbose = TRUE) {

  cache_dir <- tools::R_user_dir("SHARK4R", which = "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (year > current_year + 1) {
    warning("Requested year is in the future. Using ", current_year + 1, " as starting year.")
    year <- current_year + 1
  }

  # Try requested year and previous years until success
  while (year > 2020) { # safety stop
    filename <- paste0("nomp_taxa_biovolumes_and_carbon_", year, ".zip")
    destfile <- file.path(cache_dir, filename)
    url <- file.path(base_url, filename)

    # Attempt download if needed
    if (!file.exists(destfile) || force) {
      ok <- tryCatch({
        utils::download.file(url, destfile, mode = "wb", quiet = TRUE)
        TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)

      if (ok) {
        if (verbose) message("File for year ", year, " downloaded and cached.")
        return(destfile)
      } else if (file.exists(destfile)) {
        warning("Download failed, using cached copy at: ", destfile)
        return(destfile)
      } else {
        if (verbose) message("File for year ", year, " not available. Trying previous year...")
        year <- year - 1
        next
      }
    } else {
      if (verbose) message("File for year ", year, " found in cache.")
      # Already cached
      return(destfile)
    }
  }

  stop("No NOMP biovolume file could be downloaded or found in cache for recent years.", call. = FALSE)
}

cache_peg_zip <- function(url = "https://www.ices.dk/data/Documents/ENV/PEG_BVOL.zip",
                          force = FALSE) {

  cache_dir <- tools::R_user_dir("SHARK4R", which = "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  filename <- basename(url)
  destfile <- file.path(cache_dir, filename)

  # Download only if missing or forced
  if (!file.exists(destfile) || force) {
    ok <- tryCatch({
      utils::download.file(url, destfile, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)

    if (!ok) {
      if (file.exists(destfile)) {
        warning("Download failed, using cached copy at: ", destfile)
      } else {
        stop("Download failed and no cached file available. Check your internet connection or URL: ", url, call. = FALSE)
      }
    }
  }

  destfile
}

# Helper to normalize stringi detected encodings to our keys
normalize_encoding <- function(enc) {
  enc <- toupper(enc)

  # Map common variants
  if (enc %in% c("WINDOWS-1252", "CP1252")) return("cp1252")
  if (enc %in% c("UTF-8", "UTF8")) return("utf_8")
  if (enc %in% c("UTF-16BE", "UTF-16LE", "UTF16")) return("utf_16")
  if (enc %in% c("ISO-8859-1", "LATIN1", "LATIN_1")) return("latin_1")

  # Fallback for unknowns
  warning("Unknown encoding detected: ", enc, ". Defaulting to 'utf_8'.")
  return("utf_8")
}

# Cleaning function for worms names
clean_taxon <- function(x) {
  # Ensure string
  if (is.na(x)) x <- ""
  x <- as.character(x)

  # Normalize encoding to UTF-8
  x <- iconv(x, to = "UTF-8", sub = "")

  # Replace problematic characters (excluding periods)
  pattern <- "[/|\\\\?&%#\\[\\]\\{\\}_]"
  out <- gsub(pattern, " ", x, perl = TRUE)

  # Remove trailing periods (one or more)
  out <- gsub("\\.+$", "", out, perl = TRUE)

  # Collapse multiple spaces and trim
  out <- gsub("\\s+", " ", out, perl = TRUE)
  out <- trimws(out)

  # If empty after cleaning, set placeholder
  if (identical(out, "")) out <- "_empty_"
  out
}

is_check <- function() {
  nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", ""))
}

.hab_state <- new.env(parent = emptyenv())
.hab_state$last_call_time <- NULL

rate_limit <- function(min_seconds = 5) {
  now <- Sys.time()

  if (!is.null(.hab_state$last_call_time)) {
    elapsed <- as.numeric(difftime(now, .hab_state$last_call_time, units = "secs"))

    if (elapsed < min_seconds) {
      Sys.sleep(min_seconds - elapsed)
    }
  }

  .hab_state$last_call_time <- Sys.time()
}
