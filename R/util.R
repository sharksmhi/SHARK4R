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
#' \dontrun{
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
#' By default, the folders are downloaded into the current working directory.
#' If the folders already exist, the download will be skipped unless
#' \code{force = TRUE} is specified. Optionally, the function can launch the
#' QC Shiny app directly after setup.
#'
#' @param path Character string specifying the target directory where the
#'   \code{products} folder should be stored.
#'   Defaults to the current working directory (\code{"."}).
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
#' \dontrun{
#' # Download support files into current working directory
#' check_setup()
#'
#' # Download into a specific folder in current working directory
#' check_setup(path = "shark_qc")
#'
#' # Force re-download if already present
#' check_setup(force = TRUE)
#'
#' # Download and run the QC Shiny app
#' check_setup(run_app = TRUE)
#' }
#'
#' @export
check_setup <- function(path = ".", run_app = FALSE, force = FALSE, verbose = TRUE) {
  # Target folders
  products_dir <- file.path(path, "products")

  # If already present
  if (!force && dir.exists(products_dir)) {
    if (verbose) message("Products and scripts already exist in: ", normalizePath(path))
  } else {
    if (verbose) message("Downloading setup files for SHARK4R...")

    # GitHub commit permalink (fixed for reproducibility)
    commit <- "2b16911d55b18279ef6334f103b55f8202f727be"
    zip_url <- sprintf("https://github.com/sharksmhi/SHARK4R/archive/%s.zip", commit)

    # Temporary download location
    tmp <- tempfile(fileext = ".zip")
    utils::download.file(zip_url, tmp, quiet = !verbose)

    # Unpack into temp folder
    unpack_dir <- tempfile()
    utils::unzip(tmp, exdir = unpack_dir)

    # Source root inside the zip
    src_root <- file.path(unpack_dir, sprintf("SHARK4R-%s", commit))

    dir.create(path, showWarnings = FALSE, recursive = TRUE)

    # Copy the wanted folders
    file.copy(file.path(src_root, "products"), path, recursive = TRUE, overwrite = force)

    # Clean up
    unlink(tmp)
    unlink(unpack_dir, recursive = TRUE)

    if (verbose) message("Setup complete. Files are available in ", normalizePath(path))
  }

  # Run Shiny app if requested
  if (run_app) {
    # Check required packages
    needed_pkgs <- c("shiny", "shinythemes", "htmltools", "rmarkdown", "skimr")
    missing <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing)) {
      stop("The following packages are required to run the app: ",
           paste(missing, collapse = ", "), ". Please install them.")
    }

    # Check that server.R and ui.R exist
    if (!file.exists(file.path(products_dir, "server.R")) ||
        !file.exists(file.path(products_dir, "ui.R"))) {
      stop("Could not find 'server.R' and 'ui.R' in ", products_dir)
    }

    message("Launching SHARK4R Bio-QC Shiny app...")
    shiny::runApp(products_dir, launch.browser = TRUE)
  }

  invisible(list(products = products_dir))
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
                           force = FALSE) {

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
        return(destfile)
      } else if (file.exists(destfile)) {
        warning("Download failed, using cached copy at: ", destfile)
        return(destfile)
      } else {
        message("File for year ", year, " not available. Trying previous year...")
        year <- year - 1
        next
      }
    } else {
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

read_translate_file <- function(file = NULL, verbose = TRUE) {
  # ---- Determine file ----
  if (is.null(file)) {
    # Check environment variable
    env_path <- Sys.getenv("NODC_CONFIG", unset = NA)
    if (!is.na(env_path) && dir.exists(env_path)) {
      file <- file.path(env_path, "nodc_codes", "translate_codes.txt")
      if (verbose) message("Using station.txt from NODC_CONFIG: ", file)
    }
  }

  translate <- readr::read_delim(
    file,
    delim = "\t",
    guess_max = 2000,
    col_names = TRUE,
    locale = readr::locale(encoding = "latin1"),
    col_types = readr::cols(),
    progress = FALSE
  )

  return(translate)
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
