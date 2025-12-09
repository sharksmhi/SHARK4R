#' Read a Plankton Toolbox export file
#'
#' This function reads a sample file exported as an Excel (.xlsx) file from Plankton Toolbox
#' and extracts data from a specified sheet. The default sheet is "sample_data.txt",
#' which contains count data.
#'
#' @param file_path Character. Path to the Excel file.
#' @param sheet Character. The name of the sheet to read. Must be one of:
#'   "sample_data.txt", "Sample summary", "sample_info.txt", "counting_method.txt", or "README".
#'   Default is "sample_data.txt".
#'
#' @return A `tibble` containing the contents of the selected sheet.
#'
#' @seealso \url{https://nordicmicroalgae.org/plankton-toolbox/} for downloading Plankton Toolbox.
#' @seealso \url{https://github.com/planktontoolbox/plankton-toolbox/} for Plankton Toolbox source code.
#'
#' @examples
#'
#' # Read the default data sheet
#' sample_data <- read_ptbx(system.file("extdata/Anholt_E_2024-09-15_0-10m.xlsx",
#'                                      package = "SHARK4R"))
#'
#' # Print output
#' sample_data
#'
#'
#' # Read a specific sheet
#' sample_info <- read_ptbx(system.file("extdata/Anholt_E_2024-09-15_0-10m.xlsx",
#'                                      package = "SHARK4R"),
#'                          sheet = "sample_info.txt")
#' # Print output
#' sample_info
#'
#' @export
read_ptbx <- function(file_path, sheet = c("sample_data.txt", "sample_info.txt", "counting_method.txt", "Sample summary", "README")) {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  sheet <- match.arg(sheet)  # Ensure sheet is one of the allowed options

  if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
    if (sheet %in% excel_sheets(file_path)) {
      return(read_excel(file_path, sheet = sheet, progress = FALSE))
    } else {
      stop("Sheet not found in the Excel file.")
    }
  } else {
    stop("Only Excel (.xlsx) files are supported.")
  }
}
#' Get the latest NOMP biovolume Excel list
#'
#' This function downloads the latest available Nordic Marine Phytoplankton Group (NOMP)
#' biovolume zip archive from SMHI, unzips it, and reads the first
#' Excel file by default. You can also specify which file in the archive to read.
#'
#' @param year Numeric year to download. Default is current year; if not available,
#'   previous years are automatically tried.
#' @param file Character string specifying which file in the zip archive to read.
#'   Defaults to the first Excel file in the archive.
#' @param sheet Character or numeric; the name or index of the sheet to read from the Excel file.
#'        If neither argument specifies the sheet, defaults to the first sheet.
#' @param force Logical; if `TRUE`, forces re-download of the zip file even if cached copy exists.
#' @param base_url Base URL (without "/nomp_taxa_biovolumes_and_carbon_YYYY.zip") for the NOMP biovolume files. Defaults to the SMHI directory.
#' @param clean_cache_days Numeric; if not `NULL`, cached NOMP zip files older than
#'   this number of days will be automatically deleted and replaced by a new download.
#'   Defaults to 30. Set to `NULL` to disable automatic cleanup.
#' @param verbose A logical indicating whether to print progress messages. Default is TRUE.
#'
#' @return A tibble with the contents of the requested Excel file.
#' @export
#'
#' @seealso [clean_shark4r_cache()] to manually clear cached files.
#'
#' @examples
#' \donttest{
#'   # Get the latest available list
#'   nomp_list <- get_nomp_list()
#'   head(nomp_list)
#'
#'   # Get the 2023 list and clean old cache files older than 60 days
#'   nomp_list_2023 <- get_nomp_list(2023, clean_cache_days = 60)
#'   head(nomp_list_2023)
#' }
get_nomp_list <- function(year = as.numeric(format(Sys.Date(), "%Y")),
                          file = NULL,
                          sheet = NULL,
                          force = FALSE,
                          base_url = NULL,
                          clean_cache_days = 30,
                          verbose = TRUE) {

  # Optional: remove old NOMP cache files
  if (!is.null(clean_cache_days) && clean_cache_days > 0) {
    cache_dir <- tools::R_user_dir("SHARK4R", "cache")
    if (dir.exists(cache_dir)) {
      nomp_files <- list.files(cache_dir, pattern = "nomp_taxa_biovolumes_and_carbon_.*\\.zip$", full.names = TRUE)
      old_files <- nomp_files[file.info(nomp_files)$mtime < Sys.time() - clean_cache_days*24*60*60]
      if (length(old_files) > 0) unlink(old_files, force = TRUE)
    }
  }

  if (is.null(base_url)) {
    base_url <- "https://www.smhi.se/oceanografi/oce_info_data/shark_web/downloads/sbdi/NOMP/biovolume"
  }

  zip_path <- cache_nomp_zip(base_url = base_url, year = year, force = force, verbose = verbose)

  # utils::unzip to a temporary directory
  tmp_dir <- tempdir()
  unzipped_files <- utils::unzip(zip_path, exdir = tmp_dir)

  # Select file
  if (is.null(file)) {
    # Default: first Excel file found
    excel_files <- unzipped_files[grepl("\\.xlsx$", unzipped_files, ignore.case = TRUE)]
    if (length(excel_files) == 0) stop("No Excel files found in the zip archive.")
    file_to_read <- excel_files[1]
  } else {
    file_to_read <- file.path(tmp_dir, file)
    if (!file.exists(file_to_read)) stop("Specified file not found in the zip archive: ", file)
  }

  if (!grepl("bvol_nomp", basename(file_to_read), ignore.case = TRUE)) {
    warning("Selected file does not contain 'bvol_nomp' in its name: ",
            basename(file_to_read),
            ". Consider specifying the correct file using the 'file' argument.")
  }

  # Read Excel
  readxl::read_excel(file_to_read, sheet = sheet, guess_max = 10000, progress = FALSE)
}

#' Get the latest EG-Phyto/PEG biovolume Excel list
#'
#' This function downloads the EG-Phyto (previously PEG) biovolume zip archive from ICES (using
#' `cache_peg_zip()`), unzips it, and reads the first Excel file by default.
#' You can also specify which file in the archive to read.
#'
#' @param file Character string specifying which file in the zip archive to read.
#'   Defaults to the first Excel file in the archive.
#' @param sheet Character or numeric; the name or index of the sheet to read from the Excel file.
#'        If neither argument specifies the sheet, defaults to the first sheet.
#' @param force Logical; if `TRUE`, forces re-download of the zip file even if a cached copy exists.
#' @param url Character string with the URL of the PEG zip file.
#'   Defaults to the official ICES link.
#' @param clean_cache_days Numeric; if not `NULL`, cached PEG zip files older than
#'   this number of days will be automatically deleted and replaced by a new download.
#'   Defaults to 30. Set to `NULL` to disable automatic cleanup.
#' @param verbose A logical indicating whether to print progress messages. Default is TRUE.
#'
#' @return A tibble with the contents of the requested Excel file.
#' @export
#'
#' @seealso [clean_shark4r_cache()] to manually clear cached files.
#'
#' @examples
#' \donttest{
#'   # Read the first Excel file from the PEG zip
#'   peg_list <- get_peg_list()
#'   head(peg_list)
#' }
get_peg_list <- function(file = NULL,
                         sheet = NULL,
                         force = FALSE,
                         url = "https://www.ices.dk/data/Documents/ENV/PEG_BVOL.zip",
                         clean_cache_days = 30,
                         verbose = TRUE) {

  # Optional: remove old PEG cache files
  if (!is.null(clean_cache_days) && clean_cache_days > 0) {
    cache_dir <- tools::R_user_dir("SHARK4R", "cache")
    if (dir.exists(cache_dir)) {
      peg_files <- list.files(cache_dir, pattern = "PEG_BVOL.*\\.zip$", full.names = TRUE)
      old_files <- peg_files[file.info(peg_files)$mtime < Sys.time() - clean_cache_days*24*60*60]
      if (length(old_files) > 0) unlink(old_files, force = TRUE)
    }
  }

  zip_path <- cache_peg_zip(url = url, force = force)

  # utils::unzip to a temporary directory
  tmp_dir <- tempdir()
  unzipped_files <- utils::unzip(zip_path, exdir = tmp_dir)

  # Select file
  if (is.null(file)) {
    # Default: first Excel file found
    excel_files <- unzipped_files[grepl("\\.xlsx$", unzipped_files, ignore.case = TRUE)]
    if (length(excel_files) == 0) stop("No Excel files found in the zip archive.")
    file_to_read <- excel_files[1]
  } else {
    file_to_read <- file.path(tmp_dir, file)
    if (!file.exists(file_to_read)) stop("Specified file not found in the zip archive: ", file)
  }

  if (!grepl("PEG_BVOL", basename(file_to_read), ignore.case = TRUE)) {
    warning("Selected file does not contain 'PEG_BVOL' in its name: ",
            basename(file_to_read),
            ". Consider specifying the correct file using the 'file' argument.")
  }

  # Extract year from filename
  year_match <- regmatches(basename(file_to_read), regexpr("\\d{4}", basename(file_to_read)))
  if (length(year_match) == 1) {
    if (verbose) message("Reading PEG biovolume Excel file for year: ", year_match)
  }

  # Read Excel
  readxl::read_excel(file_to_read, sheet = sheet, guess_max = 10000, progress = FALSE)
}
