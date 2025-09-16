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
#' @return A data frame containing the contents of the selected sheet.
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

  if (grepl(".xlsx$", file_path, ignore.case = TRUE)) {
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
#' This function downloads the latest available NOMP biovolume zip archive
#' from SMHI (using `cache_nomps_zip()`), unzips it, and reads the first
#' Excel file by default. You can also specify which file in the archive to read.
#'
#' @param year Numeric year to download. Default is current year; if not available,
#'   previous years are automatically tried.
#' @param file Character string specifying which file in the zip archive to read.
#'   Defaults to the first Excel file in the archive.
#' @param force Logical; if `TRUE`, forces re-download of the zip file even if cached copy exists.
#' @param base_url Base URL (without "/nomp_taxa_biovolumes_and_carbon_YYYY.zip") for the NOMP biovolume files. Defaults to the SMHI directory.
#'
#' @return A tibble with the contents of the requested Excel file.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get the latest available list
#'   nomp_list <- get_nomp_list()
#'
#'   # Get the 2023 list
#'   nomp_list_2023 <- get_nomp_list(2023)
#' }
get_nomp_list <- function(year = as.numeric(format(Sys.Date(), "%Y")),
                          file = NULL,
                          force = FALSE,
                          base_url = NULL) {

  if (is.null(base_url)) {
    base_url <- "https://www.smhi.se/oceanografi/oce_info_data/shark_web/downloads/sbdi/NOMP/biovolume"
  }

  zip_path <- cache_nomps_zip(base_url = base_url, year = year, force = force)

  # Unzip to a temporary directory
  tmp_dir <- tempdir()
  unzipped_files <- unzip(zip_path, exdir = tmp_dir)

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

  # Read Excel
  readxl::read_excel(file_to_read, guess_max = 10000, progress = FALSE)
}

#' Get PEG biovolume Excel list
#'
#' This function downloads the PEG biovolume zip archive from ICES (using
#' `cache_peg_zip()`), unzips it, and reads the first Excel file by default.
#' You can also specify which file in the archive to read.
#'
#' @param file Character string specifying which file in the zip archive to read.
#'   Defaults to the first Excel file in the archive.
#' @param force Logical; if `TRUE`, forces re-download of the zip file even if a cached copy exists.
#' @param url Character string with the URL of the PEG zip file.
#'   Defaults to the official ICES link.
#'
#' @return A tibble with the contents of the requested Excel file.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Read the first Excel file from the PEG zip
#'   peg_list <- get_peg_list()
#'
#'   # Read a specific file inside the zip
#'   peg_list2 <- get_peg_list(file = "PEG_extra_sheet.xlsx")
#' }
get_peg_list <- function(file = NULL,
                         force = FALSE,
                         url = "https://www.ices.dk/data/Documents/ENV/PEG_BVOL.zip") {

  zip_path <- cache_peg_zip(url = url, force = force)

  # Unzip to a temporary directory
  tmp_dir <- tempdir()
  unzipped_files <- unzip(zip_path, exdir = tmp_dir)

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

  # Extract year from filename
  year_match <- regmatches(basename(file_to_read), regexpr("\\d{4}", basename(file_to_read)))
  if (length(year_match) == 1) {
    message("Reading PEG biovolume Excel file for year: ", year_match)
  }

  # Read Excel
  readxl::read_excel(file_to_read, guess_max = 10000, progress = FALSE)
}
