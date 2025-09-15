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
#' sample_data <- read_ptbx(system.file("extdata/Anholt E_2024-09-15_0-10m.xlsx",
#'                                      package = "SHARK4R"))
#'
#' # Print output
#' sample_data
#'
#'
#' # Read a specific sheet
#' sample_info <- read_ptbx(system.file("extdata/Anholt E_2024-09-15_0-10m.xlsx",
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
