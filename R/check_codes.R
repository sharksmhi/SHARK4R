#' Get SHARK codes from cached Excel file
#'
#' This function downloads the SHARK codes Excel file from SMHI (if not already cached)
#' and reads it into R. The file is stored in a persistent cache directory
#' so it does not need to be downloaded again in subsequent sessions.
#'
#' @param url Character string with the URL to the SHARK codes Excel file.
#'   Defaults to the official SMHI codelist.
#' @param sheet Sheet to read. Can be either the sheet name or its index
#'   (default is `1`).
#' @param skip Number of rows to skip before reading data
#'   (default is `1`, to skip the header row).
#' @param force Logical; if `TRUE`, forces re-download of the Excel file
#'   even if a cached copy is available. Default is `FALSE`.
#'
#' @return A tibble containing the contents of the requested sheet.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Read the first sheet, skipping the first row
#'   codes <- get_shark_codes()
#'
#'   # Read second sheet without skipping rows
#'   codes2 <- get_shark_codes(sheet = 2, skip = 0)
#'
#'   # Force re-download of the Excel file
#'   codes3 <- get_shark_codes(force = TRUE)
#' }
get_shark_codes <- function(url = "https://smhi.se/oceanografi/oce_info_data/shark_web/downloads/codelist_SMHI.xlsx",
                            sheet = 1,
                            skip = 1,
                            force = FALSE) {
  cache_file <- cache_excel_download(url = url, force = force)
  readxl::read_excel(cache_file, sheet = sheet, skip = skip, guess_max = 2000, progress = FALSE)
}

#' Check matches of reported project (PROJ) codes in SMHIs codelist
#' @param data for tibble be be checked
#' @return unmatched codes with true or false results
#' @export

check_code_proj <- function(data) {

  toMatch <- unique(data$sample_project_name_sv)
  shark_codes <- get_shark_codes()
  shark_proj_codes <- shark_codes %>%
    filter(Data_field == "PROJ")
  match_type <- toMatch %in% shark_proj_codes$`Description/English translate`
  matches <- data.frame(reported_PROJ_code = unique(data$sample_project_name_sv), match_type = match_type)

  if (length(which(match_type == FALSE)) > 0) {
    message("ERROR: Unmatched Project (PROJ) code found")
    print(matches[!match_type,])
  }
    else {
      message("All project (PROJ) codes found")
    }
}
