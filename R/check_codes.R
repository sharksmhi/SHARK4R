#' Get SHARK codelist from SMHI
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
#' @param clean_cache_days Numeric; if not `NULL`, cached SHARK code Excel files older than
#'   this number of days will be automatically deleted. Defaults to 30. Set to `NULL`
#'   to disable automatic cleanup.
#'
#' @return A tibble containing the contents of the requested sheet.
#' @export
#'
#' @seealso [clean_shark4r_cache()] to manually clear cached files.
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
                            force = FALSE,
                            clean_cache_days = 30) {
  # Optional: remove old cached SHARK code files
  if (!is.null(clean_cache_days) && clean_cache_days > 0) {
    cache_dir <- tools::R_user_dir("SHARK4R", "cache")
    if (dir.exists(cache_dir)) {
      shark_files <- list.files(cache_dir, pattern = "codelist_SMHI\\.xlsx$", full.names = TRUE)
      old_files <- shark_files[file.info(shark_files)$mtime < Sys.time() - clean_cache_days*24*60*60]
      if (length(old_files) > 0) unlink(old_files, force = TRUE)
    }
  }

  cache_file <- cache_excel_download(url = url, force = force)
  readxl::read_excel(cache_file, sheet = sheet, skip = skip, guess_max = 2000, progress = FALSE)
}

#' Check matches of reported project (PROJ) codes in SMHIs codelist
#' @param data for tibble be be checked
#' @param clean_cache_days Numeric; if not `NULL`, cached SHARK code Excel files older than
#'   this number of days will be automatically deleted and be replaced by a new download.
#'   Defaults to 30. Set to `NULL` to disable automatic cleanup.
#' @return unmatched codes with true or false results
#' @export
check_code_proj <- function(data, clean_cache_days = 30) {

  toMatch <- unique(data$sample_project_name_sv)
  shark_codes <- get_shark_codes(clean_cache_days = clean_cache_days)
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
