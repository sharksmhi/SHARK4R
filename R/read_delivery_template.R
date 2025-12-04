#' Get a delivery template for a SHARK datatype
#'
#' Downloads and reads the SHARK Excel delivery template for a given datatype.
#' The template contains the column definitions and headers used for submission.
#'
#' @param datatype Character. The datatype name.
#'   Available options include:
#'   \itemize{
#'     \item "Bacterioplankton" (subtypes: "abundance", "production")
#'     \item "Chlorophyll"
#'     \item "Epibenthos" (dive transect)
#'     \item "Dropvideo" (epibenthos drop video)
#'     \item "Grey seal"
#'     \item "Harbour seal"
#'     \item "Ringed seal"
#'     \item "Harbour Porpoise"
#'     \item "Physical and Chemical"
#'     \item "Primary production"
#'     \item "Phytoplankton"
#'     \item "Picoplankton"
#'     \item "Sedimentation"
#'     \item "Seal pathology"
#'     \item "Profile"
#'     \item "Zooplankton"
#'     \item "Zoobenthos"
#'   }
#' @param sheet Character or numeric. Name (e.g., "Kolumner") or index (e.g., 1) of
#'   the sheet in the Excel file to read. Default is "Kolumner".
#' @param header_row Integer. Row number in the Excel file that contains the column headers.
#'   Default is 4.
#' @param skip Integer. Number of rows to skip before reading data. Default is 1.
#' @param bacterioplankton_subtype Character. For "Bacterioplankton" only: either
#'   "abundance" (default) or "production". Ignored for other datatypes.
#' @param force Logical; if `TRUE`, forces re-download even if cached copy exists.
#' @param clean_cache_days Numeric; if not `NULL`, cached template files older than
#'   this number of days are deleted automatically. Default is 1.
#'
#' @return A `tibble` containing the delivery template. Column names are set
#'   from the header row.
#'
#' @examples
#' \donttest{
#' # Bacterioplankton abundance
#' abun <- get_delivery_template("Bacterioplankton",
#'                               bacterioplankton_subtype = "abundance")
#'
#' print(abun)
#'
#' # Bacterioplankton production
#' prod <- get_delivery_template("Bacterioplankton",
#'                               bacterioplankton_subtype = "production")
#'
#' # Phytoplankton template
#' phyto <- get_delivery_template("Phytoplankton")
#'
#' # Phytoplankton column explanation (sheet number 3)
#' phyto_column_explanation <- get_delivery_template("Phytoplankton",
#'                                                   sheet = 3,
#'                                                   header_row = 4,
#'                                                   skip = 3)
#'
#' print(phyto_column_explanation)
#' }
#'
#' @export
get_delivery_template <- function(datatype,
                                  sheet = "Kolumner",
                                  header_row = 4,
                                  skip = 1,
                                  bacterioplankton_subtype = "abundance",
                                  force = FALSE,
                                  clean_cache_days = 1) {

  # Remove optional "deliv_" prefix and lowercase
  datatype_clean <- tolower(gsub("^deliv_", "", datatype))

  # Mapping of cleaned input to standard SHARK datatype names
  datatype_map <- c(
    "bacterioplankton"         = "Bacterioplankton",
    "chlorophyll"              = "Chlorophyll",
    "epibenthosdropvideo"      = "Dropvideo",
    "epibenthosdivetransect"   = "Epibenthos",
    "greyseal"                 = "Grey seal",
    "harbourseal"              = "Harbour seal",
    "physicalchemical"         = "Physical and Chemical",
    "primaryproduction"        = "Primary production",
    "ringedseal"               = "Ringed seal",
    "sealpathology"            = "Seal pathology",
    "harbourporpoise"          = "Harbour Porpoise",
    "phytoplankton"            = "Phytoplankton",
    "zooplankton"              = "Zooplankton",
    "zoobenthos"               = "Zoobenthos",
    "picoplankton"             = "Picoplankton",
    "planktonbarcoding"        = "Plankton Barcoding",
    "planktonimaging"          = "Plankton Imaging",
    "sedimentation"            = "Sedimentation",
    "profile"                  = "Profile"
  )

  if (datatype_clean %in% names(datatype_map)) {
    datatype <- datatype_map[[datatype_clean]]
  } else {
    datatype <- gsub("^deliv_", "", datatype)
  }

  # Base URL for SHARK delivery templates
  base_url <- "https://smhi.se/oceanografi/oce_info_data/shark_web/downloads/"

  # Mapping between datatype and URL
  url_map <- list(
    "Bacterioplankton" = list(
      "abundance"   = "Format%20Bacterioplankton%20abundance.xlsx",
      "production"  = "Format%20Bacterioplankton%20production.xlsx"
    ),
    "Chlorophyll"           = "Format%20Chlorophyll.xlsx",
    "Epibenthos"            = "Format%20Epibenthos%20dive_transect.xlsx",
    "Dropvideo"             = "Format%20Epibenthos%20dropvideo.xlsx",
    "Grey seal"             = "Format%20Grey%20and%20Harbour%20seal.xlsx",
    "Harbour Porpoise"      = "Format%20Harbour%20Porpoise.xlsx",
    "Harbour seal"          = "Format%20Grey%20and%20Harbour%20seal.xlsx",
    "Physical and Chemical" = "Format%20Physical%20and%20chemical.xlsx",
    "Phytoplankton"         = "Format%20Phytoplankton.xlsx",
    "Picoplankton"          = "Format%20Phytoplankton.xlsx", # Same as Phytoplankton
    "Plankton Barcoding"    = NA,
    "Plankton Imaging"      = NA,
    "Primary production"    = "Format%20Primary%20production.xlsx",
    "Profile"               = "Format%20Profile.xlsx",
    "Ringed seal"           = "Format%20Ringed%20seal.xlsx",
    "Seal pathology"        = "Format%20Seal%20pathology.xlsx",
    "Sedimentation"         = "Format%20Sedimentation.xlsx",
    "Zoobenthos"            = "Format%20Zoobenthos.xlsx",
    "Zooplankton"           = "Format%20Zooplankton.xlsx"
  )

  if (!datatype %in% names(url_map)) {
    stop(sprintf("Datatype '%s' is not recognized. Must be one of: %s",
                 datatype, paste(names(url_map), collapse = ", ")))
  }

  # Determine file URL
  if (datatype == "Bacterioplankton") {
    bacterioplankton_subtype <- match.arg(bacterioplankton_subtype, choices = c("abundance", "production"))
    file_url <- url_map[[datatype]][[bacterioplankton_subtype]]
  } else {
    file_url <- url_map[[datatype]]
  }

  if (is.na(file_url)) {
    stop(sprintf("No URL is available for datatype '%s'.", datatype))
  }

  file_url <- paste0(base_url, file_url)

  # Cache directory
  cache_dir <- tools::R_user_dir("SHARK4R", "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  # Cache file path
  cache_file <- file.path(cache_dir, paste0("delivery_", gsub(" ", "_", datatype), ".xlsx"))

  # Clean old cache files
  if (!is.null(clean_cache_days) && clean_cache_days > 0) {
    cache_files <- list.files(cache_dir, pattern = "^delivery_.*\\.xlsx$", full.names = TRUE)
    old_files <- cache_files[file.info(cache_files)$mtime < Sys.time() - clean_cache_days*24*60*60]
    if (length(old_files) > 0) unlink(old_files, force = TRUE)
  }

  # Download if needed
  if (force || !file.exists(cache_file)) {
    utils::download.file(file_url, cache_file, mode = "wb", quiet = TRUE)
  }

  # Read without headers
  raw <- suppressMessages(
    readxl::read_excel(cache_file, sheet = sheet, col_names = FALSE, skip = skip, progress = FALSE)
  )

  # Extract header row
  headers <- as.character(unlist(raw[header_row - skip, ]))

  # Replace empty or NA headers with generic names
  headers[is.na(headers) | headers == ""] <- paste0("...", which(is.na(headers) | headers == ""))

  # Apply headers, drop header row
  df <- raw[-header_row, ]
  names(df) <- headers

  return(df)
}

#' Find required fields in a SHARK delivery template
#'
#' Identifies which columns are mandatory in the SHARK delivery template based on
#' rows starting with "*" (one or more). You can specify how many levels of
#' asterisks to include.
#'
#' Note: A single "*" marks required fields in the standard SHARK template.
#' A double "**" is often used to specify columns required for **national monitoring only**.
#' For more information, see:
#' https://www.smhi.se/data/hav-och-havsmiljo/datavardskap-oceanografi-och-marinbiologi/leverera-data
#'
#' @param datatype Character. The datatype name.
#'   Available options include:
#'   \itemize{
#'     \item "Bacterioplankton" (subtypes: "abundance", "production")
#'     \item "Chlorophyll"
#'     \item "Epibenthos" (dive transect)
#'     \item "Dropvideo" (epibenthos drop video)
#'     \item "Grey seal"
#'     \item "Harbour seal"
#'     \item "Ringed seal"
#'     \item "Harbour Porpoise"
#'     \item "Physical and Chemical"
#'     \item "Primary production"
#'     \item "Phytoplankton"
#'     \item "Picoplankton"
#'     \item "Sedimentation"
#'     \item "Seal pathology"
#'     \item "Profile"
#'     \item "Zooplankton"
#'     \item "Zoobenthos"
#'   }
#' @param stars Integer. Maximum number of "*" levels to include.
#'   Default = 1 (only single "*").
#'   For example, `stars = 2` includes "*" and "**",
#'   `stars = 3` includes "*", "**", and "***".
#' @param bacterioplankton_subtype Character. For "Bacterioplankton" only: either
#'   "abundance" (default) or "production". Ignored for other datatypes.
#'
#' @return A character vector of column names that are required in the template.
#'
#' @examples
#' \donttest{
#' # Only single "*" required columns
#' find_required_fields("Bacterioplankton")
#'
#' # Include both "*" and "**" required columns (national monitoring too)
#' find_required_fields("Bacterioplankton", stars = 2)
#'
#' # Include up to three levels of "*"
#' find_required_fields("Phytoplankton", stars = 3)
#' }
#'
#' @export
find_required_fields <- function(datatype,
                                 stars = 1,
                                 bacterioplankton_subtype = "abundance") {

  df <- get_delivery_template(datatype,
                              sheet = 3,
                              header_row = 1,
                              skip = 0,
                              bacterioplankton_subtype = bacterioplankton_subtype)

  # ensure first column is character
  col1 <- as.character(df[[1]])

  # compute number of leading stars in each entry (0 if none)
  m <- regexpr("^\\*+", col1)
  leading_stars <- ifelse(m > 0, attr(m, "match.length"), 0L)

  # select rows with 1..stars leading stars
  req_idx <- which(leading_stars > 0 & leading_stars <= stars)
  req_rows <- df[req_idx, , drop = FALSE]

  # convert to data.frame (keeps behavior from your original)
  df_req <- as.data.frame(req_rows)

  # find the column with most ALL_CAPS-like entries
  all_caps_mask <- sapply(df_req, function(col) {
    vals <- stats::na.omit(as.character(col))
    vals <- vals[vals != ""]
    if (length(vals) == 0) return(FALSE)
    sum(grepl("^[A-Z0-9_]+$", vals)) / length(vals) > 0.5
  })

  all_caps_col_index <- which(all_caps_mask)[1]

  required <- df_req[[all_caps_col_index]]
  required <- required[!is.na(required) & required != ""]

  return(required)
}
