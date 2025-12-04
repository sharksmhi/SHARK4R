#' Retrieve marine biotoxin data from IOC-UNESCO Toxins Database
#'
#' This function collects data from the [IOC-UNESCO Toxins Database](https://toxins.hais.ioc-unesco.org/) and returns information about toxins.
#'
#' @param return_count Logical. If `TRUE`, the function returns the count of toxins available in the database. If `FALSE` (default), it returns detailed toxin data.
#'
#' @return If `return_count = TRUE`, the function returns a numeric value representing the number of toxins in the database. Otherwise, it returns a `tibble` of toxins with detailed information.
#'
#' @seealso \url{https://toxins.hais.ioc-unesco.org/} for IOC-UNESCO Toxins Database.
#'
#' @examples
#' \donttest{
#' # Retrieve the full list of toxins
#' toxin_list <- get_toxin_list()
#' head(toxin_list)
#'
#' # Retrieve only the count of toxins
#' toxin_count <- get_toxin_list(return_count = TRUE)
#' print(toxin_count)
#' }
#'
#' @export
get_toxin_list <- function(return_count = FALSE) {

  url_toxins <- "https://toxins.hais.ioc-unesco.org/api/toxins/"

  temp_file <- tempfile(fileext = ".json")

  res <- tryCatch(
    GET(url_toxins, write_disk(temp_file, overwrite = TRUE), timeout(300)),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    warning(
      "Partial response detected during download. Returning only complete toxin records. ",
      "Error: ", conditionMessage(res)
    )
  }

  txt <- readLines(temp_file, warn = FALSE)
  json_raw <- paste(txt, collapse = "")

  # Try direct parse to data frame
  parsed <- try(fromJSON(json_raw, simplifyDataFrame = TRUE), silent = TRUE)

  if (!inherits(parsed, "try-error")) {
    if (return_count) return(nrow(parsed$toxins))
    else return(as_tibble(parsed$toxins))
  }

  # Fallback: repair partial JSON
  repaired <- repair_toxins_json(json_raw)
  parsed_recovered <- fromJSON(repaired, simplifyDataFrame = TRUE)

  if (return_count) {
    return(nrow(parsed_recovered$toxins))
  } else {
    return(as_tibble(parsed_recovered$toxins))
  }
}
#' Download the IOC-UNESCO Taxonomic Reference List of Harmful Micro Algae
#'
#' This function retrieves the IOC-UNESCO Taxonomic Reference List of Harmful Micro Algae from the World Register of Marine Species (WoRMS).
#' The data is returned as a dataframe, with options to customize the fields included in the download.
#'
#' @param aphia_id Logical. Include the AphiaID field. Defaults to `TRUE`.
#' @param scientific_name Logical. Include the scientific name field. Defaults to `TRUE`.
#' @param authority Logical. Include the authority field. Defaults to `TRUE`.
#' @param fossil Logical. Include information about fossil status. Defaults to `TRUE`.
#' @param rank_name Logical. Include the taxonomic rank (e.g., species, variety, forma). Defaults to `TRUE`.
#' @param status_name Logical. Include the taxonomic status field. Defaults to `TRUE`.
#' @param qualitystatus_name Logical. Include the quality status field. Defaults to `TRUE`.
#' @param modified Logical. Include the date of last modification field. Defaults to `TRUE`.
#' @param lsid Logical. Include the Life Science Identifier (LSID) field. Defaults to `TRUE`.
#' @param parent_id Logical. Include the parent AphiaID field. Defaults to `TRUE`.
#' @param stored_path Logical. Include the stored path field. Defaults to `TRUE`.
#' @param citation Logical. Include citation information. Defaults to `TRUE`.
#' @param classification Logical. Include the full taxonomic classification (e.g., kingdom, phylum, class). Defaults to `TRUE`.
#' @param environment Logical. Include environmental data (e.g., marine, brackish, freshwater, terrestrial). Defaults to `TRUE`.
#' @param accepted_taxon Logical. Include information about the accepted taxon (e.g., scientific name and authority). Defaults to `TRUE`.
#'
#' @return A `tibble` containing the HABs taxonomic list, with columns based on the selected parameters.
#' @export
#'
#' @details
#' This function submits a POST request to the WoRMS database to retrieve the IOC-UNESCO Taxonomic Reference List of Harmful Micro Algae.
#' The downloaded data can include various fields, which are controlled by the input parameters.
#' If a field is not required, set the corresponding parameter to `FALSE` to exclude it from the output.
#'
#' @seealso \url{https://www.marinespecies.org/hab/} for IOC-UNESCO Taxonomic Reference List of Harmful Micro Algae
#'
#' @examples
#' \donttest{
#' # Download the default HABs taxonomic list
#' habs_taxlist_df <- get_hab_list()
#' head(habs_taxlist_df)
#'
#' # Include only specific fields in the output
#' habs_taxlist_df <- get_hab_list(aphia_id = TRUE, scientific_name = TRUE, authority = FALSE)
#' head(habs_taxlist_df)
#' }
get_hab_list <- function(aphia_id = TRUE, scientific_name = TRUE, authority = TRUE, fossil = TRUE,
                         rank_name = TRUE, status_name = TRUE, qualitystatus_name = TRUE,
                         modified = TRUE, lsid = TRUE, parent_id = TRUE, stored_path = TRUE,
                         citation = TRUE, classification = TRUE, environment = TRUE,
                         accepted_taxon = TRUE) {

  # Check if all parameters are FALSE
  params <- c(aphia_id, scientific_name, authority, fossil,
              rank_name, status_name, qualitystatus_name,
              modified, lsid, parent_id, stored_path,
              citation, classification, environment,
              accepted_taxon)

  if (!any(params)) {
    stop("At least one column must be selected (i.e., one parameter set to TRUE).")
  }

  # Convert TRUE/FALSE parameters to "1"/"0" for the request
  body <- list(
    output_type = "txt",
    id = ifelse(aphia_id, "1", "0"),
    dn = ifelse(scientific_name, "1", "0"),
    auth = ifelse(authority, "1", "0"),
    tu_fossil = ifelse(fossil, "1", "0"),
    RankName = ifelse(rank_name, "1", "0"),
    status_name = ifelse(status_name, "1", "0"),
    qualitystatus_name = ifelse(qualitystatus_name, "1", "0"),
    modified = ifelse(modified, "1", "0"),
    lsid = ifelse(lsid, "1", "0"),
    tu_parent = ifelse(parent_id, "1", "0"),
    tu_sp = ifelse(stored_path, "1", "0"),
    citation = ifelse(citation, "1", "0"),
    Classification = ifelse(classification, "1", "0"),
    Environment = ifelse(environment, "1", "0"),
    Accepted_taxon = ifelse(accepted_taxon, "1", "0")
  )

  # URL to submit the download form for HABs
  url_habs <- "https://www.marinespecies.org/hab/aphia.php?p=export&what=taxlist"

  # Download the file directly into memory
  response <- POST(url_habs, body = body)

  # Check if the file was successfully downloaded
  if (response$status_code == 200) {

    # Read the raw content as text
    content_raw <- content(response, as = "raw", encoding = "UTF-8")
    content_text <- rawToChar(content_raw)

    # Load the data into a dataframe using read.delim on the text
    habs_taxlist_df <- read_delim(
      file = content_text,
      delim = "\t",
      col_types = cols(),
      na = c("", "NA"),
      progress = FALSE
    )

    # Return the dataframe
    return(habs_taxlist_df)
  } else {
    stop("Failed to download the HABs list. Status code:", response$status_code)
  }
}

# Helpers
extract_complete_toxins <- function(json_raw) {
  start <- regexpr('"toxins"\\s*:\\s*\\[', json_raw)
  if (start < 0) return(character())

  pos <- start + attr(start, "match.length")
  chars <- strsplit(substr(json_raw, pos, nchar(json_raw)), "")[[1]]

  out <- character()
  buffer <- ""
  depth <- 0
  in_object <- FALSE

  for (ch in chars) {
    buffer <- paste0(buffer, ch)
    if (ch == "{") { depth <- depth + 1; in_object <- TRUE }
    else if (ch == "}") { depth <- depth - 1 }

    if (in_object && depth == 0) {
      obj <- gsub(",$", "", buffer)
      obj <- trimws(obj)
      obj <- sub('^,', '', obj)
      obj <- sub(',$', '', obj)
      if (nchar(obj) > 2) out <- c(out, obj)
      buffer <- ""
      in_object <- FALSE
    }
  }
  out
}

repair_toxins_json <- function(json_raw) {
  objs <- extract_complete_toxins(json_raw)
  if (!length(objs)) return('{"toxins": []}')
  paste0('{"toxins": [', paste(objs, collapse = ","), "]}")
}
