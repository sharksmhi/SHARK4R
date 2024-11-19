#' Update taxonomy from SHARKdata datasets via WoRMS API
#'
#' This function collects WoRMS (World Register of Marine Species) taxonomy information for a given set of Aphia IDs.
#' The data is organized into a full taxonomic table that can be joined with data downloaded from [SHARKdata](https://sharkdata.smhi.se/)
#'
#' @param aphiaid A numeric vector containing Aphia IDs for which WoRMS taxonomy needs to be updated.
#'
#' @return A data frame containing updated WoRMS taxonomy information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Update WoRMS taxonomy for a set of Aphia IDs
#' updated_taxonomy <- update_worms_taxonomy(c(149619, 149122, 11))
#' print(updated_taxonomy)
#' }
#'
#' @importFrom worrms wm_classification
#' @importFrom dplyr select mutate rename bind_rows relocate any_of last_col last
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#'
#' @seealso \code{\link{download_sharkdata}}, \code{\link{update_dyntaxa_taxonomy}}, [WoRMS API Documentation](http://www.marinespecies.org/rest/)
#'
update_worms_taxonomy <- function(aphiaid) {
  worms_class <- data.frame()
  for (ids in aphiaid) {
    tryCatch({
      worms_class_i <- wm_classification(ids) %>%
        select(-AphiaID) %>%
        mutate(scientific_name = last(scientificname)) %>%
        pivot_wider(names_from = rank, values_from = scientificname) %>%
        mutate(worms_hierarchy = paste(na.omit(.), collapse = " - "),
               aphia_id = ids) %>%
        mutate(Kingdom = ifelse("Kingdom" %in% names(.), Kingdom, NA),
               Phylum = ifelse("Phylum" %in% names(.), Phylum, NA),
               Class = ifelse("Class" %in% names(.), Class, NA),
               Order = ifelse("Order" %in% names(.), Order, NA),
               Family = ifelse("Family" %in% names(.), Family, NA),
               Genus = ifelse("Genus" %in% names(.), Genus, NA),
               Species = ifelse("Species" %in% names(.), Species, NA)) %>%
        rename(worms_kingdom = Kingdom,
               worms_phylum = Phylum,
               worms_class = Class,
               worms_order = Order,
               worms_family = Family,
               worms_genus = Genus,
               worms_species = Species)
    }, error=function(e) {
      worms_class_i <<- data.frame(aphia_id = ids)
    })
    worms_class <- bind_rows(worms_class, worms_class_i)
  }

  names <- c("aphia_id", "scientific_name", names(worms_class)[grepl("worms_", names(worms_class))])
  
  return(worms_class %>%
           select(any_of(names)) %>%
           relocate(worms_hierarchy, .after = last_col()))
}
#' Retrieve WoRMS Records
#'
#' This function retrieves records from the WoRMS (World Register of Marine Species) database for a given list of Aphia IDs.
#' If the retrieval fails, it retries a specified number of times before stopping.
#'
#' @param aphia_id A vector of Aphia IDs for which records should be retrieved.
#' @param max_retries An integer specifying the maximum number of retry attempts for each Aphia ID in case of failure. Default is 3.
#' @param sleep_time A numeric value specifying the time (in seconds) to wait between retry attempts. Default is 10 seconds.
#'
#' @return A data frame containing the retrieved WoRMS records for the provided Aphia IDs. Each row corresponds to one Aphia ID.
#' 
#' @details The function attempts to fetch records for each Aphia ID in the provided vector. If a retrieval fails, it retries up to
#' the specified `max_retries`, with a pause of `sleep_time` seconds between attempts. If all retries fail for an Aphia ID, the function
#' stops with an error message.
#'
#' @examples
#' \dontrun{
#' # Example usage with a vector of Aphia IDs
#' aphia_ids <- c(12345, 67890, 112233)
#' worms_records <- retrieve_worms_records(aphia_ids)
#' }
#'
#' @seealso [wm_record()] for fetching individual WoRMS records by Aphia ID.
#'
#' @importFrom dplyr bind_rows
#' @importFrom worrms wm_record
#' 
#' @export
get_worms_records <- function(aphia_id, max_retries = 3, sleep_time = 10) {
  worms_records <- list()
  
  for (id in seq_along(aphia_id)) {
    attempt <- 1
    worms_record <- NULL  # Initialize for the current aphia_id
    
    while (attempt <= max_retries) {
      tryCatch({
        worms_record <- wm_record(aphia_id[id])
        if (!is.null(worms_record)) break  # Exit retry loop if successful
      }, error = function(err) {
        if (attempt == max_retries) {
          stop("Error occurred while retrieving WoRMS records for ID ", aphia_id[id], 
               " after ", max_retries, " attempts: ", conditionMessage(err))
        } else {
          message("Attempt ", attempt, " failed for ID ", aphia_id[id], ": ", 
                  conditionMessage(err), " - Retrying...")
          Sys.sleep(sleep_time)
        }
      })
      
      attempt <- attempt + 1
    }
    
    if (!is.null(worms_record)) {
      worms_records <- bind_rows(worms_records, worms_record)
    }
  }
  
  worms_records
}
#' Retrieve WoRMS Records by Taxonomic Names with Retry Logic
#'
#' This function retrieves records from the WoRMS database for a vector of taxonomic names.
#' It includes retry logic to handle temporary failures and ensures all names are processed.
#'
#' @param taxa_names A vector of taxonomic names for which to retrieve records.
#' @param fuzzy A logical value indicating whether to search using a fuzzy search pattern. Default is TRUE.
#' @param best_match_only A logical value indicating whether to automatically select the first match and return a single match. Default is TRUE.
#' @param max_retries An integer specifying the maximum number of retries for the request in case of failure. Default is 3.
#' @param sleep_time A numeric value specifying the number of seconds to wait before retrying a failed request. Default is 10.
#' @param marine_only A logical value indicating whether to restrict the results to marine taxa only. Default is `FALSE`.
#' @param verbose Logical. Default is TRUE.
#'
#' @return A data frame containing the retrieved WoRMS records. Each row corresponds to a record for a taxonomic name.
#'
#' @details
#' The function attempts to retrieve records for the input taxonomic names using the `wm_records_names` function from the WoRMS API.
#' If a request fails, it retries up to `max_retries` times, pausing for `sleep_time` seconds between attempts.
#' If all attempts fail, the function stops and throws an error.
#'
#' @examples
#' \dontrun{
#' # Retrieve WoRMS records for the taxonomic names "Amphidinium" and "Karenia"
#' records <- retrieve_worms_records_name(c("Amphidinium", "Karenia"), 
#'                                        max_retries = 3, sleep_time = 5, marine_only = TRUE)
#' }
#'
#' @seealso [wm_records_names()] for retrieving records by taxonomic names from the WoRMS database.
#'
#' @importFrom dplyr bind_rows
#' @importFrom worrms wm_records_names
#'
#' @export
get_worms_records_name <- function(taxa_names, fuzzy = TRUE, best_match_only = TRUE, max_retries = 3, sleep_time = 10, marine_only = FALSE, verbose = TRUE) {
  worms_records <- list()  # Initialize an empty list to collect records for each name
  
  for (name in taxa_names) {
    attempt <- 1
    worms_record <- NULL  # Reset for the current taxon name
    success <- FALSE      # Track whether retrieval was successful
    
    while (attempt <= max_retries && !success) {
      tryCatch({
        worms_record <- data.frame(name,
                                   wm_records_name(name, 
                                                   fuzzy = fuzzy,
                                                   marine_only = marine_only))
        
        if (best_match_only) {
          worms_record <- worms_record[1,]
        }
        
        if (!is.null(worms_record)) {
          success <- TRUE  # Mark success to exit loop
        }
      }, error = function(err) {
        error_message <- conditionMessage(err)
        
        # Check for 204 "No Content" response
        if (grepl("204", error_message)) {
          if(verbose) {message("No WoRMS content for '", name, "' - Proceeding with NA.")}
          worms_record <<- data.frame(name = name, status = "no content", stringsAsFactors = FALSE)
          success <<- TRUE  # Mark success to prevent further retries
        } else if (attempt == max_retries) {
          stop("Error occurred while retrieving WoRMS record for '", name, 
               "' after ", max_retries, " attempts: ", error_message)
        } else {
          if(verbose) {message("Attempt ", attempt, " failed for '", name, "': ", 
                  error_message, " - Retrying...")}
          Sys.sleep(sleep_time)
        }
      })
      
      attempt <- attempt + 1
    }
    
    # If still NULL after retries, insert NA
    if (is.null(worms_record)) {
      worms_record <- data.frame(name = name, status = "Failed", stringsAsFactors = FALSE)
    }
    
    worms_records <- bind_rows(worms_records, worms_record)  # Combine results into a single data frame
  }
  
  worms_records
}
