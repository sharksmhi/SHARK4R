#' Update taxonomy from SHARKdata datasets via WoRMS API
#'
#' This function collects WoRMS (World Register of Marine Species) taxonomy information for a given set of Aphia IDs.
#' The data is organized into a full taxonomic table that can be joined with data downloaded from [SHARK](https://shark.smhi.se/).
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
#' @seealso https://cran.r-project.org/web/packages/worrms/index.html
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
#' This function retrieves records from the WoRMS (World Register of Marine Species) database using the `worrms` R package for a given list of Aphia IDs.
#' If the retrieval fails, it retries a specified number of times before stopping.
#'
#' @param aphia_id A vector of Aphia IDs for which records should be retrieved.
#' @param max_retries An integer specifying the maximum number of retry attempts for each Aphia ID in case of failure. Default is 3.
#' @param sleep_time A numeric value specifying the time (in seconds) to wait between retry attempts. Default is 10 seconds.
#' @param verbose A logical indicating whether to print progress messages. Default is TRUE.
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
#' @importFrom utils txtProgressBar setTxtProgressBar
#' 
#' @seealso \url{https://cran.r-project.org/web/packages/worrms/index.html}
#' 
#' @export
get_worms_records <- function(aphia_id, max_retries = 3, sleep_time = 10, verbose = TRUE) {
  worms_records <- list()  # Initialize an empty list to collect records for each ID
  no_content_messages <- c()  # Store "No content" messages
  
  # Set up progress bar
  if (verbose) {pb <- txtProgressBar(min = 0, max = length(aphia_id), style = 3)}
  
  for (id in seq_along(aphia_id)) {
    attempt <- 1
    worms_record <- NULL  # Initialize for the current aphia_id
    success <- FALSE  # Track whether retrieval was successful
    
    # Update progress bar
    if (verbose) {setTxtProgressBar(pb, id)}
    
    while (attempt <= max_retries && !success) {
      tryCatch({
        worms_record <- wm_record(aphia_id[id])
        
        if (!is.null(worms_record)) {
          success <- TRUE  # Exit retry loop if successful
        }
      }, error = function(err) {
        error_message <- conditionMessage(err)
        
        # Check for 204 "No Content" response
        if (grepl("204", error_message)) {
          no_content_messages <<- c(no_content_messages, 
                                    paste0("No WoRMS content for AphiaID '", aphia_id[id], "'"))
          worms_record <<- data.frame(
            AphiaID = aphia_id[id],
            status = "no content",
            stringsAsFactors = FALSE
          )
          success <<- TRUE  # Mark success to prevent further retries
        } else if (attempt == max_retries) {
          stop("Error occurred while retrieving WoRMS record for AphiaID ", aphia_id[id],
               " after ", max_retries, " attempts: ", error_message)
        } else {
          message("Attempt ", attempt, " failed for AphiaID ", aphia_id[id], ": ", 
                  error_message, " - Retrying...")
          Sys.sleep(sleep_time)
        }
      })
      
      attempt <- attempt + 1
    }
    
    # If still NULL after retries, insert a placeholder record
    if (is.null(worms_record)) {
      worms_record <- data.frame(
        AphiaID = aphia_id[id],
        status = "Failed",
        stringsAsFactors = FALSE
      )
    }
    
    worms_records <- bind_rows(worms_records, worms_record)  # Combine results
  }
  
  if (verbose) {close(pb)}
  
  # Print all "No content" messages after progress bar finishes
  if (verbose && length(no_content_messages) > 0) {
    cat(paste(no_content_messages, collapse = "\n"), "\n")
  }
  
  worms_records
}
#' Retrieve WoRMS Records by Taxonomic Names with Retry Logic
#'
#' This function retrieves records from the WoRMS database using the `worrms` R package for a vector of taxonomic names.
#' It includes retry logic to handle temporary failures and ensures all names are processed.
#'
#' @param taxa_names A vector of taxonomic names for which to retrieve records.
#' @param fuzzy A logical value indicating whether to search using a fuzzy search pattern. Default is TRUE.
#' @param best_match_only A logical value indicating whether to automatically select the first match and return a single match. Default is TRUE.
#' @param max_retries An integer specifying the maximum number of retries for the request in case of failure. Default is 3.
#' @param sleep_time A numeric value specifying the number of seconds to wait before retrying a failed request. Default is 10.
#' @param marine_only A logical value indicating whether to restrict the results to marine taxa only. Default is `FALSE`.
#' @param verbose A logical indicating whether to print progress messages. Default is TRUE.
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
#' @importFrom worrms wm_records_name
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @seealso \url{https://cran.r-project.org/web/packages/worrms/index.html}
#'
#' @export
get_worms_records_name <- function(taxa_names, fuzzy = TRUE, best_match_only = TRUE, 
                                   max_retries = 3, sleep_time = 10, marine_only = TRUE, 
                                   verbose = TRUE) {
  worms_records <- list()  # Initialize an empty list to collect records for each name
  
  # Set up progress bar
  if (verbose) {pb <- txtProgressBar(min = 0, max = length(taxa_names), style = 3)}
  
  no_content_messages <- c()  # Store "No content" messages
  
  for (i in seq_along(taxa_names)) {
    attempt <- 1
    worms_record <- NULL  # Reset for the current taxon name
    success <- FALSE      # Track whether retrieval was successful
    
    # Update progress bar
    if (verbose) {setTxtProgressBar(pb, i)}
    
    while (attempt <= max_retries && !success) {
      tryCatch({
        worms_record <- data.frame(
          name = taxa_names[i],
          wm_records_name(taxa_names[i], 
                          fuzzy = fuzzy,
                          marine_only = marine_only)
        )
        
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
          no_content_messages <<- c(no_content_messages, 
                                    paste0("No WoRMS content for '", taxa_names[i], "'"))
          worms_record <<- data.frame(name = taxa_names[i], status = "no content", AphiaID = NA, stringsAsFactors = FALSE)
          success <<- TRUE  # Mark success to prevent further retries
        } else if (attempt == max_retries) {
          stop("Error occurred while retrieving WoRMS record for '", taxa_names[i], 
               "' after ", max_retries, " attempts: ", error_message)
        } else {
          Sys.sleep(sleep_time)
        }
      })
      
      attempt <- attempt + 1
    }
    
    # If still NULL after retries, insert NA
    if (is.null(worms_record)) {
      worms_record <- data.frame(name = taxa_names[i], status = "Failed", stringsAsFactors = FALSE)
    }
    
    worms_records <- bind_rows(worms_records, worms_record)  # Combine results into a single data frame
  }
  
  if (verbose) {close(pb)}
  
  # Print all "No content" messages after progress bar finishes
  if (verbose && length(no_content_messages) > 0) {
    cat(paste(no_content_messages, collapse = "\n"), "\n")
  }
  
  worms_records
}
#' Assign Phytoplankton Group to Scientific Names
#' 
#' This function assigns phytoplankton groups (Diatoms, Dinoflagellates, Cyanobacteria, or Other) 
#' to a list of scientific names or Aphia IDs by retrieving species information from the 
#' World Register of Marine Species (WoRMS). The function checks both Aphia IDs and scientific names, 
#' handles missing records, and assigns the appropriate plankton group based on taxonomic classification in WoRMS.
#'
#' @param scientific_names A character vector of scientific names of marine species.
#' @param aphia_ids A numeric vector of Aphia IDs corresponding to the scientific names. If provided, it improves the accuracy and speed of the matching process. The length of `aphia_ids` must match the length of `scientific_names`. Defaults to `NULL`, in which case the function will attempt to assign plankton groups based only on the scientific names.
#' @param diatom_class A character string representing the diatom class. Default is "Bacillariophyceae".
#' @param dinoflagellate_class A character string representing the dinoflagellate class. Default is "Dinophyceae".
#' @param cyanobacteria_class A character string representing the cyanobacteria class. Default is "Cyanophyceae".
#' @param cyanobacteria_phylum A character string representing the cyanobacteria phylum. Default is "Cyanobacteria".
#' @param match_first_word A logical value indicating whether to match the first word of the scientific name if the Aphia ID is missing. Default is TRUE.
#' @param marine_only A logical value indicating whether to restrict the results to marine taxa only. Default is `FALSE`.
#' @param verbose A logical value indicating whether to print progress messages. Default is TRUE.
#'
#' @return A data frame with two columns: `scientific_name` and `plankton_group`, where the plankton group is assigned based on taxonomic classification.
#'
#' @details The `aphia_ids` parameter is not necessary but, if provided, will improve the certainty of the 
#'   matching process. If `aphia_ids` are available, they will be used directly to retrieve more accurate 
#'   WoRMS records. If missing, the function will attempt to match the scientific names to Aphia IDs by 
#'   querying WoRMS using the scientific name(s), with an additional fallback mechanism to match based on the 
#'   first word of the scientific name.
#'
#' @examples
#' \dontrun{
#' # Assign plankton groups to a list of species
#' result <- assign_phytoplankton_group(
#'   scientific_names = c("Tripos fusus", "Diatoma", "Nodularia spumigena", "Octactis speculum"),
#'   aphia_ids = c(840626, 149013, 160566, NA)
#' )
#'}
#'
#' @importFrom dplyr bind_rows case_when distinct filter left_join mutate select
#' @importFrom stringr word
#' @importFrom magrittr %>%
#' 
#' @seealso \url{https://cran.r-project.org/web/packages/worrms/index.html}
#' 
#' @export
assign_phytoplankton_group <- function(scientific_names, aphia_ids = NULL, diatom_class = "Bacillariophyceae", 
                                       dinoflagellate_class = "Dinophyceae", cyanobacteria_class = "Cyanophyceae", 
                                       cyanobacteria_phylum = "Cyanobacteria", match_first_word = TRUE, 
                                       marine_only = FALSE, verbose = TRUE) {
  # Ensure input lengths match
  if (!length(aphia_ids) == length(scientific_names) & !is.null(aphia_ids)) {
    stop("'aphia_ids' and 'scientific_names' must have the same length.")
  }
  
  # Create a data frame to store input data
  if (is.null(aphia_ids)) {
    input_data <- data.frame(aphia_id = NA, scientific_name = scientific_names)
  } else {
    input_data <- data.frame(aphia_id = aphia_ids, scientific_name = scientific_names)
  }
  
  # Remove duplicates
  unique_data <- distinct(input_data)
  
  # Extract unique non-NA AphiaIDs
  valid_aphia_ids <- unique_data$aphia_id[!is.na(unique_data$aphia_id)]
  
  # Retrieve WoRMS records based on AphiaID
  if (length(valid_aphia_ids) > 0) {
    if (verbose) cat("Retrieving", length(valid_aphia_ids), "WoRMS records from input 'aphia_ids'.\n")
    aphia_records <- get_worms_records(valid_aphia_ids, verbose = verbose) %>% 
      mutate(class = ifelse(status == "no content", NA, class),
             phylum = ifelse(status == "no content", NA, phylum))
  } else {
    aphia_records <- data.frame(AphiaID = NA_integer_, 
                                status = NA_character_, 
                                class = NA_character_, 
                                phylum = NA_character_, 
                                stringsAsFactors = FALSE)
  }
  
  # List IDs without content
  no_content <- aphia_records %>%
    filter(status == "no content")
  
  # List IDs with content
  aphia_records <- aphia_records %>%
    filter(!status == "no content")
  
  # Handle entries with missing AphiaIDs
  missing_aphia_data <- unique_data[is.na(unique_data$aphia_id) | unique_data$aphia_id %in% no_content$AphiaID,]
  if (nrow(missing_aphia_data) > 0) {
    if (verbose) cat("Retrieving", nrow(missing_aphia_data), "WoRMS records from input 'scientific_names'.\n")
    missing_aphia_records <- get_worms_records_name(missing_aphia_data$scientific_name, 
                                                    marine_only = marine_only,
                                                    verbose = verbose) %>% 
      mutate(class = ifelse(status == "no content", NA, class),
             phylum = ifelse(status == "no content", NA, phylum))
  } else {
    missing_aphia_records <- data.frame(name = NA_character_,
                                        AphiaID = NA_integer_,
                                        class = NA_character_,
                                        phylum = NA_character_,
                                        stringsAsFactors = FALSE)
  }

  # Merge records into input data
  matched_aphia_data <- unique_data %>%
    left_join(distinct(aphia_records), by = c("aphia_id" = "AphiaID")) %>%
    filter(!is.na(aphia_id) & !is.na(status))
  
  matched_missing_data <- missing_aphia_data %>%
    left_join(missing_aphia_records, by = c("scientific_name" = "name")) %>%
    mutate(aphia_id = AphiaID)
  
  combined_data <- bind_rows(matched_aphia_data, matched_missing_data)
  
  # Handle still-missing entries
  unresolved_data <- combined_data %>%
    filter(is.na(aphia_id))
  
  if (match_first_word & nrow(unresolved_data) > 0) {
    first_words <- word(unresolved_data$scientific_name, 1)
    if (verbose) cat("Retrieving", length(first_words), "WoRMS records from the first word in 'scientific_names'.\n")
    first_word_records <- data.frame(
      scientific_name = unresolved_data$scientific_name,
      get_worms_records_name(first_words, 
                             marine_only = marine_only,
                             verbose = verbose)
    ) %>%
      filter(!is.na(AphiaID)) %>%
      mutate(aphia_id = AphiaID)
    
    combined_data <- combined_data %>%
      filter(!scientific_name %in% first_word_records$scientific_name) %>%
      bind_rows(first_word_records)
  }
  
  # Handle deleted records
  deleted_records <- combined_data %>%
    filter(status == "deleted")
  
  if (nrow(deleted_records) > 0) {
    if (verbose) cat("Retrieving", nrow(deleted_records), "valid 'aphia_id' records from deleted entries.\n")
    valid_deleted_records <- data.frame(
      aphia_id = deleted_records$valid_AphiaID,
      scientific_name = deleted_records$scientific_name,
      get_worms_records(deleted_records$valid_AphiaID, verbose = verbose)
    )
  } else {
    valid_deleted_records <- data.frame()
  }
  
  combined_data <- combined_data %>%
    filter(!scientific_name %in% valid_deleted_records$scientific_name) %>%
    bind_rows(valid_deleted_records)
  
  classes <- unique(combined_data$class)
  other_classes <- classes[!classes %in% c(diatom_class, dinoflagellate_class, cyanobacteria_class)]
  
  # Assign plankton groups
  class_mapping <- combined_data %>%
    select(aphia_id, scientific_name, class, phylum) %>%
    mutate(plankton_group = case_when(
      class %in% diatom_class ~ "Diatoms",
      class %in% dinoflagellate_class ~ "Dinoflagellates",
      class %in% cyanobacteria_class ~ "Cyanobacteria",
      phylum %in% cyanobacteria_phylum ~ "Cyanobacteria",
      class %in% other_classes & !(phylum %in% cyanobacteria_phylum | is.na(phylum)) ~ "Other",
      TRUE ~ NA_character_
    ))
  
  # Handle unknown classifications
  unknown_classifications <- class_mapping %>%
    filter(is.na(plankton_group))
  
  for (group in seq_along(unknown_classifications$scientific_name)) {
    message("No plankton group found for '", unknown_classifications$scientific_name[group], "', placing in 'Other'.")
  }
  
  class_mapping <- class_mapping %>%
    mutate(plankton_group = ifelse(is.na(plankton_group), "Other", plankton_group)) %>%
    select(scientific_name, plankton_group) %>%
    distinct()
  
  # Finalize output
  final_output <- input_data %>%
    left_join(class_mapping, by = "scientific_name", relationship = "many-to-many") %>%
    select(-aphia_id)
  
  return(final_output)
}
