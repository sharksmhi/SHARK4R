#' Retrieve and Organize WoRMS Taxonomy for SHARK Aphia IDs
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated and replaced by a function with more accurate name. Use [add_worms_taxonomy()] instead.
#'
#' This function collects WoRMS (World Register of Marine Species) taxonomy information for a given set of Aphia IDs.
#' The data is organized into a full taxonomic table that can be joined with data downloaded from [SHARK](https://shark.smhi.se/).
#'
#' @param aphia_id A numeric vector containing Aphia IDs for which WoRMS taxonomy needs to be updated.
#' @param aphiaid
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{aphia_id} instead.
#'
#' @return A data frame containing updated WoRMS taxonomy information.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Update WoRMS taxonomy for a set of Aphia IDs
#' updated_taxonomy <- update_worms_taxonomy(c(149619, 149122, 11))
#' print(updated_taxonomy)
#' }
#'
#' @seealso https://CRAN.R-project.org/package=worrms
#'
#' @seealso \code{\link{get_shark_data}}, \code{\link{update_dyntaxa_taxonomy}}, [WoRMS API Documentation](https://www.marinespecies.org/rest/)
#'
update_worms_taxonomy <- function(aphia_id, aphiaid=deprecated()) {
  if (is_present(aphiaid)) {

    # Signal the deprecation to the user
    deprecate_warn("0.1.3", "SHARK4R::update_worms_taxonomy(aphiaid = )", "SHARK4R::update_worms_taxonomy(aphia_id = )")

    # Deal with the deprecated argument for compatibility
    aphia_id <- aphiaid
  }

  lifecycle::deprecate_warn("0.1.3", "SHARK4R::update_worms_taxonomy()", "SHARK4R::add_worms_taxonomy()")

  add_worms_taxonomy(aphia_id)
}
#' Retrieve and Organize WoRMS Taxonomy for SHARK Aphia IDs
#'
#' This function collects WoRMS (World Register of Marine Species) taxonomy information for a given set of Aphia IDs.
#' The data is organized into a full taxonomic table that can be joined with data downloaded from [SHARK](https://shark.smhi.se/).
#'
#' @param aphia_id A numeric vector containing Aphia IDs for which WoRMS taxonomy needs to be updated.
#' @param scientific_name A character vector of scientific names. If provided, Aphia IDs will be retrieved from the scientific names. The length of `scientific_name` must match the length of `aphia_id`. Defaults to `NULL`, in which case the function will only add taxonomy to the provided Aphia IDs.
#' @param verbose A logical indicating whether to print progress messages. Default is TRUE.
#'
#' @return A data frame containing current WoRMS taxonomy information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Update WoRMS taxonomy for a set of Aphia IDs
#' updated_taxonomy <- add_worms_taxonomy(c(149619, 149122, 11))
#' print(updated_taxonomy)
#'
#' # Update WoRMS with an unknown Aphia ID and scientific names
#' with_names <- add_worms_taxonomy(c(149619, 149122, 11, NA),
#'                                  c("Cerataulina pelagica",
#'                                    "Chaetoceros didymus",
#'                                    "Ciliophora",
#'                                    "Dinophysis"))
#' print(with_names)
#' }
#'
#' @seealso \code{\link{get_shark_data}}, \code{\link{update_dyntaxa_taxonomy}}, \url{https://www.marinespecies.org/rest/}, \url{https://CRAN.R-project.org/package=worrms}
add_worms_taxonomy <- function(aphia_id, scientific_name = NULL, verbose = TRUE) {

  # Ensure input lengths match
  if (!length(aphia_id) == length(scientific_name) & !is.null(scientific_name)) {
    stop("'scientific_name' and 'aphia_id' must have the same length.")
  }

  if (!is.null(scientific_name)) {
    aphia_id_df <- data.frame(aphia_id, scientific_name)

    to_match <- scientific_name[is.na(aphia_id)]

    if (verbose) cat("Retrieving", length(to_match), "'aphia_ids' from 'scientific_name'.\n")

    # Get all records from scientific_name
    worms_records <- get_worms_records_name(to_match,
                                            verbose = verbose)

    # Select relevant information
    name <- select(worms_records, name, AphiaID)

    aphia_id_df <- aphia_id_df %>%
      left_join(name, by = c("scientific_name" = "name")) %>%
      mutate(aphia_id = coalesce(aphia_id, AphiaID))

    aphia_id <- aphia_id_df$aphia_id
  }

  # Set up progress bar
  if (verbose) {
    cat("Retrieving", length(aphia_id), "records from 'aphia_ids'.\n")
    pb <- txtProgressBar(min = 0, max = length(aphia_id), style = 3)
    }

  worms_class <- data.frame()
  for (i in seq_along(aphia_id)) {

    # Update progress bar
    if (verbose) {setTxtProgressBar(pb, i)}

    tryCatch({
      worms_class_i <- wm_classification(aphia_id[i]) %>%
        select(-AphiaID) %>%
        mutate(scientific_name = last(scientificname)) %>%
        pivot_wider(names_from = rank, values_from = scientificname) %>%
        mutate(worms_hierarchy = paste(drop_na(.), collapse = " - "),
               aphia_id = aphia_id[i]) %>%
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
      worms_class_i <<- data.frame(aphia_id = aphia_id[i])
    })
    worms_class <- bind_rows(worms_class, worms_class_i)
  }

  if (verbose) {close(pb)}

  if (ncol(worms_class) == 1) {
    stop("No WoRMS records found")
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
#' @seealso \url{https://CRAN.R-project.org/package=worrms}
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
#' @seealso \url{https://CRAN.R-project.org/package=worrms}
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
#' This function assigns default phytoplankton groups (Diatoms, Dinoflagellates, Cyanobacteria, or Other)
#' to a list of scientific names or Aphia IDs by retrieving species information from the
#' World Register of Marine Species (WoRMS). The function checks both Aphia IDs and scientific names,
#' handles missing records, and assigns the appropriate plankton group based on taxonomic classification in WoRMS.
#' Additionally, custom plankton groups can be specified using the `custom_groups` parameter,
#' allowing users to define additional classifications based on specific taxonomic criteria.
#'
#' @param scientific_names A character vector of scientific names of marine species.
#' @param aphia_ids A numeric vector of Aphia IDs corresponding to the scientific names. If provided, it improves the accuracy and speed of the matching process. The length of `aphia_ids` must match the length of `scientific_names`. Defaults to `NULL`, in which case the function will attempt to assign plankton groups based only on the scientific names.
#' @param diatom_class A character string or vector representing the diatom class. Default is "Bacillariophyceae", "Coscinodiscophyceae" and "Mediophyceae".
#' @param dinoflagellate_class A character string or vector representing the dinoflagellate class. Default is "Dinophyceae".
#' @param cyanobacteria_class A character string or vector representing the cyanobacteria class. Default is "Cyanophyceae".
#' @param cyanobacteria_phylum A character string or vector representing the cyanobacteria phylum. Default is "Cyanobacteria".
#' @param match_first_word A logical value indicating whether to match the first word of the scientific name if the Aphia ID is missing. Default is TRUE.
#' @param marine_only A logical value indicating whether to restrict the results to marine taxa only. Default is `FALSE`.
#' @param return_class A logical value indicating whether to include class information in the result. Default is `FALSE`.
#' @param custom_groups A named list of additional custom plankton groups (optional). The names of the list correspond to the custom group names (e.g., "Cryptophytes"), and the values should be character vectors specifying one or more of the following taxonomic levels: `phylum`, `class`, `order`, `family`, `genus`, or `scientific_name`. For example:
#'   \code{list("Green Algae" = list(class = c("Chlorophyceae", "Ulvophyceae")))}.
#'   This allows users to extend the default classifications (e.g., Cyanobacteria, Diatoms, Dinoflagellates) with their own groups.
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
#'   To skip one of the default plankton groups, you can set the class or phylum of the respective group to an empty string (`""`).
#'   For example, to skip the "Cyanobacteria" group, you can set `cyanobacteria_class = ""` or `cyanobacteria_phylum = ""`. These
#'   taxa will then be placed in `Others`.
#'
#'   Custom groups are processed in the order they appear in the `custom_groups` list. If a taxon matches
#'   multiple custom groups, it will be assigned to the group that appears last in the list, as later matches
#'   overwrite earlier ones. For example, if `Teleaulax amphioxeia` matches both `Cryptophytes` (class-based)
#'   and a specific group `Teleaulax` (name-based), it will be assigned to `Teleaulax` if `Teleaulax` is listed after
#'   `Cryptophytes` in the `custom_groups` list.
#'
#' @examples
#' \dontrun{
#' # Assign plankton groups to a list of species
#' result <- assign_phytoplankton_group(
#'   scientific_names = c("Tripos fusus", "Diatoma", "Nodularia spumigena", "Octactis speculum"),
#'   aphia_ids = c(840626, 149013, 160566, NA))
#'
#' print(result)
#'
#' # Assign plankton groups using additional custom grouping
#' custom_groups <- list(
#'     Cryptophytes = list(class = "Cryptophyceae"),
#'     Ciliates = list(phylum = "Ciliophora")
#' )
#'
#' # Assign with custom groups
#' result_custom <- assign_phytoplankton_group(
#'   scientific_names = c("Teleaulax amphioxeia", "Mesodinium rubrum", "Dinophysis acuta"),
#'   aphia_ids = c(106306, 232069, 109604),
#'   custom_groups = custom_groups,         # Adding custom groups
#'   verbose = TRUE
#' )
#'
#' print(result_custom)
#' }
#'
#' @seealso \url{https://CRAN.R-project.org/package=worrms}
#'
#' @export
assign_phytoplankton_group <- function(scientific_names, aphia_ids = NULL,
                                       diatom_class = c("Bacillariophyceae", "Coscinodiscophyceae", "Mediophyceae", "Diatomophyceae"),
                                       dinoflagellate_class = "Dinophyceae", cyanobacteria_class = "Cyanophyceae",
                                       cyanobacteria_phylum = "Cyanobacteria", match_first_word = TRUE,
                                       marine_only = FALSE, return_class = FALSE, custom_groups = list(),
                                       verbose = TRUE) {
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

  # Filter rows that have aphia_id
  unique_data <- unique_data %>%
    group_by(scientific_name) %>%
    filter(!is.na(aphia_id) | n() == 1) %>%
    ungroup()

  # Extract unique non-NA AphiaIDs
  valid_aphia_ids <- unique_data$aphia_id[!is.na(unique_data$aphia_id)]

  # Retrieve WoRMS records based on AphiaID
  if (length(valid_aphia_ids) > 0) {
    if (verbose) cat("Retrieving", length(valid_aphia_ids), "WoRMS records from input 'aphia_ids'.\n")
    aphia_records <- get_worms_records(valid_aphia_ids, verbose = verbose) %>%
      mutate(class = as.character(ifelse(status == "no content", NA, class)),
             phylum = as.character(ifelse(status == "no content", NA, phylum)))
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

  # Merge records into input data
  matched_aphia_data <- unique_data %>%
    left_join(distinct(aphia_records), by = c("aphia_id" = "AphiaID")) %>%
    filter(!is.na(aphia_id) & !is.na(status))

  # Handle entries with missing AphiaIDs
  missing_aphia_data <- unique_data[is.na(unique_data$aphia_id) | unique_data$aphia_id %in% no_content$AphiaID,]

  if (nrow(missing_aphia_data) > 0) {
    if (verbose) cat("Retrieving", nrow(missing_aphia_data), "WoRMS records from input 'scientific_names'.\n")
    missing_aphia_records <- get_worms_records_name(missing_aphia_data$scientific_name,
                                                    marine_only = marine_only,
                                                    verbose = verbose) %>%
      mutate(class = as.character(ifelse(status == "no content", NA, class)),
             phylum = as.character(ifelse(status == "no content", NA, phylum)))
  } else {
    missing_aphia_records <- data.frame(name = NA_character_,
                                        AphiaID = NA_integer_,
                                        class = NA_character_,
                                        phylum = NA_character_,
                                        stringsAsFactors = FALSE)
  }

  # Merge records into input data
  matched_missing_data <- missing_aphia_data %>%
    left_join(missing_aphia_records, by = c("scientific_name" = "name")) %>%
    mutate(aphia_id = AphiaID)

  combined_data <- bind_rows(matched_aphia_data, matched_missing_data)

  # Handle still-missing entries
  unresolved_data <- combined_data %>%
    filter(is.na(aphia_id))

  if (match_first_word & nrow(unresolved_data) > 0) {
    first_words <- sub(" .*", "", unresolved_data$scientific_name)
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

  # Combine all classes
  classes <- unique(combined_data$class)
  other_classes <- classes[!classes %in% c(diatom_class, dinoflagellate_class, cyanobacteria_class)]

  # Assign plankton groups
  class_mapping <- combined_data %>%
    select(aphia_id, scientific_name, phylum, class, order, family, genus) %>%
    mutate(plankton_group = case_when(
      class %in% diatom_class ~ "Diatoms",
      class %in% dinoflagellate_class ~ "Dinoflagellates",
      class %in% cyanobacteria_class ~ "Cyanobacteria",
      phylum %in% cyanobacteria_phylum ~ "Cyanobacteria",
      TRUE ~ NA_character_
    ))

  # Apply custom groups
  for (group_name in names(custom_groups)) {
    group_criteria <- custom_groups[[group_name]]
    if (is.null(group_criteria$phylum)) group_criteria$phylum <- character(0)
    if (is.null(group_criteria$class)) group_criteria$class <- character(0)
    if (is.null(group_criteria$order)) group_criteria$order <- character(0)
    if (is.null(group_criteria$family)) group_criteria$family <- character(0)
    if (is.null(group_criteria$genus)) group_criteria$genus <- character(0)
    if (is.null(group_criteria$scientific_name)) group_criteria$scientific_name <- character(0)

    class_mapping <- class_mapping %>%
      mutate(plankton_group = case_when(
        phylum %in% group_criteria$phylum |
          class %in% group_criteria$class |
          order %in% group_criteria$order |
          family %in% group_criteria$family |  # New condition
          genus %in% group_criteria$genus |
          scientific_name %in% group_criteria$scientific_name ~ group_name,
        TRUE ~ plankton_group
      ))
  }

  # Handle unknown classifications
  class_mapping <- class_mapping %>%
    mutate(plankton_group = ifelse(is.na(plankton_group), "Other", plankton_group))

  if (return_class) {
    class_mapping <- class_mapping %>%
      select(scientific_name, class, plankton_group) %>%
      distinct()
  } else {
    class_mapping <- class_mapping %>%
      select(scientific_name, plankton_group) %>%
      distinct()
  }

  # Finalize output
  final_output <- input_data %>%
    left_join(class_mapping, by = "scientific_name") %>%
    select(-aphia_id)

  return(final_output)
}
