#' Get taxonomic information from SLU Artdatabanken API for specified taxon IDs
#'
#' This function queries the SLU Artdatabanken API (Dyntaxa) to retrieve taxonomic information for the specified taxon IDs.
#' It constructs a request with the provided taxon IDs, sends the request to the SLU Artdatabanken API, and
#' processes the response to return taxonomic information in a data frame.
#' 
#' Please read the [conditions](https://www.artdatabanken.se/tjanster-och-miljodata/oppna-data-och-apier/api-villkor/) and [register](https://api-portal.artdatabanken.se/) before using the API.
#' 
#' Data collected through the API is stored at SLU Artdatabanken.
#' 
#' Genom att anvanda denna applikation atar jag mig att folja regler for 
#' anvandning av SLU Artdatabankens information, inklusive att respektera tredje 
#' mans upphovsratt. Detta atagande gors aven direkt mot SLU. Vid brott mot 
#' detta kan jag forlora ratten att anvanda applikationen
#'
#' @param taxon_ids A vector of numeric taxon IDs (Dyntaxa ID) for which taxonomic information is requested.
#' @param subscription_key A character string containing the subscription key for accessing the SLU Artdatabanken API. A key is provided for registered users at [Artdatabanken](https://api-portal.artdatabanken.se/).
#'
#' @return A data frame containing taxonomic information for the specified taxon IDs.
#'   Columns include taxonId, names, category, rank, isRecommended, and parentTaxonId.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get taxonomic information for taxon IDs 238366 and 1010380
#' taxon_info <- get_dyntaxa_records(c(238366, 1010380), "your_subscription_key")
#' print(taxon_info)
#' }
#'
#' @seealso [SLU Artdatabanken API Documentation](https://api-portal.artdatabanken.se/)
#'
get_dyntaxa_records <- function(taxon_ids, subscription_key) {
  if (length(taxon_ids) == 0) {
    stop("taxon_ids should not be empty.")
  }
  
  if (any(is.na(taxon_ids))) {
    stop("taxon_ids should not contain NA.")
  }
  
  url <- "https://api.artdatabanken.se/taxonservice/v1/taxa?culture=sv_SE"
  
  headers <- c(
    'Content-Type' = 'application/json-patch+json',
    'Cache-Control' = 'no-cache',
    'Ocp-Apim-Subscription-Key' = subscription_key
  )
  
  # Request body
  data <- list(
    taxonIds = as.list(taxon_ids)
  )
  
  body <- toJSON(data, auto_unbox = TRUE)
  
  response <- POST(url, add_headers(.headers=headers), body = body)
  
  # Check if the request was successful (status code 200)
  if (status_code(response) == 200) {
    # Convert JSON content to a data frame
    df <- fromJSON(content(response, "text"), flatten = TRUE)
    return(df)
  } else {
    # If the request was not successful, return an error message
    return(paste("Error: ", status_code(response), " - ", content(response, "text")))
  }
}

#' Get parent taxon IDs for specified taxon IDs from SLU Artdatabanken API (Dyntaxa)
#'
#' This function queries the SLU Artdatabanken API (Dyntaxa) to retrieve parent taxon IDs for the specified taxon IDs.
#' It constructs a request with the provided taxon IDs, sends the request to the SLU Artdatabanken API, and
#' processes the response to return a list of parent taxon IDs.
#'
#' Please read the [conditions](https://www.artdatabanken.se/tjanster-och-miljodata/oppna-data-och-apier/api-villkor/) and [register](https://api-portal.artdatabanken.se/) before using the API.
#' 
#' Data collected through the API is stored at SLU Artdatabanken.
#' 
#' Genom att anvanda denna applikation atar jag mig att folja regler for 
#' anvandning av SLU Artdatabankens information, inklusive att respektera tredje 
#' mans upphovsratt. Detta atagande gors aven direkt mot SLU. Vid brott mot 
#' detta kan jag forlora ratten att anvanda applikationen
#'
#' @param taxon_ids A vector of numeric taxon IDs for which parent taxon IDs are requested.
#' @param subscription_key A character string containing the subscription key for accessing the SLU Artdatabanken API. A key is provided for registered users at [Artdatabanken](https://api-portal.artdatabanken.se/).
#'
#' @return A list containing parent taxon IDs corresponding to the specified taxon IDs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get parent taxon IDs for taxon IDs 238366 and 1010380
#' parent_ids <- get_dyntaxa_parent_ids(c(238366, 1010380), "your_subscription_key")
#' print(parent_ids)
#' }
#'
#' @seealso [SLU Artdatabanken API Documentation](https://api-portal.artdatabanken.se/)
#'
get_dyntaxa_parent_ids <- function(taxon_ids, subscription_key) {
  if (length(taxon_ids) == 0) {
    stop("taxon_ids should not be empty.")
  }
  
  if (any(is.na(taxon_ids))) {
    stop("taxon_ids should not contain NA.")
  }
  
  url <- paste0("https://api.artdatabanken.se/taxonservice/v1/taxa/", taxon_ids, "/parentids")
  
  headers <- c(
    'Cache-Control' = 'no-cache',
    'Ocp-Apim-Subscription-Key' = subscription_key
  )
  
  # Set up the progress bar
  pb <- txtProgressBar(min = 0, max = length(taxon_ids), style = 3)
  
  responses <- lapply(seq_along(url), function(i) {
    setTxtProgressBar(pb, i)
    return(GET(url[i], add_headers(headers)))
  })
  
  results <- lapply(responses, function(response) {
    if (http_status(response)$category == "Success") {
      result <- content(response, "text")
      parsed_result <- c(fromJSON(result)$taxonIds)
      parsed_result <- parsed_result[parsed_result != 0]  # Remove root
      return(parsed_result)
    } else {
      stop(paste("Error:", http_status(response)$reason))
    }
  })
  
  results <- Map(function(vec, val) c(vec, val), results, taxon_ids)
  
  return(results)
}

#' Construct Dyntaxa taxonomy table from API
#'
#' This function constructs a taxonomy table based on Dyntaxa parent taxon IDs.
#' It queries the SLU Artdatabanken API (Dyntaxa) to fetch taxonomy information and organizes the data into a hierarchical table.
#'
#' Please read the [conditions](https://www.artdatabanken.se/tjanster-och-miljodata/oppna-data-och-apier/api-villkor/) and [register](https://api-portal.artdatabanken.se/) before using the API.
#' 
#' Data collected through the API is stored at SLU Artdatabanken.
#' 
#' Genom att anvanda denna applikation atar jag mig att folja regler for 
#' anvandning av SLU Artdatabankens information, inklusive att respektera tredje 
#' mans upphovsratt. Detta atagande gors aven direkt mot SLU. Vid brott mot 
#' detta kan jag forlora ratten att anvanda applikationen
#'
#' @param parent_ids A list containing parent taxon IDs for which taxonomy information is requested.
#' @param subscription_key A character string containing the subscription key for accessing the SLU Artdatabanken API. A key is provided for registered users at [Artdatabanken](https://api-portal.artdatabanken.se/).
#' @param shark_output Logical. If TRUE, the function will return selected column headers that match SHARK output. If FALSE, all columns are returned. Default is TRUE.
#' @return A data frame representing the constructed taxonomy table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Construct Dyntaxa taxonomy table for taxon IDs 238366 and 1010380
#' parent_ids <- get_dyntaxa_parent_ids(c(238366, 1010380), "your_subscription_key")
#' taxonomy_table <- construct_dyntaxa_table(parent_ids, "your_subscription_key")
#' print(taxonomy_table)
#' }
#'
#' @seealso [SLU Artdatabanken API Documentation](https://api-portal.artdatabanken.se/)
#'
construct_dyntaxa_table <- function(parent_ids, subscription_key, shark_output = TRUE) {
  if (!is.list(parent_ids)) {
    parent_ids <- list(parent_ids)
  }
  
  if (any(is.na(unlist(parent_ids)))) {
    stop("parent_ids should not contain NA.")
  }
  
  taxa <- data.frame()
  
  # Set up progress bar
  pb <- txtProgressBar(min = 0, max = length(parent_ids), style = 3)
  
  # Initialize counters for debugging
  if_counter <- 0
  else_counter <- 0
  
  for (i in seq_along(parent_ids)) {
    ids <- parent_ids[[i]]
    single <- unique(ids)
    
    taxa_i <- data.frame()
    
    for (id in 1:length(single)) {
      if (single[id] %in% taxa$taxon_id) {
        if_counter <- if_counter + 1 # For debugging
        
        selected <- taxa %>%
          filter(taxon_id == single[id])
        taxon_id <- selected$taxon_id
        parent_id <- selected$parent_id
        name <- selected$name
        rank <- selected$rank
        hierarchy <- selected$hierarchy
        guid <- selected$guid
        author <- selected$author
      } else {
        else_counter <- else_counter + 1 # For debugging
        
        taxa_ix <- get_dyntaxa_records(single[id], subscription_key)
        taxon_id <- taxa_ix$taxonId
        parent_id <- taxa_ix$parentId
        name <- taxa_ix$names %>%
          map_df(as.data.frame) %>%
          filter(nameShort == "sci" & isRecommended == TRUE) %>%
          slice(1) %>%
          pull(name)
        author <- taxa_ix$names %>%
          map_df(as.data.frame) %>%
          filter(nameShort == "sci" & isRecommended == TRUE) %>%
          slice(1) %>%
          pull(author)
        rank <- taxa_ix$category.value
        hierarchy <- ifelse(
          length(taxa_i) > 0,
          paste(paste(taxa_i$name, collapse = " - "), name, sep = " - "),
          paste0(taxa_ix$names %>%
                   map_df(as.data.frame) %>%
                   filter(nameShort == "sci" & isRecommended == TRUE) %>%
                   slice(1) %>%
                   pull(name))) 
        guid <- taxa_ix$names %>%
          map_df(as.data.frame) %>%
          filter(nameShort == "Guid" & isRecommended == TRUE) %>%
          slice(1) %>%
          pull(name)
        guid <- ifelse(length(guid) == 0, NA, guid)
        author <- ifelse(length(author) == 0, NA, author)
      }
      
      taxa_i <- bind_rows(
        taxa_i,
        data.frame(taxon_id, parent_id, name, rank, author, hierarchy, guid)
      )
    }
    
    taxa_i <- taxa_i %>%
      distinct() %>%
      pivot_wider(names_from = rank, values_from = name) %>%
      left_join(., taxa_i, by = c("taxon_id", "parent_id", "hierarchy", "guid", "author"))
    
    shark_taxonomy <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
    taxa_i <- taxa_i %>%
      mutate(across(all_of(shark_taxonomy[shark_taxonomy %in% taxa_i$rank]), fill_na_below_first_non_na))
    
    taxa <- bind_rows(taxa, taxa_i) %>%
      distinct()
    
    # Update progress bar at the end of each iteration
    setTxtProgressBar(pb, i)
  }
  
  # Close progress bar
  close(pb)
  
  taxa_filtered <- taxa %>%
    select(taxon_id, parent_id, name, rank, author, any_of(shark_taxonomy), hierarchy, guid) %>%
    distinct()
  
  if (shark_output) {
    taxa_filtered <- taxa_filtered %>%
      filter(rank %in% shark_taxonomy) %>%
      select(-parent_id)
  }
  
  # Print the counters, for debugging
  cat("Cached taxa requests:", if_counter, "\n")
  cat("Unique taxa requests:", else_counter, "\n")
  
  return(taxa_filtered)
}

#' Function to fill NA values below the first non-NA value in a vector
#'
#' This internal function fills NA values below the first non-NA value in a vector.
#'
#' @param x A vector.
#' @return A vector with NAs filled below the first non-NA value.
#'
#' @keywords internal
fill_na_below_first_non_na <- function(x) {
  non_na_values <- x[!is.na(x)]
  if (length(non_na_values) > 0) {
    first_non_na_index <- which(!is.na(x))[1]
    x[first_non_na_index:length(x)] <- non_na_values[1]
  }
  return(x)
}

#' Update taxonomy from SHARKdata datasets via SLU Artdatabanken API (Dyntaxa)
#'
#' This function updates Dyntaxa taxonomy records based on a list of Dyntaxa taxon IDs.
#' It collects parent IDs from SLU Artdatabanken API (Dyntaxa), retrieves full taxonomy records, and organizes 
#' the data into a full taxonomic table that can be joined with data downloaded from [SHARKdata](https://sharkdata.smhi.se/)
#'
#' Please read the [conditions](https://www.artdatabanken.se/tjanster-och-miljodata/oppna-data-och-apier/api-villkor/) and [register](https://api-portal.artdatabanken.se/) before using the API.
#' 
#' Data collected through the API is stored at SLU Artdatabanken.
#' 
#' Genom att anvanda denna applikation atar jag mig att folja regler for 
#' anvandning av SLU Artdatabankens information, inklusive att respektera tredje 
#' mans upphovsratt. Detta atagande gors aven direkt mot SLU. Vid brott mot 
#' detta kan jag forlora ratten att anvanda applikationen
#' 
#' @param dyntaxa_ids A vector of Dyntaxa taxon IDs to update.
#' @param subscription_key A character string containing the subscription key for accessing the SLU Artdatabanken API for Dyntaxa. A key is provided for registered users at [SLU Artdatabanken](https://api-portal.artdatabanken.se/).
#'
#' @return A data frame representing the updated Dyntaxa taxonomy table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Update Dyntaxa taxonomy for taxon IDs 238366 and 1010380
#' updated_taxonomy <- update_dyntaxa_taxonomy(c(238366, 1010380), "your_subscription_key")
#' print(updated_taxonomy)
#' }
#'
#' @seealso \code{\link{download_sharkdata}}, \code{\link{update_worms_taxonomy}}, [SLU Artdatabanken API Documentation](https://api-portal.artdatabanken.se/)
#'
update_dyntaxa_taxonomy <- function(dyntaxa_ids, subscription_key) {
  cat("Collecting parent IDs from Dyntaxa\n")
  parents_ids <- get_dyntaxa_parent_ids(dyntaxa_ids, subscription_key)
  cat("\nCollecting full taxonomy records from Dyntaxa\n")
  tax_table <- construct_dyntaxa_table(parents_ids, subscription_key)
  tax_table_shark <- tax_table %>%
    select(-rank, -author, -guid) %>%
    rename("dyntaxa_id" = taxon_id,
           "scientific_name" = name,
           "taxon_kingdom" = Kingdom,
           "taxon_phylum" = Phylum,
           "taxon_class" = Class,
           "taxon_order" = Order,
           "taxon_family" = Family,
           "taxon_genus" = Genus,
           "taxon_species" = Species,
           "taxon_hierarchy" = hierarchy)
  return(tax_table_shark)
}

#' Match Dyntaxa taxon names via API
#'
#' This function matches a list of taxon names against the SLU Artdatabanken API (Dyntaxa) and retrieves the best matches along with their taxon IDs.
#'
#' Please read the [conditions](https://www.artdatabanken.se/tjanster-och-miljodata/oppna-data-och-apier/api-villkor/) and [register](https://api-portal.artdatabanken.se/) before using the API.
#' 
#' Data collected through the API is stored at SLU Artdatabanken.
#' 
#' Genom att anvanda denna applikation atar jag mig att folja regler for 
#' anvandning av SLU Artdatabankens information, inklusive att respektera tredje 
#' mans upphovsratt. Detta atagande gors aven direkt mot SLU. Vid brott mot 
#' detta kan jag forlora ratten att anvanda applikationen
#'
#' @param taxon_names A vector of taxon names to match.
#' @param subscription_key A character string containing the subscription key for accessing the SLU Artdatabanken API. A key is provided for registered users at [Artdatabanken](https://api-portal.artdatabanken.se/).
#' @param multiple_options Logical. If TRUE, the function will return multiple matching names. Default is FALSE, selecting the first match.
#' @param searchFields A character string indicating the search fields. Defaults to 'Both'.
#' @param isRecommended A character string indicating whether the taxon is recommended. Defaults to 'NotSet'.
#' @param isOkForObservationSystems A character string indicating whether the taxon is suitable for observation systems. Defaults to 'NotSet'.
#' @param culture A character string indicating the culture. Defaults to 'sv_SE'.
#' @param page An integer specifying the page number for pagination. Defaults to 1.
#' @param pageSize An integer specifying the page size for pagination. Defaults to 100.
#'
#' @return A data frame containing the search pattern, taxon ID, and best match for each taxon name.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Match taxon names against SLU Artdatabanken API
#' matched_taxa <- match_taxon_name(c("Homo sapiens", "Canis lupus"), "your_subscription_key")
#' print(matched_taxa)
#' }
#'
#' @seealso [SLU Artdatabanken API Documentation](https://api-portal.artdatabanken.se/)
#'
match_taxon_name <- function(taxon_names, subscription_key, multiple_options = FALSE, searchFields = 'Both', isRecommended = 'NotSet', 
                          isOkForObservationSystems = 'NotSet', culture = 'sv_SE', 
                          page = 1, pageSize = 100) {
  
  # Make sure there are no NA
  taxon_names <- taxon_names[!is.na(taxon_names)]
  
  # Original list of taxon names for comparison
  original_taxon_names <- taxon_names
  
  # Regular expression to allow alphanumeric characters, spaces, and accented characters (å, ä, ö, ë, etc.)
  if (any(grepl("[^a-zA-Z0-9 åäöÅÄÖéèêëÉÈÊËùúûüÙÚÛÜçÇáàâñÑ./()'\\-]", taxon_names))) {
    warning("Some taxon names contain special characters, which may cause API issues.")
    
    # Remove special characters
    taxon_names <- gsub("[^a-zA-Z0-9 åäöÅÄÖéèêëÉÈÊËùúûüÙÚÛÜçÇáàâñÑ./()'\\-]", "", taxon_names)
    
    # Find and report modified names
    modified_names <- original_taxon_names[original_taxon_names != taxon_names]
    
    if (length(modified_names) > 0) {
      message("The following taxon names were modified due to special characters: ")
      print(modified_names)
    }
  }
  
  # Remove empty names
  taxon_names <- taxon_names[!taxon_names == ""]
  
  url <- "https://api.artdatabanken.se/taxonservice/v1/taxa/names"
  headers <- c(
    'Cache-Control' = 'no-cache',
    'Ocp-Apim-Subscription-Key' = subscription_key
  )
  
  result_list <- map(taxon_names, ~{
    query <- list(
      searchString = .x,
      searchFields = searchFields,
      isRecommended = isRecommended,
      isOkForObservationSystems = isOkForObservationSystems,
      culture = culture,
      page = page,
      pageSize = pageSize
    )
    
    response <- GET(url, query = query, add_headers(.headers = headers))
    
    result <- list(
      taxon_name = .x,
      statusCode = status_code(response),
      responseBody = fromJSON(content(response, "text"))
    )
    
    if (length(result$responseBody$data) > 0) {
      if (multiple_options) {
        name <- result$responseBody$data$name[result$responseBody$data$name == result$taxon_name]
        taxon_id <- result$responseBody$data$taxonInformation$taxonId[result$responseBody$data$name == result$taxon_name]
        author <- result$responseBody$data$author[result$responseBody$data$name == result$taxon_name]
        return(data.frame(search_pattern = result$taxon_name, taxon_id = taxon_id, best_match = name, author = author))
      } else {
        taxon_id <- result$responseBody$data$taxonInformation$taxonId[1]
        name <- result$responseBody$data$name[1]
        author <- result$responseBody$data$author[1]
        return(data.frame(search_pattern = result$taxon_name, taxon_id = taxon_id, best_match = name, author = author))
      }
    } else {
      return(data.frame(search_pattern = result$taxon_name, taxon_id = NA, best_match = NA, author = NA))
    }
  })
  
  result_df <- do.call(rbind, result_list)
  return(result_df)
}