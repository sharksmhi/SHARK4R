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

#' Get children hierarchies for specified taxon IDs from SLU Artdatabanken API (Dyntaxa)
#'
#' This function queries the SLU Artdatabanken API (Dyntaxa) to retrieve children taxon hierarchy information for the specified taxon IDs.
#' It constructs a request with the provided taxon IDs, sends the request to the SLU Artdatabanken API, and
#' processes the response to return a data frame of taxon children.
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
#' @param taxon_ids A vector of numeric taxon IDs for which children taxon IDs are requested.
#' @param subscription_key A character string containing the subscription key for accessing the SLU Artdatabanken API. A key is provided for registered users at [Artdatabanken](https://api-portal.artdatabanken.se/).
#' @param levels Integer. Default is 1
#' @param main_children Logical. Default is TRUE.
#' 
#' @return A data frame containing children taxon information corresponding to the specified taxon IDs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get children taxon hierarchy for taxon IDs 1010608 and 5000062
#' children_hierarchy <- get_dyntaxa_children_hierarchy(c(1010608, 5000062), "your_subscription_key")
#' print(children_hierarchy)
#' }
#'
#' @seealso [SLU Artdatabanken API Documentation](https://api-portal.artdatabanken.se/)
#'
get_dyntaxa_children_hierarchy <- function(taxon_ids, subscription_key, levels = 1, main_children = TRUE) {
  if (length(taxon_ids) == 0) {
    stop("taxon_ids should not be empty.")
  }
  
  if (any(is.na(taxon_ids))) {
    stop("taxon_ids should not contain NA.")
  }
  
  url <- paste0("https://api.artdatabanken.se/taxonservice/v1/taxa/", taxon_ids, "/childhierarchy?levels=", levels, "&onlyMainChildren=", main_children)
  
  headers <- c(
    'Cache-Control' = 'no-cache',
    'Ocp-Apim-Subscription-Key' = subscription_key
  )
  
  # Set up the progress bar
  pb <- txtProgressBar(min = 0, max = length(taxon_ids), style = 3)
  
  # Perform GET requests and check status
  responses <- lapply(seq_along(url), function(i) {
    setTxtProgressBar(pb, i)
    res <- GET(url[i], add_headers(headers))
    
    # Check if the response is successful
    if (http_status(res)$category == "Success") {
      return(res)
    } else {
      stop(paste("Error:", http_status(res)$reason))
    }
  })
  
  # Extract and parse JSON content from each successful response
  results <- map(responses, function(response) {
    # Extract and parse the JSON content
    json_content <- content(response, "text", encoding = "UTF-8")
    parsed_data <- fromJSON(json_content, flatten = TRUE)
    as_tibble(parsed_data)
  })
  
  # Combine all parsed tibbles into one
  results <- bind_rows(results)
  
  return(results)
}

#' Get children taxon IDs for specified taxon IDs from SLU Artdatabanken API (Dyntaxa)
#'
#' This function queries the SLU Artdatabanken API (Dyntaxa) to retrieve children taxon IDs for the specified taxon IDs.
#' It constructs a request with the provided taxon IDs, sends the request to the SLU Artdatabanken API, and
#' processes the response to return a list of children taxon IDs.
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
#' @param taxon_ids A vector of numeric taxon IDs for which children taxon IDs are requested.
#' @param subscription_key A character string containing the subscription key for accessing the SLU Artdatabanken API. A key is provided for registered users at [Artdatabanken](https://api-portal.artdatabanken.se/).
#' @param main_children Logical. Default is TRUE.
#' @param verbose Logical. Default is TRUE.
#'
#' @return A list containing children taxon IDs corresponding to the specified taxon IDs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get children taxon IDs for taxon IDs 1010608 and 5000062
#' children_ids <- get_dyntaxa_children_ids(c(1010608, 5000062), "your_subscription_key")
#' print(children_ids)
#' }
#'
#' @seealso [SLU Artdatabanken API Documentation](https://api-portal.artdatabanken.se/)
#'
get_dyntaxa_children_ids <- function(taxon_ids, subscription_key, main_children = TRUE, verbose = TRUE) {
  if (length(taxon_ids) == 0) {
    stop("taxon_ids should not be empty.")
  }
  
  if (any(is.na(taxon_ids))) {
    stop("taxon_ids should not contain NA.")
  }
  
  url <- paste0("https://api.artdatabanken.se/taxonservice/v1/taxa/", taxon_ids, "/childids?useMainChildren=", main_children)
  
  headers <- c(
    'Cache-Control' = 'no-cache',
    'Ocp-Apim-Subscription-Key' = subscription_key
  )
  
  # Set up the progress bar
  if (verbose) {pb <- txtProgressBar(min = 0, max = length(taxon_ids), style = 3)}
  
  responses <- lapply(seq_along(url), function(i) {
    if (verbose) {setTxtProgressBar(pb, i)}
    return(GET(url[i], add_headers(headers)))
  })
  
  # Close the progress bar
  if (verbose) {
    close(pb)
  }
  
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
#' @param recommended_only Logical. If TRUE, the function will return only recommended (accepted) names. If FALSE, all names are returned. Default is TRUE.
#' @param add_genus_children Logical. If TRUE, the output will include children from all valid genera taxon_ids present in `parent_ids`. Default is FALSE.
#' 
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
construct_dyntaxa_table <- function(parent_ids, subscription_key, shark_output = TRUE, recommended_only = TRUE, add_genus_children = FALSE) {
  if (!is.list(parent_ids)) {
    parent_ids <- list(parent_ids)
  }
  
  if (any(is.na(unlist(parent_ids)))) {
    stop("parent_ids should not contain NA.")
  }
  
  taxa <- data.frame()
  
  # Set up progress bar
  pb <- txtProgressBar(min = 0, max = length(parent_ids), style = 3)
  
  # Initialize counters
  if_counter <- 0
  else_counter <- 0
  
  for (i in seq_along(parent_ids)) {
    ids <- parent_ids[[i]]
    single <- unique(ids)
    
    taxa_i <- data.frame()
    
    for (id in 1:length(single)) {
      if (single[id] %in% taxa$taxon_id) {
        if_counter <- if_counter + 1
        
        selected <- taxa %>%
          filter(taxon_id == single[id])
        taxon_id <- selected$taxon_id
        parent_id <- selected$parent_id
        parent_name <- selected$parent_name
        name <- selected$name
        name_recommended <- selected$name_recommended
        recommended <- selected$recommended
        usage_value <- selected$usage_value
        taxon_id_recommended <- selected$taxon_id_recommended
        rank <- selected$rank
        hierarchy <- selected$hierarchy
        # guid <- selected$guid
        author <- selected$author
      } else {
        else_counter <- else_counter + 1
        
        taxa_ix <- get_dyntaxa_records(single[id], subscription_key)
        parent_id <- taxa_ix$parentId
        rank <- taxa_ix$category.value
        parent_name <- NA
        
        if (!shark_output) {
          taxon_id_recommended <- taxa_ix$taxonId
          name_recommended <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci" & isRecommended == TRUE) %>%
            slice(1) %>%
            pull(name)
          
          taxon_id <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(id)
          name <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(name)
          author <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(author)
          recommended <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(isRecommended)
          usage_value <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(usage.name)
          hierarchy <- if (length(taxa_i) > 0) {
            paste(paste(taxa_i$name[taxa_i$recommended], collapse = " - "), name_recommended, sep = " - ")
          } else {
            taxa_ix$names %>%
              map_df(as.data.frame) %>%
              filter(nameShort == "sci" & isRecommended == TRUE) %>%
              pull(name) %>%
              paste(collapse = " - ")
          }
        } else {
          taxon_id <- taxa_ix$taxonId
          taxon_id_recommended <- taxon_id
          
          name <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci" & isRecommended == TRUE) %>%
            slice(1) %>%
            pull(name)
          
          name_recommended <- name
          
          parent_name <- NA
          
          author <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci" & isRecommended == TRUE) %>%
            slice(1) %>%
            pull(author)
          
          hierarchy <- ifelse(
            length(taxa_i) > 0,
            paste(paste(taxa_i$name, collapse = " - "), name, sep = " - "),
            paste0(taxa_ix$names %>%
                     map_df(as.data.frame) %>%
                     filter(nameShort == "sci" & isRecommended == TRUE) %>%
                     slice(1) %>%
                     pull(name))) 
          usage_value <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci" & isRecommended == TRUE) %>%
            slice(1) %>%
            pull(usage.value)
          recommended <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci" & isRecommended == TRUE) %>%
            slice(1) %>%
            pull(isRecommended)
          author <- ifelse(length(author) == 0, NA, author)
        }
      }
      taxa_temp <- data.frame(taxon_id, parent_id, parent_name, name, rank, author, hierarchy, recommended, usage_value, taxon_id_recommended, name_recommended) %>%
        mutate(taxon_id = ifelse(recommended, taxon_id_recommended, taxon_id)) %>%
        mutate(taxonId = ifelse(recommended, paste0("urn:lsid:dyntaxa.se:Taxon:", taxon_id),  paste0("urn:lsid:dyntaxa.se:TaxonName:", taxon_id))) %>%
        mutate(taxonId_recommended = paste0("urn:lsid:dyntaxa.se:Taxon:", taxon_id_recommended)) 
      
      taxa_i <- bind_rows(
        taxa_i,taxa_temp) %>%
        distinct()
    }
    if (add_genus_children) {
      genus <- taxa_i %>%
        filter(rank == "Genus" & recommended)
      
      if (nrow(genus > 0)) {
        children_ids <- get_dyntaxa_children_ids(genus$taxon_id, subscription_key, verbose = FALSE)
        
        children_records <- get_dyntaxa_records(unlist(children_ids), subscription_key)
        
        children_additions <- data.frame(
          taxon_id = integer(0),
          parent_id = integer(0),
          parent_name = character(0),
          name = character(0),
          rank = character(0),
          author = character(0),
          hierarchy = character(0),
          recommended = logical(0),
          usage_value = character(0),
          taxon_id_recommended = integer(0),
          name_recommended = character(0),
          taxonId = character(0),
          taxonId_recommended = character(0)
        )
        
        for (j in 1:nrow(children_records)) {

          children_record <- children_records %>%
            filter(taxonId == children_records$taxonId[j])
          
          if (nrow(children_record) == 0 | !children_record$status.value == "Accepted") {
            next
          }
          
          taxon_id_recommended <- children_record$taxonId
          rank <- children_record$category.value
          parent <- children_record$parentId
          
          taxon_id <- children_record$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(id)
          
          recommended <- children_record$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(isRecommended)
          
          name_recommended <- children_record$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci" & isRecommended == TRUE) %>%
            pull(name)
          
          name <- children_record$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(name)
          
          usage_value <- children_record$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(usage.name)
          
          author <- children_record$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(author)
          
          if (rank == "Hybrid") {
            next
          }
          
          if (rank %in% c("Genus", "Species", "SpeciesComplex", "CollectiveTaxon")) {
            hierarchy <- paste(genus$hierarchy, name_recommended, sep = " - ")
            parent_name <- NA
            
            if (rank == "CollectiveTaxon") {
              parent_name <- name_recommended
            }
            
          } else {
            parent_info <- children_additions %>%
              filter(taxon_id_recommended == parent & recommended)
            
            parent_name <- unique(parent_info$name)
            
            if (length(parent_name) == 0) {
              records <- get_dyntaxa_records(parent, subscription_key)
              
              parent_name<-records$names %>%
                map_df(as.data.frame) %>%
                filter(nameShort == "sci" & isRecommended) %>%
                pull(name)
            }
            
            hierarchy <- paste(genus$hierarchy, parent_name, name_recommended, sep = " - ")
          }
          
          taxa_recommended <- data.frame(taxon_id_recommended, parent_id = parent, parent_name, taxon_id, recommended, usage_value, name, author, name_recommended, rank, hierarchy) %>%
            mutate(taxonId = ifelse(recommended, paste0("urn:lsid:dyntaxa.se:Taxon:", taxon_id_recommended),  paste0("urn:lsid:dyntaxa.se:TaxonName:", taxon_id))) %>%
            mutate(taxonId_recommended = paste0("urn:lsid:dyntaxa.se:Taxon:", taxon_id_recommended))
          
          children_additions <- bind_rows(children_additions, taxa_recommended) %>%
            filter(!rank == "Genus") %>%
            filter(!taxonId %in% taxa_i$taxonId)
        }
        
        taxa_i <- bind_rows(taxa_i, children_additions)
      }
    }
    
    taxa_i <- taxa_i %>%
      pivot_wider(names_from = rank, values_from = name_recommended) %>%
      left_join(., taxa_i, by = c("taxon_id", "name", "parent_id", "parent_name", "hierarchy", "author", "recommended", "usage_value", "taxonId", "taxonId_recommended", "taxon_id_recommended"))
    
    if ("Species" %in% colnames(taxa_i)) {
      taxa_i <- taxa_i %>%
        mutate(Species = ifelse(!is.na(parent_name), parent_name, Species))
    }

    shark_taxonomy <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species") 
    
    taxa_i <- taxa_i %>%
      mutate(across(all_of(shark_taxonomy[shark_taxonomy %in% taxa_i$rank]), fill_na_below_first_non_na))
    
    taxa <- bind_rows(taxa, taxa_i) 

    # Update progress bar at the end of each iteration
    setTxtProgressBar(pb, i)
  }
  
  # Close progress bar
  close(pb)
  
  if (recommended_only) {
    taxa <- taxa %>%
      filter(recommended)
  }
  
  if (shark_output) {
    taxa_filtered <- taxa %>%
      filter(recommended) %>%
      filter(rank %in% shark_taxonomy) %>%
      select(taxon_id, name, rank, author, any_of(shark_taxonomy), hierarchy) %>%
      distinct()
  } else {
    taxa_filtered <- taxa %>%
      mutate(parentNameUsageID =  paste0("urn:lsid:dyntaxa.se:Taxon:", parent_id),
             nomenclaturalStatus = NA,
             taxonRemarks = NA) %>%
      rename(acceptedNameUsageID = taxonId_recommended,
             scientificName = name,
             taxonRank = rank,
             scientificNameAuthorship = author,
             taxonomicStatus = usage_value) %>%
      rename_with(tolower, any_of(shark_taxonomy)) %>% 
      select(taxonId, acceptedNameUsageID, parentNameUsageID, scientificName, taxonRank, scientificNameAuthorship, taxonomicStatus, nomenclaturalStatus, taxonRemarks, any_of(tolower(shark_taxonomy)), hierarchy) %>%
      distinct()
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
  # Find the index of the first non-NA value
  first_non_na_index <- which(!is.na(x))[1]
  
  # If there's a non-NA value, fill NA values below it
  if (!is.na(first_non_na_index)) {
    non_na_value <- x[first_non_na_index]
    
    # Only replace NA values below the first non-NA value
    x[is.na(x) & seq_along(x) > first_non_na_index] <- non_na_value
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
                          page = 1, pageSize = 100, verbose = TRUE) {
  
  # Make sure there are no NA
  taxon_names <- taxon_names[!is.na(taxon_names)]
  
  # Regular expression to allow alphanumeric characters, spaces, and accented characters
  invalid_names <- taxon_names[grepl("[^a-zA-Z0-9 ./()'\\-]", taxon_names, useBytes = TRUE)]
  
  # Check if there are any invalid names and print them with a warning
  if (length(invalid_names) > 0) {
    warning("Some taxon names contain special characters, which may cause API issues: ", 
            paste(invalid_names, collapse = ", "))
  }
  
  url <- "https://api.artdatabanken.se/taxonservice/v1/taxa/names"
  headers <- c(
    'Cache-Control' = 'no-cache',
    'Ocp-Apim-Subscription-Key' = subscription_key
  )
  
  # Initialize progress bar if verbose is TRUE
  if (verbose) {
    pb <- txtProgressBar(min = 0, max = length(taxon_names), style = 3)
  }
  
  # Loop over taxon_names and collect results
  result_list <- map(seq_along(taxon_names), ~{
    taxon_name <- taxon_names[.x]
    query <- list(
      searchString = taxon_name,
      searchFields = searchFields,
      isRecommended = isRecommended,
      isOkForObservationSystems = isOkForObservationSystems,
      culture = culture,
      page = page,
      pageSize = pageSize
    )
    
    response <- GET(url, query = query, add_headers(.headers = headers))
    
    # Update the progress bar
    if (verbose) {
      setTxtProgressBar(pb, .x)
    }
    
    result <- list(
      taxon_name = taxon_name,
      statusCode = status_code(response),
      responseBody = fromJSON(content(response, "text"))
    )
    
    result$responseBody$data <- result$responseBody$data %>%
      filter(status$value == "Accepted")
    
    if (length(result$responseBody$data) > 0) {
      result$responseBody$data <- result$responseBody$data %>%
        filter(status$value == "Accepted")
    }
    
    # Process response and extract relevant data
    if (length(result$responseBody$data) > 0) {
      if (multiple_options) {
        name <- result$responseBody$data$name[tolower(result$responseBody$data$name) == tolower(result$taxon_name)]
        taxon_id <- result$responseBody$data$taxonInformation$taxonId[tolower(result$responseBody$data$name) == tolower(result$taxon_name)]
        author <- result$responseBody$data$author[tolower(result$responseBody$data$name) == tolower(result$taxon_name)]
        valid_name <- result$responseBody$data$taxonInformation$recommendedScientificName[tolower(result$responseBody$data$name) == tolower(result$taxon_name)]
        
        # Set any empty variables to NA
        if (length(name) == 0) name <- NA
        if (length(taxon_id) == 0) taxon_id <- NA
        if (length(author) == 0) author <- NA
        if (length(valid_name) == 0) valid_name <- NA
        
        return(data.frame(search_pattern = result$taxon_name, taxon_id = taxon_id, best_match = name, author = author, valid_name = valid_name))
      } else {
        taxon_id <- result$responseBody$data$taxonInformation$taxonId[1]
        name <- result$responseBody$data$name[1]
        author <- result$responseBody$data$author[1]
        valid_name <- result$responseBody$data$taxonInformation$recommendedScientificName[1]
        return(data.frame(search_pattern = result$taxon_name, taxon_id = taxon_id, best_match = name, author = author, valid_name = valid_name))
      }
    } else {
      return(data.frame(search_pattern = result$taxon_name, taxon_id = NA, best_match = NA, author = NA, valid_name = NA))
    }
  })
  
  # Close the progress bar
  if (verbose) {
    close(pb)
  }
  
  result_df <- do.call(rbind, result_list) %>%
    distinct()
  return(result_df)
}