#' Get taxonomic information from Dyntaxa for specified taxon IDs
#'
#' This function queries the SLU Artdatabanken API (Dyntaxa) to retrieve taxonomic information for the specified taxon IDs.
#' It constructs a request with the provided taxon IDs, sends the request to the SLU Artdatabanken API, and
#' processes the response to return taxonomic information in a data frame.
#'
#' @details
#' A valid Dyntaxa API subscription key is required.
#' You can request a free key for the "Taxonomy" service from the ArtDatabanken API portal:
#' <https://api-portal.artdatabanken.se/>
#'
#' **Note**: Please review the [API conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
#' and [register for access](https://api-portal.artdatabanken.se/) before using the API. Data collected through the API
#' is stored at SLU Artdatabanken. Please also note that the authors of `SHARK4R` are not affiliated with SLU Artdatabanken.
#'
#' @param taxon_ids A vector of numeric taxon IDs (Dyntaxa ID) for which taxonomic information is requested.
#' @param subscription_key A Dyntaxa API subscription key. By default, the key
#'   is read from the environment variable \code{DYNTAXA_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{get_dyntaxa_records(238366, subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(DYNTAXA_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{DYNTAXA_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#'
#' @return A data frame containing taxonomic information for the specified taxon IDs.
#'   Columns include `taxonId`, `names`, `category`, `rank`, `isRecommended`, and `parentTaxonId.`
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
#'
#' @seealso [SLU Artdatabanken API Documentation](https://api-portal.artdatabanken.se/)
#'
get_dyntaxa_records <- function(taxon_ids,
                                subscription_key = Sys.getenv("DYNTAXA_KEY")) {
  if (is.null(subscription_key) || subscription_key == "") {
    stop("No Dyntaxa subscription key provided. See ?get_dyntaxa_records for setup instructions.")
  }

  if (length(taxon_ids) == 0) {
    stop("taxon_ids should not be empty.")
  }

  if (any(taxon_ids > .Machine$integer.max, na.rm = TRUE)) {
    stop("One or more taxon_ids exceed the maximum integer value: ", .Machine$integer.max, ".")
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
#' Get parent taxon IDs for specified taxon IDs from Dyntaxa
#'
#' This function queries the SLU Artdatabanken API (Dyntaxa) to retrieve parent taxon IDs for the specified taxon IDs.
#' It constructs a request with the provided taxon IDs, sends the request to the SLU Artdatabanken API, and
#' processes the response to return a list of parent taxon IDs.
#'
#' @details
#' A valid Dyntaxa API subscription key is required.
#' You can request a free key for the "Taxonomy" service from the ArtDatabanken API portal:
#' <https://api-portal.artdatabanken.se/>
#'
#' **Note**: Please review the [API conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
#' and [register for access](https://api-portal.artdatabanken.se/) before using the API. Data collected through the API
#' is stored at SLU Artdatabanken. Please also note that the authors of `SHARK4R` are not affiliated with SLU Artdatabanken.
#'
#' @param taxon_ids A vector of numeric taxon IDs for which parent taxon IDs are requested.
#' @param subscription_key A Dyntaxa API subscription key. By default, the key
#'   is read from the environment variable \code{DYNTAXA_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{get_dyntaxa_parent_ids(238366, subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(DYNTAXA_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{DYNTAXA_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param verbose Logical. Default is TRUE.
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
#'
#' @seealso [SLU Artdatabanken API Documentation](https://api-portal.artdatabanken.se/)
#'
get_dyntaxa_parent_ids <- function(taxon_ids,
                                   subscription_key = Sys.getenv("DYNTAXA_KEY"),
                                   verbose = TRUE) {
  if (is.null(subscription_key) || subscription_key == "") {
    stop("No Dyntaxa subscription key provided. See ?get_dyntaxa_parent_ids for setup instructions.")
  }

  if (length(taxon_ids) == 0) {
    stop("taxon_ids should not be empty.")
  }

  if (any(taxon_ids > .Machine$integer.max, na.rm = TRUE)) {
    stop("One or more taxon_ids exceed the maximum integer value: ", .Machine$integer.max, ".")
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
  if (verbose) {pb <- utils::txtProgressBar(min = 0, max = length(taxon_ids), style = 3)}

  responses <- lapply(seq_along(url), function(i) {
    if (verbose) {utils::setTxtProgressBar(pb, i)}
    return(GET(url[i], add_headers(headers)))
  })

  results <- lapply(seq_along(responses), function(i) {
    response <- responses[[i]]
    if (http_status(response)$category == "Success") {
      result <- content(response, "text")
      parsed_result <- c(fromJSON(result)$taxonIds)
      parsed_result <- parsed_result[parsed_result != 0]  # Remove root
      return(parsed_result)
    } else {
      stop(paste0("Error in `get_dyntaxa_parent_ids` for taxon_id ", taxon_ids[i], ": ", http_status(response)$message))
    }
  })

  # Close the progress bar
  if (verbose) {
    close(pb)
  }

  results <- Map(function(vec, val) if (!is.null(vec)) c(vec, val) else vec, results, taxon_ids)

  return(results)
}

#' Get children hierarchies for specified taxon IDs from Dyntaxa
#'
#' This function queries the SLU Artdatabanken API (Dyntaxa) to retrieve children taxon hierarchy information for the specified taxon IDs.
#' It constructs a request with the provided taxon IDs, sends the request to the SLU Artdatabanken API, and
#' processes the response to return a data frame of taxon children.
#'
#' @details
#' A valid Dyntaxa API subscription key is required.
#' You can request a free key for the "Taxonomy" service from the ArtDatabanken API portal:
#' <https://api-portal.artdatabanken.se/>
#'
#' **Note**: Please review the [API conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
#' and [register for access](https://api-portal.artdatabanken.se/) before using the API. Data collected through the API
#' is stored at SLU Artdatabanken. Please also note that the authors of `SHARK4R` are not affiliated with SLU Artdatabanken.
#'
#' @param taxon_ids A vector of numeric taxon IDs for which children taxon IDs are requested.
#' @param subscription_key A Dyntaxa API subscription key. By default, the key
#'   is read from the environment variable \code{DYNTAXA_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{get_dyntaxa_children_hierarchy(1010608, subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(DYNTAXA_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{DYNTAXA_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param levels Integer. Default is 1
#' @param main_children Logical. Default is TRUE.
#' @param verbose Logical. Default is TRUE.
#'
#' @return A data frame containing children taxon information corresponding to the specified taxon IDs.
#'
#' @noRd
#'
#' @keywords internal
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
get_dyntaxa_children_hierarchy <- function(taxon_ids,
                                           subscription_key = Sys.getenv("DYNTAXA_KEY"),
                                           levels = 1,
                                           main_children = TRUE,
                                           verbose = TRUE) {
  if (is.null(subscription_key) || subscription_key == "") {
    stop("No Dyntaxa subscription key provided. See ?get_dyntaxa_children_hierarchy for setup instructions.")
  }

  if (length(taxon_ids) == 0) {
    stop("taxon_ids should not be empty.")
  }

  if (any(taxon_ids > .Machine$integer.max, na.rm = TRUE)) {
    stop("One or more taxon_ids exceed the maximum integer value: ", .Machine$integer.max, ".")
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
  if (verbose) {pb <- utils::txtProgressBar(min = 0, max = length(taxon_ids), style = 3)}

  # Perform GET requests and check status
  responses <- lapply(seq_along(url), function(i) {
    if (verbose) {utils::setTxtProgressBar(pb, i)}
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

  # Close the progress bar
  if (verbose) {
    close(pb)
  }

  # Combine all parsed tibbles into one
  results <- bind_rows(results)

  return(results)
}
#' Get children taxon IDs for specified taxon IDs from Dyntaxa
#'
#' This function queries the SLU Artdatabanken API (Dyntaxa) to retrieve children taxon IDs for the specified taxon IDs.
#' It constructs a request with the provided taxon IDs, sends the request to the SLU Artdatabanken API, and
#' processes the response to return a list of children taxon IDs.
#'
#' @details
#' A valid Dyntaxa API subscription key is required.
#' You can request a free key for the "Taxonomy" service from the ArtDatabanken API portal:
#' <https://api-portal.artdatabanken.se/>
#'
#' **Note**: Please review the [API conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
#' and [register for access](https://api-portal.artdatabanken.se/) before using the API. Data collected through the API
#' is stored at SLU Artdatabanken. Please also note that the authors of `SHARK4R` are not affiliated with SLU Artdatabanken.
#'
#' @param taxon_ids A vector of numeric taxon IDs for which children taxon IDs are requested.
#' @param subscription_key A Dyntaxa API subscription key. By default, the key
#'   is read from the environment variable \code{DYNTAXA_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{get_dyntaxa_children_ids(1010608, subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(DYNTAXA_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{DYNTAXA_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param main_children Logical. Default is TRUE.
#' @param verbose Logical. Default is TRUE.
#'
#' @return A list containing children taxon IDs corresponding to the specified taxon IDs.
#'
#' @noRd
#'
#' @keywords internal
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
get_dyntaxa_children_ids <- function(taxon_ids,
                                     subscription_key = Sys.getenv("DYNTAXA_KEY"),
                                     main_children = TRUE,
                                     verbose = TRUE) {
  if (is.null(subscription_key) || subscription_key == "") {
    stop("No Dyntaxa subscription key provided. See ?get_dyntaxa_children_ids for setup instructions.")
  }

  if (length(taxon_ids) == 0) {
    stop("taxon_ids should not be empty.")
  }

  if (any(taxon_ids > .Machine$integer.max, na.rm = TRUE)) {
    stop("One or more taxon_ids exceed the maximum integer value: ", .Machine$integer.max, ".")
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
  if (verbose) {pb <- utils::txtProgressBar(min = 0, max = length(taxon_ids), style = 3)}

  responses <- lapply(seq_along(url), function(i) {
    if (verbose) {utils::setTxtProgressBar(pb, i)}
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

#' Construct Dyntaxa taxonomy table from individually requests
#'
#' This internal function constructs a taxonomy table by individually querying the SLU Artdatabanken API (Dyntaxa)
#' using a list of parent taxon IDs. It fetches taxonomy information for the provided taxon IDs and
#' organizes them into a hierarchical structure. The function is capable of filtering based on
#' recommended (accepted) names, handling genus-related children, and formatting the results for SHARK output.
#'
#' @details
#' A valid Dyntaxa API subscription key is required.
#' You can request a free key for the "Taxonomy" service from the ArtDatabanken API portal:
#' <https://api-portal.artdatabanken.se/>
#'
#' **Note**: Please review the [API conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
#' and [register for access](https://api-portal.artdatabanken.se/) before using the API. Data collected through the API
#' is stored at SLU Artdatabanken. Please also note that the authors of `SHARK4R` are not affiliated with SLU Artdatabanken.
#'
#' @param parent_ids A list or vector of parent taxon IDs for which taxonomy information is requested. These IDs must be valid
#'                   according to the Dyntaxa API and can be a combination of single or multiple IDs.
#' @param subscription_key A Dyntaxa API subscription key. By default, the key
#'   is read from the environment variable \code{DYNTAXA_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{construct_dyntaxa_missing_table(list(c(5000055, 6011755)), subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(DYNTAXA_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{DYNTAXA_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param shark_output Logical. If TRUE, the function will return columns formatted to match the SHARK data submission format.
#'                     If FALSE, it will return a broader set of taxonomy information. Defaults to TRUE.
#' @param recommended_only Logical. If TRUE, only recommended (accepted) names will be returned. Defaults to TRUE.
#' @param add_genus_children Logical. If TRUE, the function will include all valid genus-level children for each parent taxon.
#'                            Defaults to FALSE.
#' @param drop_morphotypes Logical. If TRUE, taxa with the rank of "MorphoType" will be excluded from the output to simplify
#'                         the taxonomy structure. Defaults to TRUE.
#' @param add_hierarchy Logical. If TRUE, the hierarchical relationship of taxa (from parent to child) will be included in the output.
#'                      Defaults to FALSE.
#' @param verbose Logical. If TRUE, a progress bar will be displayed during execution. Defaults to TRUE.
#'
#' @return A data frame with taxonomy information, including taxon IDs, parent IDs, ranks, names, and other details.
#'         The data frame may also include hierarchical information depending on the parameters set.
#'
#' @noRd
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Construct Dyntaxa taxonomy table for taxon IDs 238366 and 1010380
#' parent_ids <- get_dyntaxa_parent_ids(c(238366, 1010380), "your_subscription_key")
#' taxonomy_table <- SHARK4R:::construct_dyntaxa_missing_table(parent_ids, "your_subscription_key")
#' print(taxonomy_table)
#' }
#'
construct_dyntaxa_missing_table <- function(parent_ids,
                                            subscription_key = Sys.getenv("DYNTAXA_KEY"),
                                            shark_output = TRUE,
                                            recommended_only = TRUE,
                                            add_genus_children = FALSE,
                                            drop_morphotypes = TRUE,
                                            add_hierarchy = FALSE,
                                            verbose = TRUE) {
  if (is.null(subscription_key) || subscription_key == "") {
    stop("No Dyntaxa subscription key provided. See ?construct_dyntaxa_missing_table for setup instructions.")
  }

  if (!is.list(parent_ids)) {
    parent_ids <- list(parent_ids)
  }

  if (any(is.na(unlist(parent_ids)))) {
    stop("parent_ids should not contain NA.")
  }

  taxa <- data.frame()

  # Set up progress bar
  if (verbose) {pb <- utils::txtProgressBar(min = 0, max = length(parent_ids), style = 3)}

  # Initialize counters
  if_counter <- 0
  else_counter <- 0

  for (i in seq_along(parent_ids)) {
    ids <- parent_ids[[i]]
    single <- unique(ids)

    if (is.null(ids)) {
      # Update progress bar at the end of each iteration
      if (verbose) {utils::setTxtProgressBar(pb, i)}
      next
    }

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
        nomenclature <- selected$nomenclature
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
          nomenclature <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(nomenclature)
          usage_value <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(usage.value)
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
          nomenclature <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci" & isRecommended == TRUE) %>%
            slice(1) %>%
            pull(nomenclature)
          recommended <- taxa_ix$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci" & isRecommended == TRUE) %>%
            slice(1) %>%
            pull(isRecommended)
          author <- ifelse(length(author) == 0, NA, author)
        }
      }
      taxa_temp <- data.frame(taxon_id, parent_id, parent_name, name, rank, author, hierarchy, recommended, usage_value, taxon_id_recommended, name_recommended, nomenclature) %>%
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
          nomenclature = character(0),
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

          nomenclature <- children_record$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(nomenclature)

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
            pull(usage.value)

          author <- children_record$names %>%
            map_df(as.data.frame) %>%
            filter(nameShort == "sci") %>%
            pull(author)

          if (rank == "Hybrid") {
            next
          }

          if (rank %in% c("Genus", "Subgenus", "Species", "SpeciesComplex", "CollectiveTaxon", "Section", "SpeciesAggregate")) {
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

          taxa_recommended <- data.frame(taxon_id_recommended, parent_id = parent, parent_name, taxon_id, recommended, usage_value, name, author, name_recommended, rank, hierarchy, nomenclature) %>%
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
      left_join(., taxa_i, by = c("taxon_id", "name", "parent_id", "parent_name", "hierarchy", "author", "recommended", "usage_value", "taxonId", "taxonId_recommended", "taxon_id_recommended","nomenclature"))

    if ("Species" %in% colnames(taxa_i)) {
      taxa_i <- taxa_i %>%
        mutate(Species = ifelse(!is.na(parent_name), parent_name, Species))
    }

    shark_taxonomy <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

    taxa_i <- taxa_i %>%
      mutate(across(all_of(shark_taxonomy[shark_taxonomy %in% taxa_i$rank]), fill_na_below_first_non_na))

    if ("Subgenus" %in% colnames(taxa_i) && "Species" %in% colnames(taxa_i)) {
      taxa_i <- taxa_i %>%
        mutate(Species = ifelse(rank == "Subgenus", NA, Species))
    }

    if ("SpeciesComplex" %in% colnames(taxa_i) && "Species" %in% colnames(taxa_i)) {
      taxa_i <- taxa_i %>%
        mutate(Species = ifelse(rank == "SpeciesComplex", NA, Species))
    }

    if ("Section" %in% colnames(taxa_i) && "Species" %in% colnames(taxa_i)) {
      taxa_i <- taxa_i %>%
        mutate(Species = ifelse(rank == "Section", NA, Species))
    }

    if ("CollectiveTaxon" %in% colnames(taxa_i) && "Species" %in% colnames(taxa_i)) {
      taxa_i <- taxa_i %>%
        mutate(Species = ifelse(rank == "CollectiveTaxon", NA, Species))
    }

    if ("SpeciesAggregate" %in% colnames(taxa_i) && "Species" %in% colnames(taxa_i)) {
      taxa_i <- taxa_i %>%
        mutate(Species = ifelse(rank == "SpeciesAggregate", NA, Species))
    }

    taxa <- bind_rows(taxa, taxa_i)

    # Update progress bar at the end of each iteration
    if (verbose) {utils::setTxtProgressBar(pb, i)}
  }

  # Close progress bar
  if (verbose) {close(pb)}

  if (nrow(taxa) == 0) {
    return(data.frame())
  }

  if (recommended_only) {
    taxa <- taxa %>%
      filter(recommended)
  }

  if (drop_morphotypes) {
    taxa <- taxa %>%
      filter(!rank == "MorphoType")
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
             nomenclaturalStatus = nomenclature,
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

  if (!add_hierarchy) {
    taxa_filtered <- taxa_filtered %>%
      select(-hierarchy) %>%
      distinct()
  }

  # Remove blank parent taxon ID (root)
  if (!shark_output) {
    taxa_filtered <- taxa_filtered %>%
      mutate(parentNameUsageID = gsub("urn:lsid:dyntaxa.se:Taxon:NA", NA, parentNameUsageID))
  }

  # Print the counters, for debugging
  if (verbose) {
    cat("Cached taxa requests:", if_counter, "\n")
    cat("Unique taxa requests:", else_counter, "\n")
  }

  return(taxa_filtered)
}

#' Function to fill NA values below the first non-NA value in a vector
#'
#' This internal function fills NA values below the first non-NA value in a vector.
#'
#' @param x A vector.
#' @return A vector with NAs filled below the first non-NA value.
#'
#' @noRd
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

#' Update SHARK taxonomy records using Dyntaxa
#'
#' This function updates Dyntaxa taxonomy records based on a list of Dyntaxa taxon IDs.
#' It collects parent IDs from SLU Artdatabanken API (Dyntaxa), retrieves full taxonomy records, and organizes
#' the data into a full taxonomic table that can be joined with data downloaded from [SHARK](https://shark.smhi.se/en/)
#'
#' @details
#' A valid Dyntaxa API subscription key is required.
#' You can request a free key for the "Taxonomy" service from the ArtDatabanken API portal:
#' <https://api-portal.artdatabanken.se/>
#'
#' **Note**: Please review the [API conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
#' and [register for access](https://api-portal.artdatabanken.se/) before using the API. Data collected through the API
#' is stored at SLU Artdatabanken. Please also note that the authors of `SHARK4R` are not affiliated with SLU Artdatabanken.
#'
#' @param dyntaxa_ids A vector of Dyntaxa taxon IDs to update.
#' @param subscription_key A Dyntaxa API subscription key. By default, the key
#'   is read from the environment variable \code{DYNTAXA_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{update_dyntaxa_taxonomy(238366, subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(DYNTAXA_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{DYNTAXA_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param add_missing_taxa Logical. If TRUE, the function will attempt to fetch missing taxa (i.e., taxon_ids not found in the initial Dyntaxa DwC-A query). Default is FALSE.
#' @param verbose Logical. Print progress messages. Default is TRUE.
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
#'
#' @seealso \code{\link{get_shark_data}}, \code{\link{update_worms_taxonomy}}, [SLU Artdatabanken API Documentation](https://api-portal.artdatabanken.se/)
#'
update_dyntaxa_taxonomy <- function(dyntaxa_ids,
                                    subscription_key = Sys.getenv("DYNTAXA_KEY"),
                                    add_missing_taxa = FALSE,
                                    verbose = TRUE) {
  if (is.null(subscription_key) || subscription_key == "") {
    stop("No Dyntaxa subscription key provided. See ?update_dyntaxa_taxonomy for setup instructions.")
  }

  if (verbose) {
    cat("Collecting full taxonomy records from Dyntaxa\n")
  }

  tax_table <- construct_dyntaxa_table(dyntaxa_ids,
                                       subscription_key,
                                       shark_output = TRUE,
                                       add_hierarchy = TRUE,
                                       add_parents = FALSE,
                                       add_synonyms = FALSE,
                                       add_descendants = FALSE,
                                       add_missing_taxa = add_missing_taxa,
                                       verbose = verbose)

  tax_table_shark <- tax_table %>%
    select(-rank, -author) %>%
    rename("dyntaxa_id" = taxon_id,
           "scientific_name" = name,
           "taxon_kingdom" = kingdom,
           "taxon_phylum" = phylum,
           "taxon_class" = class,
           "taxon_order" = order,
           "taxon_family" = family,
           "taxon_genus" = genus,
           "taxon_species" = species,
           "taxon_hierarchy" = hierarchy)

  # Ensure the output matches the input `dyntaxa_ids` in length and order
  output <- data.frame(dyntaxa_id = dyntaxa_ids) %>%
    left_join(tax_table_shark, by = "dyntaxa_id")

  return(tibble(output))
}

#' Match Dyntaxa taxon names
#'
#' This function matches a list of taxon names against the SLU Artdatabanken API (Dyntaxa) and retrieves the best matches along with their taxon IDs.
#'
#' @details
#' A valid Dyntaxa API subscription key is required.
#' You can request a free key for the "Taxonomy" service from the ArtDatabanken API portal:
#' <https://api-portal.artdatabanken.se/>
#'
#' **Note**: Please review the [API conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
#' and [register for access](https://api-portal.artdatabanken.se/) before using the API. Data collected through the API
#' is stored at SLU Artdatabanken. Please also note that the authors of `SHARK4R` are not affiliated with SLU Artdatabanken.
#'
#' @param taxon_names A vector of taxon names to match.
#' @param subscription_key A Dyntaxa API subscription key. By default, the key
#'   is read from the environment variable \code{DYNTAXA_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{match_dyntaxa_taxa("Skeletonema marinoi", subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(DYNTAXA_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{DYNTAXA_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param multiple_options Logical. If TRUE, the function will return multiple matching names. Default is FALSE, selecting the first match.
#' @param searchFields A character string indicating the search fields. Defaults to 'Both'.
#' @param isRecommended A character string indicating whether the taxon is recommended. Defaults to 'NotSet'.
#' @param isOkForObservationSystems A character string indicating whether the taxon is suitable for observation systems. Defaults to 'NotSet'.
#' @param culture A character string indicating the culture. Defaults to 'sv_SE'.
#' @param page An integer specifying the page number for pagination. Defaults to 1.
#' @param pageSize An integer specifying the page size for pagination. Defaults to 100.
#' @param verbose Logical. Print progress bar. Default is TRUE.
#'
#' @return A data frame containing the search pattern, taxon ID, and best match for each taxon name.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Match taxon names against SLU Artdatabanken API
#' matched_taxa <- match_dyntaxa_taxa(c("Homo sapiens", "Canis lupus"), "your_subscription_key")
#' print(matched_taxa)
#' }
#'
#' @seealso [SLU Artdatabanken API Documentation](https://api-portal.artdatabanken.se/)
#'
match_dyntaxa_taxa <- function(taxon_names,
                               subscription_key = Sys.getenv("DYNTAXA_KEY"),
                               multiple_options = FALSE,
                               searchFields = 'Both',
                               isRecommended = 'NotSet',
                               isOkForObservationSystems = 'NotSet',
                               culture = 'sv_SE',
                               page = 1,
                               pageSize = 100,
                               verbose = TRUE) {

  if (is.null(subscription_key) || subscription_key == "") {
    stop("No Dyntaxa subscription key provided. See ?match_dyntaxa_taxa for setup instructions.")
  }

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
    pb <- utils::txtProgressBar(min = 0, max = length(taxon_names), style = 3)
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
      utils::setTxtProgressBar(pb, .x)
    }

    result <- list(
      taxon_name = taxon_name,
      statusCode = status_code(response),
      responseBody = fromJSON(content(response, "text"))
    )

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

#' Match Dyntaxa taxon names
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated. Users are encouraged to use \code{\link{match_dyntaxa_taxa}} instead.
#'
#' This function matches a list of taxon names against the SLU Artdatabanken API (Dyntaxa) and retrieves the best matches along with their taxon IDs.
#'
#' @details
#' A valid Dyntaxa API subscription key is required.
#' You can request a free key for the "Taxonomy" service from the ArtDatabanken API portal:
#' <https://api-portal.artdatabanken.se/>
#'
#' **Note**: Please review the [API conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
#' and [register for access](https://api-portal.artdatabanken.se/) before using the API. Data collected through the API
#' is stored at SLU Artdatabanken. Please also note that the authors of `SHARK4R` are not affiliated with SLU Artdatabanken.
#'
#' @param taxon_names A vector of taxon names to match.
#' @param subscription_key A Dyntaxa API subscription key. By default, the key
#'   is read from the environment variable \code{DYNTAXA_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{match_taxon_name("Skeletonema marinoi", subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(DYNTAXA_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{DYNTAXA_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param multiple_options Logical. If TRUE, the function will return multiple matching names. Default is FALSE, selecting the first match.
#' @param searchFields A character string indicating the search fields. Defaults to 'Both'.
#' @param isRecommended A character string indicating whether the taxon is recommended. Defaults to 'NotSet'.
#' @param isOkForObservationSystems A character string indicating whether the taxon is suitable for observation systems. Defaults to 'NotSet'.
#' @param culture A character string indicating the culture. Defaults to 'sv_SE'.
#' @param page An integer specifying the page number for pagination. Defaults to 1.
#' @param pageSize An integer specifying the page size for pagination. Defaults to 100.
#' @param verbose Logical. Print progress bar. Default is TRUE.
#'
#' @return A data frame containing the search pattern, taxon ID, and best match for each taxon name.
#'
#' @keywords internal
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
match_taxon_name <- function(taxon_names,
                             subscription_key = Sys.getenv("DYNTAXA_KEY"),
                             multiple_options = FALSE,
                             searchFields = 'Both',
                             isRecommended = 'NotSet',
                             isOkForObservationSystems = 'NotSet',
                             culture = 'sv_SE',
                             page = 1,
                             pageSize = 100,
                             verbose = TRUE) {

  lifecycle::deprecate_warn("1.0.0", "match_taxon_name()", "match_dyntaxa_taxa()")

  match_dyntaxa_taxa(taxon_names = taxon_names,
                     subscription_key = subscription_key,
                     multiple_options = multiple_options,
                     searchFields = searchFields,
                     isRecommended = isRecommended,
                     isOkForObservationSystems = isOkForObservationSystems,
                     culture = culture,
                     page = page,
                     pageSize = pageSize,
                     verbose = verbose)
}

#' Download and read Darwin Core Archive files from Dyntaxa
#'
#' This function downloads a complete Darwin Core Archive (DwCA) of Dyntaxa from the SLU Artdatabanken API,
#' extracts the archive, and reads the specified CSV file into R.
#'
#' @details
#' By default, the archive is downloaded and cached across R sessions. On subsequent calls,
#' the function reuses the cached copy of the extracted files to avoid repeated downloads.
#' Use the `force` parameter to re-download the archive if needed. The cache is cleared
#' automatically after 24 hours, but you can also manually clear it using
#' \code{\link{clean_shark4r_cache}}.
#'
#' A valid Dyntaxa API subscription key is required.
#' You can request a free key for the "Taxonomy" service from the ArtDatabanken API portal:
#' <https://api-portal.artdatabanken.se/>
#'
#' **Note**: Please review the [API conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
#' and [register for access](https://api-portal.artdatabanken.se/) before using the API. Data collected through the API
#' is stored at SLU Artdatabanken. Please also note that the authors of `SHARK4R` are not affiliated with SLU Artdatabanken.
#'
#' @param subscription_key A Dyntaxa API subscription key. By default, the key
#'   is read from the environment variable \code{DYNTAXA_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{get_dyntaxa_dwca(subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(DYNTAXA_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{DYNTAXA_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param file_to_read A string specifying the name of the CSV file to read from the extracted archive.
#'   Allowed options are: `"Reference.csv"`, `"SpeciesDistribution.csv"`, `"Taxon.csv"`, or `"VernacularName.csv"`. Defaults to `"Taxon.csv"`.
#' @param force A logical value indicating whether to force a fresh download of the archive,
#'   even if a cached copy is available. Defaults to `FALSE`.
#' @param verbose A logical value indicating whether to show download progress. Defaults to `TRUE`.
#'
#' @return A tibble containing the data from the specified CSV file.
#'
#' @seealso [clean_shark4r_cache()] to manually clear cached files.
#'
#' @examples
#' \dontrun{
#' # Provide your Dyntaxa API subscription key
#' subscription_key <- "your_subscription_key"
#'
#' # Download and read the Taxon.csv file
#' taxon_data <- get_dyntaxa_dwca(subscription_key, file_to_read = "Taxon.csv")
#' }
#'
#' @export
get_dyntaxa_dwca <- function(subscription_key = Sys.getenv("DYNTAXA_KEY"),
                             file_to_read = "Taxon.csv",
                             force = FALSE,
                             verbose = TRUE) {

  if (is.null(subscription_key) || subscription_key == "") {
    stop("No Dyntaxa subscription key provided. See ?get_dyntaxa_dwca for setup instructions.")
  }

  allowed_files <- c("Reference.csv", "SpeciesDistribution.csv",
                     "Taxon.csv", "VernacularName.csv")
  if (!file_to_read %in% allowed_files) {
    stop("Invalid file_to_read. Allowed options are: ",
         paste(allowed_files, collapse = ", "))
  }

  cache_dir <- tools::R_user_dir("SHARK4R", "cache")
  csv_path <- file.path(cache_dir, file_to_read)

  # Check cache
  if (!force && file.exists(csv_path)) {
    if (verbose) message("Using cached copy of ", file_to_read)
    csv <- tryCatch(
      readr::read_tsv(csv_path, col_types = cols(), progress = FALSE),
      error = function(e) stop(
        sprintf(
          "Failed to read CSV file at '%s': %s\nConsider using force = TRUE to re-download the file.",
          csv_path, e$message
        ),
        call. = FALSE
      )
    )
    return(csv)
  } else {
    # Download if no cache or force = TRUE
    url <- "https://api.artdatabanken.se/taxonservice/v1/darwincore/download"
    headers <- c(
      'Cache-Control' = 'no-cache',
      'Ocp-Apim-Subscription-Key' = subscription_key
    )

    response <- httr::GET(url,
                          httr::add_headers(headers),
                          if (verbose) httr::progress())

    if (httr::status_code(response) == 200) {
      temp_file <- tempfile(fileext = ".zip")
      writeBin(httr::content(response, "raw"), temp_file)

      # temp_dir <- tempfile(pattern = "dyntaxa_dwca_")
      # dir.create(temp_dir)
      utils::unzip(temp_file, exdir = cache_dir)

      # Store in cache
      # .shark4r_cache$extracted_dir <- temp_dir

      csv_path <- file.path(cache_dir, file_to_read)
      if (file.exists(csv_path)) {
        return(readr::read_tsv(csv_path, col_types = cols(), progress = FALSE))
      } else {
        stop(file_to_read, " not found in the extracted files.")
      }
    } else {
      stop("Failed to download the zip file: ", httr::status_code(response))
    }
  }
}
#' Construct a hierarchical taxonomy table from Dyntaxa
#'
#' This function constructs a taxonomy table based on Dyntaxa taxon IDs.
#' It queries the SLU Artdatabanken API (Dyntaxa) to fetch taxonomy information and organizes the data into a hierarchical table.
#'
#' @details
#' A valid Dyntaxa API subscription key is required.
#' You can request a free key for the "Taxonomy" service from the ArtDatabanken API portal:
#' <https://api-portal.artdatabanken.se/>
#'
#' **Note**: Please review the [API conditions](https://www.slu.se/artdatabanken/rapportering-och-fynd/oppna-data-och-apier/)
#' and [register for access](https://api-portal.artdatabanken.se/) before using the API. Data collected through the API
#' is stored at SLU Artdatabanken. Please also note that the authors of `SHARK4R` are not affiliated with SLU Artdatabanken.
#'
#' @param taxon_ids An integer vector containing taxon IDs for which taxonomy information is requested. These IDs should correspond to specific taxonomic entities within the Dyntaxa database.
#' @param subscription_key A Dyntaxa API subscription key. By default, the key
#'   is read from the environment variable \code{DYNTAXA_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{construct_dyntaxa_table(238366, subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(DYNTAXA_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{DYNTAXA_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param shark_output Logical. If TRUE, the function will return a table formatted with SHARK-compatible columns. If FALSE, all available columns are returned. Default is TRUE.
#' @param add_parents Logical. If TRUE, the function will include parent taxa (higher ranks) for the specified taxon IDs in the output. Default is TRUE.
#' @param add_descendants Logical. If TRUE, the output will include descendant taxa (lower ranks) for the specified taxon IDs and the rank specified in `add_descendants_rank`. Default is FALSE.
#' @param add_descendants_rank Character string specifying the rank of descendant taxa to include. Allowed values are "kingdom", "phylum", "class", "order", "family", "genus", and "species". Default is "genus".
#' @param add_synonyms Logical. If TRUE, the function will include synonyms for the accepted taxa in the output. Default is TRUE.
#' @param add_missing_taxa Logical. If TRUE, the function will attempt to fetch missing taxa (i.e., taxa not found in the initial Dyntaxa DwC-A query, such as species complexes). Default is FALSE.
#' @param add_hierarchy Logical. If TRUE, the function will add a hierarchy column indicating the taxonomic relationships (e.g., parent-child) among the taxa. Default is FALSE.
#' @param verbose Logical. If TRUE, the function will print additional messages to provide feedback on its progress. Default is TRUE.
#' @param add_genus_children
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{add_descendants} instead.
#' @param recommended_only
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{add_synonyms} instead.
#' @param parent_ids
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{taxon_ids} instead. `construct_dyntaxa_table` now handles taxon IDs.
#'
#' @return A data frame representing the constructed taxonomy table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Construct Dyntaxa taxonomy table for taxon IDs 238366 and 1010380
#' taxon_ids <- c(238366, 1010380)
#' taxonomy_table <- construct_dyntaxa_table(taxon_ids, "your_subscription_key")
#' print(taxonomy_table)
#' }
#'
#' @seealso \code{\link{get_worms_taxonomy_tree}} for an equivalent WoRMS function
#' @seealso [SLU Artdatabanken API Documentation](https://api-portal.artdatabanken.se/)
#'
construct_dyntaxa_table <- function(taxon_ids, subscription_key = Sys.getenv("DYNTAXA_KEY"),
                                    shark_output = TRUE, add_parents = TRUE, add_descendants = FALSE,
                                    add_descendants_rank = "genus", add_synonyms = TRUE,
                                    add_missing_taxa = FALSE, add_hierarchy = FALSE,
                                    verbose = TRUE, add_genus_children = deprecated(),
                                    recommended_only = deprecated(), parent_ids = deprecated()) {

  if (is.null(subscription_key) || subscription_key == "") {
    stop("No Dyntaxa subscription key provided. See ?construct_dyntaxa_table for setup instructions.")
  }

  # Check for deprecated 'parent_ids' argument
  if (is_present(parent_ids) | is.list(taxon_ids)) {
    # Signal to the user that the `parent_ids` argument is deprecated
    deprecate_warn("0.1.5", "SHARK4R::construct_dyntaxa_table(parent_ids = )",
                   "SHARK4R::construct_dyntaxa_table(taxon_ids = )",
                   "The `construct_dyntaxa_table` function now handles taxon IDs and do not require parent IDs. The last element in each vector of `parent_ids` has been used to construct the table and `add_parents` has been set to TRUE.")

    if (is.list(taxon_ids)) {
      parent_ids <- taxon_ids
    }

    taxon_ids <- sapply(parent_ids, function(x) x[length(x)])
    add_parents <- TRUE
  }

  # Check for deprecated 'parent_ids' argument
  if (is_present(recommended_only)) {
    # Signal to the user that the `parent_ids` argument is deprecated
    deprecate_warn("0.1.5", "SHARK4R::construct_dyntaxa_table(recommended_only = )", "SHARK4R::construct_dyntaxa_table(add_synonyms = )")

    if (recommended_only) {
      add_synonyms <- FALSE
    } else {
      add_synonyms <- TRUE
    }
  }

  # Handle deprecated 'add_genus_children' argument and adjust to 'add_descendants'
  if (is_present(add_genus_children)) {
    # Signal a warning to the user that the 'add_genus_children' argument is deprecated
    deprecate_warn("0.1.5", "SHARK4R::construct_dyntaxa_table(add_genus_children = )", "SHARK4R::construct_dyntaxa_table(add_descendants = )")

    # If 'add_genus_children' is provided, set 'add_descendants' to that value
    add_descendants <- add_genus_children
    add_descendants_rank <- "genus"
  }

  # Validate the 'add_descendants_rank' parameter to ensure it is one of the allowed ranks
  allowed_ranks <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
  if (add_descendants && !add_descendants_rank %in% allowed_ranks) {
    stop("Invalid add_descendants_rank. Allowed options are: ", paste(allowed_ranks, collapse = ", "))
  }

  if (any(taxon_ids > .Machine$integer.max, na.rm = TRUE)) {
    stop("One or more taxon_ids exceed the maximum integer value: ", .Machine$integer.max, ".")
  }

  # Print a message to indicate data download
  if (verbose) {
    cat("Downloading DwC-A list:", "\n")
  }

  # Download the DyNTaxa data using the subscription key
  data <- get_dyntaxa_dwca(subscription_key, verbose = verbose)

  # Filter the data to only include rows that match the provided taxon_ids
  data_all <- data %>%
    filter(taxonId %in% paste0("urn:lsid:dyntaxa.se:Taxon:", taxon_ids))

  # If requested, add parent taxa
  if (add_parents) {
    all_parents <- get_all_parents(data, taxon_ids) # Function to fetch all parents
    data_all <- data_all %>%
      bind_rows(all_parents) %>%
      distinct() # Ensure unique rows
  }

  # If requested, add descendant taxa
  if (add_descendants) {
    # Filter to find the taxa at the specified rank (e.g., genus)
    rank_data <- data_all %>% filter(taxonRank == add_descendants_rank)

    # Initialize an empty dataframe to store descendant taxa
    descendants <- tibble()

    # Initialize a progress bar for the descendant search
    if (verbose) {
      cat("Finding descendants from:", add_descendants_rank, "\n")
      pb <- utils::txtProgressBar(min = 0, max = nrow(rank_data), style = 3)
    }

    # Loop over each genus and find its descendants
    for (i in 1:nrow(rank_data)) {
      genus <- rank_data$taxonId[i]  # Get the taxonId of the current genus
      descendants <- bind_rows(descendants, find_descendants(genus, data))  # Find descendants for this genus

      # Update the progress bar after each iteration
      if (verbose) {
        utils::setTxtProgressBar(pb, i)
      }
    }

    # Close the progress bar once done
    if (verbose) {
      close(pb)
    }

    # Combine the descendants with the main data frame and ensure uniqueness
    data_all <- distinct(bind_rows(data_all, descendants))
  }

  # If requested, add synonyms for accepted taxa
  if (add_synonyms) {
    synonyms <- data %>%
      filter(acceptedNameUsageID %in% data_all$taxonId) %>%  # Find synonyms where acceptedNameUsageID matches taxonId
      filter(!acceptedNameUsageID == taxonId)  # Ensure the synonym is not the same as the taxon

    data_all <- distinct(bind_rows(data_all, synonyms))  # Add the synonyms to the data
  }

  # If requested, add missing taxa from the match_adj table
  if (add_missing_taxa) {
    # Find missing taxa not already present in the data
    missing <- taxon_ids[!paste0("urn:lsid:dyntaxa.se:Taxon:", taxon_ids) %in% data_all$taxonId]

    # Extract unique missing taxon IDs
    missing_ids <- unique(missing)[!is.na(unique(missing))]

    if (length(missing_ids) > 0) {

      if (verbose) {
        cat("Getting parent IDs from", length(missing_ids), "missing taxa:", "\n")
      }

      # Retrieve the parent IDs for the missing taxa
      parents <- get_dyntaxa_parent_ids(missing_ids, subscription_key, verbose = verbose)

      if (verbose) {
        cat("Getting records from", length(missing_ids), "missing taxa:", "\n")
      }

      # Construct a table for the missing taxa
      missing_table <- construct_dyntaxa_missing_table(parents,
                                                       subscription_key,
                                                       shark_output = FALSE,
                                                       verbose = verbose)

      if (nrow(missing_table) > 0) {
        # Clean up and modify the missing taxa table
        missing_table <- missing_table %>%
          filter(!taxonId %in% data_all$taxonId) %>%  # Exclude taxa already in the data
          mutate(nomenclaturalStatus = gsub("Acceptable", "valid", nomenclaturalStatus),
                 taxonomicStatus = gsub("Accepted", "accepted", taxonomicStatus),
                 taxonomicStatus = gsub("Synonym", "synonym", taxonomicStatus),
                 taxonRank = tolower(taxonRank)) %>%
          mutate(taxonRank = gsub("speciesaggregate", "speciesAggregate", taxonRank))

        if (nrow(missing_table) > 0) {
          data_all <- distinct(bind_rows(data_all, missing_table))
        }
      }
    }
  }

  # If requested, add a hierarchy column
  if (add_hierarchy) {
    data <- distinct(bind_rows(data_all, data))

    if (verbose) {
      cat("Building hierarchy:", "\n")
    }
    data_all <- add_hierarchy_column(data_all,
                                     data,
                                     verbose = verbose)
  }

  # If requested, format output for SHARK taxonomy database
  if (shark_output) {
    shark_taxonomy <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")

    cols_to_select <- c("taxon_id", "name", "rank", "author", tolower(shark_taxonomy))
    if (add_hierarchy) {
      cols_to_select <- c(cols_to_select, "hierarchy")
    }

    data_all <- data_all %>%
      rename(
        taxon_id = taxonId,
        name = scientificName,
        rank = taxonRank,
        author = scientificNameAuthorship
      ) %>%
      select(all_of(cols_to_select)) %>%
      mutate(taxon_id = gsub("urn:lsid:dyntaxa.se:Taxon:", "", taxon_id)) %>%
      mutate(taxon_id = as.numeric(gsub("urn:lsid:dyntaxa.se:TaxonName:", "", taxon_id))) %>%
      distinct()
  }

  # Return the final data table with all requested modifications
  return(data_all)
}
#' Find all descendants of a taxon
#'
#' This helper function recursively finds all descendant taxa of a given taxon ID
#' by traversing the parent-child hierarchy in the provided dataset.
#'
#' @param taxon_id A character string representing the taxon ID for which to find descendants.
#' @param data A data frame containing taxonomic information with columns
#'   `taxonId` and `parentNameUsageID`.
#'
#' @return A tibble containing all descendants of the given taxon, including their
#'   `taxonId`, `parentNameUsageID`, and any other columns present in the input `data`.
#'
#' @examples
#' # Example data
#' data <- dplyr::tibble(
#'   taxonId = c("1", "2", "3", "4"),
#'   parentNameUsageID = c(NA, "1", "2", "2"),
#'   scientificName = c("Root", "Child1", "Child2", "Grandchild1")
#' )
#'
#' # Find descendants of taxon "1"
#' SHARK4R:::find_descendants("1", data)
#'
#' @noRd
#'
#' @keywords internal
find_descendants <- function(taxon_id, data) {
  # Get immediate children of the current taxon
  descendants <- data %>% filter(parentNameUsageID == taxon_id)

  # If there are no descendants, return an empty dataframe
  if (nrow(descendants) == 0) {
    return(tibble())
  }

  # Initialize a dataframe to store all descendants
  all_descendants <- descendants

  # For each child, find their descendants recursively
  for (child in descendants$taxonId) {
    all_descendants <- bind_rows(all_descendants, find_descendants(child, data))
  }

  return(all_descendants)
}
#' Get all parent taxa for a set of taxon IDs
#'
#' This helper function iteratively retrieves all parent taxa for a given set
#' of initial taxon IDs, traversing up the taxonomic hierarchy until the root
#' (taxa with no parents) is reached.
#'
#' @param data A data frame containing taxonomic information with columns
#'   `taxonId` and `parentNameUsageID`. The data can be collected using the
#'   `get_dyntaxa_dwca()` function.
#' @param initial_taxon_ids A character vector of initial taxon IDs for which
#'   to retrieve all parent taxa. These IDs should correspond to the `taxonId`
#'   column in the dataset.
#'
#' @return A tibble containing all parent taxa for the given initial taxon IDs,
#'   including their `taxonId`, `parentNameUsageID`, and any other columns present
#'   in the input `data`.
#'
#' @examples
#' # Example data
#' data <- dplyr::tibble(
#'   taxonId = c("1", "2", "3", "4"),
#'   parentNameUsageID = c(NA, "1", "1", "2"),
#'   scientificName = c("Root", "Child1", "Child2", "Grandchild1")
#' )
#'
#' # Find all parents of taxon IDs "3" and "4"
#' SHARK4R:::get_all_parents(data, initial_taxon_ids = c("3", "4"))
#'
#' @noRd
#'
#' @keywords internal
get_all_parents <- function(data, initial_taxon_ids) {
  # Start with the initial filtered data
  all_parents <- data %>%
    filter(taxonId %in% paste0("urn:lsid:dyntaxa.se:Taxon:", initial_taxon_ids))

  # Iteratively find parents until there are no more parents
  repeat {
    # Find the next set of parents
    next_parents <- data %>%
      filter(taxonId %in% all_parents$parentNameUsageID) %>%
      filter(!taxonId %in% all_parents$taxonId) # Avoid duplicates

    # Break if no new parents are found
    if (nrow(next_parents) == 0) break

    # Append the new parents to the results
    all_parents <- bind_rows(all_parents, next_parents)
  }

  return(all_parents)
}
#' Recursively retrieve parent taxonomic hierarchy
#'
#' This helper function retrieves the taxonomic hierarchy for a given taxon ID by
#' recursively traversing its parent relationships in the provided dataset.
#'
#' @param taxon_id A character string representing the taxon ID for which the hierarchy is to be retrieved.
#' @param data A data frame containing taxonomic information. Must include columns `taxonId`, `parentNameUsageID`, and `scientificName`.
#'
#' @return A character vector containing the hierarchy of parent `scientificName` values, starting with the immediate parent and ascending.
#'
#' @examples
#' # Example dataset
#' data <- dplyr::tibble(
#'   taxonId = c("1", "2", "3"),
#'   parentNameUsageID = c(NA, "1", "2"),
#'   scientificName = c("Kingdom", "Phylum", "Class")
#' )
#'
#' # Get hierarchy for taxon ID "3"
#' SHARK4R:::get_hierarchy("3", data)
#' # [1] "Phylum" "Kingdom"
#'
#' @noRd
#'
#' @keywords internal
get_hierarchy <- function(taxon_id, data) {
  # Find the parentNameUsageID for the given taxon_id
  parent_row <- data %>% filter(taxonId == taxon_id)

  # If no parent is found or the parent is NA, stop recursion
  if (nrow(parent_row) == 0 || is.na(parent_row$parentNameUsageID)) {
    return(character(0))
  }

  # Get the parent's scientificName
  parent_id <- parent_row$parentNameUsageID
  parent_name <- data %>% filter(taxonId == parent_id) %>% pull(scientificName)

  # Recurse to find the hierarchy of the parent
  c(get_hierarchy(parent_id, data), parent_name) # Reverse order, add parent first
}
#' Add taxonomic hierarchy column to a dataset
#'
#' This function adds a new column, `hierarchy`, to a dataset, where each row contains
#' the taxonomic hierarchy of parent `scientificName` values for the given `taxonId`.
#' The hierarchy is constructed using a recursive helper function.
#'
#' @param data A data frame containing taxonomic information. Must include columns `taxonId`,
#'   `parentNameUsageID`, and `scientificName`.
#' @param data_dwca A data frame representing the complete taxonomic dataset (e.g., full taxonomy table).
#'   Must include the same columns as `data`. Defaults to `data` if not provided.
#' @param verbose Logical. If TRUE, the function will print additional messages to provide feedback on its progress. Default is TRUE.
#'
#' @return A data frame with an additional `hierarchy` column. The `hierarchy` column contains
#'   a string representing the lineage of parent `scientificName` values, separated by " > ".
#'
#' @examples
#' # Example dataset
#' data <- dplyr::tibble(
#'   taxonId = c("1", "2", "3"),
#'   parentNameUsageID = c(NA, "1", "2"),
#'   scientificName = c("Kingdom", "Phylum", "Class")
#' )
#'
#' # Add hierarchy column
#' data_with_hierarchy <- SHARK4R:::add_hierarchy_column(data)
#' print(data_with_hierarchy)
#'
#' @noRd
#'
#' @keywords internal
add_hierarchy_column <- function(data, data_dwca = NULL, verbose = TRUE) {

  if (is.null(data_dwca)) {
    data_dwca <- data
  }

  # Initialize progress bar
  if (verbose) { pb <- utils::txtProgressBar(min = 0, max = nrow(data), style = 3) }

  # Add hierarchy column
  data$hierarchy <- vector("list", nrow(data)) # Preallocate list column
  for (i in seq_len(nrow(data))) {
    # Get the full hierarchy including the scientificName of the row itself
    hierarchy <- get_hierarchy(data$taxonId[i], data_dwca)

    # Add the current row's scientificName as the last element in the hierarchy
    data$hierarchy[[i]] <- c(hierarchy, data$scientificName[i])

    # Update progress bar
    if (verbose) { utils::setTxtProgressBar(pb, i) }
  }

  # Close progress bar
  if (verbose) { close(pb) }

  # Convert hierarchy list to a readable string
  data <- data %>%
    mutate(hierarchy = sapply(hierarchy, function(h) paste(h, collapse = " - ")))

  return(data)
}

#' Taxon matching using Dyntaxa (https://www.dyntaxa.se/)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated and has been replaced by [is_in_dyntaxa()].
#'
#' @param names Character vector of scientific names to check in Dyntaxa.
#' @param subscription_key A Dyntaxa API subscription key. By default, the key
#'   is read from the environment variable \code{DYNTAXA_KEY}.
#'
#' @return A logical vector indicating whether each input name was found in Dyntaxa,
#'   same as [is_in_dyntaxa()]. Messages about unmatched taxa are printed.
#'
#' @details
#' This function is retained for backward compatibility but may be removed in future versions.
#' Use the newer function [is_in_dyntaxa()] instead.
#'
#' A valid Dyntaxa API subscription key is required.
#' You can request a free key for the "Taxonomy" service from the ArtDatabanken API portal:
#' <https://api-portal.artdatabanken.se/>
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Deprecated function usage
#' match_dyntaxa(c("Skeletonema marinoi", "Nonexistent species"),
#'               subscription_key = "your_key_here")
#' }
#'
#' @export
match_dyntaxa <- function(names,
                          subscription_key = Sys.getenv("DYNTAXA_KEY")) {

  lifecycle::deprecate_warn("1.0.0", "match_dyntaxa()", "is_in_dyntaxa()", "Replaced by a new function name")

  is_in_dyntaxa(taxon_names = names,
                subscription_key = subscription_key,
                verbose = TRUE)
}

#' Check if taxon names exist in Dyntaxa
#'
#' Checks whether the supplied scientific names exist in the
#' Swedish taxonomic database Dyntaxa. Optionally, returns a data frame
#' with taxon names, taxon IDs, and match status.
#'
#' @param taxon_names Character vector of taxon names to check.
#' @param subscription_key A Dyntaxa API subscription key. By default, the key
#'   is read from the environment variable \code{DYNTAXA_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{is_in_dyntaxa("Skeletonema marinoi", subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(DYNTAXA_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{DYNTAXA_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param use_dwca Logical; if TRUE, uses the DwCA version of Dyntaxa instead of querying the API.
#' @param return_df Logical; if TRUE, returns a data frame with columns \code{taxon_name},
#'   \code{taxon_id}, and \code{match}. Default is FALSE (returns a logical vector).
#' @param verbose Logical; if TRUE, prints messages about unmatched taxa.
#'
#' @details
#' A valid Dyntaxa API subscription key is required.
#' You can request a free key for the "Taxonomy" service from the ArtDatabanken API portal:
#' <https://api-portal.artdatabanken.se/>
#'
#' @return If \code{return_df = FALSE} (default), a logical vector indicating whether each input
#'   name was found in Dyntaxa. Returned invisibly if \code{verbose = TRUE}.
#'   If \code{return_df = TRUE}, a data frame with columns:
#'   \itemize{
#'     \item \code{taxon_name}: original input names
#'     \item \code{taxon_id}: corresponding Dyntaxa taxon IDs (NA if not found)
#'     \item \code{match}: logical indicating presence in Dyntaxa
#'   }
#'
#' @examples
#' \dontrun{
#' # Using an environment variable (recommended for convenience)
#' Sys.setenv(DYNTAXA_KEY = "your_key_here")
#' is_in_dyntaxa(c("Skeletonema marinoi", "Nonexistent species"))
#'
#' # Return a data frame instead of logical vector
#' is_in_dyntaxa(c("Skeletonema marinoi", "Nonexistent species"), return_df = TRUE)
#'
#' # Or pass the key directly
#' is_in_dyntaxa("Skeletonema marinoi", subscription_key = "your_key_here")
#'
#' # Suppress messages
#' is_in_dyntaxa("Skeletonema marinoi", verbose = FALSE)
#' }
#'
#' @export
is_in_dyntaxa <- function(taxon_names,
                          subscription_key = Sys.getenv("DYNTAXA_KEY"),
                          use_dwca = FALSE,
                          return_df = FALSE,
                          verbose = FALSE) {

  if (is.null(subscription_key) || subscription_key == "") {
    stop("No Dyntaxa subscription key provided. See ?is_in_dyntaxa for setup instructions.")
  }

  # Get unique taxon names
  unique_taxa <- unique(taxon_names)

  if (use_dwca) {
    # Get dyntaxa DwCA
    dyntaxa_dwca <- get_dyntaxa_dwca(subscription_key, verbose = verbose)

    subset <- dyntaxa_dwca[dyntaxa_dwca$scientificName %in% unique_taxa, ] %>%
      dplyr::filter(grepl(":Taxon:", taxonId)) %>%
      dplyr::select(scientificName, taxonId)

    # Logical vector for unique taxa
    unique_match <- unique_taxa %in% dyntaxa_dwca$scientificName

    # Map taxon IDs
    taxon_ids <- subset$taxonId[match(unique_taxa, subset$scientificName)]

    # Convert to numeric
    taxon_ids <- as.numeric(sub(".*:Taxon:", "", taxon_ids))

  } else {
    # Query Dyntaxa for unique taxa
    dyntaxa_match <- match_dyntaxa_taxa(unique_taxa, subscription_key, verbose = FALSE)

    # Logical vector for unique taxa
    unique_match <- unique_taxa %in% dyntaxa_match$best_match

    # Map taxon IDs
    taxon_ids <- dyntaxa_match$taxon_id[match(unique_taxa, dyntaxa_match$best_match)]
  }

  # Map back to original input length
  match <- unique_match[match(taxon_names, unique_taxa)]
  taxon_ids <- taxon_ids[match(taxon_names, unique_taxa)]

  if (verbose) {
    if (any(!match)) {
      message("Unmatched taxa found:")
      print(data.frame(
        reported_ScientificName = taxon_names[!match],
        in_dyntaxa = match[!match]
      ))
    } else {
      message("All taxa found")
    }
  }

  if (return_df) {
    return(data.frame(
      taxon_name = taxon_names,
      in_dyntaxa = match,
      dyntaxa_id = taxon_ids,
      stringsAsFactors = FALSE
    ))
  } else {
    return(match)
  }
}
