#' Retrieve taxa information from Nordic Microalgae
#'
#' This function retrieves all taxonomic information for algae taxa from the Nordic Microalgae API.
#' It fetches details including scientific names, authorities, ranks, and image URLs (in different sizes: large, medium, original, and small).
#'
#' @param unparsed Logical. If `TRUE`, complete API response is returned as an unparsed list. Default is `FALSE`.
#'
#' @return When unparsed = `FALSE`: a `tibble` containing the following columns:
#'   \item{slug}{A unique identifier for the taxon.}
#'   \item{scientific_name}{The scientific name of the taxon.}
#'   \item{authority}{The authority associated with the scientific name.}
#'   \item{rank}{The taxonomic rank of the taxon.}
#'
#' @examples
#' \donttest{
#'   # Retrieve and display taxa data
#'   taxa_data <- get_nua_taxa(unparsed = FALSE)
#'   head(taxa_data)
#' }
#'
#' @seealso \url{https://nordicmicroalgae.org/} for Nordic Microalgae website.
#' @seealso \url{https://nordicmicroalgae.org/api/} for Nordic Microalgae API documentation.
#'
#' @export
get_nua_taxa <- function(unparsed = FALSE) {
  # Define the URL for options
  url <- "https://nordicmicroalgae.org/api/taxa/"

  # Simplified URL for connection check
  url_short <- gsub("api/taxa/", "", url)

  # Check if the URL is reachable
  url_response <- try(GET(url_short), silent = TRUE)

  if (inherits(url_response, "try-error") || http_error(url_response)) {
    stop("The Nordic Microalgae server cannot be reached: ", url_short, ". Please check network connection.")
  }

  # Make the GET request
  response <- GET(url, add_headers("accept" = "application/json"))

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON response content
    nua_taxa <- content(response, as = "parsed", type = "application/json")
    nua_taxa <- nua_taxa$taxa

    if (unparsed) {
      return(nua_taxa)
    }

    # Extract required fields with NULL handling
    extract_taxa_info <- function(taxa) {
      taxa_info <- tibble(
        scientific_name = ifelse(!is.null(taxa$scientific_name), taxa$scientific_name, NA),
        authority = ifelse(!is.null(taxa$authority), taxa$authority, NA),
        rank = ifelse(!is.null(taxa$rank), taxa$rank, NA),
        slug = ifelse(!is.null(taxa$slug), taxa$slug, NA),
        nua_url = paste0("https://nordicmicroalgae.org/taxon/", ifelse(!is.null(taxa$slug), taxa$slug, NA))
      )

      return(taxa_info)
    }

    # Apply extraction and handle possible empty cases
    result <- do.call(rbind, lapply(nua_taxa, function(x) {
      if (length(x) > 0) {
        extract_taxa_info(x)
      } else {
        tibble(slug = NA, scientific_name = NA, authority = NA, rank = NA,
               image_l_url = NA, image_m_url = NA, image_o_url = NA, image_s_url = NA)
      }
    }))

    return(result)
  } else {
    # Return the error message if the request failed
    stop("Failed to retrieve options: ", status_code(response))
  }
}
#' Retrieve external links or facts for taxa from Nordic Microalgae
#'
#' This function retrieves external links related to algae taxa from the Nordic Microalgae API.
#' It takes a vector of slugs (taxon identifiers) and returns a data frame containing the external links
#' associated with each taxon. The data includes the provider, label, external ID, and the URL of the external link.
#'
#' The slugs (taxon identifiers) used in this function can be retrieved using the `get_nua_taxa()` function,
#' which returns a data frame with a column for taxon slugs, along with other relevant metadata for each taxon.
#'
#' @param slug A vector of taxon slugs (identifiers) for which to retrieve external links.
#' @param verbose A logical flag indicating whether to display a progress bar. Default is `TRUE`.
#' @param unparsed Logical. If `TRUE`, the API response with all facts is returned as an unparsed list. Default is `FALSE`.
#'
#' @return When unparsed = `FALSE`: a `tibble` containing the following columns:
#'   \item{slug}{The slug (identifier) of the taxon.}
#'   \item{provider}{The provider of the external link.}
#'   \item{label}{The label of the external link.}
#'   \item{external_id}{The external ID associated with the external link.}
#'   \item{external_url}{The URL of the external link.}
#'   \item{collection}{The collection category, which is "External Links" for all rows.}
#'
#' @seealso \url{https://nordicmicroalgae.org/} for Nordic Microalgae website.
#' @seealso \url{https://nordicmicroalgae.org/api/} for Nordic Microalgae API documentation.
#'
#' @examples
#' \donttest{
#'   # Retrieve external links for a vector of slugs
#'   external_links <- get_nua_external_links(slug = c("chaetoceros-debilis", "alexandrium-tamarense"),
#'                                            verbose = FALSE)
#'   head(external_links)
#' }
#' @export
get_nua_external_links <- function(slug, verbose = TRUE, unparsed = FALSE) {
  # Define the base URL
  base_url <- "https://nordicmicroalgae.org/api/facts/"

  # Check if the server is reachable
  base_url_short <- gsub("api/facts/", "", base_url)
  url_response <- try(GET(base_url_short), silent = TRUE)
  if (inherits(url_response, "try-error") || http_error(url_response)) {
    stop("The Nordic Microalgae server cannot be reached: ", base_url_short, ". Please check your network connection.")
  }

  # Set up progress bar
  if (verbose) {pb <- utils::txtProgressBar(min = 0, max = length(slug), style = 3)}

  # Initialize an empty dataframe or list to store results
  if (unparsed) {
    nua_facts <- list()
  } else {
    nua_facts <- tibble()
  }

  for (i in seq_along(slug)) {
    url <- paste0(base_url, slug[i])

    # Update progress bar
    if (verbose) {utils::setTxtProgressBar(pb, i)}

    # Make the GET request
    response <- GET(url, add_headers("accept" = "application/json"))

    if (status_code(response) == 200) {
      # Parse the JSON response content
      nua_fact <- content(response, as = "parsed", type = "application/json")

      if (unparsed) {
        # Combine the data into the result list
        nua_facts[[slug[i]]] <- nua_fact
      } else {
        # Filter collections with "External Links"
        external_links <- Filter(function(x) x$collection == "External Links", nua_fact$facts)

        # Extract the relevant data
        facts_df <- do.call(rbind, lapply(external_links, function(fact) {
          provider <- fact$provider
          attributes <- fact$attributes
          do.call(rbind, lapply(attributes, function(attr) {
            tibble(
              slug = slug[i],
              provider = provider,
              label = attr$label,
              external_id = attr$external_id,
              external_url = attr$external_url,
              collection = fact$collection
            )
          }))
        }))

        # Combine the data into the result dataframe
        nua_facts <- bind_rows(nua_facts, facts_df)
      }
    } else {
      warning("Failed to retrieve facts for slug: ", slug[i], " (status code: ", status_code(response), ")")
    }
  }

  if (verbose) {close(pb)}

  return(nua_facts)
}
#' Retrieve harmfulness for taxa from Nordic Microalgae
#'
#' This function retrieves harmfulness information related to algae taxa from the Nordic Microalgae API.
#' It takes a vector of slugs (taxon identifiers) and returns a data frame containing the harmfulness information
#' associated with each taxon. The data includes the provider, label, external ID, and the URL of the external link.
#'
#' The slugs (taxon identifiers) used in this function can be retrieved using the `get_nua_taxa()` function,
#' which returns a data frame with a column for taxon slugs, along with other relevant metadata for each taxon.
#'
#' @param slug A vector of taxon slugs (identifiers) for which to retrieve external links.
#' @param verbose A logical flag indicating whether to display a progress bar. Default is `TRUE`.
#'
#' @return A `tibble` containing the following columns:
#'   \item{slug}{The slug (identifier) of the taxon.}
#'   \item{provider}{The provider of the external link.}
#'   \item{label}{The label of the external link.}
#'   \item{external_id}{The external ID associated with the external link.}
#'   \item{external_url}{The URL of the external link.}
#'   \item{collection}{The collection category, which is "Harmful algae blooms" for all rows.}
#'
#' @seealso \url{https://nordicmicroalgae.org/} for Nordic Microalgae website.
#' @seealso \url{https://nordicmicroalgae.org/api/} for Nordic Microalgae API documentation.
#'
#' @examples
#' \donttest{
#'   # Retrieve external links for a vector of slugs
#'   harmfulness <- get_nua_harmfulness(slug = c("dinophysis-acuta",
#'                                               "alexandrium-ostenfeldii"),
#'                                      verbose = FALSE)
#'   print(harmfulness)
#' }
#' @export
get_nua_harmfulness <- function(slug, verbose = TRUE) {
  # Define the base URL
  base_url <- "https://nordicmicroalgae.org/api/facts/"

  # Check if the server is reachable
  base_url_short <- gsub("api/facts/", "", base_url)
  url_response <- try(GET(base_url_short), silent = TRUE)
  if (inherits(url_response, "try-error") || http_error(url_response)) {
    stop("The Nordic Microalgae server cannot be reached: ", base_url_short, ". Please check your network connection.")
  }

  # Set up progress bar
  if (verbose) {pb <- utils::txtProgressBar(min = 0, max = length(slug), style = 3)}

  # Initialize an empty data frame to store results
  nua_facts <- tibble()

  for (i in seq_along(slug)) {
    url <- paste0(base_url, slug[i])

    # Update progress bar
    if (verbose) {utils::setTxtProgressBar(pb, i)}

    # Make the GET request
    response <- GET(url, add_headers("accept" = "application/json"))

    if (status_code(response) == 200) {
      # Parse the JSON response content
      nua_fact <- content(response, as = "parsed", type = "application/json")

      # Filter collections with "External Links"
      external_links <- Filter(function(x) x$collection == "Harmful algae blooms", nua_fact$facts)

      # Extract the relevant data
      facts_df <- do.call(rbind, lapply(external_links, function(fact) {
        provider <- fact$provider
        attributes <- fact$attributes
        do.call(rbind, lapply(attributes, function(attr) {
          tibble(
            slug = slug[i],
            provider = provider,
            label = attr$label,
            external_id = attr$external_id,
            external_url = attr$external_url,
            collection = fact$collection
          )
        }))
      }))

      # Combine the data into the result dataframe
      nua_facts <- bind_rows(nua_facts, facts_df)

    } else {
      warning("Failed to retrieve facts for slug: ", slug[i], " (status code: ", status_code(response), ")")
    }
  }

  if (verbose) {close(pb)}

  return(nua_facts)
}
#' Retrieve and extract media URLs from Nordic Microalgae
#'
#' This function retrieves media information from the Nordic Microalgae API and extracts slugs
#' and URLs for different renditions (large, original, small, medium) for each media item.
#'
#' @param unparsed Logical. If `TRUE`, complete API response is returned as an unparsed list. Default is `FALSE`.
#'
#' @return When unparsed = `FALSE`: a `tibble` with the following columns:
#'   \itemize{
#'     \item \code{slug}: The slug of the related taxon.
#'     \item \code{l_url}: The URL for the "large" rendition.
#'     \item \code{o_url}: The URL for the "original" rendition.
#'     \item \code{s_url}: The URL for the "small" rendition.
#'     \item \code{m_url}: The URL for the "medium" rendition.
#'   }
#'
#' @seealso \url{https://nordicmicroalgae.org/} for Nordic Microalgae website.
#' @seealso \url{https://nordicmicroalgae.org/api/} for Nordic Microalgae API documentation.
#'
#' @examples
#' \donttest{
#' # Retrieve media information
#' media_info <- get_nua_media_links(unparsed = FALSE)
#'
#' # Preview the extracted data
#' head(media_info)
#' }
#' @export
get_nua_media_links <- function(unparsed = FALSE) {
  # Define the base URL
  base_url <- "https://nordicmicroalgae.org/api/media/"

  # Check if the server is reachable
  base_url_short <- gsub("api/media/", "", base_url)
  url_response <- try(GET(base_url_short), silent = TRUE)
  if (inherits(url_response, "try-error") || http_error(url_response)) {
    stop("The Nordic Microalgae server cannot be reached: ", base_url_short, ". Please check your network connection.")
  }

  # Make the GET request
  response <- GET(base_url, add_headers("accept" = "application/json"))

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON response content
    nua_media <- content(response, as = "parsed", type = "application/json")
    nua_media <- nua_media$media

    if (unparsed) {
      return(nua_media)
    }

    # Function to extract slug and URLs
    extract_media_info <- function(nua_media) {
      do.call(rbind, lapply(nua_media, function(media_item) {
        # Extract related_taxon slug
        related_slug <- if (!is.null(media_item$related_taxon)) media_item$related_taxon$slug else NA

        # Extract galleries info
        galleries <- if (!is.null(media_item$attributes$galleries)) media_item$attributes$galleries else NA

        # Extract license info
        license <- if (!is.null(media_item$attributes$license)) media_item$attributes$license else NA

        # Extract copyright holder info
        contributor <- if (!is.null(media_item$attributes$contributor)) media_item$attributes$contributor else NA

        # Extract copyright holder info
        copyright_holder <- if (!is.null(media_item$attributes$copyright_holder)) media_item$attributes$copyright_holder else NA

        # Extract photographer_artist info
        photographer_artist <- if (!is.null(media_item$attributes$photographer_artist)) media_item$attributes$photographer_artist else NA

        # Extract URLs for renditions (l, o, s, m)
        renditions <- media_item$renditions
        urls <- list(
          l = if (!is.null(renditions$l)) renditions$l$url else NA,
          o = if (!is.null(renditions$o)) renditions$o$url else NA,
          s = if (!is.null(renditions$s)) renditions$s$url else NA,
          m = if (!is.null(renditions$m)) renditions$m$url else NA
        )

        # Combine into a data frame
        tibble(
          slug = related_slug,
          image_l_url = urls$l,
          image_o_url = urls$o,
          image_s_url = urls$s,
          image_m_url = urls$m,
          contributor = contributor,
          photographer_artist = photographer_artist,
          copyright_holder = copyright_holder,
          license = license,
          galleries = paste(galleries, collapse = ", ")
        )
      }))
    }

    # Extract data
    nua_media_info <- extract_media_info(nua_media)

    return(nua_media_info)

  } else {
    # Return the error message if the request failed
    stop("Failed to retrieve options: ", status_code(response))
  }
}
