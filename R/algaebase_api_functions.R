#' Search Algaebase for Taxonomic Information
#'
#' This function queries the Algaebase API to retrieve taxonomic information for a list of algae names based on genus and (optionally) species.
#'  It supports exact matching, genus-only searches, and retrieval of higher taxonomic ranks.
#'
#' @param genus A character vector of genus names.
#' @param species A character vector of species names corresponding to the `genus` vector. Must be the same length as `genus`.
#' @param apikey A character string containing the API key for accessing the Algaebase API.
#' @param genus_only Logical. If `TRUE`, searches are based solely on the genus name, ignoring species. Defaults to `FALSE`.
#' @param higher Logical. If `TRUE`, includes higher taxonomy (e.g., kingdom, phylum) in the output. Defaults to `TRUE`.
#' @param unparsed Logical. If `TRUE`, returns raw JSON output instead of an R data frame. Defaults to `FALSE`.
#' @param exact_matches_only Logical. If `TRUE`, restricts results to exact matches. Defaults to `TRUE`.
#' @param sleep_time Numeric. The delay (in seconds) between consecutive Algaebase API queries. Defaults to `1`. A delay is recommended to avoid overwhelming the API for large queries.
#' @param verbose Logical. If `TRUE`, displays a progress bar to indicate query status. Defaults to `TRUE`.
#'
#' @return A data frame containing taxonomic information for each input genus-species combination. Columns may include:
#' \itemize{
#'   \item \code{id}: Algaebase ID (if available)
#'   \item \code{kingdom}, \code{phylum}, \code{class}, \code{order}, \code{family}: Higher taxonomy (if \code{higher = TRUE})
#'   \item \code{genus}, \code{species}, \code{infrasp}: Genus, species, and infraspecies names (if applicable)
#'   \item \code{taxonomic_status}: Status of the name (e.g., "accepted", "synonym", "unverified")
#'   \item \code{currently_accepted}: Logical indicator for whether the name is currently accepted
#'   \item \code{accepted_name}: Currently accepted name if different from the input name
#'   \item \code{input_name}: Name supplied by the user
#'   \item \code{input_match}: \code{1} for exact matches, otherwise \code{0}
#'   \item \code{taxon_rank}: Taxonomic rank of the accepted name (e.g., "genus", "species")
#'   \item \code{mod_date}: Date when the entry was last modified in Algaebase
#'   \item \code{long_name}: Full species name with authorship and date
#'   \item \code{authorship}: Authors associated with the species name
#' }
#'
#' @details
#'
#' Scientific names can be parsed using the \code{parse_scientific_names()} function before being processed by \code{match_algaebase()}.
#'
#' Duplicate genus-species combinations are handled efficiently by querying each unique combination only once. Genus-only searches are performed when \code{genus_only = TRUE}
#' or when the species name is missing or invalid. Errors during API queries are gracefully handled by returning rows with \code{NA} values for missing or unavailable data.
#'
#' The function allows for integration with data analysis workflows that require resolving or verifying taxonomic names against Algaebase.
#'
#' @seealso \code{\link{parse_scientific_names}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with genus and species vectors
#' genus_vec <- c("Thalassiosira", "Skeletonema", "Tripos")
#' species_vec <- c("pseudonana", "costatum", "furca")
#'
#' algaebase_results <- match_algaebase(
#'   genus = genus_vec,
#'   species = species_vec,
#'   apikey = "your_api_key",
#'   exact_matches_only = TRUE,
#'   verbose = TRUE
#' )
#' head(algaebase_results)
#' }
match_algaebase <- function(genus, species, apikey = NULL, genus_only = FALSE,
                            higher = TRUE, unparsed = FALSE, exact_matches_only = TRUE,
                            sleep_time = 1, verbose = TRUE) {

  # Check input lengths
  if (length(genus) != length(species)) {
    stop("`genus` and `species` vectors must be of equal length.")
  }

  # Create unique combinations of genus and species
  input_data <- data.frame(genus = genus, species = species, stringsAsFactors = FALSE)
  unique_data <- unique(input_data)

  # Prepare output dataframe
  algaebase_df <- data.frame()

  # Helper function to generate an error row
  generate_error_row <- function(index, genus_only, genus, species, higher) {
    input_name <- if (genus_only) {
      genus[index]
    } else {
      trimws(paste(genus[index], species[index]))
    }

    err_df <- data.frame(
      id = NA, kingdom = NA, phylum = NA, class = NA, order = NA, family = NA,
      genus = NA, species = NA, infrasp = NA, taxonomic_status = NA,
      currently_accepted = NA, accepted_name = NA, genus_only = genus_only,
      input_name = input_name, input_match = 0, taxon_rank = NA,
      mod_date = NA, long_name = NA, authorship = NA
    )

    if (!higher) {
      err_df <- subset(err_df, select = -c(kingdom, phylum, class, order, family))
    }
    return(err_df)
  }

  # Set up progress bar
  if (verbose) {pb <- txtProgressBar(min = 0, max = nrow(unique_data), style = 3)}

  # Main loop over unique combinations
  for (i in seq_len(nrow(unique_data))) {
    Sys.sleep(sleep_time)

    # Update progress bar
    if (verbose) {setTxtProgressBar(pb, i)}

    genus_i <- unique_data$genus[i]
    species_i <- unique_data$species[i]

    if (is.na(genus_i) || genus_i == "") {
      tmp <- generate_error_row(i, genus_only, unique_data$genus, unique_data$species, higher)
    } else if (genus_only || is.na(species_i) || species_i == "") {
      tmp <- tryCatch(
        get_algaebase_genus(
          genus = genus_i, apikey = apikey,
          higher = higher, unparsed = unparsed, exact_matches_only = exact_matches_only
        ),
        error = function(e) generate_error_row(i, genus_only, unique_data$genus, unique_data$species, higher)
      )
    } else {
      tmp <- tryCatch(
        get_algaebase_species(
          genus = genus_i, species = species_i, apikey = apikey,
          higher = higher, unparsed = unparsed, exact_matches_only = exact_matches_only
        ),
        error = function(e) {
          tryCatch(
            get_algaebase_genus(
              genus = genus_i, apikey = apikey,
              higher = higher, unparsed = unparsed, exact_matches_only = exact_matches_only
            ),
            error = function(e) generate_error_row(i, genus_only, unique_data$genus, unique_data$species, higher)
          )
        }
      )
    }

    algaebase_df <- rbind(algaebase_df, tmp)
  }

  if (verbose) {close(pb)}

  # Replace blanks with NA to prepare for merge
  input_data$species[input_data$species == ""] <- NA

  # Merge results back to the original input
  final_results <- merge(input_data, algaebase_df, by = c("genus", "species"), all.x = TRUE)

  # Remove potential duplicates
  final_results <- distinct(final_results)

  return(final_results)
}
#' Algaebase Species Search
#'
#' This function searches the Algaebase API for species based on genus and species names.
#' It allows for flexible search parameters such as filtering by exact matches, returning
#' the most recent results, and including higher taxonomy details.
#'
#' @param genus A character string specifying the genus name.
#' @param species A character string specifying the species or specific epithet.
#' @param apikey A character string containing the API key for accessing the Algaebase API.
#' @param higher A logical value indicating whether to include higher taxonomy details (default is `TRUE`).
#' @param unparsed A logical value indicating whether to print the full JSON response from the API (default is `FALSE`).
#' @param newest_only A logical value indicating whether to return only the most recent entries (default is `TRUE`).
#' @param exact_matches_only A logical value indicating whether to return only exact matches (default is `TRUE`).
#'
#' @return A data frame with details about the species, including taxonomic status, ranks, and other relevant information.
#'
#' @details This function queries the Algaebase API for species based on the genus and species names,
#' and filters the results based on various parameters. The function handles different taxonomic ranks
#' and formats the output for easy use. It can merge higher taxonomy data if requested.
#'
#' @examples
#' \dontrun{
#' # Search for a species with exact matches only, return the most recent results
#' result <- get_algaebase_species(
#'   genus = "Skeletonema", species = "marinoi", apikey = "your_api_key"
#' )
#'
#' # Print result
#' print(result)
#' }
#'
#' @export
get_algaebase_species <- function(genus, species, apikey, higher = TRUE,
                                  unparsed = FALSE, newest_only = TRUE, exact_matches_only = TRUE) {

  # Validate inputs
  if (is.null(genus) || genus == "" || is.na(genus)) stop("Genus name is required.")
  if (is.null(species) || species == "" || is.na(species)) stop("Species name is required.")
  if (is.null(apikey) || apikey == "") stop("API key is required.")

  if (grepl(" ", species)) {
    species_split <- strsplit(species, split=' ')[[1]]
    sp <- species_split[1]

    infrasp <- species_split[2]

    species_query <- paste0("https://api.algaebase.org/v1.3/species?genus=",
                                  genus,"&dwc:specificEpithet=",sp,
                                  "&dwc:scientificName=",infrasp)
  } else {
    species_query <- paste0("https://api.algaebase.org/v1.3/species?genus=", genus,
                            "&dwc:specificEpithet=", species)
  }

  # Send GET request
  response <- GET(
    species_query,
    add_headers("Content-Type" = "application/json", "abapikey" = apikey)
  )
  if (response$status_code != 200) stop(paste0("Error ", response$status_code, ": Unable to fetch data from AlgaeBase"))

  # Parse JSON response
  results <- prettify(content(response, "text", encoding = "UTF-8"))
  if (unparsed) return(results)

  result_list <- fromJSON(results)
  results_output <- result_list[[2]]

  # Handle infraspecific names
  output_infraspname <- case_when(
    results_output$`dwc:taxonRank` == "forma" ~ results_output$infraspecificEpithet_forma,
    results_output$`dwc:taxonRank` == "variety" ~ results_output$infraspecificEpithet_variety,
    results_output$`dwc:taxonRank` == "subspecies" ~ results_output$infraspecificEpithet_subspecies,
    TRUE ~ ""
  )

  # Construct clean names and trim whitespace using base R
  output_clean_names <- trimws(paste(
    results_output$`dwc:genus`,
    results_output$`dwc:specificEpithet`,
    output_infraspname
  ))

  input_clean_name <- paste(genus, species)

  # Match indices
  output_match_indices <- output_clean_names == input_clean_name

  # Apply filters
  if (exact_matches_only) {
    if (!any(output_match_indices)) stop("No exact matches found.")
    results_output <- results_output[output_match_indices, ]
    output_match_indices <- TRUE
  }

  # Parse `mod_date` once
  mod_date <- ymd(extract_algaebase_field(results_output, "dcterms:modified"))

  # Extract higher taxonomy if requested
  if (higher) {
    genus_taxonomy <- get_algaebase_genus(genus, apikey)

    higher_taxonomy <- data.frame(
      kingdom = genus_taxonomy$kingdom,
      phylum = genus_taxonomy$phylum,
      class = genus_taxonomy$class,
      order = genus_taxonomy$order,
      family = genus_taxonomy$family,
      genus = genus_taxonomy$genus
    )
  }

  # Extract additional fields
  long_name <- extract_algaebase_field(results_output, "dwc:scientificName")
  taxonomic_status <- extract_algaebase_field(results_output, "dwc:taxonomicStatus")
  taxon_rank <- extract_algaebase_field(results_output, "dwc:taxonRank")
  authorship <- extract_algaebase_field(results_output, "dwc:scientificNameAuthorship")
  accepted_name <- extract_algaebase_field(results_output, "dwc:acceptedNameUsage")
  input_name <- paste(genus, species)
  input_match <- ifelse(
    paste(genus, species) == paste(
      extract_algaebase_field(results_output, "dwc:genus"),
      extract_algaebase_field(results_output, "dwc:specificEpithet")
    ), 1, 0
  )
  currently_accepted <- ifelse(taxonomic_status == "currently accepted taxonomically", 1, 0)
  accepted_name <- ifelse(currently_accepted == 1, input_name, accepted_name)

  # Create output data frame
  output <- data.frame(
    id = extract_algaebase_field(results_output, "dwc:scientificNameID"),
    genus = extract_algaebase_field(results_output, "dwc:genus"),
    species = extract_algaebase_field(results_output, "dwc:specificEpithet"),
    infrasp = extract_algaebase_field(results_output, "dwc:infraspecificEpithet"),
    taxonomic_status, currently_accepted, accepted_name,
    genus_only = 0, input_name, input_match,
    taxon_rank, mod_date, long_name, authorship
  )

  # Include higher taxonomy if requested
  if (higher) {
    output <- cbind(higher_taxonomy, output)
    output <- output[, c(
      'id', 'accepted_name', 'input_name', 'input_match', 'currently_accepted', 'genus_only',
      'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species', 'infrasp',
      'long_name', 'taxonomic_status', 'taxon_rank', 'mod_date', 'authorship'
    )]
  } else {
    output <- output[, c(
      'id', 'accepted_name', 'input_name', 'input_match', 'currently_accepted', 'genus_only',
      'genus', 'species', 'infrasp', 'long_name', 'taxonomic_status', 'taxon_rank', 'mod_date',
      'authorship'
    )]
  }

  if (newest_only) {
    output <- output[output$mod_date == max(output$mod_date), ]
  } else {
    output <- output[order(output$mod_date, decreasing = TRUE), ]
  }

  # Remove potential duplicated rows
  output <- distinct(output)

  return(output)
}
#' Search AlgaeBase for Information About a Genus of Algae
#'
#' This function searches the AlgaeBase API for genus information and returns detailed taxonomic data,
#' including higher taxonomy, taxonomic status, scientific names, and other related metadata.
#'
#' @param genus The genus name to search for (character string). This parameter is required.
#' @param apikey A valid API key for AlgaeBase (character string). This is required to authenticate API requests.
#' @param higher A boolean flag indicating whether to include higher taxonomy in the output (default is TRUE).
#' @param unparsed A boolean flag indicating whether to return the raw JSON output from the API (default is FALSE).
#' @param newest_only A boolean flag to return only the most recent entry (default is TRUE).
#' @param exact_matches_only A boolean flag to limit results to exact matches (default is TRUE).
#'
#' @return A data frame containing taxonomic data from AlgaeBase. Columns may include:
#'         - `id`: AlgaeBase ID.
#'         - `accepted_name`: Accepted scientific name (if different from input).
#'         - `input_name`: The genus name supplied by the user.
#'         - `input_match`: Whether the genus exactly matches (1 if exact, 0 if not).
#'         - `currently_accepted`: Whether the taxon is currently accepted (1=TRUE, 0=FALSE).
#'         - `genus_only`: Whether the search was for a genus only (1 for genus, 0 for genus + species).
#'         - `kingdom`, `phylum`, `class`, `order`, `family`: Higher taxonomy (if `higher` is TRUE).
#'         - `taxonomic_status`: Status of the taxon (currently accepted, synonym, unverified).
#'         - `taxon_rank`: The taxonomic rank of the accepted name (e.g., genus, species).
#'         - `mod_date`: Date when the entry was last modified.
#'         - `long_name`: Full scientific name including author and date (if available).
#'         - `authorship`: Author information (if available).
#'
#' @examples
#' \dontrun{
#'   get_algaebase_genus("Anabaena", apikey = "your_api_key")
#' }
#'
#' @export
get_algaebase_genus <- function(genus, apikey, higher = TRUE, unparsed = FALSE,
                                newest_only = TRUE, exact_matches_only = TRUE) {

  # Validate inputs
  if (is.null(genus) || genus == "" || is.na(genus)) stop("No genus name supplied")
  if (is.null(apikey) || apikey == "") stop("API key is required")

  # Construct the search URL
  genus_search_string <- paste0('https://api.algaebase.org/v1.3/genus?genus=', genus)

  # Send GET request
  response <- GET(
    genus_search_string,
    add_headers("Content-Type" = "application/json", "abapikey" = apikey)
  )

  if (response$status_code != 200) stop(paste0("Error ", response$status_code, ": Unable to fetch data from AlgaeBase"))

  # Parse JSON response
  results <- prettify(content(response, "text", encoding = "UTF-8"))
  if (unparsed) return(results)

  result_list <- fromJSON(results)
  results_output <- result_list[[2]]

  # Parse `mod_date` once
  mod_date <- ymd(extract_algaebase_field(results_output, "dcterms:modified"))

  # Handle higher taxonomy if requested
  if (higher) {
    higher_taxonomy <- data.frame(
      kingdom = extract_algaebase_field(results_output, "dwc:kingdom"),
      phylum = extract_algaebase_field(results_output, "dwc:phylum"),
      class = extract_algaebase_field(results_output, "dwc:class"),
      order = extract_algaebase_field(results_output, "dwc:order"),
      family = extract_algaebase_field(results_output, "dwc:family"),
      genus = extract_algaebase_field(results_output, "dwc:genus")
    )
  }

  # Extract additional fields
  long_name <- extract_algaebase_field(results_output, "dwc:scientificName")
  taxonomic_status <- extract_algaebase_field(results_output, "dwc:taxonomicStatus")
  taxon_rank <- extract_algaebase_field(results_output, "dwc:taxonRank")
  authorship <- extract_algaebase_field(results_output, "dwc:scientificNameAuthorship")
  accepted_name <- extract_algaebase_field(results_output, "dwc:acceptedNameUsage")
  input_match <- ifelse(genus == extract_algaebase_field(results_output, "dwc:genus"), 1, 0)
  currently_accepted <- ifelse(taxonomic_status == "currently accepted taxonomically", 1, 0)
  accepted_name <- ifelse(currently_accepted == 1, genus, accepted_name)

  # Create output data frame
  output <- data.frame(
    id = extract_algaebase_field(results_output, "dwc:scientificNameID"),
    genus = extract_algaebase_field(results_output, "dwc:genus"),
    species = NA, infrasp = NA,
    taxonomic_status, currently_accepted, accepted_name,
    genus_only = 1, input_name = genus, input_match,
    taxon_rank, mod_date, long_name, authorship
  )

  # Include higher taxonomy if requested
  if (higher) {
    output <- cbind(higher_taxonomy, output)
    output <- output[, c(
      'id', 'accepted_name', 'input_name', 'input_match', 'currently_accepted', 'genus_only',
      'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species', 'infrasp',
      'long_name', 'taxonomic_status', 'taxon_rank', 'mod_date', 'authorship'
    )]
  } else {
    output <- output[, c(
      'id', 'accepted_name', 'input_name', 'input_match', 'currently_accepted', 'genus_only',
      'genus', 'species', 'infrasp', 'long_name', 'taxonomic_status', 'taxon_rank', 'mod_date',
      'authorship'
    )]
  }

  # Apply filters
  if (exact_matches_only) {
    if (sum(output$input_match) == 0) stop("No exact matches found")
    output <- output[output$input_match == 1, ]
  }

  if (newest_only) {
    output <- output[output$mod_date == max(output$mod_date), ]
  } else {
    output <- output[order(output$mod_date, decreasing = TRUE), ]
  }

  # Remove potential duplicated rows
  output <- distinct(output)

  return(output)
}

#' Extract specific field from AlgaeBase query result
#'
#' This helper function extracts a specified field from the list object returned by an AlgaeBase query.
#' If the requested field is not present, it returns `NA` to indicate missing data.
#'
#' @param query_result A list object containing the output from an AlgaeBase query.
#' @param field_name A character string specifying the field name to extract from the query result.
#'
#' @return A character vector containing the values of the specified field, or `NA` if the field is not found.
#'
#' @examples
#' # Example output from an AlgaeBase query
#' query_result <- list(species = "Skeletonema marinoi", kingdom = "Chromista")
#' field_value <- SHARK4R:::extract_algaebase_field(query_result, "species")
#' print(field_value)
#'
#' @keywords internal
extract_algaebase_field <- function(query_result, field_name) {

  # Attempt to extract the field from the query result
  result <- query_result[[field_name]]

  # Return NA if the field is not found
  if (is.null(result)) {
    result <- NA
  }

  return(result)
}

#' Parse scientific names into genus and species components.
#'
#' This function processes a character vector of scientific names, splitting them into genus and species components.
#' It handles binomial names (e.g., "Homo sapiens"), removes undesired descriptors (e.g., 'Cfr.', 'cf.', 'sp.', 'spp.'),
#' and manages cases involving varieties, subspecies, or invalid species names. Special characters and whitespace are handled appropriately.
#'
#' @param scientific_name A character vector containing scientific names, which may include binomials, additional descriptors, or varieties.
#'
#' @return A `data.frame` with two columns:
#' - `genus`: Contains the genus names.
#' - `species`: Contains the species names (empty if unavailable or invalid).
#' Invalid descriptors like 'sp.', 'spp.', and numeric entries are excluded from the 'species' column.
#'
#' @examples
#' # Example with a vector of scientific names
#' scientific_names <- c("Skeletonema marinoi", "Cf. Azadinium perforatum", "Gymnodinium sp.",
#'                       "Melosira varians", "Aulacoseira islandica var. subarctica")
#' result <- parse_scientific_names(scientific_names)
#'
#' # Check the resulting data frame
#' print(result)
#'
#' @export
parse_scientific_names <- function(scientific_name) {
  # Ensure the input is a character vector
  spp_list <- as.character(scientific_name)

  # Convert to UTF-8 to handle special characters correctly
  spp_list <- iconv(spp_list, to = 'UTF-8')

  # Remove undesired descriptors like 'Cfr.', 'cf', 'colony', 'cells', etc.
  spp_list <- gsub('Cfr. |cf[.]? |colony|colonies|cells|cell', '', spp_list, ignore.case = TRUE)

  # Remove subspecies/variety descriptors (e.g., var., subsp., f., etc.)
  spp_list <- gsub('var[.]? |subsp[.]? |ssp[.]? |f[.]? |v[.]? |morph[.]? |gr[.]? |aff[.]? |tab[.]?', '', spp_list, ignore.case = TRUE)

  # Trim whitespace
  spp_list <- trimws(spp_list, 'both')

  # Split names into components
  components <- strsplit(spp_list, split = ' ')

  # Extract genus and combine the rest as species
  genus <- sapply(components, function(x) x[1])
  species <- sapply(components, function(x) {
    if (length(x) > 1) {
      paste(x[-1], collapse = ' ')
    } else {
      ''
    }
  })

  # Remove invalid species names (e.g., sp., spp.)
  species <- ifelse(species %in% c('sp.', 'spp.', 'sp', 'spp'), '', species)

  # Remove species names with numbers
  species[grepl('[0-9]', species)] <- ''

  # Ensure genus and species are valid
  genus[is.na(genus)] <- ''
  species[is.na(species)] <- ''

  # Trim any remaining whitespace
  genus <- trimws(genus, 'both')
  species <- trimws(species, 'both')

  # Create the output dataframe
  output_df <- data.frame(genus = genus, species = species, stringsAsFactors = FALSE)

  return(output_df)
}
