#' Search AlgaeBase for taxonomic information
#'
#' This function queries the AlgaeBase API to retrieve taxonomic information for a list of algae names based on genus and (optionally) species.
#'  It supports exact matching, genus-only searches, and retrieval of higher taxonomic ranks.
#'
#' @param genera A character vector of genus names.
#' @param species A character vector of species names corresponding to the `genera` vector. Must be the same length as `genera`.
#' @param subscription_key A character string containing the API key for accessing the AlgaeBase API. By default, the key
#'   is read from the environment variable \code{ALGAEBASE_KEY}.
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{match_algaebase_taxa("Skeletonema", "marinoi", subscription_key = "your_key_here")}
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(ALGAEBASE_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{ALGAEBASE_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param genus_only Logical. If `TRUE`, searches are based solely on the genus name, ignoring species. Defaults to `FALSE`.
#' @param higher Logical. If `TRUE`, includes higher taxonomy (e.g., kingdom, phylum) in the output. Defaults to `TRUE`.
#' @param unparsed Logical. If `TRUE`, returns raw JSON output instead of a `tibble`. Defaults to `FALSE`.
#' @param exact_matches_only Logical. If `TRUE`, restricts results to exact matches. Defaults to `TRUE`.
#' @param sleep_time Numeric. The delay (in seconds) between consecutive AlgaeBase API queries. Defaults to `1`. A delay is recommended to avoid overwhelming the API for large queries.
#' @param newest_only A logical value indicating whether to return only the most recent entries (default is `TRUE`).
#' @param verbose Logical. If `TRUE`, displays a progress bar to indicate query status. Defaults to `TRUE`.
#' @param apikey
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{subscription_key} instead.
#' @param genus
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{genera} instead.
#'
#' @return A `tibble` containing taxonomic information for each input genus–species combination.
#' The following columns may be included:
#' \itemize{
#'   \item \code{id} — AlgaeBase ID (if available).
#'   \item \code{kingdom}, \code{phylum}, \code{class}, \code{order}, \code{family} — Higher taxonomy (returned if \code{higher = TRUE}).
#'   \item \code{genus}, \code{species}, \code{infrasp} — Genus, species, and infraspecies names (if applicable).
#'   \item \code{taxonomic_status} — Status of the name (e.g., accepted, synonym, unverified).
#'   \item \code{currently_accepted} — Logical indicator whether the name is currently accepted (\code{TRUE}/\code{FALSE}).
#'   \item \code{accepted_name} — Currently accepted name if different from the input name.
#'   \item \code{input_name} — The name supplied by the user.
#'   \item \code{input_match} — Indicator of exact match (\code{1} = exact, \code{0} = not exact).
#'   \item \code{taxon_rank} — Taxonomic rank of the accepted name (e.g., genus, species).
#'   \item \code{mod_date} — Date when the entry was last modified in AlgaeBase.
#'   \item \code{long_name} — Full species name with authorship and date.
#'   \item \code{authorship} — Author(s) associated with the species name.
#' }
#'
#' @details
#'
#' A valid API key is requested from the AlgaeBase team.
#'
#' Scientific names can be parsed using the \code{parse_scientific_names()} function before being processed by \code{match_algaebase_taxa()}.
#'
#' Duplicate genus-species combinations are handled efficiently by querying each unique combination only once. Genus-only searches are performed when \code{genus_only = TRUE}
#' or when the species name is missing or invalid. Errors during API queries are gracefully handled by returning rows with \code{NA} values for missing or unavailable data.
#'
#' The function allows for integration with data analysis workflows that require resolving or verifying taxonomic names against AlgaeBase.
#'
#' @seealso \url{https://www.algaebase.org/} for AlgaeBase website.
#' @seealso \code{\link{parse_scientific_names}} for parsing taxonomic names before passing them to the function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with genus and species vectors
#' genus_vec <- c("Thalassiosira", "Skeletonema", "Tripos")
#' species_vec <- c("pseudonana", "costatum", "furca")
#'
#' algaebase_results <- match_algaebase_taxa(
#'   genera = genus_vec,
#'   species = species_vec,
#'   subscription_key = "your_api_key",
#'   exact_matches_only = TRUE,
#'   verbose = TRUE
#' )
#' head(algaebase_results)
#' }
match_algaebase_taxa <- function(genera, species, subscription_key = Sys.getenv("ALGAEBASE_KEY"), genus_only = FALSE,
                                 higher = TRUE, unparsed = FALSE, exact_matches_only = TRUE,
                                 sleep_time = 1, newest_only = TRUE, verbose = TRUE,
                                 apikey = deprecated(), genus = deprecated()) {

  if (is_present(genus)) {
    lifecycle::deprecate_warn("1.0.0", "match_algaebase_taxa(genus = )", "match_algaebase_taxa(genera = )",
                              "match_algaebase_taxa() handles multiple 'genus' inputs")
    genera <- genus
  }

  # Check input lengths
  if (length(genera) != length(species)) {
    stop("`genera` and `species` vectors must be of equal length.")
  }

  # Check for deprecated 'apikey' argument
  if (is_present(apikey)) {
    # Signal to the user that the `apikey` argument is deprecated
    lifecycle::deprecate_warn("0.1.7.9000", "SHARK4R::match_algaebase_taxa(apikey = )",
                              "SHARK4R::match_algaebase_taxa(subscription_key = )")

    subscription_key <- apikey
  }

  if (is.null(subscription_key) || subscription_key == "") {
    stop("No AlgaeBase subscription key provided. See ?match_algaebase_taxa for setup instructions.")
  }

  # Check if API is operational
  if (!check_algaebase_api(subscription_key)) {
    stop("API is not operational or the API key is invalid. Please check and try again.")
  }

  # Create unique combinations of genera and species
  input_data <- tibble(genus = genera, species = species)
  unique_data <- unique(input_data)

  # Prepare output dataframe
  algaebase_df <- tibble()

  # Helper function to generate an error row
  generate_error_row <- function(index, genus_only, genus, species, higher) {
    input_name <- if (genus_only) {
      genus[index]
    } else {
      trimws(paste(genus[index], species[index]))
    }

    err_df <- tibble(
      id = NA, kingdom = NA, phylum = NA, class = NA, order = NA, family = NA,
      genus = NA, species = NA, infrasp = NA, taxonomic_status = NA, nomenclatural_status = NA,
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
  if (verbose) {pb <- utils::txtProgressBar(min = 0, max = nrow(unique_data), style = 3)}

  # Main loop over unique combinations
  for (i in seq_len(nrow(unique_data))) {
    Sys.sleep(sleep_time)

    # Update progress bar
    if (verbose) {utils::setTxtProgressBar(pb, i)}

    genus_i <- unique_data$genus[i]
    species_i <- unique_data$species[i]

    if (is.na(genus_i) || genus_i == "") {
      tmp <- generate_error_row(i, genus_only, unique_data$genus, unique_data$species, higher)
    } else if (genus_only || is.na(species_i) || species_i == "") {
      tmp <- tryCatch(
        match_algaebase_genus(
          genus = genus_i, subscription_key = subscription_key, higher = higher,
          unparsed = unparsed, exact_matches_only = exact_matches_only, newest_only = newest_only
        ),
        error = function(e) generate_error_row(i, genus_only, unique_data$genus, unique_data$species, higher)
      )
    } else {
      tmp <- tryCatch(
        match_algaebase_species(
          genus = genus_i, species = species_i, subscription_key = subscription_key,
          higher = higher, unparsed = unparsed, exact_matches_only = exact_matches_only, newest_only = newest_only
        ),
        error = function(e) {
          tryCatch(
            match_algaebase_genus(
              genus = genus_i, subscription_key = subscription_key, higher = higher,
              unparsed = unparsed, exact_matches_only = exact_matches_only, newest_only = newest_only
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
  final_results <- input_data %>%
    left_join(algaebase_df, by = c("genus", "species"))

  # Remove potential duplicates
  final_results <- distinct(final_results)

  return(final_results)
}

#' Search AlgaeBase for taxonomic information
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated. Users are encouraged to use \code{\link{match_algaebase_taxa}} instead.
#'
#' This function queries the AlgaeBase API to retrieve taxonomic information for a list of algae names based on genus and (optionally) species.
#'  It supports exact matching, genus-only searches, and retrieval of higher taxonomic ranks.
#'
#' @param genus A character vector of genus names.
#' @param species A character vector of species names corresponding to the `genus` vector. Must be the same length as `genus`.
#' @param subscription_key A character string containing the API key for accessing the AlgaeBase API. By default, the key
#'   is read from the environment variable \code{ALGAEBASE_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{match_algaebase("Skeletonema", "marinoi", subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(ALGAEBASE_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{ALGAEBASE_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param genus_only Logical. If `TRUE`, searches are based solely on the genus name, ignoring species. Defaults to `FALSE`.
#' @param higher Logical. If `TRUE`, includes higher taxonomy (e.g., kingdom, phylum) in the output. Defaults to `TRUE`.
#' @param unparsed Logical. If `TRUE`, returns raw JSON output instead of a `tibble`. Defaults to `FALSE`.
#' @param exact_matches_only Logical. If `TRUE`, restricts results to exact matches. Defaults to `TRUE`.
#' @param sleep_time Numeric. The delay (in seconds) between consecutive AlgaeBase API queries. Defaults to `1`. A delay is recommended to avoid overwhelming the API for large queries.
#' @param newest_only A logical value indicating whether to return only the most recent entries (default is `TRUE`).
#' @param verbose Logical. If `TRUE`, displays a progress bar to indicate query status. Defaults to `TRUE`.
#' @param apikey
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{subscription_key} instead.
#'
#' @return A `tibble` containing taxonomic information for each input genus–species combination.
#' The following columns may be included:
#' \itemize{
#'   \item \code{id} — AlgaeBase ID (if available).
#'   \item \code{kingdom}, \code{phylum}, \code{class}, \code{order}, \code{family} — Higher taxonomy (returned if \code{higher = TRUE}).
#'   \item \code{genus}, \code{species}, \code{infrasp} — Genus, species, and infraspecies names (if applicable).
#'   \item \code{taxonomic_status} — Status of the name (e.g., accepted, synonym, unverified).
#'   \item \code{currently_accepted} — Logical indicator whether the name is currently accepted (\code{TRUE}/\code{FALSE}).
#'   \item \code{accepted_name} — Currently accepted name if different from the input name.
#'   \item \code{input_name} — The name supplied by the user.
#'   \item \code{input_match} — Indicator of exact match (\code{1} = exact, \code{0} = not exact).
#'   \item \code{taxon_rank} — Taxonomic rank of the accepted name (e.g., genus, species).
#'   \item \code{mod_date} — Date when the entry was last modified in AlgaeBase.
#'   \item \code{long_name} — Full species name with authorship and date.
#'   \item \code{authorship} — Author(s) associated with the species name.
#' }
#'
#' @details
#'
#' A valid API key is requested from the AlgaeBase team.
#'
#' Scientific names can be parsed using the \code{parse_scientific_names()} function before being processed by \code{match_algaebase()}.
#'
#' Duplicate genus-species combinations are handled efficiently by querying each unique combination only once. Genus-only searches are performed when \code{genus_only = TRUE}
#' or when the species name is missing or invalid. Errors during API queries are gracefully handled by returning rows with \code{NA} values for missing or unavailable data.
#'
#' The function allows for integration with data analysis workflows that require resolving or verifying taxonomic names against AlgaeBase.
#'
#' @seealso \url{https://www.algaebase.org/} for AlgaeBase website.
#' @seealso \code{\link{parse_scientific_names}}
#'
#' @keywords internal
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
#'   subscription_key = "your_api_key",
#'   exact_matches_only = TRUE,
#'   verbose = TRUE
#' )
#' head(algaebase_results)
#' }
match_algaebase <- function(genus, species, subscription_key = Sys.getenv("ALGAEBASE_KEY"), genus_only = FALSE,
                            higher = TRUE, unparsed = FALSE, exact_matches_only = TRUE,
                            sleep_time = 1, newest_only = TRUE, verbose = TRUE, apikey = deprecated()) {

  lifecycle::deprecate_warn("0.1.7.9000", "match_algaebase()", "match_algaebase_taxa()")

  match_algaebase_taxa(genera = genus,
                       species = species,
                       subscription_key = subscription_key,
                       genus_only = genus_only,
                       higher = higher,
                       unparsed = unparsed,
                       exact_matches_only = exact_matches_only,
                       sleep_time = sleep_time,
                       newest_only = newest_only,
                       verbose = verbose,
                       apikey = apikey)
}
#' Search AlgaeBase for information about a species of algae
#'
#' This function searches the AlgaeBase API for species based on genus and species names.
#' It allows for flexible search parameters such as filtering by exact matches, returning
#' the most recent results, and including higher taxonomy details.
#'
#' @param genus A character string specifying the genus name.
#' @param species A character string specifying the species or specific epithet.
#' @param subscription_key A character string containing the API key for accessing the AlgaeBase API. By default, the key
#'   is read from the environment variable \code{ALGAEBASE_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{match_algaebase_species("Skeletonema", "marinoi", subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(ALGAEBASE_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{ALGAEBASE_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param higher A logical value indicating whether to include higher taxonomy details (default is `TRUE`).
#' @param unparsed A logical value indicating whether to print the full JSON response from the API (default is `FALSE`).
#' @param newest_only A logical value indicating whether to return only the most recent entries (default is `TRUE`).
#' @param exact_matches_only A logical value indicating whether to return only exact matches (default is `TRUE`).
#' @param apikey
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{subscription_key} instead.
#'
#' @return A `tibble` with details about the species, including:
#' \itemize{
#'   \item \code{taxonomic_status} — The current status of the taxon (e.g., accepted, synonym, unverified).
#'   \item \code{taxon_rank} — The rank of the taxon (e.g., species, genus).
#'   \item \code{accepted_name} — The currently accepted scientific name, if applicable.
#'   \item \code{authorship} — Author information for the scientific name (if available).
#'   \item \code{mod_date} — Date when the taxonomic record was last modified.
#'   \item \code{...} — Other relevant information returned by the data source.
#' }
#'
#' @details
#' A valid API key is requested from the AlgaeBase team.
#'
#' This function queries the AlgaeBase API for species based on the genus and species names,
#' and filters the results based on various parameters. The function handles different taxonomic ranks
#' and formats the output for easy use. It can merge higher taxonomy data if requested.
#'
#' @seealso \url{https://www.algaebase.org/} for AlgaeBase website.
#'
#' @examples
#' \dontrun{
#' # Search for a species with exact matches only, return the most recent results
#' result <- match_algaebase_species(
#'   genus = "Skeletonema", species = "marinoi", subscription_key = "your_api_key"
#' )
#'
#' # Print result
#' print(result)
#' }
#'
#' @export
match_algaebase_species <- function(genus, species, subscription_key = Sys.getenv("ALGAEBASE_KEY"), higher = TRUE,
                                    unparsed = FALSE, newest_only = TRUE, exact_matches_only = TRUE,
                                    apikey = deprecated()) {

  # Check for deprecated 'apikey' argument
  if (is_present(apikey)) {
    # Signal to the user that the `apikey` argument is deprecated
    lifecycle::deprecate_warn("0.1.7.9000", "SHARK4R::match_algaebase_species(apikey = )",
                              "SHARK4R::match_algaebase_species(subscription_key = )")

    subscription_key <- apikey
  }

  # Ensure that genus and species each contain exactly one value
  if (length(genus) != 1) stop("The 'genus' argument must have length 1.")
  if (length(species) != 1) stop("The 'species' argument must have length 1.")

  # Validate inputs
  if (is.null(genus) || genus == "" || is.na(genus)) stop("Genus name is required.")
  if (is.null(species) || species == "" || is.na(species)) stop("Species name is required.")
  if (is.null(subscription_key) || subscription_key == "") stop("API key is required.")

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

  # Initialize pagination variables
  offset <- 0
  count <- 50
  combined_results <- list()  # Use a list to store pages temporarily
  total_retrieved <- 0        # Track the total number of results retrieved
  total_number_of_results <- Inf  # Initialize as infinite until the first response

  repeat {
    # Build the URL with offset and count
    query_url <- paste0(species_query, "&offset=", offset, "&count=", count)

    # Send GET request
    response <- GET(
      query_url,
      add_headers("Content-Type" = "application/json", "abapikey" = subscription_key)
    )

    # Check for response errors
    if (response$status_code != 200) stop(paste0("Error ", response$status_code, ": Unable to fetch data from AlgaeBase"))

    # Parse the response
    results <- fromJSON(content(response, "text", encoding = "UTF-8"))
    results_page <- results[[2]]

    # Get total number of results from the first response
    if (is.infinite(total_number_of_results)) {
      total_number_of_results <- results[[1]]$`_total_number_of_results`
    }

    # Break the loop if no more results are returned
    if (nrow(results_page) == 0) break

    # Append the results page to the list
    combined_results[[length(combined_results) + 1]] <- results_page

    # Update the total number of results retrieved
    total_retrieved <- total_retrieved + nrow(results_page)

    # Break the loop if all results have been retrieved
    if (total_retrieved >= total_number_of_results) break

    # Update offset for the next request
    offset <- offset + count

    # Pause between requests to avoid hitting rate limits
    Sys.sleep(1)
  }

  # Combine all results into a single data frame
  results_output <- bind_rows(combined_results)

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
  mod_date <- as.Date(extract_algaebase_field(results_output, "dcterms:modified"))

  # Extract higher taxonomy if requested
  if (higher) {
    genus_taxonomy <- match_algaebase_genus(genus, subscription_key)

    higher_taxonomy <- tibble(
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
  nomenclatural_status <- extract_algaebase_field(results_output, "dwc:nomenclaturalStatus")

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
  output <- tibble(
    id = extract_algaebase_field(results_output, "dwc:scientificNameID"),
    genus = extract_algaebase_field(results_output, "dwc:genus"),
    species = extract_algaebase_field(results_output, "dwc:specificEpithet"),
    infrasp = extract_algaebase_field(results_output, "dwc:infraspecificEpithet"),
    taxonomic_status, nomenclatural_status, currently_accepted, accepted_name,
    genus_only = 0, input_name, input_match,
    taxon_rank, mod_date, long_name, authorship
  )

  # Include higher taxonomy if requested
  if (higher) {
    output <- cbind(higher_taxonomy, output)
    output <- output[, c(
      'id', 'accepted_name', 'input_name', 'input_match', 'currently_accepted', 'genus_only',
      'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species', 'infrasp',
      'long_name', 'taxonomic_status', 'nomenclatural_status', 'taxon_rank', 'mod_date', 'authorship'
    )]
  } else {
    output <- output[, c(
      'id', 'accepted_name', 'input_name', 'input_match', 'currently_accepted', 'genus_only',
      'genus', 'species', 'infrasp', 'long_name', 'taxonomic_status', 'nomenclatural_status', 'taxon_rank', 'mod_date',
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

  return(tibble(output))
}
#' Search AlgaeBase for information about a species of algae
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated. Users are encouraged to use \code{\link{match_algaebase_species}} instead.
#'
#' This function searches the AlgaeBase API for species based on genus and species names.
#' It allows for flexible search parameters such as filtering by exact matches, returning
#' the most recent results, and including higher taxonomy details.
#'
#' @param genus A character string specifying the genus name.
#' @param species A character string specifying the species or specific epithet.
#' @param subscription_key A character string containing the API key for accessing the AlgaeBase API. By default, the key
#'   is read from the environment variable \code{ALGAEBASE_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{get_algaebase_species("Skeletonema", "marinoi", subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(ALGAEBASE_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{ALGAEBASE_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param higher A logical value indicating whether to include higher taxonomy details (default is `TRUE`).
#' @param unparsed A logical value indicating whether to print the full JSON response from the API (default is `FALSE`).
#' @param newest_only A logical value indicating whether to return only the most recent entries (default is `TRUE`).
#' @param exact_matches_only A logical value indicating whether to return only exact matches (default is `TRUE`).
#' @param apikey
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{subscription_key} instead.
#'
#' @return A `tibble` with details about the species, including:
#' \itemize{
#'   \item \code{taxonomic_status} — The current status of the taxon (e.g., accepted, synonym, unverified).
#'   \item \code{taxon_rank} — The rank of the taxon (e.g., species, genus).
#'   \item \code{accepted_name} — The currently accepted scientific name, if applicable.
#'   \item \code{authorship} — Author information for the scientific name (if available).
#'   \item \code{mod_date} — Date when the taxonomic record was last modified.
#'   \item \code{...} — Other relevant information returned by the data source.
#' }
#'
#' @details
#' A valid API key is requested from the AlgaeBase team.
#'
#' This function queries the AlgaeBase API for species based on the genus and species names,
#' and filters the results based on various parameters. The function handles different taxonomic ranks
#' and formats the output for easy use. It can merge higher taxonomy data if requested.
#'
#' @seealso \url{https://www.algaebase.org/} for AlgaeBase website.
#'
#' @examples
#' \dontrun{
#' # Search for a species with exact matches only, return the most recent results
#' result <- get_algaebase_species(
#'   genus = "Skeletonema", species = "marinoi", subscription_key = "your_api_key"
#' )
#'
#' # Print result
#' print(result)
#' }
#'
#' @keywords internal
#' @export
get_algaebase_species <- function(genus, species, subscription_key = Sys.getenv("ALGAEBASE_KEY"), higher = TRUE,
                                  unparsed = FALSE, newest_only = TRUE, exact_matches_only = TRUE,
                                  apikey = deprecated()) {

  lifecycle::deprecate_warn("0.1.7.9000", "get_algaebase_species()", "match_algaebase_species()")

  match_algaebase_species(genus = genus,
                          species = species,
                          subscription_key = subscription_key,
                          higher = higher,
                          unparsed = unparsed,
                          newest_only = newest_only,
                          exact_matches_only = exact_matches_only,
                          apikey = apikey)
}
#' Search AlgaeBase for information about a genus of algae
#'
#' This function searches the AlgaeBase API for genus information and returns detailed taxonomic data,
#' including higher taxonomy, taxonomic status, scientific names, and other related metadata.
#'
#' @param genus The genus name to search for (character string). This parameter is required.
#' @param subscription_key A character string containing the API key for accessing the AlgaeBase API. By default, the key
#'   is read from the environment variable \code{ALGAEBASE_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{match_algaebase_genus("Skeletonema", subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(ALGAEBASE_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{ALGAEBASE_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param higher A boolean flag indicating whether to include higher taxonomy in the output (default is TRUE).
#' @param unparsed A boolean flag indicating whether to return the raw JSON output from the API (default is FALSE).
#' @param newest_only A boolean flag to return only the most recent entry (default is TRUE).
#' @param exact_matches_only A boolean flag to limit results to exact matches (default is TRUE).
#' @param apikey
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{subscription_key} instead.
#'
#' @details
#' A valid API key is requested from the AlgaeBase team.
#'
#' @return A `tibble` with the following columns:
#' \itemize{
#'   \item \code{id} — AlgaeBase identifier.
#'   \item \code{accepted_name} — Accepted scientific name (if different from the input).
#'   \item \code{input_name} — The genus name supplied by the user.
#'   \item \code{input_match} — Indicator of exact match (\code{1} = exact, \code{0} = not exact).
#'   \item \code{currently_accepted} — Indicator if the taxon is currently accepted (\code{1} = TRUE, \code{0} = FALSE).
#'   \item \code{genus_only} — Indicator if the search was for a genus only (\code{1} = genus, \code{0} = genus + species).
#'   \item \code{kingdom}, \code{phylum}, \code{class}, \code{order}, \code{family} — Higher taxonomy (returned if \code{higher = TRUE}).
#'   \item \code{taxonomic_status} — Status of the taxon (e.g., currently accepted, synonym, unverified).
#'   \item \code{taxon_rank} — Taxonomic rank of the accepted name (e.g., genus, species).
#'   \item \code{mod_date} — Date when the entry was last modified.
#'   \item \code{long_name} — Full scientific name including author and date (if available).
#'   \item \code{authorship} — Author information (if available).
#' }
#'
#' @seealso \url{https://www.algaebase.org/} for AlgaeBase website.
#'
#' @examples
#' \dontrun{
#'   match_algaebase_genus("Anabaena", subscription_key = "your_api_key")
#' }
#'
#' @export
match_algaebase_genus <- function(genus, subscription_key = Sys.getenv("ALGAEBASE_KEY"),
                                  higher = TRUE, unparsed = FALSE,
                                  newest_only = TRUE, exact_matches_only = TRUE,
                                  apikey = deprecated()) {

  # Check for deprecated 'apikey' argument
  if (is_present(apikey)) {
    # Signal to the user that the `apikey` argument is deprecated
    lifecycle::deprecate_warn("0.1.7.9000", "SHARK4R::match_algaebase_genus(apikey = )",
                              "SHARK4R::match_algaebase_genus(subscription_key = )")

    subscription_key <- apikey
  }

  # Ensure that genus and species each contain exactly one value
  if (length(genus) != 1) stop("The 'genus' argument must have length 1.")

  # Validate inputs
  if (is.null(genus) || genus == "" || is.na(genus)) stop("No genus name supplied")
  if (is.null(subscription_key) || subscription_key == "") stop("API key is required")

  # Base search URL
  genus_search_string <- paste0('https://api.algaebase.org/v1.3/genus?genus=', genus)

  # Initialize pagination variables
  offset <- 0
  count <- 50
  combined_results <- list()  # Use a list to store pages temporarily
  total_retrieved <- 0        # Track the total number of results retrieved
  total_number_of_results <- Inf  # Initialize as infinite until the first response

  repeat {
    # Build the URL with offset and count
    query_url <- paste0(genus_search_string, "&offset=", offset, "&count=", count)

    # Send GET request
    response <- GET(
      query_url,
      add_headers("Content-Type" = "application/json", "abapikey" = subscription_key)
    )

    # Check for response errors
    if (response$status_code != 200) stop(paste0("Error ", response$status_code, ": Unable to fetch data from AlgaeBase"))

    # Parse the response
    results <- fromJSON(content(response, "text", encoding = "UTF-8"))
    results_page <- results[[2]]

    # Get total number of results from the first response
    if (is.infinite(total_number_of_results)) {
      total_number_of_results <- results[[1]]$`_total_number_of_results`
    }

    # Break the loop if no more results are returned
    if (nrow(results_page) == 0) break

    # Append the results page to the list
    combined_results[[length(combined_results) + 1]] <- results_page

    # Update the total number of results retrieved
    total_retrieved <- total_retrieved + nrow(results_page)

    # Break the loop if all results have been retrieved
    if (total_retrieved >= total_number_of_results) break

    # Update offset for the next request
    offset <- offset + count

    # Pause between requests to avoid hitting rate limits
    Sys.sleep(1)
  }

  # Combine all results into a single data frame
  combined_results <- bind_rows(combined_results)

  if (unparsed) return(combined_results)

  # Parse `mod_date` once
  mod_date <- as.Date(combined_results$`dcterms:modified`)

  if (higher) {
    higher_taxonomy <- combined_results[, c("dwc:kingdom", "dwc:phylum", "dwc:class", "dwc:order", "dwc:family")]

    # Remove 'dwc:' prefix from column names
    colnames(higher_taxonomy) <- gsub("^dwc:", "", colnames(higher_taxonomy))
  }

  output <- tibble(
    id = combined_results$`dwc:scientificNameID`,
    genus = combined_results$`dwc:genus`,
    species = NA, infrasp = NA,
    taxonomic_status = combined_results$`dwc:taxonomicStatus`,
    nomenclatural_status = combined_results$`dwc:nomenclaturalStatus`,
    currently_accepted = ifelse(combined_results$`dwc:taxonomicStatus` == "currently accepted taxonomically", 1, 0),
    accepted_name = combined_results$`dwc:acceptedNameUsage`,
    genus_only = 1,
    input_name = genus,
    input_match = ifelse(genus == combined_results$`dwc:genus`, 1, 0),
    taxon_rank = combined_results$`dwc:taxonRank`,
    mod_date = mod_date,
    long_name = combined_results$`dwc:scientificName`,
    authorship = combined_results$`dwc:scientificNameAuthorship`
  )

  if (higher) {
    output <- cbind(higher_taxonomy, output)
  }

  if (exact_matches_only) {
    if (sum(output$input_match) == 0) stop("No exact matches found")
    output <- output[output$input_match == 1, ]
  }

  if (newest_only) {
    output <- output[output$mod_date == max(output$mod_date), ]
  } else {
    output <- output[order(output$mod_date, decreasing = TRUE), ]
  }

  # Remove duplicates
  output <- distinct(output)

  return(tibble(output))
}

#' Search AlgaeBase for information about a genus of algae
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated. Users are encouraged to use \code{\link{match_algaebase_species}} instead.
#'
#' This function searches the AlgaeBase API for genus information and returns detailed taxonomic data,
#' including higher taxonomy, taxonomic status, scientific names, and other related metadata.
#'
#' @param genus The genus name to search for (character string). This parameter is required.
#' @param subscription_key A character string containing the API key for accessing the AlgaeBase API. By default, the key
#'   is read from the environment variable \code{ALGAEBASE_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{get_algaebase_genus("Skeletonema", subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(ALGAEBASE_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{ALGAEBASE_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param higher A boolean flag indicating whether to include higher taxonomy in the output (default is TRUE).
#' @param unparsed A boolean flag indicating whether to return the raw JSON output from the API (default is FALSE).
#' @param newest_only A boolean flag to return only the most recent entry (default is TRUE).
#' @param exact_matches_only A boolean flag to limit results to exact matches (default is TRUE).
#' @param apikey
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{subscription_key} instead.
#'
#' @details
#' A valid API key is requested from the AlgaeBase team.
#'
#' @return A `tibble` with the following columns:
#' \itemize{
#'   \item \code{id} — AlgaeBase identifier.
#'   \item \code{accepted_name} — Accepted scientific name (if different from the input).
#'   \item \code{input_name} — The genus name supplied by the user.
#'   \item \code{input_match} — Indicator of exact match (\code{1} = exact, \code{0} = not exact).
#'   \item \code{currently_accepted} — Indicator if the taxon is currently accepted (\code{1} = TRUE, \code{0} = FALSE).
#'   \item \code{genus_only} — Indicator if the search was for a genus only (\code{1} = genus, \code{0} = genus + species).
#'   \item \code{kingdom}, \code{phylum}, \code{class}, \code{order}, \code{family} — Higher taxonomy (returned if \code{higher = TRUE}).
#'   \item \code{taxonomic_status} — Status of the taxon (e.g., currently accepted, synonym, unverified).
#'   \item \code{taxon_rank} — Taxonomic rank of the accepted name (e.g., genus, species).
#'   \item \code{mod_date} — Date when the entry was last modified.
#'   \item \code{long_name} — Full scientific name including author and date (if available).
#'   \item \code{authorship} — Author information (if available).
#' }
#'
#' @seealso \url{https://www.algaebase.org/} for AlgaeBase website.
#'
#' @examples
#' \dontrun{
#'   get_algaebase_genus("Anabaena", subscription_key = "your_api_key")
#' }
#'
#' @keywords internal
#' @export
get_algaebase_genus <- function(genus, subscription_key = Sys.getenv("ALGAEBASE_KEY"),
                                higher = TRUE, unparsed = FALSE,
                                newest_only = TRUE, exact_matches_only = TRUE,
                                apikey = deprecated()) {

  lifecycle::deprecate_warn("0.1.7.9000", "get_algaebase_genus()", "match_algaebase_genus()")

  match_algaebase_genus(genus = genus,
                        subscription_key = subscription_key,
                        higher = higher,
                        unparsed = unparsed,
                        newest_only = newest_only,
                        exact_matches_only = exact_matches_only,
                        apikey = apikey)
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
#' @seealso \url{https://www.algaebase.org/} for AlgaeBase website.
#'
#' @examples
#' # Example output from an AlgaeBase query
#' query_result <- list(species = "Skeletonema marinoi", kingdom = "Chromista")
#' field_value <- SHARK4R:::extract_algaebase_field(query_result, "species")
#' print(field_value)
#'
#' @noRd
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
#' @param scientific_names A character vector containing scientific names, which may include binomials, additional descriptors, or varieties.
#' @param remove_undesired_descriptors Logical, if TRUE, undesired descriptors (e.g., 'Cfr.', 'cf.', 'colony', 'cells', etc.) are removed. Default is TRUE.
#' @param remove_subspecies Logical, if TRUE, subspecies/variety descriptors (e.g., 'var.', 'subsp.', 'f.', etc.) are removed. Default is TRUE.
#' @param remove_invalid_species Logical, if TRUE, invalid species names (e.g., 'sp.', 'spp.') are removed. Default is TRUE.
#' @param encoding A string specifying the encoding to be used for the input names (e.g., 'UTF-8'). Default is 'UTF-8'.
#' @param scientific_name
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{scientific_names} instead.
#'
#' @return A `tibble` with two columns:
#' \itemize{
#'   \item \code{genus} — Genus names.
#'   \item \code{species} — Species names (empty if unavailable or invalid).
#'         Invalid descriptors such as \code{"sp."}, \code{"spp."}, and numeric entries
#'         are excluded from this column.
#' }
#'
#' @seealso \url{https://www.algaebase.org/} for AlgaeBase website.
#'
#' @examples
#' # Example with a vector of scientific names
#' scientific_names <- c("Skeletonema marinoi", "Cf. Azadinium perforatum", "Gymnodinium sp.",
#'                       "Melosira varians", "Aulacoseira islandica var. subarctica")
#'
#' # Parse names
#' result <- parse_scientific_names(scientific_names)
#'
#' # Check the resulting data
#' print(result)
#'
#' @export
parse_scientific_names <- function(scientific_names,
                                   remove_undesired_descriptors = TRUE,
                                   remove_subspecies = TRUE,
                                   remove_invalid_species = TRUE,
                                   encoding = 'UTF-8',
                                   scientific_name = deprecated()) {

  if (is_present(scientific_name)) {

    # Signal the deprecation to the user
    lifecycle::deprecate_warn("1.0.0", "SHARK4R::parse_scientific_names(scientific_name = )", "SHARK4R::parse_scientific_names(scientific_names = )",
                              "parse_scientific_names() handles multiple 'scientific_name' inputs")

    # Deal with the deprecated argument for compatibility
    scientific_names <- scientific_name
  }

  # Ensure the input is a character vector
  spp_list <- as.character(scientific_names)

  # Convert to specified encoding to handle special characters correctly
  spp_list <- iconv(spp_list, to = encoding)

  # Remove undesired descriptors like 'Cfr.', 'cf', 'colony', 'cells', etc.
  if (remove_undesired_descriptors) {
    spp_list <- gsub('\\b(Cfr[.]?|cf[.]?|GRP[.]?|CPX[.]?|CF[.]?|colony|colonies|cells|cell)\\b', '', spp_list, ignore.case = TRUE)
  }

  # Remove subspecies/variety descriptors (e.g., var., subsp., f., etc.)
  if (remove_subspecies) {
    spp_list <- gsub('\\b(var[.]?|subsp[.]?|ssp[.]?|f[.]?|v[.]?|morph[.]?|gr[.]?|aff[.]?|tab[.]?)\\b', '', spp_list, ignore.case = TRUE)
  }

  # Trim whitespace
  spp_list <- trimws(spp_list, 'both')

  # Remove any remaining standalone punctuation or stray dots
  spp_list <- gsub('[[:punct:]&&[^-]]', '', spp_list)

  # Remove standalone periods (including trailing ones)
  spp_list <- gsub('\\s+\\.+|\\.+\\s+|\\.+$', '', spp_list)

  # Replace multiple spaces with a single space
  spp_list <- gsub('\\s+', ' ', spp_list)

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
  if (remove_invalid_species) {
    species <- ifelse(species %in% c('sp.', 'spp.', 'sp', 'spp', 'SP.', 'SP', "SPP.", "SPP"), '', species)
  }

  # Remove species names with numbers
  species[grepl('[0-9]', species)] <- ''

  # Ensure genus and species are valid
  genus[is.na(genus)] <- ''
  species[is.na(species)] <- ''

  # Trim any remaining whitespace
  genus <- trimws(genus, 'both')
  species <- trimws(species, 'both')

  # Create the output dataframe
  output_df <- tibble(genus = genus, species = species)

  return(output_df)
}

#' Check AlgaeBase API operational status
#'
#' Internal function to verify whether the AlgaeBase API is operational.
#' It sends a request to a stable genus endpoint to confirm API availability.
#'
#' @param subscription_key A character string containing the API key for accessing the AlgaeBase API. By default, the key
#'   is read from the environment variable \code{ALGAEBASE_KEY}.
#'
#'   You can provide the key in three ways:
#'   \itemize{
#'     \item **Directly as a parameter**:
#'       \code{check_algaebase_api(subscription_key = "your_key_here")}.
#'     \item **Temporarily for the session**:
#'       \code{Sys.setenv(ALGAEBASE_KEY = "your_key_here")}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'     \item **Permanently across sessions** by adding it to your \code{~/.Renviron} file.
#'       Use \code{usethis::edit_r_environ()} to open the file, then add:
#'       \code{ALGAEBASE_KEY=your_key_here}.
#'       After this, you do not need to pass `subscription_key` to the function.
#'   }
#' @param genus_id A numeric value. The unique genus ID used to test the API endpoint.
#' Default is `43375`, corresponding to the `Haematococcus` genus record in AlgaeBase.
#' @param apikey
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{subscription_key} instead.
#'
#' @return A logical value: `TRUE` if the API is operational, `FALSE` otherwise.
#'
#' @details
#' A valid API key is requested from the AlgaeBase team.
#'
#' This function performs a GET request to the AlgaeBase API using a stable genus ID
#' to ensure that the API is accessible and that the provided API key is valid.
#' It is used internally to prevent unnecessary queries when the API is unavailable.
#'
#' @seealso \url{https://www.algaebase.org/} for AlgaeBase website.
#'
#' @examples
#' \dontrun{
#' # Check API status with an API key
#' check_algaebase_api(subscription_key = "your_api_key")
#' }
#'
#' @noRd
#'
#' @keywords internal
check_algaebase_api <- function(subscription_key = Sys.getenv("ALGAEBASE_KEY"), genus_id = 43375, apikey = deprecated()) {

  # Check for deprecated 'apikey' argument
  if (is_present(apikey)) {
    # Signal to the user that the `apikey` argument is deprecated
    lifecycle::deprecate_warn("0.1.7.9000", "SHARK4R::check_algaebase_api(apikey = )",
                              "SHARK4R::check_algaebase_api(subscription_key = )")

    subscription_key <- apikey
  }

  tryCatch(
    {
      # Perform the GET request for a stable genus ID
      response <- httr::GET(
        url = paste0("https://api.algaebase.org/v1.3/genus/", genus_id),
        httr::add_headers("Content-Type" = "application/json", "abapikey" = subscription_key)
      )

      # Check the HTTP status code
      if (httr::status_code(response) == 200) {
        return(TRUE)
      } else {
        stop("API request failed with status: ", httr::status_code(response))
      }
    },
    error = function(e) {
      return(FALSE)
    }
  )
}
