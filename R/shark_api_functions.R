#' Retrieve and Format SHARK Data from SMHI API
#'
#' The `get_shark_table` function retrieves data from the SHARK database hosted by SMHI, with options
#' to filter based on year range, months, and data types. It sends a POST request to
#' the SHARK API and returns the results as a structured `data.frame`.
#' To view available filter options, see \code{\link{get_shark_options}}.
#'
#' @param tableView Character. Specifies the view of the table to retrieve. Options include:
#' @param tableView Character. Specifies the view of the table to retrieve. Options include:
#'   \describe{
#'     \item{"sharkweb_overview"}{}
#'     \item{"sharkweb_all"}{}
#'     \item{"sharkdata_bacterioplankton"}{}
#'     \item{"sharkdata_chlorophyll"}{}
#'     \item{"sharkdata_epibenthos"}{}
#'     \item{"sharkdata_greyseal"}{}
#'     \item{"sharkdata_harbourporpoise"}{}
#'     \item{"sharkdata_harbourseal"}{}
#'     \item{"sharkdata_jellyfish"}{}
#'     \item{"sharkdata_physicalchemical_columns"}{}
#'     \item{"sharkdata_phytoplankton"}{}
#'     \item{"sharkdata_picoplankton"}{}
#'     \item{"sharkdata_planktonbarcoding"}{}
#'     \item{"sharkdata_primaryproduction"}{}
#'     \item{"sharkdata_ringedseal"}{}
#'     \item{"sharkdata_sealpathology"}{}
#'     \item{"sharkdata_sedimentation"}{}
#'     \item{"sharkdata_zoobenthos"}{}
#'     \item{"sharkdata_zooplankton"}{}
#'     \item{"report_sum_year_param"}{}
#'     \item{"report_sum_year_param_taxon"}{}
#'     \item{"report_sampling_per_station"}{}
#'     \item{"report_obs_taxon"}{}
#'     \item{"report_stations"}{}
#'     \item{"report_taxon"}{}
#'   }
#'   Default is `"sharkweb_overview"`.
#' @param limit Integer. Maximum number of records to retrieve per request. Default is `0` (all records).
#' @param headerLang Character. Language option for column headers. Possible values:
#' \itemize{
#'   \item `"sv"`: Swedish
#'   \item `"en"`: English
#'   \item `"short"`: Shortened version
#'   \item `"internal_key"`: Internal key (default)
#' }
#' @param fromYear Integer. The starting year for the data to retrieve. Default is `2019`.
#' @param toYear Integer. The ending year for the data to retrieve. Default is `2020`.
#' @param months Integer vector. The months to retrieve data for, e.g., `c(4, 5, 6)` for April to June.
#' @param dataTypes Character vector. Specifies data types to filter, such as `"Chlorophyll"` or `"Epibenthos"`. 
#' @param parameters Character vector. Optional vector of parameters to filter results, such as `"Chlorophyll-a"`.
#' @param qualityFlags Character vector. Optional vector of quality flags to filter data.
#' @param orderers Character vector. Optional vector of orderers to filter data by specific individuals or organizations.
#' @param deliverers Character vector. Optional vector of deliverers to filter data by data providers.
#' @param projects Character vector. Optional vector of projects to filter data by specific research or monitoring projects.
#' @param datasets Character vector. Optional vector of datasets to filter data by specific dataset names.
#' @param minSamplingDepth Numeric. Optional minimum depth (in meters) for sampling data to filter results.
#' @param maxSamplingDepth Numeric. Optional maximum depth (in meters) for sampling data to filter results.
#' @param checkStatus Character string. Optional status check to filter results.
#' @param redListedCategory Character vector. Optional vector of red-listed taxa for conservation filtering.
#' @param taxonName Character vector. Optional vector of taxa names for filtering specific species or taxa.
#' @param stationName Character vector. Optional vector of station names to retrieve data from specific stations.
#' @param vattenDistrikt Character vector. Optional vector of water district names to filter data by Swedish water districts.
#' @param seaBasins Character vector. Optional vector of sea basin names to filter data by different sea areas.
#' @param counties Character vector. Optional vector of counties to filter data within specific administrative regions in Sweden.
#' @param municipalities Character vector. Optional vector of municipalities to filter data within specific local administrative regions.
#' @param waterCategories Character vector. Optional vector of water categories to filter from.
#' @param typOmraden Character vector. Optional vector of type areas to filter data by specific areas.
#' @param helcomOspar Character vector. Optional vector of HELCOM or OSPAR areas for regional filtering.
#' @param seaAreas Character vector. Optional vector of sea area codes for filtering by specific sea areas
#' @param offset Integer. The starting point for data retrieval, useful for pagination. Default is `0`.
#' @param prod Logical. Query against PROD or TEST (SMHI internal) server. Default is TRUE (PROD).
#'
#' @return A `data.frame` containing the retrieved data, with column names based on the API's `headers`.
#' Columns are filled with `NA` when rows have differing lengths.
#'
#' @details This function constructs a JSON body with specified parameters and sends a POST request
#' to the SHARK API. The API returns data in JSON format, which is then parsed into a `data.frame`.
#' If rows have differing lengths, `rbind.fill` fills missing columns with `NA`.
#'
#' @seealso \code{\link{get_shark_options}}
#'
#' @importFrom httr GET POST content status_code http_error
#' @importFrom jsonlite toJSON
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate everything across
#' @importFrom purrr map_dfr map
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'   # Retrieve chlorophyll data for April to June from 2019 to 2020
#'   shark_data <- get_shark_table(fromYear = 2019, toYear = 2020,
#'                                 months = c(4, 5, 6), dataTypes = c("Chlorophyll"))
#'   View(shark_data)
#' }
#'
#' @export
get_shark_table <- function(tableView = "sharkweb_overview", limit = 0, headerLang = "internal_key", 
                            fromYear = 2019, toYear = 2020, months = c(), dataTypes = c(),
                            parameters = c(), orderers = c(), qualityFlags = c(),
                            deliverers = c(), projects = c(), datasets = c(),
                            minSamplingDepth = "", maxSamplingDepth = "", checkStatus = "",
                            redListedCategory = c(), taxonName = c(), stationName = c(),
                            vattenDistrikt = c(), seaBasins = c(), counties = c(),
                            municipalities = c(), waterCategories = c(), typOmraden = c(),
                            helcomOspar = c(), seaAreas = c(), offset = 0, prod = TRUE) {
  
  # Define the URL
  url <- if (prod) "https://shark.smhi.se/api/sample/table" else "https://shark-tst.smhi.se/api/sample/table"
  url_short <- gsub("api/sample/table", "", url)
  
  # Check if the URL is reachable
  url_response <- try(GET(url_short), silent = TRUE)
  if (inherits(url_response, "try-error") || http_error(url_response)) {
    stop("The SHARK ", ifelse(prod, "PROD", "TEST"), " server cannot be reached: ", url_short, ". Please check network connection.")
  }
  
  if (!is.numeric(limit) || limit <= 0) {
    limit <- get_shark_table_counts(tableView = tableView, fromYear = fromYear, toYear = toYear,
                                    months = months, dataTypes = dataTypes, parameters = parameters, 
                                    orderers = orderers, qualityFlags = qualityFlags, deliverers = deliverers, 
                                    projects = projects, datasets = datasets, minSamplingDepth = minSamplingDepth, 
                                    maxSamplingDepth = maxSamplingDepth, checkStatus = checkStatus,
                                    redListedCategory = redListedCategory, taxonName = taxonName, 
                                    stationName = stationName,vattenDistrikt = vattenDistrikt, 
                                    seaBasins = seaBasins, counties = counties, municipalities = municipalities, 
                                    waterCategories = waterCategories, typOmraden = typOmraden, 
                                    helcomOspar = helcomOspar, seaAreas = seaAreas, prod = prod)
                                    
  }
  
  # Create the JSON body as a list
  body <- list(
    params = list(
      tableView = tableView,
      limit = limit,
      offset = offset,
      headerLang = headerLang
    ),
    query = list(
      bounds = list(),
      fromYear = fromYear,
      toYear = toYear,
      months = months,
      dataTypes = dataTypes,
      parameters = parameters,
      checkStatus = checkStatus,
      qualityFlags = qualityFlags,
      deliverers = deliverers,
      orderers = orderers,
      projects = projects,
      datasets = datasets,
      minSamplingDepth = minSamplingDepth,
      maxSamplingDepth = maxSamplingDepth,
      redListedCategory = redListedCategory,
      taxonName = taxonName,
      stationName = stationName,
      vattenDistrikt = vattenDistrikt,
      seaBasins = seaBasins,
      counties = counties,
      municipalities = municipalities,
      waterCategories = waterCategories,
      typOmraden = typOmraden,
      helcomOspar = helcomOspar,
      seaAreas = seaAreas
    )
  )
  
  # Convert body to JSON
  body_json <- toJSON(body, auto_unbox = TRUE)
  
  # Make the POST request
  response <- POST(url,
                   add_headers("accept" = "application/json", "Content-Type" = "application/json"),
                   body = body_json)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON response content
    shark_data <- content(response, as = "parsed", type = "application/json")
    
    # Extract headers and rows
    headers <- unlist(shark_data$headers)
    
    # Process rows by binding each row to the headers, filling with NA where necessary
    data <- map_dfr(shark_data$rows, ~{
      row <- as.data.frame(t(.), stringsAsFactors = FALSE)
      names(row) <- headers
      row
    }) %>%
      as_tibble() %>%
      # Replace NULLs with NA and unnest list-columns
      mutate(across(everything(), ~ map(.x, ~ if (is.null(.x)) NA else .x))) %>%
      unnest(cols = everything())
    
    return(data)
  } else {
    # Return the error message
    stop("Failed to retrieve data: ", status_code(response))
  }
}
#' Retrieve Available Search Options from SHARK API
#'
#' The `get_shark_options` function retrieves available search options from the SHARK database hosted by SMHI.
#' It sends a GET request to the SHARK API and returns the results as a structured `data.frame`.
#'
#' @param prod Logical. Query against PROD or TEST (SMHI internal) server. Default is TRUE (PROD).
#'
#' @return A `data.frame` containing the available search options from the SHARK API.
#'
#' @details This function sends a GET request to the SHARK API options endpoint to retrieve available search filters and options
#' for querying the database. The API returns data in JSON format, which is then parsed into a `data.frame`.
#'
#' @importFrom httr GET content status_code
#'
#' @seealso \code{\link{get_shark_table}}
#'
#' @examples
#' \dontrun{
#'   # Retrieve available search options
#'   shark_options <- get_shark_options()
#'   View(shark_options)
#'   
#'   # View available datatypes
#'   dataTypes <- unlist(shark_options$dataTypes)
#'   print(dataTypes)
#' }
#'
#' @export
get_shark_options <- function(prod = TRUE) {
  # Define the URL for options
  url <- "https://shark.smhi.se/api/options"
  
  if (prod) {
    url <- "https://shark.smhi.se/api/options"
  } else {
    url <- "https://shark-tst.smhi.se/api/options"
  }
  
  url_short <- gsub("api/options", "", url)
  
  # Check if the URL is reachable
  url_response <- try(GET(url_short), silent = TRUE)
  
  if (inherits(url_response, "try-error") || http_error(url_response)) {
    stop("The SHARK ", ifelse(prod, "PROD", "TEST"), " server cannot be reached: ", url_short, ". Please check network connection.")
  }
  
  # Make the GET request
  response <- GET(url, add_headers("accept" = "application/json"))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON response content
    shark_options <- content(response, as = "parsed", type = "application/json")
    
    return(shark_options)
  } else {
    # Return the error message if the request failed
    stop("Failed to retrieve options: ", status_code(response))
  }
}
#' Retrieve SHARK Data Table Row Counts
#'
#' The `get_shark_table_counts` function retrieves the row counts of data records from various SHARK data tables.
#' To view available filter options, see \code{\link{get_shark_options}}.
#'
#' @param tableView Character. Specifies the view of the table to retrieve. Options include:
#'   \describe{
#'     \item{"sharkweb_overview"}{}
#'     \item{"sharkweb_all"}{}
#'     \item{"sharkdata_bacterioplankton"}{}
#'     \item{"sharkdata_chlorophyll"}{}
#'     \item{"sharkdata_epibenthos"}{}
#'     \item{"sharkdata_greyseal"}{}
#'     \item{"sharkdata_harbourporpoise"}{}
#'     \item{"sharkdata_harbourseal"}{}
#'     \item{"sharkdata_jellyfish"}{}
#'     \item{"sharkdata_physicalchemical_columns"}{}
#'     \item{"sharkdata_phytoplankton"}{}
#'     \item{"sharkdata_picoplankton"}{}
#'     \item{"sharkdata_planktonbarcoding"}{}
#'     \item{"sharkdata_primaryproduction"}{}
#'     \item{"sharkdata_ringedseal"}{}
#'     \item{"sharkdata_sealpathology"}{}
#'     \item{"sharkdata_sedimentation"}{}
#'     \item{"sharkdata_zoobenthos"}{}
#'     \item{"sharkdata_zooplankton"}{}
#'     \item{"report_sum_year_param"}{}
#'     \item{"report_sum_year_param_taxon"}{}
#'     \item{"report_sampling_per_station"}{}
#'     \item{"report_obs_taxon"}{}
#'     \item{"report_stations"}{}
#'     \item{"report_taxon"}{}
#'   }
#'   Default is `"sharkweb_overview"`.
#' @param fromYear Integer. The starting year for the data to retrieve. Default is `2019`.
#' @param toYear Integer. The ending year for the data to retrieve. Default is `2020`.
#' @param months Integer vector. The months to retrieve data for (e.g., `c(4, 5, 6)` for April to June).
#' @param dataTypes Character vector. Specifies data types to filter, such as `"Chlorophyll"` or `"Epibenthos"`.
#' @param parameters Character vector. Optional. Parameters to filter results, such as `"Chlorophyll-a"`.
#' @param qualityFlags Character vector. Optional. Quality flags to filter data.
#' @param orderers Character vector. Optional. Orderers to filter data by specific organizations.
#' @param deliverers Character vector. Optional. Deliverers to filter data by data providers.
#' @param projects Character vector. Optional. Projects to filter data by specific research or monitoring projects.
#' @param datasets Character vector. Optional. Datasets to filter data by specific dataset names.
#' @param minSamplingDepth Numeric. Optional. Minimum depth (in meters) for sampling data.
#' @param maxSamplingDepth Numeric. Optional. Maximum depth (in meters) for sampling data.
#' @param checkStatus Character string. Optional. Status check to filter results.
#' @param redListedCategory Character vector. Optional. Red-listed taxa for conservation filtering.
#' @param taxonName Character vector. Optional. Taxa names for filtering specific species or taxa.
#' @param stationName Character vector. Optional. Station names to retrieve data from specific stations.
#' @param vattenDistrikt Character vector. Optional. Water district names to filter data by Swedish water districts.
#' @param seaBasins Character vector. Optional. Sea basin names to filter data by different sea areas.
#' @param counties Character vector. Optional. Counties to filter data within specific administrative regions in Sweden.
#' @param municipalities Character vector. Optional. Municipalities to filter data within specific local regions.
#' @param waterCategories Character vector. Optional. Water categories to filter data by.
#' @param typOmraden Character vector. Optional. Type areas to filter data by specific areas.
#' @param helcomOspar Character vector. Optional. HELCOM or OSPAR areas for regional filtering.
#' @param seaAreas Character vector. Optional. Sea area codes for filtering by specific sea areas.
#' @param prod Logical. Query against PROD or TEST (SMHI internal) server. Default is `TRUE` (PROD).
#'
#' @return A `data.frame` containing the retrieved data, with column names based on the API's `headers`.
#'   Missing columns in rows with differing lengths are filled with `NA`.
#'
#' @details This function constructs a JSON body using the specified parameters and sends a POST request
#'   to the SHARK API. The returned JSON data is parsed into a `data.frame`. Rows with differing lengths are handled by filling missing columns with `NA`.
#'
#' @seealso \code{\link{get_shark_options}}
#'
#' @importFrom httr GET POST content status_code http_error
#' @importFrom jsonlite toJSON
#'
#' @examples
#' \dontrun{
#'   # Retrieve chlorophyll data for April to June from 2019 to 2020
#'   shark_data_counts <- get_shark_table_counts(fromYear = 2019, toYear = 2020,
#'                                               months = c(4, 5, 6), dataTypes = c("Chlorophyll"))
#'   print(shark_data_counts)
#' }
#'
#' @export
get_shark_table_counts <- function(tableView = "sharkweb_overview", 
                                   fromYear = 2019, toYear = 2020, months = c(), dataTypes = c(),
                                   parameters = c(), orderers = c(), qualityFlags = c(),
                                   deliverers = c(), projects = c(), datasets = c(),
                                   minSamplingDepth = "", maxSamplingDepth = "", checkStatus = "",
                                   redListedCategory = c(), taxonName = c(), stationName = c(),
                                   vattenDistrikt = c(), seaBasins = c(), counties = c(),
                                   municipalities = c(), waterCategories = c(), typOmraden = c(),
                                   helcomOspar = c(), seaAreas = c(), prod = TRUE) {
  
  # Define the URL
  url <- if (prod) "https://shark.smhi.se/api/sample/count" else "https://shark-tst.smhi.se/api/sample/count"
  url_short <- gsub("api/sample/count", "", url)
  
  # Check if the URL is reachable
  url_response <- try(GET(url_short), silent = TRUE)
  if (inherits(url_response, "try-error") || http_error(url_response)) {
    stop("The SHARK ", ifelse(prod, "PROD", "TEST"), " server cannot be reached: ", url_short, ". Please check network connection.")
  }
  
  # Create the JSON body as a list
  body <- list(
    params = list(
      tableView = tableView
    ),
    query = list(
      bounds = list(),
      fromYear = fromYear,
      toYear = toYear,
      months = months,
      dataTypes = dataTypes,
      parameters = parameters,
      checkStatus = checkStatus,
      qualityFlags = qualityFlags,
      deliverers = deliverers,
      orderers = orderers,
      projects = projects,
      datasets = datasets,
      minSamplingDepth = minSamplingDepth,
      maxSamplingDepth = maxSamplingDepth,
      redListedCategory = redListedCategory,
      taxonName = taxonName,
      stationName = stationName,
      vattenDistrikt = vattenDistrikt,
      seaBasins = seaBasins,
      counties = counties,
      municipalities = municipalities,
      waterCategories = waterCategories,
      typOmraden = typOmraden,
      helcomOspar = helcomOspar,
      seaAreas = seaAreas
    )
  )
  
  # Convert body to JSON
  body_json <- toJSON(body, auto_unbox = TRUE)
  
  # Make the POST request
  response <- POST(url,
                   add_headers("accept" = "application/json", "Content-Type" = "application/json"),
                   body = body_json)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON response content
    data <- content(response, as = "parsed", type = "application/json")
    
    return(data)
  } else {
    # Return the error message
    stop("Failed to retrieve data: ", status_code(response))
  }
}
