#' Retrieve SHARK Table Data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated due to inefficiency in handling large datasets.
#' Users are encouraged to use \code{\link{get_shark_data}} for such cases.
#' However, the `get_shark_table` function remains effective for retrieving smaller
#' datasets (< 10^5 rows) from the SHARK database hosted by SMHI. Its functionality
#' is similar to the table view available at \url{https://shark.smhi.se/}.
#' For larger requests, switch to \code{\link{get_shark_data}}, and to explore available
#' filter options, see \code{\link{get_shark_options}}.
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
#' @param limit Integer. Maximum number of records to retrieve per request. Default is `0` (all records). Maximum 10^5.
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
#' @examples
#' \dontrun{
#'   # Retrieve chlorophyll data for April to June from 2019 to 2020
#'   shark_data <- get_shark_table(fromYear = 2019, toYear = 2020,
#'                                 months = c(4, 5, 6), dataTypes = c("Chlorophyll"))
#'   View(shark_data)
#' }
#'
#' @keywords internal
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
                            helcomOspar = c(), seaAreas = c(), prod = TRUE, verbose = TRUE) {

  lifecycle::deprecate_warn("0.1.1", "get_shark_table()", "get_shark_data()", "The `get_shark_table` function is inefficient at handling large data requests")

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
                                    stationName = stationName, vattenDistrikt = vattenDistrikt,
                                    seaBasins = seaBasins, counties = counties, municipalities = municipalities,
                                    waterCategories = waterCategories, typOmraden = typOmraden,
                                    helcomOspar = helcomOspar, seaAreas = seaAreas, prod = prod)
  }

  # Warn if the request is too large
  if (limit > 10^5) {
    stop("Your request contains ", limit, " rows and will take significant time to retrieve using `get_shark_table`. Please use `get_shark_data` instead.")
  }

  # Initialize variables
  batch_size <- 1000
  all_rows <- list()
  total_retrieved <- 0
  headers <- NULL

  # Set up the progress bar
  if (verbose) {pb <- txtProgressBar(min = 0, max = limit, style = 3)}

  # Loop to fetch data in batches
  while (total_retrieved < limit) {
    # Calculate remaining rows to fetch
    remaining <- min(batch_size, limit - total_retrieved)

    # Update progress bar
    if (verbose) {setTxtProgressBar(pb, total_retrieved + batch_size)}

    # Create the JSON body as a list
    body <- list(
      params = list(
        tableView = tableView,
        limit = remaining,
        offset = total_retrieved,
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

      # Extract headers from the first response
      if (is.null(headers)) {
        headers <- unlist(shark_data$headers)
      }

      # Extract rows and store them
      all_rows <- append(all_rows, shark_data$rows)
      total_retrieved <- total_retrieved + length(shark_data$rows)

      # Stop if no more rows are returned
      if (length(shark_data$rows) < batch_size) break
    } else {
      stop("Failed to retrieve data: ", status_code(response))
    }
  }

  # Close the progress bar
  if (verbose) {
    close(pb)
  }

  # Combine all rows into a single data.frame after the loop
  combined_data <- map_dfr(all_rows, ~{
    row <- as.data.frame(t(.), stringsAsFactors = FALSE)
    names(row) <- headers
    row
  }) %>%
    as_tibble() %>%
    # Replace NULLs and blanks ("") with NA, and unnest list-columns
    mutate(across(everything(), ~ map(.x, ~ if (is.null(.x) || .x == "") NA else .x))) %>%
    unnest(cols = everything())

  return(combined_data)
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
#' @seealso \code{\link{get_shark_options}}
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
#' Retrieve Data from SHARK API
#'
#' The `get_shark_data` function retrieves data from the SHARK database hosted by SMHI. The function sends a POST request
#' to the SHARK API with customizable filters, including year, month, taxon name, water category, and more, and returns the
#' retrieved data as a structured `data.frame`. To view available filter options, see \code{\link{get_shark_options}}.
#'
#' @param tableView Character. Specifies the columns of the table to retrieve. Options include:
#'   \itemize{
#'     \item `"sharkweb_overview"`: Overview table
#'     \item `"sharkweb_all"`: All available columns
#'     \item `"sharkdata_bacterioplankton"`: Bacterioplankton table
#'     \item `"sharkdata_chlorophyll"`: Chlorophyll table
#'     \item `"sharkdata_epibenthos"`: Epibenthos table
#'     \item `"sharkdata_greyseal"`: Greyseal table
#'     \item `"sharkdata_harbourporpoise"`: Harbour porpoise table
#'     \item `"sharkdata_harbourseal`: Harbour seal table
#'     \item `"sharkdata_jellyfish"`: Jellyfish table
#'     \item `"sharkdata_physicalchemical_columns"`: Physical chemical table
#'     \item `"sharkdata_phytoplankton"`: Phytoplankton table
#'     \item `"sharkdata_picoplankton"`: Picoplankton table
#'     \item `"sharkdata_planktonbarcoding"`: Planktonbarcoding table
#'     \item `"sharkdata_primaryproduction"`: Primary production table
#'     \item `"sharkdata_ringedseal"`: Ringed seal table
#'     \item `"sharkdata_sealpathology"`: Seal pathology table
#'     \item `"sharkdata_sedimentation"`: Sedimentation table
#'     \item `"sharkdata_zoobenthos"`: Zoobenthos table
#'     \item `"sharkdata_zooplankton"`: Zooplankton table
#'     \item `"report_sum_year_param"`: Report sum per year and parameter
#'     \item `"report_sum_year_param_taxon"`: Report sum per year, parameter and taxon
#'     \item `"report_sampling_per_station"`: Report sampling per station
#'     \item `"report_obs_taxon"`: Report observed taxa
#'     \item `"report_stations"`: Report stations
#'     \item `"report_taxon"`: Report taxa
#'   }
#'   Default is `"sharkweb_overview"`.
#' @param headerLang Character. Language option for column headers. Possible values:
#'   \itemize{
#'     \item `"sv"`: Swedish.
#'     \item `"en"`: English.
#'     \item `"short"`: Shortened version.
#'     \item `"internal_key"`: Internal key (default).
#'   }
#' @param save_data Logical. If TRUE, the data will be saved to a specified file (see `file_path`). If FALSE, a temporary file will be created instead.
#' @param file_path Character. The file path where the data should be saved. Required if `save_data` is TRUE. Ignored if `save_data` is FALSE.
#' @param delimiters Character. Specifies the delimiter used to separate values in the file, if `save_data` is TRUE.
#'   Options are `"point-tab"` (tab-separated) or `"point-semi"` (semicolon-separated).
#'   Default is `"point-tab"`.
#' @param lineEnd Character. Defines the type of line endings in the file, if `save_data` is TRUE.
#'   Options are `"win"` (Windows-style, `\r\n`) or `"unix"` (Unix-style, `\n`).
#'   Default is `"win"`.
#' @param encoding Character. Sets the file's text encoding, if `save_data` is TRUE.
#'   Options are `"cp1252"`, `"utf_8"`, `"utf_16"`, or `"latin_1"`.
#'   Default is `"utf_8"`.
#' @param dataTypes Character vector. Specifies data types to filter, such as `"Chlorophyll"`, `"Epibenthos"`, etc.
#' @param fromYear Integer. Starting year for data retrieval. Default is `2019`.
#' @param toYear Integer. Ending year for data retrieval. Default is `2020`.
#' @param months Integer vector. The months to retrieve data for, e.g., `c(4, 5, 6)` for April to June.
#' @param parameters Character vector. Optional parameters to filter the results by, such as `"Chlorophyll-a"`.
#' @param checkStatus Character string. Optional status check to filter results.
#' @param qualityFlags Character vector. Quality flags to filter the data.
#' @param deliverers Character vector. Specifies the data deliverers to filter by.
#' @param orderers Character vector. Orderers to filter by specific organizations or individuals.
#' @param projects Character vector. Projects to filter data by specific research or monitoring projects.
#' @param datasets Character vector. Datasets to filter data by specific datasets.
#' @param minSamplingDepth Numeric. Minimum sampling depth (in meters) to filter the data.
#' @param maxSamplingDepth Numeric. Maximum sampling depth (in meters) to filter the data.
#' @param redListedCategory Character vector. Red-listed taxa for conservation filtering.
#' @param taxonName Character vector. Optional vector of taxa names to filter by.
#' @param stationName Character vector. Station names to filter data by specific stations.
#' @param vattenDistrikt Character vector. Water district names to filter by Swedish water districts.
#' @param seaBasins Character vector. Sea basins to filter by.
#' @param counties Character vector. Counties to filter by specific administrative regions.
#' @param municipalities Character vector. Municipalities to filter by.
#' @param waterCategories Character vector. Water categories to filter by.
#' @param typOmraden Character vector. Type areas to filter by.
#' @param helcomOspar Character vector. HELCOM or OSPAR areas for regional filtering.
#' @param seaAreas Character vector. Sea area codes to filter by specific sea areas.
#' @param hideEmptyColumns Logical. Whether to hide empty columns. Default is `FALSE`.
#' @param prod Logical. Whether to query the PROD (production) server or the TEST (testing) server. Default is `TRUE` (PROD).
#' @param verbose Logical. Whether to display progress information. Default is `TRUE`.
#'
#' @return A `data.frame` containing the retrieved SHARK data, with column names based on the API's response.
#'
#' @details This function sends a POST request to the SHARK API with the specified filters. The response is parsed as JSON and then converted into a `data.frame`.
#' The function handles the dynamic construction of the query body to filter the data based on the provided parameters.
#'
#' @seealso \code{\link{get_shark_options}}
#'
#' @examples
#' \dontrun{
#'   # Retrieve chlorophyll data from 2019 to 2020 for April to June
#'   shark_data <- get_shark_data(fromYear = 2019, toYear = 2020,
#'                                months = c(4, 5, 6), dataTypes = c("Chlorophyll"))
#'   View(shark_data)
#' }
#'
#' @export
get_shark_data <- function(tableView = "sharkweb_overview", headerLang = "internal_key", save_data = FALSE,
                           file_path = NULL, delimiters = "point-tab", lineEnd = "win", encoding = "utf_8",
                           dataTypes = c(), fromYear = 2019, toYear = 2020, months = c(), parameters = c(),
                           checkStatus = "", qualityFlags = c(), deliverers = c(), orderers = c(),
                           projects = c(), datasets = c(), minSamplingDepth = "", maxSamplingDepth = "",
                           redListedCategory = c(), taxonName = c(), stationName = c(), vattenDistrikt = c(),
                           seaBasins = c(), counties = c(), municipalities = c(), waterCategories = c(),
                           typOmraden = c(), helcomOspar = c(), seaAreas = c(), hideEmptyColumns = FALSE,
                           prod = TRUE, verbose = TRUE) {

  # Set up file path to .txt file
  if (save_data) {
    if (!is.null(file_path)) {
      file <- file_path
    } else {
      stop("Please specify 'file_path' when 'save_data' is TRUE")
    }
  } else {
    file <- tempfile(fileext = ".tsv")
  }

  if (!save_data & !is.null(file_path)) {
    stop("To save the data, set 'save_data' to TRUE and specify a valid 'file_path': ", file_path)
  }

  # Define the URL
  url <- if (prod) "https://shark.smhi.se/api/sample/download" else "https://shark-tst.smhi.se/api/sample/download"
  url_short <- gsub("api/sample/download", "", url)

  # Check if the URL is reachable
  url_response <- try(GET(url_short), silent = TRUE)
  if (inherits(url_response, "try-error") || http_error(url_response)) {
    stop("The SHARK ", ifelse(prod, "PROD", "TEST"), " server cannot be reached: ", url_short, ". Please check network connection.")
  }

  # Encoding translation
  encoding_map <- c("cp1252" = "windows-1252",
                    "utf_8" = "UTF-8",
                    "utf_16" = "UTF-16",
                    "latin_1" = "ISO-8859-1")

  if (!encoding %in% c("cp1252", "utf_8", "utf_16", "latin_1")) {
    warning("'encoding' must be one of 'cp1252', 'utf_8', 'utf_16', or 'latin_1'. Defaulting to 'utf_8'.")

    encoding<-"utf_8"
  }

  # Check if the provided encoding is valid
  content_encoding <- encoding_map[[encoding]]

  if (!delimiters %in% c("point-tab", "point-semi")) {
    warning("'delimiters' must be one of 'point-tab' or 'point-semi'. Defaulting to 'point-tab'.")

    delimiters<-"point-tab"
  }

  # Map 'delimiters' input to actual separator
  sep_char <- switch(
    delimiters,
    "point-tab" = "\t",   # Tab-separated
    "point-semi" = ";",   # Semicolon-separated
    stop("Invalid 'delimiters' value. Use 'point-tab' or 'point-semi'.")
  )

  if (!lineEnd %in% c("win", "unix")) {
    warning("'lineEnd' must be one of 'win' or 'unix'. Defaulting to 'win'.")

    lineEnd<-"win"
  }

  # Create the JSON body as a list
  body <- list(
    params = list(
      tableView = tableView,
      delimiters = delimiters,
      lineEnd = lineEnd,
      encoding = encoding,
      headerLang = headerLang,
      hideEmptyColumns = hideEmptyColumns
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

  # Send the POST request
  response <- POST(
    url,
    add_headers("accept" = "application/json", "Content-Type" = "application/json"),
    body = body_json,
    write_disk(file, overwrite = TRUE),
    if (verbose) {
      progress()
    }
  )

  # Check response status
  if (status_code(response) == 200) {
    # Load the file into R as a tibble
    parsed_table<-read_delim(file = file,
                             delim = sep_char,
                             locale = locale(encoding = content_encoding),
                             col_types = cols(),
                             progress = FALSE)

    if (!save_data) {
      # Clean up temporary file
      unlink(file)
    }

    return(parsed_table)
  } else {
    # Clean up temporary file in case of an error
    unlink(file)
    stop("Failed to retrieve data: HTTP Status ", status_code(response), "\n",
         content(response, as = "text", encoding = "UTF-8"))
  }
}
