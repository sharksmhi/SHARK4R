#' Retrieve available search options from SHARK API
#'
#' The `get_shark_options` function retrieves available search options from the SHARK database.
#' It sends a GET request to the SHARK API and returns the results as a structured `data.frame`.
#'
#' @param prod Logical. Query against PROD or TEST (SMHI internal) server. Default is TRUE (PROD).
#' @param unparsed Logical. If `TRUE`, returns the complete JSON output as list. Defaults to `FALSE`.
#'
#' @return A `data.frame` containing the available search options from the SHARK API.
#'
#' @details This function sends a GET request to the SHARK API options endpoint to retrieve available search filters and options
#' for querying the database. The API returns data in JSON format, which is then parsed into a `data.frame`.
#'
#' @seealso \url{https://shark.smhi.se} for SHARK database.
#' @seealso \code{\link{get_shark_data}}
#'
#' @examples
#' \dontrun{
#'   # Retrieve available search options
#'   shark_options <- get_shark_options()
#'   View(shark_options)
#'
#'   # View available datatypes
#'   dataTypes <- shark_options$dataTypes
#'   print(dataTypes)
#' }
#'
#' @export
get_shark_options <- function(prod = TRUE, unparsed = FALSE) {

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

    # Return unparsed options
    if (unparsed) {
      return(shark_options)
    }

    parsed_options <- lapply(shark_options, function(x) {
      if (is.list(x)) {
        # If it's a list, check if it contains numeric or character data
        if (all(sapply(x, is.numeric))) {
          return(as.numeric(unlist(x)))  # Convert to numeric vector if all elements are numeric
        } else {
          return(unlist(x, use.names = FALSE))  # Otherwise, convert to character vector
        }
      } else {
        return(x)  # Non-list elements are returned as-is
      }
    })

    return(parsed_options)
  } else {
    # Return the error message if the request failed
    stop("Failed to retrieve options: ", status_code(response))
  }
}
#' Retrieve SHARK data table row counts
#'
#' The `get_shark_table_counts` function retrieves the number of records (row counts)
#' from various SHARK data tables based on specified filters such as year, months,
#' data type, stations, and taxa. To view available filter options, see
#' \code{\link{get_shark_options}}.
#'
#' @param tableView Character. Specifies the view of the table to retrieve. Options include:
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
#'     \item `"sharkdata_physicalchemical"`: Physical chemical table
#'     \item `"sharkdata_physicalchemical_columns"`: Physical chemical table: column view
#'     \item `"sharkdata_phytoplankton"`: Phytoplankton table
#'     \item `"sharkdata_picoplankton"`: Picoplankton table
#'     \item `"sharkdata_planktonbarcoding"`: Plankton barcoding table
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
#' @seealso \url{https://shark.smhi.se} for SHARK database.
#' @seealso \code{\link{get_shark_options}}
#'
#' @return An integer representing the total number of rows in the requested SHARK table
#'   after applying the specified filters.
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
#' Retrieve data from the SHARK API
#'
#' The `get_shark_data` function retrieves tabular data from the SHARK database hosted by SMHI. The function sends a POST request
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
#'     \item `"sharkdata_physicalchemical"`: Physical chemical table
#'     \item `"sharkdata_physicalchemical_columns"`: Physical chemical table: column view
#'     \item `"sharkdata_phytoplankton"`: Phytoplankton table
#'     \item `"sharkdata_picoplankton"`: Picoplankton table
#'     \item `"sharkdata_planktonbarcoding"`: Plankton barcoding table
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
#' @param save_data Logical. If TRUE, the data will be saved to a specified file (see `file_path`).
#'   If FALSE, a temporary file will be created instead. The temporary file will be automatically deleted after it is loaded into memory.
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
#' @param dataTypes Character vector. Specifies data types to filter. Possible values include:
#' \itemize{
#'   \item "Bacterioplankton"
#'   \item "Chlorophyll"
#'   \item "Epibenthos"
#'   \item "Grey seal"
#'   \item "Harbour Porpoise"
#'   \item "Harbour seal"
#'   \item "Jellyfish"
#'   \item "Physical and Chemical"
#'   \item "Phytoplankton"
#'   \item "Picoplankton"
#'   \item "PlanktonBarcoding"
#'   \item "Primary production"
#'   \item "Profile"
#'   \item "Ringed seal"
#'   \item "Seal pathology"
#'   \item "Sedimentation"
#'   \item "Zoobenthos"
#'   \item "Zooplankton"
#' }
#' @param bounds A numeric vector of length 4 specifying the geographical search boundaries in decimal degrees,
#'   formatted as `c(lon_min, lat_min, lon_max, lat_max)`, e.g., `c(11, 58, 12, 59)`. Default is `c()` to include all data.
#' @param fromYear Integer (optional). The starting year for data retrieval.
#'   If set to `NULL` (default), the function will use the earliest available year in SHARK.
#' @param toYear Integer (optional). The ending year for data retrieval.
#'   If set to `NULL` (default), the function will use the latest available year in SHARK.
#' @param months Integer vector. The months to retrieve data for, e.g., `c(4, 5, 6)` for April to June.
#' @param parameters Character vector. Optional parameters to filter the results by, such as `"Chlorophyll-a"`.
#' @param checkStatus Character string. Optional status check to filter results.
#' @param qualityFlags Character vector. Specifies the quality flags to filter the data. By default, all data are included, including those with the "B" flag (Bad).
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
#' @param hideEmptyColumns Logical. Whether to hide empty columns. Default is FALSE.
#' @param row_limit Numeric. Specifies the maximum number of rows that can be retrieved in a single request. If the requested data exceeds this limit, the function automatically downloads the data in yearly chunks. The default value is 10 million rows.
#' @param prod Logical. Whether to query the PROD (production) server or the SMHI internal TEST (testing) server. Default is TRUE (PROD).
#' @param verbose Logical. Whether to display progress information. Default is TRUE.
#'
#' @return A `data.frame` containing the retrieved SHARK data, with column names based on the API's response.
#'
#' @details This function sends a POST request to the SHARK API with the specified filters. The response is parsed as JSON
#'   and then converted into a `data.frame`. The function handles the dynamic construction of the query body to filter
#'   the data based on the provided parameters. If the `row_limit` parameter is reached, the data retrieval process is
#'   split into manageable chunks to avoid overwhelming the API or running into memory issues. Please note that making very
#'   large requests, such as retrieving the entire database, can be extremely memory-intensive.
#'
#' @seealso \url{https://shark.smhi.se} for SHARK database.
#' @seealso \code{\link{get_shark_options}} \code{\link{get_shark_table_counts}}
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
                           dataTypes = c(), bounds = c(), fromYear = NULL, toYear = NULL, months = c(), parameters = c(),
                           checkStatus = "", qualityFlags = c(), deliverers = c(), orderers = c(),
                           projects = c(), datasets = c(), minSamplingDepth = "", maxSamplingDepth = "",
                           redListedCategory = c(), taxonName = c(), stationName = c(), vattenDistrikt = c(),
                           seaBasins = c(), counties = c(), municipalities = c(), waterCategories = c(),
                           typOmraden = c(), helcomOspar = c(), seaAreas = c(), hideEmptyColumns = FALSE,
                           row_limit = 10^7, prod = TRUE, verbose = TRUE) {

  # Set up file path to .txt file
  if (save_data && is.null(file_path)) {
    stop("Please specify 'file_path' when 'save_data' is TRUE")
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

  # Retrieve default year options, such as minYear and maxYear
  options <- get_shark_options(prod = prod, unparsed = TRUE)

  if (length(dataTypes) == 0) {
    dataTypes <- unlist(options$dataTypes)
  }

  # Check if either 'fromYear' or 'toYear' is NULL
  if (is.null(fromYear) | is.null(toYear)) {

    # If 'fromYear' is NULL, set it to the minimum year from the options
    if (is.null(fromYear)) {
      fromYear <- options$minYear
    }

    # If 'toYear' is NULL, set it to the maximum year from the options
    if (is.null(toYear)) {
      toYear <- options$maxYear
    }

    # Store min years as dataframe
    min_year_df <- data.frame(
      dataTypes = names(options$minYearPerDatatype),
      minYear = unlist(options$minYearPerDatatype),
      row.names = NULL,
      stringsAsFactors = TRUE
    )

    # Filter datatypes
    min_year_df <- dplyr::filter(min_year_df, dataTypes %in% dataTypes)

    # Identify the minimum year
    fromYear <- min(min_year_df$minYear)
  }

  # Create a vector of years
  years <- c(fromYear:toYear)

  # Get row count before downloading data
  count <- get_shark_table_counts(tableView = tableView, fromYear = fromYear, toYear = toYear, months = months,
                                  dataTypes = dataTypes, parameters = parameters, orderers = orderers,
                                  qualityFlags = qualityFlags, deliverers = deliverers, projects = projects,
                                  datasets = datasets, maxSamplingDepth = maxSamplingDepth, checkStatus = checkStatus,
                                  minSamplingDepth = minSamplingDepth, redListedCategory = redListedCategory, taxonName = taxonName,
                                  stationName = stationName, vattenDistrikt = vattenDistrikt, seaBasins = seaBasins,
                                  counties = counties, municipalities = c(), waterCategories = c(),
                                  typOmraden = c(), helcomOspar = helcomOspar, seaAreas = seaAreas, prod = prod)

  # Download data in chunks or everything at once
  if (count > row_limit) {

    # Print message
    if (verbose) {
      message("The requested data exceeds the maximum row limit of ", row_limit, ". Data will be downloaded in yearly chunks to ensure all data is retrieved.")
    }

    all_data <- list()  # Initialize a list to store yearly data

    # Set up the progress bar
    if (verbose) {pb <- utils::txtProgressBar(min = 0, max = length(years), style = 3)}

    # Temporary folder to save yearly files
    temp_dir <- tempdir()

    for (i in seq_along(years)) {
      year <- years[i]  # Get the current year

      # Update progress bar
      if (verbose) { utils::setTxtProgressBar(pb, i) }

      # Update the body for the POST request with the current year
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
          bounds = bounds,
          fromYear = year,
          toYear = year,
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
        body = body_json
      )

      # Check response status
      if (status_code(response) == 200) {
        year_data <- read_delim(file = content(response),
                                delim = sep_char,
                                locale = locale(encoding = content_encoding),
                                na = c("", "-", "NA"),
                                col_types = cols(.default = col_character()),
                                progress = FALSE)

        # Save to disk
        temp_file <- file.path(temp_dir, paste0("data_", year, ".tsv"))
        write_tsv(year_data, temp_file, progress = FALSE)

      } else {
        warning("Failed to retrieve data for year: ", year, " HTTP Status: ", status_code(response))
      }
    }

    # Close the progress bar
    if (verbose) {
      close(pb)
    }

    # Combine files from disk
    file_list <- list.files(temp_dir, pattern = "data_.*\\.tsv", full.names = TRUE)
    combined_data <- vroom(rev(file_list),
                           delim = "\t",
                           col_types = cols(.default = col_character()),
                           progress = FALSE)

    # Convert to correct column types
    combined_data <- type_convert(combined_data, col_types = cols())

    if (save_data) {
      utils::write.table(combined_data, file = file_path, sep = sep_char, row.names = FALSE, col.names = TRUE,
                         quote = FALSE, fileEncoding = content_encoding)
    }

    return(combined_data)

  } else {
    # Set up file path to .tsv file
    if (!save_data) {
      file_path <- tempfile(fileext = ".tsv")
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
        bounds = bounds,
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
      write_disk(file_path, overwrite = TRUE),
      if (verbose) {
        progress()
      }
    )

    # Check response status
    if (status_code(response) == 200) {
      # Load the file into R as a (character) tibble
      parsed_table <- read_delim(file = file_path,
                                 delim = sep_char,
                                 locale = locale(encoding = content_encoding),
                                 na = c("", "-", "NA"),
                                 col_types = cols(
                                   .default = col_character()
                                 ),
                                 progress = FALSE)

      # Convert to correct column type
      parsed_table <- type_convert(parsed_table, col_types = cols())

      if (!save_data) {
        # Clean up temporary file
        unlink(file_path)
      }

      return(parsed_table)
    } else {
      # Clean up temporary file in case of an error
      unlink(file_path)
      stop("Failed to retrieve data: HTTP Status ", status_code(response), "\n",
           content(response, as = "text", encoding = "UTF-8"))
    }
  }
}
#' Download SHARK dataset zip archives
#'
#' Downloads one or more datasets (zip-archives) from the SHARK database
#' (Swedish national marine environmental data archive) and
#' optionally unzips them. The function matches provided dataset
#' names against all available SHARK datasets.
#'
#' @param dataset_name Character vector with one or more dataset
#'   names (or partial names). Each entry will be matched against
#'   available SHARK dataset identifiers (e.g.,
#'   `"SHARK_Phytoplankton_2023_SMHI_BVVF"` for a specific dataset,
#'   or `"SHARK_Phytoplankton"` for all Phytoplankton datasets).
#' @param save_dir Directory where zip files (and optionally their
#'   extracted contents) should be stored. Defaults to `""`. If
#'   `NULL` or `""`, the current working directory is used.
#' @param prod Logical, whether to download from the production
#'   (`TRUE`, default) or test (`FALSE`) SHARK server.
#' @param unzip_file Logical, whether to extract downloaded zip
#'   archives (`TRUE`) or only save them (`FALSE`, default).
#' @param verbose Logical, whether to show download and extraction
#'   progress messages. Default is `TRUE`.
#'
#' @return A named list of character vectors. Each element
#'   corresponds to one matched dataset and contains either the
#'   path to the downloaded zip file (if `unzip_file = FALSE`) or
#'   the path to the extraction directory (if `unzip_file = TRUE`).
#'
#' @seealso \url{https://shark.smhi.se} for SHARK database.
#' @seealso [get_shark_options()] for listing available datasets.
#' @seealso [get_shark_data()] for listing available datasets.
#'
#' @examples
#' \dontrun{
#' # Download one dataset to a temporary folder
#' get_shark_datasets("SHARK_Phytoplankton_2023_SMHI_BVVF")
#'
#' # Download multiple datasets and unzip them into the data directory
#' get_shark_datasets(
#'   dataset_name = c("Phytoplankton_2023", "Zooplankton_2022"),
#'   save_dir = "data",
#'   unzip_file = TRUE
#' )
#' }
#'
#' @export
get_shark_datasets <- function(dataset_name,
                               save_dir = "",
                               prod = TRUE,
                               unzip_file = FALSE,
                               verbose = TRUE) {
  # Validate input
  if (missing(dataset_name) || length(dataset_name) == 0) {
    stop("Please provide at least one 'dataset_name' (e.g., 'SHARK_Phytoplankton_2023_PELA_GVVF').")
  }

  # Get complete list of available datasets
  available_datasets <- get_shark_options(prod = prod)$dataset

  # Find all matches
  matched_datasets <- unlist(lapply(dataset_name, function(dn) {
    available_datasets[grepl(dn, available_datasets)]
  }))

  matched_datasets <- unique(matched_datasets)

  if (length(matched_datasets) == 0) {
    stop("No datasets found matching: ", paste(dataset_name, collapse = ", "))
  }

  # Base URL
  base_url <- if (prod) {
    "https://shark.smhi.se/api/dataset/download/"
  } else {
    "https://shark-tst.smhi.se/api/dataset/download/"
  }

  # Connectivity check
  url_short <- sub("api/dataset/download/.*", "", base_url)
  url_response <- try(httr::GET(url_short), silent = TRUE)
  if (inherits(url_response, "try-error") || httr::http_error(url_response)) {
    stop("The SHARK ", ifelse(prod, "PROD", "TEST"),
         " server cannot be reached: ", url_short,
         ". Please check your network connection.")
  }

  # Handle save_dir
  if (is.null(save_dir) || identical(save_dir, "") || nchar(save_dir) == 0) {
    save_dir <- getwd()
  }
  save_dir <- normalizePath(path.expand(save_dir), winslash = "/", mustWork = FALSE)
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Loop over matched datasets
  results <- lapply(matched_datasets, function(md) {
    url <- paste0(base_url, md)
    zip_path <- file.path(save_dir, md)

    if (file.exists(zip_path)) {
      if (verbose) message("Already exists, skipping download: ", zip_path)
    } else {
      if (verbose) message("Downloading zip archive: ", md)
      response <- httr::GET(
        url,
        httr::add_headers("accept" = "application/octet-stream"),
        httr::write_disk(zip_path, overwrite = TRUE),
        if (verbose) httr::progress()
      )

      if (httr::status_code(response) != 200) {
        warning("Failed to download dataset: ", md,
                " HTTP Status: ", httr::status_code(response))
        return(NA_character_)
      }

      if (verbose) message("\nZip file saved at: ", zip_path)
    }

    if (unzip_file) {
      unzip_dir <- file.path(save_dir, tools::file_path_sans_ext(md))
      dir.create(unzip_dir, showWarnings = FALSE, recursive = TRUE)
      utils::unzip(zip_path, exdir = unzip_dir)
      if (verbose) message("Files extracted to: ", unzip_dir)
      return(unzip_dir)
    } else {
      return(zip_path)
    }
  })

  names(results) <- matched_datasets
  return(results)
}

#' Summarize numeric SHARK parameters with ranges and outlier thresholds
#'
#' Downloads SHARK data for a given time period, filters to numeric parameters,
#' and calculates descriptive statistics and Tukey outlier thresholds.
#'
#' By default, the function uses the *previous five complete years*.
#' For example, if called in 2025 it will use data from 2020–2024.
#'
#' @param fromYear Start year for download (numeric).
#'   Defaults to 5 years before the last complete year.
#' @param toYear End year for download (numeric).
#'   Defaults to the last complete year.
#' @param datatype Optional, one or more datatypes to filter on
#'   (e.g. `"Bacterioplankton"`). If `NULL`, all datatypes are included.
#' @param min_obs Minimum number of numeric observations required
#'   for a parameter to be included (default: 3).
#' @param max_non_numeric_frac Maximum allowed fraction of non-numeric values
#'   for a parameter to be kept (default: 0.05).
#' @param verbose Logical, whether to show download progress messages. Default is `TRUE`.
#' @param cache_result Logical, whether to save the result in a persistent cache
#'   (`statistics.rds`) for use by other functions. Default is `FALSE`.
#'
#' @return A tibble with one row per parameter and the following columns:
#' \describe{
#'   \item{parameter}{Parameter name (character).}
#'   \item{min, Q1, median, Q3, max}{Observed quantiles.}
#'   \item{P05, P95}{5th and 95th percentiles.}
#'   \item{IQR}{Interquartile range.}
#'   \item{mean}{Arithmetic mean of numeric values.}
#'   \item{sd}{Standard deviation of numeric values.}
#'   \item{var}{Variance of numeric values.}
#'   \item{cv}{Coefficient of variation (sd / mean).}
#'   \item{mad}{Median absolute deviation.}
#'   \item{mild_lower, mild_upper}{Lower/upper bounds for mild outliers (1.5 × IQR).}
#'   \item{extreme_lower, extreme_upper}{Lower/upper bounds for extreme outliers (3 × IQR).}
#'   \item{n}{Number of numeric observations used.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#'   # Uses previous 5 years automatically
#'   res <- get_shark_statistics()
#'
#'   # Explicitly set years and datatype
#'   res <- get_shark_statistics(2018, 2022, datatype = "Chlorophyll")
#'
#'   # Save result in persistent cache
#'   res <- get_shark_statistics(cache_result = TRUE)
#'
#'   # Print result
#'   print(res)
#' }
get_shark_statistics <- function(fromYear = NULL, toYear = NULL, datatype = NULL,
                                 min_obs = 3, max_non_numeric_frac = 0.05, verbose = TRUE,
                                 cache_result = FALSE) {

  # Set default years
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  last_complete_year <- current_year - 1
  if (is.null(toYear)) toYear <- last_complete_year
  if (is.null(fromYear)) fromYear <- toYear - 4

  if (verbose) {
    message(sprintf("Downloading SHARK data from %d to %d...", fromYear, toYear))
  }

  if (is.null(datatype)) {
    datatype <- c()
  }

  # Download data
  data <- get_shark_data(dataTypes = datatype, fromYear = fromYear, toYear = toYear)

  if (nrow(data) == 0) {
    warning("No data retrieved from SHARK for the specified years and datatype.")
    return(tibble())
  }

  df <- data %>%
    dplyr::select(delivery_datatype, parameter, value) %>%
    dplyr::filter(!is.na(value))

  if (!is.null(datatype)) {
    df <- df %>% dplyr::filter(delivery_datatype %in% datatype)
  }

  # Parse numeric
  df <- df %>% dplyr::mutate(value_num = parse_shark_value(value))

  # Compute non-numeric fractions per parameter
  param_quality <- df %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarise(n_total = n(),
                     n_non_numeric = sum(is.na(value_num)),
                     frac_non_numeric = n_non_numeric / n_total,
                     .groups = "drop")

  # Keep only numeric-like parameters
  keep_params <- param_quality %>%
    dplyr::filter(frac_non_numeric <= max_non_numeric_frac) %>%
    dplyr::pull(parameter)

  df <- df %>% dplyr::filter(parameter %in% keep_params)

  # Summariser
  summarise_param <- function(v) {
    v <- v[!is.na(v)]
    if (length(v) < min_obs) {
      return(tibble(
        min = NA_real_, Q1 = NA_real_, median = NA_real_, Q3 = NA_real_, max = NA_real_,
        P05 = NA_real_, P95 = NA_real_,
        IQR = NA_real_, mean = NA_real_, sd = NA_real_, var = NA_real_, cv = NA_real_,
        mad = NA_real_, mild_lower = NA_real_, mild_upper = NA_real_,
        extreme_lower = NA_real_, extreme_upper = NA_real_, n = length(v)
      ))
    }

    q <- stats::quantile(v, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, type = 7)
    p <- stats::quantile(v, probs = c(0.05, 0.95), na.rm = TRUE, type = 7)

    lowerq <- q[2]; upperq <- q[4]; iqr <- upperq - lowerq

    m <- mean(v)
    s <- stats::sd(v)

    dplyr::tibble(
      min = q[1], Q1 = lowerq, median = q[3], Q3 = upperq, max = q[5],
      P05 = p[1], P95 = p[2],
      IQR = iqr,
      mean = m,
      sd = s,
      var = s^2,
      cv = if (!is.na(m) && m != 0) s / m else NA_real_,
      mad = stats::mad(v, constant = 1),
      mild_lower = lowerq - 1.5 * iqr,
      mild_upper = upperq + 1.5 * iqr,
      extreme_lower = lowerq - 3 * iqr,
      extreme_upper = upperq + 3 * iqr,
      n = length(v)
    )
  }

  # Summarize per parameter -> dataframe
  result_tbl <- df %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarise(dplyr::across(value_num, summarise_param), .groups = "drop")

  # Flatten out nested tibble from summarise_param
  result_tbl <- df %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarise(stats = list(summarise_param(value_num)), .groups = "drop") %>%
    tidyr::unnest(stats) %>%
    dplyr::filter(n >= min_obs)

  # Cache result if requested
  if (cache_result) {
    cache_dir <- file.path(tools::R_user_dir("SHARK4R", "cache"), "perm")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    saveRDS(result_tbl, file = file.path(cache_dir, "statistics.rds"))
    if (verbose) message("Cached SHARK statistics at: ", file.path(cache_dir, "statistics.rds"))
  }

  return(result_tbl)
}

# Helper: parse SHARK values to numeric
parse_shark_value <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub(",", ".", x)              # fix decimal commas
  x <- gsub("^\\s*[<>~=]+\\s*", "", x) # drop <, >, = signs
  x <- gsub("[^0-9eE+\\-\\.]", "", x) # keep only numeric chars
  x[nzchar(x) == FALSE] <- NA_character_
  suppressWarnings(as.numeric(x))
}
