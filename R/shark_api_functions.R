#' Retrieve and Format SHARK Data from SMHI API
#'
#' The `get_shark_table` function retrieves data from the SHARK database hosted by SMHI, with options
#' to filter based on year range, months, and data types. It sends a POST request to
#' the SHARK API and returns the results as a structured `data.frame`.
#'
#' @param table_view Character. Specifies the view of the table to retrieve. Options include:
#' \itemize{
#'   \item `"sharkweb_overview"`
#'   \item `"sharkweb_all"`
#'   \item `"sharkdata_bacterioplankton"`
#'   \item `"sharkdata_chlorophyll"`
#'   \item `"sharkdata_epibenthos"`
#'   \item `"sharkdata_greyseal"`
#'   \item `"sharkdata_harbourporpoise"`
#'   \item `"sharkdata_harbourseal"`
#'   \item `"sharkdata_jellyfish"`
#'   \item `"sharkdata_physicalchemical_columns"`
#'   \item `"sharkdata_phytoplankton"`
#'   \item `"sharkdata_picoplankton"`
#'   \item `"sharkdata_planktonbarcoding"`
#'   \item `"sharkdata_primaryproduction"`
#'   \item `"sharkdata_ringedseal"`
#'   \item `"sharkdata_sealpathology"`
#'   \item `"sharkdata_sedimentation"`
#'   \item `"sharkdata_zoobenthos"`
#'   \item `"sharkdata_zooplankton"`
#'   \item `"report_sum_year_param"`
#'   \item `"report_sum_year_param_taxon"`
#'   \item `"report_sampling_per_station"`
#'   \item `"report_obs_taxon"`
#'   \item `"report_stations"`
#'   \item `"report_taxon"`
#' }
#' Default is `"sharkweb_overview"`.
#'
#' @param limit Integer. Maximum number of records to retrieve per request. Default is `200`.
#'
#' @param offset Integer. The starting point for data retrieval, useful for pagination. Default is `0`.
#'
#' @param header_lang Character. Language option for column headers. Possible values:
#' \itemize{
#'   \item `"sv"`: Swedish
#'   \item `"en"`: English
#'   \item `"short"`: Shortened version
#'   \item `"internal_key"`: Internal key (default)
#' }
#'
#' @param from_year Integer. The starting year for the data to retrieve. Default is `2019`.
#'
#' @param to_year Integer. The ending year for the data to retrieve. Default is `2020`.
#'
#' @param months Integer vector. The months to retrieve data for, e.g., `c(4, 5, 6)` for April to June.
#'
#' @param data_types Character vector. Specifies data types to filter, such as `"Chlorophyll"` or `"Epibenthos"`. See \link{get_shark_options} for available data types.
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
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @import tidyr
#'
#' @examples
#' \dontrun{
#'   # Retrieve chlorophyll data for April to June from 2019 to 2020
#'   shark_data <- get_shark_table(from_year = 2019, to_year = 2020,
#'                                months = c(4, 5, 6), data_types = c("Chlorophyll"))
#'   View(shark_data)
#' }
#'
#' @export
get_shark_table <- function(table_view = "sharkweb_overview", limit = 200, offset = 0,
                            header_lang = "internal_key", from_year = 2019, to_year = 2020,
                            months = c(4, 5, 6), data_types = c("Chlorophyll", "Epibenthos")) {
  
  # Define the URL
  url <- "https://shark.smhi.se/api/sample/table"

  # Create the JSON body as a list
  body <- list(
    params = list(
      tableView = table_view,
      limit = limit,
      offset = offset,
      headerLang = header_lang
    ),
    query = list(
      bounds = list(),
      fromYear = from_year,
      toYear = to_year,
      months = months,
      dataTypes = data_types,
      parameters = list(),
      checkStatus = "",
      qualityFlags = list(),
      deliverers = list(),
      orderers = list(),
      projects = list(),
      datasets = list(),
      minSamplingDepth = "",
      maxSamplingDepth = "",
      redListedCategory = list(),
      taxonName = list(),
      stationName = list(),
      vattenDistrikt = list(),
      seaBasins = list(),
      counties = list(),
      municipalities = list(),
      waterCategories = list(),
      typOmraden = list(),
      helcomOspar = list(),
      seaAreas = list()
    )
  )

  # Convert body to JSON
  body_json <- toJSON(body, auto_unbox = TRUE)

  # Make the POST request
  response <- POST(url,
                   add_headers("accept" = "application/json",
                               "Content-Type" = "application/json"),
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
#' @return A `data.frame` containing the available search options from the SHARK API.
#'
#' @details This function sends a GET request to the SHARK API options endpoint to retrieve available search filters and options
#' for querying the database. The API returns data in JSON format, which is then parsed into a `data.frame`.
#'
#' @import httr
#' @import jsonlite
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
#'   data_types <- unlist(shark_options$dataTypes)
#'   print(data_types)
#' }
#'
#' @export
get_shark_options <- function() {
  # Define the URL for options
  url <- "https://shark.smhi.se/api/options"
  
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
