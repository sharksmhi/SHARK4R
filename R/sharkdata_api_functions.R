#' Load all dataset names available at SHARKdata from SMHI
#'
#' This function loads all datasets listed in SHARK at SMHI by calling the [SHARKdata API](https://sharkdata.smhi.se/).
#'
#' @return A data frame containing all datasets available in SHARKdata
#'
#' @seealso \code{\link{load_dataset_types}}, \code{\link{load_dataset_names}}, [SHARKdata](https://sharkdata.smhi.se/), [SHARKweb](https://sharkweb.smhi.se/)
#'
#' @examples
#' \dontrun{
#' all_dataset_names <- load_sharkdata()
#' }
#'
#' @export
load_sharkdata <- function() {
  fromJSON('https://sharkdata.smhi.se/datasets/list.json')
}

#' List available dataset types in SHARKdata
#'
#' This function lists the available dataset types from the [SHARKdata API](https://sharkdata.smhi.se/).
#'
#' @return A vector of unique dataset types.
#'
#' @seealso \code{\link{load_sharkdata}}, \code{\link{load_dataset_names}}, [SHARKdata](https://sharkdata.smhi.se/), [SHARKweb](https://sharkweb.smhi.se/)
#' 
#' @examples
#' \dontrun{
#' all_dataset_types <- load_dataset_types()
#' }
#'
#' @export
load_dataset_types <- function() {
  datasets <- load_sharkdata()
  dataset_types <- unique(datasets$datatype)
  return(dataset_types)
}

#' Load dataset names from SHARKdata based on type, year, and data deliverer
#'
#' This function gets dataset names fron the [SHARKdata API](https://sharkdata.smhi.se/) based on the specified data type, year, and data deliverer.
#'
#' @param dataset_type The type of dataset to load.
#' @param year The year to filter datasets based on dataset name (default NA results in all available years). Please note that dataset names can contain multiple years (e.g. a dataset with data from 2012-2014 will not be selected when specifying year 2013 in argument). Consider filter data after download for more extensive data.
#' @param data_deliverer The data deliverer (code) to filter datasets (default is NA, results in all deliverers). See [SMHI codelist](http://smhi.se/oceanografi/oce_info_data/shark_web/downloads/codelist_SMHI.xlsx) for available codes. Consider filter data after download for more extensive data.
#' @return A data frame containing filtered dataset names.
#'
#' @seealso \code{\link{load_sharkdata}}, \code{\link{load_dataset_types}}, [SHARKdata](https://sharkdata.smhi.se/), [SHARKweb](https://sharkweb.smhi.se/)
#'
#' @examples
#' \dontrun{
#' # All Zoobenthos datasets
#' load_dataset_names("Zoobenthos")
#' 
#' # All Phytoplankton datasets from 2012:2020 from SMHI and UMSC.
#' load_dataset_names("Phytoplankton", year = 2012:2020, data_deliverer = c("SMHI", "UMSC"))
#' }
#'
#' @export
load_dataset_names <- function(dataset_type, year = NA, data_deliverer = NA) {
  datasets <- load_sharkdata()
  
  filtered_datasets <- datasets %>%
    filter(datatype == dataset_type) %>%
    year_filter(year) %>%
    data_deliverer_filter(data_deliverer)
  
  return(filtered_datasets)
}

#' Filter datasets based on year
#'
#' This internal function filters datasets based on the specified year.
#'
#' @param datasets The input datasets to filter.
#' @param year The year to filter datasets.
#' @return A filtered data frame.
#'
#' @keywords internal
year_filter <- function(datasets, year) {
  if (!any(is.na(year))) {
    datasets %>% filter(grepl(paste(year, collapse="|"), dataset_name))
  } else {
    datasets
  }
}

#' Filter datasets based on data deliverer
#'
#' This internal function filters datasets based on the specified data deliverer.
#'
#' @param datasets The input datasets to filter.
#' @param data_deliverer The data deliverer to filter datasets.
#' @return A filtered data frame.
#'
#' @keywords internal
data_deliverer_filter <- function(datasets, data_deliverer) {
  if (!any(is.na(data_deliverer))) {
    datasets %>% filter(grepl(paste(data_deliverer, collapse="|"), dataset_name))
  } else {
    datasets
  }
}

#' Download and process SHARKdata datasets
#'
#' This function downloads and processes datasets from the [SHARKdata API](https://sharkdata.smhi.se/) based on the specified dataset names.
#'
#' @param dataset_names A character vector specifying the names of the datasets to be downloaded and processed.
#'
#' @return A processed data frame containing the SHARKdata datasets.
#'
#' @details
#' This function performs the following steps:
#' 1. Downloads datasets with the specified names.
#' 2. Reads and processes the downloaded data.
#' 3. Converts columns to appropriate types.
#' 4. Adds Dyntaxa higher taxonomy information to biological data from stored file. Current taxonomy can be accessed via the Dyntaxa API by, see \code{\link{update_dyntaxa_taxonomy}}
#' 5. Adds WoRMS higher taxonomy information to biological data from stored file. Current taxonomy can be accessed via the WoRMS API by, see \code{\link{update_worms_taxonomy}}
#' 6. Extracts and adds geographical information (e.g. location_sea_basin) from coordinates.
#'
#' @seealso \code{\link{load_dataset_names}}, \code{\link{update_dyntaxa_taxonomy}}, \code{\link{update_worms_taxonomy}}, [SHARKdata](https://sharkdata.smhi.se/), [SHARKweb](https://sharkweb.smhi.se/)
#'
#' @examples
#' \dontrun{
#' datasets <- load_dataset_names("Phytoplankton")
#' dataset_names <- unique(datasets$dataset_name)
#' data <- download_sharkdata(dataset_names)
#' 
#' print(data)
#' }
#'
#' @export
download_sharkdata <- function(dataset_names) {
  datasets <- load_sharkdata()
  filtered_datasets <- datasets %>%
    filter(dataset_name %in% dataset_names)
  
  data_shark <- data.frame()
  
  if (length(unique(filtered_datasets$datatype)) > 1) {
    stop("Only one datatype can be downloaded at a time")
  } else {
    validate_dataset_names(filtered_datasets, dataset_names)
    
    # Set up progress bar
    pb <- txtProgressBar(min = 0, max = length(dataset_names), style = 3)
    
    for (i in 1:length(dataset_names)) {
      temp <- download_file(dataset_names[i])
      data_ix <- read_data(temp, filtered_datasets, dataset_names[i])
      
      # Check if dataset_file_name has unique values
      if (length(unique(data_ix$dataset_file_name)) == 1) {
        # Convert all columns to character
        data_ix <- data_ix %>%
          mutate(across(everything(), as.character))
        
        data_shark <- bind_rows(data_shark, data_ix)
      } else {
        warning("Skipping dataset ", dataset_names[i], " due to missing or multiple dataset_file_name entries.")
      }
      
      # Update progress bar at the end of each iteration
      setTxtProgressBar(pb, i)
    }
  }
  
  # Close progress bar
  close(pb)
  
  # Automatically convert columns to appropriate types
  data_shark <- type_convert(data_shark, col_types = cols())
  
  if ("dyntaxa_id" %in% names(data_shark)) {
    # Add Dyntaxa higher taxonomy from file
    data_shark <- data_shark %>% 
      left_join(load_dyntaxa_taxonomy(data_shark$dyntaxa_id), by = "dyntaxa_id")
  }
  
  if ("aphia_id" %in% names(data_shark)) {
  # Add WoRMS higher taxonomy from file
    data_shark <- data_shark %>% 
      left_join(load_worms_taxonomy(data_shark$aphia_id), by = "aphia_id")
  }
  
  # Extract geographical information from coordinates
  geographical_info <- get_geographical_info(data_shark$sample_latitude_dd, 
                                             data_shark$sample_longitude_dd) %>%
    distinct()
  
  # Join data with geographical information 
  data_shark <- data_shark %>%
    left_join(geographical_info,
              by = c("sample_latitude_dd", "sample_longitude_dd"))
  
  return(data_shark)
}

#' Helper function to download file
#'
#' This internal function downloads a file based on the specified dataset name.
#'
#' @param dataset_name The name of the dataset to download.
#' @return A temporary file path.
#'
#' @keywords internal
download_file <- function(dataset_name) {
  temp <- tempfile()
  download.file(paste("https://sharkdata.smhi.se/datasets/",
                      dataset_name,
                      "/shark_archive.zip",
                      sep = ""),
                temp,
                mode = "wb",
                quiet = TRUE)
  return(temp)
}

#' Helper function to read data
#'
#' This internal function reads and converts data from a temporary file.
#'
#' @param temp The temporary file path.
#' @param filtered_datasets The filtered datasets.
#' @param dataset_name The name of the dataset.
#' @return A data frame containing the read and converted data.
#'
#' @keywords internal
read_data <- function(temp, filtered_datasets, dataset_name) {
  dataset_file_name <- filtered_datasets$dataset_file_name[filtered_datasets$dataset_name == dataset_name]
  
  data_ix <- read_delim(unz(temp, "shark_data.txt"),
                        locale = locale(encoding = "ISO8859-1"),
                        col_types = cols(),
                        progress = FALSE)
  
  data_ix$dataset_file_name <- dataset_file_name
  return(data_ix)
}

#' Helper function to validate dataset names
#'
#' This internal function validates whether all specified dataset names are in the database.
#'
#' @param filtered_datasets A data frame of filtered datasets.
#' @param dataset_names A vector of dataset names to validate.
#' @return NULL. It stops the execution with an error message if validation fails.
#'
#' @keywords internal
validate_dataset_names <- function(filtered_datasets, dataset_names) {
  if (!all(dataset_names %in% filtered_datasets$dataset_name)) {
    stop("Not all dataset names are in the database")
  }
}

#' Load higher taxonomy from Dyntaxa file
#'
#' This internal function loads higher taxonomy information from the Dyntaxa file.
#'
#' @param dyntaxa_id_input A vector of Dyntaxa IDs to filter the higher taxonomy.
#' @return A data frame containing higher taxonomy information.
#'
#' @seealso \code{\link{load_worms_taxonomy}}, \code{\link{update_dyntaxa_taxonomy}}, \code{\link{update_worms_taxonomy}}, [SHARKdata](https://sharkdata.smhi.se/), [SHARKweb](https://sharkweb.smhi.se/)
#'
#' @keywords internal
load_dyntaxa_taxonomy <- function(dyntaxa_id_input) {
  shark_species_list <- read_species_list()
  species <- gather_species_info(shark_species_list)
  shark_species_list <- add_species_info(shark_species_list, species) %>%
    filter(dyntaxa_id %in% dyntaxa_id_input)

  return(shark_species_list)
}

#' Read and convert species list
#'
#' This internal function reads and converts a species list from the specified file.
#'
#' @param filename The name of the file containing the species list.
#' @return A data frame containing the species list with converted columns.
#'
#' @keywords internal
read_species_list <- function(filename) {
  read_delim(system.file("extdata", "shark_species_list_1252.txt", package = "SHARK4R"), 
             delim = "\t", 
             col_names = TRUE, 
             col_types = cols(),
             progress = FALSE,
             locale = readr::locale(encoding = "windows-1252")) %>%
    select(taxon_id,
           rank,
           scientific_name,
           Class,
           Order,
           Family,
           Genus,
           taxon_hierarchy) %>%
    rename("dyntaxa_id" = taxon_id,
           "taxon_name" = scientific_name,
           "taxon_class" = Class,
           "taxon_order" = Order,
           "taxon_family" = Family,
           "taxon_genus" = Genus)
}

#' Gather species information
#'
#' This internal function gathers species information from the shark species list.
#'
#' @param shark_species_list A data frame containing shark species information.
#' @return A data frame containing gathered species information.
#'
#' @keywords internal
gather_species_info <- function(shark_species_list) {
  species <- shark_species_list %>%
    filter(rank == "species") %>%
    select(taxon_name) %>%
    mutate(taxon_species = taxon_name)
  
  species_form <- shark_species_list %>%
    filter(rank %in% c("variety", "form")) %>%
    select(rank, taxon_name) %>%
    mutate(taxon_species = word(taxon_name, 1, 2))
  
  species <- bind_rows(species, species_form) %>%
    select(-rank) %>%
    distinct()
  
  return(species)
}

#' Helper function to add species information to species list
#'
#' This internal function adds species information to the shark species list.
#'
#' @param shark_species_list A data frame containing shark species information.
#' @param species A data frame containing gathered species information.
#' @return A data frame containing the shark species list with added species information.
#'
#' @keywords internal
add_species_info <- function(shark_species_list, species) {
  shark_species_list <- shark_species_list %>%
    left_join(species, by = "taxon_name") %>%
    relocate(taxon_species, .after = taxon_genus) %>%
    select(-rank)
  
  return(shark_species_list)
}

#' Load higher taxonomy from WoRMS file
#'
#' This internal function loads higher taxonomy information from the stored WoRMS taxonomy file.
#'
#' @param aphia_id_input A vector of Aphia IDs to filter the higher taxonomy.
#' @return A data frame containing higher taxonomy information.
#'
#' @seealso \code{\link{load_dyntaxa_taxonomy}}, \code{\link{update_dyntaxa_taxonomy}}, \code{\link{update_worms_taxonomy}}, [SHARKdata](https://sharkdata.smhi.se/), [SHARKweb](https://sharkweb.smhi.se/)
#'
#' @keywords internal
load_worms_taxonomy <- function(aphia_id_input) {
  taxa_worms <- read_delim(system.file("extdata", "taxa_worms_1252.txt", package = "SHARK4R"),
                           delim = "\t",
                           col_names = TRUE,
                           col_types = cols(),
                           progress = FALSE,
                           locale = readr::locale(encoding = "windows-1252")) %>%
    select(aphia_id,
           rank,
           scientific_name,
           class,
           order,
           family,
           genus) %>%
    rename("worms_class" = class,
           "worms_order" = order,
           "worms_family" = family,
           "worms_genus" = genus)
  
  species <- gather_worms_species_info(taxa_worms)
  taxa_worms <- add_worms_species_info(taxa_worms, species) %>%
    filter(aphia_id %in% aphia_id_input)
  
  return(taxa_worms)
}

#' Gather WoRMS species information
#'
#' This internal function gathers WoRMS species information from the taxa_worms data frame.
#'
#' @param taxa_worms A data frame containing WoRMS species information.
#' @return A data frame containing gathered WoRMS species information.
#'
#' @keywords internal
gather_worms_species_info <- function(taxa_worms) {
  species <- taxa_worms %>%
    filter(rank == "Species") %>%
    select(scientific_name, aphia_id) %>%
    rename(worms_species = scientific_name)
  
  return(species)
}

#' Function to add WoRMS species information to taxa list
#'
#' This internal function adds WoRMS species information to the taxa_worms data frame.
#'
#' @param taxa_worms A data frame containing WoRMS taxonomy information.
#' @param species A data frame containing gathered WoRMS species information.
#' @return A data frame containing the taxa_worms with added WoRMS species information.
#'
#' @keywords internal
add_worms_species_info <- function(taxa_worms, species) {
  taxa_worms <- taxa_worms %>%
    left_join(species, by = "aphia_id") %>%
    relocate(worms_species, .after = worms_genus) %>%
    select(-rank, -scientific_name)
  
  return(taxa_worms)
}

#' Get geographical information from coordinates
#'
#' This function enriches the provided data with geographical information based on latitude and longitude coordinates in the Baltic Sea, Kattegat and Skagerrak.
#'
#' @param latitude_dd A numeric vector representing latitude coordinates in decimal degrees.
#' @param longitude_dd A numeric vector representing longitude coordinates in decimal degrees.
#'
#' @return A data frame containing the original coordinates with the added geographical information location_type_area, location_svar_sea_area_name and location_sea_basin.
#'
#' @details
#' The function reads shapefiles and basin names to assign geographical information to the dataset.
#' It provides details about the location type area and the Svar Sea area name based on the given coordinates.
#'
#' @seealso [SHARKdata](https://sharkdata.smhi.se/), [SHARKweb](https://sharkweb.smhi.se/)
#'
#' @examples
#' \dontrun{
#' latitude <- c(58.25830, 58.59367, 63.49983)
#' longitude <- c(11.43330, 18.23550, 19.81933)
#' dataset_with_geo_info <- get_geographical_info(latitude, longitude)
#' }
#' 
#' @export
get_geographical_info <- function(latitude_dd, longitude_dd) {

  # Read shapefiles and list of basin names
  layer <- st_read(system.file("extdata/sharkweb_shapefiles", "Havsomr_SVAR_2016_3b_CP1252.shp", package = "SHARK4R"), 
                   options = "ENCODING=WINDOWS-1252", 
                   quiet = TRUE)
  
  # Read translation list of sea basin names
  basin_names <- read_delim(system.file("extdata", "sea_basin_1252.txt", package = "SHARK4R"), 
                            delim = "\t", 
                            col_names = TRUE, 
                            progress = FALSE,
                            locale = locale(encoding = "windows-1252"),
                            col_types = cols())
  
  # Set CRS of basin layer and transform
  layer <- st_set_crs(layer, 3006) %>%
    st_transform(4326)
  
  # Add geometry information to data
  data <- data.frame("sample_latitude_dd" = latitude_dd, 
                     "sample_longitude_dd" = longitude_dd) %>%
    mutate(lon = sample_longitude_dd,
           lat = sample_latitude_dd)
  
  # Gather all unique positions and convert to sf
  points_sf <- st_as_sf(data %>%
                          distinct(),
                        coords = c("lon", "lat"),
                        crs = st_crs(layer))
  
  # Assign geo info by position
  data_basin <- data %>%
    group_by(sample_latitude_dd, sample_longitude_dd) %>%
    left_join(st_join(points_sf, layer) %>%
                select(sample_latitude_dd, sample_longitude_dd, TYPOMRNAMN, NAMN, BASIN_NR),
              by = c("sample_latitude_dd", "sample_longitude_dd")) %>%
    ungroup() %>%
    left_join(basin_names, by = "BASIN_NR") %>%
    select(-lon, -lat, -BASIN_NR, -geometry) %>%
    rename(location_type_area = TYPOMRNAMN,
           location_svar_sea_area_name = NAMN)
  
  return(data_basin)
}

#' Check if dataset versions are up to date
#'
#' This function checks the status of dataset versions by comparing them with the latest available versions.
#'
#' @param dataset_file_name A character vector containing the dataset file names (with the .zip extension).
#'
#' @return A data frame indicating the status of dataset versions, including whether they are up to date.
#'
#' @details
#' This function compares the provided dataset file names with the latest versions available in the [SHARKdata](https://sharkdata.smhi.se/) repository.
#' It returns a data frame with information about each dataset, specifying whether it is up to date or not.
#'
#' @seealso \code{\link{load_dataset_names}}, \code{\link{download_sharkdata}}, \code{\link{update_data}}, [SHARKdata](https://sharkdata.smhi.se/), [SHARKweb](https://sharkweb.smhi.se/)
#'
#' @examples
#' # Example with a list of dataset file names
#' \dontrun{
#' datasets <- load_dataset_names("Phytoplankton")
#' dataset_names <- unique(datasets$dataset_name[1:5])
#' data <- download_sharkdata(dataset_names)
#' status_list <- check_data_version(data$dataset_file_name)
#' }
#' 
#' @export
check_data_version <- function(dataset_file_name) {
  dataset_name <- gsub("_version_.*", "", dataset_file_name)
  
  # Load available datasets
  datasets <- load_sharkdata() %>%
    filter(dataset_name %in% dataset_name)
  
  # Create a status list
  status_list <- data.frame(dataset_name = unique(dataset_name), 
                            dataset_file_name = unique(dataset_file_name),
                            up_to_date = unique(dataset_file_name) %in% datasets$dataset_file_name)
  
  return(status_list)
}

#' Function to update data if a more recent version is available at SHARKdata
#'
#' This function updates the data if an updated version is available at [SHARKdata](https://sharkdata.smhi.se/) by downloading the latest datasets.
#'
#' @param data A data frame containing the current data.
#' @return A data frame with updated data if available.
#'
#' @seealso \code{\link{load_dataset_names}}, \code{\link{download_sharkdata}}, \code{\link{check_data_version}}, [SHARKdata](https://sharkdata.smhi.se/), [SHARKweb](https://sharkweb.smhi.se/)
#'
#' @examples
#' # Assume you have a sharkdata dataset named 'data' loaded in your R environment
#' # with a 'dataset_file_name' column indicating the file name of the dataset.
#' \dontrun{
#' datasets <- load_dataset_names("Phytoplankton")
#' dataset_names <- unique(datasets$dataset_name[1:5])
#' data <- download_sharkdata(dataset_names)
#' updated_data <- update_data(data)
#' }
#'
#' @export
update_data <- function(data) {
  
  status_list <- check_data_version(data$dataset_file_name)
  
  if (any(status_list$up_to_date == FALSE)) {
    datasets_to_update <- status_list %>%
      filter(up_to_date == FALSE)
    
    data <- filter_outdated_datasets(data, datasets_to_update)
    updated_data <- download_sharkdata(datasets_to_update$dataset_name)
    
    # Convert to character to enable bind
    updated_data <- updated_data %>%
      mutate(across(everything(), as.character))
    
    # Convert to character to enable bind
    data <- data %>%
      mutate(across(everything(), as.character))
    
    # Bind data together
    data <- bind_rows(data, updated_data)
    
    # Automatically convert columns to appropriate types
    updated_data <- type_convert(data, col_types = cols())
    
    return(data)
  } else {
    message("Data are already up to date")
  }
}

#' Function to filter outdated datasets
#'
#' This internal function filters out outdated datasets from the current data based on the provided list of datasets to update.
#'
#' @param data A data frame containing the current data.
#' @param datasets_to_update A data frame containing information about datasets that need to be updated.
#' @return A filtered data frame excluding outdated datasets.
#'
#' @keywords internal
filter_outdated_datasets <- function(data, datasets_to_update) {
  data %>%
    filter(!dataset_name %in% datasets_to_update$dataset_name)
}
