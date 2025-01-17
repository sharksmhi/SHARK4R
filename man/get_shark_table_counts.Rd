% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shark_api_functions.R
\name{get_shark_table_counts}
\alias{get_shark_table_counts}
\title{Retrieve SHARK Data Table Row Counts}
\usage{
get_shark_table_counts(
  tableView = "sharkweb_overview",
  fromYear = 2019,
  toYear = 2020,
  months = c(),
  dataTypes = c(),
  parameters = c(),
  orderers = c(),
  qualityFlags = c(),
  deliverers = c(),
  projects = c(),
  datasets = c(),
  minSamplingDepth = "",
  maxSamplingDepth = "",
  checkStatus = "",
  redListedCategory = c(),
  taxonName = c(),
  stationName = c(),
  vattenDistrikt = c(),
  seaBasins = c(),
  counties = c(),
  municipalities = c(),
  waterCategories = c(),
  typOmraden = c(),
  helcomOspar = c(),
  seaAreas = c(),
  prod = TRUE
)
}
\arguments{
\item{tableView}{Character. Specifies the view of the table to retrieve. Options include:
\itemize{
\item \code{"sharkweb_overview"}: Overview table
\item \code{"sharkweb_all"}: All available columns
\item \code{"sharkdata_bacterioplankton"}: Bacterioplankton table
\item \code{"sharkdata_chlorophyll"}: Chlorophyll table
\item \code{"sharkdata_epibenthos"}: Epibenthos table
\item \code{"sharkdata_greyseal"}: Greyseal table
\item \code{"sharkdata_harbourporpoise"}: Harbour porpoise table
\item \verb{"sharkdata_harbourseal}: Harbour seal table
\item \code{"sharkdata_jellyfish"}: Jellyfish table
\item \code{"sharkdata_physicalchemical"}: Physical chemical table
\item \code{"sharkdata_physicalchemical_columns"}: Physical chemical table: column view
\item \code{"sharkdata_phytoplankton"}: Phytoplankton table
\item \code{"sharkdata_picoplankton"}: Picoplankton table
\item \code{"sharkdata_planktonbarcoding"}: Plankton barcoding table
\item \code{"sharkdata_primaryproduction"}: Primary production table
\item \code{"sharkdata_ringedseal"}: Ringed seal table
\item \code{"sharkdata_sealpathology"}: Seal pathology table
\item \code{"sharkdata_sedimentation"}: Sedimentation table
\item \code{"sharkdata_zoobenthos"}: Zoobenthos table
\item \code{"sharkdata_zooplankton"}: Zooplankton table
\item \code{"report_sum_year_param"}: Report sum per year and parameter
\item \code{"report_sum_year_param_taxon"}: Report sum per year, parameter and taxon
\item \code{"report_sampling_per_station"}: Report sampling per station
\item \code{"report_obs_taxon"}: Report observed taxa
\item \code{"report_stations"}: Report stations
\item \code{"report_taxon"}: Report taxa
}
Default is \code{"sharkweb_overview"}.}

\item{fromYear}{Integer. The starting year for the data to retrieve. Default is \code{2019}.}

\item{toYear}{Integer. The ending year for the data to retrieve. Default is \code{2020}.}

\item{months}{Integer vector. The months to retrieve data for (e.g., \code{c(4, 5, 6)} for April to June).}

\item{dataTypes}{Character vector. Specifies data types to filter, such as \code{"Chlorophyll"} or \code{"Epibenthos"}.}

\item{parameters}{Character vector. Optional. Parameters to filter results, such as \code{"Chlorophyll-a"}.}

\item{orderers}{Character vector. Optional. Orderers to filter data by specific organizations.}

\item{qualityFlags}{Character vector. Optional. Quality flags to filter data.}

\item{deliverers}{Character vector. Optional. Deliverers to filter data by data providers.}

\item{projects}{Character vector. Optional. Projects to filter data by specific research or monitoring projects.}

\item{datasets}{Character vector. Optional. Datasets to filter data by specific dataset names.}

\item{minSamplingDepth}{Numeric. Optional. Minimum depth (in meters) for sampling data.}

\item{maxSamplingDepth}{Numeric. Optional. Maximum depth (in meters) for sampling data.}

\item{checkStatus}{Character string. Optional. Status check to filter results.}

\item{redListedCategory}{Character vector. Optional. Red-listed taxa for conservation filtering.}

\item{taxonName}{Character vector. Optional. Taxa names for filtering specific species or taxa.}

\item{stationName}{Character vector. Optional. Station names to retrieve data from specific stations.}

\item{vattenDistrikt}{Character vector. Optional. Water district names to filter data by Swedish water districts.}

\item{seaBasins}{Character vector. Optional. Sea basin names to filter data by different sea areas.}

\item{counties}{Character vector. Optional. Counties to filter data within specific administrative regions in Sweden.}

\item{municipalities}{Character vector. Optional. Municipalities to filter data within specific local regions.}

\item{waterCategories}{Character vector. Optional. Water categories to filter data by.}

\item{typOmraden}{Character vector. Optional. Type areas to filter data by specific areas.}

\item{helcomOspar}{Character vector. Optional. HELCOM or OSPAR areas for regional filtering.}

\item{seaAreas}{Character vector. Optional. Sea area codes for filtering by specific sea areas.}

\item{prod}{Logical. Query against PROD or TEST (SMHI internal) server. Default is \code{TRUE} (PROD).}
}
\description{
The \code{get_shark_table_counts} function retrieves the row counts of data records from various SHARK data tables.
To view available filter options, see \code{\link{get_shark_options}}.
}
\examples{
\dontrun{
  # Retrieve chlorophyll data for April to June from 2019 to 2020
  shark_data_counts <- get_shark_table_counts(fromYear = 2019, toYear = 2020,
                                              months = c(4, 5, 6), dataTypes = c("Chlorophyll"))
  print(shark_data_counts)
}

}
\seealso{
\code{\link{get_shark_options}}
}
