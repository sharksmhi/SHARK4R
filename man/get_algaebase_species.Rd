% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algaebase_api_functions.R
\name{get_algaebase_species}
\alias{get_algaebase_species}
\title{Algaebase Species Search}
\usage{
get_algaebase_species(
  genus,
  species,
  apikey,
  higher = TRUE,
  unparsed = FALSE,
  newest_only = TRUE,
  exact_matches_only = TRUE
)
}
\arguments{
\item{genus}{A character string specifying the genus name.}

\item{species}{A character string specifying the species or specific epithet.}

\item{apikey}{A character string containing the API key for accessing the Algaebase API.}

\item{higher}{A logical value indicating whether to include higher taxonomy details (default is \code{TRUE}).}

\item{unparsed}{A logical value indicating whether to print the full JSON response from the API (default is \code{FALSE}).}

\item{newest_only}{A logical value indicating whether to return only the most recent entries (default is \code{TRUE}).}

\item{exact_matches_only}{A logical value indicating whether to return only exact matches (default is \code{TRUE}).}
}
\value{
A data frame with details about the species, including taxonomic status, ranks, and other relevant information.
}
\description{
This function searches the Algaebase API for species based on genus and species names.
It allows for flexible search parameters such as filtering by exact matches, returning
the most recent results, and including higher taxonomy details.
}
\details{
This function queries the Algaebase API for species based on the genus and species names,
and filters the results based on various parameters. The function handles different taxonomic ranks
and formats the output for easy use. It can merge higher taxonomy data if requested.
}
\examples{
\dontrun{
# Search for a species with exact matches only, return the most recent results
result <- get_algaebase_species(
  genus = "Skeletonema", species = "marinoi", apikey = "your_api_key"
)

# Print result
print(result)
}

}
