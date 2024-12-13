% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algaebase_api_functions.R
\name{check_algaebase_api}
\alias{check_algaebase_api}
\title{Check AlgaeBase API Operational Status}
\usage{
check_algaebase_api(apikey = NULL, genus_id = 43375)
}
\arguments{
\item{apikey}{A string. The API key for accessing the AlgaeBase API. Defaults to \code{NULL}.}

\item{genus_id}{A numeric value. The unique genus ID used to test the API endpoint.
Default is \code{43375}, corresponding to the \code{Haematococcus} genus record in AlgaeBase.}
}
\value{
A logical value: \code{TRUE} if the API is operational, \code{FALSE} otherwise.
}
\description{
Internal function to verify whether the AlgaeBase API is operational.
It sends a request to a stable genus endpoint to confirm API availability.
}
\details{
This function performs a GET request to the AlgaeBase API using a stable genus ID
to ensure that the API is accessible and that the provided API key is valid.
It is used internally to prevent unnecessary queries when the API is unavailable.
}
\examples{
\dontrun{
# Check API status with an API key
check_algaebase_api(apikey = "your_api_key")
}

}
\keyword{internal}
