# Skip test if a remote resource is not responding
skip_if_resource_unavailable <- function(url, msg = NULL) {
  ok <- tryCatch({
    resp <- httr::GET(url, httr::timeout(5), httr::config(nobody = TRUE))
    httr::status_code(resp) < 400
  }, error = function(e) FALSE)

  if (!ok) {
    if (is.null(msg)) {
      msg <- paste("Resource not responding:", url)
    }
    testthat::skip(msg)
  }
}
