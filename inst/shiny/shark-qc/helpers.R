# --- Function to load shark options
load_shark_options <- function(prod) {
  shark_options <- SHARK4R::get_shark_options(prod)
  list(
    types = shark_options$dataTypes,
    datasets = shark_options$datasets
  )
}

safe_datatable <- function(x) {
  # Only create a datatable if x is a data.frame or matrix
  if (is.data.frame(x) || is.matrix(x)) {
    DT::datatable(x,
                  style = "bootstrap",
                  rownames = FALSE,
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    autoWidth = TRUE
                  ))
  } else {
    # Otherwise, show a message or an empty table
    DT::datatable(data.frame(Message = "No data available"),
                  style = "bootstrap")
  }
}
