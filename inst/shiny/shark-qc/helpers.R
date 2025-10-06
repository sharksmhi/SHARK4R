# --- Function to load shark options
load_shark_options <- function() {
  shark_options <- get_shark_options()
  list(
    types = shark_options$dataTypes,
    datasets = shark_options$datasets
  )
}
