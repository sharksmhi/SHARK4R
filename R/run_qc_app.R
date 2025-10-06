#' Launch the SHARK4R Bio-QC Tool
#'
#' This function launches the interactive Shiny application for performing
#' quality control (QC) on SHARK data. The application provides a graphical
#' interface for exploring and validating data before or after submission to SHARK.
#'
#' @param additional_packages Character vector of additional package names
#'   required by the app. These will be checked alongside the core dependencies.
#'   Defaults to `NULL`.
#' @param interactive Logical value whether the session is interactive or not.
#'
#' @details
#' The function checks that all required packages for the app are installed
#' before launching. If any are missing, the user is notified. In interactive
#' sessions, the function will prompt whether the missing packages should be
#' installed automatically. In non-interactive sessions (e.g. scripts or CI),
#' the function instead raises an error and lists the missing packages so they
#' can be installed manually.
#'
#' Users can supply additional package names via `additional_packages` to ensure
#' that optional features of the app also work.
#'
#' @return
#' This function is called for its side effect of launching a Shiny application.
#' It does not return a value.
#'
#' @examples
#' \dontrun{
#' # Launch the SHARK4R Bio-QC Tool
#' run_qc_app()
#'
#' # Launch with additional package checks
#' run_qc_app(additional_packages = c("plotly", "DT"))
#' }
#'
#' @export
run_qc_app <- function(additional_packages = NULL, interactive = TRUE) {
  appDir <- system.file("shiny", "shark-qc", package = "SHARK4R")
  if (appDir == "") {
    stop("Could not find shark-qc directory. Try re-installing `SHARK4R`.", call. = FALSE)
  }

  # Core required packages
  needed_pkgs <- c("shiny", "shinythemes", "htmltools", "rmarkdown", "skimr", "DT", "leaflet", "dplyr", "plotly")

  # Include user-specified packages
  needed_pkgs <- unique(c(needed_pkgs, additional_packages))

  # Check for missing packages
  missing <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing)) {
    msg <- paste("The following packages are required to run the app:",
                 paste(missing, collapse = ", "))

    if (interactive() && interactive) {
      message(msg, "\nWould you like to install them now? [y/N] ")
      ans <- tolower(trimws(readline()))
      if (ans %in% c("y", "yes")) {
        utils::install.packages(missing)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, " Please install them manually.", call. = FALSE)
    }
  }

  if (interactive) {
    shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
  }
}
