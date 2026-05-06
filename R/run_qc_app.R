#' Launch the SHARK4R Bio-QC Tool
#'
#' This function launches the interactive Shiny application for performing
#' quality control (QC) on SHARK data. The application provides a graphical
#' interface for exploring and validating data before or after submission to SHARK.
#'
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
#' @return
#' This function is called for its side effect of launching a Shiny application.
#' It does not return a value.
#'
#' @examples
#' \donttest{
#' # Launch the SHARK4R Bio-QC Tool
#' if(interactive()){
#'   run_qc_app()
#' }
#' }
#'
#' @export
run_qc_app <- function(interactive = TRUE) {
  appDir <- system.file("shiny", "shark-qc", package = "SHARK4R")
  if (appDir == "") {
    cli::cli_abort("Could not find shark-qc directory. Try re-installing {.pkg SHARK4R}.")
  }

  # Core required packages
  needed_pkgs <- c(
    "shiny", "bslib", "bsicons", "htmltools", "rmarkdown",
    "skimr", "DT", "leaflet", "dplyr", "plotly"
  )

  # Check for missing packages
  missing <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing)) {
    if (interactive() && interactive) {
      cli::cli_inform(c(
        "The following package{?s} {?is/are} required to run the app: {.pkg {missing}}",
        "i" = "Would you like to install them now? [y/N]"
      ))
      ans <- tolower(trimws(readline()))
      if (ans %in% c("y", "yes")) {
        utils::install.packages(missing)
      } else {
        cli::cli_abort("Required package{?s} not installed: {.pkg {missing}}")
      }
    } else {
      cli::cli_abort(c(
        "Required package{?s} not installed: {.pkg {missing}}",
        "i" = "Please install them manually."
      ))
    }
  }

  if (interactive() && interactive) {
    shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
  }
}
