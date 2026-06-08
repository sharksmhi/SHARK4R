test_that("function runs without output when interactive is FALSE", {
  # Skip when any Suggested app dependency is absent (e.g. on CRAN, where
  # Suggested packages may not be installed for checking).
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("skimr")
  skip_if_not_installed("plotly")

  expect_silent(run_qc_app(interactive = FALSE))
})

test_that("run_qc_app stops when dependencies are missing", {
  # Copy the function to a local variable to modify temporarily
  run_qc_app_test <- run_qc_app

  # Override needed_pkgs in the function body
  body(run_qc_app_test)[[4]][[3]] <- quote(needed_pkgs <- "this_package_does_not_exist_999")

  expect_error(
    run_qc_app_test(interactive = FALSE),
    "Required package.*not installed"
  )
})
