test_that("function runs without output when interactive is FALSE", {
  expect_silent(run_qc_app(interactive = FALSE))
})

test_that("run_qc_app stops when dependencies are missing", {
  # Copy the function to a local variable to modify temporarily
  run_qc_app_test <- run_qc_app

  # Override needed_pkgs in the function body
  body(run_qc_app_test)[[4]][[3]] <- quote(needed_pkgs <- "this_package_does_not_exist_999")

  expect_error(
    run_qc_app_test(interactive = FALSE),
    "required to run the app"
  )
})
