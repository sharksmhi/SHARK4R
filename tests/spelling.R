if (requireNamespace("spelling", quietly = TRUE)) {
  testthat::test_that("spelling", {
    testthat::skip_on_cran()
    testthat::skip_on_covr()

    spelling::spell_check_test(
      vignettes = TRUE,
      error = FALSE
    )
  })
}
