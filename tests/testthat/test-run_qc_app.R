test_that("errors if additional packages are missing", {
    expect_error(run_qc_app(additional_packages = "nonexistentpkg",
                            interactive = FALSE),
                 "nonexistentpkg")
})
