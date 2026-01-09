test_that("positions_are_near_land works correctly", {
  # Skip slow test on CRAN
  skip_on_cran()
  skip_if_not_installed("iRfcb")
  skip_if_resource_unavailable("https://obis-resources.s3.amazonaws.com",
                               allow_status = c(0:399, 403))

  # Define test latitudes and longitudes
  latitudes <- c(62.500353, 58.964498, 57.638725, 56.575338, NA, 60.0)
  longitudes <- c(17.845993, 20.394418, 18.284523, 16.227174, 15.0, NA)

  # Test with default parameters
  near_land_default <- positions_are_near_land(latitudes, longitudes)
  expect_type(near_land_default, "logical")
  expect_length(near_land_default, length(latitudes))
  expect_true(all(is.na(near_land_default[is.na(latitudes)])))

  # Check that positions are near land correctly identified (dummy check)
  expected_near_land <- c(TRUE, FALSE, TRUE, TRUE, NA, NA)
  expect_equal(near_land_default, expected_near_land)
})

test_that("positions_are_near_land works correctly with eea vectors", {
  # Skip slow test on CRAN
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("iRfcb")
  skip_if_resource_unavailable("https://www.eea.europa.eu/data-and-maps/data/eea-coastline-for-analysis-2/gis-data/eea-coastline-polygon/download/",
                               timeout = 30)

  # Define test latitudes and longitudes
  latitudes <- c(62.500353, 58.964498, 57.638725, 56.575338, NA, 60.0)
  longitudes <- c(17.845993, 20.394418, 18.284523, 16.227174, 15.0, NA)

  # Test with default parameters
  near_land_eea <- positions_are_near_land(latitudes, longitudes, source = "eea")
  expect_type(near_land_eea, "logical")
  expect_length(near_land_eea, length(latitudes))
  expect_true(all(is.na(near_land_eea[is.na(latitudes)])))

  # Check that positions are near land correctly identified (dummy check)
  expected_near_land <- c(TRUE, FALSE, TRUE, TRUE, NA, NA)
  expect_equal(near_land_eea, expected_near_land)

  cache_files <- SHARK4R:::list_cache(include_perm = TRUE)

  # Verify that the EEA coastline file is cached
  expect_true(any(grepl("EEA_Coastline", cache_files)))

  # Clean up cached files
  clean_shark4r_cache(days = 0,
                      clear_perm_cache = TRUE,
                      search_pattern = "EEA_Coastline")

  # Verify that the cache is cleaned
  cache_files <- SHARK4R:::list_cache(include_perm = TRUE)
  expect_true(!any(grepl("EEA_Coastline", cache_files)))
})

test_that("positions_are_near_land works correctly with ne vectors", {
  # Skip slow test on CRAN
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("iRfcb")

  # Define test latitudes and longitudes
  latitudes <- c(62.500353, 58.964498, 57.638725, 56.575338, NA, 60.0)
  longitudes <- c(17.845993, 20.394418, 18.284523, 16.227174, 15.0, NA)

  # Test with default parameters
  near_land_ne <- positions_are_near_land(latitudes, longitudes, source = "ne")
  expect_type(near_land_ne, "logical")
  expect_length(near_land_ne, length(latitudes))
  expect_true(all(is.na(near_land_ne[is.na(latitudes)])))

  # Check that positions are near land correctly identified (dummy check)
  expected_near_land <- c(TRUE, FALSE, TRUE, FALSE, NA, NA)
  expect_equal(near_land_ne, expected_near_land)
})

test_that("which_basin correctly identifies basins", {
  skip_if_not_installed("iRfcb")

  # Define example latitude and longitude vectors for testing
  latitudes <- c(55.337, 54.729, 56.311, 57.975)
  longitudes <- c(12.674, 14.643, 12.237, 10.637)

  # Expected results (replace with the actual expected basins for these coordinates)
  expected_basins <- c("13 - Arkona Basin", "12 - Bornholm Basin",
                       "16 - Kattegat","17 - Skagerrak")

  # Call the function
  result <- which_basin(latitudes, longitudes)

  # Check that the result is as expected
  expect_equal(result, expected_basins)
})

test_that("deprecated function is still working", {
  # Skip slow test on CRAN
  skip_on_cran()
  skip_if_not_installed("iRfcb")

  # Define test latitudes and longitudes
  latitudes <- c(62.500353, 58.964498, 57.638725, 56.575338, NA, 60.0)
  longitudes <- c(17.845993, 20.394418, 18.284523, 16.227174, 15.0, NA)

  # Test if deprecated function still works
  lifecycle::expect_deprecated(ifcb_is_near_land(latitudes, longitudes))
  deprecated <- suppressWarnings(ifcb_is_near_land(latitudes, longitudes))

  expect_type(deprecated, "logical")
  expect_length(deprecated, length(latitudes))
  expect_true(all(is.na(deprecated[is.na(latitudes)])))

  # Check that positions are near land correctly identified (dummy check)
  expected_near_land <- c(TRUE, FALSE, TRUE, FALSE, NA, NA)
  expect_equal(deprecated, expected_near_land)
})

test_that("deprecated function is still working", {
  skip_if_not_installed("iRfcb")

  # Define example latitude and longitude vectors for testing
  latitudes <- c(55.337, 54.729, 56.311, 57.975)
  longitudes <- c(12.674, 14.643, 12.237, 10.637)

  # Expected results (replace with the actual expected basins for these coordinates)
  expected_basins <- c("13 - Arkona Basin", "12 - Bornholm Basin",
                       "16 - Kattegat","17 - Skagerrak")

  # Test if deprecated function still works
  lifecycle::expect_deprecated(ifcb_which_basin(latitudes, longitudes))
  result <- suppressWarnings(ifcb_which_basin(latitudes, longitudes))

  # Check that the result is as expected
  expect_equal(result, expected_basins)
})

test_that("positions_are_near_land fails gracefully", {
  skip_if_not_installed("iRfcb")

  # Define test latitudes and longitudes
  latitudes <- c(62.500353, 58.964498, 57.638725, 56.575338, NA, 60.0)
  longitudes <- c(17.845993, 20.394418, 18.284523, 16.227174, 15.0, NA)

  # Test with default parameters
  expect_error(positions_are_near_land(latitudes, longitudes,
                                       source = "invalid_source"),
               "Invalid value for 'source'")
})
