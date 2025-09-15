test_that("positions_are_near_land works correctly", {
  # Skip slow test on CRAN
  skip_on_cran()
  skip_if_not_installed("iRfcb")

  # Define test latitudes and longitudes
  latitudes <- c(62.500353, 58.964498, 57.638725, 56.575338, NA, 60.0)
  longitudes <- c(17.845993, 20.394418, 18.284523, 16.227174, 15.0, NA)

  # Test with default parameters
  near_land_default <- positions_are_near_land(latitudes, longitudes)
  expect_type(near_land_default, "logical")
  expect_length(near_land_default, length(latitudes))
  expect_true(all(is.na(near_land_default[is.na(latitudes)])))

  # Check that positions are near land correctly identified (dummy check)
  expected_near_land <- c(TRUE, FALSE, TRUE, FALSE, NA, NA)
  expect_equal(near_land_default, expected_near_land)
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
